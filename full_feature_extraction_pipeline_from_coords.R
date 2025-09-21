# It takes a small list of locations (each with an id, longitude, and latitude) and builds a table of features for each location.
#It reads raster layers and extracts the value of each raster at each location.
#It reads a final prediction raster and adds a column called predicted_from_raster.
#It uses U.S. Census (ACS) data to compute simple 10 km buffer stats around each point:
   #total population in 10 km (total_pop_10km)
   # median household income in 10 km (median_income)

#It downloads OpenStreetMap roads around each point and computes:
    #total road length in 10 km and 5 km
    #a  “road size” score in 5 km (lanes/type weighted)
    #number of nearby intersections
    #a corner score within 50 m

#It loads two competitor lists:
    #ER list (hospitals + FEMCs combined) from er_combined_list.rds
    #FEMC list from femc_clean.rds and computes competitor metrics:
  
#How many ERs within 10 km, and the nearest ER distance
#how many FEMCs within 50 km, and the nearest FEMC distance

#It then combines everything into one table and saves it to CSV.

# It uses CRS 3083 for metric calculations.
     #er_combined_list.rds
     #femc_clean.rds
     #raster_predictions.tif 
     #KDE raster files (.tif files inside kde_rasters/)

# IMPORTANT: Set ROOT to the base folder on YOUR computer
# This script uses U.S. Census ACS data (population and income) via the tidycensus package
# so you must first request a free Census API key, activate it in R with census_api_key("YOUR_KEY", install = TRUE), then restart R before running the script.


suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(osmdata)
  library(tidycensus)
  library(tools)
})

# ==============================
# Global settings
# ==============================
options(tigris_use_cache = TRUE)
terraOptions(progress = 1, memfrac = 0.6, cores = max(1, parallel::detectCores() - 1))
METRIC_EPSG <- 3083                         
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ==============================
# Helpers
# ==============================
align_to_template <- function(path, tmpl) {
  stopifnot(file.exists(path))
  r <- terra::rast(path)
  if (!terra::compareGeom(r, tmpl, stopOnError = FALSE)) {
    r <- terra::mask(terra::crop(terra::project(r, tmpl), tmpl), tmpl)
  }
  r
}

make_feature_names <- function(paths) {
  cat  <- basename(dirname(paths))
  stem <- file_path_sans_ext(basename(paths))
  nm   <- paste0(cat, "_", mapply(function(s, c) sub(paste0("^", c, "_"), "", s), stem, cat))
  make.unique(gsub("[^A-Za-z0-9_]+", "_", nm), sep = "_")
}

norm_lonlat <- function(df, label) {
  stopifnot(is.data.frame(df))
  nm <- tolower(names(df))
  lon <- which(nm %in% c("lon","long","longitude"))
  lat <- which(nm %in% c("lat","latitude"))
  if (!length(lon) || !length(lat)) stop(sprintf("[%s] Missing lon/lat columns.", label))
  names(df)[lon[1]] <- "long"; names(df)[lat[1]] <- "lat"
  df$long <- suppressWarnings(as.numeric(df$long))
  df$lat  <- suppressWarnings(as.numeric(df$lat))
  dplyr::filter(df, is.finite(long), is.finite(lat))
}

# ==============================
# Census (10km buffer): total_pop_10km, median_income
# ==============================
census_stats <- function(points_sf_metric, year = 2022, survey = "acs5", buffer_km = 10) {
  if (is.na(sf::st_crs(points_sf_metric)$epsg) || sf::st_crs(points_sf_metric)$epsg != METRIC_EPSG)
    points_sf_metric <- sf::st_transform(points_sf_metric, METRIC_EPSG)
  
  key <- trimws(Sys.getenv("CENSUS_API_KEY", unset = ""))
  if (!nzchar(key)) stop("[census] No API key. Run tidycensus::census_api_key('YOUR_KEY', install=TRUE) then restart R.")
  
  tr <- tidycensus::get_acs(
    geography = "tract", state = "TX",
    variables = c(pop = "B01003_001", inc = "B19013_001"),
    year = year, survey = survey, geometry = TRUE, cache_table = TRUE
  )
  
  tr_wide <- tr |>
    dplyr::select(GEOID, variable, estimate, geometry) |>
    sf::st_as_sf() |>
    sf::st_transform(METRIC_EPSG) |>
    sf::st_make_valid() |>
    dplyr::distinct(GEOID, variable, .keep_all = TRUE) |>
    sf::st_drop_geometry() |>
    tidyr::pivot_wider(names_from = variable, values_from = estimate) |>
    dplyr::rename(total_pop = pop, median_hh_inc = inc)
  
  tr_geom <- tr |>
    dplyr::select(GEOID, geometry) |>
    sf::st_as_sf() |>
    sf::st_transform(METRIC_EPSG) |>
    sf::st_make_valid() |>
    dplyr::distinct(GEOID, .keep_all = TRUE)
  
  tr_full <- dplyr::left_join(tr_geom, tr_wide, by = "GEOID")
  bufs <- sf::st_buffer(points_sf_metric, buffer_km * 1000)
  
  agg_one <- function(g) {
    poly <- sf::st_sf(geometry = sf::st_sfc(g, crs = METRIC_EPSG))
    inter <- suppressWarnings(sf::st_intersection(tr_full, poly))
    if (!nrow(inter)) return(tibble(total_pop_10km = NA_real_, median_income = NA_real_))
    tibble(
      total_pop_10km = sum(as.numeric(inter$total_pop), na.rm = TRUE),
      median_income  = stats::median(as.numeric(inter$median_hh_inc), na.rm = TRUE)
    )
  }
  dplyr::bind_rows(lapply(bufs$geometry, agg_one))
}

# ==============================
# OSM road metrics (10km/5km + corner)
# ==============================
road_metrics <- function(lat, lon, r10_km = 10, r5_km = 5, corner_m = 50) {
  pt_m <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326) |> sf::st_transform(METRIC_EPSG)
  buf10 <- sf::st_buffer(pt_m, r10_km * 1000)
  buf5  <- sf::st_buffer(pt_m, r5_km  * 1000)
  bufC  <- sf::st_buffer(pt_m, corner_m)
  
  bbox <- sf::st_bbox(sf::st_transform(buf10, 4326))
  roads <- tryCatch({
    osmdata::opq(bbox) |> osmdata::add_osm_feature("highway") |> osmdata::osmdata_sf() |> (\(x) x$osm_lines)()
  }, error = function(e) NULL)
  
  out <- list(
    total_road_length_10km = NA_real_, intersection_count = NA_real_,
    total_road_length_5km = NA_real_,  total_weighted_road_size_5km = NA_real_,
    avg_road_size_5km = NA_real_,      total_corner_score = 0
  )
  if (is.null(roads) || !nrow(roads)) return(out)
  
  if (is.na(sf::st_crs(roads))) roads <- sf::st_set_crs(roads, 4326)
  roads_m <- sf::st_transform(roads, METRIC_EPSG)
  
  # 10km totals + intersections
  r10 <- suppressWarnings(sf::st_intersection(roads_m, buf10))
  out$total_road_length_10km <- as.numeric(sum(sf::st_length(r10), na.rm = TRUE) / 1000)
  pts <- suppressWarnings(sf::st_cast(r10, "POINT"))
  if (nrow(pts)) {
    xy <- as.data.frame(sf::st_coordinates(pts)) |>
      dplyr::mutate(x = round(X, -1), y = round(Y, -1))
    out$intersection_count <- xy |>
      dplyr::count(x, y) |>
      dplyr::filter(n >= 2) |>
      nrow()
  } else out$intersection_count <- 0
  
  # 5km metrics
  r5 <- suppressWarnings(sf::st_intersection(roads_m, buf5))
  if (nrow(r5)) {
    out$total_road_length_5km <- as.numeric(sum(sf::st_length(r5), na.rm = TRUE) / 1000)
    if (is.finite(out$total_road_length_5km) && out$total_road_length_5km > 0) {
      if (!"lanes" %in% names(r5))    r5$lanes   <- NA
      if (!"highway" %in% names(r5))  r5$highway <- NA_character_
      score <- r5 |>
        dplyr::mutate(
          lane_score = suppressWarnings(as.numeric(lanes)),
          type_score = dplyr::case_when(
            grepl("motorway", highway %||% "") ~ 6,
            grepl("trunk",    highway %||% "") ~ 5,
            grepl("primary",  highway %||% "") ~ 4,
            grepl("secondary",highway %||% "") ~ 3,
            grepl("tertiary", highway %||% "") ~ 2,
            grepl("residential|unclassified|living_street", highway %||% "") ~ 1,
            TRUE ~ 0
          ),
          final_score = ifelse(!is.na(lane_score), lane_score, type_score),
          len_m       = as.numeric(sf::st_length(r5)),
          weight      = final_score * len_m
        )
      out$total_weighted_road_size_5km <- sum(score$weight, na.rm = TRUE) / 1000
      out$avg_road_size_5km <- out$total_weighted_road_size_5km / out$total_road_length_5km
    }
  }
  
  # corner score (within 50 m)
  rC <- suppressWarnings(sf::st_intersection(roads_m, bufC))
  if (nrow(rC) >= 2) {
    if (!"lanes" %in% names(rC))   rC$lanes   <- NA
    if (!"highway" %in% names(rC)) rC$highway <- NA_character_
    out$total_corner_score <- rC |>
      dplyr::mutate(
        lane_score = suppressWarnings(as.numeric(lanes)),
        type_score = dplyr::case_when(
          grepl("motorway", highway %||% "") ~ 6,
          grepl("trunk",    highway %||% "") ~ 5,
          grepl("primary",  highway %||% "") ~ 4,
          grepl("secondary",highway %||% "") ~ 3,
          grepl("tertiary", highway %||% "") ~ 2,
          grepl("residential|unclassified|living_street", highway %||% "") ~ 1,
          TRUE ~ 0
        ),
        final_score = ifelse(!is.na(lane_score), lane_score, type_score)
      ) |>
      dplyr::pull(final_score) |>
      sum(na.rm = TRUE)
  }
  out
}

# ==============================
# Competitors 
# ==============================
competitor_metrics <- function(lon, lat, er_path, femc_path,
                               er_radius_km = 10, femc_radius_km = 50,
                               self_tol_m = 1) {
  if (!file.exists(er_path))   stop(sprintf("[ER] File not found: %s", er_path))
  if (!file.exists(femc_path)) stop(sprintf("[FEMC] File not found: %s", femc_path))
  er   <- norm_lonlat(readRDS(er_path),   "ER")
  femc <- norm_lonlat(readRDS(femc_path), "FEMC")
  if (!nrow(er))   stop("[ER] No valid rows.")
  if (!nrow(femc)) stop("[FEMC] No valid rows.")
  
  pt    <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326) |> sf::st_transform(METRIC_EPSG)
  er_sf <- sf::st_as_sf(er,   coords = c("long","lat"), crs = 4326, remove = FALSE) |> sf::st_transform(METRIC_EPSG)
  fm_sf <- sf::st_as_sf(femc, coords = c("long","lat"), crs = 4326, remove = FALSE) |> sf::st_transform(METRIC_EPSG)
  
  d_er <- as.numeric(sf::st_distance(pt, er_sf))
  d_fm <- as.numeric(sf::st_distance(pt, fm_sf))
  d_er_nz <- d_er[d_er > self_tol_m]
  d_fm_nz <- d_fm[d_fm > self_tol_m]
  
  list(
    er_within_10km   = as.integer(sum(d_er < er_radius_km * 1000, na.rm = TRUE) - sum(d_er <= self_tol_m, na.rm = TRUE)),
    nearest_er_km    = if (length(d_er_nz)) min(d_er_nz) / 1000 else NA_real_,
    femc_within_50km = as.integer(sum(d_fm < femc_radius_km * 1000, na.rm = TRUE) - sum(d_fm <= self_tol_m, na.rm = TRUE)),
    nearest_femc_km  = if (length(d_fm_nz)) min(d_fm_nz) / 1000 else NA_real_
  )
}

# ==============================
# Main: build feature table from coords
# ==============================
coords_to_ml_features <- function(points, feature_paths,
                                  final_pred_tif,              
                                  acs_year = 2022, acs_survey = "acs5",
                                  census_buffer_km = 10, er_path, femc_path) {
  stopifnot(is.data.frame(points), all(c("id","lon","lat") %in% names(points)))
  stopifnot(length(feature_paths) > 0, all(file.exists(feature_paths)))
  if (is.null(final_pred_tif) || !file.exists(final_pred_tif)) {
    stop("[pred] final_pred_tif is required and must exist.")
  }
  
  # template & feature stack 
  tmpl <- terra::rast(final_pred_tif)
  feat <- terra::rast(lapply(feature_paths, align_to_template, tmpl = tmpl))
  names(feat) <- make_feature_names(feature_paths)
  
  pts_wgs  <- sf::st_as_sf(points, coords = c("lon","lat"), crs = 4326, remove = FALSE)
  pts_tmpl <- sf::st_transform(pts_wgs, terra::crs(tmpl))
  pts_v    <- terra::vect(pts_tmpl)
  
  # raster features
  vals <- terra::extract(feat, pts_v) |>
    tibble::as_tibble() |>
    dplyr::select(-ID)
  
  # prediction 
  pv <- terra::extract(tmpl, pts_v) |>
    tibble::as_tibble() |>
    dplyr::select(-ID)
  names(pv) <- "predicted_from_raster"
  pred <- pv[, 1, drop = FALSE]
  
  # census (10km buffer)
  pts_sf_metric <- sf::st_transform(pts_wgs, METRIC_EPSG)
  cen <- census_stats(pts_sf_metric, year = acs_year, survey = acs_survey,
                      buffer_km = census_buffer_km)
  
  # roads + competitors
  rows <- vector("list", nrow(points))
  for (i in seq_len(nrow(points))) {
    lon <- points$lon[i]; lat <- points$lat[i]
    r <- tryCatch(road_metrics(lat, lon), error = function(e) {
      list(total_road_length_10km=NA, intersection_count=NA, total_road_length_5km=NA,
           total_weighted_road_size_5km=NA, avg_road_size_5km=NA, total_corner_score=0)
    })
    c <- tryCatch(competitor_metrics(lon, lat, er_path, femc_path), error = function(e) {
      list(er_within_10km=NA_integer_, nearest_er_km=NA_real_,
           femc_within_50km=NA_integer_, nearest_femc_km=NA_real_)
    })
    rows[[i]] <- tibble::as_tibble(c(r, c))
  }
  custom <- dplyr::bind_rows(rows)
  
  # Combine
  out <- dplyr::bind_cols(points, pred, vals, cen, custom)
  
  roads_comp_cols <- c(
    "total_road_length_10km","intersection_count","total_road_length_5km",
    "total_weighted_road_size_5km","avg_road_size_5km","total_corner_score",
    "er_within_10km","nearest_er_km","femc_within_50km","nearest_femc_km"
  )
  out <- dplyr::relocate(out, "predicted_from_raster", .after = lat)
  exist_cols <- intersect(roads_comp_cols, names(out))
  out <- dplyr::relocate(out, dplyr::all_of(exist_cols), .after = predicted_from_raster)
  
  out
}

# ==============================
# RUN (edit paths below)
# ==============================
if (identical(environment(), globalenv())) {
  ROOT        <- "C:/Users/Pamela/OneDrive/Documents"     # <-- change as needed
  KDE_DIR     <- file.path(ROOT, "kde_rasters")
  ER_PATH     <- file.path(ROOT, "er_combined_list.rds")  # hospitals+FEMC combined 
  FEMC_PATH   <- file.path(ROOT, "femc_clean.rds")        # FEMC only
  FINAL_PRED  <- file.path(ROOT, "raster_predictions.tif")
  OUT_CSV     <- file.path(ROOT, "ml_enriched_from_coords_full.csv")
  
  if (!file.exists(FINAL_PRED)) stop("[setup] Missing FINAL_PRED raster: ", FINAL_PRED)
  if (!file.exists(ER_PATH))   stop("[setup] Missing ER_PATH: ", ER_PATH)
  if (!file.exists(FEMC_PATH)) stop("[setup] Missing FEMC_PATH: ", FEMC_PATH)
  
  feature_paths <- list.files(KDE_DIR, pattern = "\\.tif(f)?$", recursive = TRUE, full.names = TRUE)
  if (!length(feature_paths)) stop("[setup] No .tif rasters found under: ", KDE_DIR)
  
  # Example sites (replace with your own)
  sites <- tibble::tibble(
    id  = c("S1","S2"),
    lon = c(-97.7431, -96.7969),   
    lat = c( 30.2672,  32.7767)
  )
  
  ml_features <- coords_to_ml_features(
    points            = sites,
    feature_paths     = feature_paths,
    final_pred_tif    = FINAL_PRED,     
    acs_year          = 2022,
    acs_survey        = "acs5",
    census_buffer_km  = 10,
    er_path           = ER_PATH,
    femc_path         = FEMC_PATH
  )
  
  readr::write_csv(ml_features, OUT_CSV)
  message("[saved] ", OUT_CSV)
}
