
suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(readr)
  library(tibble)
})

# runtime knobs for terra that control how raster ops run (speed, memory, responsiveness)
terraOptions(progress = 1, memfrac = 0.6,
             cores = max(1, parallel::detectCores() - 1))


# ------
# Paths 
# ------
tiles_path          <- "final_tiles.rds"      
hospital_table_path <- "hospital_clean.rds"   
femc_table_path     <- "femc_clean.rds"       

kde_out_root <- "kde_rasters"
progress_csv <- "competitor_progress.csv"
loo_out_rds  <- "femc_with_competitor_loo.rds"

dir.create(kde_out_root, showWarnings = FALSE, recursive = TRUE)

# ------------
# Parameters
# ------------
PROJECTED_CRS <- 3083              
RESOLUTION_M  <- 200
SNAP_ORIGIN   <- c(0, 0)
SIGMAS_COMP   <- c(1000, 3000, 5000, 10000)
RADIUS_MULT   <- 3
GDAL_OPTS     <- c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER")

tag_hosp <- "competitor_hospital"
tag_femc <- "competitor_femc"
dir.create(file.path(kde_out_root, tag_hosp), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(kde_out_root, tag_femc), showWarnings = FALSE, recursive = TRUE)

# --------------------------
# Helpers
# --------------------------
# Useful for expanding bounding boxes so all rasters align perfectly
# It forces the raster’s bounding box to snap to a clean multiple of the resolution (200 m) from a fixed origin (0,0)

snap <- function(x, origin, res, up = FALSE) {
  if (up) origin + ceiling((x - origin) / res) * res else origin + floor((x - origin) / res) * res
}

# It creates a blank raster grid whose extent is snapped to  multiples of the resolution
# ensuring all rasters align perfectly on the same grid.

tile_template_on_buffer <- function(buffer_proj_sfc,
                                    res_m = RESOLUTION_M,
                                    origin_xy = SNAP_ORIGIN) {
  stopifnot(!is.na(sf::st_crs(buffer_proj_sfc)))
  bb <- sf::st_bbox(buffer_proj_sfc)
  xmin <- snap(bb["xmin"], origin_xy[1], res_m, up = FALSE)
  ymin <- snap(bb["ymin"], origin_xy[2], res_m, up = FALSE)
  xmax <- snap(bb["xmax"], origin_xy[1], res_m, up = TRUE)
  ymax <- snap(bb["ymax"], origin_xy[2], res_m, up = TRUE)
  terra::rast(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
              crs = paste0("EPSG:", PROJECTED_CRS), resolution = res_m)
}

# ---- Full 2-D Gaussian kernel ----
# It builds a 2-D Gaussian weight matrix whose values are largest in the center and taper off smoothly
# Then normalizes it so all weights sum to 1

gauss_kernel_2d <- function(sigma_cells, radius_mult = RADIUS_MULT) {
  r  <- ceiling(radius_mult * sigma_cells)
  ax <- (-r):r
  K  <- outer(ax, ax, function(i, j) exp(-(i*i + j*j) / (2 * sigma_cells^2)))
  K / sum(K)
}



# focal2d_sum Takes a raster and a kernel
# Applies moving-window convolution
# Treats outside cells as 0 when the kernel sticks out at edges
# writes result directly to disk instead of RAM
# Returns a raster object pointing to that temporary file

focal2d_sum <- function(x, K, tmp_prefix) {
  tf <- paste0(tmp_prefix, "_focal2d.tif")
  terra::focal(x, w = K, fun = "sum",
               pad = TRUE, padValue = 0,
               filename = tf, overwrite = TRUE,
               wopt = list(gdal = GDAL_OPTS))
}

# load an rds table  and standardize its column names
read_table_rds <- function(path) {
  stopifnot(file.exists(path))
  x <- readRDS(path); if (!is.data.frame(x)) x <- as.data.frame(x)
  names(x) <- tolower(names(x))
  x
}

# Returns an sf object with POINT geometries in WGS84 (EPSG:4326)
make_points_sf <- function(df) {
  stopifnot(all(c("lon","lat") %in% names(df)))
  df2 <- df %>%
    mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
    filter(is.finite(lon), is.finite(lat))
  sf::st_as_sf(df2, coords = c("lon","lat"), crs = 4326)
}

# Convert an sf POINT layer to a count raster aligned to template_r
# Reproject points to the CRS of template_r (meters) to match its grid
rasterize_points <- function(sf_pts, template_r) {
  if (!inherits(sf_pts, "sf") || nrow(sf_pts) == 0) return(NULL)
  pts_proj <- suppressWarnings(sf::st_transform(sf_pts, terra::crs(template_r)))
  terra::rasterize(terra::vect(pts_proj), template_r, field = 1, fun = "sum", background = 0)
}

kde_out_path <- function(tag, sigma_m) {
  file.path(kde_out_root, tag, sprintf("%s_KDE_sigma%skm.tif",
                                       tag, format(sigma_m/1000, trim=TRUE, nsmall=0)))
}

# ---- Progress CSV ----
progress_schema <- tibble(
  tag     = character(),
  sigma_m = double(),
  step    = character(),   
  status  = character(),  
  out     = character()
)

# we use it to keep track of completed steps and allow restarting the script without redoing everything.

load_progress <- function() {
  if (file.exists(progress_csv)) {
    suppressMessages(readr::read_csv(progress_csv, show_col_types = FALSE)) %>%
      dplyr::select(any_of(names(progress_schema))) %>%
      dplyr::mutate(
        tag     = as.character(tag),
        sigma_m = as.numeric(sigma_m),
        step    = as.character(step),
        status  = as.character(status),
        out     = as.character(out)
      )
  } else progress_schema
}

save_progress <- function(df) readr::write_csv(df, progress_csv)

# Set_status() records or updates the status of a processing step in the progress log
# Already_done() checks that log (and the file) to decide if the step can be safely skipped.
set_status <- function(p, tag, sigma_m, step, out, status) {
  key <- (p$tag==tag & p$sigma_m==sigma_m & p$step==step)
  if (any(key)) {
    p$status[key] <- status
    p$out[key]    <- out
  } else {
    p <- dplyr::bind_rows(p, tibble(tag=tag, sigma_m=sigma_m, step=step, status=status, out=out))
  }
  p
}

already_done <- function(p, tag, sigma_m, step, out) {
  file.exists(out) && any(p$tag==tag & p$sigma_m==sigma_m & p$step==step & p$status=="DONE")
}

# --------------------------------------------------------------------
# This section merges all tile polygons into one study-area polygon,
# Then creates a clean, snapped raster template and mask to ensure all later rasters align perfectly
# -------------------------------------------------------------------

if (!file.exists(tiles_path)) {
  stop("Required tiles file not found: ", tiles_path,
       "\nPlease provide final_tiles.rds (union-of-tiles is mandatory).")
}

tiles    <- readRDS(tiles_path)
get_tile <- function(i) tiles[[i]]
ntiles   <- length(tiles)

buffers_proj <- lapply(seq_len(ntiles), function(i) {
  geom <- suppressWarnings(sf::st_transform(get_tile(i)$geometry, PROJECTED_CRS))
  sf::st_make_valid(geom)
})
buffers_proj <- do.call(c, buffers_proj)
buffers_proj <- buffers_proj[!sf::st_is_empty(buffers_proj)]
buffers_proj <- sf::st_make_valid(buffers_proj)
coverage_proj <- sf::st_union(buffers_proj)

# Template + mask on the official grid
r_template <- tile_template_on_buffer(coverage_proj, RESOLUTION_M, SNAP_ORIGIN)
coverage_r <- terra::rasterize(terra::vect(coverage_proj), r_template,
                               field = 1, background = NA, touches = TRUE)
res_m <- terra::res(r_template)[1]

# -------------
# Load inputs 
# -------------
hospital_clean <- read_table_rds(hospital_table_path)
femc_clean     <- read_table_rds(femc_table_path)

req_cols <- c("thcic_id","facility_type","facility","lat","lon")
stopifnot(all(req_cols %in% names(hospital_clean)))
stopifnot(all(req_cols %in% names(femc_clean)))

# --------------------------
# Prepare point-count rasters
# --------------------------
# Convert hospital/FEMC tables to sf POINTS in WGS84
hosp_pts_wgs <- make_points_sf(hospital_clean)
femc_pts_wgs <- make_points_sf(femc_clean)

# Rasterize points onto the aligned template
hosp_r_count <- rasterize_points(hosp_pts_wgs, r_template)
femc_r_count <- rasterize_points(femc_pts_wgs, r_template)
stopifnot(!is.null(hosp_r_count), !is.null(femc_r_count))

# Build validity masks (1 where cell is finite, else 0) for edge-bias correction (num/den)
valid_h <- terra::ifel(is.finite(hosp_r_count), 1, 0)
valid_f <- terra::ifel(is.finite(femc_r_count), 1, 0)

# Reproject FEMC points to the projected CRS for accurate raster extraction
femc_pts_m <- suppressWarnings(sf::st_transform(femc_pts_wgs, PROJECTED_CRS))
xy_proj    <- sf::st_coordinates(femc_pts_m)

# Initialize LOO table 
femc_with_comp <- if (file.exists(loo_out_rds)) readRDS(loo_out_rds) else femc_clean

# Load progress
progress <- load_progress()

# --------------------------
# A) HOSPITAL KDE 
# Loop over each σ, build a Gaussian kernel
# Compute hospital KDE as num/den with edge correction
# mask to coverage, write the TIFF, and record progress so re-runs can safely skip finished work
# --------------------------
for (s in SIGMAS_COMP) {
  out_fp <- kde_out_path(tag_hosp, s)
  if (already_done(progress, tag_hosp, s, "kde", out_fp)) {
    message(sprintf("[SKIP] %s sigma=%dm (DONE)", tag_hosp, s)); next
  }
  if (file.exists(out_fp)) {
    message(sprintf("[SKIP-FILE] %s sigma=%dm (file exists)", tag_hosp, s))
    progress <- set_status(progress, tag_hosp, s, "kde", out_fp, "DONE"); save_progress(progress)
    next
  }
  
  tryCatch({
    sigma_cells <- s / res_m
    K <- gauss_kernel_2d(sigma_cells)
    
    # NUM: smoothed counts (NA->0)
    num_in <- terra::ifel(is.finite(hosp_r_count), hosp_r_count, 0)
    num    <- focal2d_sum(num_in, K, tempfile("num2d_hosp"))
    
    # DEN: smoothed valid mask
    den    <- focal2d_sum(valid_h, K, tempfile("den2d_hosp"))
    
    r_kde <- num / den
    r_kde[den == 0] <- NA
    
    r_kde_masked <- terra::mask(r_kde, coverage_r)
    terra::writeRaster(r_kde_masked, filename = out_fp, overwrite = TRUE, gdal = GDAL_OPTS)
    
    message(sprintf("[DONE] %s sigma=%dm -> %s", tag_hosp, s, basename(out_fp)))
    progress <- set_status(progress, tag_hosp, s, "kde", out_fp, "DONE"); save_progress(progress)
  }, error = function(e) {
    message(sprintf("[ERROR] %s sigma=%dm: %s", tag_hosp, s, e$message))
    progress <<- set_status(progress, tag_hosp, s, "kde", out_fp, "ERROR"); save_progress(progress)
  })
}

# --------------------------
# B) FEMC KDE + LOO 
# For each σ, compute FEMC KDE with edge correction, save the raster
# Then compute per-FEMC leave-one-out competitor values by subtracting each point’s self-contribution
# All with resume-safe logging.
# --------------------------
for (s in SIGMAS_COMP) {
  out_fp <- kde_out_path(tag_femc, s)
  if (!already_done(progress, tag_femc, s, "kde", out_fp)) {
    if (file.exists(out_fp)) {
      message(sprintf("[SKIP-FILE] %s sigma=%dm (file exists)", tag_femc, s))
      progress <- set_status(progress, tag_femc, s, "kde", out_fp, "DONE"); save_progress(progress)
    } else {
      tryCatch({
        sigma_cells <- s / res_m
        K <- gauss_kernel_2d(sigma_cells)
        
        # NUM & DEN once per sigma 
        num_in <- terra::ifel(is.finite(femc_r_count), femc_r_count, 0)
        num    <- focal2d_sum(num_in, K, tempfile("num2d_femc"))
        den    <- focal2d_sum(valid_f, K, tempfile("den2d_femc"))
        
        r_kde <- num / den
        r_kde[den == 0] <- NA
        
        r_kde_masked <- terra::mask(r_kde, coverage_r)
        terra::writeRaster(r_kde_masked, filename = out_fp, overwrite = TRUE, gdal = GDAL_OPTS)
        
        message(sprintf("[DONE] %s sigma=%dm -> %s", tag_femc, s, basename(out_fp)))
        progress <- set_status(progress, tag_femc, s, "kde", out_fp, "DONE"); save_progress(progress)
        
        colnm  <- paste0("femc_competitor_", format(s/1000, trim=TRUE, nsmall=0), "km")
        loo_id <- paste0(loo_out_rds, "::", colnm)
        
        if (already_done(progress, tag_femc, s, "loo", loo_id) && (colnm %in% names(femc_with_comp))) {
          message(sprintf("[SKIP] LOO %s (DONE)", colnm))
        } else {
          center_idx  <- c(ceiling(nrow(K)/2), ceiling(ncol(K)/2))
          K_center_2D <- K[center_idx[1], center_idx[2]]
          
          v_all_norm <- terra::extract(r_kde, xy_proj)[,1]
          den_at_pts <- terra::extract(den,   xy_proj)[,1]
          den_at_pts[!is.finite(den_at_pts) | den_at_pts <= 0] <- NA_real_
          
          v_loo <- v_all_norm - (K_center_2D / den_at_pts)
          v_loo <- pmax(v_loo, 0)  # clamp tiny negatives from rounding
          
          femc_with_comp[[colnm]] <- v_loo
          saveRDS(femc_with_comp, loo_out_rds)
          
          message(sprintf("[DONE] LOO column -> %s", colnm))
          progress <- set_status(progress, tag_femc, s, "loo", loo_id, "DONE"); save_progress(progress)
        }
      }, error = function(e) {
        message(sprintf("[ERROR] %s sigma=%dm: %s", tag_femc, s, e$message))
        progress <<- set_status(progress, tag_femc, s, "kde", out_fp, "ERROR"); save_progress(progress)
      })
    }
  } else {
    message(sprintf("[SKIP] %s sigma=%dm (DONE)", tag_femc, s))
  }
}

message("All competitor tasks complete (resume-safe, tiles required, direct 2-D Gaussian).")



# ----------------------------------------------------------------------
# the following part extracts hospital KDE values at each FEMC location for all sigmas
# Adds them as new columns, and saves the updated FEMC table
# ----------------------------------------------------------------------

# Inputs you already have
femc_table_path <- "femc_clean.rds"            # or use femc_with_competitor_loo.rds to append
PROJECTED_CRS   <- 3083

# Helper to build raster paths
kde_out_root <- "kde_rasters"
tag_hosp     <- "competitor_hospital"
kde_out_path <- function(tag, sigma_m) {
  file.path(kde_out_root, tag,
            sprintf("%s_KDE_sigma%skm.tif", tag, format(sigma_m/1000, trim=TRUE, nsmall=0)))
}

SIGMAS <- c(1000, 3000, 5000, 10000)

# Load FEMC table and make projected points
femc_df    <- readRDS(femc_table_path)
femc_pts_w <- st_as_sf(
  femc_df %>% mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>% 
    filter(is.finite(lon), is.finite(lat)),
  coords = c("lon","lat"), crs = 4326
)
femc_pts_m <- suppressWarnings(st_transform(femc_pts_w, PROJECTED_CRS))
xy_proj    <- st_coordinates(femc_pts_m)

# Extract from each hospital KDE raster and add columns
out_df <- femc_df
for (s in SIGMAS) {
  rpath <- kde_out_path(tag_hosp, s)
  if (!file.exists(rpath)) stop("Missing hospital KDE raster: ", rpath)
  r     <- rast(rpath)  # masked, normalized KDE
  
  vals  <- terra::extract(r, xy_proj)[,1]  # numeric vector (NA if outside coverage)
  colnm <- paste0("hospital_competitor_", format(s/1000, trim=TRUE, nsmall=0), "km")
  out_df[[colnm]] <- vals
}

# Save a new table (append to existing if you want)
saveRDS(out_df, "femc_with_hospital_kde.rds")
cat("Saved hospital KDE features -> femc_with_hospital_kde.rds\n")





# This block loads the 2 FEMC tables (one with FEMC LOO competitor features and one with hospital KDE-at-FEMC features) Selects only the hospital feature columns from the second table
# And then performs a 1:1 left join so each FEMC row gets its corresponding hospital competitor columns; finally it saves the combined result as femc_competitor_plus_hospital_kde.rds.

# Load both tables
femc_loo <- readRDS("femc_with_competitor_loo.rds")     # has femc_competitor_*km
hosp_kde <- readRDS("femc_with_hospital_kde.rds")       # has hospital_competitor_*km

# Harmonize the key column name/type
std_key <- function(df) {
  nm <- names(df)
  # accept either thcic_id or thcic
  if ("thcic" %in% nm && !"thcic_id" %in% nm) df <- rename(df, thcic_id = thcic)
  df %>% mutate(thcic_id = as.character(thcic_id))
}

femc_loo <- std_key(femc_loo)
hosp_kde <- std_key(hosp_kde)

# Keep only thcic_id + the hospital KDE columns
hosp_feats <- hosp_kde %>%
  select(thcic_id, starts_with("hospital_competitor_"))

# Join on FEMC rows
femc_joined <- femc_loo %>%
  left_join(hosp_feats, by = "thcic_id")

# Save
saveRDS(femc_joined, "femc_competitor_plus_hospital_kde.rds")
cat("Saved -> femc_competitor_plus_hospital_kde.rds\n")


