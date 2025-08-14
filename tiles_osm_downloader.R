# ============================
# OSM Downloader 
# - Features:
#     restaurants -> osm_points + centroids
#     highways    -> osm_lines + osm_multilines 
# - Resumes by skipping existing files; logs to CSV
# ============================

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(sf)
  library(osmdata)
  library(dplyr)
  library(readr)
  library(purrr)
  library(stringr)
})

# ------------ Setup -------------
tiles_path          <- "final_tiles.rds"
output_root         <- "tile_downloads"
progress_csv        <- file.path(output_root, "download_progress.csv")
sleep_between_calls <- 1.0
opq_timeout_sec     <- 120

feature_dirs <- list(
  restaurants = file.path(output_root, "restaurants"),
  highways    = file.path(output_root, "highways")
)

dir.create(output_root, showWarnings = FALSE, recursive = TRUE)
purrr::walk(feature_dirs, ~dir.create(.x, showWarnings = FALSE, recursive = TRUE))

# ------------ Helpers ------------
# Reproject a tile’s sfc geometry to WGS84 (EPSG:4326) and returns its bounding box

tile_bbox_wgs84 <- function(sfc_geom) {
  if (!inherits(sfc_geom, "sfc")) stop("geometry must be an sfc object")
  if (is.na(sf::st_crs(sfc_geom))) stop("Tile geometry has no CRS.")
  g84 <- suppressWarnings(sf::st_transform(sfc_geom, 4326))
  sf::st_bbox(g84)
}
# Extract/normalize the bbox
.as_bbox_vec <- function(x) {
  if (inherits(x, "bbox")) return(as.numeric(c(x["xmin"], x["ymin"], x["xmax"], x["ymax"])))
  if (is.numeric(x) && length(x) >= 4) return(as.numeric(x[1:4]))
  if (is.list(x)) {
    if (!is.null(x$geometry) && inherits(x$geometry, "sfc")) {
      return(as.numeric(tile_bbox_wgs84(x$geometry[1])))
    }
    nm <- names(x)
    schemes <- list(
      c("xmin","ymin","xmax","ymax"),
      c("minx","miny","maxx","maxy"),
      c("left","bottom","right","top"),
      c("west","south","east","north"),
      c("lon_min","lat_min","lon_max","lat_max"),
      c("x_min","y_min","x_max","y_max")
    )
    for (sch in schemes) if (!is.null(nm) && all(sch %in% nm)) return(as.numeric(unlist(x[sch])))
    if (!is.null(x$bbox)) return(.as_bbox_vec(x$bbox))
  }
  stop("Could not interpret bbox from object.")
}

get_bbox_from_tile <- function(tile_obj) {
  if (inherits(tile_obj, "sf")) {
    g <- sf::st_geometry(tile_obj); if (length(g) < 1) stop("sf tile has no geometry.")
    return(as.numeric(tile_bbox_wgs84(g[1])))
  }
  if (inherits(tile_obj, "sfc")) {
    if (length(tile_obj) < 1) stop("sfc tile has no geometry.")
    return(as.numeric(tile_bbox_wgs84(tile_obj[1])))
  }
  return(.as_bbox_vec(tile_obj))
}

# Safety check to confirm an OSM layer is non-empty and usable
has_rows <- function(x) {
  !is.null(x) && inherits(x, "sf") && nrow(x) > 0 && ncol(x) > 0 && !is.null(sf::st_geometry(x))
}

# This step cleans up list-columns and fixes problematic column names so GeoPackage writes don’t fail again.
# Turns an sf object into a GPKG-safe table: flattens list-columns (except geometry) to strings.
# Cleans non-geometry column names while preserving the geometry column, so st_write() won’t error

prep_for_write <- function(x) {
  if (is.null(x) || !inherits(x, "sf")) return(x)
  geo_col <- attr(x, "sf_column")
  if (is.null(geo_col) || !(geo_col %in% names(x))) stop("sf object has no recognized geometry column.")
  MAXLEN <- 63
  
  is_list_col <- vapply(x, is.list, logical(1))
  if (any(is_list_col)) {
    is_list_col[names(x) == geo_col] <- FALSE
    x[is_list_col] <- lapply(x[is_list_col], function(col) {
      vapply(col, function(v) if (is.null(v)) "" else paste0(v, collapse = ";"), character(1))
    })
  }
  
  nm      <- names(x)
  geo_idx <- which(nm == geo_col)
  out     <- nm
  used_lc <- tolower(nm[geo_idx])   
  
  sanitize_base <- function(s) {
    s <- gsub("[^A-Za-z0-9_]+", "_", s)    
    s <- gsub("^([0-9])", "X\\1", s)        
    if (s == "") s <- "X"
    s
  }
  
  make_unique_trunc <- function(base, used_lc, maxlen = MAXLEN) {
    if (tolower(base) %in% c("geom", used_lc)) base <- paste0(base, "_attr")
    cand <- substr(base, 1, maxlen)
    if (!(tolower(cand) %in% used_lc)) return(list(name = cand, used = c(used_lc, tolower(cand))))
    k <- 1
    repeat {
      suffix <- paste0("_", k)
      lim    <- maxlen - nchar(suffix)
      if (lim < 1) lim <- 1
      cand2  <- paste0(substr(base, 1, lim), suffix)
      if (!(tolower(cand2) %in% used_lc)) return(list(name = cand2, used = c(used_lc, tolower(cand2))))
      k <- k + 1
    }
  }
  
  for (j in seq_along(nm)) {
    if (j == geo_idx) next  
    base <- sanitize_base(nm[j])
    mk   <- make_unique_trunc(base, used_lc, MAXLEN)
    out[j]  <- mk$name
    used_lc <- mk$used
  }
  
  names(x) <- out
  attr(x, "sf_column") <- geo_col
  x
}

# Centroid helper
# Computes a centroid robustly for polygon sf data: try st_centroid(), and if that errors, temporarily turn s2 off and try again.

safe_centroid <- function(x) {
  if (is.null(x) || !inherits(x, "sf") || nrow(x) == 0) return(NULL)
  
  # Try current s2 setting
  res <- try(sf::st_centroid(x), silent = TRUE)
  if (!inherits(res, "try-error")) return(res)
  
  # Temporarily turn s2 OFF 
  old <- sf::sf_use_s2()
  if (isTRUE(old)) {
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old)), add = TRUE)
  }
  
  res <- try(sf::st_centroid(x), silent = TRUE)
  if (!inherits(res, "try-error")) return(res)
  

  res <- try(sf::st_point_on_surface(x), silent = TRUE)
  if (!inherits(res, "try-error")) return(res)
  
  if ("st_make_valid" %in% getNamespaceExports("sf")) {
    xv <- try(sf::st_make_valid(x), silent = TRUE)
    if (!inherits(xv, "try-error")) {
      res <- try(sf::st_point_on_surface(xv), silent = TRUE)
      if (!inherits(res, "try-error")) return(res)
      res <- try(sf::st_centroid(xv), silent = TRUE)
      if (!inherits(res, "try-error")) return(res)
    }
  }
  NULL
}

# Progress log
if (!file.exists(progress_csv)) {
  readr::write_csv(
    tibble::tibble(
      timestamp = character(),
      tile_idx  = integer(),
      feature   = character(),
      status    = character(),
      note      = character()
    ),
    progress_csv
  )
}

append_progress <- function(tile_idx, feature, status, note = "") {
  readr::write_csv(
    tibble::tibble(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      tile_idx  = tile_idx,
      feature   = feature,
      status    = status,
      note      = note
    ),
    progress_csv,
    append = TRUE
  )
}

# ------------ Load tiles ------------
if (!file.exists(tiles_path)) stop("final_tiles.rds not found at: ", tiles_path)
final_tiles <- readRDS(tiles_path)

n_tiles <- if (inherits(final_tiles, "sf")) nrow(final_tiles) else if (is.list(final_tiles)) length(final_tiles) else stop("final_tiles must be an sf or a list.")
cat("Total tiles:", n_tiles, "\n")

# -------- Main loop --------
# It loops over every tile, builds a WGS84 bbox, and creates an Overpass query.
# For each tile it downloads restaurants (points + centroids of area features) and highways (lines/multilines),
# combines them robustly, cleans column names, logs success/errors, and pauses before the next tile
# skipping tiles whose output file already exists.

for (i in seq_len(n_tiles)) {
  tile_obj <- if (inherits(final_tiles, "sf")) final_tiles[i, , drop = FALSE] else final_tiles[[i]]
  
  # BBOX (WGS84)
  bbox_vec <- try(get_bbox_from_tile(tile_obj), silent = TRUE)
  if (inherits(bbox_vec, "try-error")) {
    msg <- as.character(bbox_vec)
    append_progress(i, "ALL", "error", paste0("bbox extraction failed: ", msg))
    cat(sprintf("\nTile %d/%d -> ERROR bbox extraction: %s\n", i, n_tiles, msg))
    next
  }
  
  cat(sprintf("\nTile %d/%d | bbox: [%.6f, %.6f, %.6f, %.6f]\n",
              i, n_tiles, bbox_vec[1], bbox_vec[2], bbox_vec[3], bbox_vec[4]))
  
  q_base <- try(opq(bbox = bbox_vec, timeout = opq_timeout_sec), silent = TRUE)
  if (inherits(q_base, "try-error")) {
    msg <- as.character(q_base)
    append_progress(i, "ALL", "error", paste0("opq init failed: ", msg))
    cat("  ERROR: opq init failed -> ", msg, "\n")
    next
  }
  
# ---------------- Restaurants ----------------
  {
    out_path <- file.path(feature_dirs$restaurants, sprintf("tile_%03d.gpkg", i))
    if (file.exists(out_path)) {
      cat("  restaurants -> already exists, skipping\n")
    } else {
      cat("  restaurants -> Querying OSM…\n")
      res_sf <- try({
        q <- q_base %>% add_osm_feature(key = "amenity", value = "restaurant")
        osm <- osmdata_sf(q)
        
        pts    <- osm$osm_points
        polys  <- osm$osm_polygons
        mpolys <- osm$osm_multipolygons
        
        pts_ok     <- has_rows(pts)
        poly_pts   <- if (has_rows(polys))  safe_centroid(polys)  else NULL
        mpoly_pts  <- if (has_rows(mpolys)) safe_centroid(mpolys) else NULL
        
        pieces <- list(
          if (pts_ok) pts else NULL,
          poly_pts,
          mpoly_pts
        )
        pieces <- pieces[!vapply(pieces, is.null, logical(1))]
        
        if (length(pieces) == 0) {
          append_progress(i, "restaurants", "empty", "No features in this tile")
          cat("    restaurants -> no features; logged as empty\n")
          NULL
        } else {
          dat <- try(suppressWarnings(do.call(rbind, pieces)), silent = TRUE)
          if (inherits(dat, "try-error")) {
            dat <- suppressWarnings(dplyr::bind_rows(pieces))
            dat <- sf::st_as_sf(dat)  
          }
          
          dat <- dplyr::mutate(dat, tile_index = i, feature_tag = "restaurants")
          dat <- prep_for_write(dat)
          sf::st_write(dat, dsn = out_path, layer = "data", delete_dsn = TRUE, quiet = TRUE)
          append_progress(i, "restaurants", "done")
          cat(sprintf("    restaurants -> Saved %s (n=%s)\n", basename(out_path), nrow(dat)))
        }
      }, silent = TRUE)
      
      if (inherits(res_sf, "try-error")) {
        msg <- as.character(res_sf)
        append_progress(i, "restaurants", "error", msg)
        cat("    ERROR (restaurants): ", msg, "\n")
      }
      
      Sys.sleep(sleep_between_calls)
    }
  }
  
  # ---------------- Highways ----------------
  {
    out_path <- file.path(feature_dirs$highways, sprintf("tile_%03d.gpkg", i))
    if (file.exists(out_path)) {
      cat("  highways -> already exists, skipping\n")
    } else {
      cat("  highways -> Querying OSM…\n")
      res_sf <- try({
        q <- q_base %>% add_osm_feature(key = "highway")
        osm <- osmdata_sf(q)
        
        ln  <- osm$osm_lines
        mln <- osm$osm_multilines
        
        ln_ok  <- has_rows(ln)
        mln_ok <- has_rows(mln)
        
        if (!ln_ok && !mln_ok) {
          append_progress(i, "highways", "empty", "No features in this tile")
          cat("    highways -> no features; logged as empty\n")
          NULL
        } else {
          bind_ok <- TRUE
          dat <- NULL
          if (ln_ok && mln_ok) {
            dat <- try(suppressWarnings(do.call(rbind, list(
              dplyr::mutate(ln,  tile_index = i, feature_tag = "highways"),
              dplyr::mutate(mln, tile_index = i, feature_tag = "highways")
            ))), silent = TRUE)
            if (inherits(dat, "try-error")) {
              dat <- try(suppressWarnings(dplyr::bind_rows(
                dplyr::mutate(ln,  tile_index = i, feature_tag = "highways"),
                dplyr::mutate(mln, tile_index = i, feature_tag = "highways")
              )), silent = TRUE)
              if (inherits(dat, "try-error")) bind_ok <- FALSE else dat <- sf::st_as_sf(dat)
            }
          }
          
          if (bind_ok && (ln_ok || mln_ok)) {
            if (is.null(dat)) {
              dat <- if (ln_ok) dplyr::mutate(ln,  tile_index = i, feature_tag = "highways")
              else        dplyr::mutate(mln, tile_index = i, feature_tag = "highways")
            }
            dat <- prep_for_write(dat)
            sf::st_write(dat, dsn = out_path, layer = "data", delete_dsn = TRUE, quiet = TRUE)
            append_progress(i, "highways", "done")
            cat(sprintf("    highways -> Saved %s (n=%s)\n", basename(out_path), nrow(dat)))
          } else {
            wrote_any <- FALSE
            if (ln_ok) {
              ln2 <- dplyr::mutate(ln, tile_index = i, feature_tag = "highways_lines")
              ln2 <- prep_for_write(ln2)
              sf::st_write(ln2, dsn = out_path, layer = "lines", delete_dsn = TRUE, quiet = TRUE)
              wrote_any <- TRUE
            }
            if (mln_ok) {
              mln2 <- dplyr::mutate(mln, tile_index = i, feature_tag = "highways_multilines")
              mln2 <- prep_for_write(mln2)
              sf::st_write(mln2, dsn = out_path, layer = "multilines",
                           delete_layer = TRUE, quiet = TRUE)
              wrote_any <- TRUE
            }
            if (wrote_any) {
              append_progress(i, "highways", "done", "Wrote separate layers (lines/multilines)")
              cat(sprintf("    highways -> Saved %s as separate layers%s%s\n",
                          basename(out_path),
                          if (ln_ok) " [lines]" else "",
                          if (mln_ok) " [multilines]" else ""))
            } else {
              append_progress(i, "highways", "empty", "No valid sf layers after prep")
              cat("    highways -> no valid layers after prep; logged as empty\n")
            }
          }
        }
      }, silent = TRUE)
      
      if (inherits(res_sf, "try-error")) {
        msg <- as.character(res_sf)
        append_progress(i, "highways", "error", msg)
        cat("    ERROR (highways): ", msg, "\n")
      }
      
      Sys.sleep(sleep_between_calls)
    }
  }
}  

cat("\nAll done. Progress log at:", progress_csv, "\n")
