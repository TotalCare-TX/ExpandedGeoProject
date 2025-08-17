# ============================
# OSM Downloader
# ============================

options(stringsAsFactors = FALSE)
suppressPackageStartupMessages({
  library(sf)
  library(osmdata)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

# ------------ Config------------
OUTPUT_LONLAT_EPSG <- 4326   
OSM_TIMEOUT_SEC    <- 120


features <- list(
  restaurant = list(
    key                = "amenity",
    values             = "restaurant",
    tag                = "restaurants",
    target_geom        = "POINT",
    include_multilines = FALSE,
    centroid_polys     = TRUE,
    simplify           = FALSE,
    tol                = 0,
    tol_units          = "deg",
    projected_crs      = NA_integer_,
    keep_attrs         = character(0)       
  ),
  highway = list(
    key                = "highway",
    values             = c("motorway","trunk","primary"),
    tag                = "highways_major",
    target_geom        = "LINESTRING",
    include_multilines = FALSE,               
    simplify           = TRUE,
    tol                = 0.001,               
    tol_units          = "deg",               
    projected_crs      = NA_integer_,         
    keep_attrs         = character(0)         
  )
 
)

# ------------ Paths & Setup ------------
tiles_path   <- "final_tiles.rds"
output_root  <- "tile_downloads"
progress_csv <- "download_progress.csv"
dir.create(output_root, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(tiles_path)) stop("final_tiles.rds not found.")
final_tiles <- readRDS(tiles_path)
if (!is.list(final_tiles)) stop("final_tiles must be a list.")
stopifnot(all(c("geometry","buffer","status") %in% names(final_tiles[[1]])))

# ------------ Helpers------------
# Return bbox in OUTPUT_LONLAT_EPSG as numeric c(xmin, ymin, xmax, ymax)
tile_bbox_wgs84 <- function(sfc_geom) {
  if (!inherits(sfc_geom, "sfc")) stop("geometry must be an sfc object")
  if (is.na(st_crs(sfc_geom)))   stop("Tile geometry has no CRS.")
  g84 <- suppressWarnings(st_transform(sfc_geom, OUTPUT_LONLAT_EPSG))
  as.numeric(st_bbox(g84))
}
# It filters a list by removing elements that are NULL or sf objects with zero rows, returning only the non-empty items
.compact <- function(x) x[!vapply(x, function(z) is.null(z) || (inherits(z,"sf") && nrow(z)==0), logical(1))]


# This function It safely turns restaurant polygons into single points
# try a centroid, if that fails, use a point on the surface; if shapes are invalid, fix them and retry so you always get usable points without errors

safe_centroid <- function(x) {
  if (is.null(x) || !inherits(x, "sf") || nrow(x) == 0) return(NULL)
  out <- try(sf::st_centroid(x), silent = TRUE); if (!inherits(out, "try-error")) return(out)
  out <- try(sf::st_point_on_surface(x), silent = TRUE); if (!inherits(out, "try-error")) return(out)
  xv  <- try(suppressWarnings(sf::st_make_valid(x)), silent = TRUE)
  if (!inherits(xv, "try-error")) {
    out <- try(sf::st_centroid(xv), silent = TRUE); if (!inherits(out, "try-error")) return(out)
  }
  NULL
}

# ensures the geometry column is named geometry and strips any dimensions (keeps 2D only), so downstream code can assume a consistent 2D geometry column.

normalize_geom <- function(x) {
  if (!inherits(x, "sf")) return(x)
  geoname <- attr(x, "sf_column")
  if (!identical(geoname, "geometry")) {
    names(x)[names(x) == geoname] <- "geometry"
    sf::st_geometry(x) <- "geometry"
  }
  x$geometry <- sf::st_zm(x$geometry, drop = TRUE, what = "ZM")
  x
}

# It merges a bunch of sf layers that don’t share the exact same columns into one clean layer.
bind_schema_safe <- function(lst) {
  lst <- .compact(lst)
  if (!length(lst)) return(NULL)
  lst <- lapply(lst, normalize_geom)
  cols_union <- Reduce(union, lapply(lst, function(x) setdiff(names(x), "geometry")))
  lst <- lapply(lst, function(x) {
    miss <- setdiff(cols_union, setdiff(names(x), "geometry"))
    for (m in miss) x[[m]] <- NA
    x[, c(cols_union, "geometry"), drop = FALSE]
  })
  out <- try(suppressWarnings(do.call(rbind, lst)), silent = TRUE)
  if (inherits(out, "try-error")) {
    tmp <- suppressWarnings(dplyr::bind_rows(lst))
    out <- try(sf::st_as_sf(tmp), silent = TRUE)
    if (inherits(out, "try-error")) return(NULL)
  }
  out
}

# It guarantees your layer is in WGS84 lon/lat
ensure_lonlat <- function(x) {
  if (is.na(sf::st_crs(x))) sf::st_crs(x) <- OUTPUT_LONLAT_EPSG
  if (!sf::st_is_longlat(x)) x <- sf::st_transform(x, OUTPUT_LONLAT_EPSG)
  x
}

simplify_feature <- function(x, simplify, tol, units, projected_crs) {
  if (!simplify || tol <= 0) return(x)
  if (units == "deg") {
    x <- ensure_lonlat(x)
    return(suppressWarnings(sf::st_simplify(x, dTolerance = tol, preserveTopology = TRUE)))
  } else if (units == "m") {
    if (is.na(projected_crs)) stop("Simplification in meters requires 'projected_crs'.")
    x <- suppressWarnings(sf::st_transform(x, projected_crs))
    x <- suppressWarnings(sf::st_simplify(x, dTolerance = tol, preserveTopology = TRUE))
    x <- suppressWarnings(sf::st_transform(x, OUTPUT_LONLAT_EPSG))
    return(x)
  }
  stop("Unknown tolerance units; use 'deg' or 'm'.")
}

count_osm_features <- function(osm) {
  as.integer(
    (if (!is.null(osm$osm_points))        nrow(osm$osm_points)        else 0L) +
      (if (!is.null(osm$osm_lines))         nrow(osm$osm_lines)         else 0L) +
      (if (!is.null(osm$osm_multilines))    nrow(osm$osm_multilines)    else 0L) +
      (if (!is.null(osm$osm_polygons))      nrow(osm$osm_polygons)      else 0L) +
      (if (!is.null(osm$osm_multipolygons)) nrow(osm$osm_multipolygons) else 0L)
  )
}

count_osm_features_list <- function(res_list) {
  if (!length(res_list)) return(0L)
  totals <- vapply(res_list, function(osm) {
    as.integer(
      (if (!is.null(osm$osm_points))        nrow(osm$osm_points)        else 0L) +
        (if (!is.null(osm$osm_lines))         nrow(osm$osm_lines)         else 0L) +
        (if (!is.null(osm$osm_multilines))    nrow(osm$osm_multilines)    else 0L) +
        (if (!is.null(osm$osm_polygons))      nrow(osm$osm_polygons)      else 0L) +
        (if (!is.null(osm$osm_multipolygons)) nrow(osm$osm_multipolygons) else 0L)
    )
  }, integer(1))
  sum(totals, na.rm = TRUE)
}

# ------------ Binder ------------
bind_feature <- function(res_list, spec) {
  tgt <- toupper(spec$target_geom)
  if (tgt == "POINT") {
    parts <- list()
    for (r in res_list) {
      if (inherits(r$osm_points, "sf") && nrow(r$osm_points) > 0)
        parts <- c(parts, list(r$osm_points))
      if (isTRUE(spec$centroid_polys)) {
        if (inherits(r$osm_polygons, "sf") && nrow(r$osm_polygons) > 0)
          parts <- c(parts, list(safe_centroid(r$osm_polygons)))
        if (inherits(r$osm_multipolygons, "sf") && nrow(r$osm_multipolygons) > 0)
          parts <- c(parts, list(safe_centroid(r$osm_multipolygons)))
      }
    }
    out <- bind_schema_safe(parts)
    if (is.null(out)) return(NULL)
    try_out <- try(suppressWarnings(sf::st_cast(out, "POINT")), silent = TRUE)
    if (!inherits(try_out, "try-error")) out <- try_out
    return(out)
  }
  
  if (tgt == "LINESTRING") {
    parts <- list()
    for (r in res_list) {
      if (inherits(r$osm_lines, "sf") && nrow(r$osm_lines) > 0)
        parts <- c(parts, list(r$osm_lines))
      if (isTRUE(spec$include_multilines) &&
          inherits(r$osm_multilines, "sf") && nrow(r$osm_multilines) > 0)
        parts <- c(parts, list(r$osm_multilines))
    }
    out <- bind_schema_safe(parts)
    if (is.null(out)) return(NULL)
    gtypes <- unique(as.character(sf::st_geometry_type(out, by_geometry = TRUE)))
    if (all(gtypes %in% c("LINESTRING","MULTILINESTRING","GEOMETRY"))) {
      try_out <- try(suppressWarnings(sf::st_cast(out, "LINESTRING")), silent = TRUE)
      if (inherits(try_out, "try-error")) {
        try_out <- try(suppressWarnings(sf::st_line_merge(out)), silent = TRUE)
      }
      if (!inherits(try_out, "try-error")) out <- try_out
    }
    return(out)
  }
  
  if (tgt == "POLYGON") {
    parts <- list()
    for (r in res_list) {
      if (inherits(r$osm_polygons, "sf") && nrow(r$osm_polygons) > 0)
        parts <- c(parts, list(r$osm_polygons))
      if (inherits(r$osm_multipolygons, "sf") && nrow(r$osm_multipolygons) > 0)
        parts <- c(parts, list(r$osm_multipolygons))
    }
    out <- bind_schema_safe(parts)
    if (is.null(out)) return(NULL)
    # cast to POLYGON (best-effort)
    try_out <- try(suppressWarnings(sf::st_cast(out, "POLYGON")), silent = TRUE)
    if (!inherits(try_out, "try-error")) out <- try_out
    return(out)
  }
  
  stop("Unsupported target_geom: ", spec$target_geom)
}

# ------------ Writer ------------
write_feature_rds <- function(res_list, spec, tile_stub, tile_idx) {
  files_written <- character(0)
  out <- bind_feature(res_list, spec)
  if (is.null(out) || !inherits(out, "sf") || nrow(out) == 0) return(files_written)
  
  if (!is.null(spec$values) && length(spec$values) > 0 && spec$key %in% names(out)) {
    out <- dplyr::filter(out, .data[[spec$key]] %in% spec$values)
    if (nrow(out) == 0) return(files_written)
  }
  
  out <- ensure_lonlat(out)
  out <- simplify_feature(out, spec$simplify, spec$tol, spec$tol_units, spec$projected_crs)
  
  if (length(spec$keep_attrs) == 0) {
    out <- out["geometry"]
  } else {
    keep <- intersect(c(spec$keep_attrs, "geometry"), names(out))
    out <- out[, keep, drop = FALSE]
  }
  
  out$tile_index  <- tile_idx
  out$feature_tag <- spec$tag
  
  out_dir <- file.path(output_root, spec$tag)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  f <- paste0(tile_stub, "_", spec$tag, ".rds")
  saveRDS(out, f)
  files_written <- c(files_written, f)
  files_written
}

# ------------ Progress Log ------------
if (file.exists(progress_csv)) {
  progress <- readr::read_csv(
    progress_csv,
    col_types = readr::cols(
      tile        = readr::col_integer(),
      feature     = readr::col_character(),
      tag         = readr::col_character(),
      started_at  = readr::col_character(),
      finished_at = readr::col_character(),
      status      = readr::col_character(),
      n_features  = readr::col_integer(),
      file_path   = readr::col_character(),
      error_msg   = readr::col_character()
    ),
    show_col_types = FALSE
  )
} else {
  progress <- tibble::tibble(
    tile        = integer(),
    feature     = character(),
    tag         = character(),
    started_at  = character(),
    finished_at = character(),
    status      = character(),
    n_features  = integer(),
    file_path   = character(),
    error_msg   = character()
  )
}
save_progress <- function(df) write_csv(df, progress_csv)

is_done <- function(tile_idx, feature_name) {
  any(progress$tile == tile_idx &
        progress$feature == feature_name &
        progress$status %in% c("Success", "Empty"))
}

first_incomplete_tile <- function(progress, total_tiles, features) {
  if (!nrow(progress)) return(1L)
  need <- length(features)
  done <- progress |>
    dplyr::filter(status %in% c("Success","Empty")) |>
    dplyr::distinct(tile, feature) |>
    dplyr::count(tile, name = "ok")
  complete_tiles <- done$tile[done$ok >= need]
  candidates <- setdiff(seq_len(total_tiles), complete_tiles)
  if (length(candidates) == 0) 1L else min(candidates)
}

# ------------ Main Loop ------------
total_tiles <- length(final_tiles)
start_tile  <- first_incomplete_tile(progress, total_tiles, features)
cat(sprintf("Starting download for %d tiles and %d features. Resuming at tile %d.\n",
            total_tiles, length(features), start_tile))

for (tile_idx in seq(from = start_tile, to = total_tiles)) {
  tile_obj <- final_tiles[[tile_idx]]
  if (!inherits(tile_obj$geometry, "sfc")) {
    warning(sprintf("Tile %d has no valid sfc geometry. Skipping.", tile_idx)); next
  }
  bbox_vec <- try(tile_bbox_wgs84(tile_obj$geometry), silent = TRUE)
  if (inherits(bbox_vec, "try-error")) {
    warning(sprintf("Tile %d: could not build WGS84 bbox. Skipping.", tile_idx)); next
  }
  
  for (fname in names(features)) {
    spec <- features[[fname]]
    if (is_done(tile_idx, fname)) {
      cat(sprintf("Tile %d | %-14s -> already done, skipping.\n", tile_idx, fname)); next
    }
    
    feat_dir  <- file.path(output_root, spec$tag)
    dir.create(feat_dir, showWarnings = FALSE, recursive = TRUE)
    tile_stub <- file.path(feat_dir, sprintf("tile_%03d", tile_idx))
    
    row_stub <- tibble(
      tile        = tile_idx,
      feature     = fname,
      tag         = spec$tag,
      started_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
      finished_at = NA_character_,
      status      = NA_character_,
      n_features  = NA_integer_,
      file_path   = paste0(tile_stub, "_", spec$tag, ".rds"),
      error_msg   = NA_character_
    )
    progress <- bind_rows(progress, row_stub); save_progress(progress)
    
    cat(sprintf("Tile %d/%d | Feature: %s | Querying OSM…\n",
                tile_idx, total_tiles, fname))
    
    q <- opq(bbox = bbox_vec, timeout = OSM_TIMEOUT_SEC)
    
    res_list <- list()
    if (length(spec$values) <= 1 || (length(spec$values) == 1 && is.na(spec$values[1]))) {
      q1 <- if (length(spec$values) == 0 || is.na(spec$values[1]))
        add_osm_feature(q, key = spec$key)
      else
        add_osm_feature(q, key = spec$key, value = spec$values)
      r1 <- try(osmdata_sf(q1), silent = TRUE)
      if (!inherits(r1, "try-error")) res_list <- list(r1) else res_list <- list()
    } else {
      for (v in spec$values) {
        qv <- add_osm_feature(q, key = spec$key, value = v)
        rv <- try(osmdata_sf(qv), silent = TRUE)
        if (!inherits(rv, "try-error")) res_list <- append(res_list, list(rv))
      }
    }
    
    n_total <- count_osm_features_list(res_list)
    if (n_total == 0) {
      cat(sprintf("Tile %d | %s -> Empty result.\n", tile_idx, fname))
      sel <- which(progress$tile == tile_idx & progress$feature == fname & is.na(progress$finished_at)); if (length(sel)) sel <- tail(sel, 1)
      progress$status[sel]      <- "Empty"
      progress$n_features[sel]  <- 0L
      progress$finished_at[sel] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
      save_progress(progress)
      Sys.sleep(runif(1, 0.8, 1.6))
      next
    }
    
    files <- character(0)
    err   <- NULL
    files <- try(write_feature_rds(res_list, spec, tile_stub, tile_idx), silent = TRUE)
    if (inherits(files, "try-error")) err <- as.character(files)
    
    if (!is.null(err)) {
      cat(sprintf("Tile %d | %s -> ERROR: %s\n", tile_idx, fname, err))
      sel <- which(progress$tile == tile_idx & progress$feature == fname & is.na(progress$finished_at)); if (length(sel)) sel <- tail(sel, 1)
      progress$status[sel]      <- "Error"
      progress$error_msg[sel]   <- str_trunc(err, 300)
      progress$finished_at[sel] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
      save_progress(progress)
      next
    }
    
    cat(sprintf("Tile %d | %s -> Saved %d features into %s (%d file%s)\n",
                tile_idx, fname, n_total, paste0(tile_stub, "_", spec$tag, ".rds"),
                length(files), ifelse(length(files)==1,"","s")))
    
    sel <- which(progress$tile == tile_idx & progress$feature == fname & is.na(progress$finished_at)); if (length(sel)) sel <- tail(sel, 1)
    progress$status[sel]      <- "Success"
    progress$n_features[sel]  <- n_total
    progress$finished_at[sel] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
    save_progress(progress)
    
    Sys.sleep(runif(1, 0.8, 1.6))  # polite jitter
  }
}

cat("All done.\n")

