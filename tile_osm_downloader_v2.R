# =========================================================
# OSM Downloader (Modular & Documented)
# =========================================================
# NOTE: All library() calls are intentionally commented out.
# Load them in a separate bootstrap script before sourcing this file.
#
# library(sf)
# library(osmdata)
# library(dplyr)
# library(readr)
# library(stringr)
# library(purrr)
# library(tibble)
# library(gdalUtilities)
# library(matrixStats)

options(stringsAsFactors = FALSE)

# ---------------------------------------------------------
#  Feature Spec Template (COPY & EDIT THIS FOR YOUR FEATURES)
# ---------------------------------------------------------
# Each feature you want to download is a named list with the fields below.
#
# my_features <- list(
#   restaurants = list(
#     key                = "amenity",          # OSM key to filter by (e.g., amenity, highway, landuse)
#     values             = "restaurant",       # Character vector of one or more OSM values for that key
#     tag                = "restaurants",      # Short tag used in output folder & filenames
#     target_geom        = "POINT",            # Desired output geometry: "POINT", "LINESTRING", or "POLYGON"
#     include_multilines = FALSE,              # If TRUE, also bind 'osm_multilines' for line features
#     centroid_polys     = TRUE,               # If TRUE and target is POINT, convert polygons to centroids/surface points
#     simplify           = FALSE,              # If TRUE, simplify geometry (topology-preserving)
#     tol                = 0,                  # Simplification tolerance. Units depend on 'tol_units'
#     tol_units          = "deg",              # "deg" (degrees in EPSG:4326) or "m" (meters; requires 'projected_crs')
#     projected_crs      = NA_integer_,        # EPSG code used ONLY if tol_units == "m" (e.g., 3083 for TX meters)
#     keep_attrs         = character(0)        # Character vector of attribute names to retain (empty = geometry only)
#   ),
#   highways_major = list(
#     key                = "highway",
#     values             = c("motorway", "trunk", "primary"),
#     tag                = "highways_major",
#     target_geom        = "LINESTRING",
#     include_multilines = FALSE,
#     centroid_polys     = FALSE,
#     simplify           = TRUE,
#     tol                = 0.001,
#     tol_units          = "deg",
#     projected_crs      = NA_integer_,
#     keep_attrs         = character(0)
#   )
# )
#
# Hyperparameter meanings:
# - key / values: Passed to Overpass via osmdata; restricts features by OSM tag.
# - tag: Used to name output subdirectories and files (organizational only).
# - target_geom: Forces output to a specific geom class; polygons can be cast to POINT via centroid if requested.
# - include_multilines: Adds 'osm_multilines' for LINESTRING targets if TRUE.
# - centroid_polys: For POINT targets, polygons/multipolygons get turned into centroids/surface points.
# - simplify: Toggle topology-preserving simplification; helps reduce file size.
# - tol: Simplification tolerance. Pair with tol_units.
# - tol_units: "deg" (simplify in EPSG:4326) or "m" (simplify after reprojecting to 'projected_crs').
# - projected_crs: EPSG integer required when tol_units == "m" (e.g., 3083 for Texas meters).
# - keep_attrs: Keep only these attributes (plus geometry). Empty = drop attributes (geometry only).

# ---------------------------------------------------------
#  Generate feature lists - allows us to generate features with defaults assigned
# ---------------------------------------------------------

generate_feature_lists <- function(key,
                                   values = NA,
                                   tag,
                                   target_geom = "POINT",
                                   include_multilines = FALSE,
                                   centroid_polys = TRUE,
                                   simplify = TRUE,
                                   tol = 0.001,
                                   tol_units = "deg",
                                   projected_crs = NA_integer_,
                                   keep_attrs = character(0)) {
    
    list(key = key, values = values, tag = tag, target_geom = target_geom, include_multilines = include_multilines, centroid_polys = centroid_polys, simplify = simplify, tol = tol, tol_units = tol_units, projected_crs = projected_crs, keep_attrs = keep_attrs)
    
}

generate_current_feature_list <- function() {
    
    restaurant <- generate_feature_lists(key = "amenity", values = "restaurant", tag = "restaurant")
    school <- generate_feature_lists(key = "amenity", values = "school", tag = "school")
    pharmacy <- generate_feature_lists(key = "amenity", values = "pharmacy", tag = "pharmacy")
    library <- generate_feature_lists(key = "amenity", values = "library", tag = "library")
    shop <- generate_feature_lists(key = "shop", values = NULL, tag = "shop")
    pawnbroker <- generate_feature_lists(key = "shop", values = "pawnbroker", tag = "pawnbroker")
    gold_buyer <- generate_feature_lists(key = "shop", values = "gold_buyer", tag = "gold_buyer")
    supermarket <- generate_feature_lists(key = "shop", values = "supermarket", tag = "supermarket")
    traffic_signals <- generate_feature_lists(key = "highway", values = "traffic_signals", tag = "traffic_signals")
    highway_major <- generate_feature_lists(key = "highway", values = c("motorway", "trunk", "primary"), tag = "highway_major")
    
    return(list(restaurant, school, pharmacy, library, shop, pawnbroker, gold_buyer, supermarket, traffic_signals, highway_major))
    
}

# ---------------------------------------------------------
#  Configuration (defaults; override via run_osm_downloader args)
# ---------------------------------------------------------
DEFAULT_OUTPUT_LONLAT_EPSG <- 4326      # Force outputs to WGS84 lon/lat
DEFAULT_OSM_TIMEOUT_SEC    <- 120       # Overpass timeout (seconds)

# ---------------------------------------------------------
#  Helper: build WGS84 bbox for a tile geometry
# ---------------------------------------------------------
# Returns numeric c(xmin, ymin, xmax, ymax) in EPSG:4326.
tile_bbox_wgs84 <- function(sfc_geom, out_epsg = DEFAULT_OUTPUT_LONLAT_EPSG) {
    if (!inherits(sfc_geom, "sfc")) stop("geometry must be an 'sfc' object.")
    if (is.na(sf::st_crs(sfc_geom))) stop("Tile geometry has no CRS.")
    g84 <- suppressWarnings(sf::st_transform(sfc_geom, out_epsg))
    as.numeric(sf::st_bbox(g84))
}

# ---------------------------------------------------------
#  Helper: compact a list (drop NULLs and empty sf layers)
# ---------------------------------------------------------
.compact <- function(x) {
    x[!vapply(
        x,
        function(z) is.null(z) || (inherits(z, "sf") && nrow(z) == 0),
        logical(1)
    )]
}

# ---------------------------------------------------------
#  Helper: robust polygon → point conversion for POINT targets
# ---------------------------------------------------------
# Tries st_centroid, then st_point_on_surface; makes geometries valid if needed.
safe_centroid <- function(x) {
    if (is.null(x) || !inherits(x, "sf") || nrow(x) == 0) return(NULL)
    out <- try(sf::st_centroid(x), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    out <- try(sf::st_point_on_surface(x), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
    xv  <- try(suppressWarnings(sf::st_make_valid(x)), silent = TRUE)
    if (!inherits(xv, "try-error")) {
        out <- try(sf::st_centroid(xv), silent = TRUE)
        if (!inherits(out, "try-error")) return(out)
    }
    NULL
}

# ---------------------------------------------------------
#  Helper: standardize geometry column name + drop Z/M dims
# ---------------------------------------------------------
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

# ---------------------------------------------------------
#  Helper: bind a list of sf layers with different schemas
# ---------------------------------------------------------
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

# ---------------------------------------------------------
#  Helper: ensure layer is in lon/lat EPSG:4326
# ---------------------------------------------------------
ensure_lonlat <- function(x, out_epsg = DEFAULT_OUTPUT_LONLAT_EPSG) {
    if (is.na(sf::st_crs(x))) sf::st_crs(x) <- out_epsg
    if (!sf::st_is_longlat(x)) x <- sf::st_transform(x, out_epsg)
    x
}

# ---------------------------------------------------------
#  Helper: optional topology-preserving simplify
# ---------------------------------------------------------
# - If units == "deg": simplify in EPSG:4326 directly.
# - If units == "m": reproject to 'projected_crs', simplify, then back to EPSG:4326.
simplify_feature <- function(x, simplify, tol, units, projected_crs, out_epsg = DEFAULT_OUTPUT_LONLAT_EPSG) {
    if (!simplify || tol <= 0) return(x)
    
    if (identical(units, "deg")) {
        x <- ensure_lonlat(x, out_epsg)
        return(suppressWarnings(sf::st_simplify(x, dTolerance = tol, preserveTopology = TRUE)))
    }
    
    if (identical(units, "m")) {
        if (is.na(projected_crs)) stop("Simplification in meters requires 'projected_crs'.")
        x <- suppressWarnings(sf::st_transform(x, projected_crs))
        x <- suppressWarnings(sf::st_simplify(x, dTolerance = tol, preserveTopology = TRUE))
        x <- suppressWarnings(sf::st_transform(x, out_epsg))
        return(x)
    }
    
    stop("Unknown tolerance units; use 'deg' or 'm'.")
}

# ---------------------------------------------------------
#  Helper: count rows across osmdata sf slots
# ---------------------------------------------------------
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
    totals <- vapply(res_list, count_osm_features, integer(1))
    sum(totals, na.rm = TRUE)
}

# ---------------------------------------------------------
#  Binder: convert raw osmdata results to a single sf layer
# ---------------------------------------------------------
bind_feature <- function(res_list, spec) {
    tgt <- toupper(spec$target_geom)
    
    if (tgt == "POINT") {
        parts <- list()
        
        for (r in res_list) {
            if (inherits(r$osm_points, "sf") && nrow(r$osm_points) > 0) {
                parts <- c(parts, list(r$osm_points))
            }
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
        if (all(gtypes %in% c("LINESTRING", "MULTILINESTRING", "GEOMETRY"))) {
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
        
        try_out <- try(suppressWarnings(sf::st_cast(out, "POLYGON")), silent = TRUE)
        if (!inherits(try_out, "try-error")) out <- try_out
        
        return(out)
    }
    
    stop("Unsupported target_geom: ", spec$target_geom)
}

# ---------------------------------------------------------
#  Writer: normalize, simplify, filter attrs, save RDS
# ---------------------------------------------------------
write_feature_rds <- function(res_list, spec, tile_stub, tile_idx, output_root,
                              out_epsg = DEFAULT_OUTPUT_LONLAT_EPSG) {
    files_written <- character(0)
    
    out <- bind_feature(res_list, spec)
    if (is.null(out) || !inherits(out, "sf") || nrow(out) == 0) return(files_written)
    
    # Defensive: if the expected 'key' exists, filter to allowed values
    # We will not implement this for now - this filters out too many variables
    #if (!is.null(spec$values) && length(spec$values) > 0 && spec$key %in% names(out)) {
        #out <- dplyr::filter(out, .data[[spec$key]] %in% spec$values)
        #if (nrow(out) == 0) return(files_written)
    #}
    
    # Force lon/lat, then optionally simplify
    out <- ensure_lonlat(out, out_epsg)
    out <- simplify_feature(out, spec$simplify, spec$tol, spec$tol_units, spec$projected_crs, out_epsg)
    
    # Keep only desired attributes (or geometry only)
    if (length(spec$keep_attrs) == 0) {
        out <- out["geometry"]
    } else {
        keep <- intersect(c(spec$keep_attrs, "geometry"), names(out))
        out  <- out[, keep, drop = FALSE]
    }
    
    # Add bookkeeping columns
    out$tile_index  <- tile_idx
    out$feature_tag <- spec$tag
    
    # Write RDS
    out_dir <- file.path(output_root, spec$tag)
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    
    f <- file.path(out_dir, paste0(basename(tile_stub), "_", spec$tag, ".rds"))
    saveRDS(out, f)
    
    files_written <- c(files_written, f)
    files_written
}

# ---------------------------------------------------------
#  Progress log helpers (CSV-based, resumable)
# ---------------------------------------------------------
load_or_init_progress <- function(progress_csv) {
    if (file.exists(progress_csv)) {
        progress <- readr::read_csv(
            progress_csv,
            col_types = readr::cols(
                tile        = readr::col_integer(),
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
            tag         = character(),
            started_at  = character(),
            finished_at = character(),
            status      = character(),
            n_features  = integer(),
            file_path   = character(),
            error_msg   = character()
        )
        
        write_csv(progress, progress_csv)
    }
    
    return(progress)
}

save_progress <- function(df, progress_csv) {
    readr::write_csv(df, progress_csv)
}

is_done <- function(progress, tile_idx, tag) {
    sel <- which(progress$tile == tile_idx &
                     progress$tag == tag &
                     progress$status == "Success")
    
    if(length(sel) == 0) {
        
        FALSE
        
    } else {
        
        TRUE
        
    }
    
}

is_empty <- function(progress, tile_idx, tag) {
    sel <- which(progress$tile == tile_idx &
                     progress$tag == tag &
                     progress$status == "Empty")
    
    if(length(sel) == 0) {
        
        FALSE
        
    } else {
        
        TRUE
        
    }
    
}

first_incomplete_tile <- function(progress, total_tiles, features) {
    if (!nrow(progress)) return(1L)
    need <- length(features)
    done <- progress |>
        dplyr::filter(status %in% c("Success", "Empty")) |>
        dplyr::distinct(tile, feature) |>
        dplyr::count(tile, name = "ok")
    complete_tiles <- done$tile[done$ok >= need]
    candidates <- setdiff(seq_len(total_tiles), complete_tiles)
    if (length(candidates) == 0) 1L else min(candidates)
}

# ---------------------------------------------------------
#  Overpass query executor for a single feature + bbox
# ---------------------------------------------------------
query_overpass_feature <- function(spec, bbox_vec, timeout_sec) {
    q <- osmdata::opq(bbox = bbox_vec, timeout = timeout_sec)
    
    res_list <- list()
    
    if (length(spec$values) <= 1 || (length(spec$values) == 1 && is.na(spec$values[1]))) {
        q1 <- if (length(spec$values) == 0 || is.na(spec$values[1])) {
            osmdata::add_osm_feature(q, key = spec$key)
        } else {
            osmdata::add_osm_feature(q, key = spec$key, value = spec$values)
        }
        r1 <- try(osmdata::osmdata_sf(q1), silent = TRUE)
        if (!inherits(r1, "try-error")) res_list <- list(r1)
    } else {
        for (v in spec$values) {
            qv <- osmdata::add_osm_feature(q, key = spec$key, value = v)
            rv <- try(osmdata::osmdata_sf(qv), silent = TRUE)
            if (!inherits(rv, "try-error")) res_list <- append(res_list, list(rv))
        }
    }
    
    res_list
}

# =========================================================
#  MAIN: Run the downloader for a set of tiles & features
# =========================================================
# Arguments:
# - final_tiles_path: RDS path to your tiles (list). Each element must contain:
#       $geometry (sfc), $buffer (numeric), $status (any).
# - feature_specs    : named list of feature specs (see template at top).
# - output_root      : folder where per-feature subfolders are written.
# - progress_csv     : CSV log file to support resume.
# - osm_timeout_sec  : Overpass timeout per request (seconds).
# - output_epsg      : EPSG for final outputs (default WGS84 4326).
run_osm_downloader <- function(
        final_tiles_path,
        feature_specs,
        output_root   = "tile_downloads",
        progress_csv  = "download_progress.csv",
        osm_timeout_sec = DEFAULT_OSM_TIMEOUT_SEC,
        output_epsg     = DEFAULT_OUTPUT_LONLAT_EPSG,
        tile_stop = -1
) {
    # --- Inputs & setup ---
    if (!file.exists(final_tiles_path)) stop("Tiles file not found: ", final_tiles_path)
    
    dir.create(output_root, showWarnings = FALSE, recursive = TRUE)
    
    final_tiles <- readRDS(final_tiles_path)
    if (!is.list(final_tiles)) stop("final_tiles must be a list.")
    stopifnot(all(c("geometry","buffer","status") %in% names(final_tiles[[1]])))
    
    progress <- load_or_init_progress(progress_csv)
    
    total_tiles <- length(final_tiles)
    
    #I don't think we need this function for determining the start tile, yet. As long as we skip tiles that aren't done.
    #start_tile  <- first_incomplete_tile(progress, total_tiles, feature_specs)
    start_tile <- 1
    if (tile_stop == -1) {tile_stop <- length(final_tiles)}
    
    cat(sprintf("Starting download for %d tiles and %d features. Resuming at tile %d.\n",
                total_tiles, length(feature_specs), start_tile))
    
    # --- Tile loop ---
    for (tile_idx in seq(from = start_tile, to = tile_stop)) {
        
        tile_obj <- final_tiles[[tile_idx]]
        
        if (!inherits(tile_obj$geometry, "sfc")) {
            warning(sprintf("Tile %d has no valid sfc geometry. Skipping.", tile_idx))
            next
        }
        
        bbox_vec <- try(tile_bbox_wgs84(tile_obj$geometry, out_epsg = output_epsg), silent = TRUE)
        if (inherits(bbox_vec, "try-error")) {
            warning(sprintf("Tile %d: could not build WGS84 bbox. Skipping.", tile_idx))
            next
        }
        
        # --- Feature loop (modular: just pass whatever features you want) ---
        for (feature_idx in 1:length(feature_specs)) {
            
            spec <- feature_specs[[feature_idx]]
            
            if (is_done(progress, tile_idx, spec$tag)) {
                cat(sprintf("Tile %d | %-16s -> already done, skipping.\n", tile_idx, spec$tag))
                next
            }
            
            if (is_empty(progress, tile_idx, spec$tag)) {
                cat(sprintf("Tile %d | %-16s -> already done and empty, skipping.\n", tile_idx, spec$tag))
                next
            }
            
            feat_dir  <- file.path(output_root, spec$tag)
            dir.create(feat_dir, showWarnings = FALSE, recursive = TRUE)
            
            tile_stub <- file.path(feat_dir, sprintf("tile_%03d", tile_idx))
            
            # Log START
            row_stub <- tibble::tibble(
                tile        = tile_idx,
                tag         = spec$tag,
                started_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
                finished_at = NA_character_,
                status      = NA_character_,
                n_features  = NA_integer_,
                file_path   = paste0(tile_stub, "_", spec$tag, ".rds"),
                error_msg   = NA_character_
            )
            progress <- dplyr::bind_rows(progress, row_stub)
            save_progress(progress, progress_csv)
            
            cat(sprintf("Tile %d/%d | Feature: %-16s | Querying OSM…\n",
                        tile_idx, total_tiles, spec$tag))
            
            # Run Overpass queries for this feature
            res_list <- query_overpass_feature(spec, bbox_vec, timeout_sec = osm_timeout_sec)
            n_total  <- count_osm_features_list(res_list)
            
            if (n_total == 0) {
                cat(sprintf("Tile %d | %s -> Empty result.\n", tile_idx, spec$tag))
                sel <- which(progress$tile == tile_idx &
                                 progress$tag == spec$tag &
                                 is.na(progress$finished_at))
                if (length(sel)) sel <- tail(sel, 1)
                progress$status[sel]      <- "Empty"
                progress$n_features[sel]  <- 0L
                progress$finished_at[sel] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
                save_progress(progress, progress_csv)
                Sys.sleep(runif(1, 0.8, 1.6))  # polite jitter
                next
            }
            
            # Write output
            files <- character(0)
            err   <- NULL
            
            files <- try(
                write_feature_rds(
                    res_list   = res_list,
                    spec       = spec,
                    tile_stub  = tile_stub,
                    tile_idx   = tile_idx,
                    output_root = output_root,
                    out_epsg   = output_epsg
                ),
                silent = TRUE
            )
            if (inherits(files, "try-error")) err <- as.character(files)
            
            # Log END
            sel <- which(progress$tile == tile_idx &
                             progress$tag == spec$tag &
                             is.na(progress$finished_at))
            if (length(sel)) sel <- tail(sel, 1)
            
            if (!is.null(err)) {
                cat(sprintf("Tile %d | %s -> ERROR: %s\n", tile_idx, feature_idx, stringr::str_trunc(err, 300)))
                progress$status[sel]      <- "Error"
                progress$error_msg[sel]   <- stringr::str_trunc(err, 300)
            } else {
                cat(sprintf("Tile %d | %s -> Saved %d features into %s\n",
                            tile_idx, feature_idx, n_total, paste0(tile_stub, "_", spec$tag, ".rds")))
                progress$status[sel]     <- "Success"
                progress$n_features[sel] <- n_total
            }
            
            progress$finished_at[sel] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
            save_progress(progress, progress_csv)
            
            Sys.sleep(runif(1, 0.8, 1.6))  # polite jitter
        }
    }
    
    cat("All done.\n")
}

