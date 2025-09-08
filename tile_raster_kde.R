suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
})

# =========================
# Paths & basic parameters
# =========================
tiles_path       <- "final_tiles.rds"        
download_root    <- "tile_downloads"         
feature_out_root <- "feature_rasters"        
kde_out_root     <- "kde_rasters"           
progress_csv     <- "progress.csv"           

dir.create(feature_out_root, showWarnings = FALSE, recursive = TRUE)
dir.create(kde_out_root,     showWarnings = FALSE, recursive = TRUE)

# ==========
# Switches
# ==========
if (!exists("RUN_STEP1_RASTERIZE", inherits = FALSE)) RUN_STEP1_RASTERIZE <- TRUE
if (!exists("RUN_STEP2_KDE",       inherits = FALSE)) RUN_STEP2_KDE       <- TRUE

# =========================
# Grid & file settings
# =========================
PROJECTED_CRS  <- 3083                      
RESOLUTION_M   <- 200                       
SNAP_ORIGIN    <- c(0, 0)  # Grid anchor (x,y) in projected units; ensures all tiles align to the same grid.
GDAL_OPTS      <- c("COMPRESS=LZW", "TILED=YES", "BIGTIFF=IF_SAFER") # It’s a set of GeoTIFF write options passed to GDAL (via terra::writeRaster) to make output smaller and faster to read

# ==================================================================================
# Line sampling step, used to densify lines into points, one point every 50 m
# ==================================================================================
LINE_STEP_M    <- 50

# =========================
# KDE parameters
# =========================
SIGMAS_M       <- c(1000, 3000, 5000)       # Gaussian sigmas in meters
RADIUS_MULT    <- 3                         # kernel radius = RADIUS_MULT * sigma

# ====================================================================================
# Load tiles & discover tags, It checks that tiles_path exists and loads the tiles
# Then finds all tag folders under download_root. 
# For each tag, it creates matching output subfolders in feature_out_root and kde_out_root.
# ====================================================================================
stopifnot(file.exists(tiles_path))
final_tiles <- readRDS(tiles_path)

tag_dirs <- list.dirs(download_root, full.names = TRUE, recursive = FALSE)
tag_dirs <- tag_dirs[file.info(tag_dirs)$isdir]

for (td in tag_dirs) {
  tg <- basename(td)
  dir.create(file.path(feature_out_root, tg), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(kde_out_root,     tg), showWarnings = FALSE, recursive = TRUE)
}

# =======
# Helpers
# =======

# Snap a coordinate to grid lines
snap <- function(x, origin, res, up = FALSE) {
  if (up) origin + ceiling((x - origin) / res) * res else origin + floor((x - origin) / res) * res
}

# It checks the geometry has a CRS, takes its bounding box
# snaps the min edges down and max edges up to a common grid (based on origin and resolution)
# returns a raster covering that snapped extent with the chosen CRS and cell size

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

# Project a tile polygon to the working CRS
derive_buffer_proj <- function(tile_obj) {
  geom_m <- suppressWarnings(sf::st_transform(tile_obj$geometry, PROJECTED_CRS))
  sf::st_make_valid(geom_m)
}

# It inspects the sf layer’s geometry types and, if all features are points returns "POINT"
# if all are lines returns "LINE", otherwise returns NA.
geom_cat <- function(sfl) {
  g <- unique(as.character(sf::st_geometry_type(sfl, by_geometry = TRUE)))
  if (length(g) == 1L && g == "POINT")      return("POINT")
  if (length(g) == 1L && g == "LINESTRING") return("LINE")
  NA_character_
}

# =============
# Rasterizers 
#==============

# It takes an sf points layer and a raster template
# reprojects the points to the template’s CRS, then rasterizes them by counting how many points fall in each cell. 
# Cells with no points are written as 0

rasterize_points <- function(sf_pts, template_r) {
  if (!inherits(sf_pts, "sf") || nrow(sf_pts) == 0) return(NULL)
  pts_proj <- suppressWarnings(sf::st_transform(sf_pts, terra::crs(template_r)))
  terra::rasterize(terra::vect(pts_proj), template_r, field = 1, fun = "sum", background = 0)
}

# Converts a linestring layer into a raster of approximate line length per cell
# Densifies each line to points at step_m spacing (st_segmentize)
# Rasterizes points by counting per cell (empty cells will get 0)
# Multiplies the count by step_m so (points × step_m) = meters of line in the cell

rasterize_lines <- function(sf_lines, template_r, step_m) {
  if (!inherits(sf_lines, "sf") || nrow(sf_lines) == 0) return(NULL)
  crs_proj <- terra::crs(template_r)
  ln_proj  <- suppressWarnings(sf::st_transform(sf_lines, crs_proj))
  # densify to points so each sampled point contributes 1, then scale back by step_m to approximate length
  segs     <- suppressWarnings(sf::st_segmentize(ln_proj, dfMaxLength = step_m))
  pts      <- sf::st_cast(sf::st_geometry(segs), "POINT", warn = FALSE)
  pts_sf   <- sf::st_sf(geometry = pts, crs = crs_proj)
  rp <- terra::rasterize(terra::vect(pts_sf), template_r, field = 1, fun = "sum", background = 0)
  rp * step_m
}

# --- Progress CSV ---
if (file.exists(progress_csv)) {
  progress <- readr::read_csv(progress_csv, show_col_types = FALSE)
} else {
  progress <- tibble::tibble(tile = integer(), tag = character(), status = character())
}
save_progress <- function(df, path) readr::write_csv(df, path)

# --- Path helpers ---
rds_path_for     <- function(tag, tile_idx) file.path(download_root, tag, sprintf("tile_%03d_%s.rds", tile_idx, tag))
feature_path_for <- function(tag, tile_idx) file.path(feature_out_root, tag, sprintf("tile_%03d_%s.tif", tile_idx, tag))
kde_path_for     <- function(tag, sigma_m)  file.path(kde_out_root, tag, sprintf("%s_KDE_sigma%skm.tif",
                                                                                 tag, format(sigma_m/1000, trim=TRUE, nsmall=0)))

# ==============================
# Step 1: Build per-tile rasters
# ==============================
# Loops over all tiles × tags
# Reprojects the tile polygon to the working CRS and builds a snapped raster template
# Rasterizes points or lines
# masks the raster to the tile polygon so snapped bbox padding becomes NA (prevents seam dilution)
# Writes the per-tile GeoTIFF and logs status to the progress CSV

if (isTRUE(RUN_STEP1_RASTERIZE)) {
  total_tiles <- length(final_tiles)
  get_tile    <- function(i) final_tiles[[i]]
  
  cat(sprintf("Step 1: Rasterizing %d tiles across %d tags\n", total_tiles, length(tag_dirs)))
  
  for (tile_idx in seq_len(total_tiles)) {
    # Project tile polygon to working CRS
    buffer_proj <- derive_buffer_proj(get_tile(tile_idx))
    if (length(buffer_proj) == 0 || sf::st_is_empty(buffer_proj)) {
      cat(sprintf("Tile %03d -> empty/invalid geometry; skipping all tags.\n", tile_idx))
      next
    }
    
    r_template  <- tile_template_on_buffer(buffer_proj, RESOLUTION_M, SNAP_ORIGIN)
    
    for (td in tag_dirs) {
      tag      <- basename(td)
      in_file  <- rds_path_for(tag, tile_idx)
      out_file <- feature_path_for(tag, tile_idx)
      
      if (!file.exists(in_file)) {
        progress <- dplyr::bind_rows(progress, tibble::tibble(tile = tile_idx, tag = tag, status = "skip_no_input"))
        save_progress(progress, progress_csv)
        next
      }
      if (file.exists(out_file)) {
        progress <- dplyr::bind_rows(progress, tibble::tibble(tile = tile_idx, tag = tag, status = "done"))
        save_progress(progress, progress_csv)
        cat(sprintf("Tile %03d | %-16s -> raster exists (skip)\n", tile_idx, tag))
        next
      }
      
      sfl <- readRDS(in_file)
      if (!inherits(sfl, "sf") || nrow(sfl) == 0) {
        progress <- dplyr::bind_rows(progress, tibble::tibble(tile = tile_idx, tag = tag, status = "skip_empty"))
        save_progress(progress, progress_csv)
        next
      }
      
      gcat <- geom_cat(sfl)
      if (is.na(gcat)) {
        progress <- dplyr::bind_rows(progress, tibble::tibble(tile = tile_idx, tag = tag, status = "skip_invalid_geom"))
        save_progress(progress, progress_csv)
        cat(sprintf("Tile %03d | %-16s -> skipped (invalid geometry types)\n", tile_idx, tag))
        next
      }
      
      rast <- if (gcat == "POINT") {
        rasterize_points(sfl["geometry"], r_template)
      } else { # gcat == "LINE"
        rasterize_lines(sfl["geometry"], r_template, LINE_STEP_M)
      }
      
      if (is.null(rast)) {
        progress <- dplyr::bind_rows(progress, tibble::tibble(tile = tile_idx, tag = tag, status = "error_no_raster"))
        save_progress(progress, progress_csv)
        cat(sprintf("Tile %03d | %-16s -> no raster output\n", tile_idx, tag))
        next
      }
      
      # Mask the snapped rectangle to the actual tile polygon so padding becomes NA (not 0)
      rast <- terra::mask(rast, terra::vect(buffer_proj))  # outside polygon -> NA
      
      # Write per-tile raster
      terra::writeRaster(rast, filename = out_file, overwrite = TRUE, gdal = GDAL_OPTS)
      progress <- dplyr::bind_rows(progress, tibble::tibble(tile = tile_idx, tag = tag, status = "done"))
      save_progress(progress, progress_csv)
      cat(sprintf("Tile %03d | %-16s -> wrote raster\n", tile_idx, tag))
    }
  }
  cat("Step 1 complete.\n")
}

# =====================
# Step 2: Seamless KDE
# =====================
gauss_kernel <- function(sigma_cells, radius_mult = RADIUS_MULT) {
  r  <- ceiling(radius_mult * sigma_cells)
  ax <- -r:r
  K  <- outer(ax, ax, function(i, j) exp(-(i*i + j*j) / (2 * sigma_cells^2)))
  K / sum(K)
}

# Mosaic helper 
# Mosaics a list of GeoTIFFs using the cellwise mean
safe_mosaic_mean <- function(files) {
  if (!length(files)) return(NULL)
  m <- try(terra::mosaic(terra::sprc(files), fun = "mean"), silent = TRUE)
  if (inherits(m, "try-error")) m <- terra::mosaic(terra::sprc(files), fun = mean)
  m
}
# Creates a single coverage polygon from all tiles
# For each tag, loads all per-tile rasters and mosaics them by mean, (padding was masked to NA in Step 1, so seams aren’t diluted)
# For each sigma, builds a Gaussian kernel
# Numerator: Smooth the raster values with the Gaussian kernel, but replace any NA with 0 first so missing cells don’t add anything to the sum.
# Denominator: Smooth a mask where valid cells = 1 and missing cells = 0, this tells you how much real data contributed at each location.
# KDE value: Numerator ÷ Denominator to get the NA aware average; if the Denominator = 0 (no valid neighbors), set the result to NA.
# Mask the result to the union coverage (keeps output within mapped area)
# Then writes the per-tag, per-sigma GeoTIFF

if (isTRUE(RUN_STEP2_KDE)) {
  get_tile <- function(i) final_tiles[[i]]
  ntiles <- length(final_tiles)
  
  # Build union coverage (in projected CRS)
  buffers_proj <- purrr::map(seq_len(ntiles), ~derive_buffer_proj(get_tile(.x)))
  buffers_proj <- do.call(c, buffers_proj)
  buffers_proj <- buffers_proj[!sf::st_is_empty(buffers_proj)]
  buffers_proj <- sf::st_make_valid(buffers_proj)
  coverage_union_proj <- sf::st_union(buffers_proj)
  
  # Discover tags that have per-tile rasters
  tags <- if (dir.exists(feature_out_root))
    basename(list.dirs(feature_out_root, recursive = FALSE, full.names = TRUE)) else character(0)
  
  cat(sprintf("Step 2: Global KDE for %d tag(s): %s\n", length(tags), paste(tags, collapse = ", ")))
  
  for (tag in tags) {
    in_dir  <- file.path(feature_out_root, tag)
    out_dir <- file.path(kde_out_root,     tag)
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    
    files <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
    if (!length(files)) { cat(sprintf("Tag %-16s | No feature rasters. Skipping.\n", tag)); next }
    
    # Mosaic per-tile rasters; because we masked padding to NA, overlaps won't get diluted by zeros
    mos <- safe_mosaic_mean(files)
    if (is.null(mos)) { cat(sprintf("Tag %-16s | Mosaic failed. Skipping.\n", tag)); next }
    
    res_m <- terra::res(mos)[1]
    kernels <- setNames(lapply(SIGMAS_M, function(s) gauss_kernel(sigma_cells = s / res_m)),
                        as.character(SIGMAS_M))
    
    # Prepare fields for NA-aware convolution
    z     <- mos
    valid <- terra::ifel(is.finite(z), 1, 0)   # valid=1 where we have data (including true zeros), 0 otherwise
    
    # Build a coverage mask raster from union polygon (keeps the result inside the mapped area)
    coverage_r <- terra::rast(mos)
    coverage_r <- terra::rasterize(terra::vect(coverage_union_proj), coverage_r,
                                   field = 1, background = NA, touches = TRUE)
    
    for (s in SIGMAS_M) {
      out_file <- kde_path_for(tag, s)
      K <- kernels[[as.character(s)]]
      
      # NA-aware smoothing:
      # Numerator: treat NA as 0 so they don't contribute
      # Denominator: smooth the valid mask to count effective weights
      num <- terra::focal(terra::ifel(is.finite(z), z, 0), w = K, fun = "sum", pad = TRUE, padValue = 0)
      den <- terra::focal(valid,                         w = K, fun = "sum", pad = TRUE, padValue = 0)
      
      r_kde <- num / den
      r_kde[den == 0] <- NA  # where no valid neighbors within kernel
      
      # Clip to coverage
      r_kde_masked <- terra::mask(r_kde, coverage_r)
      
      terra::writeRaster(r_kde_masked, filename = out_file, overwrite = TRUE, gdal = GDAL_OPTS)
      cat(sprintf("Tag %-16s | σ=%4dm -> %s\n", tag, s, basename(out_file)))
    }
  }
  cat("Step 2 complete.\n")
}

cat("All done.\n")
