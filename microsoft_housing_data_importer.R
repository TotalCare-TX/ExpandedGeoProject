# This script parses housing data from Microsoft Bing's visual registry.

# Downlaod link for the data: https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/Texas.geojson.zip 

# Uses tidyverse, leaflet, sf, future.apply, units, lwgeom

# First, we will write a function for producing the FlatGeoBuf file for the Texas hosuing. This function downloads and takes the data and turns it into fgb

# The script creates a list of housing points inside each tile. This list can be rasterized or joined together to create circles around points.

download_and_fgb_texas_housing <- function() {
    
    # ---- directories ----
    data_dir <- "data/ms_buildings_tx"
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    
    # ---- Microsoft TX footprints (GeoJSON.zip) ----
    tx_zip_url <- "https://minedbuildings.z5.web.core.windows.net/legacy/usbuildings-v2/Texas.geojson.zip"  # :contentReference[oaicite:3]{index=3}
    tx_zip <- file.path(data_dir, "Texas.geojson.zip")
    
    if (!file.exists(tx_zip)) {
        download.file(tx_zip_url, tx_zip, mode = "wb", quiet = TRUE)
    }
    file.exists(tx_zip) || stop("Download failed. Check URL / connectivity.")
    
    # Output FlatGeoBuf
    tx_fgb <- file.path(data_dir, "Texas.fgb")
    
    if (!file.exists(tx_fgb)) {
        # Use GDAL via sf; adds robustness for large files
        # -makevalid: fix odd polygons
        # -skipfailures: keep going if a few bad geoms exist
        sf::gdal_utils(
            util = "vectortranslate",
            source = sprintf("/vsizip/%s", normalizePath(tx_zip, winslash = "/")),
            destination = tx_fgb,
            options = c("-f", "FlatGeobuf", "-nln", "buildings_tx", "-makevalid", "-skipfailures")
        )
    }
    file.exists(tx_fgb) || stop("FGB conversion failed. Is GDAL available in your sf build?")
    
}

# Now we load the tiles

# ---- read your tile list (RDS) ----
#tiles_rds <- "final_tiles.rds"  # <— change if needed
#stopifnot(file.exists(tiles_rds))
#tiles_list <- readRDS(tiles_rds)
tiles_list <- final_tiles
stopifnot(is.list(tiles_list), length(tiles_list) > 0)

# ---- determine / set CRS for tiles ----
# Try to read CRS from the first element; fall back to 4326
tiles_crs_epsg <- tryCatch({
    cr <- sf::st_crs(tiles_list[[1]]$geometry)
    if (!is.null(cr) && !is.na(cr$epsg)) cr$epsg else 4326L
}, error = function(e) 4326L)

# ---- extract geometries as sfg, build a single sfc ----
geom_sfg <- lapply(tiles_list, function(rec) {
    g <- rec$geometry
    # If stored as sfc length-1, convert to sfg
    if (inherits(g, "sfc")) g <- g[[1]]
    # If still not sfg, coerce
    if (!inherits(g, "sfg")) g <- sf::st_as_sfc(g)[[1]]
    g
})

tiles_sfc <- sf::st_sfc(geom_sfg, crs = tiles_crs_epsg)
tiles_sfc <- st_make_valid(tiles_sfc)  # defensive

# ---- create sf with a synthetic tile_id (preserve yours if you have one) ----
tiles <- sf::st_sf(tile_id = seq_along(tiles_sfc), geometry = tiles_sfc)

# ---- equal-area (5070) for area; 4326 for GDAL filtering ----
tiles_5070 <- sf::st_transform(tiles, 5070)
tiles_4326 <- sf::st_transform(tiles_5070, 4326)

# ensure tiles are valid in both CRSs
tiles_4326 <- st_make_valid(tiles_4326)
tiles_5070 <- st_make_valid(tiles_5070)

# Finds the (single) sfc column and sets it as the active geometry.
# Safe to run on sf or plain data.frames.
ensure_sf_geom <- function(x) {
    # identify sfc columns
    gcols <- names(x)[vapply(x, function(col) inherits(col, "sfc"), logical(1))]
    if (length(gcols) == 1) {
        sf::st_geometry(x) <- gcols[1]   # this also (re)adds the "sf" class if needed
        return(x)
    }
    if (length(gcols) == 0) stop("No sfc column found in object handed to ensure_sf_geom().")
    stop(sprintf("Multiple sfc columns found (%s); specify which to use.",
                 paste(gcols, collapse = ", ")))
}

tiles_4326 <- ensure_sf_geom(tiles_4326)
tiles_5070 <- ensure_sf_geom(tiles_5070)


# ---- tile area in km² ----
tiles_5070$tile_km2 <- units::set_units(sf::st_area(tiles_5070), km^2) |> units::drop_units()

# quick sanity check
message(sprintf("Loaded %d tiles; example area: %.3f km²",
                nrow(tiles_5070), tiles_5070$tile_km2[1]))

house_min_m2 <- 25
house_max_m2 <- 450  # raise to ~900 to include large homes/small multifamily

# assumes you already have:
# - tiles_4326, tiles_5070 (from your Step 5′)
# - tx_fgb, data_dir, house_min_m2, house_max_m2 (from earlier steps)

# pick the (only) layer inside the FGB
ly <- sf::st_layers(tx_fgb)
layer_name <- if ("buildings_tx" %in% ly$name) "buildings_tx" else ly$name[1]

# tiny bbox expansion (degrees) so edge-touching footprints aren't dropped by driver quirks
eps_deg <- 1e-4  # ~11 m in lat; adjust if your tiles are very large/small

bbox_wkt <- function(geom_4326, eps) {
    stopifnot(sf::st_is_longlat(geom_4326))
    bb <- sf::st_bbox(geom_4326)
    bb["xmin"] <- bb["xmin"] - eps
    bb["xmax"] <- bb["xmax"] + eps
    bb["ymin"] <- bb["ymin"] - eps
    bb["ymax"] <- bb["ymax"] + eps
    sf::st_as_text(sf::st_as_sfc(bb, crs = 4326))
}

ensure_sf_geom <- function(x) {
    gcols <- names(x)[vapply(x, function(col) inherits(col, "sfc"), logical(1))]
    if (length(gcols) != 1) stop("Expected exactly one sfc column.")
    sf::st_geometry(x) <- gcols[1]
    x
}

empty_points_sf <- function(tile_id, crs = 4326) {
    sf::st_sf(
        tile_id = integer(0),
        area_m2 = numeric(0),
        bld_uid = character(0),
        geometry = sf::st_sfc(crs = crs)
    )
}

# prerequisites already defined in your session:
# tiles_4326, tiles_5070, tx_fgb, layer_name, house_min_m2, house_max_m2

tile_points_one <- function(i) {
    tile4326 <- tiles_4326[i, ]
    tile5070 <- tiles_5070[i, ]
    
    # 1) Candidate pull by bbox (fast, index-friendly)
    wkt1 <- bbox_wkt(tile4326, eps = eps_deg)
    bld  <- try(sf::st_read(tx_fgb, layer = layer_name, wkt_filter = wkt1, quiet = TRUE), silent = TRUE)
    if (inherits(bld, "try-error") || nrow(bld) == 0) {
        wkt2 <- bbox_wkt(tile4326, eps = eps_deg * 5)
        bld  <- try(sf::st_read(tx_fgb, layer = layer_name, wkt_filter = wkt2, quiet = TRUE), silent = TRUE)
        if (inherits(bld, "try-error") || nrow(bld) == 0) return(empty_points_sf(tiles_5070$tile_id[i]))
    }
    bld <- ensure_sf_geom(bld)
    
    # 2) Planar transform and clean (avoid s2)
    bld_5070 <- try(sf::st_transform(bld, 5070), silent = TRUE)
    if (inherits(bld_5070, "try-error")) { bld <- st_make_valid(bld); bld_5070 <- sf::st_transform(bld, 5070) }
    if (nrow(bld_5070) == 0) return(empty_points_sf(tiles_5070$tile_id[i]))
    bld_5070 <- bld_5070[!sf::st_is_empty(bld_5070), , drop = FALSE]
    inv <- !sf::st_is_valid(bld_5070); if (any(inv, na.rm = TRUE)) bld_5070[inv, ] <- st_make_valid(bld_5070[inv, ])
    if (!all(sf::st_geometry_type(bld_5070) %in% c("POLYGON","MULTIPOLYGON"))) {
        bld_5070 <- sf::st_collection_extract(bld_5070, "POLYGON")
    }
    if (nrow(bld_5070) == 0) return(empty_points_sf(tiles_5070$tile_id[i]))
    
    # 3) Exact clip to tile (GEOS in 5070)
    tile5070 <- ensure_sf_geom(tile5070)
    inside <- sf::st_intersects(bld_5070, sf::st_geometry(tile5070), sparse = FALSE)[,1]
    if (!any(inside)) return(empty_points_sf(tiles_5070$tile_id[i]))
    bld_tile <- bld_5070[inside, , drop = FALSE]
    
    # 4) House-sized proxy filter by area
    areas_m2 <- as.numeric(units::set_units(sf::st_area(bld_tile), m^2))
    keep <- (areas_m2 >= house_min_m2) & (areas_m2 <= house_max_m2)
    if (!any(keep)) return(empty_points_sf(tiles_5070$tile_id[i]))
    
    bld_keep <- bld_tile[keep, , drop = FALSE]
    areas_keep <- areas_m2[keep]
    
    # 5) Convert polygons → single representative point per building
    # Use interior points (always inside the footprint; safer than centroids for concave shapes)
    pts_5070 <- sf::st_point_on_surface(bld_keep)
    
    # 6) Attributes and IDs; back to 4326 for general use/web maps
    # stable geometry hash (use binary WKB; xxhash64 is fast)
    wkb_list <- sf::st_as_binary(sf::st_geometry(bld_keep))
    uids <- vapply(wkb_list, function(w) digest::digest(w, algo = "xxhash64"), FUN.VALUE = character(1))
    
    pts_5070 <- sf::st_sf(
        tile_id = tiles_5070$tile_id[i],
        area_m2 = areas_keep,
        bld_uid = uids,
        geometry = sf::st_geometry(pts_5070),
        crs = sf::st_crs(pts_5070)
    )
    pts_4326 <- sf::st_transform(pts_5070, 4326)
    pts_4326
}

# If futures were flaky, run sequential; otherwise use your preferred parallel plan.
# future::plan(future::sequential)
points_list <- lapply(seq_len(nrow(tiles_5070)), tile_points_one)

# Save a single RDS with the list
saveRDS(points_list, file.path(data_dir, "house_points_by_tile.rds"))

#Finally, in order to standardize our outputs, this will be placed in a target directory and subdivided by file in the same manner as our other downloaded OSM data.

output_root <- file.path("tile_downloads", "microsoft_housing")

dir.create(output_root, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(points_list)) {
    
    if (nrow(points_list[[i]]) == 0) {next}
    
    tile_stub <- paste0(sprintf("tile_%03d", points_list[[i]]$tile_id[1]))
    
    f <- file.path(output_root, paste0(tile_stub, "_microsoft_housing.rds"))
    
    out <- points_list[[i]] |>
        dplyr::select(c(tile_id, geometry)) |>
        rename(tile_index = tile_id) |>
        mutate(feature_tag = "microsoft_housing")
    
    saveRDS(out, f)
    
}

