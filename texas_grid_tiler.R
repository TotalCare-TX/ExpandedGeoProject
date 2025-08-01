#This script will create a grid for the state of Texas for downloading osmdata. It uses housing as its main control since this is the rate-limiting factor.

options(tigris_use_cache = TRUE)

# --- Parameters ---
tile_size <- 1       # degrees
overlap <- 0.1       # degrees
timeout_sec <- 180    # seconds
key <- "building"
value <- c("residential", "house", "apartments")
cache_dir <- "osm_cache"
tiles_output <- "final_tiles.rds"
min_tile_size_deg <- 0.01  # prevent infinite subdivision

dir.create(cache_dir, showWarnings = FALSE)

# --- Get Texas geometry ---
tx <- states(cb = TRUE) |>
    filter(STUSPS == "TX") |>
    st_transform(4326)

# --- Make initial tile grid ---
grid <- st_make_grid(tx, cellsize = c(tile_size, tile_size), square = TRUE) |>
    st_intersection(tx) |>
    st_make_valid()

# Add overlap
grid_buffered <- st_buffer(grid, dist = overlap)

# Convert to list of individual tiles
tile_list <- lapply(seq_along(grid_buffered), function(i) {
    list(geometry = grid_buffered[i], buffer = overlap)
})

#Create a queue in case the list generation fails
tile_list <- lapply(tile_list, function(x) c(x, list(status = "Not tried")))

#Try 2
download_osm_safe <- function(tile_geom, buffer_dist, key, value, cache_dir, timeout_sec) {
    hash <- digest::digest(st_as_text(tile_geom))
    path <- file.path(cache_dir, paste0(hash, ".rds"))
    
    if (file.exists(path)) {
        message("Using cached file for tile.")
        return(readRDS(path))
    }
    
    bbox <- st_bbox(tile_geom)
    
    result <- tryCatch({
        withTimeout({
            opq(bbox = bbox) %>%
                add_osm_feature(key = key, value = value) %>%
                osmdata_sf()
        }, timeout = timeout_sec, onTimeout = "error")
    },
    TimeoutException = function(e) {
        message("Timed out on tile.")
        return("timeout")
    },
    error = function(e) {
        if (grepl("elapsed time limit", e$message)) {
            message("Caught timeout-related error.")
            return("timeout")
        } else {
            message("General error: ", e$message)
            return("error")
        }
    })
    
    if (!is.character(result)) {
        saveRDS(result, path)
    }
    
    return(result)
}



# Recursive tile processor
process_tile <- function(tile_geom, buffer_dist, key, value, timeout_sec, cache_dir, min_size_deg, status) {
    bbox <- st_bbox(tile_geom)
    width <- bbox["xmax"] - bbox["xmin"]
    height <- bbox["ymax"] - bbox["ymin"]
    
    result <- "timeout"
    
    if(status != "Failure") {
        
        result <- download_osm_safe(tile_geom, buffer_dist, key, value, cache_dir, timeout_sec)
        
    }
    
    if (identical(result, "timeout") && width > min_size_deg && height > min_size_deg) {
        message("Timeout: Subdividing tile...")
        
        xmid <- (bbox["xmin"] + bbox["xmax"]) / 2
        ymid <- (bbox["ymin"] + bbox["ymax"]) / 2
        
        new_quadrants <- list(
            list(geometry = st_buffer(st_sfc(st_polygon(list(rbind(
                c(bbox["xmin"], bbox["ymin"]),
                c(xmid, bbox["ymin"]),
                c(xmid, ymid),
                c(bbox["xmin"], ymid),
                c(bbox["xmin"], bbox["ymin"])
            ))), crs = 4326), dist = buffer_dist / 2),
            buffer = buffer_dist / 2,
            status = "Not tried"),
            
            list(geometry = st_buffer(st_sfc(st_polygon(list(rbind(
                c(xmid, bbox["ymin"]),
                c(bbox["xmax"], bbox["ymin"]),
                c(bbox["xmax"], ymid),
                c(xmid, ymid),
                c(xmid, bbox["ymin"])
            ))), crs = 4326), dist = buffer_dist / 2),
            buffer = buffer_dist / 2,
            status = "Not tried"),
            
            list(geometry = st_buffer(st_sfc(st_polygon(list(rbind(
                c(bbox["xmin"], ymid),
                c(xmid, ymid),
                c(xmid, bbox["ymax"]),
                c(bbox["xmin"], bbox["ymax"]),
                c(bbox["xmin"], ymid)
            ))), crs = 4326), dist = buffer_dist / 2),
            buffer = buffer_dist / 2,
            status = "Not tried"),
            
            list(geometry = st_buffer(st_sfc(st_polygon(list(rbind(
                c(xmid, ymid),
                c(bbox["xmax"], ymid),
                c(bbox["xmax"], bbox["ymax"]),
                c(xmid, bbox["ymax"]),
                c(xmid, ymid)
            ))), crs = 4326), dist = buffer_dist / 2),
            buffer = buffer_dist / 2,
            status = "Not tried")
        )

        return(new_quadrants)
    }
    
    if (identical(result, "timeout")) {
        message("Tile timed out even at minimum size. Skipping.")
        return(list())
    }
    
    if (identical(result, "error")) {
        message("General error. Skipping.")
        return(list())
    }
    
    return(list(list(geometry = tile_geom, buffer = buffer_dist)))
}


# --- Tile processing loop ---

i <- 1

while (i <= length(tile_list)) {
    
    #If the tile has previously succeeded, it will be skipped.
    if(tile_list[[i]]$status == "Success") {
        
        message("Skipping. Previously successful.")
        i <- i + 1
        next()
        
    }
    prev_tile_status <- tile_list[[i]]$status
    tile_list[[i]]$status <- "Failure"
    message("Processing tile ", i, "/", length(tile_list))
    
    result <- process_tile(
        tile_geom = tile_list[[i]]$geometry,
        buffer_dist = tile_list[[i]]$buffer,
        key = key,
        value = value,
        timeout_sec = timeout_sec,
        cache_dir = cache_dir,
        min_size_deg = min_tile_size_deg,
        status = prev_tile_status
    )
    
    tile_list[[i]]$status <- "Success"
    
    if(length(result) == 4) {
        tile_list <- tile_list[-i]
        tile_list <- c(tile_list, result)
        i <- i - 1
    }
    
    if(length(result) == 0) {
        
        tile_list[[i]]$status <- "Failure" 
        
    }
    
    i <- i + 1
    
    saveRDS(tile_list, "final_tiles.rds")
}

message("Saved ", length(final_tiles), " successfully processed tiles.")

#The following code uses leaflet to check the configuration of the successful tiles

# Load your final tiles (assuming it's the saved RDS list)
final_tiles <- readRDS("final_tiles.rds")
#Remove all non-successful tiles

j <- 1
while(j <= length(final_tiles)) {
    
    if(final_tiles[[j]]$status != "Success") {
        
        final_tiles <- final_tiles[-j]
        j <- j - 1
        
    }
    
    j <- j + 1
    
}

# Extract geometries and flatten into an sfc object
tile_sfc <- do.call(c, lapply(final_tiles, function(x) x$geometry))

# Convert to sf object
tile_sf <- st_sf(geometry = tile_sfc)

# Plot with leaflet
leaflet(tile_sf) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        color = "black",        # border color
        weight = 1,             # border thickness
        fillColor = "blue",     # tile fill color
        fillOpacity = 0.5,      # transparency (0=transparent, 1=opaque)
        opacity = 1             # border opacity
    )

