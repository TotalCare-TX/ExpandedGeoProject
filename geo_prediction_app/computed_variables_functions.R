# computed_variables_functions.R
# This script contains functions for computed variables.

# This function will use a single download of OSM data in a 10km radius to compute road variables.
compute_road_variables <- function(lat, lon, large_radius_km = 10, small_radius_km = 5, corner_radius_m = 50) {
  pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  pt_proj <- st_transform(pt, 3857)
  buffer_proj_large <- st_buffer(pt_proj, large_radius_km*1000)
  buffer_proj_small <- st_buffer(pt_proj, small_radius_km*1000)
  buffer_proj_corner <- st_buffer(pt_proj, corner_radius_m)
  bbox <- st_bbox(st_transform(buffer_proj_large, 4326))
  
  # Download OSM roads
  message("Downloading OSM roads data...")
  osm_data_raw <- opq(bbox = bbox) %>%
    osmdata::add_osm_feature(key = "highway") %>%
    osmdata::osmdata_sf()
  
  roads <- osm_data_raw$osm_lines # Extract the lines object
  
  # ROBUSTNESS CHECK: If query failed or returned no data
  if (is.null(roads) || nrow(roads) == 0) {
    warning("OSM data query failed to return valid road lines or returned no data. Returning 0/NA for road variables.")
    return(list(
      total_road_length_10km = 0,
      intersection_count = 0,
      total_road_length_5km = 0,
      total_weighted_road_size_5km = 0,
      avg_road_size_5km = NA_real_,
      total_corner_score = 0
    ))
  }
  
  # Apply transformation and geometry repair
  roads_10km <- roads |>
    st_transform(3857) |>
    st_make_valid() # <--- ADDED: Attempt to repair geometries
  
  # First, we'll calculate the total road distance in the larger radius.
  roads_in_buffer <- st_intersection(roads_10km, buffer_proj_large)
  total_road_length_10km <- as.numeric(sum(st_length(roads_in_buffer)) / 1000)
  
  # Approximate intersection count via line endpoints
  message("Estimating intersection count...")
  
  # Use st_make_valid before casting to reduce geometry warnings
  road_points <- roads_in_buffer |>
    st_make_valid() |> 
    st_cast("POINT")
  
  coords <- st_coordinates(road_points) %>% as.data.frame()
  coords <- coords %>%
    mutate(x = round(X, -1), y = round(Y, -1))  # snap to 10m grid
  
  intersection_count <- coords %>%
    count(x, y) %>%
    filter(n >= 2) %>%
    nrow()
  
  # Now let's subset down to 5km for calculations in this area
  
  roads_5km <- subset_roads_radius(roads, lon, lat, small_radius_km*1000) |>
    st_transform(3857)
  
  # Next, we'll calculate the total road distance in the smaller radius.
  roads_in_buffer <- st_intersection(roads_5km, buffer_proj_small)
  total_road_length_5km <- as.numeric(sum(st_length(roads_in_buffer)) / 1000)
  
  # Next we'll calculate the total road distance weighted by the road size
  
  # Handle missing 'lanes' safely
  if (!"lanes" %in% names(roads_in_buffer)) {
    roads_in_buffer$lanes <- NA
  }
  
  # Scoring roads by lane count or fallback to road type
  road_scores <- roads_in_buffer %>%
    mutate(
      lane_score = suppressWarnings(as.numeric(lanes)),
      type_score = case_when(
        grepl("motorway", highway) ~ 6,
        grepl("trunk", highway) ~ 5,
        grepl("primary", highway) ~ 4,
        grepl("secondary", highway) ~ 3,
        grepl("tertiary", highway) ~ 2,
        grepl("residential|unclassified|living_street", highway) ~ 1,
        TRUE ~ 0
      ),
      final_score = ifelse(!is.na(lane_score), lane_score, type_score),
      road_length = as.numeric(st_length(roads_in_buffer)),
      final_road_weight = final_score*road_length
    )
  
  total_weighted_road_size_5km <- sum(road_scores$final_road_weight, na.rm = TRUE)/1000
  
  # Finally, we'll divide the weighted road size by the total road distance
  
  avg_road_size_5km <- total_weighted_road_size_5km/total_road_length_5km
  
  # Now let's filter down REALLY small to examine the corner visibility
  
  roads_corner <- subset_roads_radius(roads, lon, lat, corner_radius_m)
  
  if(is.null(roads_corner) || nrow(roads_corner) == 0) { # Combined corner check
    total_corner_score <- 0
  } else {
    
    roads_corner <- roads_corner |>
      st_transform(3857)
    
    # Find nearby roads
    roads_near_pt <- st_intersection(roads_corner, buffer_proj_corner)
    
    # Check if the intersection is valid (at least two road segments near the point)
    if (nrow(roads_near_pt) < 2) {
      total_corner_score <- 0  # Not an intersection
    } else {
      # Handle missing 'lanes' safely
      if (!"lanes" %in% names(roads_near_pt)) {
        roads_near_pt$lanes <- NA
      }
      
      # Scoring roads by lane count or fallback to road type
      road_scores <- roads_near_pt %>%
        mutate(
          lane_score = suppressWarnings(as.numeric(lanes)),
          type_score = case_when(
            grepl("motorway", highway) ~ 6,
            grepl("trunk", highway) ~ 5,
            grepl("primary", highway) ~ 4,
            grepl("secondary", highway) ~ 3,
            grepl("tertiary", highway) ~ 2,
            grepl("residential|unclassified|living_street", highway) ~ 1,
            TRUE ~ 0
          ),
          final_score = ifelse(!is.na(lane_score), lane_score, type_score)
        )
      
      # Sum of all intersecting road sizes
      total_corner_score <- sum(road_scores$final_score, na.rm = TRUE)
    }
  }
  
  # Finally, we need to make a list of all computed road output variables.
  
  return_list <- list(total_road_length_10km = total_road_length_10km,
                      intersection_count = intersection_count,
                      total_road_length_5km = total_road_length_5km,
                      total_weighted_road_size_5km = total_weighted_road_size_5km,
                      avg_road_size_5km = avg_road_size_5km,
                      total_corner_score = total_corner_score)
  
  return(return_list)
}

# This general use function will allow us to subset roads.
subset_roads_radius <- function(roads, lon, lat, radius_m) {
  # Ensure we build the buffer in the same CRS as roads
  crs_metric <- st_crs(roads)
  pt_m <- st_sfc(st_point(c(lon, lat)), crs = 4326) |> st_transform(crs_metric)
  buf_r <- st_buffer(pt_m, radius_m)
  
  # Spatial filter (fast; circle is tiny)
  st_filter(roads, buf_r)
}

# Now let's compute metrics for competitor ERs including FEMCs
compute_competitor_metrics <- function(full_er_list_path, lon, lat) {
  
  # Define the point
  pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  
  # ROBUSTNESS CHECK: Load and validate the list file
  if (!file.exists(full_er_list_path)) {
    warning(paste("Competitor list file not found at:", full_er_list_path, ". Returning NA."))
    list_data <- NULL
  } else {
    # Assuming the file is an RDS containing a data frame
    list_data <- readRDS(full_er_list_path) 
  }
  
  # ROBUSTNESS CHECK: Check if data is valid before st_as_sf
  if (is.null(list_data) || !is.data.frame(list_data) || !all(c("long", "lat") %in% names(list_data))) {
    warning("Competitor list is not a valid data frame with 'long' and 'lat'. Returning NA for competitor metrics.")
    return(list(
      competitors_within_10km = NA_real_,
      nearest_er_km = NA_real_,
      femc_within_50km = NA_real_,
      nearest_femc_km = NA_real_
    ))
  }
  
  # Processing for all competitors
  all_ers_sf <- st_as_sf(list_data, coords = c("long", "lat"), crs = 4326)
  distances <- st_distance(pt, all_ers_sf)
  competitors_within_10km <- sum(as.numeric(distances) < 10000)
  
  # Ensure distances is non-empty before taking min (in case the point is on an ER)
  non_zero_distances <- distances[as.numeric(distances) > 0] 
  nearest_er_km <- if (length(non_zero_distances) > 0) as.numeric(min(non_zero_distances)) / 1000 else NA_real_
  
  # Processing for FEMC competitors
  femc_data <- filter(list_data, `Facility Type` == 'FEMC')
  if (nrow(femc_data) == 0) {
    femc_within_50km <- 0
    nearest_femc_km <- NA_real_
  } else {
    femc_ers_sf <- st_as_sf(femc_data, coords = c("long", "lat"), crs = 4326)
    distances_femc <- st_distance(pt, femc_ers_sf)
    femc_within_50km <- sum(as.numeric(distances_femc) < 50000)
    
    non_zero_distances_femc <- distances_femc[as.numeric(distances_femc) > 0]
    nearest_femc_km <- if (length(non_zero_distances_femc) > 0) as.numeric(min(non_zero_distances_femc)) / 1000 else NA_real_
  }
  
  return_list <- list(competitors_within_10km = competitors_within_10km,
                      nearest_er_km = nearest_er_km,
                      femc_within_50km = femc_within_50km,
                      nearest_femc_km = nearest_femc_km)
  
  return(return_list)
  
}