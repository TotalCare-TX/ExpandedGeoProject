# full_ml_feature_extraction_bel.R
# This will extract all features for the expanded model. For now, it will rely on reading the birdseye data from the existing predictions raster.

# Load necessary libraries
# NOTE: You MUST have all these packages installed (e.g., install.packages("sf", "dplyr", ...))
library(sf)
library(dplyr)
library(terra)
library(tidycensus)
library(tidyr)
library(osmdata)

# Step 1: The input. We will need to make sure this is a set of coordinates set to CRS 3083.
use_correct_crs <- function(lat, lon) {
  
  df <- data.frame(lon = lon, lat = lat)
  # Ensure all st_ functions are available (requires library(sf))
  pt_sf <- st_as_sf(df, coords = c("lon","lat"), crs = 4326) |>
    st_transform(3083)
  
  return(pt_sf)
  
}

# Step 2: We will need to extract the raster value at this point.
birdseye_raster_path <- "raster_predictions.tif"

extract_computed_raster_val <- function(sf_obj, birdseye_raster_path = "raster_predictions.tif") {
  
  # Check if raster file exists
  if (!file.exists(birdseye_raster_path)) {
    warning(paste("Raster file not found at:", birdseye_raster_path, ". Returning NA."))
    return(NA_real_)
  }
  
  r <- terra::rast(birdseye_raster_path)
  
  # === FIX APPLIED HERE: Using all() to force the condition to a single logical value ===
  # We compare the WKT strings of the sf object and the raster object
  crs_mismatch <- all(as.character(st_crs(sf_obj)$wkt) != as.character(terra::crs(r, proj = TRUE)))
  
  if (crs_mismatch) { 
    # Use terra::crs() for the raster's CRS
    pts_for_extract <- st_transform(sf_obj, terra::crs(r))
  } else {
    pts_for_extract <- sf_obj
  }
  
  ext <- terra::extract(
    r,
    terra::vect(pts_for_extract), # Use terra::vect
    method = "bilinear",
    ID = FALSE
  )
  
  # Handle the case where extraction returns multiple columns or fails
  if (is.null(ext) || length(ext) == 0) return(NA_real_)
  
  # If the output is a data frame/matrix, return the first column value
  if (is.data.frame(ext) && ncol(ext) > 0) return(ext[[1]])
  
  return(ext)
  
}

# Step 3: We will need the relevant census data at this point.

# ==============================
# Global settings
# ==============================
options(tigris_use_cache = TRUE)
terraOptions(progress = 1, memfrac = 0.6, cores = max(1, parallel::detectCores() - 1))
METRIC_EPSG <- 3083                         
`%||%` <- function(a, b) if (!is.null(a)) a else b

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


# Step 4: We will need the calculated data at this point, including competitor data.
# Ensure this file is saved with the latest robust functions!
source("computed_variables_functions.R")

# Step 5: We will call the functions and report the output.

# Test coords:
lon <- -96.7053342
lat <- 33.2042821

generate_full_ml_sf <- function(lat, lon) {
  
  output_sf <- use_correct_crs(lat, lon)
  
  output_sf <- output_sf |>
    # as.vector is needed because extract_computed_raster_val now returns a dataframe/matrix
    mutate(Birdseye = as.vector(extract_computed_raster_val(output_sf)))
  
  output_sf <- output_sf |>
    bind_cols(census_stats(output_sf))
  
  # compute_road_variables is sourced and contains robust checks
  output_road_vars <- compute_road_variables(lat, lon)
  
  output_sf <- output_sf |>
    mutate(total_road_length_10km = output_road_vars$total_road_length_10km,
           intersection_count = output_road_vars$intersection_count,
           total_road_length_5km = output_road_vars$total_road_length_5km,
           total_weighted_road_size_5km = output_road_vars$total_weighted_road_size_5km,
           avg_road_size_5km = output_road_vars$avg_road_size_5km,
           total_corner_score = output_road_vars$total_corner_score)
  
  # compute_competitor_metrics is sourced and contains robust checks
  output_competitor <- compute_competitor_metrics("full_er_list.rds", lat = lat, lon = lon)
  
  output_sf <- output_sf |>
    mutate(competitors_within_10km = output_competitor$competitors_within_10km,
           nearest_er_km = output_competitor$nearest_er_km,
           femc_within_50km = output_competitor$femc_within_50km,
           nearest_femc_km = output_competitor$nearest_femc_km)
  
  return(output_sf)
  
}

# ----------------------------------------------------------------------
# TO RUN THE SCRIPT, execute the following lines in your R console:
# ----------------------------------------------------------------------
# lon <- -96.7053342
# lat <- 33.2042821
# final_features_sf <- generate_full_ml_sf(lat, lon)
# print(final_features_sf)
library(sf)
library(dplyr)
library(sf)        # for st_transform, st_as_sf, st_buffer, etc.
library(dplyr)     # for mutate, bind_cols, the pipe operator (|>), etc.
library(tidycensus) # for get_acs and census data
library(terra)     # for rast, extract, and vector/raster operations
library(tidyr)     # for pivot_wider
library(osmdata)   # for querying and processing OpenStreetMap data (osmdata_sf)
# Potentially others needed by the sourced file:
# library(ggplot2) # often used for plotting sf/osmdata objects
# library(purrr)   # often used with lists/lapply/map functions
final_features_sf <- generate_full_ml_sf(lat, lon)
