# This script will create a framework for generating our table for initial 
# ML (raster-based). A raster-based ML will be HIGH-LEVEL only. This is because it is 
# impossible to generate some of the specific, low-level data across the entire state.

# We will compose the table in pieces. The first piece we will start with is the list 
# of ERs and their associated outcome variables.

# In order to use files as our database gets updated, we will refer to filepaths for now. 
# This will be easy becasue then we can replace the filepaths with database calls later 
# as 


# 1. Start with the list of ERs in the outcome table. The important columns to be present 
# in this table are the THCIC_ID, the location coordinates, and the scaled patients per 
# day. I do not have these at present and they are not in their raw form in the current 
# directory so these will need to be inputted.

# 2. The next step is to join this with a column that indicates if a facility accepts 
# Medicare/Medicaid or not. This will function as a single column that is then joined on 
# the THCIC_ID. If this is the last time we use the THCIC_ID, this column may also be 
# removed.

# For now, this joined table will be represented by the following using the ml_table.csv 
# (delete this function when no longer in use):

generate_temporary_ml_start <- function() {
    
  starting_table <- read_csv("./ml_creation_scripts/ml_data_temp_storage/ml_table.csv") |>
    select(c(THCIC_ID, Accepts_Med, scaled_ppd, lon, lat)) 
  
  fake_ers <- read_csv("./ml_creation_scripts/ml_data_temp_storage/Fake_ERs.csv") |>
    rename(lon = Lon,
           lat = Lat,
           scaled_ppd = Daily_Insured_Volume,
           THCIC_ID =ID) |>
    dplyr::mutate(Accepts_Med = FALSE) |>
    select(c(THCIC_ID, Accepts_Med, scaled_ppd, lon, lat)) 
  
  
  competitor_features <- readRDS("./ml_creation_scripts/ml_data_temp_storage/FEMC_competitor_plus_hospital_kde.rds") |>
    rename(THCIC_ID = "thcic_id") |>
    select(-c("lon", "lat"))
  
  out <- starting_table |>
    select(c(THCIC_ID, scaled_ppd, lon, lat, Accepts_Med)) |>
    mutate(THCIC_ID = as.character(THCIC_ID)) |>
    bind_rows(fake_ers) |> left_join(competitor_features, by = "THCIC_ID") |>
    na.omit() |> select(-c("THCIC_ID", "facility_type", "facility"))

    return(out)
    
}

ml_sf <- generate_temporary_ml_start() |>
    st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    st_transform(3857)

# 3. The next step will be to pull raster information for each location. To do this,
# we will first identify the raster storage location and its subordinate directories.
# Then we will read in each raster individually and write the full column before moving
# to the next raster.

# raster_path <- "./kde_rasters/"
raster_path <- "kde_rasters/"


raster_dirs <- list.dirs(raster_path)
raster_dirs <- raster_dirs[raster_dirs != raster_path]


for (i in 1:length(raster_dirs)) {

    raster_files <- list.files(raster_dirs[i])
    
    for (j in 1:length(raster_files)) {

        temp_col_name <- str_c("raster_", gsub(raster_dirs[i], "", raster_files[j]))
        temp_col_name <- gsub("\\.tif(f)?$", "", temp_col_name)
        
        temp_file_path <- str_c(raster_dirs[i], "/",raster_files[j])
        
        r <- terra::rast(temp_file_path)
        
        if (st_crs(ml_sf)$wkt != crs(r)) {
            pts_for_extract <- st_transform(ml_sf, crs(r))
        } else {
            pts_for_extract <- ml_sf
        }
        
        ext <- terra::extract(
            r,
            vect(pts_for_extract),
            method = "bilinear",  # or "simple"
            ID = FALSE
        )
        
        ml_sf[[temp_col_name]] <- ext$focal_sum
        
    }
    
}

ml_table <- ml_sf %>%
    st_drop_geometry(pts_sf) |>
    as_tibble() |>
    mutate(across(everything(), ~replace(., is.na(.) | is.nan(.), 0)))


names(ml_table) <- sub(
  "^(.*?)_competitor_(.*)$", 
  "raster_competitor_\\1_KDE_sigma\\2", 
  names(ml_table)
)  

# # 4. The next step is to input this data into the desired model. For now, I will put 
# #in a placeholder function that serves as a basic model for our use.
# 
# placeholder_model <- lm(scaled_ppd ~ ., data = ml_table)
# 
# stepped_placeholder <- stats::step(placeholder_model)
# 
# #Now we need to save this model. We will save the placeholder, but later we can 
# #change this to the actual real model.
# 
# real_raster_model <- stepped_placeholder
# saveRDS(real_raster_model, "raster_model.rds")

# ml_table <- read.csv("ml_table.csv")
# ml_table <- ml_table %>% 
#   mutate(
#     scaled_ppd = log(scaled_ppd) 
#   ) %>%
#   dplyr::select(-X)

source("ml_creation_scripts/workflow-ml.R")
geo_formula <- create_geo_formula(ml_table, outcome_var = "scaled_ppd")

real_raster_model <- model_stacking(ml_table, geo_formula)
saveRDS(real_raster_model, "raster_model.rds")

# base_models <- fit_base_learners(real_raster_model, ml_table, "scaled_ppd")

# 5. Next, we will construct our frame for prediction. We will do this by turning 
#all rasters into dataframes and joining them together.

raster_prediction_tibble <- tibble()

for (i in 1:length(raster_dirs)) {
    
    raster_files <- list.files(raster_dirs[i])
    
    for (j in 1:length(raster_files)) {
        
        temp_col_name <- str_c("raster_", gsub(raster_dirs[i], "", raster_files[j]))
        temp_col_name <- gsub("\\.tif(f)?$", "", temp_col_name)
        
        temp_file_path <- str_c(raster_dirs[i], "/",raster_files[j])
        
        r <- terra::rast(temp_file_path)
        
        if (st_crs(ml_sf)$wkt != crs(r)) {
            pts_for_extract <- st_transform(ml_sf, crs(r))
        } else {
            pts_for_extract <- ml_sf
        }
        
        df <- as.data.frame(r, xy = TRUE, na.rm = TRUE) |>
            as_tibble() |>
            rename(!!temp_col_name := focal_sum)
        
        if(nrow(raster_prediction_tibble) == 0) {
            
            raster_prediction_tibble <- df
            
        } else {
            
            raster_prediction_tibble <- raster_prediction_tibble |>
                full_join(df)
            
        }
        
    }
    
}

raster_prediction_tibble <- raster_prediction_tibble |>
    mutate(Accepts_Med = FALSE) |>
    mutate(across(everything(), ~replace(., is.na(.) | is.nan(.), 0)))

# 6. Now that we have our complete raster for the state, we can PREDICT using our 
#model and develop a table of predictions. If we convert this back to a raster, 
#we can map it and then see our predictions - voila!

predictions_df <- raster_prediction_tibble |>
    select(c(x, y)) |>
    mutate(Predicted = predict(real_raster_model, raster_prediction_tibble))

# Now we convert this back to a raster

# library(terra)
# library(leaflet)
# library(dplyr)

# Template raster - we will use the microsoft housing one since it is the most complete
r_template <- terra::rast("./kde_rasters/microsoft_housing/microsoft_housing_KDE_sigma5km.tif") 

# Create an empty raster with same grid
r_out <- terra::rast(r_template)

# Map df rows to template cell indices via x/y
xy_mat <- as.matrix(predictions_df[, c("x","y")])
cell_id <- terra::cellFromXY(r_out, xy_mat)

# Assign values
vals <- predictions_df$Predicted

# Initialize with 0s
terra::values(r_out) <- 0
terra::values(r_out)[cell_id] <- vals

# Plot
plot(r_out, main = "Raster rebuilt on template grid")

# 7. Now we save the output raster

writeRaster(r_out, "raster_predictions.tif", overwrite = TRUE)

