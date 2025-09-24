#This is the final drill-down model generator. It will take the most granular information we have and turn it into a model. This model will then be saved so that we can use it to predict the volumes at any location on our map.

#Step 1: We need to assemble our data. We will take a list of FEMC locations with lat lon in degrees and use these to gather a new dataset for performing our drilldown ML.

#We will start with our list our FEMCs. Please change this location when able.!!!!!!

femc_list <- readRDS("femc_clean.rds") |>
    dplyr::select(c(scaled_ppd, lat, lon))

source("full_ml_feature_extraction_bel.R")

dd_ml_sf <- tibble()

for (i in 1:femc_list) {
    
    dd_ml_sf <- dd_ml_sf |>
        bind_rows(generate_full_ml_sf(lat = femc_list$lat[i], lon = femc_list$lon[i]))
    
}

dd_ml_sf <- dd_ml_sf |>
    mutate(scaled_ppd = femc_list$scaled_ppd)

#Step 2: Now that our data is assembled, we need to run our ML algorithm on this.

#Step 3: Finally, we need to save the resulting model.