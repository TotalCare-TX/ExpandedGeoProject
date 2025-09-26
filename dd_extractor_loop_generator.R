#This script will run a loop with error logging to construct the extended features on the drilldown ml method.

#It is meant to work with full_ml_feature_extraction_bel.R

source("full_ml_feature_extraction_bel.R")

all_femcs_only <- tibble()
all_competitors <- tibble() #both of these are placeholders for database calls

debug_now <- TRUE #This lets you debug without using the database
    
if (debug_now) {
    
    full_er_list <- readRDS("full_er_list.RDS")
    
    all_competitors <- full_er_list |> filter(`Facility Type` %in% c('FEMC','Hospital','Critical Access Hospital'))
    
    all_femcs_only <- full_er_list |> filter(`Facility Type` %in% c('FEMC'))
    
}

#Now we run generate_full_ml_sf on a loop and log it as we go in case the loop aborts.

for (i in 1:nrow(all_femcs_only)) {
    
    if(file.exists("dd_ml_sf.rds")) {
        
        dd_ml_sf <- readRDS("dd_ml_sf.rds")
        
        read_from_file <- TRUE
        
        if(all_femcs_only$thcic_id[i] %in% dd_ml_sf$thcic_d) {
            
            next()
            
        }
        
    }
    
    temp_row_stub <- generate_full_ml_sf(lat = all_femcs_only$lat[i], lon = all_femcs_only$lon[i]) |>
        mutate(thcic_id = all_femcs_only$thcic_id[i])
    
    if(read_from_file) {
        
        dd_ml_sf <- dd_ml_sf |>
            bind_rows(temp_row_stub)
        
    } else {
        
        dd_ml_sf <- temp_row_stub
        
    }
    
    saveRDS(dd_ml_sf, "dd_ml_sf.rds")
    
}
