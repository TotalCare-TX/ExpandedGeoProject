#This is the final drill-down model generator. It will take the most granular information we have and turn it into a model. This model will then be saved so that we can use it to predict the volumes at any location on our map.

#Step 1: We need to assemble our data. We will take a list of FEMC locations with lat lon in degrees and use these to gather a new dataset for performing our drilldown ML.

#We will start with our list our FEMCs. Please change this location when able.!!!!!! 



## Reading in Location and Outcome data ---

tcdw_con = dbConnect(RMySQL::MySQL(),
                     dbname= 'TCDW',
                     host= '10.10.70.45',
                     port= 3306,
                     user= 'r.maatouk',
                     password= 'D2taW@r3H0us3&')

geo_analysis_2 <- dbSendQuery(tcdw_con, "SELECT l.thcic_id, l.facility_type, l.lat, l.lon FROM TCDW.geo_texas_facilities l")
location_df = fetch(geo_analysis_2, n = Inf)

geo_analysis_3 <- dbSendQuery(tcdw_con, "SELECT o.thcic_id, o.scaled_ppd FROM TCDW.geo_scaled_fsed_ppd o")
outcome_df = fetch(geo_analysis_3, n = Inf)

dbDisconnect(tcdw_con)

## Joining the dataframes => Facility Type = FEMC => Inner join resulting in only FEMC Facilities
joined_df <- location_df %>% 
  select(thcic_id, lon, lat) %>% 
  filter(!is.na(lat)) %>%
  filter(!is.na(lon)) %>%
  inner_join(outcome_df, by = "thcic_id") %>% 
  filter(!is.na(scaled_ppd)) %>%
  rename(THCIC_ID = thcic_id)

fake_ers <- read_sheet("1UHi_W2J74PevuhLAfMZUBjR8kF0AHo30oqFQmFOKhb4") 
fake_ers <- fake_ers %>% select(-Location)

femc_list <- rbind(joined_df, fake_ers)

femc_list <- femc_list |> 
    dplyr::select(c(scaled_ppd, lat, lon)) |> 
    mutate(lat = as.numeric(lat),
           lon = as.numeric(lon)) |>
    filter(!is.na(lon))
  

source("full_ml_feature_extraction_bel.R")

dd_ml_sf <- tibble()

for (i in 1:femc_list$scaled_ppd) {
    
    dd_ml_sf <- dd_ml_sf |>
        bind_rows(generate_full_ml_sf(lat = femc_list$lat[i], lon = femc_list$lon[i]))
    
}

dd_ml_sf <- dd_ml_sf |>
    mutate(scaled_ppd = femc_list$scaled_ppd)

#Step 2: Now that our data is assembled, we need to run our ML algorithm on this.

#Step 3: Finally, we need to save the resulting model.