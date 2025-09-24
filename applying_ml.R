# Improving ML Model ---

# Getting Data ---
source("Input_features_for_ML_new.R")
geo_data <- features_df %>% dplyr::select(-THCIC_ID)

# ML Functions ---
source("workflow-ml.R")

## Applying the formula and stacked model ---

set.seed(42) # Setting up data splits
geo_data[is.na(geo_data)] <- 0
geo_data_split <- initial_split(geo_data)
geo_data_train <- training(geo_data_split)
geo_data_test <- testing(geo_data_split)

geo_frmla <- as.formula("Log_Vol ~ Accepts_Med + total_pop_10km + median_income + shop_KDE_sigma1km + shop_KDE_sigma3km + shop_KDE_sigma5km + traffic_signals_KDE_sigma1km + traffic_signals_KDE_sigma3km + traffic_signals_KDE_sigma5km + highway_major_KDE_sigma1km + highway_major_KDE_sigma3km + highway_major_KDE_sigma5km + microsoft_housing_KDE_sigma1km + microsoft_housing_KDE_sigma3km + microsoft_housing_KDE_sigma5km + pharmacy_KDE_sigma1km + pharmacy_KDE_sigma3km + pharmacy_KDE_sigma5km + shop_KDE_sigma1km.1 + shop_KDE_sigma3km.1 + shop_KDE_sigma5km.1 + gold_buyer_KDE_sigma1km + gold_buyer_KDE_sigma3km + gold_buyer_KDE_sigma5km + restaurant_KDE_sigma1km + restaurant_KDE_sigma3km + restaurant_KDE_sigma5km + school_KDE_sigma1km + school_KDE_sigma3km + school_KDE_sigma5km + pawnbroker_KDE_sigma1km + pawnbroker_KDE_sigma3km + pawnbroker_KDE_sigma5km + supermarket_KDE_sigma1km + supermarket_KDE_sigma3km + supermarket_KDE_sigma5km + library_KDE_sigma1km + library_KDE_sigma3km + library_KDE_sigma5km")

set.seed(42) # Stacked Model + Evaluation
geo_stack <- model_stacking(geo_data_train, geo_frmla) # Feeding training set into the stacked model

eval_results <- evaluate_model(geo_stack, geo_data_test, geo_frmla)
eval_results$test_results
head(eval_results$predictions)

## Saving the stacked model (.rds)
saveRDS(geo_stack, "stacked_model.rds")

