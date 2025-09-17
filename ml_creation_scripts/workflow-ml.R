## Tidymodels Workflow Script ---

## Installing & Loading Packages ---
# pacman::p_load(dplyr, tidymodels, stacks, spatialsample, sf, brms, MASS, doParallel, xgboost, ranger, modeldata, kknn, stringr)

# library(dplyr)
# library(tidymodels)
# library(stacks)
# library(spatialsample)
# library(sf)
# library(brms)
# library(MASS)
# library(doParallel)
# library(xgboost)
# library(ranger)
# library(modeldata)
# library(kknn)
# library(stringr)

## If running through an error related to control_stack_grid(), install stacks package through this script \/
# remotes::install_github("tidymodels/stacks")

## Dataset 1 ---
# geo_data <- read.csv("ml_dataset.csv")
geo_data <- read.csv("ml_with_all_rasters.csv")
geo_data <- geo_data %>% dplyr::select(-THCIC_ID)

## Creating GeoFormula Function ---
# create_geo_formula <- function(scaled_ppd = "scaled_ppd",
#                                total_pop = "total_pop",
#                                weighted_median_income = "weighted_median_income",
#                                Accepts_Med = "Accepts_Med",
#                                Per_MedMed = "Per_MedMed",
#                                DFW_highways_merged_kde = "DFW_highways_merged_kde",
#                                DFW_restaurant_merged_kde = "DFW_restaurant_merged_kde",
#                                coords = c("lon", "lat")) {
#   
#   temp_formula <- "scaled_ppd ~ total_pop + weighted_median_income + Accepts_Med +"
#   temp_formula <- str_c(temp_formula, paste(c(Per_MedMed, DFW_highways_merged_kde, DFW_restaurant_merged_kde, coords), collapse = " + "))
#   
#   return(as.formula(temp_formula))
#   
# }

## This model will set up a stacking model series for our GeoProject ---

## Model Stacking Function ---
model_stacking <- function(df, frmla) {
  
  ## 5 Fold cross validation
  folds <- rsample::vfold_cv(df, v = 5)
  df_rec <- recipe(frmla, data = df)
  metric <- metric_set(rmse)
  
  ## Stack control parameters
  ctrl_grid <- control_stack_grid()
  ctrl_res <- control_stack_resamples()
  
  ## Defining the ML models
  
  ## LASSO Model
  lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
    set_engine("glmnet")
  
  lasso_rec <- df_rec %>% 
    step_log(all_numeric_predictors(), offset = 1) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
  
  lasso_wflow <- workflow() %>% 
    add_model(lasso_spec) %>% 
    add_recipe(lasso_rec)
  
  lasso_res <- tune_grid(lasso_wflow, resamples = folds, metrics = metric, grid = 4, control = ctrl_res)
  
  ## PCA Model
  pca_spec <- linear_reg() %>% 
    set_engine("lm")
  
  pca_rec <- df_rec %>% 
    step_log(all_numeric_predictors(), offset = 1) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_pca(all_numeric_predictors(), num_comp = tune())
  
  pca_wflow <- workflow() %>% 
    add_model(pca_spec) %>% 
    add_recipe(pca_rec)
  
  pca_res <- tune_grid(pca_wflow, resamples = folds, metrics = metric, grid = 4, control = ctrl_res)
  
  ## KNN Model
  knn_spec <- nearest_neighbor(mode = "regression", neighbors = tune("k")) %>% 
    set_engine("kknn")
  
  knn_rec <- df_rec %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_mean(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors())
  
  knn_wflow <- workflow() %>% 
    add_model(knn_spec) %>% 
    add_recipe(knn_rec)
  
  knn_res <- tune_grid(knn_wflow, resamples = folds, metrics = metric, grid = 4, control = ctrl_grid)
  
  ## Linear Regression Model
  lm_spec <- linear_reg() %>% 
    set_engine("lm")
  
  lm_rec <- df_rec %>% 
    step_log(all_numeric_predictors(), offset = 1) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
  
  lm_wflow <- workflow() %>% 
    add_model(lm_spec) %>% 
    add_recipe(lm_rec)
  
  lm_res <- fit_resamples(lm_wflow, resamples = folds, metrics = metric, control = ctrl_res)
  
  ## Random Forest Model
  rf_spec <- rand_forest(trees = 1000) %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  rf_rec <- df_rec %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_mean(all_numeric_predictors())
  
  rf_wflow <- workflow() %>% 
    add_model(rf_spec) %>% 
    add_recipe(rf_rec)
  
  rf_res <- fit_resamples(rf_wflow, resamples = folds, metrics = metric, control = ctrl_res)
  
  ## XGBoost Model
  xgb_spec <- boost_tree(trees = 1000) %>% 
    set_engine("xgboost") %>% 
    set_mode("regression")
  
  xgb_rec <- df_rec %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_mean(all_numeric_predictors())
  
  xgb_wflow <- workflow() %>% 
    add_model(xgb_spec) %>% 
    add_recipe(xgb_rec)
  
  xgb_res <- fit_resamples(xgb_wflow, resamples = folds, metrics = metric, control = ctrl_res)
  
  ## Setting up the stacks
  df_st <- stacks() %>% 
    add_candidates(lasso_res) %>% 
    add_candidates(pca_res) %>% 
    add_candidates(knn_res) %>% 
    add_candidates(lm_res) %>% 
    add_candidates(rf_res) %>% 
    add_candidates(xgb_res)
  
  df_st <- df_st %>% 
    blend_predictions()
  
  df_st <- df_st %>% 
    fit_members()

  return(df_st)
}

## Evaluate Model ---

evaluate_model <- function(model, df, frmla) {
  
  ## Test data -> tibble
  df <- as.tibble(df)
  outcome_var <- all.vars(frmla)[1]
  
  preds <- predict(model, new_data = df) %>% 
    rename(.pred = .pred)
  
  preds <- preds %>% 
    bind_cols(y_true = df %>% 
                pull(outcome_var))
  
  results <- metrics(preds, truth = y_true, estimate = .pred)
  
  return(
    list(
      predictions = preds,
      test_results = results
    )
  )
}

## Applying the formula and stacked model ---

set.seed(42) # Setting up data splits
geo_data[is.na(geo_data)] <- 0
geo_data_split <- initial_split(geo_data)
geo_data_train <- training(geo_data_split)
geo_data_test <- testing(geo_data_split)

# geo_frmla <- create_geo_formula() # Formula
geo_frmla <- as.formula("Log_Vol ~ Accepts_Med + total_pop_10km + median_income + shop_KDE_sigma1km + shop_KDE_sigma3km + shop_KDE_sigma5km + traffic_signals_KDE_sigma1km + traffic_signals_KDE_sigma3km + traffic_signals_KDE_sigma5km + highway_major_KDE_sigma1km + highway_major_KDE_sigma3km + highway_major_KDE_sigma5km + microsoft_housing_KDE_sigma1km + microsoft_housing_KDE_sigma3km + microsoft_housing_KDE_sigma5km + pharmacy_KDE_sigma1km + pharmacy_KDE_sigma3km + pharmacy_KDE_sigma5km + shop_KDE_sigma1km.1 + shop_KDE_sigma3km.1 + shop_KDE_sigma5km.1 + gold_buyer_KDE_sigma1km + gold_buyer_KDE_sigma3km + gold_buyer_KDE_sigma5km + restaurant_KDE_sigma1km + restaurant_KDE_sigma3km + restaurant_KDE_sigma5km + school_KDE_sigma1km + school_KDE_sigma3km + school_KDE_sigma5km + pawnbroker_KDE_sigma1km + pawnbroker_KDE_sigma3km + pawnbroker_KDE_sigma5km + supermarket_KDE_sigma1km + supermarket_KDE_sigma3km + supermarket_KDE_sigma5km + library_KDE_sigma1km + library_KDE_sigma3km + library_KDE_sigma5km")

set.seed(42) # Stacked Model + Evaluation
geo_stack <- model_stacking(geo_data_train, geo_frmla) # Feeding training set into the stacked model

eval_results <- evaluate_model(geo_stack, geo_data_test, geo_frmla)
eval_results$test_results
head(eval_results$predictions)

