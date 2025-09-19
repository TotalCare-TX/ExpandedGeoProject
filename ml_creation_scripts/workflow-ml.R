## Tidymodels Workflow Script ---

## Installing & Loading Packages ---
pacman::p_load(dplyr, tidymodels, stacks, spatialsample, sf, brms, MASS, doParallel, xgboost, ranger, modeldata, kknn, stringr)

library(dplyr)
library(tidymodels)
library(stacks)
library(spatialsample)
library(sf)
library(brms)
library(MASS)
library(doParallel)
library(xgboost)
library(ranger)
library(modeldata)
library(kknn)
library(stringr)

## If running through an error related to control_stack_grid(), install stacks package through this script \/
## remotes::install_github("tidymodels/stacks")
## This model will set up a stacking model series for our GeoProject

## Create Geo Formula ---
create_geo_formula <- function(df, outcome_var = "scaled_ppd") {
  predictors <- setdiff(names(df), outcome_var)
  temp <- paste(predictors, collapse = " + ")
  
  as.formula(paste(outcome_var, "~", temp))
}

## Model Stacking Function ---
library(tidymodels)
library(stacks)

model_stacking <- function(df, frmla) {
  
  # 5-Fold cross-validation
  folds <- vfold_cv(df, v = 5)
  df_rec <- recipe(frmla, data = df)
  metric <- metric_set(rmse)
  
  ctrl_res <- control_stack_resamples()
  
  # LASSO Model
  lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet")
  
  lasso_rec <- df_rec %>% 
    step_log(all_numeric_predictors(), offset = 1) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
  
  lasso_wflow <- workflow() %>% add_model(lasso_spec) %>% add_recipe(lasso_rec)
  
  lasso_res <- tune_grid(
    lasso_wflow, 
    resamples = folds, 
    metrics = metric, 
    grid = 4,
    control = ctrl_res
  )
  
  # PCA Model
  pca_spec <- linear_reg() %>% set_engine("lm")
  
  pca_rec <- df_rec %>% 
    step_log(all_numeric_predictors(), offset = 1) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_pca(all_numeric_predictors(), num_comp = tune())
  
  pca_wflow <- workflow() %>% add_model(pca_spec) %>% add_recipe(pca_rec)
  
  pca_res <- tune_grid(
    pca_wflow,
    resamples = folds,
    metrics = metric,
    grid = 4,
    control = ctrl_res
  )
  
  # KNN Model
  knn_spec <- nearest_neighbor(mode = "regression", neighbors = tune()) %>% set_engine("kknn")
  
  knn_rec <- df_rec %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_mean(all_numeric_predictors()) %>% 
    step_normalize(all_numeric_predictors())
  
  knn_wflow <- workflow() %>% add_model(knn_spec) %>% add_recipe(knn_rec)
  
  knn_res <- tune_grid(
    knn_wflow,
    resamples = folds,
    metrics = metric,
    grid = 4,
    control = ctrl_res
  )
  
  # Linear Regression Model
  lm_spec <- linear_reg() %>% set_engine("lm")
  
  lm_rec <- df_rec %>% 
    step_log(all_numeric_predictors(), offset = 1) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
  
  lm_wflow <- workflow() %>% add_model(lm_spec) %>% add_recipe(lm_rec)
  
  lm_res <- fit_resamples(lm_wflow, resamples = folds, metrics = metric, control = ctrl_res)
  
  # Random Forest Model
  rf_spec <- rand_forest(trees = 1000) %>% set_engine("ranger", importance = "permutation") %>% set_mode("regression")
  
  rf_rec <- df_rec %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_mean(all_numeric_predictors())
  
  rf_wflow <- workflow() %>% add_model(rf_spec) %>% add_recipe(rf_rec)
  
  rf_res <- fit_resamples(rf_wflow, resamples = folds, metrics = metric, control = ctrl_res)
  
  # XGBoost Model
  xgb_spec <- boost_tree(trees = 1000) %>% set_engine("xgboost") %>% set_mode("regression")
  
  xgb_rec <- df_rec %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_mean(all_numeric_predictors())
  
  xgb_wflow <- workflow() %>% add_model(xgb_spec) %>% add_recipe(xgb_rec)
  
  xgb_res <- fit_resamples(xgb_wflow, resamples = folds, metrics = metric, control = ctrl_res)
  
  # Stacking
  df_st <- stacks() %>% 
    add_candidates(lasso_res) %>% 
    add_candidates(pca_res) %>% 
    add_candidates(knn_res) %>% 
    add_candidates(lm_res) %>% 
    add_candidates(rf_res) %>% 
    add_candidates(xgb_res) %>% 
    blend_predictions() %>% 
    fit_members()
  
  return(df_st)
}


library(workflows)
library(parsnip)
library(recipes)

fit_base_learners <- function(stack, df, outcome_var) {
  
  fitted_learners <- list()
  
  for(model_name in names(stack$model_defs)) {
    message("Fitting full model for: ", model_name)
    
    # Recreate recipe (fallback to simple dummy + ZV)
    rec <- stack$model_defs[[model_name]]$actions$recipe
    if(is.null(rec)) {
      rec <- recipe(as.formula(paste(outcome_var, "~ .")), data = df) %>%
        step_dummy(all_nominal_predictors()) %>%
        step_zv(all_predictors())
    }
    
    # Rebuild model spec based on model name
    model_spec <- switch(model_name,
                         "lasso_res" = linear_reg(penalty = 0.01, mixture = 1) %>% set_engine("glmnet"),
                         "pca_res"   = linear_reg() %>% set_engine("lm"),
                         "knn_res"   = nearest_neighbor(mode = "regression", neighbors = 5) %>% set_engine("kknn"),
                         "lm_res"    = linear_reg() %>% set_engine("lm"),
                         "rf_res"    = rand_forest(trees = 1000) %>% set_engine("ranger", importance = "permutation") %>% set_mode("regression"),
                         "xgb_res"   = boost_tree(trees = 1000) %>% set_engine("xgboost") %>% set_mode("regression"),
                         stop("Unknown model: ", model_name))
    
    # Build workflow
    wflow <- workflow() %>% add_model(model_spec) %>% add_recipe(rec)
    
    # Fit full model
    fitted_model <- fit(wflow, data = df)
    
    fitted_learners[[model_name]] <- fitted_model
  }
  
  return(fitted_learners)
}
