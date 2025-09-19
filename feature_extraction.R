# Feature Importance & Local Explanation Script ---

# Libraries ---
library(tidymodels)
library(vip)
library(iml)
library(dplyr)

# Load models and data ---
source("raster_ml_generation_bel.R")

outcome_var <- "scaled_ppd"
stacked_model <- readRDS("raster_model.rds")
base_models <- fit_base_learners(stacked_model, ml_table, "scaled_ppd")

# Global Feature Importance ---
# Random Forest ---
rf_model <- base_models$rf_res$fit$fit$fit
rand_for_feat_imp <- vip(rf_model, num_features = 20, geom = "col")
rand_for_feat_imp

# XGBoost
xgb_model <- base_models$xgb_res$fit$fit$fit  # xgboost object
xgb_feat_imp <- vip(xgb_model, num_features = 20, geom = "col")
xgb_feat_imp

# 3. Local Explanation for a single prediction
# Select a row of interest (e.g., ER 1 on day 100)
row_index <- 100
X <- ml_table %>% dplyr::select(-outcome_var)
x_interest <- X[row_index, ]

# Function to compute Shapley values for a given model
explain_local <- function(model, X, x_interest, model_name) {
  predictor <- Predictor$new(model, data = X, y = ml_table[[outcome_var]])
  shapley <- Shapley$new(predictor, x.interest = x_interest)
  
  cat("\nShapley values for", model_name, "prediction:\n")
  print(shapley$results %>% arrange(desc(abs(phi))) %>% head(10))
  
  # Plot
  plot(shapley) + ggtitle(paste("Local Feature Contributions -", model_name))
}

# Random Forest local explanation
explanation_rdf <- explain_local(rf_model, X, x_interest, "Random Forest")

