# Extracting Feature Importance ---
library(vip)
library(DALEX)
library(iml)

## Applying the Stacked ML Model / Loading Model .rds ---
source("applying_ml.R")

stacked_model <- readRDS("stacked_model.rds")
data_set <- features_df
target <- data_set$Log_Vol
features <- subset(data_set, select = -Log_Vol)

## Extracting models ---
find_xgb_booster <- function(obj) {
  if (inherits(obj, "xgb.Booster")) return(obj)
  if (is.list(obj)) {
    for (el in obj) {
      res <- find_xgb_booster(el)
      if (!is.null(res)) return(res)
    }
  }
  return(NULL)
}

xgb_fit <- find_xgb_booster(stacked_model)
class(xgb_fit)
if (is.null(xgb_fit)) stop("No XGBoost booster found in the loaded object!")

## GLobal Feature Importance ---
vip(xgb_fit, num_features = 39)

## Explanation ---
er_row <- 5
single_er <- features[er_row, , drop = FALSE]

pred_function <- function(m, new_data) {
  predict(m, as.matrix(new_data))
}

explainer <- explain(
  model = xgb_fit,
  data = as.matrix(features),
  y = target,
  predict_function = pred_function,
  label = "XGB ER Model"
)

breakdown <- predict_parts(explainer, new_observation = single_er, type = "break_down")
plot(breakdown)





