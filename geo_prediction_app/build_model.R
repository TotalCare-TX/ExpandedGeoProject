# build_model.R

set.seed(123)
library(randomForest)

# ---- Fake training dataset ----
n <- 200
lat <- runif(n, 33, 37)       # Example latitude range
lon <- runif(n, -119, -115)   # Example longitude range

# Fake labeling rule (you can replace with real rules/data later)
label <- ifelse(lat > 35 & lon < -117, "Good", "Bad")

# Dataframe
training_data <- data.frame(lat, lon, label = factor(label))

# Save dataset
write.csv(training_data, "training_data.csv", row.names = FALSE)

# ---- Train random forest model ----
model <- randomForest(label ~ lat + lon, data = training_data, ntree = 200)

# Save trained model
saveRDS(model, "model.rds")

cat("✅ Model and training data saved.\n")

