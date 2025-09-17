library(shiny)
library(raster)
library(terra)
library(leaflet)
library(leafem)
library(httr)
library(jsonlite)
library(stringr)
library(randomForest)

# ---- Load external geocode function ----
source("find_address_coordinates.R")

# ---- Load pre-trained model ----
if (file.exists("model.rds")) {
  model <- readRDS("model.rds")
} else {
  stop("⚠️ model.rds not found. Please run build_model.R first.")
}

# ---- Shiny Server ----
shinyServer(function(input, output, session) {
  
  # ---- TAB 1: Map Prediction ----
  output$prediction_map <- renderLeaflet({
    r <- terra::rast("prediction_map.tif")
    
    if (tryCatch(terra::crs(r, proj = TRUE) != "EPSG:4326", error = function(e) TRUE)) {
      r <- terra::project(r, "EPSG:4326")
    }
    
    rr <- raster::raster(r)
    vals <- raster::values(rr)
    pal <- colorNumeric("viridis", domain = range(vals, na.rm = TRUE), na.color = NA)
    
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      addRasterImage(rr, colors = pal, opacity = 0.8, project = FALSE) |>
      addLegend("bottomright", pal = pal, values = vals, title = "Value") |>
      fitBounds(xmin(rr), ymin(rr), xmax(rr), ymax(rr)) |>
      addMouseCoordinates()
  })
  
  # Capture map clicks
  observeEvent(input$prediction_map_click, {
    lat <- input$prediction_map_click$lat
    lng <- input$prediction_map_click$lng
    updateTextInput(session, "x1", value = round(lat, 5))
    updateTextInput(session, "x2", value = round(lng, 5))
    output$clicked_coords <- renderText({
      paste("Clicked coordinates:", round(lat, 5), ",", round(lng, 5))
    })
  })
  
  # ---- TAB 2: Site Prediction ----
  history <- reactiveVal(data.frame(lat = numeric(), lon = numeric(), prediction = character(), confidence = numeric()))
  
  # Geocode address
  observeEvent(input$geocode_btn, {
    res <- get_coordinates(input$address)
    if (res$status == "success") {
      updateTextInput(session, "x1", value = res$latitude)
      updateTextInput(session, "x2", value = res$longitude)
      output$coords <- renderText({
        paste("Geocoded:", res$display_name, "→ Lat:", res$latitude, "Lon:", res$longitude)
      })
    } else {
      output$coords <- renderText(res$message)
    }
  })
  
  # ---- Prediction ----
  observeEvent(input$predict_button, {
    
    # Convert inputs to numeric
    lat <- suppressWarnings(as.numeric(input$x1))
    lon <- suppressWarnings(as.numeric(input$x2))
    
    # ✅ Manual validation
    if (is.na(lat) || is.na(lon)) {
      showNotification("Please enter valid latitude and longitude!", type = "error")
      return(NULL)
    }
    
    new_data <- data.frame(lat = lat, lon = lon)
    
    pred_class <- predict(model, new_data)
    pred_probs <- predict(model, new_data, type = "prob")
    
    confidence <- round(100 * max(pred_probs), 1)
    
    # Save history
    old <- history()
    history(rbind(old, data.frame(lat = lat, lon = lon,
                                  prediction = as.character(pred_class),
                                  confidence = confidence)))
    
    output$result <- renderText({
      paste("Prediction:", pred_class, " (Confidence:", confidence, "%)")
    })
    
    output$history_table <- renderTable(history())
  })
})
