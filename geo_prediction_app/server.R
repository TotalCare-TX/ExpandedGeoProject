library(shiny)
library(raster)
library(terra)
library(leaflet)
library(leafem)
library(httr)
library(jsonlite)
library(stringr)

# ---- Load external geocoding function ----
source("find_address_coordinates.R")  # This loads get_coordinates()

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
  
  # Capture clicks on the map
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
  model <- reactiveVal(NULL)
  history <- reactiveVal(data.frame(lat = numeric(), lon = numeric(), prediction = numeric()))
  
  # Geocode address
  observeEvent(input$geocode_btn, {
    res <- get_coordinates(input$address)  # <- now using external function
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
  
  # Train model (placeholder)
  observeEvent(input$train_model, {
    df <- read.csv("training_data.csv")
    trained <- lm(y ~ x1 + x2, data = df)  # replace with your ML pipeline later
    model(trained)
    showNotification("Model trained successfully!", type = "message")
  })
  
  # Predict
  observeEvent(input$predict_button, {
    validate(
      need(!is.null(model()), "Please train the model first."),
      need(input$x1 != "" && input$x2 != "", "Coordinates are required")
    )
    
    new_data <- data.frame(x1 = as.numeric(input$x1), x2 = as.numeric(input$x2))
    prediction <- predict(model(), new_data)
    
    old <- history()
    history(rbind(old, cbind(new_data, prediction)))
    
    output$result <- renderText({
      paste("Prediction:", round(prediction, 2))
    })
    output$history_table <- renderTable(history())
  })
})
