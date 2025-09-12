library(shiny)
library(raster)
library(terra)
library(leaflet)
library(leafem)

server <- function(input, output, session) {
  
  # Reactive value to store clicked coords
  clicked_coords <- reactiveVal(NULL)
  
  # -------------------
  # TAB 1: Map Prediction
  # -------------------
  output$prediction_map <- renderLeaflet({
    validate(need(file.exists("prediction_map.tif"), "GeoTIFF not found."))
    
    r <- terra::rast("prediction_map.tif")
    if (tryCatch(terra::crs(r, proj = TRUE) != "EPSG:4326",
                 error = function(e) TRUE)) {
      r <- terra::project(r, "EPSG:4326")
    }
    
    rr <- raster::raster(r)
    vals <- raster::values(rr)
    pal  <- colorNumeric("viridis", domain = range(vals, na.rm = TRUE), na.color = NA)
    
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      addRasterImage(rr, colors = pal, opacity = 0.8, project = FALSE) |>
      addLegend("bottomright", pal = pal, values = vals, title = "Value") |>
      fitBounds(xmin(rr), ymin(rr), xmax(rr), ymax(rr)) |>
      addMouseCoordinates()
  })
  
  # Capture click on the map
  observeEvent(input$prediction_map_click, {
    lat <- input$prediction_map_click$lat
    lon <- input$prediction_map_click$lng
    coords <- paste(round(lat, 5), round(lon, 5))
    clicked_coords(coords)
    
    # Update UI text in tab 1
    output$clicked_coords <- renderText({
      paste("Lat, Lon:", coords)
    })
    
    # Autofill x1 and x2 in tab 2
    updateTextInput(session, "x1", value = round(lat, 5))
    updateTextInput(session, "x2", value = round(lon, 5))
  })
  
  
  # -------------------
  # TAB 2: Site Prediction
  # -------------------
  model <- reactiveVal(NULL)
  
  # Dummy geocoding example
  observeEvent(input$geocode_btn, {
    if (nzchar(input$address)) {
      coords <- data.frame(lat = 29.76, lon = -95.37)  # Houston, TX
      output$coords <- renderText({
        paste("Coordinates:", coords$lat, ",", coords$lon)
      })
      
      # Autofill into predictors
      updateTextInput(session, "x1", value = coords$lat)
      updateTextInput(session, "x2", value = coords$lon)
    }
  })
  
  # Model training
  observeEvent(input$train_model, {
    df <- read.csv("training_data.csv")
    trained <- lm(y ~ x1 + x2, data = df)
    model(trained)
    showNotification("Model retrained successfully!", type = "message")
  })
  
  # Prediction history
  history <- reactiveVal(data.frame(x1 = numeric(),
                                    x2 = numeric(),
                                    prediction = numeric()))
  
  observeEvent(input$predict_button, {
    validate(
      need(!is.null(model()), "Please train the model first."),
      need(!is.na(as.numeric(input$x1)), "x1 must be a number"),
      need(!is.na(as.numeric(input$x2)), "x2 must be a number")
    )
    
    new_data <- data.frame(x1 = as.numeric(input$x1),
                           x2 = as.numeric(input$x2))
    prediction <- predict(model(), new_data)
    
    old <- history()
    history(rbind(old, cbind(new_data, prediction)))
    
    output$result <- renderText({
      paste("Prediction:", round(prediction, 2))
    })
    output$history_table <- renderTable(history())
  })
}
