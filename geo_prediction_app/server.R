library(shiny)
library(raster)
library(terra)
library(leaflet)
library(leafem)

source("find_address_coordinates.R")

server <- function(input, output, session) {
  
  v <- reactiveValues()
  
  v$lon <- ""
  v$lat <- ""
  
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

    vals <- terra::values(r)
    pal  <- colorNumeric("viridis", domain = range(vals, na.rm = TRUE), na.color = NA)
    
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      leaflet::addRasterImage(r, colors = pal, opacity = 0.8, project = FALSE) |>
      addLegend("bottomright", pal = pal, values = vals, title = "Value") |>
      fitBounds(xmin(r), ymin(r), xmax(r), ymax(r)) |>
      addMouseCoordinates()
  })
  
  
  # -------------------
  # TAB 2: Site Prediction
  # -------------------
  model <- reactiveVal(NULL)
  
  # Dummy geocoding example
  observeEvent(input$geocode_btn, {
    if (nzchar(input$address)) {
      temp_coordinates <- get_coordinates(input$address)
      v$lat <- temp_coordinates$latitude
      v$lon <- temp_coordinates$longitude
      output$coords <- renderText({
        paste("Coordinates:", v$lat, ",", v$lon)
      })
      
    }
  })
  
  # Model training
  observeEvent(input$train_model, {
    df <- read.csv("training_data.csv")
    trained <- lm(y ~ x1 + x2, data = df)
    model(trained)
    showNotification("Model retrained successfully!", type = "message")
  })
  
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
