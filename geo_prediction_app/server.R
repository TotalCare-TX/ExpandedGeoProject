#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(raster)
library(leaflet)
library(terra)
library(leafem)

# Define server logic required to draw a histogram
function(input, output, session) {

    output$map <- renderLeaflet({
        # Load your GeoTIFF
        r <- terra::rast("prediction_map.tif")
        # Reproject to EPSG:4326 if needed
        if (tryCatch(terra::crs(r, proj=TRUE) != "EPSG:4326", error = function(e) TRUE)) {
            r <- terra::project(r, "EPSG:4326")
        }
        rr <- raster::raster(r)
        vals <- raster::values(rr)
        pal  <- colorNumeric("viridis", domain = range(vals, na.rm=TRUE), na.color = NA)
        
        leaflet() |>
            addProviderTiles("CartoDB.Positron") |>
            addRasterImage(rr, colors = pal, opacity = 0.8, project = FALSE) |>
            addLegend("bottomright", pal = pal, values = vals, title = "Value") |>
            fitBounds(lng1 = xmin(rr), lat1 = ymin(rr), lng2 = xmax(rr), lat2 = ymax(rr)) |>
            addMouseCoordinates()
    })

}
