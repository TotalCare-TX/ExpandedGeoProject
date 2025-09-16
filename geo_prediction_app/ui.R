library(shiny)
library(bslib)
library(leaflet)

shinyUI(
  fluidPage(
    tags$head(
      tags$style(HTML("
        #prediction_map {
          height: 90vh !important;
          width: 100% !important;
        }
      "))
    ),
    
    navset_pill(
      # ---- TAB 1: Map Prediction ----
      nav_panel("Map Prediction",
                wellPanel(
                  h4("📍 Instructions: Map Prediction"),
                  tags$ol(
                    tags$li("Click anywhere on the map to select a location."),
                    tags$li("The latitude/longitude of your click will be displayed below."),
                    tags$li("These coordinates will also be sent automatically to the Site Prediction tab.")
                  )
                ),
                leafletOutput("prediction_map"),
                textOutput("clicked_coords")  # Show clicked coordinates
      ),
      
      # ---- TAB 2: Site Prediction ----
      nav_panel("Site Prediction",
                wellPanel(
                  h4("📍 Instructions: Site Prediction"),
                  tags$ol(
                    tags$li("Coordinates from the map will auto-fill below if you clicked on the map."),
                    tags$li("Or enter an address and click 'Geocode Address' to get coordinates."),
                    tags$li("Click 'Train Model' to load the model using `training_data.csv`."),
                    tags$li("Click 'Predict' to run the ML model using the chosen coordinates.")
                  )
                ),
                
                wellPanel(
                  textInput("address", "Enter Address:", ""),
                  actionButton("geocode_btn", "Geocode Address"),
                  textOutput("coords"),
                  tags$hr(),
                  textInput("x1", "Latitude:", ""),
                  textInput("x2", "Longitude:", ""),
                  actionButton("train_model", "Train Model"),
                  actionButton("predict_button", "Predict")
                ),
                
                wellPanel(
                  textOutput("result"),
                  tableOutput("history_table")
                )
      )
    ),
    id = "tab"
  )
)
