library(shiny)
library(bslib)
library(leaflet)

shinyUI(
  fluidPage(
    # ---- CSS to make map taller ----
    tags$head(
      tags$style(HTML("
        #prediction_map {
          height: 90vh !important;  
          width: 100% !important;   
        }
      "))
    ),
    
    navset_pill(
      # -------------------
      # TAB 1: Map Prediction
      # -------------------
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
                wellPanel(
                  h5("Selected Coordinates:"),
                  textOutput("clicked_coords")
                )
      ),
      
      # -------------------
      # TAB 2: Site Prediction
      # -------------------
      nav_panel("Site Prediction",
                wellPanel(
                  h4("📍 Instructions: Site Prediction"),
                  tags$ol(
                    tags$li("Coordinates from the map will auto-fill below if you clicked on the map."),
                    tags$li("Or enter an address and click 'Geocode Address' to get coordinates."),
                    tags$li("Click 'Train Model' to load the model using `training_data.csv`."),
                    tags$li("Click 'Predict' to generate a prediction using the chosen coordinates (or manual X1/X2 values).")
                  )
                ),
                
                # Inputs for model
                wellPanel(
                  textInput("address", "Enter Address:", ""),
                  actionButton("geocode_btn", "Geocode Address"),
                  textOutput("coords"),
                  tags$hr(),
                  actionButton("train_model", "Train Model"),
                  textInput("x1", "Enter X1 (lat):"),
                  textInput("x2", "Enter X2 (lon):"),
                  actionButton("predict_button", "Predict")
                ),
                
                # Outputs
                wellPanel(
                  textOutput("result"),
                  tableOutput("history_table")
                )
      )
    ),
    id = "tab"
  )
)
