library(shiny)
library(bslib)
library(leaflet)
library(shinycssloaders)
library(shinymanager)

ui <- secure_app(
  fluidPage(
    style = "margin:0; padding:0;",  # remove default margins to avoid scroll
    tags$head(
      # ---- Focus username field after short delay for Enter key ----
      tags$script(HTML("
        $(document).ready(function() {
          setTimeout(function() {
            $('#shinymanager-user').focus();
          }, 200); // 200 ms delay
        });
      ")),
      # ---- Map styling ----
      tags$style(HTML("
        body {
          overflow: hidden;  /* prevent scrolling */
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
                # ---- Map container filling remaining screen ----
                shinycssloaders::withSpinner(
                  leafletOutput("prediction_map", height = "calc(100vh - 180px)", width = "100%")
                ),
                textOutput("clicked_coords")
      ),
      
      # ---- TAB 2: Site Prediction ----
      nav_panel("Site Prediction",
                wellPanel(
                  h4("📍 Instructions: Site Prediction"),
                  tags$ol(
                    tags$li("Coordinates from the map will auto-fill below if you clicked on the map."),
                    tags$li("Or enter an address and click 'Geocode Address' to get coordinates."),
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
                  actionButton("predict_button", "Predict")
                ),
                
                wellPanel(
                  textOutput("result"),
                  tableOutput("history_table")
                )
      ),
      id = "tab"
    )
  ),
  enable_enter = TRUE  # Enter key triggers login
)
