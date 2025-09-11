# ui.R
# --------------------------
# Shiny UI for ER Volume Explorer
# - Left: controls (address entry, optional lat/lon, buttons)
# - Right: Leaflet map with raster overlay (Texas) + legend
# - Bottom: predicted value + site metrics table
# --------------------------

# NOTE: Per your workflow, keep package loading in a separate script if you prefer.
# library(shiny)
# library(leaflet)
# library(DT)

shinyUI(fluidPage(
    titlePanel("Texas ER Volume Explorer"),
    
    sidebarLayout(
        sidebarPanel(
            width = 4,
            h4("Find a site"),
            textInput("address", "Address", placeholder = "123 Main St, City, TX"),
            div(
                style = "margin-top: -10px; font-size: 12px; color: #666;",
                "Tip: You can also supply lat/lon directly (overrides address)."
            ),
            numericInput("lat", "Latitude (optional)", value = NA, step = 0.0001),
            numericInput("lon", "Longitude (optional)", value = NA, step = 0.0001),
            actionButton("go", "Predict at this location", class = "btn-primary"),
            
            tags$hr(),
            h4("Model"),
            fileInput("model_file", "Load trained model (.rds)", accept = ".rds"),
            helpText("If not provided, a simple demo model will be used."),
            
            tags$hr(),
            h4("Map"),
            checkboxInput("show_marker", "Show site marker", TRUE),
            checkboxInput("show_popup", "Show popup with prediction", TRUE),
            helpText("Raster shows predicted ER volume over Texas (demo layer).")
        ),
        
        mainPanel(
            width = 8,
            leafletOutput("map", height = 600),
            tags$hr(),
            fluidRow(
                column(
                    width = 4,
                    h4("Prediction"),
                    verbatimTextOutput("pred_text")
                ),
                column(
                    width = 8,
                    h4("Site metrics"),
                    DT::dataTableOutput("metrics_table")
                )
            )
        )
    )
))
