#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)


# Define UI for application that draws a histogram
fluidPage(

    navset_pill(
        
        nav_panel("Map Prediction",
                  wellPanel("Instructions for using the map"),
                  
                  wellPanel(
                      leafletOutput("prediction_map", height = 600)
                  )
                  ),
        
        nav_panel("Site Prediction",
                  wellPanel("Instructions for using site prediction"),
                  
                  wellPanel("Address Entry"),
                  
                  wellPanel("Results display")
                  
                  )
        
    ),
    id = "tab"
    
)
