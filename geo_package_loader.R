# This script loads all packages associated with the project using renv.

# R/packages.R

# List of required packages
packages <- c(
    "tidyverse",
    "tidymodels",
    "usethis",
    "sf",
    "tidygeocoder",
    "osmdata",
    "tigris",
    "digest",
    "R.utils",
    "leaflet",
    "raster",
    "httr",
    "jsonlite",
    "units",
    "future.apply",
    "lwgeom",
    "terra",
    "shiny",
    "bslib",
    "leafem",
    "workflowsets",
    "glmnet",
    "ranger",
    "xgboost",
    "INLA",
    "mgcv"
    
)

# Install missing packages using renv
missing <- packages[!packages %in% installed.packages()[,"Package"]]

if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    renv::install(missing)
    
    install.packages(
        "INLA",
        repos = c(getOption("repos"),
                  INLA = "https://inla.r-inla-download.org/R/stable"),
        dep = TRUE
    )
    
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))
