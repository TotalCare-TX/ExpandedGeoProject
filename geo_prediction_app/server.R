# server.R
# --------------------------
# Shiny server for ER Volume Explorer
# - Creates a demo raster over Texas and renders it in Leaflet
# - Accepts address (or optional lat/lon), builds feature set (placeholder),
#   runs prediction, and displays metrics + marker on the map
# --------------------------

# NOTE: Keep package loading in your separate loader if desired.
# library(shiny)
# library(leaflet)
# library(DT)
# library(terra)     # for rasters
# library(raster)    # for addRasterImage compatibility
# library(scales)    # for pretty breaks/labels (optional)

# --------------------------
# ---- CONFIG / HELPERS ----
# --------------------------

# Approximate Texas center and zoom
TX_CENTER <- list(lat = 31.0, lon = -99.0, zoom = 6)

# 1) PLACEHOLDER GEOCODER --------------------------------------------
# Replace this with your preferred geocoding (e.g., Google, Mapbox, ArcGIS, or tidygeocoder)
geocode_address <- function(address) {
    # TODO: Implement real geocoding. Must return c(lat = ..., lon = ...)
    # For now, return NA to force lat/lon entry unless user supplies coordinates.
    c(lat = NA_real_, lon = NA_real_)
}

# 2) PLACEHOLDER FEATURE BUILDER -------------------------------------
# Replace this with your real feature engineering function
# Given lat/lon, acquire all predictor variables for your model
get_site_features <- function(lat, lon) {
    # TODO: Implement your feature acquisition logic here
    # Return a one-row data.frame with named columns matching your model’s training features.
    data.frame(
        # Example placeholders (replace with your actual features):
        population_density_10km = NA_real_,
        driving_time_to_nearest_ER = NA_real_,
        median_age = NA_real_,
        accepts_medicaid = NA,   # logical or numeric
        housing_density_km2 = NA_real_,
        longitude = lon,
        latitude = lat,
        stringsAsFactors = FALSE
    )
}

# 3) MODEL LOADER AND PREDICTOR --------------------------------------
# Try to load a model from input; otherwise create a toy demo model.
# Your real model should accept a data.frame with your engineered features.
make_demo_model <- function() {
    # Simple demo: predicted volume = 30 + (fake lat/lon linear terms)
    structure(
        list(
            type = "demo_linear",
            predict_fun = function(newdata) {
                base <- 30
                adj  <- 2 * (newdata$latitude %||% 0) - 1.5 * (newdata$longitude %||% 0)
                pmax(0, base + adj)
            }
        ),
        class = "demo_er_model"
    )
}

`%||%` <- function(x, y) if (is.null(x) || all(is.na(x))) y else x

predict_er_volume <- function(model_obj, newdata) {
    if (inherits(model_obj, "demo_er_model")) {
        return(model_obj$predict_fun(newdata))
    }
    # Generic fallback: try stats::predict
    pred <- try({
        predict(model_obj, newdata = newdata)
    }, silent = TRUE)
    
    if (inherits(pred, "try-error")) {
        stop("Model predict failed. Ensure your model supports predict(newdata=...).")
    }
    as.numeric(pred)
}

# 4) DEMO TEXAS RASTER ------------------------------------------------
# Create a toy raster over an approximate Texas bounding box
# (Replace this with your real predicted raster.)
make_demo_texas_raster <- function() {
    # Rough Texas bbox (lon/lat): west=-106.65, east=-93.51, south=25.84, north=36.5
    r <- terra::rast(
        nrows = 250, ncols = 350,
        xmin = -106.65, xmax = -93.51, ymin = 25.84, ymax = 36.50,
        crs = "EPSG:4326"
    )
    set.seed(42)
    # Create a smooth-ish gradient + noise to mimic predicted volumes
    x <- terra::xFromCol(r, 1:ncol(r))
    y <- terra::yFromRow(r, 1:nrow(r))
    grid <- outer(y, x, function(yy, xx) 50 + 0.9*(yy - 31) - 0.7*(xx + 99))
    noise <- matrix(rnorm(ncell(r), sd = 3), nrow = nrow(r), ncol = ncol(r))
    terra::values(r) <- as.vector(grid + noise)
    r
}

# Convert terra rast -> raster for leaflet::addRasterImage
to_raster_pkg <- function(terra_rast) {
    raster::raster(terra_rast)
}

# --------------------------
# ------- SERVER -----------
# --------------------------

shinyServer(function(input, output, session) {
    
    # Reactive: load model if provided; else demo
    model_obj <- reactiveVal(make_demo_model())
    
    observeEvent(input$model_file, {
        req(input$model_file$datapath)
        obj <- try(readRDS(input$model_file$datapath), silent = TRUE)
        if (inherits(obj, "try-error")) {
            showNotification("Failed to load model .rds. Using demo model.", type = "error")
            model_obj(make_demo_model())
        } else {
            model_obj(obj)
            showNotification("Model loaded.", type = "message")
        }
    }, ignoreInit = TRUE)
    
    # Prepare the raster once
    tx_rast <- make_demo_texas_raster()
    tx_rast_rasterpkg <- to_raster_pkg(tx_rast)
    
    # Color scale for raster (viridis-like without external deps)
    rng <- range(terra::values(tx_rast), na.rm = TRUE)
    pal <- colorNumeric(palette = "viridis", domain = rng, na.color = NA)
    
    # Base map + raster
    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(zoomControl = TRUE)) |>
            addProviderTiles("CartoDB.Positron") |>
            setView(lng = TX_CENTER$lon, lat = TX_CENTER$lat, zoom = TX_CENTER$zoom) |>
            addRasterImage(
                tx_rast_rasterpkg,
                colors = pal,
                opacity = 0.7,
                project = FALSE # raster is already EPSG:4326
            ) |>
            addLegend(
                position = "bottomright",
                pal = pal, values = terra::values(tx_rast),
                title = "Predicted ER Volume",
                labFormat = labelFormat(digits = 0)
            )
    })
    
    # Handle prediction request
    observeEvent(input$go, {
        # Determine coordinates
        lat <- input$lat
        lon <- input$lon
        
        if (is.na(lat) || is.na(lon)) {
            # Try geocoding if lat/lon not provided
            if (nzchar(input$address)) {
                coords <- geocode_address(input$address)
                lat <- coords["lat"]; lon <- coords["lon"]
            }
        }
        
        # Validate
        if (is.na(lat) || is.na(lon)) {
            showNotification("Please enter latitude & longitude or implement geocoding.", type = "warning")
            return(NULL)
        }
        
        # Build feature row (your real function should populate actual features)
        feats <- get_site_features(lat, lon)
        
        # Predict
        pred <- try(predict_er_volume(model_obj(), feats), silent = TRUE)
        if (inherits(pred, "try-error") || length(pred) != 1 || is.na(pred)) {
            showNotification("Prediction failed. Check model and features.", type = "error")
            return(NULL)
        }
        
        # Update map: marker + optional popup
        leafletProxy("map") |>
            clearGroup("site") |>
            {
                m <- .
                if (isTRUE(input$show_marker)) {
                    if (isTRUE(input$show_popup)) {
                        m <- addMarkers(m, lng = lon, lat = lat, popup = paste0(
                            "<b>Prediction:</b> ", sprintf("%.1f", pred), " visits/day<br/>",
                            "<b>Lat/Lon:</b> ", sprintf("%.5f, %.5f", lat, lon)
                        ), group = "site")
                    } else {
                        m <- addMarkers(m, lng = lon, lat = lat, group = "site")
                    }
                }
                m
            } |>
            setView(lng = lon, lat = lat, zoom = 13)
        
        # Show outputs
        output$pred_text <- renderText({
            paste0("Predicted ER Volume: ", sprintf("%.1f", pred), " visits/day")
        })
        
        output$metrics_table <- DT::renderDataTable({
            # Show features + prediction for transparency
            out <- cbind(feats, data.frame(predicted_ER_volume = pred))
            DT::datatable(
                out,
                options = list(pageLength = 5, scrollX = TRUE),
                rownames = FALSE
            )
        })
    }, ignoreInit = TRUE)
    
})
