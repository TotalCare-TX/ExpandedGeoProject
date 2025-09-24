#This script will focus on building a PCA/GAM model for generating our high-level predictions.

# pick your raw raster columns
rcols <- names(er)[startsWith(names(er), "raster_")]

# Named vectors of mins/maxes (simple + fast)
train_min <- vapply(train_df[rcols], function(x) min(x, na.rm = TRUE), numeric(1))
train_max <- vapply(train_df[rcols], function(x) max(x, na.rm = TRUE), numeric(1))

# Clamper that uses those named vectors
clamp_to_bounds <- function(df_raw, mins, maxs) {
    for (v in intersect(names(mins), names(df_raw))) {
        df_raw[[v]] <- pmin(maxs[[v]], pmax(mins[[v]], df_raw[[v]]))
    }
    df_raw
}

# --- your sf points (EPSG in meters recommended) ---
# er: sf POINTS with Daily_Volume, Accepts_Med, and many raster_* cols

# helper from earlier, slightly simplified and geometry-safe
make_pca_rec <- function(dat, n_pcs = 15, prefix = "raster_") {
    dat_tbl <- dat |> sf::st_drop_geometry() |> tibble::as_tibble()
    rcols   <- names(dat_tbl)[startsWith(names(dat_tbl), prefix)]
    n_pcs_eff <- min(n_pcs, length(rcols))
    
    # Base recipe (no PCA yet)
    rec <- recipes::recipe(
        log1_scaled_ppd ~ .,
        data = dplyr::select(dat_tbl, log1_scaled_ppd, Accepts_Med, dplyr::any_of(rcols))
    ) |>
        recipes::step_mutate(Accepts_Med = as.factor(Accepts_Med)) |>
        recipes::step_dummy(Accepts_Med, one_hot = TRUE) |>
        recipes::step_zv(recipes::all_predictors()) |>
        recipes::step_normalize(dplyr::any_of(rcols))
    
    # Conditionally append PCA
    if (length(rcols) >= 2 && n_pcs_eff > 0) {
        rec <- rec |>
            recipes::step_pca(dplyr::any_of(rcols), num_comp = n_pcs_eff)
    }
    
    rec
}


fit_birdseye <- function(train_sf, prepped_rec) {
    tr <- recipes::bake(prepped_rec, new_data = sf::st_drop_geometry(train_sf), composition = "tibble")
    xy <- sf::st_coordinates(train_sf)
    
    # manual center/scale (don’t rely on scale() attributes)
    eps <- 1e-8
    Xc <- mean(xy[,1], na.rm = TRUE); Xs <- sd(xy[,1], na.rm = TRUE); if (is.na(Xs) || Xs < eps) Xs <- 1
    Yc <- mean(xy[,2], na.rm = TRUE); Ys <- sd(xy[,2], na.rm = TRUE); if (is.na(Ys) || Ys < eps) Ys <- 1
    
    tr$X <- (xy[,1] - Xc) / Xs
    tr$Y <- (xy[,2] - Yc) / Ys
    
    lin_preds <- setdiff(names(tr), c("log1_scaled_ppd","X","Y"))
    form <- as.formula(paste("log1_scaled_ppd ~",
                             paste(lin_preds, collapse = " + "),
                             "+ s(X, Y, bs='gp', k=100)"))
    
    gm <- mgcv::gam(form, data = tr, method = "REML")
    
    # return model + the scaling params you’ll need later
    list(
        gam = gm,
        x_center = Xc, x_scale = Xs,
        y_center = Yc, y_scale = Ys,
        recipe = prepped_rec   # optional: tuck recipe alongside for convenience
    )
}


predict_birdseye <- function(bird_obj, new_sf) {
    stopifnot(is.list(bird_obj), !is.null(bird_obj$gam), !is.null(bird_obj$recipe))
    
    nd <- recipes::bake(bird_obj$recipe, new_data = sf::st_drop_geometry(new_sf), composition = "tibble")
    xy <- sf::st_coordinates(new_sf)
    
    nd$X <- (xy[,1] - bird_obj$x_center) / bird_obj$x_scale
    nd$Y <- (xy[,2] - bird_obj$y_center) / bird_obj$y_scale
    
    pr <- predict(bird_obj$gam, newdata = nd, se.fit = TRUE)
    tibble::tibble(
        baseline_mean = as.numeric(pr$fit),
        baseline_sd   = as.numeric(pr$se.fit)
    )
}




# ---- fit PRODUCTION birds-eye artifacts ----
n_pcs <- 15
rec_all  <- make_pca_rec(ml_sf, n_pcs = n_pcs) |> recipes::prep()
bird_mod <- fit_birdseye(ml_sf, rec_all)

# save for later use
saveRDS(rec_all,  "birdseye_pca_rec_all.rds")
saveRDS(bird_mod, "birdseye_model_all.rds")

