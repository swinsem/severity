library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(reshape2)

data_dir <- "data/"
fig_dir  <- "figs/"

# ── 1. Load data ───────────────────────────────────────────────────────────────

# Spatial folds from CSV (western-us domain = one row per unique plot)
ard <- readr::read_csv(
  here::here(glue::glue("data/ARD_20250804_with-spatial-folds.csv")),
  col_types = list(spatial_fold = "factor"),
  show_col_types = FALSE
) |>
  dplyr::filter(domain == "western-us")

# Coordinates from gpkg (3280 unique plots, WGS84)
v_ard    <- terra::vect("data/saved/ARD_20250804.gpkg")
ard_crds <- as.data.frame(terra::crds(v_ard))
ard_crds$UniqueID <- v_ard$UniqueID

# Attach coordinates to the fold table; Fire already present from the CSV
ard <- dplyr::left_join(ard, ard_crds, by = "UniqueID")

# Drop unused factor levels left over from other domains in the full CSV
ard$spatial_fold <- droplevels(ard$spatial_fold)

# Project to CONUS Albers (EPSG:5070) for metre-based distances
ard_sf   <- sf::st_as_sf(ard, coords = c("x", "y"), crs = 4326)
ard_proj <- sf::st_transform(ard_sf, crs = 5070)
coords_m <- sf::st_coordinates(ard_proj)   # n × 2 matrix, metres


# ── 2. KNN nearest-neighbor distances ─────────────────────────────────────────
# Use nabor if installed (fast FLANN-based KNN); fall back to FNN; final fallback
# to a brute-force chunk approach via sf.

knn_done <- FALSE

if (requireNamespace("nabor", quietly = TRUE)) {
  nn       <- nabor::knn(data = coords_m, query = coords_m, k = 2)
  nn_dists <- nn$nn.dists[, 2]   # nearest non-self distance (m)
  nn_idx   <- nn$nn.idx[, 2]
  knn_done <- TRUE
}

if (!knn_done && requireNamespace("FNN", quietly = TRUE)) {
  nn       <- FNN::get.knn(coords_m, k = 1)
  nn_dists <- nn$nn.dist[, 1]
  nn_idx   <- nn$nn.index[, 1]
  knn_done <- TRUE
}

if (!knn_done) {
  stop("Install 'nabor' or 'FNN' to run this script (needed for KNN distances).")
}

# Flag pairs under 30 m
close_mask <- nn_dists < 30
n_close    <- sum(close_mask)
cat("\nNearest-neighbor distances:\n")
cat("  Total plots:", nrow(ard), "\n")
cat("  Plots with nearest neighbour < 30 m:", n_close, "\n")

if (n_close > 0) {
  close_df <- data.frame(
    plot_i    = ard$UniqueID[close_mask],
    plot_j    = ard$UniqueID[nn_idx[close_mask]],
    dist_m    = nn_dists[close_mask],
    fire_i    = ard$Fire[close_mask],
    fire_j    = ard$Fire[nn_idx[close_mask]]
  )
  close_df$same_fire <- close_df$fire_i == close_df$fire_j
  cat("  Same-fire pairs among flagged:", sum(close_df$same_fire), "\n")
  print(close_df)
}

p_nn <- ggplot(data.frame(dist_km = nn_dists / 1000), aes(x = dist_km)) +
  geom_histogram(bins = 60, fill = "#4393C3", color = NA, alpha = 0.85) +
  geom_vline(xintercept = 0.03, linetype = "dashed", color = "#D6604D",
             linewidth = 0.8) +
  annotate("text", x = 0.15, y = Inf, vjust = 1.5, hjust = 0, size = 3,
           color = "#D6604D", label = "30-m threshold") +
  xlab("Nearest-neighbour distance (km)") +
  ylab("Number of plots") +
  ggtitle("Nearest-neighbour distances between field plots") +
  theme_bw()
ggsave(paste0(fig_dir, "nn_distance_histogram.png"), p_nn,
       width = 7, height = 4, units = "in", dpi = 300)
cat("Saved:", paste0(fig_dir, "nn_distance_histogram.png"), "\n")


# ── 3. Fold-pair minimum distance matrix ───────────────────────────────────────

folds   <- levels(ard$spatial_fold)
n_folds <- length(folds)

# Pre-split coordinate matrices by fold
fold_coords <- lapply(folds, function(f) coords_m[ard$spatial_fold == f, , drop = FALSE])
names(fold_coords) <- folds

cat("\nComputing fold-pair minimum distances...\n")
fold_sep <- matrix(0, nrow = n_folds, ncol = n_folds,
                   dimnames = list(folds, folds))

for (i in seq_len(n_folds)) {
  for (j in seq_len(n_folds)) {
    if (j <= i) next
    ci <- fold_coords[[i]]
    cj <- fold_coords[[j]]
    if (requireNamespace("nabor", quietly = TRUE)) {
      nn_ij  <- nabor::knn(data = cj, query = ci, k = 1)
      min_d  <- min(nn_ij$nn.dists)
    } else {
      nn_ij  <- FNN::get.knnx(cj, query = ci, k = 1)
      min_d  <- min(nn_ij$nn.dist)
    }
    fold_sep[i, j] <- min_d
    fold_sep[j, i] <- min_d
  }
}

fold_sep_km   <- fold_sep / 1000
diag(fold_sep_km) <- NA

fold_melt <- reshape2::melt(fold_sep_km, varnames = c("Fold_A", "Fold_B"),
                            value.name = "min_dist_km")
fold_melt <- fold_melt[!is.na(fold_melt$min_dist_km), ]

# Short fold labels for axis readability
fold_melt$Fold_A <- gsub("Fold0?", "", fold_melt$Fold_A)
fold_melt$Fold_B <- gsub("Fold0?", "", fold_melt$Fold_B)

cat("Minimum between-fold distances (km):\n")
print(round(fold_sep_km, 1))

p_fold <- ggplot(fold_melt, aes(x = Fold_A, y = Fold_B, fill = min_dist_km)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(min_dist_km, 0)), size = 3.2) +
  scale_fill_gradient(low = "#D6604D", high = "#4393C3",
                      na.value = "white", name = "Min dist\n(km)") +
  theme_minimal() +
  theme(axis.text.x  = element_text(size = 10),
        axis.text.y  = element_text(size = 10),
        axis.title   = element_blank(),
        panel.grid   = element_blank(),
        legend.title = element_text(size = 9)) +
  coord_fixed() +
  ggtitle("Minimum between-fold distance (km)")
ggsave(paste0(fig_dir, "fold_separation_heatmap.png"), p_fold,
       width = 6, height = 5.5, units = "in", dpi = 300)
cat("Saved:", paste0(fig_dir, "fold_separation_heatmap.png"), "\n")


# ── 4. Variogram on model residuals ───────────────────────────────────────────

# CV results already have obs and pred for all western-us plots
cv_results      <- read.csv(paste0(data_dir, "saved/ranger_cv_results_20260506.csv"))
cv_results$resid <- cv_results$obs - cv_results$pred

# Join spatial coordinates
cv_geo <- dplyr::left_join(
  cv_results[, c("UniqueID", "resid")],
  ard_crds[, c("UniqueID", "x", "y")],
  by = "UniqueID"
)
cv_sf   <- sf::st_as_sf(cv_geo, coords = c("x", "y"), crs = 4326)
cv_proj <- sf::st_transform(cv_sf, crs = 5070)

cat("\nFitting variogram on model residuals (n =", nrow(cv_proj), "plots)...\n")

if (requireNamespace("automap", quietly = TRUE)) {
  vg <- automap::autofitVariogram(resid ~ 1, as(cv_proj, "Spatial"))
  range_km <- vg$var_model$range[2] / 1000
  cat("  Estimated variogram range:", round(range_km, 1), "km\n")
  png(paste0(fig_dir, "variogram_residuals.png"), width = 700, height = 480, res = 150)
  plot(vg)
  dev.off()
  cat("Saved:", paste0(fig_dir, "variogram_residuals.png"), "\n")

} else if (requireNamespace("gstat", quietly = TRUE)) {
  vg_emp <- gstat::variogram(resid ~ 1, data = cv_proj)
  vg_fit <- gstat::fit.variogram(vg_emp, model = gstat::vgm("Sph"))
  range_km <- vg_fit$range[2] / 1000
  cat("  Estimated variogram range:", round(range_km, 1), "km\n")
  cat("  Variogram model:\n")
  print(vg_fit)

  p_vg <- plot(vg_emp, vg_fit,
               main = "Variogram of model residuals (random forest, western-US)")
  png(paste0(fig_dir, "variogram_residuals.png"), width = 700, height = 480, res = 150)
  print(p_vg)
  dev.off()
  cat("Saved:", paste0(fig_dir, "variogram_residuals.png"), "\n")

} else {
   cat("  Install 'automap' or 'gstat' to compute the variogram.\n")
}
