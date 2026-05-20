
library(ggplot2)
library(purrr)
library(ranger)
library(dplyr)

data_dir <- "data/"
fig_dir <- "figs/"

cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))

cpi_results_west <- cpi_results_full[cpi_results_full$domain == "western-us", ]

cpi_results_west = cpi_results_west |>
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

# Plot Pareto frontier of cross-fold mean R2 and overall R2 of important variable reduced model
top_results_corr_v1 <- rPref::psel(
  df   = cpi_low_corr,
  pref = rPref::low(n_important_variables) * rPref::high(r2_important_variables_overall)
) |>
  select(n_important_variables, important_variable_rf_formula,
         mtry, min.node.size, sample.fraction,
         r2_mean_important_variables, r2_important_variables_overall, max_corr) |>
  mutate(across(starts_with("r2_"), \(x) signif(x, 3)),
         max_corr = round(max_corr, 3)) |>
  arrange(n_important_variables, desc(r2_important_variables_overall)) |>
  as_tibble()

best_fit = top_results_corr_v1[2,]
# top_results_corr_v1 <- read.csv(paste0(data_dir, "saved/pareto_frontier_v1.csv"))

##########################################
##### Pareto frontier plot #####
##########################################

ggplot2::ggplot(cpi_results_west, ggplot2::aes(x = r2_important_variables_overall, y = n_important_variables)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = top_results_corr_v1, color = "red", size = 3) +
  ggplot2::theme_bw() +
  labs(y="Number of important variables", x="R2 - overall mean across plots")
ggsave("figs/pareto_frontier_260514.png", width = 6, height = 4, units = "in")

##########################################
##### First-order ALE plots #####
##########################################


# load model
final_mod <- readRDS("data/rf_final_model_050626.rds")

# load ARD
filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)
ard_west <- ard[ard$domain=="western-us",]

ard_df <- as.data.frame(ard_west)

per_variable_cpi_results = cpi_results_full |>
  dplyr::filter(domain=="western-us") |> 
  dplyr::filter(mtry == best_fit$mtry & min.node.size == best_fit$min.node.size & sample.fraction == best_fit$sample.fraction) |> 
  dplyr::arrange(dplyr::desc(ci.lo)) |> 
  dplyr::filter(ci.lo > 0)

calc_plot_data = function(var_names, fitted_model, variable_order) {
  yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
  plot_data = purrr::map(
    .x = var_names,
    .f = \(J) {
      ale = ALEPlot::ALEPlot(
        X = ard_df[, fitted_model$forest$independent.variable.names], 
        X.model = fitted_model, 
        J = J, 
        pred.fun = yhat
      )
      
      out = tibble::tibble(var = J, x = ale$x.values, y = ale$f.values)
      return(out)
    }
  ) |> 
    data.table::rbindlist() |> 
    dplyr::mutate(var = factor(var, levels = variable_order))
  
  return(plot_data)
}

plot_data_important_vars = calc_plot_data(
  var_names = per_variable_cpi_results$Variable,
  fitted_model = final_mod,
  variable_order = per_variable_cpi_results$Variable
)

## Plot first order ALE plots ##

# Compute effect size per variable
var_rank <- plot_data_important_vars %>%
  group_by(var) %>%
  summarize(delta_y = max(y, na.rm = TRUE) - min(y, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(delta_y))

# Reorder the facetting factor by effect size
plot_df <- plot_data_important_vars %>%
  mutate(var = factor(var, levels = var_rank$var))


facet_labels <- c(
  "rdnbr" = "RdNBR",
  "zScorePrecip1" = "Z-Score Precipitation",
  "dnir" = "dNIR",
  "northness" = "Northness"
)



# Plot the ALE plots with reordered facets and density plots
ggplot(plot_df, aes(x = x, y = y)) +
  geom_line() + 
  geom_rug(sides = "b", alpha = 0.5) +  # Add density rug
  facet_wrap(
    facets = "var", 
    scales = "free", 
    labeller = labeller(var = facet_labels)
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),  # Adjust facet label size
    axis.title = element_text(size = 10)
  ) +
  xlab("x") +
  ylab("y")
ggsave("figs/ALE_first_order_260514.png", width = 6, height = 4, units = "in")

##########################################
##### Two-way ALE plots #####
##########################################

## 
# Load the necessary library
library(fields)

# Prediction function
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)

# RdNBR:precip

# Save the plot as a PNG file
png("figs/two_way_ale_precip_rdnbr.png", width = 6, height = 5, units="in", res=300)

# Run ALEPlot to get the ALE values
zscoreprecip1_postswir_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("rdnbr", "zScorePrecip1"), 
  pred.fun = yhat
)

# Create the heatmap with a legend
image.plot(
  zscoreprecip1_postswir_two_way_ale$x.values[[1]], 
  zscoreprecip1_postswir_two_way_ale$x.values[[2]], 
  zscoreprecip1_postswir_two_way_ale$f.values, 
  ylab = "Z-score Precipitation", 
  xlab = "RdNBR", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5  # Adjusts the margin for the legend, change if needed
)

dev.off()


# RdNBR:northness
png("figs/two_way_ale_north_rdnbr.png", width = 6, height = 5, units="in", res=300)

northness_dswir_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("rdnbr", "northness"), 
  pred.fun = yhat
)

image.plot(
  northness_dswir_two_way_ale$x.values[[1]], 
  northness_dswir_two_way_ale$x.values[[2]], 
  northness_dswir_two_way_ale$f.values, 
  ylab = "Northness", 
  xlab = "RdNBR", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5
)

dev.off()

## dNIR:Precip
png("figs/two_way_ale_dnir_precip.png", width = 6, height = 5, units="in", res=300)

dndvi_precip_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dnir", "zScorePrecip1"), 
  pred.fun = yhat
)

image.plot(
  dndvi_precip_two_way_ale$x.values[[1]], 
  dndvi_precip_two_way_ale$x.values[[2]], 
  dndvi_precip_two_way_ale$f.values, 
  xlab = "dNIR", 
  ylab = "Z-score Precipitation", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5 
) 


dev.off()


## RdNBR:dNIR
png("figs/two_way_ale_rdnbr_dnir.png", width = 6, height = 5, units="in", res=300)

pswir_dndvi_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("rdnbr", "dnir"), 
  pred.fun = yhat
)

image.plot(
  pswir_dndvi_two_way_ale$x.values[[1]], 
  pswir_dndvi_two_way_ale$x.values[[2]], 
  pswir_dndvi_two_way_ale$f.values, 
  xlab = "RdNBR", 
  ylab = "dNIR", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5  # Adjusts the margin for the legend, change if needed
) 

dev.off()


## dNIR:northness
png("figs/two_way_ale_dnir_north.png", width = 6, height = 5, units="in", res=300)

pswir_tpi_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dnir", "northness"), 
  pred.fun = yhat
)

image.plot(
  pswir_tpi_two_way_ale$x.values[[1]], 
  pswir_tpi_two_way_ale$x.values[[2]], 
  pswir_tpi_two_way_ale$f.values, 
  xlab = "dNIR", 
  ylab = "Northness", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5 
)

dev.off()




##########################################
##### Two-way ALE — ggplot with data density ######
##########################################
# Two-way ALE isolates the INTERACTION effect: how much the combination of two
# variables shifts predictions beyond what each contributes independently.
# Values near zero mean the variables act additively; red = that combination
# predicts higher BA loss than marginal effects alone would suggest.
# White contours show observed data density so readers can distinguish
# model behaviour supported by data from extrapolation into sparse regions.

ale2d_gg <- function(ale, xlab, ylab,
                     raw_x, raw_y,
                     xlim = NULL, ylim = NULL,
                     n_contour = 5) {
  x_vals <- ale$x.values[[1]]
  y_vals <- ale$x.values[[2]]

  # compute tile width/height from spacing between grid points so tiles fill
  # the space rather than rendering as pinpoints
  bin_widths <- function(v) {
    n <- length(v)
    w <- numeric(n)
    w[1]       <- v[2] - v[1]
    w[n]       <- v[n] - v[n - 1]
    if (n > 2) w[2:(n-1)] <- (v[3:n] - v[1:(n-2)]) / 2
    w
  }

  grid        <- expand.grid(x = x_vals, y = y_vals)
  grid$ale    <- as.vector(ale$f.values)
  grid$width  <- bin_widths(x_vals)[match(grid$x, x_vals)]
  grid$height <- bin_widths(y_vals)[match(grid$y, y_vals)]

  # colour scale limits based on the visible range only
  grid_vis <- grid
  if (!is.null(xlim)) grid_vis <- grid_vis[grid_vis$x >= xlim[1] & grid_vis$x <= xlim[2], ]
  if (!is.null(ylim)) grid_vis <- grid_vis[grid_vis$y >= ylim[1] & grid_vis$y <= ylim[2], ]
  lim <- max(abs(grid_vis$ale), na.rm = TRUE)

  raw_df <- data.frame(x = raw_x, y = raw_y)

  ggplot(grid, aes(x, y, fill = ale, width = width, height = height)) +
    geom_tile() +
    geom_density_2d(data = raw_df, aes(x, y), inherit.aes = FALSE,
                    colour = "grey15", alpha = 0.8, linewidth = 0.4, bins = n_contour) +
    scale_fill_distiller(palette = "RdBu", direction = -1,
                         limits = c(-lim, lim), name = "ALE\neffect") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(x = xlab, y = ylab) +
    theme_bw()
}

p_ale2d_rdnbr_precip <- ale2d_gg(
  zscoreprecip1_postswir_two_way_ale,
  xlab = "RdNBR", ylab = "Z-score precipitation",
  raw_x = ard_df$rdnbr, raw_y = ard_df$zScorePrecip1,
  xlim = c(-300, 1300)
)
ggsave(paste0(fig_dir, "ale2d_rdnbr_precip_gg.png"), p_ale2d_rdnbr_precip,
       width = 6, height = 4.5, dpi = 300)

p_ale2d_dnir_precip <- ale2d_gg(
  dndvi_precip_two_way_ale,
  xlab = "dNIR", ylab = "Z-score precipitation",
  raw_x = ard_df$dnir, raw_y = ard_df$zScorePrecip1
)
ggsave(paste0(fig_dir, "ale2d_dnir_precip_gg.png"), p_ale2d_dnir_precip,
       width = 6, height = 4.5, dpi = 300)

p_ale2d_rdnbr_north <- ale2d_gg(
  northness_dswir_two_way_ale,
  xlab = "RdNBR", ylab = "Northness",
  raw_x = ard_df$rdnbr, raw_y = ard_df$northness,
  xlim = c(-300, 1300)
)
ggsave(paste0(fig_dir, "ale2d_rdnbr_northness_gg.png"), p_ale2d_rdnbr_north,
       width = 6, height = 4.5, dpi = 300)

p_ale2d_rdnbr_dnir <- ale2d_gg(
  pswir_dndvi_two_way_ale,
  xlab = "RdNBR", ylab = "dNIR",
  raw_x = ard_df$rdnbr, raw_y = ard_df$dnir,
  xlim = c(-300, 1300)
)
ggsave(paste0(fig_dir, "ale2d_rdnbr_dnir_gg.png"), p_ale2d_rdnbr_dnir,
       width = 6, height = 4.5, dpi = 300)

p_ale2d_dnir_north <- ale2d_gg(
  pswir_tpi_two_way_ale,
  xlab = "dNIR", ylab = "Northness",
  raw_x = ard_df$dnir, raw_y = ard_df$northness
)
ggsave(paste0(fig_dir, "ale2d_dnir_northness_gg.png"), p_ale2d_dnir_north,
       width = 6, height = 4.5, dpi = 300)


### code attempts that were abandoned:

# ## attempted code to add legends and density plots
# library(ALEPlot)
# 
# # Prediction function
# yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
# 
# # Calculate 2D ALE values
# zscoreprecip1_dswir_two_way_ale = ALEPlot::ALEPlot(
#   X = sf::st_drop_geometry(ard[, fm1$forest$independent.variable.names]), 
#   X.model = fm1, 
#   J = c("zScorePrecip1", "dswir2swir1"), 
#   pred.fun = yhat
# )
# 
# # Set color palette
# colors <- hcl.colors(n = 100, palette = "Blue-Red")
# 
# # Save the plot as a PNG file
# png("VP/severity_tmp/plots/two_way_ale_precip_dswir_v6.png", width = 6, height = 5, units = "in", res = 300)
# 
# # Use filled.contour for image with legend
# filled.contour(
#   x = zscoreprecip1_dswir_two_way_ale$x.values[[1]], 
#   y = zscoreprecip1_dswir_two_way_ale$x.values[[2]], 
#   z = zscoreprecip1_dswir_two_way_ale$f.values, 
#   color.palette = function(n) hcl.colors(n, palette = "Blue-Red"),
#   xlab = "Z-score Precipitation", 
#   ylab = "dSWIR2:SWIR1",
#   plot.axes = {
#     axis(1)
#     axis(2)
#     #rug(sf::st_drop_geometry(ard$zScorePrecip1), side = 1, col = "black", ticksize = 0.01, lwd = 0.5)  # Density for x-axis
#     #rug(sf::st_drop_geometry(ard$dswir2swir1), side = 2, col = "black", ticksize = 0.01, lwd = 0.5)  # Density for y-axis
#   }
# )
# 
# # Close PNG device
# dev.off()

## v2
# library(ggplot2)
# 
# # Prepare the data for ggplot
# ale_data <- expand.grid(
#   x = zscoreprecip1_dswir_two_way_ale$x.values[[1]], 
#   y = zscoreprecip1_dswir_two_way_ale$x.values[[2]]
# )
# ale_data$ale <- as.vector(zscoreprecip1_dswir_two_way_ale$f.values)
# 
# # Check if the ale_data contains NA values or is sparse
# ale_data <- na.omit(ale_data)
# 
# # Create the ggplot
# ggplot(ale_data, aes(x = x, y = y, fill = ale)) +
#   geom_tile(width = 0.05, height = 0.05) +  # Adjust tile width and height
#   scale_fill_gradient2(
#     low = "blue", mid = "white", high = "red", midpoint = 0, 
#     name = "ALE", 
#     guide = guide_colorbar(barwidth = 1, barheight = 10)
#   ) +
#   xlab("Z-score Precipitation") +
#   ylab("dSWIR2:SWIR1") +
#   theme_minimal() +
#   theme(
#     axis.ticks = element_line(linewidth = 0.5),  # Update for ticks
#     axis.title = element_text(size = 12),
#     legend.position = "right",
#     legend.title = element_text(size = 12),
#     legend.text = element_text(size = 10)
#   ) +
#   geom_rug(
#     data = ale_data,  # Correct dataset for geom_rug
#     aes(x = x), 
#     sides = "b",  # Bottom rug only
#     alpha = 0.5
#   ) +
#   geom_rug(
#     data = ale_data,  # Correct dataset for geom_rug
#     aes(y = y), 
#     sides = "l",  # Left rug only
#     alpha = 0.5
#   )


## original code

# yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
# 
# # dSWIR2SWIR1 to precip
# 
# # Save the plot as a PNG file
# png("VP/severity_tmp/plots/two_way_ale_precip_dswir.png", width = 6, height = 5, units="in", res=300)
# 
# zscoreprecip1_dswir_two_way_ale = ALEPlot::ALEPlot(
#   X = sf::st_drop_geometry(ard[, fm1$forest$independent.variable.names]), 
#   X.model = fm1, 
#   J = c("zScorePrecip1", "dswir2swir1"), 
#   pred.fun = yhat
# )
# 
# image(
#   zscoreprecip1_dswir_two_way_ale$x.values[[1]], 
#   zscoreprecip1_dswir_two_way_ale$x.values[[2]], 
#   zscoreprecip1_dswir_two_way_ale$f.values, 
#   xlab = "Z-score Precipitation", 
#   ylab = "dSWIR2:SWIR1", 
#   col = hcl.colors(n = 100, palette = "Blue-Red")
# )
# 
# dev.off()



# # more confusing
  # contour(
  #   zscoreprecip1_rdnbr_two_way_ale$x.values[[1]], 
  #   zscoreprecip1_rdnbr_two_way_ale$x.values[[2]], 
  #   zscoreprecip1_rdnbr_two_way_ale$f.values, 
  #   add=TRUE, 
  #   drawlabels=TRUE
  # )


