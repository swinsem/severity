
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
r2_pareto_front = rPref::psel(
  df = cpi_results_west,
  pref = rPref::high(r2_important_variables_overall) * rPref::high(r2_mean_important_variables)
)

##########################################
##### Pareto frontier plot #####
##########################################

ggplot2::ggplot(cpi_results_west, ggplot2::aes(x = r2_important_variables_overall, y = r2_mean_important_variables)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = r2_pareto_front, color = "red", size = 3) +
  ggplot2::theme_bw() +
  labs(y="R2 - mean of spatial folds", x="R2 - overall mean across plots")
ggsave("figs/pareto_frontier.png", width = 6, height = 4, units = "in")

##########################################
##### First-order ALE plots #####
##########################################

top_results = r2_pareto_front |> na.omit()

best_fit = top_results[2,]

# load model
final_mod <- readRDS("data/rf_final_model.rds")

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
  "post_swir2swir1" = "post SWIR2/SWIR1",
  "dndvi" = "dNDVI",
  "zScorePrecip1" = "Z-Score Precipitation",
  "meanTPI" = "TPI",
  "dred" = "dRed",
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
ggsave("figs/ALE_first_order.png", width = 6, height = 4, units = "in")

##########################################
##### Two-way ALE plots #####
##########################################

## 
# Load the necessary library
library(fields)

# Prediction function
yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)

# dSWIR2SWIR1 to precip

# Save the plot as a PNG file
png("figs/two_way_ale_precip_pswir.png", width = 6, height = 5, units="in", res=300)

# Run ALEPlot to get the ALE values
zscoreprecip1_postswir_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("post_swir2swir1", "zScorePrecip1"), 
  pred.fun = yhat
)

# Create the heatmap with a legend
image.plot(
  zscoreprecip1_postswir_two_way_ale$x.values[[1]], 
  zscoreprecip1_postswir_two_way_ale$x.values[[2]], 
  zscoreprecip1_postswir_two_way_ale$f.values, 
  ylab = "Z-score Precipitation", 
  xlab = "post SWIR2/SWIR1", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5  # Adjusts the margin for the legend, change if needed
)

dev.off()


# postSWIR2SWIR1 to northness
png("figs/two_way_ale_north_pswir.png", width = 6, height = 5, units="in", res=300)

northness_dswir_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("northness", "post_swir2swir1"), 
  pred.fun = yhat
)

image.plot(
  northness_dswir_two_way_ale$x.values[[1]], 
  northness_dswir_two_way_ale$x.values[[2]], 
  northness_dswir_two_way_ale$f.values, 
  xlab = "Northness", 
  ylab = "post SWIR2/SWIR1", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5
)

dev.off()

## dNDVI:Precip
png("figs/two_way_ale_dndvi_precip.png", width = 6, height = 5, units="in", res=300)

dndvi_precip_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dndvi", "zScorePrecip1"), 
  pred.fun = yhat
)

image.plot(
  dndvi_precip_two_way_ale$x.values[[1]], 
  dndvi_precip_two_way_ale$x.values[[2]], 
  dndvi_precip_two_way_ale$f.values, 
  xlab = "dNDVI", 
  ylab = "Z-score Precipitation", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5 
) 


dev.off()


## postSWIR2/SWIR1:dNDVI
png("figs/two_way_ale_pswir_dndvi.png", width = 6, height = 5, units="in", res=300)

pswir_dndvi_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dndvi", "post_swir2swir1"), 
  pred.fun = yhat
)

image(
  pswir_dndvi_two_way_ale$x.values[[1]], 
  pswir_dndvi_two_way_ale$x.values[[2]], 
  pswir_dndvi_two_way_ale$f.values, 
  xlab = "dNDVI", 
  ylab = "post swir2/swir1", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
) 

dev.off()

## same as above but for the combination of post_swir2swir1 and meanTPI
png("figs/two_way_ale_pswir_tpi.png", width = 6, height = 5, units="in", res=300)

pswir_tpi_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("meanTPI", "post_swir2swir1"), 
  pred.fun = yhat
)

image(
  pswir_tpi_two_way_ale$x.values[[1]], 
  pswir_tpi_two_way_ale$x.values[[2]], 
  pswir_tpi_two_way_ale$f.values, 
  xlab = "TPI", 
  ylab = "post swir2/swir1", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)

dev.off()

## same as above but for dNDVI and meanTPI
png("figs/two_way_ale_dndvi_tpi.png", width = 6, height = 5, units="in", res=300)

dndvi_tpi_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("meanTPI", "dndvi"), 
  pred.fun = yhat
)
image(
  dndvi_tpi_two_way_ale$x.values[[1]], 
  dndvi_tpi_two_way_ale$x.values[[2]], 
  dndvi_tpi_two_way_ale$f.values, 
  xlab = "TPI", 
  ylab = "dNDVI", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

## same as above but for post SWIR2/SWIR1 and dRed
png("figs/two_way_ale_pswir_dred.png", width = 6, height = 5, units="in", res=300)
pswir_dred_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dred", "post_swir2swir1"), 
  pred.fun = yhat
)
image(
  pswir_dred_two_way_ale$x.values[[1]], 
  pswir_dred_two_way_ale$x.values[[2]], 
  pswir_dred_two_way_ale$f.values, 
  xlab = "dRed", 
  ylab = "post swir2/swir1", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

## same as above but dndvi and dred
png("figs/two_way_ale_dndvi_dred.png", width = 6, height = 5, units="in", res=300)
dndvi_dred_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dndvi", "dred"), 
  pred.fun = yhat
)
image(
  dndvi_dred_two_way_ale$x.values[[1]], 
  dndvi_dred_two_way_ale$x.values[[2]], 
  dndvi_dred_two_way_ale$f.values, 
  xlab = "dNDVI",
  ylab = "dRed",
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

## same as above but dndvi and northness
png("figs/two_way_ale_dndvi_north.png", width = 6, height = 5, units="in", res=300)
dndvi_north_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dndvi", "northness"), 
  pred.fun = yhat
)
image(
  dndvi_north_two_way_ale$x.values[[1]], 
  dndvi_north_two_way_ale$x.values[[2]], 
  dndvi_north_two_way_ale$f.values, 
  ylab = "Northness", 
  xlab = "dNDVI", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

## same as above but zscore precip to tpi
png("figs/two_way_ale_precip_tpi.png", width = 6, height = 5, units="in", res=300)
precip_tpi_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("zScorePrecip1", "meanTPI"), 
  pred.fun = yhat
)
image(
  precip_tpi_two_way_ale$x.values[[1]], 
  precip_tpi_two_way_ale$x.values[[2]], 
  precip_tpi_two_way_ale$f.values, 
  ylab = "TPI", 
  xlab = "Z-score Precipitation", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

## above but precip to dred
png("figs/two_way_ale_precip_dred.png", width = 6, height = 5, units="in", res=300)
precip_dred_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("zScorePrecip1", "dred"), 
  pred.fun = yhat
)
image(
  precip_dred_two_way_ale$x.values[[1]], 
  precip_dred_two_way_ale$x.values[[2]], 
  precip_dred_two_way_ale$f.values, 
  ylab = "dRed", 
  xlab = "Z-score Precipitation", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

## above but precip to northness
png("figs/two_way_ale_precip_north.png", width = 6, height = 5, units="in", res=300)
precip_north_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("zScorePrecip1", "northness"), 
  pred.fun = yhat
)
image(
  precip_north_two_way_ale$x.values[[1]], 
  precip_north_two_way_ale$x.values[[2]], 
  precip_north_two_way_ale$f.values, 
  ylab = "Northness", 
  xlab = "Z-score Precipitation", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

# above but tpi to northness
png("figs/two_way_ale_tpi_north.png", width = 6, height = 5, units="in", res=300)
tpi_north_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("meanTPI", "northness"), 
  pred.fun = yhat
)
image(
  tpi_north_two_way_ale$x.values[[1]], 
  tpi_north_two_way_ale$x.values[[2]], 
  tpi_north_two_way_ale$f.values, 
  ylab = "Northness", 
  xlab = "TPI", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

# above but dred to northness
png("figs/two_way_ale_dred_north.png", width = 6, height = 5, units="in", res=300)
dred_north_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("dred", "northness"), 
  pred.fun = yhat
)
image(
  dred_north_two_way_ale$x.values[[1]], 
  dred_north_two_way_ale$x.values[[2]], 
  dred_north_two_way_ale$f.values, 
  ylab = "Northness", 
  xlab = "dRed", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

# above but tpi to dred
png("figs/two_way_ale_tpi_dred.png", width = 6, height = 5, units="in", res=300)
tpi_dred_two_way_ale = ALEPlot::ALEPlot(
  X = ard_df[, final_mod$forest$independent.variable.names], 
  X.model = final_mod, 
  J = c("meanTPI", "dred"), 
  pred.fun = yhat
)
image(
  tpi_dred_two_way_ale$x.values[[1]], 
  tpi_dred_two_way_ale$x.values[[2]], 
  tpi_dred_two_way_ale$f.values, 
  ylab = "dRed", 
  xlab = "TPI", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)
dev.off()

# question: have I done all of the combinations of the 6 variables?


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


