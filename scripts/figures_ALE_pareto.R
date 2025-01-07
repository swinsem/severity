
library(ggplot2)

cpi_results_full = data.table::fread("VP/severity_tmp/data/saved/outputs/conditional-predictive-impact-results_v4.0.csv")

cpi_results = cpi_results_full |>
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

# Plot Pareto frontier of cross-fold mean R2 and overall R2 of important variable reduced model
r2_pareto_front = rPref::psel(
  df = cpi_results,
  pref = rPref::high(r2_important_variables_overall) * rPref::high(r2_important_variables)
)

##########################################
##### Pareto frontier plot #####
##########################################

ggplot2::ggplot(cpi_results, ggplot2::aes(x = r2_important_variables_overall, y = r2_important_variables)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = r2_pareto_front, color = "red", size = 3) +
  ggplot2::theme_bw() +
  labs(y="R2 - mean of spatial folds", x="R2 - overall mean across plots")
ggsave("VP/severity_tmp/plots/pareto_frontier.png", width = 6, height = 4, units = "in")

##########################################
##### First-order ALE plots #####
##########################################

top_results = r2_pareto_front |> na.omit()

best_fit = top_results[2,]

set.seed(4678)
fm1 = ranger::ranger(
  formula = as.formula(best_fit$important_variable_rf_formula), 
  data = sf::st_drop_geometry(ard[, c(target, features)]), 
  num.trees = 1000, 
  mtry = best_fit$mtry, 
  min.node.size = best_fit$min.node.size, 
  sample.fraction = best_fit$sample.fraction
)

per_variable_cpi_results = cpi_results_full |> 
  dplyr::filter(mtry == best_fit$mtry & min.node.size == best_fit$min.node.size & sample.fraction == best_fit$sample.fraction) |> 
  dplyr::arrange(dplyr::desc(ci.lo)) |> 
  dplyr::filter(ci.lo > 0)

calc_plot_data = function(var_names, fitted_model, variable_order) {
  yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)
  plot_data = purrr::map(
    .x = var_names,
    .f = \(J) {
      ale = ALEPlot::ALEPlot(
        X = sf::st_drop_geometry(ard[, fitted_model$forest$independent.variable.names]), 
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
  fitted_model = fm1,
  variable_order = per_variable_cpi_results$Variable
)

## Plot first order ALE plots ##

facet_labels <- c(
  "dswir2swir1" = "dSWIR2/SWIR1",
  "zScorePrecip1" = "Z-Score Precipitation",
  "northness" = "Northness"
)
ggplot(plot_data_important_vars, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(facets = "var", scales = "free", 
             labeller = labeller(var = facet_labels)) +
  theme_bw()
ggsave("VP/severity_tmp/plots/ALE_first_order.png", width = 6, height = 4, units = "in")


# Ensure `var` is a factor with the desired order
plot_data_important_vars2 <- plot_data_important_vars
plot_data_important_vars2$var <- factor(
  plot_data_important_vars2$var,
  levels = c("dswir2swir1", "zScorePrecip1", "northness")
)

# Plot the ALE plots with reordered facets and density plots
ggplot(plot_data_important_vars2, aes(x = x, y = y)) +
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
ggsave("VP/severity_tmp/plots/ALE_first_order2.png", width = 6, height = 4, units = "in")

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
png("VP/severity_tmp/plots/two_way_ale_precip_dswir.png", width = 6, height = 5, units="in", res=300)

# Run ALEPlot to get the ALE values
zscoreprecip1_dswir_two_way_ale = ALEPlot::ALEPlot(
  X = sf::st_drop_geometry(ard[, fm1$forest$independent.variable.names]), 
  X.model = fm1, 
  J = c("zScorePrecip1", "dswir2swir1"), 
  pred.fun = yhat
)

# Create the heatmap with a legend
image.plot(
  zscoreprecip1_dswir_two_way_ale$x.values[[1]], 
  zscoreprecip1_dswir_two_way_ale$x.values[[2]], 
  zscoreprecip1_dswir_two_way_ale$f.values, 
  xlab = "Z-score Precipitation", 
  ylab = "dSWIR2/SWIR1", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5  # Adjusts the margin for the legend, change if needed
)

dev.off()


# dSWIR2SWIR1 to northness
png("VP/severity_tmp/plots/two_way_ale_north_dswir.png", width = 6, height = 5, units="in", res=300)

northness_dswir_two_way_ale = ALEPlot::ALEPlot(
  X = sf::st_drop_geometry(ard[, fm1$forest$independent.variable.names]), 
  X.model = fm1, 
  J = c("northness", "dswir2swir1"), 
  pred.fun = yhat
)

image.plot(
  northness_dswir_two_way_ale$x.values[[1]], 
  northness_dswir_two_way_ale$x.values[[2]], 
  northness_dswir_two_way_ale$f.values, 
  xlab = "Northness", 
  ylab = "dSWIR2/SWIR1", 
  col = hcl.colors(n = 100, palette = "Blue-Red"),
  legend.mar = 5
)

dev.off()

## Northness:Precip - hard to interpret
png("VP/severity_tmp/plots/two_way_ale_north_precip.png", width = 6, height = 5, units="in", res=300)

northness_precip_two_way_ale = ALEPlot::ALEPlot(
  X = sf::st_drop_geometry(ard[, fm1$forest$independent.variable.names]), 
  X.model = fm1, 
  J = c("northness", "zScorePrecip1"), 
  pred.fun = yhat
)

image(
  northness_precip_two_way_ale$x.values[[1]], 
  northness_precip_two_way_ale$x.values[[2]], 
  northness_precip_two_way_ale$f.values, 
  xlab = "Northness", 
  ylab = "Z-score Precipitation", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
) 
   
dev.off()

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



# # The shortcut to just look at the table in my screenshot
# top_results = structure(
#   list(
#     mtry = c(4L, 3L, 4L),
#     sample.fraction = c(0.7, 0.632120558828558, 0.7),
#     min.node.size = c(50L, 60L, 60L),
#     rmse_full = c(0.268083261313512, 0.26872065739475, 0.267812714273246),
#     r2_full = c(-0.303325414602873, -0.336928485033941, -0.306773573957064),
#     mae_full = c(0.205434762569152, 0.206793084624741, 0.205591477394943),
#     mse_full = c(0.0758998325166776, 0.0764617624306148, 0.0758349130378365),
#     rmse_full_overall = c(0.283676467249458, 0.284965447853962, 0.284733322894045),
#     r2_full_overall = c(0.543345881715101, 0.539332194225563, 0.539985005772275),
#     mae_full_overall = c(0.213296590576411, 0.215123118254161, 0.214940048108721),
#     mse_full_overall = c(0.0804723380711328, 0.0812053064706093, 0.0810730651662846),
#     n_important_variables = c(5L, 3L, 5L),
#     important_variable_rf_formula = c("pcnt_ba_mo ~ dndvi + dred + northness + post_swir2swir1 + zScorePrecip1", "pcnt_ba_mo ~ dswir2swir1 + northness + zScorePrecip1", "pcnt_ba_mo ~ dndvi + dswir2 + northness + post_swir2nir + zScorePrecip1"),
#     rmse_important_variables = c(0.249324154440508, 0.254920503135365, 0.252569880773709),
#     r2_important_variables = c(0.118508630120102, 0.267309439297053, -0.290180212311028),
#     mae_important_variables = c(0.178943386446459, 0.181787332462959, 0.18018098713846),
#     mse_important_variables = c(0.0661507334466124, 0.0700145686587712, 0.0662784542817479),
#     rmse_important_variables_overall = c(0.274822298181919, 0.282639313677022, 0.271625889583909),
#     r2_important_variables_overall = c(0.572848080518024, 0.549886935546503, 0.582282453974439),
#     mae_important_variables_overall = c(0.194134380637982, 0.198871633350667, 0.190291467955942),
#     mse_important_variables_overall = c(0.0755272955779917, 0.0798849816358181, 0.0737806238922502)
#   ),
#   row.names = c(NA, -3L),
#   class = c("data.table", "data.frame")
# )