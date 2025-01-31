ard2ar <- ard2[ard2$ecoregion=="Arizona Mountains forests",]
ard2ar <- plot(ardcoords[ardcoords$ecoregion=="Arizona Mountains forests",])
ardcoords$
ard2arna <- ard2ar[apply(is.na(ard2ar), 1, any), ]

library(dplyr)

# Exclude 'PlotID' and 'UniqueID' for duplicate checking
cols_to_check <- setdiff(names(ard2ar), c("PlotID", "UniqueID"))

# Check for duplicates
duplicates <- ard2ar %>%
  filter(duplicated(select(., all_of(cols_to_check))))

# View the duplicates
print(duplicates)

# Count duplicates
n_duplicates <- nrow(duplicates)
cat("Number of duplicate rows (excluding PlotID and UniqueID):", n_duplicates, "\n")


library(ggplot2)

cpi_results_er = data.table::fread("VP/severity_tmp/data/saved/outputs/conditional-predictive-impact-results_v5.1.csv")
names(cpi_results_er)

lowest_rmse <- cpi_results_er %>%
  group_by(ecoregion, mtry, sample.fraction, min.node.size, important_variable_rf_formula, rmse_important_variables_overall, r2_important_variables_overall) %>%
  summarize()

head(lowest_rmse)

head(cpi_results_er)
cpi_results = cpi_results_er |>
  group_by(ecoregion) %>%
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

bestr2 <- cpi_results %>%
  group_by(ecoregion) %>%
  slice_max(r2_important_variables_overall, n = 1, with_ties = FALSE) %>%
  ungroup()
names(bestr2)
bestr2table <- bestr2[, c(1:5, 17,24,25)]

bestr2table$r2_important_variables_overall <- signif(bestr2table$r2_important_variables_overall, digits=3)
bestr2table$rmse_important_variables_overall <- signif(bestr2table$rmse_important_variables_overall, digits=3)
head(bestr2table)
write.csv(bestr2table, "VP/severity_tmp/data/saved/outputs/ecoregion_bestr2_v2.csv")

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
