
### I don't remember if I was actually using this script - check against other scripts!

set.seed(20240724)

# read data and set up spatial folds
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard = sf::st_read("VP/severity_tmp/data/saved/ARD_08262024.gpkg") |>
  dplyr::filter(!is.na(pcnt_ba_mo)) |> 
  dplyr::select(-lat, -aspectRad)

ard_for_task = ard |> 
  spatialsample::spatial_clustering_cv(v = 10)

ard_with_spatial_folds = ard_for_task |>
  purrr::pmap(.f = function(id, splits) {
    
    spatial_fold <- id
    assessment_data =
      splits |>
      rsample::assessment() |>
      sf::st_drop_geometry()
    
    return(cbind(assessment_data, spatial_fold))
  }) |>
  data.table::rbindlist() |>
  dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
  tibble::as_tibble()

features = ard |> 
  sf::st_drop_geometry() |> 
  dplyr::select(-c(PlotID, YrFireName, Dataset, pcnt_ba_mo, UniqueID)) |> 
  colnames()

target = "pcnt_ba_mo"


## using top_results from other script
best_fit = top_results[2,]

fm1 = ranger::ranger(
  formula = as.formula(best_fit$important_variable_rf_formula), 
  data = sf::st_drop_geometry(ard[, c(target, features)]), 
  num.trees = 1000, 
  mtry = best_fit$mtry, 
  min.node.size = best_fit$min.node.size, 
  sample.fraction = best_fit$sample.fraction
)

spatial_folds = unique(ard_with_spatial_folds$spatial_fold)
model_assessment_data = vector(mode = "list", length = length(spatial_folds))

for(i in seq_along(spatial_folds)) {
  
  print(i)
  train_data = ard_with_spatial_folds |> 
    sf::st_drop_geometry() |> 
    dplyr::filter(spatial_fold != spatial_folds[i])
  
  test_data = ard_with_spatial_folds |> 
    sf::st_drop_geometry() |> 
    dplyr::filter(spatial_fold == spatial_folds[i])
  
  
  fm = ranger::ranger(
    formula = as.formula(best_fit$important_variable_rf_formula), 
    data = train_data, 
    num.trees = 1000, 
    mtry = best_fit$mtry, 
    min.node.size = best_fit$min.node.size, 
    sample.fraction = best_fit$sample.fraction
  )
  
  out = tibble::tibble(
    obs = test_data$pcnt_ba_mo,
    preds = predict(object = fm, data = test_data)$predictions
  )
  
  model_assessment_data[[i]] = out
}

model_assessment_data = dplyr::bind_rows(model_assessment_data)
caret::R2(pred = model_assessment_data$preds, obs = model_assessment_data$obs)

ggplot(model_assessment_data, aes(x = obs, y = preds)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  theme_bw()


head(model_assessment_data)


full_rf_formula = as.formula(
  glue::glue(
    "{target} ~ {paste(features, collapse = ' + ')}"
  )
)
full_model = ranger::ranger(
  formula = full_rf_formula, 
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


plot_data_full = calc_plot_data(
  var_names = per_variable_cpi_results$Variable,
  fitted_model = full_model,
  variable_order = per_variable_cpi_results$Variable
)

facet_labels <- c(
  "zScorePrecip1" = "Z-Score Precipitation",
  "northness" = "Northness",
  "dswir2swir1" = "SWIR2/SWIR1 Difference"
)
ggplot(plot_data_important_vars, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(facets = "var", scales = "free", 
             labeller = labeller(var = facet_labels)) +
  theme_bw()

ggplot(plot_data_full, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(facets = "var", scales = "free") +
  theme_bw()

summary(lm(formula = pcnt_ba_mo ~ zScorePrecip1, data = ard[, c("pcnt_ba_mo", "zScorePrecip1")]))

ggplot(ard, aes(x = zScorePrecip1, y = pcnt_ba_mo)) +
  geom_point() +
  geom_smooth()

ggplot(ard, aes(x = rdnbr, y = zScorePrecip1, col = pcnt_ba_mo)) +
  geom_point()

ard |> 
  dplyr::mutate(zscoreprecip1_fct = zScorePrecip1 > 0) |> 
  ggplot(aes(x = rdnbr, y = pcnt_ba_mo)) +
  geom_point() +
  facet_wrap(facets = "zscoreprecip1_fct") +
  geom_smooth()

ggplot(ard, aes(x = rdnbr, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point() +
  geom_smooth() +
  scale_x_continuous(limits = c(-100, 1500))

precip_mort_plot_data = ard |> 
  dplyr::group_by(zScorePrecip1 > 0) |> 
  dplyr::summarize(pcnt_ba_mo = mean(pcnt_ba_mo))

plot(precip_mort_plot_data[, "zScorePrecip1 > 0"])


library(ggplot2)

ggplot(plot_data, aes(x = x, y = y)) +
  geom_line() +
  facet_wrap(facets = "var", scales = "free") +
  theme_bw()

# rdnbr_post_nbr_two_way_ale = ALEPlot::ALEPlot(
#   X = sf::st_drop_geometry(ard[, fm1$forest$independent.variable.names]), 
#   X.model = fm1, 
#   J = c("dswir2swir1", "zScorePrecip1"), 
#   pred.fun = yhat
# )
# 
# image(
#   rdnbr_post_nbr_two_way_ale$x.values[[1]], 
#   rdnbr_post_nbr_two_way_ale$x.values[[2]], 
#   rdnbr_post_nbr_two_way_ale$f.values, 
#   xlab = "RdNBR", 
#   ylab = "post_nbr", 
#   col = hcl.colors(n = 100, palette = "Blue-Red")
# )
# 
# contour(
#   rdnbr_post_nbr_two_way_ale$x.values[[1]], 
#   rdnbr_post_nbr_two_way_ale$x.values[[2]], 
#   rdnbr_post_nbr_two_way_ale$f.values, 
#   add=TRUE, 
#   drawlabels=TRUE
# )

yhat <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)

# dSWIR2SWIR1 to precip

# Save the plot as a PNG file
png("VP/severity_tmp/plots/two_way_ale_precip_dswir.png", width = 6, height = 5, units="in", res=300)

zscoreprecip1_dswir_two_way_ale = ALEPlot::ALEPlot(
  X = sf::st_drop_geometry(ard[, fm1$forest$independent.variable.names]), 
  X.model = fm1, 
  J = c("zScorePrecip1", "dswir2swir1"), 
  pred.fun = yhat
)

image(
  zscoreprecip1_dswir_two_way_ale$x.values[[1]], 
  zscoreprecip1_dswir_two_way_ale$x.values[[2]], 
  zscoreprecip1_dswir_two_way_ale$f.values, 
  xlab = "Z-score Precipitation", 
  ylab = "dSWIR2:SWIR1", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
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

image(
  northness_dswir_two_way_ale$x.values[[1]], 
  northness_dswir_two_way_ale$x.values[[2]], 
  northness_dswir_two_way_ale$f.values, 
  xlab = "Northness", 
  ylab = "dSWIR2:SWIR1", 
  col = hcl.colors(n = 100, palette = "Blue-Red")
)

dev.off()

## Northness:Precip - hard to interpret

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
# # more confusing
# contour(
#   zscoreprecip1_rdnbr_two_way_ale$x.values[[1]], 
#   zscoreprecip1_rdnbr_two_way_ale$x.values[[2]], 
#   zscoreprecip1_rdnbr_two_way_ale$f.values, 
#   add=TRUE, 
#   drawlabels=TRUE
# )
