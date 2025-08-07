cpi_results_version <- "v7.0"

cpi_results_fname <- glue::glue(
  "data/processed/",
  "conditional-predictive-impact-results_{cpi_results_version}.csv"
)

cpi_results_full = data.table::fread(cpi_results_fname)

cpi_results <- cpi_results_full |>
  dplyr::select(
    !c(
      Variable, CPI, SE, test, statistic, estimate, p.value, ci.lo
    )
  )|>
  unique()

if(cpi_results_version == "v4.0") {
  
  cpi_results_westwide <- cpi_results |> 
    dplyr::mutate(r2_mean_important_variables = r2_important_variables)
  
} else {
  
  cpi_results_westwide <- cpi_results |> 
    dplyr::filter(domain == "western-us")
  
}

# Plot Pareto frontier of cross-fold mean R2 and overall R2 of 
# important variable reduced model
r2_pareto_front = rPref::psel(
  df = cpi_results_westwide, 
  pref = rPref::high(r2_important_variables_overall) * 
    rPref::high(r2_mean_important_variables)
)

# Points on the Pareto Frontier will be plotted as red, with all other points
# plotted as black
ggplot2::ggplot(
  cpi_results_westwide, 
  ggplot2::aes(x = r2_important_variables_overall, y = r2_mean_important_variables)
) + 
  ggplot2::geom_point() +
  ggplot2::geom_point(data = r2_pareto_front, color = "red", size = 3) +
  ggplot2::theme_bw()

top_results = r2_pareto_front |> na.omit()

top_results_simple <- top_results |> 
  dplyr::select(
    mtry, sample.fraction, min.node.size, 
    r2_mean_important_variables, r2_median_important_variables,
    r2_important_variables_overall, important_variable_rf_formula
  )

top_results_simple

# Most parsimonious Pareto Frontier model is also the one with the highest
# cross-fold mean R^2
# pcnt_ba_mo ~ dndvi + dred + meanTPI + northness + post_swir2swir1 + zScorePrecip1
cpi_results_westwide |> 
  dplyr::filter(
    r2_mean_important_variables == max(
      r2_mean_important_variables, 
      na.rm = TRUE
    )
  ) |> dplyr::select(
    mtry, sample.fraction, min.node.size, 
    r2_mean_full, r2_full_overall,
    r2_mean_important_variables, r2_median_important_variables,
    r2_important_variables_overall, important_variable_rf_formula
  )

# Interestingly, here's what the "top result" would have looked like if we
# didn't follow up and test the model skill of the reduced model. This is the
# set of hyperparameters that yielded the best model skill based on its cross-
# fold R^2 value. It's a pretty good R^2 (0.417) and pretty comparable to the
# cross-fold R^2 value of the parsimonious model identified above (compare to
# that R^2 of 0.431). The important variables that are identified are similar
# to the parsimonious model (3 of the 4 are identical). 
# pcnt_ba_mo ~ HLI + meanTPI + northness + zScorePrecip1
# But if we build the parsimonious model, it does a terrible job of prediction
# Cross-fold R^2 drops to -0.629. Is this just further demonstration that we
# really need to do both hyperparameter tuning and variable selection in the
# same step? And then confirm that the reduced model really is skillful?

cpi_results_westwide |> 
  dplyr::filter(
    r2_mean_full == max(
      r2_mean_full, 
      na.rm = TRUE
    )
  ) |> 
  dplyr::select(
    mtry, sample.fraction, min.node.size, 
    r2_mean_full, r2_full_overall,
    r2_mean_important_variables, r2_median_important_variables,
    r2_important_variables_overall, important_variable_rf_formula
  )


cpi_results_westwide |> 
  dplyr::filter(
    r2_mean_full == 0.4172211
  ) |> 
  dplyr::select(
    mtry, sample.fraction, min.node.size, 
    r2_mean_full, r2_full_overall,
    r2_mean_important_variables, r2_median_important_variables,
    r2_important_variables_overall, important_variable_rf_formula
  )
