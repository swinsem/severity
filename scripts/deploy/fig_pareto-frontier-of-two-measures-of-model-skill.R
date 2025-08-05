cpi_results_version <- "v6.0"

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
  ggplot2::aes(x = r2_important_variables_overall, y = r2_median_important_variables)
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

