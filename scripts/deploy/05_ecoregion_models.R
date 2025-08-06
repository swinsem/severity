
## Get the best model for each ecoregion
# Also some code for Pareto frontier, which we don't use in the paper

library(ggplot2)


cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))

## filter to ecoregion models
cpi_results_er = cpi_results_full[cpi_results_full$domain != "western-us", ]
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

# Get the model with the best R2 (overall = plot-based calc rather than avg of fold R2s)
bestr2 <- cpi_results %>%
  group_by(ecoregion) %>%
  slice_max(r2_important_variables_overall, n = 1, with_ties = FALSE) %>%
  ungroup()
names(bestr2)
bestr2table <- bestr2[, c(1:5, 17,24,25)]

bestr2table$r2_important_variables_overall <- signif(bestr2table$r2_important_variables_overall, digits=3)
bestr2table$rmse_important_variables_overall <- signif(bestr2table$rmse_important_variables_overall, digits=3)
head(bestr2table)

write.csv(bestr2table, paste0(data_dir, "saved/ecoregion_models_bestr2.csv"))


##########################################
##### Pareto frontier plot #####
##########################################

# Plot Pareto frontier of cross-fold mean R2 and overall R2 of important variable reduced model
# We didn't use the Pareto frontier for selecting the best model, but one could!

r2_pareto_front = rPref::psel(
  df = cpi_results,
  pref = rPref::high(r2_important_variables_overall) * rPref::high(r2_important_variables)
)

ggplot2::ggplot(cpi_results, ggplot2::aes(x = r2_important_variables_overall, y = r2_important_variables, color=ecoregion)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = r2_pareto_front, shape=2, size = 3) +
  ggplot2::theme_bw() +
  labs(y="R2 - mean of spatial folds", x="R2 - overall mean across plots")
ggsave(paste0(fig_dir, "pareto_frontier_ecoregional.png"), width = 6, height = 3, units = "in")


