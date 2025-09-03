
## Get the best model for each ecoregion
# Also some code for Pareto frontier, which we don't use in the paper

library(ggplot2)
library(dplyr)

data_dir <- "data/"
fig_dir <- "figs/"

source("R/utils.R")

cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))

## filter to ecoregion models
cpi_results_er = cpi_results_full[cpi_results_full$domain != "western-us", ]
names(cpi_results_er)

head(cpi_results_er)
cpi_results = cpi_results_er |>
  group_by(domain) %>%
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

# Get the model with the best R2 (overall = plot-based calc rather than avg of fold R2s)
bestr2 <- cpi_results %>%
  group_by(domain) %>%
  slice_max(r2_important_variables_overall, n = 1, with_ties = FALSE) %>%
  ungroup()
names(bestr2)
bestr2table <- bestr2[, c(1:5, 18,20:22,26:27)]

bestr2table$r2_important_variables_overall <- signif(bestr2table$r2_important_variables_overall, digits=3)
bestr2table$r2_mean_important_variables <- signif(bestr2table$r2_mean_important_variables, digits=3)
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


#####
# testing Arizona Mountains 
best_fit_ar <- bestr2table[1, c(3:6)]

filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)

cv_results_az <- cross_validate(data = ard[ard$domain=="Arizona Mountains forests",], hyperparameters = best_fit_ar) 

coef_of_determin(obs=cv_results_az$obs, pred=cv_results_az$pred)
caret::R2(obs=cv_results_az$obs, pred=cv_results_az$pred)


# examining by fold
fold_summ <- cv_results_az %>%
  dplyr::group_by(fold) %>%
  dplyr::summarize(
    n = dplyr::n(),
    r2 = coef_of_determin(obs, pred)
  )
fold_summ

ard %>%
  filter(domain == "Arizona Mountains forests") %>%
  distinct(Fire)
