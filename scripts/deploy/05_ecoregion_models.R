source("R/utils.R")

## Get the best model for each ecoregion

library(ggplot2)
library(dplyr)

data_dir <- "data/"
fig_dir <- "figs/"


#### read ard with spatial folds from github severity/data folder
filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)

# replace the domains with > 300 rows (except western-us) for ecoregional models
domains_to_replace <- ard %>%
  count(domain, name = "n") %>%
  filter(n > 300, domain != "western-us") %>%
  pull(domain)


# Read data
cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))
cpi_results_large = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.1.csv"))

# splice in the new domain data
cpi_results_full <- bind_rows(
  cpi_results_full  %>% filter(!(domain %in% domains_to_replace)),
  cpi_results_large %>% filter(  domain %in% domains_to_replace)
)

## filter to ecoregion models
cpi_results_er = cpi_results_full[cpi_results_full$domain != "western-us", ]
names(cpi_results_er)

head(cpi_results_er)
cpi_results = cpi_results_er |>
  group_by(domain) %>%
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

# Compute max absolute pairwise Pearson correlation for each unique variable set.
# Uses western-us rows as the reference dataset (one row per unique plot).
ard_predictor_ref <- ard[ard$domain == "western-us", ]

get_max_corr <- function(formula_str, data) {
  vars <- trimws(strsplit(sub(".*~\\s*", "", formula_str), "\\s*\\+\\s*")[[1]])
  vars <- vars[vars %in% names(data)]
  if (length(vars) < 2) return(NA_real_)
  cm <- cor(data[, vars], use = "complete.obs", method = "pearson")
  diag(cm) <- NA
  max(abs(cm), na.rm = TRUE)
}

unique_formulas_er <- unique(cpi_results$important_variable_rf_formula)
corr_lookup_er <- setNames(
  vapply(unique_formulas_er, get_max_corr, numeric(1), data = as.data.frame(ard_predictor_ref)),
  unique_formulas_er
)
cpi_results <- cpi_results |>
  ungroup() |>
  mutate(max_corr = corr_lookup_er[important_variable_rf_formula])

# Get the model with the best R2 (overall = plot-based calc rather than avg of fold R2s)
bestr2 <- cpi_results %>%
  group_by(domain) %>%
  slice_max(r2_important_variables_overall, n = 1, with_ties = FALSE) %>%
  ungroup()
names(bestr2)
bestr2table <- bestr2[, c(1:5, 18,20:22,26:27, 30)]

bestr2table$r2_important_variables_overall <- signif(bestr2table$r2_important_variables_overall, digits=3)
bestr2table$r2_mean_important_variables <- signif(bestr2table$r2_mean_important_variables, digits=3)
head(bestr2table)

write.csv(bestr2table, paste0(data_dir, "saved/ecoregion_models_bestr2.csv"))


#### Pareto frontier — ecoregion models ####
# Selects non-dominated models per ecoregion: no other model is simultaneously
# better on both cross-fold mean R2 and overall R2. Mirrors the approach in 03
# for the western-us model. Rows with NA on either R2 are excluded first.

cpi_er_complete <- cpi_results |>
  filter(
    !is.na(r2_important_variables_overall),
    !is.na(r2_mean_important_variables),
    max_corr < 0.85 # remove models where two variables are correlated 0.85 or greater
  )

pareto_er <- cpi_er_complete |>
  group_by(domain) |>
  group_modify(~ rPref::psel(
    .x,
    rPref::high(r2_important_variables_overall) * rPref::low(n_important_variables)
  )) |>
  ungroup()

pareto_table <- pareto_er |>
  select(
    ecoregion      = domain,
    n              = n_obs,
    n_vars         = n_important_variables,
    formula        = important_variable_rf_formula,
    mtry, min.node.size, sample.fraction,
    r2_folds       = r2_mean_important_variables,
    r2_overall     = r2_important_variables_overall,
    max_corr
  ) |>
  mutate(
    r2_folds   = signif(r2_folds, 3),
    r2_overall = signif(r2_overall, 3),
    max_corr   = round(max_corr, 3)
  ) |>
  arrange(ecoregion, desc(r2_overall))

pareto_table


###
# r2 important variables x number of important variables
# then select model based on parsimony, model skill, and low correlation between features


