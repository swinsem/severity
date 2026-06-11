source("./R/utils.R")

# Random forest cross validation

library(ranger)
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

# Read data
cpi_results_full = data.table::fread(paste0(data_dir, "saved/conditional-predictive-impact-results_v7.0.csv"))


## filter for western-us domain results
cpi_results_west <- cpi_results_full[cpi_results_full$domain == "western-us", ]

cpi_results_west = cpi_results_west |>
  dplyr::select(-Variable, -CPI, -SE, -test, -statistic, -estimate, -p.value, -ci.lo) |>
  unique()

# Plot Pareto frontier of cross-fold mean R2 and overall R2 of important variable reduced model
r2_pareto_front = rPref::psel(
  df = cpi_results_west,
  pref = rPref::high(r2_important_variables_overall) * rPref::high(r2_mean_important_variables)
)

top_results = r2_pareto_front |> na.omit()

# best_fit = top_results[2,]


#### Pareto frontier with parsimony + max pairwise correlation constraint ####

# Need ard_west here for correlation calculation
ard_west <- ard[ard$domain == "western-us", ]

# For each unique variable set, find the highest absolute Pearson correlation
# between any two predictors. Computed once per unique formula for efficiency.
get_max_corr <- function(formula_str, data) {
  vars <- trimws(strsplit(sub(".*~\\s*", "", formula_str), "\\s*\\+\\s*")[[1]])
  vars <- vars[vars %in% names(data)]
  if (length(vars) < 2) return(NA_real_)
  cm <- cor(data[, vars], use = "complete.obs", method = "pearson")
  diag(cm) <- NA
  max(abs(cm), na.rm = TRUE)
}

unique_formulas <- unique(cpi_results_west$important_variable_rf_formula)
corr_lookup     <- setNames(
  vapply(unique_formulas, get_max_corr, numeric(1), data = as.data.frame(ard_west)),
  unique_formulas
)
cpi_results_west$max_corr <- corr_lookup[cpi_results_west$important_variable_rf_formula]

# Base set: complete overall R2, max pairwise correlation < 0.9
cpi_low_corr <- cpi_results_west |>
  filter(!is.na(r2_important_variables_overall), max_corr < 0.9)

# Version 1: parsimony + overall R2 only (no fold R2)
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

top_results_corr_v1
write.csv(top_results_corr_v1, paste0(data_dir, "saved/pareto_frontier_v1.csv"))

## Chosen model: the 4-variable formula with mtry 3, min.node.size 50, and sample.fraction 0.632
best_fit <- top_results_corr_v1 |>
  filter(
    important_variable_rf_formula == "pcnt_ba_mo ~ dnir + northness + rdnbr + zScorePrecip1",
    mtry          == 3,
    min.node.size == 50
  )

# Version 2: parsimony + overall R2 + mean fold R2
top_results_corr_v2 <- rPref::psel(
  df   = cpi_low_corr |> filter(!is.na(r2_mean_important_variables)),
  pref = rPref::low(n_important_variables) * rPref::high(r2_important_variables_overall) *
           rPref::high(r2_mean_important_variables)
) |>
  select(n_important_variables, important_variable_rf_formula,
         mtry, min.node.size, sample.fraction,
         r2_mean_important_variables, r2_important_variables_overall, max_corr) |>
  mutate(across(starts_with("r2_"), \(x) signif(x, 3)),
         max_corr = round(max_corr, 3)) |>
  arrange(n_important_variables, desc(r2_important_variables_overall)) |>
  as_tibble()

top_results_corr_v2
write.csv(top_results_corr_v2, paste0(data_dir, "saved/pareto_frontier_v2.csv"))


##### cross-validation #####

# filter to full western-us model only
ard_west <- ard[ard$domain=="western-us",]

modelstats <- cpi_results_west[cpi_results_west$mtry==3 & cpi_results_west$min.node.size==50 & cpi_results_west$n_important_variables==4 & cpi_results_west$important_variable_rf_formula== "pcnt_ba_mo ~ dnir + northness + rdnbr + zScorePrecip1",]

# Run CV function
cv_results <- cross_validate(data = ard_west, hyperparameters = best_fit) 


str(cv_results)
names(cv_results) <- c("UniqueID", "obs", "pred", "fold")

# Check results
head(cv_results)
coef_of_determin(obs=cv_results$obs, pred=cv_results$pred)
caret::RMSE(obs=cv_results$obs, pred=cv_results$pred)
caret::MAE(obs=cv_results$obs, pred=cv_results$pred)

### Categorical binning

## three classes (Miller et al 2009)
cv_results$pred_bin <- ifelse(cv_results$pred < 0.25, 1, 
                              ifelse(cv_results$pred >= 0.25 & cv_results$pred < 0.75, 2, 3))

# Could also rewrite the nested ifelse() using dplyr::case_when()
# The assignments are made in order, so just using TRUE for the last case 
# is just saying "everything else not covered by previous cases is a 3"
# cv_results <- cv_results |> 
#   dplyr::mutate(
#     pred_bin = dplyr::case_when(
#       pred < 0.25 ~ 1, 
#       pred >= 0.25 & pred < 0.75 ~ 2, 
#       TRUE ~ 3
#     )
#   )

cv_results$obs_bin <- ifelse(cv_results$obs < 0.25, 1,  
                             ifelse(cv_results$obs >= 0.25 & cv_results$obs < 0.75, 2, 3))

## five classes - overwrites above
cv_results$pred_bin <- ifelse(cv_results$pred < 0.25, 1, 
                              ifelse(cv_results$pred >= 0.25 & cv_results$pred < 0.5, 2, 
                                     ifelse(cv_results$pred >= 0.5 & cv_results$pred < 0.75, 3, 
                                            ifelse(cv_results$pred >= 0.75 & cv_results$pred < 0.9, 4, 5))))
cv_results$obs_bin <- ifelse(cv_results$obs < 0.25, 1,  
                             ifelse(cv_results$obs >= 0.25 & cv_results$obs < 0.5, 2, 
                                    ifelse(cv_results$obs >= 0.5 & cv_results$obs < 0.75, 3, 
                                           ifelse(cv_results$obs >= 0.75 & cv_results$obs < 0.9, 4, 5))))

##### 
##### SAVE THE MODEL #####

# Merge with ecoregion
cv_results <- merge(cv_results, ard_west[, c("UniqueID", "ecoregion")], by = "UniqueID")
names(cv_results)

write.csv(cv_results, paste0(data_dir, "saved/ranger_cv_results_20260506.csv"), row.names=FALSE)


# Fit on all data
final_mod <- ranger::ranger(
  formula = as.formula(best_fit$important_variable_rf_formula),
  data = ard_west,
  num.trees = 1000,
  mtry = best_fit$mtry,
  min.node.size = best_fit$min.node.size,
  sample.fraction = best_fit$sample.fraction,
  seed = 20250709
)

# 2) Save it once
saveRDS(
  object = final_mod, 
  file = here::here("data/rf_final_model_050626.rds")
)
