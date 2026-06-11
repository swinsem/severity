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


# ── Select one model per ecoregion ────────────────────────────────────────────
# Models are identified by ecoregion + hyperparameter triple so the selection is
# stable if pareto_table gains new rows. Verify ecoregion names against
# unique(ard$domain) if the ARD changes.

library(purrr)

eco_params <- tibble::tribble(
  ~ecoregion,                              ~mtry, ~sample.fraction, ~min.node.size,
  "Arizona Mountains forests",                 4,             0.7,              1,
  "Blue Mountains forests",                    3,             0.4,             70,
  "Central-Southern Cascades Forests",         3,             0.5,             70,
  "Colorado Rockies forests",                  4,        1 - 1/exp(1),         25,
  "Eastern Cascades forests",                  3,             0.9,              1,
  "Klamath-Siskiyou forests",                  3,             0.8,              5,
  "Montana Valley and Foothill grasslands",    4,             0.5,             25,
  "North Cascades conifer forests",                    4,             0.5,              1,
  "Northern Rockies conifer forests",          3,             0.7,             90,
  "Sierra Nevada forests",                     4,             0.4,             90,
  "South Central Rockies forests",             3,             0.5,              5
)

best_eco <- pareto_table |>
  inner_join(
    eco_params |> rename(sf_target = sample.fraction),
    by = c("ecoregion", "mtry", "min.node.size")
  ) |>
  filter(dplyr::near(sample.fraction, sf_target)) |>
  select(-sf_target)

print(best_eco[, c("ecoregion", "n_vars", "formula", "mtry", "min.node.size", "sample.fraction", "r2_overall")])

# Diagnostic: check which eco_params entries didn't match anything in pareto_table
unmatched <- eco_params |>
  anti_join(best_eco, by = c("ecoregion", "mtry", "min.node.size"))
if (nrow(unmatched) > 0) message("No pareto_table match for: ", paste(unmatched$ecoregion, collapse = ", "))

# Diagnostic: check which eco_select names don't appear in ard$domain at all
missing_domains <- setdiff(eco_params$ecoregion, unique(ard$domain))
if (length(missing_domains) > 0) message("Not found in ard$domain: ", paste(missing_domains, collapse = ", "))


# ── First-order ALE plots for each ecoregion's best model ─────────────────────

yhat_eco <- function(X.model, newdata) as.numeric(predict(X.model, newdata)$predictions)

var_labels <- c(
  rdnbr         = "RdNBR",
  dnir          = "dNIR",
  northness     = "Northness",
  zScorePrecip1 = "Z-Precip (yr+1)",
  zScorePrecip0 = "Z-Precip (fire yr)",
  zScoreAET1    = "Z-AET (yr+1)",
  zScoreAET0    = "Z-AET (fire yr)",
  zScoreCWD0    = "Z-CWD (fire yr)",
  zScoreCWD1    = "Z-CWD (yr+1)",
  zScoreVPD0    = "Z-VPD (fire yr)",
  zScoreVPD1    = "Z-VPD (yr+1)",
  meanCWD       = "Mean CWD",
  meanPrecip    = "Mean precip",
  meanVPD       = "Mean VPD",
  meanAET       = "Mean AET",
  meanTPI       = "Mean TPI"
)

n_min <- 50  # minimum plots to fit a stable model and ALE

ale_eco_plots <- purrr::map(seq_len(nrow(best_eco)), function(i) {
  row      <- best_eco[i, ]
  eco_name <- row$ecoregion
  eco_df   <- as.data.frame(ard[ard$domain == eco_name, ])

  if (nrow(eco_df) == 0) {
    message("Skipping ", eco_name, ": domain not found in ard (check name spelling)")
    return(NULL)
  }
  if (nrow(eco_df) < n_min) {
    message("Skipping ", eco_name, ": only ", nrow(eco_df), " plots (< n_min = ", n_min, ")")
    return(NULL)
  }

  mod <- ranger::ranger(
    formula         = as.formula(row$formula),
    data            = eco_df,
    num.trees       = 1000,
    mtry            = row$mtry,
    min.node.size   = row$min.node.size,
    sample.fraction = row$sample.fraction,
    seed            = 20250709
  )

  vars <- mod$forest$independent.variable.names

  ale_data <- purrr::map(vars, function(v) {
    ale <- ALEPlot::ALEPlot(
      X        = eco_df[, vars],
      X.model  = mod,
      J        = v,
      pred.fun = yhat_eco
    )
    tibble::tibble(var = v, x = ale$x.values, y = ale$f.values)
  }) |>
    data.table::rbindlist()

  # Order facets by effect size (ALE range), largest first
  var_rank <- ale_data |>
    group_by(var) |>
    summarize(delta = max(y) - min(y), .groups = "drop") |>
    arrange(desc(delta))

  ale_data <- ale_data |>
    mutate(var = factor(var, levels = var_rank$var))

  p <- ggplot(ale_data, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
    geom_line(linewidth = 0.9, colour = "#0072B2") +
    geom_rug(sides = "b", alpha = 0.4) +
    facet_wrap(~ var, scales = "free", labeller = labeller(var = var_labels)) +
    labs(title = eco_name, x = NULL, y = "ALE (BA loss)") +
    theme_bw() +
    theme(strip.text = element_text(size = 9))

  eco_slug <- gsub("[^a-z0-9]+", "_", tolower(eco_name))
  ggsave(paste0(fig_dir, "ale_ecoregion_", eco_slug, ".png"), p,
         width = 6, height = 4, dpi = 300)

  p
})
