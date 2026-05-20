library(dplyr)

data_dir <- "data/"
filename  <- "20250804"

# ── Load data ──────────────────────────────────────────────────────────────────

ard <- readr::read_csv(
  here::here(glue::glue("data/ARD_{filename}_with-spatial-folds.csv")),
  col_types = list(spatial_fold = "factor"),
  show_col_types = FALSE
) |>
  dplyr::filter(domain == "western-us")

ftm_fires <- read.csv(
  "../../VP/severity_tmp/data/original/FTM/Data/FTM_fires.csv",
  stringsAsFactors = FALSE
)

# ── Join Fire_type by fire name ────────────────────────────────────────────────
# ARD$Fire matches FTM$YrFireName (both use "YEAR - FireName" format).
# Left join keeps all ARD rows; Fire_type is NA for non-FTM fires.

fire_type_lookup <- ftm_fires |>
  select(Fire = YrFireName, Fire_type) |>
  distinct()

ard <- ard |>
  left_join(fire_type_lookup, by = "Fire")

# ── Summary ────────────────────────────────────────────────────────────────────

cat("Fire_type coverage:\n")
print(table(ard$Fire_type, useNA = "ifany"))

cat("\nMatched fires (unique):\n")
ard |>
  filter(!is.na(Fire_type)) |>
  distinct(Fire, Fire_type) |>
  arrange(Fire_type, Fire) |>
  print(n = Inf)

cat("\nUnmatched ARD fires (no Fire_type assigned):\n")
ard |>
  filter(is.na(Fire_type)) |>
  distinct(Fire) |>
  arrange(Fire) |>
  print(n = Inf)
