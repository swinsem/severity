
library(terra)

data_dir <- "research/severity/data/"

ard01 <- read.csv(paste0("VP/severity_tmp/data/saved/ARD_01282025.csv"))
ard06 <- read.csv("../winsemius/GitHub/severity/data/saved/ARD_20250804.csv")

names(ard01)
names(ard06)

library(dplyr)
library(purrr)
library(tidyr)

## Choose join key 
key <- c("UniqueID") 

stopifnot(all(key %in% names(ard01)), all(key %in% names(ard06)))

## Keep only common, non-key columns 
common_cols <- intersect(names(ard01), names(ard06)) |> setdiff(key)

x <- ard01 %>% select(all_of(key), all_of(common_cols))
y <- ard06 %>% select(all_of(key), all_of(common_cols))

## Check for missing/extra rows by key 
only_in_ard01 <- anti_join(x, y, by = key)
only_in_ard06 <- anti_join(y, x, by = key)

# If these aren't empty, there are key mismatches:
nrow(only_in_ard01); nrow(only_in_ard06)

## Align rows and suffix columns for comparison 
z <- inner_join(x, y, by = key, suffix = c(".ard01", ".ard06"))

## Define an equality with tolerance for numerics 
tol <- 1e-6
eq_with_tol <- function(a, b, tol = 1e-6) {
  # treat both-NA as equal
  both_na <- is.na(a) & is.na(b)
  # numeric with tolerance
  if (is.numeric(a) && is.numeric(b)) {
    eq <- dplyr::near(a, b, tol = tol)
    # near() returns NA when either side NA; fix that:
    eq[is.na(eq)] <- both_na[is.na(eq)]
    return(eq)
  }
  # non-numeric exact match (string/logical/factor) or both NA
  (a == b) | both_na
}

## Per-column summary 
col_summary <- map_dfr(common_cols, function(col) {
  a <- z[[paste0(col, ".ard01")]]
  b <- z[[paste0(col, ".ard06")]]
  eq <- eq_with_tol(a, b, tol)
  
  out <- tibble(
    column       = col,
    n_rows       = length(eq),
    matches      = sum(eq, na.rm = TRUE),
    mismatches   = sum(!eq, na.rm = TRUE),
    both_na      = sum(is.na(a) & is.na(b)),
    a_only_na    = sum(is.na(a) & !is.na(b)),
    b_only_na    = sum(!is.na(a) & is.na(b))
  )
  
  if (is.numeric(a) && is.numeric(b)) {
    diffs <- abs(a - b)
    out <- mutate(out,
                  max_abs_diff     = suppressWarnings(max(diffs, na.rm = TRUE)),
                  median_abs_diff  = suppressWarnings(median(diffs, na.rm = TRUE))
    )
  } else {
    out <- mutate(out,
                  max_abs_diff    = NA_real_,
                  median_abs_diff = NA_real_
    )
  }
  out
}) %>% arrange(desc(mismatches), desc(median_abs_diff))

col_summary


# safer equality: toleranced for numerics, exact (after as.character) for others
tol <- 1e-6
eq_with_tol <- function(a, b, tol = 1e-6) {
  both_na <- is.na(a) & is.na(b)
  is_num  <- is.numeric(a) && is.numeric(b)
  
  if (is_num) {
    eq <- dplyr::near(a, b, tol = tol)
    eq[is.na(eq)] <- both_na[is.na(eq)]
    eq
  } else {
    a_chr <- as.character(a)
    b_chr <- as.character(b)
    (a_chr == b_chr) | (is.na(a_chr) & is.na(b_chr))
  }
}

# build mismatch_long with stable types across columns
mismatch_long <- map_dfr(common_cols, function(col) {
  a <- z[[paste0(col, ".ard01")]]
  b <- z[[paste0(col, ".ard06")]]
  eq <- eq_with_tol(a, b, tol)
  
  is_num <- is.numeric(a) && is.numeric(b)
  
  tibble(
    !!!z[key],  # keep join keys for traceability
    column       = col,
    value_ard01  = if (is_num) as.character(signif(a, 6)) else as.character(a),
    value_ard06  = if (is_num) as.character(signif(b, 6)) else as.character(b),
    abs_diff     = if (is_num) abs(a - b) else NA_real_,
    equal        = eq
  ) %>% filter(!equal)
})

# quick peek
mismatch_long %>% slice_head(n = 20)

