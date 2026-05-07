library(terra)

set.seed(42)

# ── 1. Load data ──────────────────────────────────────────────────────────────

v <- vect("data/input/allcoords_withbaloss_v8.shp")

# ── 2. Identify NPS plots ─────────────────────────────────────────────────────

nps_mask <- startsWith(v$UniqueID, "NPS")
nps_idx  <- which(nps_mask)
cat("Total plots:", nrow(v), "\n")
cat("NPS plots to fuzz:", length(nps_idx), "\n")

# ── 3. Fuzz NPS coordinates within a 1-mile circular buffer ───────────────────
# Random point in a disk: r = sqrt(U) * radius ensures uniform area distribution.
# Offsets are computed in geographic degrees using per-latitude longitude scaling.

all_xy   <- crds(v)
nps_xy   <- all_xy[nps_idx, , drop = FALSE]

MILES_TO_M <- 1609.344
radius_m   <- 1 * MILES_TO_M
M_PER_DEG_LAT <- 111320  # meters per degree of latitude (approx constant)

fuzzed_xy <- t(vapply(seq_len(nrow(nps_xy)), function(i) {
  lon <- nps_xy[i, 1]
  lat <- nps_xy[i, 2]
  r     <- sqrt(runif(1)) * radius_m
  theta <- runif(1) * 2 * pi
  dlat  <- r * cos(theta) / M_PER_DEG_LAT
  dlon  <- r * sin(theta) / (M_PER_DEG_LAT * cos(lat * pi / 180))
  c(lon + dlon, lat + dlat)
}, numeric(2)))

# ── 4. Swap 20% of NPS plots within each fire ─────────────────────────────────
# Plots are selected per fire; their fuzzed locations are permuted among
# themselves using a random cyclic shift so no plot retains its own fuzzed spot.
#
# n_swap = round(n * 0.2), minimum 2 when n >= 2 (swap requires at least 2 plots).
# Fires with only 1 NPS plot cannot be swapped and are left as fuzzed-only.

nps_df <- data.frame(
  row    = seq_len(nrow(nps_xy)),  # index into fuzzed_xy rows
  fire   = v$Fire[nps_idx],
  fuzz_x = fuzzed_xy[, 1],
  fuzz_y = fuzzed_xy[, 2],
  swapped = FALSE
)

fires <- unique(nps_df$fire)

for (fire in fires) {
  fire_rows <- which(nps_df$fire == fire)
  n         <- length(fire_rows)

  if (n < 3) {
    cat("Fire '", fire, "': fewer than 3 NPS plots, skipping swap.\n", sep = "")
    next
  }

  n_swap <- max(round(n * 0.2), 2L)  # minimum 2 for a meaningful swap
  cat("Fire '", fire, "': ", n, " NPS plots, swapping ", n_swap, ".\n", sep = "")

  swap_rows <- sample(fire_rows, n_swap)

  # Cyclic shift by a random non-zero amount — guarantees no plot keeps its own location
  shift      <- sample(seq_len(n_swap - 1), 1)
  perm       <- ((seq_len(n_swap) - 1 + shift) %% n_swap) + 1

  orig_x <- nps_df$fuzz_x[swap_rows]
  orig_y <- nps_df$fuzz_y[swap_rows]
  nps_df$fuzz_x[swap_rows]  <- orig_x[perm]
  nps_df$fuzz_y[swap_rows]  <- orig_y[perm]
  nps_df$swapped[swap_rows] <- TRUE
}

# ── 5. Build output SpatVector with updated NPS coordinates ───────────────────

out_xy             <- all_xy
out_xy[nps_idx, 1] <- nps_df$fuzz_x
out_xy[nps_idx, 2] <- nps_df$fuzz_y

# Reconstruct SpatVector from attribute table + new geometry
out_df        <- as.data.frame(v)
out_df$x      <- out_xy[, 1]
out_df$y      <- out_xy[, 2]
out_df$nps_fuzzed  <- nps_mask
out_df$nps_swapped <- FALSE
out_df$nps_swapped[nps_idx] <- nps_df$swapped

v_fuzz <- vect(out_df, geom = c("x", "y"), crs = crs(v))

# ── 6. Summary ────────────────────────────────────────────────────────────────

n_fuzz_only <- sum(out_df$nps_fuzzed & !out_df$nps_swapped)
n_swapped   <- sum(out_df$nps_swapped)
cat("\nSummary:\n")
cat("  NPS plots fuzzed (buffer-only):", n_fuzz_only, "\n")
cat("  NPS plots additionally swapped:", n_swapped,   "\n")
cat("  Swap fraction: ", round(n_swapped / length(nps_idx) * 100, 1), "%\n", sep = "")

# ── 7. Save fuzzed shapefile ──────────────────────────────────────────────────

out_path <- "data/saved/allcoords_withbaloss_v9_fuzzed.shp"
writeVector(v_fuzz, out_path, overwrite = TRUE)
cat("\nFuzzed shapefile written to:", out_path, "\n")

# ── 8. Build shareable ARD shapefile with fuzzed NPS coordinates ──────────────
# ARD_20250804.gpkg is the full spatial ARD (3280 plots, no fold duplication).
# Coordinates are replaced for NPS plots using the fuzzed locations from v_fuzz;
# all attribute values stay unchanged. Saved as shapefile for GEE compatibility.

v_ard <- vect("data/saved/ARD_20250804.gpkg")

# Build UniqueID → fuzzed coordinate lookup from v_fuzz
coords_fuzz          <- as.data.frame(crds(v_fuzz))
coords_fuzz$UniqueID <- v_fuzz$UniqueID

# Replace NPS geometry: reconstruct SpatVector with updated coordinates
ard_xy           <- crds(v_ard)
idx              <- match(v_ard$UniqueID, coords_fuzz$UniqueID)
ard_xy[, 1]     <- coords_fuzz$x[idx]
ard_xy[, 2]     <- coords_fuzz$y[idx]

ard_df           <- as.data.frame(v_ard)
ard_df$x         <- ard_xy[, 1]
ard_df$y         <- ard_xy[, 2]
v_ard_share      <- vect(ard_df, geom = c("x", "y"), crs = crs(v_ard))

ard_share_path   <- "data/saved/ARD_20250804_share.shp"
writeVector(v_ard_share, ard_share_path, overwrite = TRUE)
cat("Shareable ARD shapefile written to:", ard_share_path, "\n")
cat("  Features:", nrow(v_ard_share), "| Fields:", ncol(as.data.frame(v_ard_share)), "\n")
