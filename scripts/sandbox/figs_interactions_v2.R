library(ggplot2)
library(dplyr)
library(patchwork)

filename <- "20250804"
ard <- readr::read_csv(
  here::here(glue::glue("data/ARD_{filename}_with-spatial-folds.csv")),
  col_types = list(spatial_fold = "factor"),
  show_col_types = FALSE
)
ard_west <- ard[ard$domain == "western-us", ]

fig_dir <- "figs/"

# ── Validation: confirm binary split semantics ────────────────────────────────
# With scale_color_manual, unnamed `labels` are assigned to `breaks` in their
# natural order. For a logical aesthetic, that order is FALSE < TRUE. Older
# ggplot2 versions (pre-3.3) instead used the ORDER OF THE VALUES VECTOR, so
# values = c("TRUE"=..., "FALSE"=...) + labels = c("A","B") gave "A" → TRUE.
# The safe fix everywhere: always specify breaks = c(FALSE, TRUE) explicitly.
#
# The checks below confirm which group each split actually selects, so you can
# verify color assignments against the faceted diagnostic plots.

precip_check <- ard_west |>
  mutate(wet = zScorePrecip1 > 0) |>
  group_by(wet) |>
  summarise(mean_zPrecip = mean(zScorePrecip1, na.rm = TRUE),
            mean_ba_loss = mean(pcnt_ba_mo, na.rm = TRUE),
            n = n())
cat("Precip split — TRUE should have mean_zPrecip > 0 (wetter than average):\n")
print(as.data.frame(precip_check))

north_check <- ard_west |>
  mutate(north = northness > 0) |>
  group_by(north) |>
  summarise(mean_northness = mean(northness, na.rm = TRUE),
            mean_ba_loss   = mean(pcnt_ba_mo, na.rm = TRUE),
            n = n())
cat("\nNorthness split — TRUE should have mean_northness > 0 (north-facing):\n")
print(as.data.frame(north_check))

# Facet diagnostics: panel labels show the actual logical value (TRUE/FALSE),
# so you can directly compare these against the colored versions below.
p_diag_precip <- ggplot(ard_west, aes(x = rdnbr, y = pcnt_ba_mo)) +
  geom_point(alpha = 0.12, size = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), color = "#2166AC") +
  coord_cartesian(xlim = c(-634, 1300)) +
  facet_wrap(~ (zScorePrecip1 > 0), labeller = label_both) +
  labs(title = "Diagnostic: zScorePrecip1 > 0  (TRUE = wetter, FALSE = drier)",
       x = "RdNBR", y = "Basal area loss") +
  theme_bw()
ggsave(paste0(fig_dir, "diag_precip_split_rdnbr.png"), p_diag_precip,
       width = 8, height = 4, units = "in", dpi = 200)

p_diag_north <- ggplot(ard_west, aes(x = rdnbr, y = pcnt_ba_mo)) +
  geom_point(alpha = 0.12, size = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6), color = "#2166AC") +
  coord_cartesian(xlim = c(-634, 1300)) +
  facet_wrap(~ (northness > 0), labeller = label_both) +
  labs(title = "Diagnostic: northness > 0  (TRUE = north-facing, FALSE = south-facing)",
       x = "RdNBR", y = "Basal area loss") +
  theme_bw()
ggsave(paste0(fig_dir, "diag_northness_split_rdnbr.png"), p_diag_north,
       width = 8, height = 4, units = "in", dpi = 200)

cat("\nDiagnostic plots saved. Compare the TRUE/FALSE panel curves against",
    "\nthe colored versions below to confirm label orientation.\n")


# ── Color scales — explicit breaks lock label ordering ────────────────────────
# breaks = c(FALSE, TRUE) guarantees: labels[1] → FALSE, labels[2] → TRUE
# regardless of ggplot2 version or the order of the named values vector.
# Convention: blue = wetter / north-facing, warm yellow-orange = drier / south-facing.

col_precip <- scale_color_manual(
  values = c("FALSE" = "#FFC20A", "TRUE" = "#0C7BDC"),
  breaks = c(FALSE, TRUE),
  labels = c("Drier than average (z ≤ 0)", "Wetter than average (z > 0)")
)

col_north <- scale_color_manual(
  values = c("FALSE" = "#E69F00", "TRUE" = "#0072B2"),
  breaks = c(FALSE, TRUE),
  labels = c("South-facing (northness ≤ 0)", "North-facing (northness > 0)")
)


# ── rdnbr and dnir × precip ───────────────────────────────────────────────────

p_rdnbr_precip <- ggplot(ard_west,
    aes(x = rdnbr, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(alpha = 0.12, size = 0.6, show.legend = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  col_precip +
  coord_cartesian(xlim = c(-634, 1300)) +
  labs(y = "Basal area loss", x = "RdNBR") +
  theme_bw()

p_dnir_precip <- ggplot(ard_west,
    aes(x = dnir, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(alpha = 0.12, size = 0.6, show.legend = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  col_precip +
  labs(y = "Basal area loss", x = "dNIR") +
  theme_bw()

combined_precip <-
  (p_rdnbr_precip + p_dnir_precip) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_blank())
combined_precip <- combined_precip +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 14),
        plot.tag.position = c(0.02, 0.98))
ggsave(paste0(fig_dir, "BA_rdnbr_dnir_precip.png"), combined_precip,
       width = 7, height = 4, units = "in", dpi = 300)


# ── rdnbr and dnir × northness ────────────────────────────────────────────────

p_rdnbr_north <- ggplot(ard_west,
    aes(x = rdnbr, y = pcnt_ba_mo, col = northness > 0)) +
  geom_point(alpha = 0.12, size = 0.6, show.legend = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  col_north +
  coord_cartesian(xlim = c(-634, 1300)) +
  labs(y = "Basal area loss", x = "RdNBR") +
  theme_bw()

p_dnir_north <- ggplot(ard_west,
    aes(x = dnir, y = pcnt_ba_mo, col = northness > 0)) +
  geom_point(alpha = 0.12, size = 0.6, show.legend = FALSE) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 6)) +
  col_north +
  labs(y = "Basal area loss", x = "dNIR") +
  theme_bw()

combined_north <-
  (p_rdnbr_north + p_dnir_north) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.title = element_blank())
combined_north <- combined_north +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 14),
        plot.tag.position = c(0.02, 0.98))
ggsave(paste0(fig_dir, "BA_rdnbr_dnir_northness.png"), combined_north,
       width = 7, height = 4, units = "in", dpi = 300)
