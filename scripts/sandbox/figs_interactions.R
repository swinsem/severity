library(dplyr)
library(ggplot2)
library(rlang)

# load ARD
filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)
ard_west <- ard[ard$domain=="western-us",]

# Helper: SD-based landform bands for TPI, coarse aspect for northness, and precip bands
ard_west2 <- ard_west %>%
  mutate(
    TPI_z   = as.numeric(scale(meanTPI)),
    TPI_cat = cut(TPI_z,
                  breaks = c(-Inf, -1, -0.33, 0.33, 1, Inf),
                  labels = c("Valley", "Lower slope", "Mid-slope", "Upper slope", "Ridge"),
                  include.lowest = TRUE),
    north_cat = cut(northness,
                    breaks = quantile(northness, probs = c(0, .33, .66, 1), na.rm = TRUE),
                    labels = c("South", "Neutral", "North"),
                    include.lowest = TRUE),
    precip_cat = cut(zScorePrecip1,
                     breaks = c(-Inf, 0, Inf), #c(-Inf,-0.5,0,0.5,Inf),
                     labels = c("< 0", "> 0"), #c("< -0.5","-0.5–0","0–0.5","> 0.5"),
                     include.lowest = TRUE)
  )

# ------------------------------------------------------------------
# plot_conditional(): scatterplot + smooth + CI with colors
# ------------------------------------------------------------------
plot_conditional <- function(
    x, moderator,
    data,
    n_sample    = 100000,        # optional downsample for speed
    method      = c("gam","loess","lm"),
    k           = 6,              # basis size for GAM
    se_alpha    = 0.18,           # ribbon transparency
    line_width  = 1.0,            # line thickness
    point_alpha = 0.1,
    point_size  = 0.7,
    show_points = TRUE,
    palette     = NULL,           # named vector or will auto-generate
    legend_title = NULL,
    xlim = NULL,
    ylim = NULL,
    xlab = NULL
) {
  method <- match.arg(method)
  
  df <- data
  if (nrow(df) > n_sample) df <- dplyr::slice_sample(df, n = n_sample)
  
  # capture symbols
  x_sym  <- ensym(x)
  m_sym  <- ensym(moderator)
  x_lab  <- as_name(x_sym)
  leglab <- legend_title %||% as_name(m_sym)
  
  # ensure moderator is a factor to get stable legend order
  df <- df %>% mutate(`.mod` = factor(!!m_sym))
  
  # auto palette if none supplied
  if (is.null(palette)) {
    levs <- levels(df$.mod)
    cols <- scales::hue_pal()(length(levs))
    names(cols) <- levs
    palette <- cols
  }
  
  p <- ggplot(df, aes(x = !!x_sym, y = pcnt_ba_mo, colour = .mod, fill = .mod))
  if (show_points) {
    p <- p + geom_point(alpha = point_alpha, size = point_size)
  }
  
  # choose smoother
  if (method == "gam") {
    p <- p +
      geom_smooth(se = TRUE,  colour = NA, alpha = se_alpha,
                  method = "gam", formula = y ~ s(x, k = k)) +
      geom_smooth(se = FALSE, linewidth = line_width,
                  method = "gam", formula = y ~ s(x, k = k))
  } else if (method == "loess") {
    p <- p +
      geom_smooth(se = TRUE,  colour = NA, alpha = se_alpha, method = "loess") +
      geom_smooth(se = FALSE, linewidth = line_width,       method = "loess")
  } else { # lm
    p <- p +
      geom_smooth(se = TRUE,  colour = NA, alpha = se_alpha, method = "lm") +
      geom_smooth(se = FALSE, linewidth = line_width,       method = "lm")
  }
  
  p +
    scale_colour_manual(values = palette, name = leglab) +
    scale_fill_manual(values   = palette, name = leglab) +
    guides(colour = guide_legend(override.aes = list(fill = NA, alpha = 1))) +
    theme_light() +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(x = xlab %||% x_lab, y = "Basal area loss")
}


# Northness
pal_north <- c("South" = "#E69F00", "Neutral" = "#999999", "North" = "#0072B2")
plot_conditional(rdnbr, north_cat, data = ard_west2, palette = pal_north,
                 xlab = "RdNBR", xlim = c(-300, 1300))
plot_conditional(dnir, north_cat, data = ard_west2, palette = pal_north)

# Precip (supply 6-color palette if you want consistency across figures)
pal_precip <- c("< 0"="#D55E00",#"-0.5–0"="#E69F00","0–0.5"="#56B4E9",
                "> 0"="#0072B2")
plot_conditional(rdnbr, precip_cat, data = ard_west2, palette = pal_precip,
                 legend_title = "z-Precip (yr+1)", xlim = c(-300, 1300), xlab = "RdNBR")
plot_conditional(dnir, precip_cat, data = ard_west2, palette = pal_precip,
                 legend_title = "z-Precip (yr+1)")


# ── Combined: RdNBR × northness and RdNBR × precip ────────────────────────────
library(patchwork)

p_north  <- plot_conditional(rdnbr, north_cat, data = ard_west2, palette = pal_north,
                              legend_title = "Northness", xlab = "RdNBR", xlim = c(-300, 1300),
                              ylim = c(-0.1, 1.1))
p_precip <- plot_conditional(rdnbr, precip_cat, data = ard_west2, palette = pal_precip,
                              legend_title = "zPrecip1", xlab = "RdNBR",
                              xlim = c(-300, 1300), ylim = c(-0.1, 1.1))

tag_bottom <- theme(plot.tag = element_text(face = "bold", size = 12),
                    legend.position = "bottom",
                    legend.justification = "center")

p_north2  <- p_north  + tag_bottom
p_precip2 <- p_precip + tag_bottom +
  guides(colour = guide_legend(ncol = 2, byrow = FALSE,
                               override.aes = list(fill = NA, alpha = 1))) #+
  # theme(legend.key.height = unit(0.7, "lines"),
  #       legend.key.width  = unit(1.8, "lines"))

combined_rdnbr <- (p_precip2 + p_north2) +
  plot_annotation(tag_levels = "a")
combined_rdnbr

ggsave("figs/rdnbr_northness_precip_combined.png", combined_rdnbr,
       width = 8, height = 5, units = "in", dpi = 300)


# ── Reviewer: where is the RdNBR × precip relationship strongest? ─────────────
# Facet the wet/dry RdNBR smooth by ecoregion; restrict to ecoregions with
# enough plots to support a GAM (n >= 60).

eco_keep <- ard_west |>
  count(ecoregion) |>
  filter(n >= 60) |>
  pull(ecoregion)

p_precip_eco <- ard_west |>
  filter(ecoregion %in% eco_keep) |>
  ggplot(aes(x = rdnbr, y = pcnt_ba_mo, colour = zScorePrecip1 > 0)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4),
              se = FALSE, linewidth = 0.9) +
  coord_cartesian(xlim = c(-300, 1300), ylim = c(0, 1)) +
  facet_wrap(~ ecoregion, labeller = label_wrap_gen(22)) +
  scale_colour_manual(
    values = c("FALSE" = "#D55E00", "TRUE" = "#0072B2"),
    breaks = c(FALSE, TRUE),
    labels = c("Drier (z ≤ 0)", "Wetter (z > 0)")
  ) +
  labs(x = "RdNBR", y = "Basal area loss", colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 7))
p_precip_eco
ggsave("figs/rdnbr_precip_by_ecoregion.png", p_precip_eco,
       width = 10, height = 8, dpi = 300)


# ── Background climate as biological gradient within ecoregions ───────────────
# Ecoregional models in Arizona (meanPrecip), Cascades (meanCWD), and
# South Central Rockies (meanVPD) selected long-term mean climate variables,
# suggesting these capture within-ecoregion productivity/vegetation gradients
# rather than year-to-year variation. Show both meanCWD and meanPrecip vs.
# BA loss to demonstrate whether background aridity indexes fire mortality.

p_cwd_bio <- ard_west |>
  filter(ecoregion %in% eco_keep) |>
  ggplot(aes(x = meanCWD, y = pcnt_ba_mo)) +
  geom_point(alpha = 0.12, size = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4),
              colour = "#0072B2", se = TRUE) +
  facet_wrap(~ ecoregion, labeller = label_wrap_gen(22), scales = "free_x") +
  labs(x = "Mean climatic water deficit (mm/yr)", y = "Basal area loss") +
  theme_bw() +
  theme(strip.text = element_text(size = 7))
ggsave("figs/mean_cwd_baloss_ecoregion.png", p_cwd_bio,
       width = 10, height = 8, dpi = 300)

p_precip_bio <- ard_west |>
  filter(ecoregion %in% eco_keep) |>
  ggplot(aes(x = meanPrecip, y = pcnt_ba_mo)) +
  geom_point(alpha = 0.12, size = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4),
              colour = "#D55E00", se = TRUE) +
  facet_wrap(~ ecoregion, labeller = label_wrap_gen(22), scales = "free_x") +
  labs(x = "Mean annual precipitation (mm)", y = "Basal area loss") +
  theme_bw() +
  theme(strip.text = element_text(size = 7))
ggsave("figs/mean_precip_baloss_ecoregion.png", p_precip_bio,
       width = 10, height = 8, dpi = 300)

p_vpd_bio <- ard_west |>
  filter(ecoregion %in% eco_keep) |>
  ggplot(aes(x = meanVPD, y = pcnt_ba_mo)) +
  geom_point(alpha = 0.12, size = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4),
              colour = "#D55E00", se = TRUE) +
  facet_wrap(~ ecoregion, labeller = label_wrap_gen(22), scales = "free_x") +
  labs(x = "Mean summer VPD", y = "Basal area loss") +
  theme_bw() +
  theme(strip.text = element_text(size = 7))
# ── Fire-year drought effects in ecoregions with year-of-fire climate vars ────
# Arizona Mountains (zCWD0, zVPD0), Central-Southern Cascades (zPrecip0),
# Colorado Rockies (zCWD0, zVPD0), Northern Rockies (zCWD0).
# Two competing hypotheses:
#   1. Physiological drought stress: higher drought → higher BA loss across full
#      severity range (parallel shift in curves).
#   2. Fire behavior: drought modifies fire intensity → curves diverge mainly at
#      intermediate RdNBR where fire behavior governs transition to high severity.

eco_fire_year <- c("Arizona Mountains forests",
                   "Central-Southern Cascades Forests",
                   "Colorado Rockies forests",
                   "Northern Rockies conifer forests")

p_cwd0_eco <- ard_west |>
  filter(ecoregion %in% eco_fire_year) |>
  ggplot(aes(x = rdnbr, y = pcnt_ba_mo, colour = zScoreCWD0 > 0)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4),
              se = FALSE, linewidth = 0.9) +
  coord_cartesian(xlim = c(-300, 1300), ylim = c(0, 1)) +
  facet_wrap(~ ecoregion, labeller = label_wrap_gen(22)) +
  scale_colour_manual(
    values = c("FALSE" = "#0072B2", "TRUE" = "#D55E00"),
    breaks = c(FALSE, TRUE),
    labels = c("Lower than avg CWD (z ≤ 0)", "Higher than avg CWD (z > 0)")
  ) +
  labs(x = "RdNBR", y = "Basal area loss", colour = "Fire-year CWD") +
  theme_bw() +
  theme(legend.position = "bottom", strip.text = element_text(size = 7))
ggsave("figs/rdnbr_cwd0_fireyr_ecoregion.png", p_cwd0_eco,
       width = 8, height = 6, dpi = 300)

p_vpd0_eco <- ard_west |>
  filter(ecoregion %in% eco_fire_year) |>
  ggplot(aes(x = rdnbr, y = pcnt_ba_mo, colour = zScoreVPD0 > 0)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4),
              se = FALSE, linewidth = 0.9) +
  coord_cartesian(xlim = c(-300, 1300), ylim = c(0, 1)) +
  facet_wrap(~ ecoregion, labeller = label_wrap_gen(22)) +
  scale_colour_manual(
    values = c("FALSE" = "#0072B2", "TRUE" = "#D55E00"),
    breaks = c(FALSE, TRUE),
    labels = c("Lower than avg VPD (z ≤ 0)", "Higher than avg VPD (z > 0)")
  ) +
  labs(x = "RdNBR", y = "Basal area loss", colour = "Fire-year VPD") +
  theme_bw() +
  theme(legend.position = "bottom", strip.text = element_text(size = 7))
ggsave("figs/rdnbr_vpd0_fireyr_ecoregion.png", p_vpd0_eco,
       width = 8, height = 6, dpi = 300)

# Central-Southern Cascades used zPrecip0 (inverse: lower = drier fire year)
p_precip0_casc <- ard_west |>
  filter(ecoregion == "Central-Southern Cascades Forests") |>
  ggplot(aes(x = rdnbr, y = pcnt_ba_mo, colour = zScorePrecip0 > 0)) +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4),
              se = FALSE, linewidth = 0.9) +
  coord_cartesian(xlim = c(-300, 1300), ylim = c(0, 1)) +
  scale_colour_manual(
    values = c("FALSE" = "#D55E00", "TRUE" = "#0072B2"),
    breaks = c(FALSE, TRUE),
    labels = c("Drier fire year (z ≤ 0)", "Wetter fire year (z > 0)")
  ) +
  labs(x = "RdNBR", y = "Basal area loss",
       colour = "Fire-year precip\n(Cascades)",
       title = "Central-Southern Cascades Forests") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("figs/rdnbr_precip0_fireyr_cascades.png", p_precip0_casc,
       width = 5, height = 4, dpi = 300)



# fires per ecoregion                                                                         
ard_west |> group_by(ecoregion) |> summarize(n_fires = n_distinct(Fire))

# fires per year per ecoregion
fires_peryear <- ard_west |> group_by(ecoregion, FireYear) |> summarize(n_fires = n_distinct(Fire), .groups = "drop")

