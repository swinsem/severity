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
                    labels = c("Southish", "Neutral", "Northish"),
                    include.lowest = TRUE),
    precip_cat = cut(zScorePrecip1,
                     breaks = c(-Inf,-1,-0.5,0,0.5,1,Inf),
                     labels = c("≤-1","-1–-0.5","-0.5–0","0–0.5","0.5–1",">1"),
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
    legend_title = NULL
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
    labs(x = x_lab, y = "Basal area loss")
}


### Interactions for dSWIR2SWIR1 and dNDVI with TPI, northness, and precip

# Okabe–Ito palette for 5-level TPI (example, named to levels for reproducibility)
pal_tpi <- c("Valley"="#0072B2","Lower slope"="#009E73","Mid-slope"="#F0E442",
             "Upper slope"="#D55E00","Ridge"="#CC79A7")

plot_conditional(post_swir2swir1, TPI_cat,  data = ard_west2, palette = pal_tpi)
plot_conditional(dndvi,            TPI_cat,  data = ard_west2, palette = pal_tpi)

# Northness (auto palette)
plot_conditional(post_swir2swir1, north_cat, data = ard_west2)
plot_conditional(dndvi, north_cat, data = ard_west2)

# Precip (supply 6-color palette if you want consistency across figures)
pal_precip <- c("≤-1"="#56B4E9","-1–-0.5"="#009E73","-0.5–0"="#F0E442",
                "0–0.5"="#E69F00","0.5–1"="#D55E00",">1"="#CC79A7")
plot_conditional(dndvi, precip_cat, data = ard_west2, palette = pal_precip,
                 legend_title = "z-Precip (yr+1)")
plot_conditional(post_swir2swir1, precip_cat, data = ard_west2, palette = pal_precip,
                 legend_title = "z-Precip (yr+1)")



# ------------------------------------------------
# surface_gam(): 2D GAM surface for var1 × var2, holding others at medians
# ------------------------------------------------
### I don't really understand these yet!

library(mgcv)

# Utility to draw a filled contour for var1 × var2, holding others at medians
surface_gam <- function(var1, var2, data = ard_west, kgrid = 60) {
  vars <- c("post_swir2swir1","dndvi","zScorePrecip1","meanTPI","dred","northness")
  stopifnot(all(c(var1, var2) %in% vars))
  
  # Build model: 2D smooth for the pair + 1D smooths for the rest
  others <- setdiff(vars, c(var1, var2))
  form <- as.formula(
    paste0("pcnt_ba_mo ~ s(", var1, ", ", var2, ", k=30) + ",
           paste(sprintf("s(%s, k=6)", others), collapse = " + "))
  )
  
  dat <- data %>% select(all_of(c("pcnt_ba_mo", vars))) %>% na.omit()
  m <- gam(form, data = dat, method = "REML", family = betar())
  
  # grid over central range (trim extremes to avoid edge artifacts)
  seq_range <- function(x, n = kgrid, trim = 0.01) {
    r <- quantile(x, c(trim, 1 - trim), na.rm = TRUE); seq(r[1], r[2], length.out = n)
  }
  g1 <- seq_range(dat[[var1]]); g2 <- seq_range(dat[[var2]])
  hold <- lapply(others, function(v) median(dat[[v]], na.rm = TRUE)) |> setNames(others)
  
  newdat <- expand.grid(
    x1 = g1, x2 = g2, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  names(newdat) <- c(var1, var2)
  newdat[names(hold)] <- hold
  newdat$fit <- predict(m, newdat, type = "response")
  
  ggplot(newdat, aes_string(var1, var2, z = "fit")) +
    geom_contour_filled() +
    labs(fill = "Pred. BA loss",
         title = paste(var1, "×", var2, "(GAM surface)")) +
    theme_bw()
}

# A few high-value pairs to try
surface_gam("post_swir2swir1", "zScorePrecip1")
surface_gam("dndvi",           "zScorePrecip1")
surface_gam("post_swir2swir1", "meanTPI")
surface_gam("dndvi",           "meanTPI")
surface_gam("post_swir2swir1", "northness")
surface_gam("dndvi",           "northness") # ?? this is the one marked "use" for 2nd order ALE


ard_west3 <- ard_west %>%
  mutate(
    TPI_cut = cut(meanTPI,
                  breaks = c(-Inf, quantile(meanTPI, .15, na.rm=TRUE), -0.05, 0.05,
                             quantile(meanTPI, .85, na.rm=TRUE), Inf),
                  labels = c("Very low","Low","Near 0","High","Very high"),
                  include.lowest = TRUE)
  )

ggplot(ard_west3, aes(dndvi, pcnt_ba_mo, colour = TPI_cut)) +
  geom_point(alpha = 0.07) +
  geom_smooth(se = TRUE, method = "gam", formula = y ~ s(x, k = 6)) +
  theme_light() +
  labs(colour = "TPI (ALE-guided)")
