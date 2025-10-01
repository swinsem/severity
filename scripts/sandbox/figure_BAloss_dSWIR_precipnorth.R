library(ggplot2)

#### read ard with spatial folds from github severity/data folder
filename <- "20250804"

ard_with_spatial_folds_fname <- here::here(
  glue::glue("data/ARD_{filename}_with-spatial-folds.csv")
)

ard <- readr::read_csv(
  ard_with_spatial_folds_fname, 
  col_types = list(spatial_fold = "factor")
)

# filter to full western-us model only
ard_west <- ard[ard$domain=="western-us",]


# tpi as x and ba loss y, color is north/south
# dndvi?


ard_west |> 
  dplyr::mutate(zscoreprecip1_fct = zScorePrecip1 > 0) |> 
  ggplot(aes(x = post_swir2swir1, y = pcnt_ba_mo)) +
  geom_point() +
  facet_wrap(facets = "zscoreprecip1_fct") +
  geom_smooth()

ggplot(ard_west, aes(x = post_swir2swir1, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point() +
  geom_smooth() +
  theme_light() 
ggplot(ard_west, aes(x = dndvi, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point() +
  geom_smooth() +
  theme_light() 


ggplot(ard_west, aes(x = post_swir2swir1, y = pcnt_ba_mo)) +
  geom_point() +
  geom_smooth() +
  theme_light() 


# version with overall line
ggplot(ard, aes(x = post_swir2swir1, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(aes(alpha=0.25), show_guide = FALSE) +
  # Smoothing split by zScorePrecip1 > 0
  geom_smooth() +
  # Smoothing over the entire dataset (ignores color grouping)
  geom_smooth(
    inherit.aes = FALSE,
    data = ard,
    aes(x = post_swir2swir1, y = pcnt_ba_mo),
    method = "gam",
    color = "black"
  ) +
  scale_color_manual(
    name   = "Z-score precipitation",
    values = c("TRUE" = "#0C7BDC", "FALSE" = "#FFC20A"),
    labels = c("Negative", "Positive")
  ) +
  labs(y = "Basal area loss (%)", x = "dSWIR2:SWIR1") +
  theme_minimal() +
  theme(legend.position="bottom", legend.direction="vertical")

ggplot(ard, aes(x = post_swir2swir1, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(aes(alpha=0.25), show_guide = FALSE) +
  geom_smooth() +
  scale_color_manual(
    name = "Z-score precipitation",          # Legend title
    values = c("TRUE" = "#0C7BDC", "FALSE" = "#FFC20A"),  # Colors for Positive and Negative
    labels = c("Negative", "Positive")       # Custom labels
  ) +
  labs(y="Basal area loss (%)", x="post-fire SWIR2/SWIR1") +
  theme_minimal() +
  theme(legend.position="bottom", legend.direction="vertical")
ggsave("VP/severity_tmp/plots/BA_pSWIR_precip2.png", width = 3, height = 4, units = "in")


ggplot(ard_west, aes(x = post_swir2swir1, y = pcnt_ba_mo, col = northness > 0)) +
  geom_point(aes(alpha=0.25), show_guide = FALSE) +
  geom_smooth() +
  scale_color_manual(
    name = "Northness",          # Legend title
    values = c("TRUE" = "#0C7BDC", "FALSE" = "#FFC20A"),  # Colors for Positive and Negative
    labels = c("South-facing", "North-facing") 
  ) +
  labs(y="Basal area loss (%)", x="dSWIR2:SWIR1") +
  theme_minimal() +
  theme(legend.position="bottom", legend.direction="vertical")
ggsave("VP/severity_tmp/plots/BA_dSWIR_northness2.png", width = 3, height = 4, units = "in")



### COMBINED - shared legend
library(ggplot2)
library(patchwork)

col_scale <- scale_color_manual(
  name   = "Z-score precipitation",
  values = c("FALSE" = "#FFC20A", "TRUE" = "#0C7BDC"),
  breaks = c(FALSE, TRUE),
  labels = c("Drier than average", "Wetter than average")
)

p1 <- ggplot(ard, aes(post_swir2swir1, pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(alpha = 0.25, show.legend = FALSE) + geom_smooth() +
  col_scale + labs(y = "Basal area loss", x = "post-fire SWIR2/SWIR1") + theme_bw()

p2 <- ggplot(ard, aes(dndvi, pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(alpha = 0.25, show.legend = FALSE) + geom_smooth() +
  col_scale + labs(y = "Basal area loss", x = "dNDVI") + theme_bw()

combined_plot <-
  (p1 + p2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Put 'a' and 'b' at the TOP-LEFT of each subplot
combined_plot +
  plot_annotation(tag_levels = "a") &
  theme(
    plot.tag = element_text(face = "bold", size = 15),
    plot.tag.position = c(0.02, 0.98),   # x ~ left, y ~ near top
    plot.tag.margin = margin(8, 0, 0, 8) # nudge in from edges
  )

# Save Combined Plot
ggsave("figs/BA_dSWIR_combined.png", 
       width = 6, height = 4, units = "in", dpi = 300)



### COMBINED - two legends (useful eg when I had northness)

# First Plot
p1 <- ggplot(ard, aes(x = post_swir2swir1, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(aes(alpha=0.25), show_guide = FALSE) +
  geom_smooth() +
  scale_color_manual(
    name = "Z-score precipitation",
    values = c("TRUE" = "#0C7BDC", "FALSE" = "#FFC20A"),
    labels = c("Drier than average","Wetter than average")
  ) +
  labs(y = "Basal area loss", x = "post-fire SWIR2/SWIR1") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title=element_blank(), legend.direction = "vertical") #, legend.direction = "vertical"
p1
# Second Plot
p2 <- ggplot(ard, aes(x = dndvi, y = pcnt_ba_mo, col = zScorePrecip1 > 0)) +
  geom_point(aes(alpha=0.25), show_guide = FALSE) +
  geom_smooth() +
  scale_color_manual(
    name = "Z-score precipitation",
    values = c("TRUE" = "#0C7BDC", "FALSE" = "#FFC20A"),
    labels = c("Drier than average","Wetter than average")
  ) +
  labs(y = "Basal area loss", x = "dNDVI") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title=element_blank(), legend.direction = "vertical") #, legend.direction = "vertical"

# Combine Plots with Labels "a" and "b" at Bottom Left
combined_plot <- cowplot::plot_grid(
  p1, p2,
  labels = NULL,  # We'll add custom annotations
  ncol = 2,
  rel_widths = c(1, 1)
)

# Add Labels "a" and "b" Manually
combined_plot <- combined_plot +
  cowplot::draw_plot_label(
    label = c("a", "b"),
    x = c(0.05, .55),       # Adjust x positions as needed
    y = c(0.13, .13),         # y=0 places the labels at the bottom
    hjust = 0,           # Left-justify
    vjust = 0,           # Bottom-justify
    size = 15,
    fontface = "bold"
  )
combined_plot
# Save Combined Plot
ggsave("VP/severity_tmp/plots/BA_dSWIR_combined3.png", 
       combined_plot, width = 6, height = 4, units = "in", dpi = 300)




# what is this for??

ggplot(ard, aes(x = zScorePrecip1, y = pcnt_ba_mo)) +
  geom_point(aes(alpha=0.25), show_guide = FALSE) +
  geom_smooth() 
library(mgcv)
m1 <- mgcv::gam(pcnt_ba_mo ~ s(zScorePrecip1) + s(northness) + s(dswir2swir1), data=ard)
m1
plot(m1)

m2 <- mgcv::gam(pcnt_ba_mo ~ s(zScorePrecip1, dswir2swir1, northness), data=ard)
plot(m2)

ard2 <- as.data.frame(merge(rfcoords[, c("UniqueID", "ECO_NAME")], ard, by="UniqueID"))
names(ard2)

m3 <- mgcv::gam(pcnt_ba_mo ~ as.factor(ECO_NAME) + s(zScorePrecip1, by=as.factor(ECO_NAME) ) + s(northness) + s(dswir2swir1), data=ard2)
m3
plot(m3, ylim=c(-1, 1))


precip_mort_plot_data = ard |> 
  dplyr::group_by(zScorePrecip1 > 0) |> 
  dplyr::summarize(pcnt_ba_mo = mean(pcnt_ba_mo))

plot(precip_mort_plot_data[, "zScorePrecip1 > 0"])
