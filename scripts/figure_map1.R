library(terra)
library(dplyr)
library(ggplot2)

## plot
library("rnaturalearth")
library("rnaturalearthdata")


ardcoords <- vect("VP/severity_tmp/data/saved/ARD_08262024.gpkg")

## plot ecoregions
library(sf)
library(scales)
sfcoords <- sf::st_as_sf(x = ardcoords,                         
                         coords = c("lon_wgs84", "lat_wgs84"),
                         crs = "epsg:4326")
ecoregions_sf <- sf::st_as_sf(ecoregions)
bbox <- st_bbox(sfcoords)

bbox["xmin"] <- -124
bbox["ymin"] <- 32
bbox["xmax"] <- -105
bbox["ymax"] <- 49
# Add a filter for the 15 ecoregions that are in r2_table to color them differently
selected_ecoregions <- r2_table$ECO_NAME



# Modify ecoregions_sf to have a new column that indicates whether it should be colored
ecoregions_sf$color_flag <- ifelse(ecoregions_sf$ECO_NAME %in% selected_ecoregions, ecoregions_sf$ECO_NAME, "Other")
# Create a color palette with enough colors
unique_ecoregions <- unique(ecoregions_sf$color_flag)
colors_for_ecoregions <- hue_pal()(length(unique_ecoregions) - 1)  # Generate colors for all but "Other"
color_palette <- setNames(c(colors_for_ecoregions, "grey90"), c(setdiff(unique_ecoregions, "Other"), "Other"))

states <- ne_states(country = "United States of America", returnclass = "sf")

# caleco_colors <- calecopal::cal_palette(name = "dudleya", n = 15, type = "continuous")
# unique_ecoregions <- unique(ecoregions_sf$color_flag)
# color_palette <- setNames(c(caleco_colors, "grey70"), c(setdiff(unique_ecoregions, "Other"), "Other"))

#calecopal::cal_palette(name = "dudleya", n = 15, type = "continuous")
ggplot() +
  geom_sf(data = ecoregions_sf, aes(fill=color_flag)) +
  geom_sf(data = states, fill = NA) +
  geom_sf(data = sfcoords, color = "black", size = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_light() +
  scale_fill_manual(values = color_palette,
                    name = "Ecoregions") +
  # Add a title to the legend
  labs(fill = "Ecoregions")
ggsave("VP/severity_tmp/plots/plot_map_ecoregion.png", width=7, height = 5, units = "in")



