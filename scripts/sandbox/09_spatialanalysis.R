library(terra)
library(dplyr)
library(ggplot2)

# Merge with original data
ardcoords <- vect("VP/severity_tmp/data/saved/ARD_01212025.gpkg")
full_model_results <- read.csv("VP/severity_tmp/data/saved/outputs/ranger_cv_results.csv")

ard <- read.csv("VP/severity_tmp/data/saved/ARD_nocoords.csv")
fires <- ard %>%
  group_by(Dataset, YrFireName) %>%
  summarize()

names(ardcoords)
#names(full_model_results)[1] <- "UniqueID"
names(full_model_results)
# make model results into a spatvector
rfcoords <- merge(ardcoords[, c("UniqueID", "ecoregion")], full_model_results, by = "UniqueID")


## extract ecoregion
#ecoregions <- vect("VP/severity_tmp/data/Ecoregions2017/Ecoregions2017.shp")
#ecoregions <- ecoregions[ecoregions$REALM=="Nearctic",]
#ecoregions


#biome_info <- terra::extract(ecoregions[, "ECO_NAME"], rfcoords)

# Make new ardcoords file with ecoregion
#neward <- terra::extract(ecoregions[, "ECO_NAME"], ardcoords)
#ardcoords$ecoregion <- neward$ECO_NAME
#ard2 <- as.data.frame(ardcoords)
#writeVector(ardcoords, "VP/severity_tmp/data/saved/ARD_01212025.gpkg") # ARD with ecoregion
#head(biome_info)

#rfcoords <- cbind(rfcoords, biome_info)

table(rfcoords$ecoregion)
## replace some
rfcoords$ecoregion <- ifelse(rfcoords$ecoregion=="California interior chaparral and woodlands", "Klamath-Siskiyou forests", rfcoords$ecoregion)
rfcoords$ecoregion <- ifelse(rfcoords$ecoregion=="Great Basin shrub steppe", "Sierra Nevada forests", rfcoords$ecoregion)


# Calculate R² for each ECO_NAME
r2_table <- as.data.frame(rfcoords) %>%
  group_by(ecoregion) %>%
  summarise(R2 = caret::R2(obs, pred), n_rows = n())
r2_table

rfcoords$ecoregion <- ifelse(rfcoords$ecoregion=="Colorado Plateau shrublands", "Wasatch and Uinta montane forests", rfcoords$ecoregion)
rfeco <- as.data.frame(rfcoords)
ggplot(rfeco) +
  geom_point(aes(x=obs, y=pred), alpha=.6) +
  geom_abline() +
  geom_smooth(aes(x=obs, y=pred), method = "loess") +
  theme_bw() +
  ylim(0, 1) +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss") +
  facet_wrap(~ ECO_NAME, scales = "free", 
             labeller = label_wrap_gen(width = 20))
ggsave("VP/severity_tmp/plots/rf_pred_by_ecoregion2.png", width = 8, height = 9, units = "in")

library(plotly)
# interactive plotly plot
p <- ggplot(r2_table) +
  geom_point(aes(x = n_rows, y = R2, text = ECO_NAME)) +  # Add 'text' aesthetic for interactivity?
  labs(y = "R-squared (R2)", x = "Number of Rows (n_rows)")  
interactive_plot <- ggplotly(p, tooltip = "text")  # Set 'tooltip' to display the 'text' aesthetic
interactive_plot

cor(r2_table$R2[r2_table$ECO_NAME!="California interior chaparral and woodlands"], r2_table$n_rows[r2_table$ECO_NAME!="California interior chaparral and woodlands"])




### MOVED BELOW TO figure_map1.R

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
color_palette <- setNames(c(colors_for_ecoregions, "grey70"), c(setdiff(unique_ecoregions, "Other"), "Other"))

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
ggsave("VP/severity_tmp/plots/plot_map_ecoregion.png")

# colorado plateau shrublands AND wasatch and uinta montane forests

## Extract state data
## plot
library("rnaturalearth")
library("rnaturalearthdata")

states <- ne_states(country = "United States of America", returnclass = "sf")
states <- states[,c("region_sub", "woe_name")]
statesvect <- vect(states)

state_info <- terra::extract(statesvect, rfcoords)

head(state_info)
state_info$id.y <- NULL
rfcoords <- cbind(rfcoords, state_info)
rfstate <- as.data.frame(rfcoords)

r2_table_state <- rfstate %>%
  group_by(woe_name) %>%
  summarise(R2 = r_squared(obs, pred), n_rows = n())
r2_table_state

ggplot(rfstate) +
  geom_point(aes(x=obs, y=pred), alpha=.6) +
  geom_abline() +
  geom_smooth(aes(x=obs, y=pred)) +
  theme_light() +
  ylim(0, 1) +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss") +
  facet_wrap(~woe_name)
ggsave("VP/severity_tmp/plots/rf_pred_by_state.png", width = 8, height = 8, units = "in")


rfstate[rfstate$woe_name=="Utah",]$UniqueID
