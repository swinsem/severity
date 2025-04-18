names(ard_for_task)
ard2 <- merge(ard, ard_for_task[, c("UniqueID", "spatial_fold")], by="UniqueID")
library("rnaturalearth")
library("rnaturalearthdata")

states <- ne_states(country = "United States of America", returnclass = "sf")

bbox <- st_bbox(sfcoords)
bbox["xmin"] <- -124
bbox["ymin"] <- 32
bbox["xmax"] <- -105
bbox["ymax"] <- 49

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = ard2, aes(color = spatial_fold), size = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_light()

table(ard_for_task$spatial_fold)


#####
## table of start and end dates for each ecoregion

img_season <- vect("VP/severity_tmp/data/saved/image-seasons-resolve-ecoregions.shp")
img_season <- terra::project(img_season, crs(vcoords))
names(img_season)
img_season_simple <- img_season[, c("img_seaso0", "img_seaso1", "eco_name")]
names(img_season_simple) <- c("startDay", "endDay", "Ecoregion")
writeVector(img_season_simple, "VP/severity_tmp/data/saved/image-seasons-resolve-simple.shp")

img_df <- as.data.frame(img_season_simple)
names(img_df)
table(c(all_coords$Ecoregion, all_coords$Start_Day, all_coords$End_Day))
table(img_df)
head(img_df)


all_coords <- vect("VP/severity_tmp/data/saved/allcoords_withbaloss_v6.shp")
names(all_coords)
img_df <- as.data.frame(all_coords)
table(c(all_coords$Start_Day, all_coords$End_Day))
