
df_vcoords <- as.data.frame(vcoords)

unique_days <- df_vcoords %>%
  group_by(YrFireName) %>%
  summarize(
    NumUniqueStartDays = n_distinct(Start_Day),
    NumUniqueEndDays = n_distinct(End_Day)
  ) %>%
  filter(NumUniqueStartDays > 1 | NumUniqueEndDays > 1)

sfcoords2 <- st_as_sf(x = vcoords,                         
                     coords = c("lon_wgs84", "lat_wgs84"),
                     crs = "epsg:4326")

library("rnaturalearth")
library("rnaturalearthdata")

img_season_sf <- st_as_sf(img_season_simple)

states <- ne_states(country = "United States of America", returnclass = "sf")

bbox <- st_bbox(sfcoords2)
bbox["xmin"] <- -124
bbox["ymin"] <- 32
bbox["xmax"] <- -105
bbox["ymax"] <- 49

ggplot() +
  geom_sf(data = states) +
  geom_sf(data = img_season_sf$img_season_start) +
  geom_sf(data = sfcoords2[sfcoords2$YrFireName %in% unique_days$YrFireName,], color = "black", size = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_light()
ggsave("VP/severity_tmp/plots/plot_map.png")

hist(allcoords$pcnt_ba_mort)

# check for one fire
names(andrus_fire)
andrus_fire <- ftmfire[ftmfire$Dataset=="Andrus",]
andruscoords <- vect(andrus_fire, geom=c("Longitude", "Latitude"), crs="epsg:4326")
sfcoords <- st_as_sf(x = andruscoords,                         
                     coords = c("Longitude", "Latitude"),
                     crs = "epsg:4326")
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = sfcoords, color = "black", size = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_light()



## convex hulls around groups of plots


vcoords <- vect("VP/severity_tmp/data/saved/allcoords_withbaloss_v3.shp")
vcoords

vcbuff <- buffer(vcoords, 45)

vcbuff$GroupID <- paste0(vcbuff$YrFireName, "_", vcbuff$Start_Day)
length(unique(vcbuff$GroupID))


fires <- convHull(vcbuff, by = "GroupID")
fires
firesdf <- as.data.frame(fires)

plot(fires)
plot(fires[10])

# Assuming `vcbuff` has multiple entries for some `YrFireName` and you want the earliest Start_Day
vcbuff_unique <- unique(as.data.frame(vcbuff[, c("GroupID", "YrFireName", "Dataset", "FireYear", "Start_Day", "End_Day")]))

# Now perform the merge
fires <- merge(fires, vcbuff_unique, by="GroupID")

head(fires)


writeVector(fires, "VP/severity_tmp/data/saved/fires_convexhulls1.shp", overwrite=TRUE)
