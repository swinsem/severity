

#############################################################
#################### COMBINE COORD DATA ####################  
############################################################

library(terra)
library(sf)
library(ggplot2)


nps <- read.csv("VP/severity_tmp/data/saved/NPS/nps_baloss_coords.csv")
ss <- read.csv("VP/severity_tmp/data/saved/Saba/IPNW_Plot_Severity_coords.csv")
mciver <- read.csv("VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_McIver_withcoordsba.csv")
hood <- read.csv("VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Hood_withcoordsba.csv")
unique(hood$YrFireName)
names(nps)
names(ss)
names(mciver)
names(hood)
head(hood)

# NPS formatting
nps$YrFireName <- paste0(nps$fireyear, " - NPS")
nps$Dataset <- "NPS"
nps$ID <- NA
nps_sub <- nps[, c("uniqueplot", "YrFireName", "Dataset", "park", "ID", "fireyear", "lat", "lon", "pcnt_ba_mort")]

# Saba formatting
ss$YrFireName <- paste0(ss$FireYear, " - ", ss$Fire)
ss$Dataset <- "Saberi"
ss$ID <- NA
ss_sub <- ss[, c("PlotID", "YrFireName", "Dataset", "REBURN", "ID", "FireYear", "Lat", "Long", "prop_Kba")]

# McIver 
names(mciver)

hood$Fire <- NULL
names(hood)

names(nps_sub) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "FireYear", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort")
names(ss_sub) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "FireYear", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort")
# these are already set correctly
names(mciver) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort", "FireYear")
names(hood) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "lat_wgs84", "lon_wgs84", "FireYear", "pcnt_ba_mort")


allcoords <- rbind(nps_sub, ss_sub, mciver, hood)

vcoords <- vect(allcoords, geom=c("lon_wgs84", "lat_wgs84"), crs="epsg:4326")
sfcoords <- st_as_sf(x = allcoords,                         
                     coords = c("lon_wgs84", "lat_wgs84"),
                     crs = "epsg:4326")

## Add image season start and end days as attributes
image_season_path <- aws.s3::save_object(
  object = "s3://vp-sci-grp/fire-severity/processed/image-season-vectors/image-season_v1.0.gpkg",
  file = file.path(tempdir(), 'image-season_v1.0.gpkg')
)
img_season <- sf::st_read(image_season_path)
img_season <- vect(img_season)
img_season <- terra::project(img_season, crs(vcoords))

# intersect the plots with the image season polygons to get Start_Day and End_Day 
days <- terra::extract(img_season, vcoords)
vcoords$Start_Day <- days$Start_Day
vcoords$End_Day <- days$End_Day

vcoords <- vcoords[vcoords$FireYear >= 1993,]
allcoords <- allcoords[allcoords$FireYear >= 1993,]

vcoords <- vcoords[!is.na(vcoords$pcnt_ba_mort),]
allcoords <- allcoords[!is.na(allcoords$pcnt_ba_mort),]
allcoords <- allcoords[!is.na(allcoords$lat_wgs84),]

write.csv(allcoords, "VP/severity_tmp/data/saved/allcoords_withbaloss.csv", row.names = FALSE)
writeVector(vcoords, "VP/severity_tmp/data/saved/allcoords_withbaloss.shp", overwrite = TRUE)

allcoords <- read.csv("VP/severity_tmp/data/saved/allcoords_withbaloss.csv")
allcoords[is.na(allcoords$pcnt_ba_mort),]



## plot
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
  geom_sf(data = sfcoords, color = "black", size = 1) +
  #geom_sf(data = sferi, color = "red", size = 1) + ## made at bottom of 01_ftm_data_requests.R

  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_light()
ggsave("VP/severity_tmp/plots/plot_map.png")
names(andrus_fire)
andrus_fire <- ftmfire[ftmfire$Dataset=="Andrus",]
andruscoords <- vect(andrus_fire, geom=c("Longitude", "Latitude"), crs="epsg:4326")
sfcoords <- st_as_sf(x = andruscoords,                         
                     coords = c("Longitude", "Latitude"),
                     crs = "epsg:4326")


ggplot() +
  geom_sf(data = states) +
  geom_sf(data = sfcoords, color = "black", size = 1) +
  #geom_sf(data = sferi, color = "red", size = 1) + ## made at bottom of 01_ftm_data_requests.R
  
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_light()
