

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
harvey <- read.csv("VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Harvey_withcoordsba.csv")
agne <- read.csv("VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Agne_withcoordsba.csv")
andrus <- read.csv("VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Andrus_withcoordsba.csv")
miller <- read.csv("VP/severity_tmp/data/saved/JayMiller/JayMiller_withcoordsba_rev.csv")
band <- read.csv("VP/severity_tmp/data/saved/NPS/BAND_coords.csv")
eri <- read.csv("VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Roccaforte_withcoordsba.csv")

unique(hood$YrFireName)
names(nps)
names(ss)
names(mciver)
names(hood)
head(hood)
names(andrus)
names(eri)

# NPS formatting
nps$YrFireName <- paste0(nps$fireyear, " - NPS ", nps$park)
nps$Dataset <- "NPS"
nps$ID <- NA
nps_sub <- nps[, c("uniqueplot", "YrFireName", "Dataset", "park", "ID", "fireyear", "lat", "lon", "pcnt_ba_mort")]
nps_sub$pcnt_ba_mort <- nps_sub$pcnt_ba_mort/100

# Saba formatting
ss$YrFireName <- paste0(ss$FireYear, " - ", ss$Fire)
ss$Dataset <- "Saberi"
ss$ID <- NA
ss_sub <- ss[, c("PlotID", "YrFireName", "Dataset", "REBURN", "ID", "FireYear", "Lat", "Long", "prop_Kba")]

# McIver 
names(mciver)

hood$Fire <- NULL
andrus$Fire <- NULL
harvey$Fire <- NULL
names(hood)

agne$FireYear <- substr(agne$YrFireName, 1, 4)

names(nps_sub) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "FireYear", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort")
names(ss_sub) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "FireYear", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort")
# these are already set correctly
names(mciver) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort", "FireYear")
names(hood) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "lat_wgs84", "lon_wgs84", "FireYear", "pcnt_ba_mort")
names(harvey) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "lat_wgs84", "lon_wgs84", "FireYear", "pcnt_ba_mort")
names(andrus) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "lat_wgs84", "lon_wgs84", "FireYear", "pcnt_ba_mort")
names(agne) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort", "FireYear")
names(miller)
names(band)

allcoords <- rbind(nps_sub, ss_sub, mciver, hood, harvey, andrus, agne, miller, band, eri)

nrow(nps_sub) + nrow(ss_sub) + nrow(mciver) + nrow(hood) + nrow(harvey) + nrow(andrus) + nrow(agne) + nrow(miller) + nrow(band) + nrow(eri)
# fix Tenderfoot Fall plot IDs to remove duplicates
allcoords[allcoords$YrFireName=="2002 - Tenderfoot Fall",]$PlotID <- 
  paste0(allcoords[allcoords$YrFireName=="2002 - Tenderfoot Fall",]$ID, "_",
         allcoords[allcoords$YrFireName=="2002 - Tenderfoot Fall",]$Unit)
head(allcoords[allcoords$YrFireName=="2002 - Tenderfoot Fall",])

# set negative to 0
allcoords$pcnt_ba_mort <- ifelse(allcoords$pcnt_ba_mort < 0, 0, allcoords$pcnt_ba_mort)

# remove older fires
allcoords <- allcoords[allcoords$FireYear >= 1993,]

# only full data
allcoords <- allcoords[!is.na(allcoords$pcnt_ba_mort),]
allcoords <- allcoords[!is.na(allcoords$lat_wgs84),]




## New for v7 (moved from later scripts for cleanliness)

allcoords <- allcoords[allcoords$PlotID != "Wallow9_1_118",]

allcoords$UniqueID <- paste0(allcoords$Dataset, "_", allcoords$PlotID)

allcoords[allcoords$Dataset=="Hood",]$UniqueID <- 
  paste0(allcoords[allcoords$Dataset=="Hood",]$UniqueID, "_", substring(allcoords[allcoords$Dataset=="Hood",]$YrFireName, 8))
allcoords[allcoords$Dataset=="Davis",]$UniqueID <- 
  paste0(allcoords[allcoords$Dataset=="Davis",]$UniqueID, "_", substring(allcoords[allcoords$Dataset=="Davis",]$YrFireName, 8))

### Spatial part
# create shapefile
vcoords <- vect(allcoords, geom=c("lon_wgs84", "lat_wgs84"), crs="epsg:4326")
sfcoords <- st_as_sf(x = allcoords,                         
                     coords = c("lon_wgs84", "lat_wgs84"),
                     crs = "epsg:4326")

## Add image season start and end days as attributes

img_season <- vect("VP/severity_tmp/data/saved/image-seasons-resolve-ecoregions.gpkg")
img_season <- terra::project(img_season, crs(vcoords))
names(img_season)
img_season_simple <- img_season[, c("img_season_start", "img_season_end")]

# intersect the plots with the image season polygons to get Start_Day and End_Day 
days <- terra::extract(img_season_simple, vcoords)
names(days)
vcoords$Start_Day <- days$img_season_start
vcoords$End_Day <- days$img_season_end

write.csv(allcoords, "VP/severity_tmp/data/saved/allcoords_withbaloss_v7.csv", row.names = FALSE)
writeVector(vcoords, "VP/severity_tmp/data/saved/allcoords_withbaloss_v7.shp", overwrite = TRUE)


## for splitting the data into two parts
#vcoords
#length(vcoords)/2
#vcoords1 <- vcoords[1:1390,]
#vcoords2 <- vcoords[1391:2780,]
#writeVector(vcoords1, "VP/severity_tmp/data/saved/allcoords_withbaloss_v3_pt1.shp", overwrite = TRUE)
#writeVector(vcoords2, "VP/severity_tmp/data/saved/allcoords_withbaloss_v3_pt2.shp", overwrite = TRUE)


#allcoords <- read.csv("VP/severity_tmp/data/saved/allcoords_withbaloss.csv")
#allcoords[is.na(allcoords$pcnt_ba_mort),]



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
