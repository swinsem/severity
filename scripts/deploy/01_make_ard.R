library(ggplot2)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(terra)
library(glue)

data_dir <- "research/severity/data/"

# use the same name at the end of spectral_, climate_, and topo_ in GEE
filename <- "05282025"

### to do:
## remove NPS plots from GEE code
## include fuzzed NPS locations below when making spatial ARD
## remove unit, id, dataset, and yrfirename (others? plotid?) from that file
## make sure the startday format is the same on gee and here. I think startDay and endDay is a nice format

################# Spectral data ##################

spectral <- read.csv(paste0(data_dir, "input/spectral_", filename, ".csv"))
spectral$system.index <- NULL
spectral$.geo <- NULL

################# Terraclimate ##################

climate <- read.csv(paste0(data_dir, "input/climate_", fileap, ".csv"))
climate$system.index <- NULL
climate$.geo <- NULL

################# Topography ##################

topo <- read.csv(paste0(data_dir, "input/topo_", fileap, ".csv"))
topo$system.index <- NULL
topo$.geo <- NULL


##########################################################
################# Merge datasets ##################
##########################################################
# merge parameters for original dataset: (note startDay and endDay changed)
# by=c("UniqueID", "PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo")

allgee <- merge(spectral, climate, by=c("UniqueID", "FireYear", "pcnt_ba_mo", "Fire"))
allgee <- merge(allgee, topo, by=c("UniqueID", "FireYear", "pcnt_ba_mo", "Fire"))

allgee$aspectRad <- NULL
allgee$lat <- NULL

names(allgee)


##########################################################
################# Save data ##################
##########################################################

ard <- allgee[,!names(allgee) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID")]

### Write ARD dataframe without coords
# write.csv(ard, paste0(data_dir, "saved/ARD_nocoords_06042025.csv"), row.names = FALSE)
write.csv(ard, paste0(data_dir, "saved/ARD_", filename, ".csv"), row.names = FALSE)


##### ARD with plot locations and ecoregion #####

# point location data
coords <- terra::vect(paste0(data_dir, "RAVG_test.shp")) 
coords <- vect(paste0(data_dir, "input/allcoords_withbaloss_v7.shp")) 

# combine 
ardcoords <- merge(coords, ard, by=c("UniqueID", "PlotID", "YrFireName", "Dataset", "pcnt_ba_mo"))
ardcoords <- terra::merge(coords, ard, by=c("UniqueID", "pcnt_ba_mo", "Fire"))

ardcoords

names(ardcoords)

ardcoords <- ardcoords[,!names(ardcoords) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID", "Fire")]

## extract ecoregion
ecoregions <- vect(paste0(data_dir, "input/Ecoregions2017/Ecoregions2017.shp"))
ecoregions <- ecoregions[ecoregions$REALM=="Nearctic",] # subset to run faster

biome_info <- terra::extract(ecoregions[, "ECO_NAME"], ardcoords)

ardcoords$ecoregion <- biome_info$ECO_NAME

# save ARD
writeVector(ardcoords, paste0(data_dir, "saved/ARD_", filename, ".gpkg"))


