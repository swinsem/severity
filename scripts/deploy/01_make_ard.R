library(ggplot2)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(terra)
library(glue)

data_dir <- "research/severity/data/"
data_dir1 <- "VP/severity_tmp/data/saved/"

### to do:
## remove NPS plots from GEE code
## include fuzzed NPS locations below when making spatial ARD

##########################################################
################# Spectral data ##################
##########################################################

spectral <- read.csv(paste0(data_dir, "input/spectral_05282025.csv"))

spectral$system.index <- NULL
spectral$.geo <- NULL


##########################################################
################# Terraclimate ##################
##########################################################

climate <- read.csv(paste0(data_dir, "input/climate_05282025.csv"))

climate$system.index <- NULL
climate$.geo <- NULL


##########################################################
################# Topography ##################
##########################################################

topo <- read.csv(paste0(data_dir, "input/topo_05282025.csv"))

topo$system.index <- NULL
topo$.geo <- NULL


##########################################################
################# Merge datasets ##################
##########################################################

allgee <- merge(spectral, climate, by=c("UniqueID", "PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo"))

allgee <- merge(allgee, topo, by=c("UniqueID", "PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo"))


allgee$aspectRad <- NULL
allgee$lat <- NULL

names(allgee)

# are there duplicates??
duplicates_count <- allgee %>%
  group_by(Dataset, Unit, YrFireName, ID, PlotID) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)
duplicates_count

##########################################################
################# Save data ##################
##########################################################

ard <- allgee[,!names(allgee) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID")]

### Write ARD dataframe without coords
write.csv(ard, paste0(data_dir, "saved/ARD_nocoords_06042025.csv"), row.names = FALSE)


##### ARD with plot locations and ecoregion #####

# point location data
coords <- vect(paste0(data_dir, "input/allcoords_withbaloss_v7.shp")) 

# combine 
ardcoords <- merge(coords, ard, by=c("UniqueID", "PlotID", "YrFireName", "Dataset", "pcnt_ba_mo"))
ardcoords

names(ardcoords)

ardcoords <- ardcoords[,!names(ardcoords) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID")]

## extract ecoregion
ecoregions <- vect(paste0(data_dir, "input/Ecoregions2017/Ecoregions2017.shp"))
ecoregions <- ecoregions[ecoregions$REALM=="Nearctic",] # subset 

biome_info <- terra::extract(ecoregions[, "ECO_NAME"], ardcoords)

ardcoords$ecoregion <- biome_info$ECO_NAME

# save ARD
writeVector(ardcoords, paste0(data_dir, "saved/ARD_06042025.gpkg"))


