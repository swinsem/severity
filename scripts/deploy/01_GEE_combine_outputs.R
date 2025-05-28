library(ggplot2)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(terra)
library(glue)

data_dir <- "research/severity/data/"
data_dir1 <- "VP/severity_tmp/data/saved/"



##########################################################
################# Spectral data ##################
##########################################################

spectral <- read.csv(paste0(data_dir, "GEE/spectral_08242024.csv"))

spectral$system.index <- NULL
spectral$.geo <- NULL


##########################################################
################# Terraclimate ##################
##########################################################

climate <- read.csv(paste0(data_dir, "climate_05282025.csv"))

climate$system.index <- NULL
climate$.geo <- NULL


##########################################################
################# Topography ##################
##########################################################

topo <- read.csv(data_dir, "GEE/topo_05282025.csv")

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
duplicates_count <- alldata %>%
  group_by(Dataset, Unit, YrFireName, ID, PlotID) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)
duplicates_count

## To get the total number of duplicated rows
#total_duplicated_rows <- sum(duplicates_count$n) - nrow(duplicates_count)
#total_duplicated_rows

#alldata <- alldata[alldata$PlotID != "Wallow9_1_118",]

##########################################################
################# Save data ##################
##########################################################

##### Dataframe
# 
# alldata$UniqueID <- paste0(alldata$Dataset, "_", alldata$PlotID)
# 
# alldata[alldata$Dataset=="Hood",]$UniqueID <- 
#   paste0(alldata[alldata$Dataset=="Hood",]$UniqueID, "_", substring(alldata[alldata$Dataset=="Hood",]$YrFireName, 8))
# alldata[alldata$Dataset=="Davis",]$UniqueID <- 
#   paste0(alldata[alldata$Dataset=="Davis",]$UniqueID, "_", substring(alldata[alldata$Dataset=="Davis",]$YrFireName, 8))

length(unique(allgee$UniqueID))
nrow(allgee)

# if there's a mismatch, look at the data to see how to make the UniqueID column truly unique
#duplicated_rows <- alldata[duplicated(alldata$UniqueID) | duplicated(alldata$UniqueID, fromLast = TRUE), ]

ard <- allgee[,!names(allgee) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID")]

### Write ARD dataframe without coords
write.csv(ard, paste0(data_dir, "ARD_nocoords.csv"), row.names = FALSE)


##### ARD with plot locations #####

# point location data
coords <- vect(paste0(data_dir, "allcoords_withbaloss_v7.shp"))

# combine 
ardcoords <- merge(coords, ard, by=c("UniqueID", "PlotID", "YrFireName", "Dataset", "pcnt_ba_mo"))
ardcoords

names(ardcoords)

ardcoords <- ardcoords[,!names(ardcoords) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID")]

# save ARD
writeVector(ardcoords, paste0(data_dir, "ARD_05282025.gpkg"))


