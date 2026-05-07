library(terra)
library(ggplot2)
library(caret)
library(ranger)
library(dplyr)

source("R/utils.R")

data_dir <- "data/"
fig_dir <- "figs/"
filename <- "ravg_test" # appended to the outputs from gee, and to the ard export from 01_make_ard

###### Step 1: Get the raw data in the format needed to extract the covariates from GEE

# get the RAVG data in the right format for the GEE code
ravg <- read.csv(paste0(data_dir, "RDS-2022-0018/Data/RAVG_V19_field.csv"))

ravg <- ravg[, c("Plot", "LonDD", "LatDD", "pdBA")]

# make sure there is a fire year column; for this RAVG data, the fire years are in the metadata
ravg$FireYear <- NULL
fy2018 <- c("BEAR", "BLUE", "DIEN", "SARD", "TIND", "VENA")

ravg$Fire <- substr(ravg$Plot, 1, 4)
ravg$FireYear <- ifelse(ravg$Fire %in% fy2018, 2018, 2017)

# make a vector
coords <- terra::vect(ravg,
                  geom = c("LonDD", "LatDD"),
                  crs  = "EPSG:4326",
                  keepgeom = FALSE)

# use names consistent with the rest of the code
names(coords) <- c("UniqueID", "pcnt_ba_mort", "Fire", "FireYear")
terra::writeVector(coords, "data/RAVG_test.shp", overwrite=TRUE)


###################################################################################

### Next steps: Upload that shapefile to GEE and run the ext_spectral, ext_climate, and ext_topo scripts.
### Download the outputs from Drive, and move them to your data directory in a subfolder called "input"

###################################################################################

### The following chunk can also be found in 01_make_ard.R but is included here for demonstrating the flow

################# Spectral data ##################
spectral <- read.csv(paste0(data_dir, "input/spectral_", filename, ".csv"))
spectral$system.index <- NULL
spectral$.geo <- NULL

################# Terraclimate ##################
climate <- read.csv(paste0(data_dir, "input/climate_", filename, ".csv"))
climate$system.index <- NULL
climate$.geo <- NULL

################# Topography ##################
topo <- read.csv(paste0(data_dir, "input/topo_", filename, ".csv"))
topo$system.index <- NULL
topo$.geo <- NULL

################# Merge datasets ##################
allgee <- merge(spectral, climate, by=c("UniqueID", "FireYear", "pcnt_ba_mo", "Fire"))
allgee <- merge(allgee, topo, by=c("UniqueID", "FireYear", "pcnt_ba_mo", "Fire"))

names(allgee)

################# Save data ##################
ard <- allgee[,!names(allgee) %in% c("FireYear", "startDay", "endDay")]

# write ARD dataframe without coords if you want to come back to it later
write.csv(ard, paste0(data_dir, "saved/ARD_", filename, ".csv"), row.names = FALSE)


##### Optionally, extract ecoregion if your data spans multiple ecoregions 
## and you want to see how the model skill varies across ecoregions

# point location data - may already be loaded above
coords <- terra::vect(paste0(data_dir, "RAVG_test.shp")) 

# combine with plot locations
ardcoords <- terra::merge(coords, ard, by=c("UniqueID", "pcnt_ba_mo", "Fire")) 

names(ardcoords)
ardcoords <- ardcoords[,!names(ardcoords) %in% c("FireYear", "Fire")]

# get the ecoregion data
ecoregions <- vect(paste0(data_dir, "input/Ecoregions2017/Ecoregions2017.shp"))
ecoregions <- ecoregions[ecoregions$REALM=="Nearctic",] # subset to run faster

biome_info <- terra::extract(ecoregions[, "ECO_NAME"], ardcoords)

ardcoords$ecoregion <- biome_info$ECO_NAME

# save ARD with coords and ecoregion
writeVector(ardcoords, paste0(data_dir, "saved/ARD_", filename, ".gpkg"), overwrite=TRUE)

#########################################################################################
### Run the random forest model (general model) on this new independent dataset

# load data
ard <- read.csv(paste0(data_dir, "saved/ARD_", filename, ".csv"))

# load model as rds
final_mod <- readRDS(paste0(data_dir, "rf_final_model_112925.rds"))

# run RF model on new data
set.seed(20250711)
preds <- predict(final_mod, data = ard)$predictions
results <- cbind(ard, pred = preds)


### assess outputs with R2
coef_of_determin(obs=results$pcnt_ba_mo, pred=results$pred) # 0.514


# calculate R2 for each ecoregion if there are multiple in the dataset, using ardcoords from above
results <- merge(results, ardcoords[,c("UniqueID", "ecoregion")], by="UniqueID")
r2_table <- as.data.frame(results) %>%
  group_by(ecoregion) %>%
  summarise(R2 = coef_of_determin(obs=pcnt_ba_mo, pred=pred), n_rows = n()) # caret gave strange results
r2_table


# visualize the results!
ggplot(results) +
  geom_point(aes(x=pcnt_ba_mo, y=pred, color=ecoregion)) + # remove ecoregion coloring if you don't have it
  geom_smooth(aes(x=pcnt_ba_mo, y=pred)) +
  ylim(0,1) +
  labs(x = "Observed BA loss",
       y = "Predicted BA loss") +
  theme_light() +
  theme(legend.position="bottom", legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE))


