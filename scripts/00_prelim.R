
## First, just a space to putz around in the FTM database
## Second, a little bit of processing on Saba's data to combine year of fire with the data (for getting the VIs for that year)

#library(aws.s3)

## first look at Alina Cansler's fire and tree mortality database
#bucket_name="tmp-sara"
#object_name= paste0("severity/data/FTM/Data/FTM_trees.csv")
#ftmtree <- s3read_using(FUN = read.csv, bucket = bucket_name, object = object_name)

ftmtree <- read.csv("VP/severity_tmp/data/original/FTM/Data/FTM_trees.csv")

head(ftmtree)
names(ftmtree)

roccaforte <- ftmtree[ftmtree$Dataset == "Roccaforte",]
unique(roccaforte$YrFireName)
treetest <- ftmtree[ftmtree$YrFireName=="2011 - Wallow",]
roccatest <- treetest[treetest$Plot == "1_4_14",]


ftmfire <- read.csv("VP/severity_tmp/data/original/FTM/Data/FTM_fires.csv")
head(ftmfire)
# save as vector for plotting
ftmfirevect <- vect(ftmfire, geom=c("Longitude", "Latitude"), crs="epsg:4326")
writeVector(ftmfirevect, "VP/severity_tmp/data/saved/FTM/FTM_fires.gpkg")

# remove fires with no plot size or min tree size
ftmfire_comp <- ftmfire[!is.na(ftmfire$Plot_size_ha),]
ftmfire_comp <- ftmfire_comp[!is.na(ftmfire_comp$Threshold_diameter_cm),] # doesn't remove any more
names(ftmfire_comp)

#############################################################
#################### SABA SABERI'S DATA ####################  
############################################################

ssplot <- read.csv("VP/severity_tmp/data/original/Saba/IPNW_BurnSeverity_SJS.csv")
head(ssplot)
ssgeo <- read.csv("VP/severity_tmp/data/original/Saba/IPNW_PlotLocations_sjs.csv")
ssgeo <- ssgeo[,1:4] # remove excess columns of NAs

ssplot <- merge(ssplot, ssgeo, by="PLOT_ID")
ssplot[ssplot$PLOT_ID=="PIONREBUNB1",2:3] <- NA
names(ssplot)[names(ssplot) == 'reburn'] <- "reburn1"
head(ssplot)


## add year of burn (note: its format is "Year_of_burn" but other datasets will have "fireyear")
yob <- read.csv("VP/severity_tmp/data/original/Saba/yearofburn.csv")

# create a dictionary to convert from the abbreviation to the full fire name
fire_name_dict <- c(
  "BERR" = "Berry",
  "MAPL" = "Maple",
  "PION" = "Pioneer",
  "RAIL" = "Rail",
  "ROCK" = "Rock Creek",
  "JOLL" = "Jolly Mountain",
  "JONE" = "Jones",
  "LIBE" = "Liberty",
  "LOLO" = "Lolo Peak",
  "MEYE" = "Meyers",
  "MILL" = "Milli",
  "NORS" = "Norse Peak",
  "REBE" = "Rebel",
  "RICE" = "Rice Ridge"
)

# create a new column in ssplot with the fire name abbreviation
ssplot$Fire_name_abbrev <- substr(ssplot$PLOT_ID, 1, 4)

# map the abbreviation to the full fire name
ssplot$Fire <- fire_name_dict[ssplot$Fire_name_abbrev]

# then merge the two data frames
merged_df <- merge(ssplot, yob, by="Fire")
head(merged_df)
merged_df[13,]$PLOT_ID <- "BERRYGLADEUNB201" # change one of the duplicate plotIDs
names(merged_df)[names(merged_df) =="PLOT_ID"] <- "PlotID"

## make this spatial to save
library(terra)
ssplotgeo <- vect(merged_df, geom=c("Long", "Lat"), crs="epsg:4326")


## Add image season start and end days as attributes
# imgseason <- aws.s3::s3read_using(FUN = vect, bucket = "vp-sci-grp", object = "fire-severity/processed/image-season-vectors/image-season_v1.0.gpkg")

image_season_path <- aws.s3::save_object(
  object = "s3://vp-sci-grp/fire-severity/processed/image-season-vectors/image-season_v1.0.gpkg",
  file = file.path(tempdir(), 'image-season_v1.0.gpkg')
)
img_season <- sf::st_read(image_season_path)
img_season <- vect(img_season)
img_season <- project(img_season, crs(ssplotgeo))

# intersect the plots with the image season polygons to get Start_Day and End_Day 
days <- terra::extract(imgseason, ssplotgeo)
ssplotgeo$Start_Day <- days$Start_Day
ssplotgeo$End_Day <- days$End_Day

# save as shapefile for uploading to GEE
writeVector(ssplotgeo, "VP/severity_tmp/data/saved/Saba/IPNW_Plot_Severity.shp", overwrite=TRUE)
write.csv(merged_df, "VP/severity_tmp/data/saved/Saba/IPNW_Plot_Severity_coords.csv", row.names = FALSE)



## make this spatial to save
library(terra)
ssplotgeo <- vect(merged_df, geom=c("Long", "Lat"), crs="epsg:4326")

# save as shapefile for uploading to GEE
writeVector(ssplotgeo, "VP/severity_tmp/data/saved/Saba/IPNW_Plot_Severity.shp", overwrite=TRUE)
write.csv(merged_df, "VP/severity_tmp/data/saved/Saba/IPNW_Plot_Severity_coords.csv", row.names = FALSE)

## test with GEE output
gee <- read.csv("VP/severity_tmp/data/saved/Saba/saba_fire_indices_zonal_stats.csv")
names(gee)
geeplot <- merge(ssplot, gee, by="PlotID")


# look at the alignment between the metrics I extracted and the GEE metrics that came with the dataset she gave me
library(ggplot2)
ggplot(geeplot, aes(x=GEE_RBR, y=rbr)) +
  geom_point()
