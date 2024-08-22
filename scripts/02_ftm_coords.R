
## This script is for combining FTM plot data with coordinates

#############################################################
############ PUBLICLY AVAILABLE COORDINATE DATA ############  
############################################################

####  McIver plot coordinates ####
## Plot.csv has the matching plot ID to FTM, Gridpoint.csv has the lat long, and GRIDPOINT_CODE is the key between them
mciver <- read.csv("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_McIver.csv")

mcivergridpoint <- read.csv("VP/severity_tmp/data/original/McIver/Data/Site/Gridpoint.csv")
mciverplot <- read.csv("VP/severity_tmp/data/original/McIver/Data/Site/Plot.csv")

mciverall <- merge(mcivergridpoint, mciverplot, by="GRIDPOINT_CODE") 
mciver_m <- merge(mciver, mciverall[,c("PLOT_CODE", "LATITUDE", "LONGITUDE")], by.x="Plot", by.y="PLOT_CODE")
head(mciver_m)
mciver_m$lat_wgs84 <- mciver_m$LATITUDE
mciver_m$lon_wgs84 <- mciver_m$LONGITUDE
mciver_m$LATITUDE <- NULL
mciver_m$LONGITUDE <- NULL

head(mciver_m)
write.csv(mciver_m, "VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_McIver_withcoords.csv", row.names = FALSE)

## add basal area loss (this is actually done in 04_combine_coords for McIver as of 8/20)

ftm_baloss <- read.csv("VP/severity_tmp/data/saved/FTM/FTM_ba.csv")
mciverba <- merge(mciver_m, ftm_baloss[, c("Plot", "YrFireName", "Dataset", "Unit", "ID", "pcnt_ba_mort")],
                by = c("Plot", "YrFireName", "Dataset", "Unit", "ID"), all.x=TRUE)

mciverba <- mciverba[!is.na(mciverba$pcnt_ba_mort),]
names(mciverba)
mciverba$FireYear <- substr(mciverba$YrFireName, 1, 4)
mciverba$utm_x <- NULL
mciverba$utm_y <- NULL
mciverba$utm_zone <- NULL
mciver_sub <- mciverba[mciverba$lon_wgs84 < -100,]
names(mciver_sub)

write.csv(mciver_sub, "VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_McIver_withcoordsba.csv", row.names = FALSE)

########################################
####  Jens Stevens plot coordinates ####
########################################

## can't actually combine with the FTM data
jens <- read.csv("VP/severity_tmp/data/original/FTM/Data/JensStevens/MasterPlotData.csv")
head(jens)

## delaying this for now - the FTM data doesn't have all tree measurements, only 4 trees/plot
## BUT, Jens' dataset does have live and dead BA for each plot for it says within 8m of the plot (but is that plot center or a buffer around the plot?)


########################################
####  Sharon Hood plot coordinates ####
########################################

hood <- read.csv("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_Hood.csv")
head(hood)
unique(hood$YrFireName)

hood$Fire <- substr(hood$YrFireName, 8, 40)
hood$FireYear <- substr(hood$YrFireName, 1, 4)

####################################################
################## first set ######################
####################################################

hoodcoord1 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/Hood_GPSpoints_fires.csv")
unique(hoodcoord1$Fire)
head(hoodcoord1)

hood1 <- hood[hood$Fire %in% hoodcoord1$Fire,] # 306 rows in FTM, but only 275 in the data from Sharon
hood <- merge(hood, hoodcoord1[hoodcoord1$Fire %in% c("Green Knoll", "Lubrecht", "Moose", "Mussigbrod"),], by=c("Fire", "Plot"), all.x = TRUE)
hood <- merge(hood, hoodcoord1[hoodcoord1$Fire %in% c("Grizzly", "Tower"),], by.x = c("Fire", "ID"), by.y = c("Fire", "Plot"), all.x = TRUE)

head(hood)

# Check where Lattitude.x and Longitude.x are not NA and update lat_wgs84 and lon_wgs84
hood$lat_wgs84 <- ifelse(!is.na(hood$Lattitude.x), hood$Lattitude.x, hood$lat_wgs84)
hood$lon_wgs84 <- ifelse(!is.na(hood$Longitude.x), hood$Longitude.x, hood$lon_wgs84)

# Check where Lattitude.y is not NA, and lat_wgs84 is still NA, then update lat and lon_wgs84
hood$lat_wgs84 <- ifelse(!is.na(hood$Lattitude.y) & is.na(hood$lat_wgs84), hood$Lattitude.y, hood$lat_wgs84)
hood$lon_wgs84 <- ifelse(!is.na(hood$Longitude.y) & is.na(hood$lon_wgs84), hood$Longitude.y, hood$lon_wgs84)

hood$Lattitude.x <- NULL
hood$Lattitude.y <- NULL
hood$Longitude.x <- NULL
hood$Longitude.y <- NULL

summary(hood$lat_wgs84)
summary(hood$lon_wgs84)

####################################################
################## second set ######################
####################################################

hoodcoord2 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/RodeoChediski_FTM_plot_coordinates.csv")
head(hoodcoord2)
unique(hoodcoord2$Territory)
hoodcoord2 <- hoodcoord2[!is.na(hoodcoord2$UTM.E),] # lots of empty rows
hoodcoord2$Fire <- "Rodeo"
hoodcoord2$Territory <- NULL

rodeo <- hood[hood$Fire=="Rodeo",] # 122 rows in Sharon's data, 112 in FTM (some maybe tossed)
head(rodeo)
head(hoodcoord2)
unique(hoodcoord2$Unit)
unique(hoodcoord2$Plot)

unique(rodeo$Unit)
unique(rodeo$Plot)

hoodcoord2$utm_zone <- "12"

library(sf)
df_sf <- st_as_sf(hoodcoord2, coords = c("UTM.E", "UTM.N"), crs = paste0("EPSG:326", hoodcoord2$utm_zone[1]))

# Reproject to lat long
df_sf_wgs84 <- st_transform(df_sf, 4326)

# Extract coordinates into a data frame
coords_wgs84 <- st_coordinates(df_sf_wgs84)

# Add them back to the original data frame if needed
hoodcoord2$latitude <- coords_wgs84[, "Y"]
hoodcoord2$longitude <- coords_wgs84[, "X"]

# only merge latlon (utm with zones is too confusing)
hoodcoord2_1 <- hoodcoord2[, c(1:2, 5, 7:8)]
head(hoodcoord2_1)

hood <- merge(hood, hoodcoord2_1, by=c("Fire", "Unit", "Plot"), all.x=TRUE)
head(hood)


hood$lat_wgs84 <- ifelse(!is.na(hood$latitude) & hood$Fire=="Rodeo", hood$latitude, hood$lat_wgs84)
hood$lon_wgs84 <- ifelse(!is.na(hood$longitude) & hood$Fire=="Rodeo", hood$longitude, hood$lon_wgs84)

head(hood)
summary(hood$lat_wgs84) # added 112 coords (n in Sharon's data was 122, but only 112 rows in ftm)
hood$latitude <- NULL
hood$longitude <- NULL

####################################################
################## third set ######################
####################################################

hoodcoord3 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/TCEF_FTM_plot_coordinates.csv")
head(hoodcoord3)
unique(hoodcoord3$YrFireName)

hood3 <- hood[hood$YrFireName %in% c("2003 - Tenderfoot Spring", "2002 - Tenderfoot Fall"),] # 233 in Sharon's data, 96 in FTM
nrow(hood[hood$YrFireName=="2003 - Tenderfoot Spring",])

df_sf <- st_as_sf(hoodcoord3, coords = c("Easting", "Northing"), crs = "EPSG:26712")

# Reproject to lat long
df_sf_wgs84 <- st_transform(df_sf, 4326)

# Extract coordinates into a data frame
coords_wgs84 <- st_coordinates(df_sf_wgs84)

# Add them back to the original data frame if needed
hoodcoord3$latitude <- coords_wgs84[, "Y"]
hoodcoord3$longitude <- coords_wgs84[, "X"]

# only merge latlon (utm with zones is too confusing)
hoodcoord3_1 <- hoodcoord3[, c(3:4, 11:13)]
head(hoodcoord3_1)

unique(hood$Unit[hood$YrFireName %in% c("2003 - Tenderfoot Spring", "2002 - Tenderfoot Fall")])
unique(hoodcoord3_1$Unit[hoodcoord3_1$YrFireName %in% c("2003 - Tenderfoot Spring", "2002 - Tenderfoot Fall")])

hood <- merge(hood, hoodcoord3_1, by=c("YrFireName", "Unit", "Plot"), all.x=TRUE)
head(hood)


hood$lat_wgs84 <- ifelse(!is.na(hood$latitude) & hood$YrFireName %in% c("2003 - Tenderfoot Spring", "2002 - Tenderfoot Fall"), hood$latitude, hood$lat_wgs84)
hood$lon_wgs84 <- ifelse(!is.na(hood$longitude) & hood$YrFireName %in% c("2003 - Tenderfoot Spring", "2002 - Tenderfoot Fall"), hood$longitude, hood$lon_wgs84)

hood$latitude <- NULL
hood$longitude <- NULL

summary(hood$lat_wgs84)

nrow(hood[!is.na(hood$lat_wgs84),]) # 460 extra plots as of 10/25!

####################################################
################## Hood fourth set ##################
####################################################

# Fixed-area plot lodgepole plots 0.02 ha and ponderosa plots 0.04
hoodtree4 <- ftmtree[ftmtree$YrFireName=="2007 - Neola North",]

hoodtree41 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/FTM_trees_corrected_Neolaonly.csv")
head(hoodtree41)
length(unique(hoodtree41$fullID))

hoodcoord4 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/Neola_PIPO_PlotLocations_corrected.csv")
head(hoodcoord4)
unique(hoodcoord4$YrFireName)

hood4 <- hood[hood$YrFireName %in% c("2007 - Neola North"),] # 233 in Sharon's data, 96 in FTM
nrow(hood[hood$YrFireName=="2007 - Neola North",])

# check column name matching
head(hood4)
head(hoodcoord4)


# only merge latlon 

unique(hood$Unit[hood$YrFireName %in% c("2007 - Neola North")])

hood <- merge(hood, hoodcoord4[, c("FullID", "LAT_WGS84", "LONG_WGS84")], by.x="Plot", by.y="FullID", all.x=TRUE)


hood$lat_wgs84 <- ifelse(!is.na(hood$LAT_WGS84) & hood$YrFireName %in% c("2007 - Neola North"), hood$LAT_WGS84, hood$lat_wgs84)
hood$lon_wgs84 <- ifelse(!is.na(hood$LONG_WGS84) & hood$YrFireName %in% c("2007 - Neola North"), hood$LONG_WGS84, hood$lon_wgs84)

hood$LAT_WGS84 <- NULL
hood$LONG_WGS84 <- NULL

summary(hood$lat_wgs84)

nrow(hood[!is.na(hood$lat_wgs84),]) # 524 plots 


####################################################
################## Hood fifth set ##################
####################################################

davis <- ftmfire[ftmfire$Dataset=="Davis",]

# Fixed-area plot lodgepole plots 0.02 ha and ponderosa plots 0.04
hoodtree5 <- ftmtree[ftmtree$YrFireName=="2004 - Parks",]

hoodcoord5 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/Payette_BlacksMountain_fires.csv")
head(hoodcoord5)
unique(hoodcoord5$File.Name)

hood5 <- hood[hood$YrFireName %in% c("2003 - Black Mountain 2", "2004 - Deadmans", "2004 - Parks"),] # 233 in Sharon's data, 96 in FTM
nrow(hood[hood$YrFireName=="2003 - Black Mountain 2",])

# check column name matching
head(hood5)
head(hoodcoord5)

# only merge latlon - the site matching needs to be figured out still
unique(hood5[hood5$YrFireName=="2003 - Black Mountain 2",]$Plot)

blackmtn <- hood5[hood5$YrFireName=="2003 - Black Mountain 2",]
blackmtn_c <- hoodcoord5[hoodcoord5$File.Name=="BLACKS_MOUNTAIN_GPS",]
blackmtn_c$Plot2 <- substr(blackmtn_c$Plot, 5,6)
blackmtn_c$Plot2 <- as.numeric(blackmtn_c$Plot2)
blackmtn_c$Plot3 <- paste0(blackmtn_c$Plot2, "B")
blackmtn <- merge(blackmtn, blackmtn_c[, c(4, 5, 10)], by.x="Plot", by.y="Plot3")
head(blackmtn)

deadman <- hood5[hood5$YrFireName=="2004 - Deadmans",]
parks <- hood5[hood5$YrFireName=="2004 - Parks",]

deadmanparks <- hoodcoord5[hoodcoord5$File.Name=="PARKS_and_DEADMAN_GPS",]
deadmanparks$fire <- substr(deadmanparks$Plot, 1, 2)
deadmanparks$Plot2 <- as.numeric(substr(deadmanparks$Plot, 4, 5))
deadmanparks$Plot3 <- paste0(deadmanparks$Plot2, "B")

deadman_c <- deadmanparks[deadmanparks$fire=="DM",]
deadman <- merge(deadman, deadman_c[, c(4, 5, 11)], by.x="Plot", by.y="Plot3")
head(deadman)
parks_c <- deadmanparks[deadmanparks$fire=="PC",]
parks_c <- parks_c[parks_c$altitude != 5127,]
parks_c$Plot3[parks_c$Plot3=="21B"] <- "21B/PC_Attack_7"
parks <- merge(parks, parks_c[, c(4, 5, 11)], by.x="Plot", by.y="Plot3")

head(parks)

hood5_1 <- rbind(blackmtn, deadman, parks)
head(hood5_1)

hood <- merge(hood, hood5_1[, c("Plot", "YrFireName", "latitude", "longitude")], all.x=TRUE)

hood$lat_wgs84 <- ifelse(!is.na(hood$latitude) & hood$YrFireName %in% c("2003 - Black Mountain 2", "2004 - Deadmans", "2004 - Parks"), hood$latitude, hood$lat_wgs84)
hood$lon_wgs84 <- ifelse(!is.na(hood$longitude) & hood$YrFireName %in% c("2003 - Black Mountain 2", "2004 - Deadmans", "2004 - Parks"), hood$longitude, hood$lon_wgs84)

hood$latitude <- NULL
hood$longitude <- NULL

summary(hood$lat_wgs84)

nrow(hood[!is.na(hood$lat_wgs84),]) # 561 extra plots as of 3/13/24!


####################################################
################## add BA loss ######################
####################################################
names(hood)
ftm_baloss <- read.csv("VP/severity_tmp/data/saved/FTM/FTM_ba_v2.csv")
names(ftm_baloss)

hood <- hood[!is.na(hood$lat_wgs84),]
length(hood[hood$YrFireName == "2002 - Rodeo",])

hoodba <- merge(hood, ftm_baloss[, c("Plot", "YrFireName", "Dataset", "Unit", "ID", "pcnt_ba_mort")], 
                by = c("Plot", "YrFireName", "Dataset", "Unit", "ID"))
unique(hood$YrFireName)
unique(hoodba$YrFireName)
# 2002 - Rodeo is removed because it's variable radius; asking Sharon about the plot sizes for Mussigbrod, Tenderfoot Fall, and Tenderfoot Spring

#hood$FireYear <- substr(hood$YrFireName, 1, 4)
hoodba$utm_x <- NULL
hoodba$utm_y <- NULL
hoodba$utm_zone <- NULL
hoodba <- hoodba[hoodba$lon_wgs84 < -100,] # make sure there aren't any too far east for this
names(hoodba)

write.csv(hoodba, "VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Hood_withcoordsba.csv", row.names = FALSE)


####################################################
################## Harvey data ##################
####################################################

harvey <- read.csv("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_Harvey.csv")
head(harvey)
unique(harvey$YrFireName)

harvey$Fire <- substr(harvey$YrFireName, 8, 40)
harvey$FireYear <- substr(harvey$YrFireName, 1, 4)

#hoodtree4 <- ftmtree[ftmtree$YrFireName=="2007 - Neola North",]

harveycoord <- read.csv("VP/severity_tmp/data/original/FTM/Data/BrianHarvey/Fire_Severity_plots_from_FTM.csv")
head(harveycoord)
unique(harveycoord$FIRE)

# east, lutz, fort, salt
# grayhills

harveycoord$Plot <- ifelse(harveycoord$FIRE %in% c("gunbarrel"),
                           paste0(harveycoord$FIRE, harveycoord$plotname), 
                           ifelse(harveycoord$FIRE %in% c("east", "fort", "lutz", "puzz", "rr", "sadd", "salt"), 
                                  harveycoord$plotname,
                                  ifelse(harveycoord$FIRE %in% c("newfork"),
                                         paste0("NewFork", harveycoord$plotname), NA)
                           )
)
harveycoord[harveycoord$Plot=="gunbarrelclearwater10",]$Plot <- "gunbarrelclearwater 10"

harvey <- merge(harvey, harveycoord[, c("Plot", "lon", "lat")], by="Plot", all.x=TRUE)


# check column name matching
head(harvey)


harvey$lat_wgs84 <- ifelse(!is.na(harvey$lat), harvey$lat, harvey$lat_wgs84)
harvey$lon_wgs84 <- ifelse(!is.na(harvey$lon), harvey$lon, harvey$lon_wgs84)

harvey$lat <- NULL
harvey$lon <- NULL

summary(harvey$lat_wgs84)
nrow(harvey[!is.na(harvey$lat_wgs84),]) # 460 extra plots as of 10/25!


#################
## Add BA loss ##
##################

ftm_baloss <- read.csv("VP/severity_tmp/data/saved/FTM/FTM_ba_v2.csv")
names(ftm_baloss)

unique(ftm_baloss[ftm_baloss$YrFireName %in% unique(harvey$YrFireName),]$YrFireName)
ftmtree_gunbarrel <- ftmtree[ftmtree$YrFireName=="2008 - Gunbarrel",]


harvey <- harvey[!is.na(harvey$lat_wgs84),]

harveyba <- merge(harvey, ftm_baloss[, c("Plot", "YrFireName", "Dataset", "Unit", "ID", "pcnt_ba_mort")], 
                by = c("Plot", "YrFireName", "Dataset", "Unit", "ID"))

#hood$FireYear <- substr(hood$YrFireName, 1, 4)
harveyba$utm_x <- NULL
harveyba$utm_y <- NULL
harveyba$utm_zone <- NULL
harveyba <- harveyba[harveyba$lon_wgs84 < -100,] # make sure there aren't any too far east for this
names(harveyba)

write.csv(harveyba, "VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Harvey_withcoordsba.csv", row.names = FALSE)


####################################################
################## Agne data ##################
####################################################

agnecoords <- read.csv("VP/severity_tmp/data/original/FTM/Data/MichelleAgne/FTM_plots_Agne.csv")

head(agnecoords)

# Load the terra package
library(terra)

# Assuming your dataframe is named 'df' and has columns 'easting', 'northing'
# First, create a SpatVector from your dataframe
agnevect <- vect(agnecoords, geom=c("utm_x", "utm_y"), crs="+proj=utm +zone=10 +datum=WGS84 +units=m +no_defs")

# Now project from UTM to WGS 84 (lat/long)
agnevect_latlong <- project(agnevect, "+proj=longlat +datum=WGS84")

# Use the coordinates or geom function to get the longitude and latitude
coords <- geom(agnevect_latlong)

# Create a dataframe from the extracted coordinates
agne_latlon <- data.frame(longitude = coords[,3], latitude = coords[,4])

head(agne_latlon)

agne <- cbind(agnecoords, agne_latlon)
head(agne)


agne$lat_wgs84 <- ifelse(!is.na(agne$latitude), agne$latitude, agne$lat_wgs84)
agne$lon_wgs84 <- ifelse(!is.na(agne$longitude), agne$longitude, agne$lon_wgs84)

agne$latitude <- NULL
agne$longitude <- NULL


#################
## Add BA loss ##
##################

ftm_baloss <- read.csv("VP/severity_tmp/data/saved/FTM/FTM_ba_v2.csv")
names(ftm_baloss)

agne <- agne[!is.na(agne$lat_wgs84),]

agneba <- merge(agne, ftm_baloss[, c("Plot", "YrFireName", "Dataset", "Unit", "ID", "pcnt_ba_mort")], 
                  by = c("Plot", "YrFireName", "Dataset", "Unit", "ID"))

agneba$utm_x <- NULL
agneba$utm_y <- NULL
agneba$utm_zone <- NULL
agneba <- agneba[agneba$lon_wgs84 < -100,] # make sure there aren't any too far east for this
names(agneba)

write.csv(agneba, "VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Agne_withcoordsba.csv", row.names = FALSE)


####################################################
################## Andrus data ######################
####################################################

andrus <- read.csv("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_Andrus.csv")
unique(andrus$YrFireName)

andrus$Fire <- substr(andrus$YrFireName, 8, 40)
andrus$FireYear <- substr(andrus$YrFireName, 1, 4)

andruscoord <- read.csv("VP/severity_tmp/data/original/FTM/Data/RobbieAndrus/Andrus_Site_locations_4Sara.csv")

andruscoord[,11:23] <- NULL
head(andruscoord)
unique(andruscoord$Fire)

unique(andrus$Fire)

andruscoord[andruscoord$Fire=="Eastfork",]$Fire <- "East Fork"
andruscoord[andruscoord$Fire=="LittleSands",]$Fire <- "Little Sands"
andruscoord[andruscoord$Fire=="WindyPass",]$Fire <- "Windy Pass"

unique(andruscoord$Plot.ID)
head(andrus)
unique(andrus$Plot)


andruscoord$utm_zone <- "13"

library(sf)
df_sf <- st_as_sf(andruscoord, coords = c("Easting", "Northing"), crs = paste0("EPSG:326", andruscoord$utm_zone[1]))

# Reproject to lat long
df_sf_wgs84 <- st_transform(df_sf, 4326)

# Extract coordinates into a data frame
coords_wgs84 <- st_coordinates(df_sf_wgs84)

# Add them back to the original data frame if needed
andruscoord$latitude <- coords_wgs84[, "Y"]
andruscoord$longitude <- coords_wgs84[, "X"]

head(andruscoord)

# only merge latlon (utm with zones is too confusing)
andruscoord_1 <- andruscoord[, c(1:2, 12:13)]
head(andruscoord_1)

andrus1 <- merge(andrus, andruscoord_1, by.x=c("Fire", "Plot"), by.y=c("Fire", "Plot.ID"))
head(andrus1)


andrus1$lat_wgs84 <- ifelse(!is.na(andrus1$latitude), andrus1$latitude, andrus1$lat_wgs84)
andrus1$lon_wgs84 <- ifelse(!is.na(andrus1$longitude), andrus1$longitude, andrus1$lon_wgs84)

head(andrus1)
summary(andrus1$lat_wgs84) # added 112 coords (n in Sharon's data was 122, but only 112 rows in ftm)
andrus1$latitude <- NULL
andrus1$longitude <- NULL

#################
## Add BA loss ##
##################

ftm_baloss <- read.csv("VP/severity_tmp/data/saved/FTM/FTM_ba_v2.csv")
names(ftm_baloss)

andrus1 <- andrus1[!is.na(andrus1$lat_wgs84),]

andrusba <- merge(andrus1, ftm_baloss[, c("Plot", "YrFireName", "Dataset", "Unit", "ID", "pcnt_ba_mort")], 
                by = c("Plot", "YrFireName", "Dataset", "Unit", "ID"))

andrusba$utm_x <- NULL
andrusba$utm_y <- NULL
andrusba$utm_zone <- NULL
andrusba <- andrusba[andrusba$lon_wgs84 < -100,] # make sure there aren't any too far east for this
names(andrusba)
head(andrusba)
write.csv(andrusba, "VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Andrus_withcoordsba.csv", row.names = FALSE)

