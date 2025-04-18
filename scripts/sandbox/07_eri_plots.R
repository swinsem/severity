

library(dplyr)

ftmtree <- read.csv("VP/severity_tmp/data/original/FTM/Data/FTM_trees.csv")

head(ftmtree)
names(ftmtree)

roccaforte <- read.csv("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_Roccaforte.csv")
unique(roccaforte$YrFireName)


####################################################
################## Wallow ##################
####################################################

wallow <- ftmtree[ftmtree$YrFireName=="2011 - Wallow",]

# Read the datasets
wallow <- ftmtree[ftmtree$YrFireName == "2011 - Wallow", ]
wallow <- wallow[, c(1:16, 21:24)]
originalwallow <- read.csv("Downloads/wallow.csv")
names(originalwallow) 


# 1. Identify trees with unique combinations of species, dbh, and height across all plots
unique_trees <- wallow %>%
  group_by(Species, DBH_cm, HT_m) %>%
  filter(n() == 1) %>%
  ungroup()

# 2. From the unique combinations, choose one tree from each plot
key_wallow <- unique_trees %>%
  group_by(Plot) %>%
  slice_head(n = 1) %>%
  ungroup()

# For the 'originalwallow' dataset, pick one tree for each plot
# key_originalwallow <- originalwallow %>%
#   group_by(Plot_ID) %>%
#   slice_head(n = 1) %>%
#   ungroup()

# 3. Join the two datasets based on the unique combinations
matched_data <- key_wallow %>%
  inner_join(originalwallow, 
             by = c("Species" = "Sp_code", 
                    "DBH_cm" = "X2012.DBH_x", 
                    "HT_m" = "X2012.TotHt_m"))

# Review the result
head(matched_data)

# Check unmatched plots - not really sure why these are here
unmatched_wallow_plots <- setdiff(unique(key_wallow$Plot), matched_data$Plot)
unmatched_originalwallow_plots <- setdiff(unique(originalwallow$Plot_ID), matched_data$Plot_ID)

for(x in unmatched_wallow_plots) {
  print(x)
  print(nrow(wallow[wallow$Plot == x,]))
}
for(x in unmatched_originalwallow_plots) {
  print(x)
  print(nrow(originalwallow[originalwallow$Plot_ID == x,]))
}

# two plots with 1 tree each
wallow[wallow$Plot=="8_1_99",]
originalwallow[originalwallow$Plot_ID=="SFK-1-TFR-132",]

wallow[wallow$Plot=="8_1_102",]
originalwallow[originalwallow$Plot_ID=="SFK-1-TFR-195",]

df <- data.frame(
  eri = c("ALP-2-TFR-81", "NUT-1-UNT-278", "ODR-2-TFR-108", "SFK-1-TFR-197", "SFK-2-TFR-16", 
          "SFK-1-TFR-132", "SFK-1-TFR-195"),
  ftm = c("2_1_26", "6_2_77", "7_1_83", "8_1_103", "9_1_114", 
          "8_1_99", "8_1_102")
)

df1 <- as.data.frame(matched_data[, c("Plot", "Plot_ID")])
names(df1) <- c("ftm", "eri")

wallowkey <- rbind(df, df1)
write.csv(wallowkey, "VP/severity_tmp/data/saved/FTM/data_request/Roccaforte_wallowkey.csv")

# Count rows in originalwallow for each eri value in wallowkey
originalwallow_counts <- originalwallow %>%
  group_by(Plot_ID) %>%
  summarise(ow_count = n()) %>%
  right_join(wallowkey, by = c("Plot_ID" = "eri"))
names(originalwallow_counts) <- c("eri", "ow_count", "ftm")

# Count rows in wallow for each ftm value in wallowkey
wallow_counts <- wallow %>%
  group_by(Plot) %>%
  summarise(w_count = n()) %>%
  right_join(wallowkey, by = c("Plot" = "ftm"))
names(wallow_counts) <- c("ftm", "w_count", "eri")

# Add these counts as new columns to wallowkey
#wallowkey2 <- left_join(wallowkey, originalwallow_counts, by = "eri")  # already done in making originalwallow_counts
wallowkey2 <- left_join(originalwallow_counts, wallow_counts, by = "ftm") %>%
  select(eri.x, ftm, ow_count, w_count)
head(wallowkey2)
#wallowkey2 <- merge(wallowkey2, wallow_counts, by.x="ftm", by.y="ftm")

############################
#### Add Coordinates ####
wallowc <- read.csv("VP/severity_tmp/data/original/FTM/Data/Roccaforte/Roccaforte_Wallow_UTM.csv")

head(wallowc)
wallowplots <- wallowc %>%
  distinct(Plot_ID, UTM.Zone, UTM_Easting_X, UTM_Northing_Y)
head(wallowplots)

## Convert utm coords to lat long
# First, create a SpatVector from your dataframe
unique(wallowplots$UTM.Zone)
wallowvect <- vect(wallowplots, geom=c("UTM_Easting_X", "UTM_Northing_Y"), crs="+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

# Now project from UTM to WGS 84 (lat/long)
wallowvect_latlong <- project(wallowvect, "+proj=longlat +datum=WGS84")

# Use the coordinates or geom function to get the longitude and latitude
coords <- geom(wallowvect_latlong)

# Create a dataframe from the extracted coordinates
wallow_latlon <- data.frame(longitude = coords[,3], latitude = coords[,4])

head(wallow_latlon)

wallowplots <- cbind(wallowplots, wallow_latlon)
head(wallowplots)
head(wallowkey)
wallowplots <- merge(wallowplots, wallowkey, by.x="Plot_ID", by.y="eri")

roccaforte <- merge(roccaforte, wallowplots[, c("ftm", "latitude", "longitude")], by.x="Plot", by.y="ftm", all.x=TRUE)


roccaforte$lat_wgs84 <- ifelse(!is.na(roccaforte$latitude), roccaforte$latitude, roccaforte$lat_wgs84)
roccaforte$lon_wgs84 <- ifelse(!is.na(roccaforte$longitude), roccaforte$longitude, roccaforte$lon_wgs84)

roccaforte$latitude <- NULL
roccaforte$longitude <- NULL

####################################################
################## Lower Middle Mountain ##################
####################################################

# Read the datasets
lowmid <- ftmtree[ftmtree$YrFireName == "2008 - Lower Middle Mountain", ]
lowmid <- lowmid[, c(1:16, 21:24)]
originallowmid <- read.csv("Downloads/lowermiddlemtn.csv")
names(originallowmid)


# 1. Identify trees with unique combinations of species, dbh, and height across all plots
unique_trees <- lowmid %>%
  group_by(Species, DBH_cm, HT_m) %>%
  filter(n() == 1) %>%
  ungroup()

# 2. From the unique combinations, choose one tree from each plot
key_lowmid <- unique_trees %>%
  group_by(Plot) %>%
  slice_head(n = 1) %>%
  ungroup()

# 3. Join the two datasets based on the unique combinations

matched_data1 <- key_lowmid %>%
  left_join(originallowmid, 
            by = c("Species" = "Sp_code", 
                   "DBH_cm" = "X2003.Pre.dbh_x", 
                   "HT_m" = "X2003.Pre.TotHt..m."))


# Check unmatched plots - not really sure why these are here
unmatched_lowmid_plots <- setdiff(unique(key_lowmid$Plot), matched_data1$Plot)
unmatched_originallowmid_plots <- setdiff(unique(originallowmid$Plot.), matched_data1$Plot.)

for(x in unmatched_lowmid_plots) {
  print(x)
  print(nrow(lowmid[lowmid$Plot == x,]))
}
for(x in unmatched_originallowmid_plots) {
  print(x)
  print(nrow(originallowmid[originallowmid$Plot. == x,]))
}

# 3_NA_37 and 1-3-17
# 4-3-19 and 3_NA_159
duplicated(matched_data1$Plot)
matched_data1 <- as.data.frame(matched_data1)
matched_data1[matched_data1$Plot=="3_NA_37",]
lowmid[lowmid$Plot=="3_NA_37",]
originallowmid[originallowmid$Plot.=="1-3-17",]

matched_data1 <- matched_data1[!(matched_data1$Plot == "3_NA_159" & matched_data1$Plot. == "1-3-17"), ]




lowmid[lowmid$Plot=="2_NA_15",]
originallowmid[originallowmid$Plot.=="1-2-15",]

lowmid[lowmid$Plot=="2_NA_54",]
originallowmid[originallowmid$Plot.=="2-2-14",]


df <- data.frame(
  eri = c("1-2-8", "1-2-13", "3-2-1", "1-2-15", "2-2-14" ),
  ftm = c("2_NA_8", "2_NA_13", "2_NA_81", "2_NA_15", "2_NA_54" )
)

df1 <- as.data.frame(matched_data1[, c("Plot", "Plot.")])
names(df1) <- c("ftm", "eri")
df1 <- df1[complete.cases(df1),]

lowermidkey <- rbind(df, df1)

duplicated(lowermidkey$eri)
write.csv(lowermidkey, "VP/severity_tmp/data/saved/FTM/data_request/Roccaforte_lowermiddlemtnkey.csv")
head(lowermidkey)

# Count rows in originallowmid for each eri value in wallowkey
originallowmid_counts <- originallowmid %>%
  group_by(Plot.) %>%
  summarise(ol_count = n()) %>%
  right_join(lowermidkey, by = c("Plot." = "eri"))
names(originallowmid_counts) <- c("eri", "ol_count", "ftm")

# Count rows in lowmid for each ftm value in wallowkey
lowmid_counts <- lowmid %>%
  group_by(Plot) %>%
  summarise(l_count = n()) %>%
  right_join(lowermidkey, by = c("Plot" = "ftm"))
names(lowmid_counts) <- c("ftm", "l_count", "eri")

# Add these counts as new columns - the mismatches in numbers are due to ftm only including trees that were alive prefire!
lowmidkey2 <- left_join(originallowmid_counts, lowmid_counts, by = "ftm") %>%
  select(eri.x, ftm, ol_count, l_count)
head(lowmidkey2) 


############################
#### Add Coordinates ####

lmm <- read.csv("VP/severity_tmp/data/original/FTM/Data/Roccaforte/Roccaforte_LMM_UTM.csv")

head(lmm)
lmmplots <- lmm %>%
  distinct(Plot_ID, UTM_Zone, UTM_Easting_X, UTM_Northing_Y)

## Convert utm coords to lat long
# First, create a SpatVector from your dataframe
lmmvect <- vect(lmmplots, geom=c("UTM_Easting_X", "UTM_Northing_Y"), crs="+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs")

# Now project from UTM to WGS 84 (lat/long)
lmmvect_latlong <- project(lmmvect, "+proj=longlat +datum=WGS84")

# Use the coordinates or geom function to get the longitude and latitude
coords <- geom(lmmvect_latlong)

# Create a dataframe from the extracted coordinates
lmm_latlon <- data.frame(longitude = coords[,3], latitude = coords[,4])

head(lmm_latlon)

lmmplots <- cbind(lmmplots, lmm_latlon)
head(lmmplots)

lmmplots <- merge(lmmplots, lowermidkey, by.x="Plot_ID", by.y="eri")
roccaforte <- merge(roccaforte, lmmplots[, c("ftm", "latitude", "longitude")], by.x="Plot", by.y="ftm", all.x=TRUE)


roccaforte$lat_wgs84 <- ifelse(!is.na(roccaforte$latitude), roccaforte$latitude, roccaforte$lat_wgs84)
roccaforte$lon_wgs84 <- ifelse(!is.na(roccaforte$longitude), roccaforte$longitude, roccaforte$lon_wgs84)

roccaforte$latitude <- NULL
roccaforte$longitude <- NULL


####################################################
################## San Juan ##################
####################################################

## San Juan - some of the MineralEco plots - wildfire burned block 3, control unit in block 4, and some of thin plus burn unit in block 4


# Read the datasets
ftmtrees <- ftmtree[ftmtree$YrFireName == "2014 - San Juan", ]
ftmtrees <- ftmtrees[, c(1:16, 21:24)]
originaleri <- read.csv("VP/severity_tmp/data/original/FTM/Data/Roccaforte/Roccaforte_MineralEco_UTM.csv")
names(originaleri)


# trt-block-plot in ftm
# block-trt-plot in eri
unique(ftmtrees$Plot)
unique(originaleri$Plot_ID)

head(ftmtrees)
head(originaleri)

originaleri$Plot_ID <- paste0(originaleri$Trt, "_", originaleri$Block, "_", originaleri$Plot)


############################
#### Add Coordinates ####

head(originaleri)
eriplots <- originaleri %>%
  distinct(Plot_ID, UTM.Zone, UTM_Easting_X, UTM_Northing_Y)

## Convert utm coords to lat long
# First, create a SpatVector from your dataframe

unique(eriplots$UTM.Zone) ## make sure this matches
eriplotsvect <- vect(eriplots, geom=c("UTM_Easting_X", "UTM_Northing_Y"), crs="+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

# Now project from UTM to WGS 84 (lat/long)
eriplotsvect_latlong <- project(eriplotsvect, "+proj=longlat +datum=WGS84")

# Use the coordinates or geom function to get the longitude and latitude
coords <- geom(eriplotsvect_latlong)

# Create a dataframe from the extracted coordinates
eriplots_latlon <- data.frame(longitude = coords[,3], latitude = coords[,4])

head(eriplots_latlon)

eriplots <- cbind(eriplots, eriplots_latlon)
head(eriplots)

unique(roccaforte[roccaforte$YrFireName=="2014 - San Juan",]$Plot)
unique(eriplots$Plot_ID)
eriplots2 <- eriplots[eriplots$Plot_ID %in% roccaforte[roccaforte$YrFireName=="2014 - San Juan",]$Plot,]
roccaforte <- merge(roccaforte, eriplots[, c("Plot_ID", "latitude", "longitude")], by.x="Plot", by.y="Plot_ID", all.x=TRUE)


roccaforte$lat_wgs84 <- ifelse(!is.na(roccaforte$latitude), roccaforte$latitude, roccaforte$lat_wgs84)
roccaforte$lon_wgs84 <- ifelse(!is.na(roccaforte$longitude), roccaforte$longitude, roccaforte$lon_wgs84)

roccaforte$latitude <- NULL
roccaforte$longitude <- NULL


####################################################
################## Leroux ##################
####################################################

## Leroux - plot IDs are correct; not in the ftm_baloss file because plot size is listed as "0.1 and 0.0.25"
# "As for the Leroux plots, the overstory plots (trees > 15.0 cm dbh) were 20 x 50 m, so 0.1 ha is correct. The “0.0.25” is obviously incorrect, but likely refers to a nested subplot 10 x 25 m (0.025 ha) in size where pole-sized trees (trees ≤ 2.5 cm dbh and ≥ 15.0 cm dbh) were measured. There is also another nested subplot 5 x 10 m (0.005 ha) on each plot where we measured seedlings and saplings (trees < 2.5 cm dbh or < 1.37 in height). I’m not sure if you have pole and/or seedling/sapling data but in case you do, those are the dimensions of the subplots."

#ftmtrees <- ftmtree[ftmtree$YrFireName == "2001 - Leroux", ]


####################################################
################## Mineral ##################
####################################################

## same originaleri data as 2014 San Juan fire above (with more plots)

# Read the datasets
ftmtrees <- ftmtree[ftmtree$YrFireName == "2008 - Mineral Ecosystem Management Area", ]
ftmtrees <- ftmtrees[, c(1:16, 21:24)]
originaleri <- read.csv("VP/severity_tmp/data/original/FTM/Data/Roccaforte/Roccaforte_MineralEco_UTM.csv")
names(originaleri)


# trt-block-plot in ftm
# block-trt-plot in eri
unique(ftmtrees$Plot)
unique(originaleri$Plot_ID)

head(ftmtrees)
head(originaleri)

originaleri$Plot_ID <- paste0(originaleri$Trt, "_", originaleri$Block, "_", originaleri$Plot)


############################
#### Add Coordinates ####

head(originaleri)
eriplots <- originaleri %>%
  distinct(Plot_ID, UTM.Zone, UTM_Easting_X, UTM_Northing_Y)

## Convert utm coords to lat long
# First, create a SpatVector from your dataframe

unique(eriplots$UTM.Zone) ## make sure this matches
eriplotsvect <- vect(eriplots, geom=c("UTM_Easting_X", "UTM_Northing_Y"), crs="+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

# Now project from UTM to WGS 84 (lat/long)
eriplotsvect_latlong <- project(eriplotsvect, "+proj=longlat +datum=WGS84")

# Use the coordinates or geom function to get the longitude and latitude
coords <- geom(eriplotsvect_latlong)

# Create a dataframe from the extracted coordinates
eriplots_latlon <- data.frame(longitude = coords[,3], latitude = coords[,4])

head(eriplots_latlon)

eriplots <- cbind(eriplots, eriplots_latlon)
head(eriplots)

unique(roccaforte[roccaforte$YrFireName=="2008 - Mineral Ecosystem Management Area",]$Plot)
unique(eriplots$Plot_ID)
eriplots2 <- eriplots[eriplots$Plot_ID %in% roccaforte[roccaforte$YrFireName=="2008 - Mineral Ecosystem Management Area",]$Plot,]
roccaforte <- merge(roccaforte, eriplots[, c("Plot_ID", "latitude", "longitude")], by.x="Plot", by.y="Plot_ID", all.x=TRUE)


roccaforte$lat_wgs84 <- ifelse(!is.na(roccaforte$latitude), roccaforte$latitude, roccaforte$lat_wgs84)
roccaforte$lon_wgs84 <- ifelse(!is.na(roccaforte$longitude), roccaforte$longitude, roccaforte$lon_wgs84)

roccaforte$latitude <- NULL
roccaforte$longitude <- NULL




#################
## Add BA loss ##
##################

ftm_baloss <- read.csv("VP/severity_tmp/data/saved/FTM/FTM_ba_v2.csv")
names(ftm_baloss)

unique(ftm_baloss[ftm_baloss$YrFireName %in% unique(roccaforte$YrFireName),]$YrFireName)

roccaforteba <- merge(roccaforte, ftm_baloss[, c("Plot", "YrFireName", "Dataset", "Unit", "ID", "pcnt_ba_mort")], 
                      by = c("Plot", "YrFireName", "Dataset", "Unit", "ID"))

roccaforteba$utm_x <- NULL
roccaforteba$utm_y <- NULL
roccaforteba$utm_zone <- NULL
roccaforteba <- roccaforteba[roccaforteba$lon_wgs84 < -100,] # make sure there aren't any too far east for this
names(roccaforteba)

head(roccaforteba)

roccaforteba$PlotID <- paste0(substr(roccaforteba$YrFireName, 8, 16), roccaforteba$Plot)
roccaforteba$Plot <- NULL

roccaforteba$FireYear <- substr(roccaforteba$YrFireName, 1, 4)


write.csv(roccaforteba, "VP/severity_tmp/data/saved/FTM/plots_with_coords/FTM_plots_Roccaforte_withcoordsba.csv", row.names = FALSE)

duplicates_count_eri <- roccaforteba %>%
  group_by(Dataset, Unit, YrFireName, ID, PlotID) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)
duplicates_count

####################################################################################################
# Code made more generic - keeping just in case (but it's the same as the code above)
# 
# # 1. Identify trees with unique combinations of species, dbh, and height across all plots
# unique_trees <- ftmtrees %>%
#   group_by(Species, DBH_cm, HT_m) %>%
#   filter(n() == 1) %>%
#   ungroup()
# 
# # 2. From the unique combinations, choose one tree from each plot
# tokey <- unique_trees %>%
#   group_by(Plot) %>%
#   slice_head(n = 1) %>%
#   ungroup()
# 
# # 3. Join the two datasets based on the unique combinations
# 
# matched_data1 <- tokey %>%
#   left_join(originaleri, 
#             by = c("Species" = "Sp_code", 
#                    "DBH_cm" = "X2009.DBH_cm", 
#                    "HT_m" = "X2009.TotHt_m"))
# 
# 
# # Check unmatched plots - not really sure why these are here
# unmatched_ftm_plots <- setdiff(unique(tokey$Plot), matched_data1$Plot.x)
# unmatched_eri_plots <- setdiff(unique(originaleri$Plot), matched_data1$Plot.x)
# 
# for(x in unmatched_ftm_plots) {
#   print(x)
#   print(nrow(ftmtrees[ftmtrees$Plot == x,]))
# }
# for(x in unmatched_eri_plots) {
#   print(x)
#   print(nrow(originaleri[originaleri$Plot_ID == x,]))
# }
# 
# # 3_NA_37 and 1-3-17
# # 4-3-19 and 3_NA_159
# duplicated(matched_data1$Plot.x)
# matched_data1 <- as.data.frame(matched_data1)
# matched_data1[matched_data1$Plot=="3_NA_37",]
# ftmtrees[ftmtrees$Plot=="3_NA_37",]
# originaleri[originaleri$Plot.=="1-3-17",]
# 
# matched_data1 <- matched_data1[!(matched_data1$Plot == "3_NA_159" & matched_data1$Plot. == "1-3-17"), ]
# 
# 
# 
# 
# ftmtrees[ftmtrees$Plot=="2_NA_15",]
# originaleri[originaleri$Plot.=="1-2-15",]
# 
# ftmtrees[ftmtrees$Plot=="2_NA_54",]
# originaleri[originaleri$Plot.=="2-2-14",]
# 
# 
# df <- data.frame(
#   eri = c("1-2-8", "1-2-13", "3-2-1", "1-2-15", "2-2-14" ),
#   ftm = c("2_NA_8", "2_NA_13", "2_NA_81", "2_NA_15", "2_NA_54" )
# )
# 
# df1 <- as.data.frame(matched_data1[, c("Plot", "Plot.")])
# names(df1) <- c("ftm", "eri")
# df1 <- df1[complete.cases(df1),]
# 
# keyftmeri <- rbind(df, df1)
# 
# duplicated(keyftmeri$eri)
# write.csv(keyftmeri, "VP/severity_tmp/data/saved/FTM/data_request/Roccaforte_sanjuankey.csv")
# head(keyftmeri)
# 
# # Count rows in originallowmid for each eri value in wallowkey
# originaleri_counts <- originaleri %>%
#   group_by(Plot.) %>%
#   summarise(eri_count = n()) %>%
#   right_join(keyftmeri, by = c("Plot." = "eri"))
# names(originaleri_counts) <- c("eri", "eri_count", "ftm")
# 
# # Count rows in lowmid for each ftm value in wallowkey
# ftmtrees_counts <- ftmtrees %>%
#   group_by(Plot) %>%
#   summarise(ftm_count = n()) %>%
#   right_join(keyftmeri, by = c("Plot" = "ftm"))
# names(ftmtrees_counts) <- c("ftm", "ftm_count", "eri")
# 
# # Add these counts as new columns - the mismatches in numbers are due to ftm only including trees that were alive prefire!
# keyftmeri2 <- left_join(originaleri_counts, ftmtrees_counts, by = "ftm") %>%
#   select(eri.x, ftm, eri_count, ftm_count)
# head(keyftmeri2) 
