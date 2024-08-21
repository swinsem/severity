library(ggplot2)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(terra)


#imgseason <- vect("VP/severity_tmp/data/saved/image-seasons-resolve-ecoregions.gpkg")
#writeVector(imgseason, "VP/severity_tmp/data/saved/image-seasons-resolve-ecoregions.shp")

##########################################################
################# Indices ##################
##########################################################

indbi <- read.csv("VP/severity_tmp/data/saved/GEE/VIs_bilinear_08122024.csv")

names(indbi)
indbi$system.index <- NULL
indbi$.geo <- NULL
head(indbi)

#indbi[] <- lapply(indbi, function(x) if(is.character(x)) na_if(x, "") else x)

ggplot(indbi, aes(x=dmirbi, y=pcnt_ba_mo)) +
  geom_point(aes(color=YrFireName)) +
  geom_smooth() +
  facet_wrap(~Dataset)+
  theme_light() +
  theme(legend.position="none")

cor(indbi$rbr, indbi$pcnt_ba_mo)


##########################################################
################# Bands ##################
##########################################################

# bands
bands <- read.csv("VP/severity_tmp/data/saved/GEE/bands_bilinear_08122024.csv")

nrow(unique(bands[, c("PlotID", "YrFireName")]))
bands$system.index <- NULL
bands$.geo <- NULL


bands$dBlue = bands$Blue_pre - bands$Blue_post
bands$dGreen = bands$Green_pre - bands$Green_post
bands$dRed = bands$Red_pre - bands$Red_post
bands$dNIR = bands$NIR_pre - bands$NIR_post
bands$dSWIR1 = bands$SWIR1_pre - bands$SWIR1_post
bands$dSWIR2 = bands$SWIR2_pre - bands$SWIR2_post
bands$SWIR1.NIR_pre = bands$SWIR1_pre/bands$NIR_pre
bands$SWIR1.NIR_post = bands$SWIR1_post/bands$NIR_post
bands$SWIR2.NIR_pre = bands$SWIR2_pre/bands$NIR_pre
bands$SWIR2.NIR_post = bands$SWIR2_post/bands$NIR_post
bands$SWIR2.SWIR1_pre = bands$SWIR2_pre/bands$SWIR1_pre
bands$SWIR2.SWIR1_post = bands$SWIR2_post/bands$SWIR1_post

bands$dSWIR1.NIR = bands$SWIR1.NIR_pre - bands$SWIR1.NIR_post
bands$dSWIR2.NIR = bands$SWIR2.NIR_pre - bands$SWIR2.NIR_post
bands$dSWIR2.SWIR1 = bands$SWIR2.SWIR1_pre - bands$SWIR2.SWIR1_post

# remove individual timestamp bands and the prefire ratios
names(bands)
bands <- bands[, -c(1:2, 6:7, 9:10, 12:17, 28, 30, 32)] 

# check relationships - not that strong
ggplot(bands, aes(x=dSWIR2, y=pcnt_ba_mo)) +
  geom_point(aes(color=YrFireName)) +
  geom_smooth() +
  facet_wrap(~Dataset)+
  theme_light() +
  theme(legend.position="none")

head(bands)


##########################################################
################# Terraclimate ##################
##########################################################
climate <- read.csv("VP/severity_tmp/data/saved/GEE/terraclimate_08122024.csv")

climate$system.index <- NULL
climate$.geo <- NULL

## Z-scores

climate$zScorePrecip0 <- (climate$precip0 - climate$meanPrecip)/climate$stdDevPrecipValue
climate$zScoreVPD0 <- (climate$vpd0 - climate$meanVPD)/climate$stdDevVPDValue
climate$zScoreAET0 <- (climate$aet0 - climate$meanAET)/climate$stdDevAETValue
climate$zScoreCWD0 <- (climate$cwd0 - climate$meanCWD)/climate$stdDevCWDValue  
climate$zScorePrecip1 <- (climate$precip1 - climate$meanPrecip)/climate$stdDevPrecipValue  
climate$zScoreAET1 <- (climate$aet1 - climate$meanAET)/climate$stdDevAETValue
climate$zScoreCWD1 <- (climate$cwd1 - climate$meanCWD)/climate$stdDevCWDValue  

climate <- climate[,!names(climate) %in% c("precip0", "precip1", "vpd0", "aet0", "aet1", "cwd0", "cwd1", "stdDevAETValue", "stdDevCWDValue", "stdDevPrecipValue", "stdDevVPDValue")]

ggplot(climate, aes(x=zScorePrecip0, y=zScoreVPD0)) +
  geom_point(alpha=.1) +
  ylab("Precipitation - z score") +
  xlab("VPD - z score") +
  theme_light()

# ggplot(alldata, aes(x=meanVPD, y=vpd0, color=pcnt_ba_mo)) +
#   geom_point() +
#   scale_color_viridis_c(option = "turbo") +
#   ylab("VPD - summer of fire") +
#   xlab("VPD - 30 year mean summer") +
#   labs(color = "BA loss") +
#   theme_light()

##########################################################
################# Topography ##################
##########################################################

topo <- read.csv("VP/severity_tmp/data/saved/GEE/zs_topo_08122024.csv")
topo$system.index <- NULL
topo$.geo <- NULL

topo$aspect <- NULL


##########################################################
################# Merge datasets ##################
##########################################################


alldata <- merge(indbi, bands, by=c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo"))

alldata <- merge(alldata, climate, by=c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo"))

alldata <- merge(alldata, topo, by=c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "ID", "YrFireName", "pcnt_ba_mo"))

names(alldata)

#alldata$pcnt_ba_mo <- signif(alldata$pcnt_ba_mo, digits = 5)

# are there duplicates??
duplicates_count <- alldata %>%
  group_by(Dataset, Unit, YrFireName, ID, PlotID) %>%
  summarise(n = n(), .groups = 'drop') %>%
  filter(n > 1)
duplicates_count
## To get the total number of duplicated rows
#total_duplicated_rows <- sum(duplicates_count$n) - nrow(duplicates_count)
#total_duplicated_rows


##########################################################
################# Save data ##################
##########################################################

##### Dataframe

alldata$UniqueID <- paste0(alldata$Dataset, "_", alldata$PlotID)
alldata[alldata$Dataset=="Hood",]$UniqueID <- paste0(alldata[alldata$Dataset=="Hood",]$UniqueID, "_", substring(alldata[alldata$Dataset=="Hood",]$YrFireName, 8))
alldata[alldata$Dataset=="Davis",]$UniqueID <- paste0(alldata[alldata$Dataset=="Davis",]$UniqueID, "_", substring(alldata[alldata$Dataset=="Davis",]$YrFireName, 8))

length(unique(alldata$UniqueID))
nrow(alldata)

# if there's a mismatch, look at the data to see how to make the UniqueID column truly unique
#duplicated_rows <- alldata[duplicated(alldata$UniqueID) | duplicated(alldata$UniqueID, fromLast = TRUE), ]

ard <- alldata[,!names(alldata) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID")]

### Write ARD dataframe without coords
write.csv(ard, "VP/severity_tmp/data/saved/ARD_nocoords.csv", row.names = FALSE)


##### ARD with plot locations

# point location data
coords <- vect("VP/severity_tmp/data/saved/allcoords_withbaloss_v5.shp")

# combine 
names(coords)
ardcoords <- merge(coords, ard, by=c("PlotID", "YrFireName", "Dataset", "pcnt_ba_mo"))
ardcoords

names(ardcoords)

ardcoords <- ardcoords[,!names(ardcoords) %in% c("FireYear", "Start_Day", "End_Day", "Unit", "ID")]

# save ARD
writeVector(ardcoords, "VP/severity_tmp/data/saved/ARD_08132024.gpkg")

ard[ard$PlotID=="JOLL3",]

##########################################################
################# Old code ##################
##########################################################

# ind1 <- read.csv("VP/severity_tmp/data/saved/GEE/zs_VIs_03202024_pt1.csv")
# ind2 <- read.csv("VP/severity_tmp/data/saved/GEE/zs_VIs_03202024_pt2.csv")
# ind <- rbind(ind1, ind2)
# ind[ind$Dataset=="NPS",]$pcnt_ba_mo <- ind[ind$Dataset=="NPS",]$pcnt_ba_mo/100
# ind$pcnt_ba_mo <- ifelse(ind$pcnt_ba_mo <= 0, 0, ind$pcnt_ba_mo)
# ind <- ind[ind$Dataset!="JayMiller",]
#sdind <- read.csv("VP/severity_tmp/data/saved/GEE/zs_SD_NDVI_110123.csv") # not working!! 




##### get mean of duplicate rows #####
# ind_summary <- ind %>%
#   group_by(Dataset, Unit, YrFireName, ID, PlotID, FireYear) %>%
#   summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')
# ind_summary$Unit[ind_summary$Unit == ""] <- NA
# ind_summary$ID[ind_summary$ID == ""] <- NA
# head(ind_summary)
# ind_summary[] <- lapply(ind_summary, function(x) if(is.character(x)) na_if(x, "") else x)
#ind_summary[ind_summary$Dataset=="NPS",]$pcnt_ba_mo <- ind_summary[ind_summary$Dataset=="NPS",]$pcnt_ba_mo/100
#ind_summary$pcnt_ba_mo <- ifelse(ind_summary$pcnt_ba_mo <= 0, 0, ind_summary$pcnt_ba_mo)

# bands_summary <- bands %>%
#   group_by(Dataset, Unit, YrFireName, ID, PlotID, FireYear) %>%
#   summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')
# head(bands_summary)
# bands_summary[] <- lapply(bands_summary, function(x) if(is.character(x)) na_if(x, "") else x)

# when substituting indbi for ind_summary, there are fewer plots because Jay's were removed; also though, NPS plots were removed due to renaming of plotID. Need to rerun terraclim and topo with v5
#alldata <- merge(ind, gridmet, by=c("PlotID", "Dataset", "FireYear", "Start_Day", "End_Day", "Unit", "YrFireName", "pcnt_ba_mo"))




# Rename columns for gridmet and terraclimate
#alldata <- alldata %>% 
#  rename_with(~str_replace(., "\\.x$", "_gm"), ends_with(".x")) %>%
#  rename_with(~str_replace(., "\\.y$", "_tc"), ends_with(".y"))