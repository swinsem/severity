vcoords <- vect("VP/severity_tmp/data/saved/allcoords_withbaloss_v6.shp")

names(vcoords)

img_season <- vect("VP/severity_tmp/data/saved/image-seasons-resolve-ecoregions.gpkg")
img_season <- terra::project(img_season, crs(vcoords))
names(img_season)
head(img_season)
eco <- img_season[, c("eco_name")]

# intersect the plots with the image season polygons to get Start_Day and End_Day 
ecor <- terra::extract(eco, vcoords)
names(ecor)
vcoords$ecoregion <- ecor$eco_name

##
plot_summary3 <- read.csv("VP/severity_tmp/data/saved/FTM/FTM_ba_v2.csv")
names(plot_summary3)
names(vcoords)
names(plot_summary3)[1] <- "PlotID"

vcoords <- merge(vcoords, plot_summary3[,c(1:2,4,6:7,12)], by=c("PlotID", "Unit", "ID", "YrFireName"))

head(vcoords)
basal <- as.data.frame(vcoords)

summary_stats <- basal %>%
  group_by(ecoregion) %>%
  summarise(
    mean_prefireBA = mean(total_BA_ha, na.rm = TRUE),
    sd_prefireBA = sd(total_BA_ha, na.rm = TRUE)
  )
summary_stats
