
## project area 7
band7 <- read.csv("VP/severity_tmp/data/original/NPS/BAND/BAND-BasalAreaProjectArea7.csv")
unique(band7$year)

# calculate percent ba loss 
band7 <- band7 %>%
  group_by(plot) %>%
  mutate(
    basal_area_pre = basal_area[year == 2009],
    percent_loss = (basal_area_pre - basal_area) / basal_area_pre) %>%
  select(-basal_area_pre)

tail(band7)

## project area 9
band9 <- read.csv("VP/severity_tmp/data/original/NPS/BAND/BAND-BasalAreaProjectArea9.csv")
unique(band9$year)

# calculate percent ba loss 
band9 <- band9 %>%
  group_by(plot) %>%
  mutate(
    basal_area_pre = basal_area[year == 2008],
    percent_loss = (basal_area_pre - basal_area) / basal_area_pre) %>%
  select(-basal_area_pre) 

tail(band9)

## project area 14
band14 <- read.csv("VP/severity_tmp/data/original/NPS/BAND/BAND-BasalAreaProjectArea14.csv")
unique(band14$year)

# calculate percent ba loss 
band14 <- band14 %>%
  group_by(plot) %>%
  mutate(
    basal_area_pre = basal_area[year == 2010],
    percent_loss = (basal_area_pre - basal_area) / basal_area_pre) %>%
  select(-basal_area_pre) 

tail(band14)

## project area 9
band289 <- read.csv("VP/severity_tmp/data/original/NPS/BAND/BAND-BasalAreaProjectArea289.csv")
unique(band289$year)

# calculate percent ba loss 
band289 <- band289 %>%
  group_by(plot) %>%
  mutate(
    basal_area_pre = basal_area[year == 2010],
    percent_loss = (basal_area_pre - basal_area) / basal_area_pre) %>%
  select(-basal_area_pre) 

tail(band289)


## merge together
band <- rbind(band7[band7$year==2012,], band9[band9$year==2012,], band289[band289$year==2012,], band14[band14$year==2016,])

band$pcnt_ba_mort <- ifelse(band$percent_loss < 0, 0, band$percent_loss) #change negatives to 0
band$FireYear <- 2011
band$PlotID1 <- substr(band$plot, 3, 14)
band$PlotID <- paste0(substr(band$plot, 3, 11), substr(band$plot, 13, 14))
band$YrFireName <- "2011 - Las Conchas"
band$Dataset <- "NPS_Bandelier"
band$Unit <- ""
band$ID <- ""

band_formatted <- as.data.frame(band[c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "FireYear", "pcnt_ba_mort")])

## add coords
bandcoords <- read.csv("VP/severity_tmp/data/original/NPS/BAND/BAND-FireEcologyPlots.csv")
names(bandcoords)

# 269 = NAD83, 326=WGS84
bandcoords_sf <- st_as_sf(bandcoords, coords = c("Easting...NAD83", "Northing...NAD83"), crs = paste0("EPSG:269", "13"))
# Reproject to lat long
bandcoords_wgs84 <- st_transform(bandcoords_sf, 4326)

# Extract coordinates into a data frame
bandcoords_wgs84df <- st_coordinates(bandcoords_wgs84)

# Add them back to the original data frame if needed
bandcoords$lat_wgs84 <- bandcoords_wgs84df[, "Y"]
bandcoords$lon_wgs84 <- bandcoords_wgs84df[, "X"]
head(bandcoords)

# check they're at least roughly in the right place
ggplot() +
  geom_sf(data = states) +
  geom_sf(data = bandcoords_wgs84, color = "black", size = 1) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), 
           ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_light()

# Add coords to dataframe
band_formatted_c <- merge(band_formatted, bandcoords[c("Plot.Name", "lat_wgs84", "lon_wgs84")], by.x="PlotID", by.y="Plot.Name")
head(band_formatted_c)

# write data
write.csv(band_formatted_c, "VP/severity_tmp/data/saved/NPS/BAND_coords.csv", row.names = FALSE)

