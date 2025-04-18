
## Jay Miller's data

#miller <- read.csv("VP/severity_tmp/data/original/JayMiller/fielddata_2002-2005.csv")
miller <- read.csv("VP/severity_tmp/data/original/JayMiller/fielddata_2002-2005_with_cbi_ba_cc_values.csv")

head(miller)

miller <- miller[, c(1:8, 14)] # keep only the FIELD BA change measure, not the calibrated or FVS scores

miller10 <- miller[!is.na(miller$UTMXZ10),]
miller11 <- miller[!is.na(miller$UTMXZ11),]



miller10$utm_zone <- "10"
miller11$utm_zone <- "11"

library(sf) 
# 326 = WGS84, 269 = NAD83, 267 = NAD27 (what the data are in)
df_sf10 <- st_as_sf(miller10, coords = c("UTMXZ10", "UTMYZ10"), crs = paste0("EPSG:267", miller10$utm_zone[1]))
df_sf11 <- st_as_sf(miller11, coords = c("UTMXZ11", "UTMYZ11"), crs = paste0("EPSG:267", miller11$utm_zone[1]))

# Reproject to lat long
df_sf10_wgs84 <- st_transform(df_sf10, 4326)
df_sf11_wgs84 <- st_transform(df_sf11, 4326)

# Extract coordinates into a data frame
coords10_wgs84 <- st_coordinates(df_sf10_wgs84)
coords11_wgs84 <- st_coordinates(df_sf11_wgs84)

# Add them back to the original data frame if needed
miller10$latitude <- coords10_wgs84[, "Y"]
miller10$longitude <- coords10_wgs84[, "X"]
miller11$latitude <- coords11_wgs84[, "Y"]
miller11$longitude <- coords11_wgs84[, "X"]

head(miller10)

millerll <- rbind(miller10, miller11)

names(millerll)
#millerll <- millerll[, c(1:3, 12:14, 19, 21, 24:25)]

unique(millerll$drop)
millerll <- millerll[millerll$drop != "x", ]

head(millerll)

millerll$YrFireName <- paste0(millerll$FireName, " - ", millerll$FireYear)
millerll$Dataset <- "JayMiller"
millerll$PlotID <- millerll$PCOrdID
millerll$Unit <- NA
millerll$ID <- NA

names(millerll)

millerllsub <- millerll[, c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "FireYear", "latitude", "longitude", "PctBAchange")]
names(millerllsub) <- c("PlotID", "YrFireName", "Dataset", "Unit", "ID", "FireYear", "lat_wgs84", "lon_wgs84", "pcnt_ba_mort")
millerllsub <- millerllsub[!is.na(millerllsub$pcnt_ba_mort),]
millerllsub$pcnt_ba_mort <- millerllsub$pcnt_ba_mort/100

head(millerllsub)
hist(millerllsub$pcnt_ba_mort)

write.csv(millerllsub, "VP/severity_tmp/data/saved/JayMiller/JayMiller_withcoordsba_rev.csv", row.names = FALSE)

