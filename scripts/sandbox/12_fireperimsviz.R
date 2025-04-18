## select fires for GEE display and figure
library(terra)
mtbs <- vect("VP/severity_tmp/data/mtbs_perimeter_data/mtbs_perims_DD.shp")
names(mtbs)

firelist <- c("RIM", "DIXIE", "EAGLE CREEK", "WALLOW", "NORTH STAR", "EAST TROUBLESOME", "ARCHIE CREEK", "HAYMAN")
perims <- mtbs[mtbs$Incid_Name %in% firelist,]
perims
head(perims)
head(perims[perims$Incid_Name=="RIM",])

rim <- perims[perims$Incid_Name=="RIM" & perims$BurnBndAc==257084,]
#dixie <- perims[perims$Incid_Name=="DIXIE" & perims$Ig_Date=="2021-07-14",]
#eaglecreek <- perims[perims$Incid_Name=="EAGLE CREEK" & perims$Ig_Date=="2017-09-02",]
wallow <- perims[perims$Incid_Name=="WALLOW" & perims$BurnBndAc==563664,]
northstar <- perims[perims$Incid_Name=="NORTH STAR",]
easttroublesome <- perims[perims$Incid_Name=="EAST TROUBLESOME",]
archiecreek <- perims[perims$Incid_Name=="ARCHIE CREEK",]
hayman <- perims[perims$Incid_Name=="HAYMAN",]

perimsub <- rbind(archiecreek, hayman, northstar, rim, wallow) # removed dixie

head(perimsub)

perimsubset <- perimsub[, c("Incid_Name", "Ig_Date")]
perimsubset$FireYear <- substr(perimsubset$Ig_Date, 0, 4)
head(perimsubset)

perimsubset$Ig_Date <- NULL
names(perimsubset) <- c("FireID", "FireYear")

writeVector(perimsubset, "VP/severity_tmp/data/saved/outputs/fireperims.shp", overwrite=TRUE)

########################################################################
#################### output visualization #############################
########################################################################
library(terra)        
library(ggplot2)
library(ggspatial)    # for annotation_scale() (scale bars)
library(rnaturalearth)# for a quick base map of the US
library(scales)
library(sf)           
library(tmap)
library(stars)
library(dplyr)

perimsubet <- vect("VP/severity_tmp/data/saved/outputs/fireperims.shp")
perimsubsf <- st_as_sf(perimsubset)


rim <- read_stars("VP/severity_tmp/data/saved/outputs/rasters/RIM_pcnt_ba_mo.tif")
hayman <- read_stars("VP/severity_tmp/data/saved/outputs/rasters/HAYMAN_pcnt_ba_mo.tif")
archiecreek <- read_stars("VP/severity_tmp/data/saved/outputs/rasters/ARCHIECREEK_pcnt_ba_mo.tif")
northstar <- read_stars("VP/severity_tmp/data/saved/outputs/rasters/NORTHSTAR_pcnt_ba_mo.tif")
wallow <- read_stars("VP/severity_tmp/data/saved/outputs/rasters/WALLOW_pcnt_ba_mo.tif")

# Create an sf geometry for the bounding box
bbox <- st_bbox(perimsubsf)
bbox["xmin"] <- -124
bbox["ymin"] <- 30
bbox["xmax"] <- -100
bbox["ymax"] <- 49

mybbox <- st_as_sfc(st_bbox(c(xmin = -124, ymin = 30, xmax = -100, ymax = 49)),
                    crs = 4326)

# Union each fire (multiple polygons for hayman)
perim_merged <- perimsubsf %>%
  group_by(FireID) %>%
  summarize(geometry = st_union(geometry)) 

# Create label points
perim_points <- st_centroid(perim_merged)

# Load state boundaries
states <- ne_states(country = "United States of America", returnclass = "sf")

# Make the map panel
map_us <- tm_shape(states, bbox = mybbox) +        # Clip to bounding box
  tm_polygons(col = "white", border.col = "grey30", lwd = 0.2) +
  tm_shape(perim_points) +
  tm_symbols(size = .25, col = "black") +            # Adjust size as needed
  tm_text("FireID", size = 0.7, ymod = -1,xmod=2) +      # Smaller text, slight vertical offset
  tm_layout(frame = FALSE)                        # Hide any bounding frame
map_us


# Function to map each fire
make_fire_map <- function(spatRaster, title) {
  tm_shape(spatRaster) +
    tm_raster(
      palette = "-RdBu",       # reversed Red‐Blue
      style   = "cont",#"fixed",
      breaks  = NULL,#seq(0, 1, 0.1),
      legend.show = FALSE,      # want to unify into one legend
      title = "% BA loss"            # label for the color ramp
    ) +
    tm_layout(
      main.title = title,
      main.title.size = .75,
      frame = FALSE # no bounding box
    ) +
    tm_scale_bar(position = c("left","bottom"), breaks = c(0,15), text.size=.7) # tm_scalebar(breaks = c(0, 5, 10), text.size = 1)
}
# One with legend
make_fire_map_leg <- function(spatRaster, title) {
  tm_shape(spatRaster) +
    tm_raster(
      palette = "-RdBu",       # reversed Red‐Blue
      style   = "cont",#"fixed",
      breaks  = seq(0, 1, 0.1),
      legend.show = TRUE,      # want to unify into one legend
      title = "% BA loss"            # label for the color ramp
    ) +
    tm_layout(
      main.title = title,
      main.title.size = .75,
      frame = FALSE, # no bounding box
      legend.outside = TRUE,legend.title.size = 1,
      legend.format = list(fun = function(x) {
        ifelse(x %in% c(0.0, 0.5, 1), x, "")
      })
    ) +
    tm_scale_bar(position = c("left","bottom"), breaks = c(0,15), text.size=.7) # tm_scalebar(breaks = c(0, 5, 10), text.size = 1)
}
# Different scale bar for Wallow
make_fire_map_wal <- function(spatRaster, title) {
  tm_shape(spatRaster) +
    tm_raster(
      palette = "-RdBu",       # reversed Red‐Blue
      style   = "cont",#"fixed",
      breaks  = NULL,#seq(0, 1, 0.1),
      legend.show = FALSE,      # want to unify into one legend
      title = "% BA loss"            # label for the color ramp
    ) +
    tm_layout(
      main.title = title,
      main.title.size = .75,
      frame = FALSE # no bounding box
    ) +
    tm_scale_bar(position = c("left","top"), breaks = c(0,30), text.size=.7) 
}


map_archiecreek <- make_fire_map_leg(archiecreek, "2020 Archie Creek")
map_hayman      <- make_fire_map(hayman,       "2002 Hayman")
map_northstar   <- make_fire_map(northstar,    "2015 North Star")
map_rim         <- make_fire_map(rim,          "2013 Rim")
map_wallow      <- make_fire_map_wal(wallow,       "2011 Wallow")


final_figure <- tmap_arrange(
  map_us,
  map_archiecreek,
  map_hayman,
  map_northstar,
  map_rim,
  map_wallow,
  ncol = 2,
  nrow = 3
)
final_figure

tmap_save(final_figure, "VP/severity_tmp/plots/Fig5_fireoutputs.png", dpi=300, width = 10, height=7, units="in")
