## select fires for GEE display and figure
library(terra)
mtbs <- vect("../../VP/severity_tmp/data/mtbs_perimeter_data/mtbs_perims_DD.shp")
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

writeVector(perimsubset, "../../VP/severity_tmp/data/saved/outputs/fireperims.shp", overwrite=TRUE)

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
library(grid)         # for viewport() layout of the final figure

perimsubset <- vect("../../VP/severity_tmp/data/saved/outputs/fireperims.shp")
perimsubsf <- st_as_sf(perimsubset)


rim <- read_stars("figs/BAloss/RIM_pcnt_ba_mo2.tif")
hayman <- read_stars("figs/BAloss/HAYMAN_pcnt_ba_mo2.tif")
archiecreek <- read_stars("figs/BAloss/ARCHIECREEK_pcnt_ba_mo2.tif")
northstar <- read_stars("figs/BAloss/NORTH STAR_pcnt_ba_mo2.tif")
wallow <- read_stars("figs/BAloss/WALLOW_pcnt_ba_mo2.tif")

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
perim_points$xmod <- ifelse(perim_points$FireID == "HAYMAN", -2, 2) # label HAYMAN to the left so it isn't clipped at the map edge

# Load state boundaries
states <- ne_states(country = "United States of America", returnclass = "sf")

# Make the map panel
map_us <- tm_shape(states, bbox = mybbox) +        # Clip to bounding box
  tm_polygons(col = "white", border.col = "grey30", lwd = 0.2) +
  tm_shape(perim_points) +
  tm_symbols(size = .25, col = "black") +            # Adjust size as needed
  tm_text("FireID", size = 0.9, ymod = -1, xmod = "xmod") +      # Smaller text, slight vertical offset
  tm_layout(frame = FALSE)                        # Hide any bounding frame
map_us


# Function to map each fire; sb_breaks sets the scale bar, bottom_margin pushes the fire up off the scale bar (fraction of panel height)
make_fire_map <- function(spatRaster, title, sb_breaks = c(0, 15), north_arrow = FALSE, bottom_margin = 0) {
  m <- tm_shape(spatRaster) +
    tm_raster(
      palette = "-RdBu",       # reversed Red‐Blue
      style   = "cont",
      breaks  = seq(0, 1, 0.1),
      legend.show = FALSE      # unified into one separate legend panel
    ) +
    tm_layout(
      main.title = title,
      main.title.size = 1.3,
      frame = FALSE, # no bounding box
      asp = 0, # fill the viewport so titles align across panels instead of hugging each map frame
      outer.margins = c(0.01, 0.01, 0.01, 0.01),
      inner.margins = c(bottom_margin, 0, 0, 0)
    ) +
    tm_scale_bar(position = c("left","bottom"), breaks = sb_breaks, text.size = .9)
  if (north_arrow) {
    m <- m + tm_compass(type = "arrow", position = c("right","top"), size = 3, text.size = 1)
  }
  m
}

map_archiecreek <- make_fire_map(archiecreek, "2020 Archie Creek", north_arrow = TRUE)
map_hayman      <- make_fire_map(hayman,      "2002 Hayman")
map_northstar   <- make_fire_map(northstar,   "2015 North Star")
map_rim         <- make_fire_map(rim,         "2013 Rim")
map_wallow      <- make_fire_map(wallow,      "2011 Wallow", sb_breaks = c(0, 30), bottom_margin = 0.12)

# Shared legend as its own panel so it doesn't shrink the Archie Creek map
legend_panel <- tm_shape(archiecreek) +
  tm_raster(
    palette = "-RdBu",
    style   = "cont",
    breaks  = seq(0, 1, 0.1),
    legend.reverse = TRUE,          # 1 at the top of the ramp
    title = "% BA loss"
  ) +
  tm_layout(
    legend.only = TRUE,
    legend.text.size = 1,
    legend.title.size = 1.3,
    legend.position = c("left", "center"),
    legend.format = list(fun = function(x) {
      ifelse(x %in% c(0.0, 0.5, 1), x, "")
    })
  )

# Draw panels into a custom grid (instead of tmap_arrange) so Archie Creek can take most of the top row, with the US map and legend in narrower columns beside it
draw_fig5 <- function() {
  grid.newpage()
  print(map_us,          vp = viewport(x = 0.17,  y = 0.86,  width = 0.34, height = 0.28))
  print(map_archiecreek, vp = viewport(x = 0.585, y = 0.86,  width = 0.49, height = 0.28))
  print(legend_panel,    vp = viewport(x = 0.915, y = 0.86,  width = 0.17, height = 0.28))
  print(map_hayman,      vp = viewport(x = 0.25,  y = 0.535, width = 0.50, height = 0.37))
  print(map_northstar,   vp = viewport(x = 0.75,  y = 0.535, width = 0.50, height = 0.37))
  print(map_rim,         vp = viewport(x = 0.28,  y = 0.175, width = 0.56, height = 0.35)) # wider panel: Rim is a wide fire, Wallow doesn't need the full half row
  print(map_wallow,      vp = viewport(x = 0.78,  y = 0.175, width = 0.44, height = 0.35))
}
draw_fig5()

png("figs/Fig5_fireoutputs.png", width = 8, height = 13, units = "in", res = 300)
draw_fig5()
dev.off()
