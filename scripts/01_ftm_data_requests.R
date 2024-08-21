## This script is for separating out the FTM database into separate chunks so I can keep track of what coordinates I'm requesting and which ones have come in.
## The main FTM files are read from s3/local (s3 was SO slow for one file) and outputs are saved locally in a "saved/data_request" folder. These are then emailed to folks when requesting data


library(aws.s3)
# aws was taking a really really long time - I think my vpn doesn't agree with s3?

## load Alina Cansler's fire and tree mortality database (FTM)
bucket_name="tmp-sara"
object_name= paste0("severity/RDS-2020-0001-2/Data/FTM_trees.csv")
#ftmtree <- s3read_using(FUN = read.csv, bucket = bucket_name, object = object_name)
ftmtree <- read.csv("VP/severity_tmp/data/original/FTM/Data/FTM_trees2.csv") 

head(ftmtree)
names(ftmtree)

# load the fires data (loads from s3 easily since it's smaller)
object_namef= paste0("severity/RDS-2020-0001-2/Data/FTM_fires.csv")
ftmfire <- s3read_using(FUN = read.csv, bucket = bucket_name, object = object_namef)
head(ftmfire)

# remove fires with no plot size or min tree size
ftmfire_comp <- ftmfire[!is.na(ftmfire$Plot_size_ha),]
ftmfire_comp <- ftmfire_comp[!is.na(ftmfire_comp$Threshold_diameter_cm),] # doesn't remove any more


################################################################
############ CREATE TEMPLATE FILES TO REQUEST DATA ############  
###############################################################

#### Single name dataset ####
get_template <- function(name) {
  ftmfire_ <- ftmfire_comp[ftmfire_comp$Dataset==name,]
  ftmtree_ <- ftmtree[ftmtree$Dataset==name,]
  print(unique(ftmtree_$YrFireName))
  
  ftmplot_ <- unique(ftmtree_[, c("YrFireName", "Dataset", "Plot", "Unit", "ID")])
  
  ftmplot_$lat_wgs84 <- NA
  ftmplot_$lon_wgs84 <- NA
  ftmplot_$utm_x <- NA
  ftmplot_$utm_y <- NA
  ftmplot_$utm_zone <- NA
  write.csv(ftmplot_, paste0("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_", name, ".csv"), row.names = FALSE)
  return(ftmplot_)
}

name <- "Vaillant" # "Roccaforte"
assign(paste0("ftmplot_", name), get_template(name))

#### Multiple names for one person (i.e. Brian Harvey, Sharon Hood) ####
get_template_multiple <- function(filter_names, output_name) {
  ftmfire_ <- ftmfire_comp[ftmfire_comp$Dataset %in% filter_names,]
  ftmtree_ <- ftmtree[ftmtree$Dataset %in% filter_names,]
  print(unique(ftmtree_$YrFireName))
  
  ftmplot_ <- unique(ftmtree_[, c("YrFireName", "Dataset", "Plot", "Unit", "ID")])
  
  ftmplot_$lat_wgs84 <- NA
  ftmplot_$lon_wgs84 <- NA
  ftmplot_$utm_x <- NA
  ftmplot_$utm_y <- NA
  ftmplot_$utm_zone <- NA
  
  write.csv(ftmplot_, paste0("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_", output_name, ".csv"), row.names = FALSE)
  
  return(ftmplot_)
}

# specify names
#filter_names <- c("Harvey", "Agne", "Andrus")
#output_name <- "Harvey"
filter_names <- c("Hood", "Breece", "Cluck", "Davis", "Hood_Idaho", "Lerch", "Progar")
output_name <- "Hood"
assign(paste0("ftmplot_", output_name), get_template_multiple(filter_names, output_name))

# sanity check
ftmplot_test <- read.csv("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_Roccaforte.csv")
# this uses ftmfire_comp from 00_prelim.R to get the number of fires (5) and locations of those fires to make map in 04_combine_coords.R
erifires <- ftmfire_comp[ftmfire_comp$YrFireName %in% ftmplot_test$YrFireName,]
sferi <- st_as_sf(x = erifires,                         
                  coords = c("Longitude", "Latitude"),
                  crs = "epsg:4326")

## check battaglia
ftmplot_test <- read.csv("VP/severity_tmp/data/saved/FTM/data_request/FTM_plots_Battaglia.csv")
head(ftmplot_test)
battagliatree <- ftmtree_comp[ftmtree_comp$YrFireName %in% unique(ftmplot_test$YrFireName),]
battagliafire <- ftmfire_comp2[ftmfire_comp2$YrFireName %in% unique(ftmplot_test$YrFireName),]
## maybe not to follow up on - it's prescribed fire, 590 plots 
