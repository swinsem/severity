library(ggplot2)
library(dplyr)
library(stringr)
library(ggcorrplot)
library(terra)
library(glue)

source("./R/utils.R")

data_dir <- "research/severity/data/"

# use the same name at the end of spectral_, climate_, and topo_ in GEE
filename <- "20250804"

### to do:
## remove NPS plots from GEE code
## include fuzzed NPS locations below when making spatial ARD
## remove unit, id, dataset, and yrfirename (others? plotid?) from that file
## make sure the startday format is the same on gee and here. I think startDay and endDay is a nice format

################# Spectral data ##################

spectral <- read.csv(paste0(data_dir, "input/spectral_", filename, ".csv"))
spectral$system.index <- NULL
spectral$.geo <- NULL

################# Terraclimate ##################

climate <- read.csv(paste0(data_dir, "input/climate_", filename, ".csv"))
climate$system.index <- NULL
climate$.geo <- NULL

################# Topography ##################

topo <- read.csv(paste0(data_dir, "input/topo_", filename, ".csv"))
topo$system.index <- NULL
topo$.geo <- NULL


##########################################################
################# Merge datasets ##################
##########################################################
# merge parameters 
allgee <- merge(spectral, climate, by=c("UniqueID", "FireYear", "Fire", "pcnt_ba_mo"))
allgee <- merge(allgee, topo, by=c("UniqueID", "FireYear", "Fire", "pcnt_ba_mo"))

names(allgee)

##########################################################
################# Save data ##################
##########################################################

ard <- allgee[,!names(allgee) %in% c("startDay", "endDay")]

### Write ARD dataframe without coords - write this below with ecoregion
#write.csv(ard, paste0(data_dir, "saved/ARD_", filename, ".csv"), row.names = FALSE)


##### ARD with plot locations and ecoregion #####

# point location data
coords <- terra::vect(paste0(data_dir, "RAVG_test.shp")) 
coords <- vect(paste0(data_dir, "input/allcoords_withbaloss_v8.shp")) 

# combine 
ardcoords <- merge(coords, ard, by=c("UniqueID", "Fire", "FireYear", "pcnt_ba_mo"))

ardcoords

names(ardcoords)

## extract ecoregion
ecoregions <- vect(paste0(data_dir, "input/Ecoregions2017/Ecoregions2017.shp"))
ecoregions <- ecoregions[ecoregions$REALM=="Nearctic",] # subset to run faster

biome_info <- terra::extract(ecoregions[, "ECO_NAME"], ardcoords)

ardcoords$ecoregion <- biome_info$ECO_NAME

# save back to csv
ardnc <- as.data.frame(ardcoords)

# save ARD
writeVector(ardcoords, paste0(data_dir, "saved/ARD_", filename, ".gpkg"))

write.csv(ard, paste0(data_dir, "saved/ARD_", filename, ".csv"), row.names = FALSE)

# Spatially fold ARD
set.seed(20250121)

# Read the ARD data
# https://spatialsample.tidymodels.org/articles/spatialsample.html
ard <- sf::st_read(paste0(data_dir, "saved/ARD_", filename, ".csv")) |>
  dplyr::filter(!is.na(pcnt_ba_mo)) 

# Create the 10 spatial folds of the full dataset
ard_for_task = ard |> 
  spatialsample::spatial_clustering_cv(v = 10)

ard_with_spatial_folds_global = ard_for_task |>
  purrr::pmap(.f = unpack_rsample_splits) |>
  data.table::rbindlist() |>
  dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
  tibble::as_tibble()

ard_with_spatial_folds_global <- list(ard_with_spatial_folds_global) |> 
  setNames("western-us")

# Next set up ecoregion specific models
ecoregions_to_model <- ard |> 
  sf::st_drop_geometry() |> 
  dplyr::group_by(ecoregion) |> 
  dplyr::summarize(n = dplyr::n()) |> 
  dplyr::filter(n >= 10) |> 
  dplyr::pull(ecoregion)

# Doing the grouping in a separate step lets us preserve the group names
# easier
ard_for_task_by_ecoregion_grouped <- ard |>
  dplyr::filter(ecoregion %in% ecoregions_to_model) |> 
  dplyr::group_by(ecoregion)

# Just 5-fold spatial cross validation if we've already subset to individual
# ecoregions otherwise the folds will be too sparse. They are arguably too
# sparse even at 5 folds given that some of those folds within ecoregions
# have no variation in observed basal area loss
ard_for_task_by_ecoregion <- ard_for_task_by_ecoregion_grouped |> 
  dplyr::group_split() |> 
  purrr::map(
    .f = spatialsample::spatial_clustering_cv, 
    v = 5, 
    .progress = TRUE
  ) |> 
  setNames(dplyr::group_keys(ard_for_task_by_ecoregion_grouped)$ecoregion)

ard_with_spatial_folds_by_ecoregion <- ard_for_task_by_ecoregion |>
  purrr::map(
    .f = \(x) {
      out <- purrr::pmap(.l = x, .f = unpack_rsample_splits) |>
        data.table::rbindlist() |>
        dplyr::mutate(spatial_fold = factor(spatial_fold)) |>
        tibble::as_tibble()
      
      out
    }
  )

# Combine the global analysis-ready data with the ecoregion-specific (both
# of which already have their spatial folds set up)
ard_with_spatial_folds <- tibble::tibble(
  ard = c(
    ard_with_spatial_folds_global, 
    ard_with_spatial_folds_by_ecoregion
  ),
  domain = names(ard)
) |> 
  tidyr::unnest(cols = "ard")

ard_with_spatial_folds

readr::write_csv(
  x = ard_with_spatial_folds, 
  here::here(glue::glue("data/ARD_{filename}_with-spatial-folds.csv"))
)
