library(terra)
library(ggplot2)

data_dir <- "research/severity/data/"
fileap <- "ravg_test" # appended to the outputs from gee, and to the ard export from 01_make_ard


# get the GEE covariates

ravg <- read.csv("research/severity/data/RDS-2022-0018/Data/RAVG_V19_field.csv")
head(ravg)
ravg <- ravg[, c("Plot", "LonDD", "LatDD", "pdBA")]
unique(ravg$Plot)

ravg$FireYear <- NULL
fy2018 <- c("BEAR", "BLUE", "DIEN", "SARD", "TIND", "VENA")

ravg$Fire <- substr(ravg$Plot, 1, 4)
ravg$FireYear <- ifelse(ravg$Fire %in% fy2018, 2018, 2017)

ravg_vect <- terra::vect(ravg,
                  geom = c("LonDD", "LatDD"),
                  crs  = "EPSG:4326",
                  keepgeom = FALSE)
head(ravg_vect)

names(ravg_vect) <- c("UniqueID", "pcnt_ba_mort", "Fire", "FireYear")
terra::writeVector(ravg_vect, "research/severity/data/RAVG_test.shp", overwrite=TRUE)



# load data
# combined in 01_make_ard
ard_new <- read.csv(paste0(data_dir, "saved/ARD_", fileap, ".csv"))

# load model as rds
final_mod <- readRDS(paste0(data_dir, "rf_final_model.rds"))


# run RF model on new data
set.seed(20250711)
preds <- predict(final_mod, data = ard_new)$predictions
results <- cbind(ard_new, pred = preds)

# assess outputs
caret::R2(results$pcnt_ba_mo, results$pred)
library(ggplot2)
ggplot(results) +
  geom_point(aes(x=pcnt_ba_mo, y=rdnbr))+
  geom_smooth(aes(x=pcnt_ba_mo, y=rdnbr)) +
  labs(title="RAVG test data")
ggplot(ard) +
  geom_point(aes(x=pcnt_ba_mo, y=rdnbr)) +
  geom_smooth(aes(x=pcnt_ba_mo, y=rdnbr)) +
  labs(title = "Original dataset")


preds   <- predict(final_mod, data = ard_new)$predictions
results <- cbind(ard_new, pred = preds)
caret::R2(results$pcnt_ba_mo, results$pred)

