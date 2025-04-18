`%notin%` <- function(x, table) !(x %in% table)

#############################################################
#################### NPS  DATA ####################  
############################################################

npsba <- read.csv("VP/severity_tmp/data/original/NPS/pre_post_obs_for_sharing/live_basal_area.csv")
tail(npsba)
## will probably need to remove any of the >2 year post-fire plots ()
table(npsba$fire_type)
#0   1   2   5   8   9  10 
#144  65   3   1   1   1   7 

class(npsba$date)
npsba$sampleyear <- as.numeric(substr(npsba$date, 1, 4))
npsba$fireyear <- npsba$sampleyear - npsba$years_since_burn
# Compute basal area per hectare
npsba$basal_area_per_hectare <- npsba$live_basal_area * (10000 / npsba$plot_area_m)
npsba$live_trees_per_hectare <- npsba$live_trees * (10000 / npsba$plot_area_m)


## separate into earlier and later sets
# First, order the dataset by plot_id and sampleyear
npsba$uniqueplot <- paste0(npsba$park, npsba$plot_id)
npsba <- npsba[order(npsba$uniqueplot, npsba$date),]

# Then, create an indexing vector for splitting
split_index <- rep(1:2, nrow(npsba)/2)

# Now, split the data into two data frames based on the indexing vector
df_list <- split(npsba, split_index)
df_earlier <- df_list[[1]]
df_later <- df_list[[2]]


# Rename "burn_class" columns in each data frame
names(df_earlier)[names(df_earlier) == "burn_class"] <- "burn_class_1"
names(df_later)[names(df_later) == "burn_class"] <- "burn_class_2"

# Merge the two data frames by plot_id
df_combined <- merge(df_earlier, df_later, by = "uniqueplot")

# Compute the change in "live_trees" and "live_basal_area"
df_combined$live_trees_ha_change <- df_combined$live_trees_per_hectare.x - df_combined$live_trees_per_hectare.y

df_combined$pcnt_n_mort <- (df_combined$live_trees_ha_change / df_combined$live_trees_per_hectare.x) * 100

df_combined$live_basal_area_change <- df_combined$basal_area_per_hectare.x - df_combined$basal_area_per_hectare.y
df_combined$pcnt_ba_mort <- (df_combined$live_basal_area_change / df_combined$basal_area_per_hectare.x) * 100

names(df_combined)
# Keep only the necessary columns
df_combined <- df_combined[, c("park.y", "plot_id.x", "uniqueplot", "years_since_burn.y", "plot_area_m.y",
                              "fireyear.y", "burn_class_1", "burn_class_2", "fire_type.y",
                              "sampleyear.x", "sampleyear.y",
                              "live_trees_ha_change", "pcnt_n_mort", 
                              "basal_area_per_hectare.x",
                              "live_basal_area_change", "pcnt_ba_mort")]


# Rename the columns to get rid of .y
names(df_combined)[names(df_combined) == "park.y"] <- "park"
names(df_combined)[names(df_combined) == "plot_id.x"] <- "plot_id"
names(df_combined)[names(df_combined) == "sampleyear.x"] <- "sampleyear_1"
names(df_combined)[names(df_combined) == "sampleyear.y"] <- "sampleyear_2"
names(df_combined)[names(df_combined) == "years_since_burn.y"] <- "years_since_burn"
names(df_combined)[names(df_combined) == "plot_area_m.y"] <- "plot_area_m"
names(df_combined)[names(df_combined) == "fireyear.y"] <- "fireyear"
names(df_combined)[names(df_combined) == "fire_type.y"] <- "fire_type"
names(df_combined)[names(df_combined) == "basal_area_per_hectare.x"] <- "prefire_ba_ha"


head(df_combined)

# filter to plots measured 0-2 years after fire, and with initial sampling 5 or fewer years before fire
df_recent <- df_combined[df_combined$years_since_burn <= 2,]

df_recent <- df_recent[(df_recent$fireyear - df_recent$sampleyear_1) < 5,]

df_recent <- df_recent[(df_recent$fireyear - df_recent$sampleyear_1) <= 5,]

## combine with locations
npscoords <- read.csv("VP/severity_tmp/data/original/NPS/pre_post_obs_for_sharing/plot_locations.csv")
head(npscoords)
npscoords$uniqueplot <- paste0(npscoords$park, npscoords$plot_id)
nps_all <- merge(df_recent, npscoords[, c("uniqueplot", "lat", "lon")], by="uniqueplot")

# make spatial
npsplotgeo <- vect(nps_all, geom=c("lon", "lat"), crs="epsg:4326")
head(npsplotgeo)

# export
write.csv(nps_all, "VP/severity_tmp/data/saved/NPS/nps_baloss_coords.csv", row.names = FALSE)
writeVector(npsplotgeo, "VP/severity_tmp/data/saved/NPS/nps_baloss_coords.gpkg", overwrite=TRUE)

nps_all <- read.csv("VP/severity_tmp/data/saved/NPS/nps_baloss_coords.csv")


hist(df_recent$percent_basal_mortality, breaks=30)
npsba[npsba$uniqueplot=="CRLAB:FTSME1BY08:01",]


ggplot(df_recent) +
  geom_point(aes(x=percent_n_mortality, y=percent_basal_mortality))

ggplot(df_recent) +
  geom_violin(aes(y=percent_basal_mortality, x=factor(fire_type))) +
  theme_light() +
  ylab("% BA mortality") +
  xlab("Fire type")

nrow(df_combined[df_combined$percent_basal_mortality < 0,])
moreba <- df_combined[df_combined$percent_basal_mortality < 0,]
nrow(moreba[moreba$percent_n_mortality < 0,])
