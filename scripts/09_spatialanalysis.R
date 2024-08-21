
# Merge with original data
ardcoords <- vect("VP/severity_tmp/data/saved/ARD_08132024.gpkg")
full_model_results <- read.csv("VP/severity_tmp/data/saved/outputs/rf_cv5_results.csv")


rfcoords <- merge(ardcoords[, "UniqueID"], full_model_results, by = "UniqueID")
names(rfcoords)

## extract ecoregion
ecoregions <- vect("VP/severity_tmp/data/Ecoregions2017/Ecoregions2017.shp")
ecoregions <- ecoregions[ecoregions$REALM=="Nearctic",]
ecoregions


biome_info <- terra::extract(ecoregions[, "ECO_NAME"], rfcoords)

head(biome_info)
rfcoords <- cbind(rfcoords, biome_info)
head(rfcoords)
table(rfcoords$ECO_NAME)
rfcoords$id.y <- NULL
rfeco <- as.data.frame(rfcoords)

# Calculate R² for each ECO_NAME
r2_table <- rfeco %>%
  group_by(ECO_NAME) %>%
  summarise(R2 = r_squared(obs, pred))
r2_table

ggplot(rfeco) +
  geom_point(aes(x=obs, y=pred), alpha=.6) +
  geom_abline() +
  geom_smooth(aes(x=obs, y=pred)) +
  theme_light() +
  ylim(0, 1) +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss") +
  facet_wrap(~ECO_NAME)
ggsave("VP/severity_tmp/plots/rf_pred_by_ecoregion.png", width = 8, height = 8, units = "in")


## Extract state data
## plot
library("rnaturalearth")
library("rnaturalearthdata")

states <- ne_states(country = "United States of America", returnclass = "sf")
states <- states[,c("region_sub", "woe_name")]
statesvect <- vect(states)

state_info <- terra::extract(statesvect, rfcoords)

head(state_info)
state_info$id.y <- NULL
rfcoords <- cbind(rfcoords, state_info)
rfstate <- as.data.frame(rfcoords)

r2_table_state <- rfstate %>%
  group_by(woe_name) %>%
  summarise(R2 = r_squared(obs, pred))
r2_table_state

ggplot(rfstate) +
  geom_point(aes(x=obs, y=pred), alpha=.6) +
  geom_abline() +
  geom_smooth(aes(x=obs, y=pred)) +
  theme_light() +
  ylim(0, 1) +
  ylab("Predicted BA loss") +
  xlab("Observed BA loss") +
  facet_wrap(~woe_name)
ggsave("VP/severity_tmp/plots/rf_pred_by_state.png", width = 8, height = 8, units = "in")
