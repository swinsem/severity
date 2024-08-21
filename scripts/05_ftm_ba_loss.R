

#############################################################
#################### % BA mortality ####################  
############################################################


library(dplyr)
library(tidyr)
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

ftmfirehood <- ftmfire[ftmfire$Dataset %in% c("Hood", "Breece", "Cluck", "Davis", "Hood_Idaho", "Lerch", "Progar"),]

# remove fires with no plot size or min tree size, get only fixed area plots, and later than 1992 (could adjust that)
ftmfire_comp <- ftmfire[!is.na(ftmfire$Plot_size_ha),]
ftmfire_comp <- ftmfire_comp[!is.na(ftmfire_comp$Threshold_diameter_cm),] # doesn't remove any more
ftmfire_comp2 <- ftmfire_comp[ftmfire_comp$Sample_design %in% c("Fixed-area plot"),]
ftmfire_comp2 <- ftmfire_comp2[ftmfire_comp2$yr_fire >=1992,]
#ftmfire_variable <- ftmfire_comp[ftmfire_comp$Sample_design %in% c("Variable radius plot"),]


ftmtree_comp <- ftmtree[ftmtree$YrFireName %in% ftmfire_comp2$YrFireName,]
ftmtree_comp$BA <- pi*(ftmtree_comp$DBH_cm/200)^2
ftmtree_comp <- merge(ftmtree_comp, ftmfire_comp2[, c("YrFireName", "Plot_size_ha")], by="YrFireName")


library(dplyr)

# Group by the desired columns
plot_summary <- ftmtree_comp %>%
  group_by(Plot, YrFireName, Dataset, Unit, Times_burned, ID, Plot_size_ha) %>% # add Unit and Dataset 8/20
  summarise(
    # Calculate the proportion basal area (BA) for each yr_status column
    yr0status_prop = sum(BA[yr0status == 1], na.rm = TRUE) / sum(BA[yr0status %in% c(0, 1)], na.rm = TRUE),
    yr1status_prop = sum(BA[yr1status == 1], na.rm = TRUE) / sum(BA[yr1status %in% c(0, 1)], na.rm = TRUE),
    yr2status_prop = sum(BA[yr2status == 1], na.rm = TRUE) / sum(BA[yr2status %in% c(0, 1)], na.rm = TRUE),
    yr3status_prop = sum(BA[yr3status == 1], na.rm = TRUE) / sum(BA[yr3status %in% c(0, 1)], na.rm = TRUE),
    
    # Calculate the total BA, mean DBH_cm, and mean HT_m
    total_BA_ha = sum(BA, na.rm = TRUE),
    # see note below about these being incorrect (commented out for now)
    #dead_BA_ha = sum(BA[yr0status == 1 | yr1status == 1 | yr2status == 1], na.rm = TRUE),
    #live_BA_ha = sum(BA[yr0status == 0 | yr1status == 0 | yr2status == 0], na.rm = TRUE),
    mean_DBH_cm = mean(DBH_cm, na.rm = TRUE),
    mean_HT_m = mean(HT_m, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Filtering out rows where all three specified columns are NA
  filter(
    !is.na(yr0status_prop) | 
      !is.na(yr1status_prop) | 
      !is.na(yr2status_prop) |
      !is.na(yr3status_prop)
  )

## One set of plots (Lerch dataset, possibly will get from Sharon Hood) had different plot sizes for PIPO vs PICO plots - so I determined dominant species based on BA and assigned the appropriate plot size

# Group by Plot and Genus_species and calculate sum of BA
dominant_species <- ftmtree_comp %>%
  group_by(Plot, YrFireName, Times_burned, ID, Genus_species) %>%
  summarise(total_BA = sum(BA, na.rm = TRUE)) %>%
  ungroup()

# Identify the dominant species for each plot
dominant_species <- dominant_species %>%
  group_by(Plot, YrFireName, Times_burned, ID) %>%
  arrange(desc(total_BA)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Join back to the original plot-level summary data
plot_summary2 <- plot_summary %>%
  left_join(dominant_species, by = c("Plot", "YrFireName", "Times_burned", "ID"))
# Rename the Genus_species column to something descriptive like "dominant_species"
plot_summary2 <- plot_summary2 %>%
  rename(dominant_species = Genus_species)

plot_summary2$Plot_size_ha[plot_summary2$Plot_size_ha=="lodgepole plots 0.02 ha and ponderosa plots 0.04 ha" & plot_summary2$dominant_species=="Pinus_ponderosa"] <- "0.04"
plot_summary2$Plot_size_ha[plot_summary2$Plot_size_ha=="lodgepole plots 0.02 ha and ponderosa plots 0.04 ha" & plot_summary2$dominant_species=="Pinus_contorta"] <- "0.02"

# Tenderfoot plots - adjusting based on Unit - Table 2 in https://www.fs.usda.gov/rm/pubs/rmrs_gtr294.pdf
plot_summary2$Plot_size_ha[plot_summary2$Plot_size_ha=="0.04 for even retention units; 0.027 for group retention units" & plot_summary2$Unit %in% c(1, 3, 10, 13)] <- "0.04"
plot_summary2$Plot_size_ha[plot_summary2$Plot_size_ha=="0.04 for even retention units; 0.027 for group retention units" & plot_summary2$Unit %in% c(2, 4, 12, 16)] <- "0.027"

# Add Mussigbrod plot distinctions

# remove multiple size plots fires
`%notin%` <- function(x, table) !(x %in% table)
unique(plot_summary2$Plot_size_ha)
plot_summary3 <- plot_summary2[plot_summary2$Plot_size_ha %notin% c("0.04 and 0.09", "0.08, 0.04", "0.06; three 0.2 ha subplots", "1 and 1.125", "0.2; four 0.4988 ha subplots"),]
# now that they're only single sizes, make numeric
plot_summary3$Plot_size_ha <- as.numeric(plot_summary3$Plot_size_ha)

# get BA values in /hectare format
plot_summary3$total_BA_ha <- plot_summary3$total_BA_ha/plot_summary3$Plot_size_ha
#plot_summary3$live_BA_ha <- plot_summary3$live_BA_ha/plot_summary3$Plot_size_ha
#plot_summary3$dead_BA_ha <- plot_summary3$dead_BA_ha/plot_summary3$Plot_size_ha

### NOTE NEED TO REDO THE LIVE AND DEAD BA CALCULATIONS IF I WANT TO KEEP THEM
### Because I calculated live and dead for if the tree was live or dead in any of the 3 years, but some trees change status (not sure I actually need these - though the total BA could be interesting to have)
## The proportion dead values in the columns yr0, yr1, and yr2 are correct but need to be combined to a single value

# only 300/3946 plots don't have a 1 year post measurement! this is 414/4194 when adding 3 yr post
sum(is.na(plot_summary3$yr1status_prop)) 

plot_summary3[(!is.na(plot_summary3$yr0status_prop) & !is.na(plot_summary3$yr2status_prop) & is.na(plot_summary3$yr1status_prop) & !is.na(plot_summary3$yr3status_prop)),]

## get a single BA loss/%deadBA column
plot_summary3$pcnt_ba_mort <- ifelse(!is.na(plot_summary3$yr1status_prop), 
                                     plot_summary3$yr1status_prop, 
                                     ifelse(!is.na(plot_summary3$yr0status_prop), 
                                            plot_summary3$yr0status_prop, 
                                            ifelse(!is.na(plot_summary2),
                                                   plot_summary3$yr2status_prop, 
                                                   plot_summary3$yr3status_prop)))
  
hist(plot_summary3$pcnt_ba_mort)
  
names(plot_summary3)

head(plot_summary3)

# v2 is with 3 year post-fire added (to include the 2008 Gunbarrel fire from Brian)
write.csv(plot_summary3, "VP/severity_tmp/data/saved/FTM/FTM_ba_v2.csv", row.names = FALSE)

ftmfire[ftmfire$Dataset=="Prichard",]
ftmfire_comp2[ftmfire_comp2$Dataset=="FERA",]
smithplot <- plot_summary3[plot_summary3$Dataset=="Kopper",]
unique(smithplot$YrFireName)
hist(smithplot$yr1status_prop)
neolaplot <- plot_summary3[plot_summary3$YrFireName=="2007 - Neola North",]
koppertree <- ftmtree[ftmtree$Dataset=="Kopper",]
feratree <- ftmtree[ftmtree$Dataset=="FERA",]
