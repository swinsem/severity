
library(aws.s3)
library(dplyr)

bucket_name="tmp-sara"
object_name= paste0("severity/data/FTM/Data/FTM_trees.csv")
ftmtree <- read.csv("VP/severity_tmp/data/original/FTM/Data/FTM_trees.csv")

head(ftmtree)
names(ftmtree)

roccaforte <- ftmtree[ftmtree$Dataset == "Roccaforte",]
unique(roccaforte$YrFireName)
wallow <- ftmtree[ftmtree$YrFireName=="2011 - Wallow",]
roccatest <- wallow[wallow$Plot == "1_4_14",]


# Read the datasets
wallow <- ftmtree[ftmtree$YrFireName == "2011 - Wallow", ]
originalwallow <- read.csv("Downloads/wallow.csv")
names(originalwallow)


# 1. Identify trees with unique combinations of species, dbh, and height across all plots
unique_trees <- wallow %>%
  group_by(Species, DBH_cm, HT_m) %>%
  filter(n() == 1) %>%
  ungroup()

# 2. From the unique combinations, choose one tree from each plot
key_wallow <- unique_trees %>%
  group_by(Plot) %>%
  slice_head(n = 1) %>%
  ungroup()

# For the 'originalwallow' dataset, pick one tree for each plot
key_originalwallow <- originalwallow %>%
  group_by(Plot_ID) %>%
  slice_head(n = 1) %>%
  ungroup()

# 3. Join the two datasets based on the unique combinations
matched_data <- key_wallow %>%
  inner_join(originalwallow, 
             by = c("Species" = "Sp_code", 
                    "DBH_cm" = "X2012.DBH_x", 
                    "HT_m" = "X2012.TotHt_m"))

# Review the result
head(matched_data)

# Check unmatched plots - not really sure why these are here
unmatched_wallow_plots <- setdiff(unique(key_wallow$Plot), matched_data$Plot)
unmatched_originalwallow_plots <- setdiff(unique(originalwallow$Plot_ID), matched_data$Plot_ID)

for(x in unmatched_wallow_plots) {
  print(x)
  print(nrow(wallow[wallow$Plot == x,]))
}
for(x in unmatched_originalwallow_plots) {
  print(x)
  print(nrow(originalwallow[originalwallow$Plot_ID == x,]))
}

# two plots with 1 tree each
wallow[wallow$Plot=="8_1_99",]
originalwallow[originalwallow$Plot_ID=="SFK-1-TFR-132",]

wallow[wallow$Plot=="8_1_102",]
originalwallow[originalwallow$Plot_ID=="SFK-1-TFR-195",]

df <- data.frame(
  eri = c("ALP-2-TFR-81", "NUT-1-UNT-278", "ODR-2-TFR-108", "SFK-1-TFR-197", "SFK-2-TFR-16", 
          "SFK-1-TFR-132", "SFK-1-TFR-195"),
  ftm = c("2_1_26", "6_2_77", "7_1_83", "8_1_103", "9_1_114", 
          "8_1_99", "8_1_102")
)

df1 <- as.data.frame(matched_data[, c("Plot", "Plot_ID")])
names(df1) <- c("ftm", "eri")

wallowkey <- rbind(df, df1)
write.csv(wallowkey, "VP/severity_tmp/data/saved/FTM/data_request/Roccaforte_wallowkey.csv")

# Count rows in originalwallow for each eri value in wallowkey
originalwallow_counts <- originalwallow %>%
  group_by(Plot_ID) %>%
  summarise(ow_count = n()) %>%
  right_join(wallowkey, by = c("Plot_ID" = "eri"))
names(originalwallow_counts) <- c("eri", "ow_count", "ftm")

# Count rows in wallow for each ftm value in wallowkey
wallow_counts <- wallow %>%
  group_by(Plot) %>%
  summarise(w_count = n()) %>%
  right_join(wallowkey, by = c("Plot" = "ftm"))
names(wallow_counts) <- c("ftm", "w_count", "eri")

# Add these counts as new columns to wallowkey
wallowkey2 <- wallowkey %>%
  left_join(originalwallow_counts, by = "eri") 
wallowkey2 <- wallowkey2 %>%
  left_join(wallow_counts, by = c("ftm.y", "ftm")) %>%
  select(eri.x, ftm.x, ow_count, w_count)

wallowkey2 <- merge(wallowkey2, wallow_counts, by.x="ftm.y", by.y="ftm")

#########################
### Lower Middle Mountain ###

# Read the datasets
lowmid <- ftmtree[ftmtree$YrFireName == "2008 - Lower Middle Mountain", ]
originallowmid <- read.csv("Downloads/lowermiddlemtn.csv")
names(originallowmid)


# 1. Identify trees with unique combinations of species, dbh, and height across all plots
unique_trees <- lowmid %>%
  group_by(Species, DBH_cm, HT_m) %>%
  filter(n() == 1) %>%
  ungroup()

# 2. From the unique combinations, choose one tree from each plot
key_lowmid <- unique_trees %>%
  group_by(Plot) %>%
  slice_head(n = 1) %>%
  ungroup()

# For the 'originalwallow' dataset, pick one tree for each plot
key_originallowmid <- originallowmid %>%
  group_by(Plot.) %>%
  slice_head(n = 1) %>%
  ungroup()

# 3. Join the two datasets based on the unique combinations

matched_data1 <- key_lowmid %>%
  left_join(key_originallowmid, 
            by = c("Species" = "Sp_code", 
                   "DBH_cm" = "X2003.Pre.dbh_x", 
                   "HT_m" = "X2003.Pre.TotHt..m."))


# Check unmatched plots - not really sure why these are here
unmatched_lowmid_plots <- setdiff(unique(key_lowmid$Plot), matched_data1$Plot)
unmatched_originallowmid_plots <- setdiff(unique(originallowmid$Plot.), matched_data1$Plot.)

for(x in unmatched_lowmid_plots) {
  print(x)
  print(nrow(lowmid[lowmid$Plot == x,]))
}
for(x in unmatched_originallowmid_plots) {
  print(x)
  print(nrow(originallowmid[originallowmid$Plot. == x,]))
}


lowmid[lowmid$Plot=="2_NA_15",]
originallowmid[originallowmid$Plot.=="1-2-15",]

lowmid[lowmid$Plot=="2_NA_54",]
originallowmid[originallowmid$Plot.=="2-2-14",]


df <- data.frame(
  eri = c("1-2-8", "1-2-13", "3-2-1", "1-2-15", "2-2-14" ),
  ftm = c("2_NA_8", "2_NA_13", "2_NA_81", "2_NA_15", "2_NA_54" )
)

df1 <- as.data.frame(matched_data1[, c("Plot", "Plot.")])
names(df1) <- c("ftm", "eri")

lowermidkey <- rbind(df, df1)

duplicated(lowermidkey$eri)
write.csv(lowermidkey, "VP/severity_tmp/data/saved/FTM/data_request/Roccaforte_lowermiddlemtnkey.csv")

