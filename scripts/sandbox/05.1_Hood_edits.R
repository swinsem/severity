
## correcting data from Sharon Hood

ftmtree <- read.csv("VP/severity_tmp/data/original/FTM/Data/FTM_trees.csv") 
class(ftmtree$TreeNum)

ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="4" & ftmtree$TreeNum=="1585",]$DBH_cm <- 30.734 
ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="4" & ftmtree$TreeNum=="1585",]$yr1status <- 0
ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="4" & ftmtree$TreeNum=="1585",]$yr2status <- 0
ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="4" & ftmtree$TreeNum=="1585",]$yr3status <- 0
ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="6" & ftmtree$TreeNum=="1808",]$DBH_cm <- 53.594
ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="61" & ftmtree$TreeNum=="2742",]$DBH_cm <- 54.102
ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="61" & ftmtree$TreeNum=="2745",]$DBH_cm <- 54.61
ftmtree[ftmtree$YrFireName=="2001 - Moose" & ftmtree$Plot=="61" & ftmtree$TreeNum=="2752",]$DBH_cm <- 57.404

ftmtree[ftmtree$YrFireName=="2004 - Parks" & ftmtree$Plot=="10B" & ftmtree$TreeNum=="4012",]$DBH_cm <- 69.342 
ftmtree[ftmtree$YrFireName=="2004 - Parks" & ftmtree$Plot=="18B" & ftmtree$TreeNum=="4051",]$DBH_cm <- 30.226 

ftmtree[ftmtree$YrFireName=="2007 - Neola North" & ftmtree$Plot=="28" & ftmtree$TreeNum=="220",]$DBH_cm <- 75.692 
ftmtree[ftmtree$YrFireName=="2007 - Neola North" & ftmtree$Plot=="28" & ftmtree$TreeNum=="222",]$DBH_cm <- 41.91 


## replace Neola trees with Plot ID

hoodtree41 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/FTM_trees_corrected_Neolaonly.csv")

head(hoodtree41)

hoodtree41$Plot <- hoodtree41$fullID
hoodtree41$fullID <- NULL

ftmtree <- ftmtree[ftmtree$YrFireName != "2007 - Neola North",]
ftmtree <- rbind(ftmtree, hoodtree41)

write.csv(ftmtree, "VP/severity_tmp/data/original/FTM/Data/FTM_trees2.csv")


# get Neola data for 

head(hoodtree41)
length(unique(hoodtree41$fullID))

hoodcoord4 <- read.csv("VP/severity_tmp/data/original/FTM/Data/SharonHood/Neola_PIPO_PlotLocations_corrected.csv")


