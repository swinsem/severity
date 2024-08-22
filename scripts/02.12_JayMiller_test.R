
miller <- read.csv("VP/severity_tmp/data/saved/JayMiller/JayMiller_withcoordsba_rev.csv")
indgee <- read.csv("VP/severity_tmp/data/saved/GEE/VIs_bilinear_04192024.csv")

names(miller)
#indgee$pcnt_ba_mo2 <- indgee$pcnt_ba_mo
indgee <- merge(indgee, miller[, c("PlotID", "pcnt_ba_mort")], by="PlotID")
names(indgee)
ggplot(indgee) +
  geom_point(aes(x=pcnt_ba_mort, y=pcnt_ba_mo))
head(indgee)


millero <- read.csv("VP/severity_tmp/data/original/JayMiller/fielddata_2002-2005.csv")

millero <- millero[millero$drop != "x",]
names(millero)
ggplot(millero) +
  geom_point(aes(x=RdNBR, y=X.BA_mort))


head(millero)
head(ind[ind$Dataset=="JayMiller",])

millerall <- merge(millero, ind[ind$Dataset=="JayMiller",], by.x = "PCOrdID", by.y="PlotID")

names(millerall)
rdnbr <- millerall$RdNBR

rdnbr[rdnbr < 190.476] <- 190.476  
rdnbr[rdnbr > 889.51] <- 889.51
ba_loss_pre <- sin((rdnbr/1.144 - 166.5)/ 389.0)
ba_loss_pre[ba_loss_pre < 0] <- 0
ba_loss <- ba_loss_pre^2 * 100

millerall$ba_loss_s <- ba_loss


ggplot(millerall, aes(x=ba_loss_s, y = pcnt_ba_mo)) +
  geom_point()

ggplot(millerall, aes(x=RdNBR, y=rdnbr)) +
  geom_point()
ggplot(millerall, aes(x=RdNBR, y=X.CC_mort)) +
  geom_point()

ggplot(alldata[alldata$Dataset!="JayMiller",], aes(x=rbr, y=pcnt_ba_mo)) +
  geom_point()

### Try categorized 

head(results_plot)
rescat <- results_plot

# Assuming rescat is your dataframe
library(dplyr)

# Define breaks and labels for the categories
breaks <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)
labels <- c("0-0.1", "0.1-0.25", "0.25-0.5", "0.5-0.75", "0.75-0.9", ">0.9")
labels <- c(0,1,2,3,4,5)

# Create the new columns using cut()
rescat <- rescat %>%
  mutate(cat = cut(pcnt_ba_mo, breaks = breaks, labels = labels, right = FALSE),
         predcat = cut(pred, breaks = breaks, labels = labels, right = FALSE))

rescat[rescat$pcnt_ba_mo==1,]$cat <- 5

# View the updated dataframe
head(rescat)
class(rescat$predcat)

ggplot(rescat) +
  geom_count(aes(x=cat, y=predcat))
ggplot(rescat) +
  geom_boxplot(aes(x=pcnt_ba_mo, y=predcat))

rf_all <- randomForest(pcnt_ba_mo ~ ., data = alldat, proximity = TRUE, ntree = 1000)
varImpPlot(rf_all,
           sort = T,
           n.var = 20,
           main = "Top 20 - Variable Importance")
