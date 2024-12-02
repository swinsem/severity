# make list of fires and datasets for Supplement

ard <- read.csv("VP/severity_tmp/data/saved/ARD_nocoords.csv")
fires <- ard %>%
  group_by(Dataset, YrFireName) %>%
  summarize()

fires <- ifelse(fires$Dataset=="JayMiller", )


fires$YrFireName[fires$Dataset == "JayMiller"] <- sub(
  pattern = "^(.*) - (\\d{4})$",
  replacement = "\\2 - \\1",
  x = fires$YrFireName[fires$Dataset == "JayMiller"]
)

