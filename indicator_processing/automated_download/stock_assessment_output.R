# Output data from Gulf stock synthesis output files. The files are hosted on google drive by the stock assessment team. We will use r4ss to pull the content from the stock synthesis model outputs. 

# Last updated by Carissa Gervasi - 6/18/25

#load packages
require(r4ss)
require(dplyr)
require(tidyr)
library(ggplot2)


# Things to add: separate biomass and ratios by area (if applicable) and separated by comm and rec fleets. Limit the time series to 2000 onward. Eventually we want to plot biomass trajectories against the ACL and landings. Also check if ratio differs drastically numbers vs. biomass.


####### PULL VARIABLES FOR A SINGLE ASSESSMENT ########

# Need to first access the stock synthesis output files from google drive. Point to drive folder shortcut from google drive desktop.
setwd("G:/.shortcut-targets-by-id/1ixDqh6nB2x_6OmR4yevLRsssxIZhJlni/Assessment Report.sso files/Scamp Grouper/SEDAR68OA/")
direct = getwd()
base=SS_output(dir = direct, printstats = T, covar=T, cormax=0.70, forecast=F)
base$startyr
base$endyr

#View the files of interest:
head(base$timeseries)
summary(base$timeseries)
head(base$recruit)
summary(base$recruit)

# BIOMASS 
# Age-1+ biomass over time
biomass = base$timeseries[,c("Yr","Era","Bio_smry")]
biomass = biomass %>% 
  filter(Era != "FORE") %>% 
  filter(Yr >=2000)

# RECRUITMENT DEVIATIONS
# log-scale recruitment deviations from the expected mean
recdev = base$recruit[,c("Yr","era","dev")]
recdev = recdev %>% 
  filter(era != "Fore") %>% 
  filter(Yr >= 2000)

#LANDINGS/DISCARDS RATIO
# retained biomass and total catch are listed by fleet, so need to sum across fleets
# first check the fleet names
str(base$FleetNames)
ts <- base$timeseries

# Identify all retained biomass columns (Ret_Bio)
retbio_cols <- grep("^retain\\(B\\):", names(ts), value = TRUE)
comm_retbio_cols = retbio_cols[1:2]
rec_retbio_cols = retbio_cols[3:4]
ts$Ret_Bio <- rowSums(ts[, retbio_cols], na.rm = TRUE)
ts$Ret_Bio_comm <- rowSums(ts[, comm_retbio_cols], na.rm = TRUE)
ts$Ret_Bio_rec <- rowSums(ts[, rec_retbio_cols], na.rm = TRUE)

# Identify all dead biomass columns (TotCat)
totcat_cols <- grep("^dead\\(B\\):", names(ts), value = TRUE)
comm_totcat_cols = totcat_cols[1:2]
rec_totcat_cols = totcat_cols[3:4]
ts$TotCat <- rowSums(ts[, totcat_cols], na.rm = TRUE)
ts$TotCatComm <- rowSums(ts[, comm_totcat_cols], na.rm = TRUE)
ts$TotCatRec <- rowSums(ts[, rec_totcat_cols], na.rm = TRUE)

land_disc = ts[,c("Yr","Era","Ret_Bio","Ret_Bio_comm","Ret_Bio_rec","TotCat","TotCatComm","TotCatRec")]

land_disc$discards = land_disc$TotCat - land_disc$Ret_Bio
land_disc$ld_ratio = land_disc$Ret_Bio / (land_disc$discards)

land_disc$discards_comm = land_disc$TotCatComm - land_disc$Ret_Bio_comm
land_disc$ld_ratio_comm = land_disc$Ret_Bio_comm / (land_disc$discards_comm)

land_disc$discards_rec = land_disc$TotCatRec - land_disc$Ret_Bio_rec
land_disc$ld_ratio_rec = land_disc$Ret_Bio_rec / (land_disc$discards_rec)

land_disc = land_disc %>% 
  filter(Era != "FORE") %>% 
  filter(Yr >= 2000)

# QUICK PLOTS

ggplot(biomass, aes(x=Yr, y=Bio_smry)) +
  geom_line() +
  xlab("Year") +
  ylab("Biomass (metric tons)") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")


ggplot(recdev, aes(x=Yr, y=dev)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("log recruitment deviations") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")



ggplot(land_disc, aes(x=Yr, y=Ret_Bio)) +
  geom_line() +
  xlab("Year") +
  ylab("Retained biomass (metric tons)") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")

ggplot(land_disc, aes(x=Yr, y=discards)) +
  geom_line() +
  xlab("Year") +
  ylab("Discarded biomass (metric tons)") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")

ggplot(land_disc, aes(x=Yr, y=ld_ratio)) +
  geom_line() +
  xlab("Year") +
  ylab("Landings:discards ratio") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")

