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
setwd("G:/.shortcut-targets-by-id/1ixDqh6nB2x_6OmR4yevLRsssxIZhJlni/Assessment Report.sso files/Red Snapper/SEDAR52/")
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


# RECRUITMENT DEVIATIONS with error
# log-scale recruitment deviations from the expected mean (with standard error)
summary(base$parameters)
View(base$parameters)
temp = base$parameters
temp2 = temp[grep("RecrDev", temp$Label), , drop = FALSE]
recdev2 = temp2[,c("Label","Value","Parm_StDev")]
recdev3 = as.data.frame(separate_wider_delim(recdev2, cols = Label, delim = "_", names = c("Era", "label", "Yr")))

recdev3 = recdev3 %>% 
  filter(Era == "Main") %>%
  filter(Yr >= 2000)

recdev3$upper = recdev3$Value + recdev3$Parm_StDev
recdev3$lower = recdev3$Value - recdev3$Parm_StDev


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
  filter(Era != "VIRG") %>% 
  filter(Yr >= 2000)

# QUICK PLOTS

a = ggplot(biomass, aes(x=Yr, y=Bio_smry)) +
  geom_line() +
  xlab("Year") +
  ylab("Biomass (metric tons)") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")


b = ggplot(recdev3, aes(x=as.numeric(Yr), y=Value)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  xlab("Year") +
  ylab("log recruitment deviations") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")



#Plot the landings and discards by fleet/area
head(land_disc)
retained = land_disc[,c(1,3,4,5)]
discards = land_disc[,c(1,9,11,13)]
ratio = land_disc[,c(1,10,12,14)]
retained_long = retained %>% 
  pivot_longer(-Yr, names_to = "Fleet", values_to = "Retained_biomass")
discards_long = discards %>% 
  pivot_longer(-Yr, names_to = "Fleet", values_to = "Discarded_biomass")
ratio_long = ratio %>% 
  pivot_longer(-Yr, names_to = "Fleet", values_to = "Ratio")



c = ggplot(retained_long, aes(x=Yr, y=Retained_biomass, group=Fleet, col=Fleet)) +
  geom_line() +
  xlab("Year") +
  ylab("Retained biomass (metric tons)") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")

d = ggplot(discards_long, aes(x=Yr, y=Discarded_biomass, group=Fleet, col=Fleet)) +
  geom_line() +
  xlab("Year") +
  ylab("Discarded biomass (metric tons)") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")

e = ggplot(ratio_long, aes(x=Yr, y=Ratio, group=Fleet, col=Fleet)) +
  geom_line() +
  xlab("Year") +
  ylab("Landings:discards ratio") +
  theme_classic() +
  ggtitle("Scamp Grouper SEDAR 68")





