# Output data from Gulf stock synthesis output files. The files are hosted on google drive by the stock assessment team. We will use r4ss to pull the content from the stock synthesis model outputs. 

# Last updated by Carissa Gervasi - 10/14/25

#load packages
require(r4ss)
require(dplyr)
require(tidyr)
require(ggplot2)
require(purrr)
require(stringr)


# Things to add: separate biomass and ratios by area (if applicable) and separated by comm and rec fleets. Limit the time series to 2000 onward. Eventually we want to plot biomass trajectories against the ACL and landings. Also check if ratio differs drastically numbers vs. biomass.


####### PULL VARIABLES FOR A SINGLE ASSESSMENT ########

#all assessments:
# Cobia 28, 28U
# Gag Grouper 33U, 72
# Gray Snapper 51, 75
# Gray Triggerfish 43
# Greater Amberjack 33, 33U, 70
# Hogfish 37, 37U
# King Mackerel 38, 38U
# Mutton Snapper 79
# Red Grouper 42, 61, 88 
# Red Snapper 52, 74
# Scamp Grouper 68OA
# Spanish Mackerel 28, 81
# Vermillion Snapper 45, 67
# Yellowedge Grouper 22, 85
# Yellowtail Snapper 96

# files that don't work

# Need to first access the stock synthesis output files from google drive. Point to drive folder shortcut from google drive desktop. For shared folders you need to access them from the shortcut targets folder in the G drive.
Species = "Hogfish"
Assessment = "SEDAR37"

setwd("G:/.shortcut-targets-by-id/1ixDqh6nB2x_6OmR4yevLRsssxIZhJlni/Assessment Report.sso files/Hogfish/SEDAR37")
direct = getwd()
base=SS_output(dir = direct, dir.mcmc = NULL, repfile = "Report.sso", forecast = FALSE, warn = TRUE, covar = FALSE, readwt = FALSE, verbose = TRUE, NoCompOK = TRUE, compfile = "none", covarfile = "none")
base$startyr
base$endyr

#View the files of interest:
head(base$timeseries)
summary(base$timeseries)
head(base$recruit)
summary(base$recruit)

# BIOMASS 
# Age-1+ biomass over time (or other ages)
biomass = base$timeseries[,c("Area","Yr","Era","Bio_smry")]
biomass = biomass %>% 
  filter(Era != "FORE") %>% 
  filter(Yr >=2000)

a = ggplot(biomass, aes(x=Yr, y=Bio_smry, Group = as.factor(Area), col = as.factor(Area))) +
  geom_line() +
  xlab("Year") +
  ylab("Biomass (metric tons)") +
  theme_classic() +
  ggtitle(paste0(Species, " ", Assessment))

output_folder = "~/My Github Projects/Gulf-ESR-OLD/indicator_data/stock assessment output plots and data"
output_file = paste0(Species, "_", Assessment, "_biomass.png")
ggsave(filename = file.path(output_folder, output_file), plot = a,
       width = 8, height = 5, dpi = 300)

biomass1 = biomass %>% 
  filter(Area == 1)
biomass2 = biomass %>% 
  filter(Area == 2)
biomass1 = as.data.frame(biomass1[,2:4])
biomass2 = as.data.frame(biomass2[,2:4])

saveRDS(biomass1, file = file.path(output_folder, paste0(Species, "_Area1_", Assessment, "_biomass.rds")))
saveRDS(biomass2, file = file.path(output_folder, paste0(Species, "_Area2_", Assessment, "_biomass.rds")))


### Combine all the biomass datasets into one data frame ###

# Set the folder where .rds files are stored
input_folder = "~/My Github Projects/Gulf-ESR/indicator_data/stock assessment output plots and data"

# List all RDS files that match the pattern *_biomass.rds
files <- list.files(input_folder, pattern = "_biomass\\.rds$", full.names = TRUE)

#check all the files
for (file in files) {
  dat <- readRDS(file)
  print(basename(file))
  print(names(dat))
}

# Read and combine
biomass_df_list <- map(files, function(file) {
  # Read the .rds file
  dat <- readRDS(file)
  
  # Extract species_assessment from filename
  filename <- basename(file)
  name <- str_remove(filename, "_biomass\\.rds$")  # remove suffix
  
  # Keep only Yr and Bio_smry, rename Bio_smry to species_assessment name
  dat %>%
    select(Yr, Bio_smry) %>%
    rename(!!name := Bio_smry)
})

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_biomass <- reduce(biomass_df_list, full_join, by = "Yr")

# Arrange by year
combined_biomass <- combined_biomass %>% arrange(Yr)

saveRDS(combined_biomass, file = file.path(input_folder, "combined_biomass_trends.rds"))






# RECRUITMENT DEVIATIONS with error
# log-scale recruitment deviations from the expected mean (with standard error)
base = SS_outputs_all$`Red Snapper_SEDAR52`
dir_path <- base$inputs$dir
path_elements <- unlist(strsplit(dir_path, split = "/|\\\\"))
path_elements <- path_elements[path_elements != ""]
Species <- path_elements[length(path_elements) - 1]
Assessment <- path_elements[length(path_elements)]

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


b = ggplot(recdev3, aes(x=as.numeric(Yr), y=Value)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  xlab("Year") +
  ylab("log recruitment deviations") +
  theme_classic() +
  ggtitle(paste0(Species, " ", Assessment)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")




#LANDINGS/DISCARDS RATIO
# retained biomass and total catch are listed by fleet, so need to sum across fleets
# first check the fleet names
base$FleetNames
base$type
ts <- base$timeseries

# Identify all retained biomass columns (Ret_Bio)
retbio_cols <- grep("^retain\\(B\\):", names(ts), value = TRUE)
comm_retbio_cols = retbio_cols[1:2]
rec_retbio_cols = retbio_cols[3:5]
ts$Ret_Bio <- rowSums(ts[, retbio_cols], na.rm = TRUE)
ts$Ret_Bio_comm <- rowSums(ts[, comm_retbio_cols], na.rm = TRUE)
ts$Ret_Bio_rec <- rowSums(ts[, rec_retbio_cols], na.rm = TRUE)

# Identify all dead biomass columns (TotCat)
totcat_cols <- grep("^dead\\(B\\):", names(ts), value = TRUE)
comm_totcat_cols = totcat_cols[1:2]
rec_totcat_cols = totcat_cols[3:5]
ts$TotCat <- rowSums(ts[, totcat_cols], na.rm = TRUE)
ts$TotCatComm <- rowSums(ts[, comm_totcat_cols], na.rm = TRUE)
ts$TotCatRec <- rowSums(ts[, rec_totcat_cols], na.rm = TRUE)

land_disc = ts[,c("Yr","Era","Ret_Bio","Ret_Bio_comm","Ret_Bio_rec","TotCat","TotCatComm","TotCatRec")]

land_disc$discards = land_disc$TotCat - land_disc$Ret_Bio #re-label discards as dead_discards
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
  ggtitle(paste0(Species, " ", Assessment))


b = ggplot(recdev3, aes(x=as.numeric(Yr), y=Value)) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  xlab("Year") +
  ylab("log recruitment deviations") +
  theme_classic() +
  ggtitle(paste0(Species, " ", Assessment)) +
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
  ggtitle(paste0(Species, " ", Assessment))

d = ggplot(discards_long, aes(x=Yr, y=Discarded_biomass, group=Fleet, col=Fleet)) +
  geom_line() +
  xlab("Year") +
  ylab("Discarded biomass (metric tons)") +
  theme_classic() +
  ggtitle(paste0(Species, " ", Assessment))

e = ggplot(ratio_long, aes(x=Yr, y=Ratio, group=Fleet, col=Fleet)) +
  geom_line() +
  xlab("Year") +
  ylab("Landings:discards ratio") +
  theme_classic() +
  ggtitle(paste0(Species, " ", Assessment))





