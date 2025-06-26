# Output data from Gulf stock synthesis output files. The files are hosted on google drive by the stock assessment team. Code here uses a series of functions and loops to pull and process all the data for all assessments in the drive folder at once.

# Last updated by Carissa Gervasi - 6/25/25

rm(list = ls())
dev.off()

# load packages
source("indicator_processing/functions/process_SS_assessments.R")

process_SS_assessments() #only need to run this if there are new/updated assessment models you want to pull from the google drive

SS_outputs_all = readRDS("indicator_data/stock assessment output plots and data V2/SS_outputs_all.rds")


######## BIOMASS #########

#View the files of interest:

base = SS_outputs_all$`G:/.shortcut-targets-by-id/1ixDqh6nB2x_6OmR4yevLRsssxIZhJlni/Assessment Report.sso files/Cobia/SEDAR28`
Species = 

head(base$timeseries)
summary(base$timeseries)
head(base$recruit)
summary(base$recruit)

# BIOMASS 
# Age-1+ biomass over time
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

output_folder = "~/My Github Projects/Gulf-ESR/indicator_data/stock assessment output plots and data"
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



