# Output data from Gulf stock synthesis output files. The files are hosted on google drive by the stock assessment team. Code here uses a series of functions and loops to pull and process all the data for all assessments in the drive folder at once.

# Last updated by Carissa Gervasi - 10/14/25

rm(list = ls())
dev.off()

#load packages
require(r4ss)
require(dplyr)
require(tidyr)
require(ggplot2)
require(purrr)
require(stringr)

# load packages
source("indicator_processing/functions/process_SS_assessments.R")

process_SS_assessments() #only need to run this if there are new/updated assessment models you want to pull from the google drive


#Start here if you don't need to load new assessments
SS_outputs_all = readRDS("indicator_data/stock assessment output plots and data V2/SS_outputs_all.rds")



######## BIOMASS (automated) #########

# Define the output folder outside the loop
output_folder <- "~/My Github Projects/Gulf-ESR-OLD/indicator_data/stock assessment output plots and data V3"

# Get the names of the list items to iterate through
model_names <- names(SS_outputs_all)

# Loop through each model name
for (name in model_names) {
  
  # Set the current model as 'base'
  base <- SS_outputs_all[[name]]
  
  dir_path <- base$inputs$dir
  path_elements <- unlist(strsplit(dir_path, split = "/|\\\\"))
  path_elements <- path_elements[path_elements != ""]
  
  # The species name is the second to last element
  Species <- path_elements[length(path_elements) - 1]
  # The assessment name is the last element
  Assessment <- path_elements[length(path_elements)]
  
  # Check if timeseries data exists and has a 'Bio_smry' column
  if (!is.null(base$timeseries) && "Bio_smry" %in% colnames(base$timeseries)) {
    # Filter and process the biomass data
    biomass <- base$timeseries %>%
      filter(Era != "FORE") %>%
      filter(Yr >= 2000) %>%
      mutate(Bio_mil_lbs = (Bio_smry * 2204.62) / 1000000)
    
    # Check if there is data after filtering
    if (nrow(biomass) > 0) {
      
      # Generate the plot
      a <- ggplot(biomass, aes(x = Yr, y = Bio_mil_lbs, group = as.factor(Area), col = as.factor(Area))) +
        geom_line(linewidth = 1.2) +
        geom_point(size = 2) +
        xlab("Year") +
        ylab("Biomass (million pounds)") +
        theme_classic() +
        ggtitle(paste0(Species, " ", Assessment)) +
        theme(
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          axis.title.y = element_text(vjust = 2),
          axis.title.x = element_text(vjust = -1)
        ) +
        scale_x_continuous(breaks = unique(biomass$Yr)) +
        scale_color_discrete(name = "Area")
      
      # Save the plot
      output_file <- paste0(Species, "_", Assessment, "_biomass.png")
      ggsave(filename = file.path(output_folder, output_file), plot = a,
             width = 10, height = 6, dpi = 300)
      
      # --- Code to save separate RDS files for each area ---
      unique_areas <- unique(biomass$Area)
      
      for (area in unique_areas) {
        area_data <- biomass %>%
          filter(Area == area) %>%
          select(Yr, Bio_smry, Bio_mil_lbs, Area) %>%
          as.data.frame()
        
        file_name <- paste0(Species, "_Area", area, "_", Assessment, "_biomass.rds")
        
        saveRDS(area_data, file = file.path(output_folder, file_name))
      }
      
    } else {
      warning(paste("No data for plotting after filtering for", name))
    }
  } else {
    warning(paste("Timeseries data or 'Bio_smry' column not found for", name))
  }
  
  # Print a message to show progress
  print(paste("Processed:", name))
}





### Combine all the biomass datasets into one data frame ###

# Set the folder where .rds files are stored
input_folder = "~/My Github Projects/Gulf-ESR-OLD/indicator_data/stock assessment output plots and data V3"

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
  
  # Keep only Yr and Bio_mil_lbs, rename Bio_mil_lbs to species_assessment name
  dat %>%
    select(Yr, Bio_mil_lbs) %>%
    rename(!!name := Bio_mil_lbs)
})

# Merge all data frames by Yr
# Reduce joins all the data frames by Yr column
combined_biomass <- reduce(biomass_df_list, full_join, by = "Yr")

# Arrange by year
combined_biomass <- combined_biomass %>% arrange(Yr)

saveRDS(combined_biomass, file = file.path(input_folder, "combined_biomass_trends.rds"))






# RECRUITMENT DEVIATIONS
# log-scale recruitment deviations from the expected mean
#recdev = base$recruit[,c("Yr","era","dev")]
#recdev = recdev %>% 
#  filter(era != "Fore") %>% 
#  filter(era != "Forecast") %>%
#  filter(Yr >= 2000)


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

