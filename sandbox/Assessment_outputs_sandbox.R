# Playing around with the stock assessment output data 

# Carissa Gervasi - last updated 6/17/25

#load packages
require(r4ss)
require(RColorBrewer)
require(dplyr)
require(tidyr)
require(ggplot2)
library(colorspace)


# Need to first access the stock synthesis output files from google drive.
setwd("G:/.shortcut-targets-by-id/1ixDqh6nB2x_6OmR4yevLRsssxIZhJlni/Assessment Report.sso files/Scamp Grouper/SEDAR68OA")
direct = getwd()
base=SS_output(dir = direct, printstats = T, covar=T, cormax=0.70, forecast=F)


DAT<-SS_readdat(paste0(getwd(),"/SEDAR68OT.dat"),verbose = FALSE)
StartYr<-base$startyr
EndYr<-base$endyr


###############
# Fleet Names #
###############

#create a CSV with fleetnames, abbreviations, etc. for your current model
flt<-as.data.frame(cbind(base$fleet_ID,                         # SS Fleet IDs
                         base$FleetNames,                                     # SS fleet Names
                         c("Commercial Vertical Line","Commercial Longline",
                           "Recreational Charter Private","Recreational Headboat",
                           "Combined Video Survey","RFOP Vertical Line Survey"), # Full fleet names
                         c("ComVL","ComLL","Char/Pri","Hbt","Video Surv","RFOP VL Surv"), # Fleet Abbreviations
                         base$fleet_type,                                     # Fleet Types
                         paste0("Fleet",base$fleet_ID),                       # Fleets Numbered
                         c(rep("FD",4),"FI","FD"),                            # Data source
                         DAT$fleetinfo$units,                                 # SS units
                         c("TRUE", rep("FALSE",2),rep("TRUE",3)),             # Index
                         c(rep("FALSE",2), rep("TRUE",2),rep("NA",2)),        # Mean Body Weight
                         c(rep("COM",2),rep("REC",2),rep("NA",2)),            # Fishery
                         c("Vertical Line","Longline","Charter Private","Headboat",
                           "Combined Video Survey","RFOP Vertical Line Survey"), # Min fleet names
                         c("pre-IFQ Vertical Line CPUE","","","Headboat CPUE",
                           "Combined Video Survey","RFOP Vertical Line Survey"))) # Index_name
colnames(flt)<-c("SSID","SSnam","fltnam_full","fltnam_abbr","type","Fleet",
                 "orig","SSunit","Index","Mean_body_wgt","Fishery","fltnam_min","index_nam")           
flt$type[which(flt$type==1)]<-"Flt"
flt$type[which(flt$type==2)]<-"Bycatch"
flt$type[which(flt$type==3)]<-"Srv"

fltnames = flt
#write.csv(flt,"fltnames.csv",row.names=F)
#fltnames <- read.csv("fltnames.csv");fltnames

#color palette
pal1 = "Dark2" #this is a colorblind friendly palette


###################
#  Save SS Plots  #
###################

setwd("~/My Github Projects/Gulf-ESR/sandbox")
PlotDIR<-paste0(getwd(),"/Plots/")
direct = getwd()

#Note here that I've renamed to fleets to clean up the labels
SS_plots(replist = base,uncertainty=T, sprtarg=0.30, btarg=0.3, datplot=T, 
         minbthresh=-1,png=T, printfolder="Plots", dir=direct, pdf=F,
         mainTitle=F,fixdims=F,fleetnames=fltnames$fltnam_full,maxrows2=5,
         maxcols2=5,maxrows=6,maxcols=6,rows=5,cols=5)
#Note here that I'm rewriting out the comp plots on as few pages as possible


test = base$timeseries
head(test)



# MSY and related quantities
msy_vals <- base$derived_quants
msy <- msy_vals[grep("MSY", msy_vals$Label), ]
print(msy)

#rec devs
recdevs <- base$recruit
recdevs <- base$recruit[, c("Yr", "dev", "dev_se")]
head(recdevs)


#biomass
biomass <- base$timeseries[, c("Yr", "SpawnBio")]
head(biomass)


# Plot spawning biomass
plot(biomass$Yr, biomass$SpawnBio, type = "l", lwd = 2,
     ylab = "Spawning Biomass", xlab = "Year")

# Plot recruitment deviations
plot(recdevs$Yr, recdevs$dev, type = "h",
     ylab = "Recruitment Deviations", xlab = "Year")

# Plot MSY
abline(h = msy$Value, col = "red", lty = 2)
