#------------------------------------------------------------------------------#
################################### Description ################################
#------------------------------------------------------------------------------#
# This file is used to obtain fraction of fishable days, a predictor used
# in Hyman et al 2024: Modeling effort in a multispecies recreational fishery; 
# influence of species-specific temporal closures, relative abundance, and 
# seasonality on angler-trips
#
# Required MRIP files can be downloaded directly from github.com via read.csv().
# These files must be downloaded prior to running code below.
#
# The code below is annotated to explain to the user what each component 
# line executes.
#
# This file was written by A. Challen Hyman, PhD, on March 23rd, 2024
#------------------------------------------------------------------------------#
##################################### Libraries ################################
#------------------------------------------------------------------------------#
suppressMessages(library(rnoaa))
suppressMessages(library(tidyverse))

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Set your directory where the files are stored (all in one folder)
## Below is an example:
# DIRECTORY <- 'C:/Users/ichal/Documents/Hyman gag grouper models/MRIP data'

## List of buoys desired for analysis
Buoys <- c("42012",
           "PCLF1",
           "PCBF1",
           "APCF1",
           "SGOF1",
           "SHPF1",
           "KTNF1",
           "CKYF1",
           "ARPF1",
           "FHPF1",
           "CWBF1",
           "42098",
           "42013",
           "VENF1",
           "BCGF1",
           "NPSF1")
## Set MRIP FL_reg region (in buoy order)
Region <- c(1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2)
Full_Data <- NULL
for(j in 1:length(Buoys)){
  my_buoy <- Buoys[j]
  for (i in 2004:2023){
    assign("data", try(buoy(dataset = "stdmet", buoyid = my_buoy, year = i)$data, silent = T)) ## ignores empty data values
    if(class(data)[1] == "try-error"){
      next
    } else {
      data <- as.data.frame(data[,c(1,2,3,5,7,6, 13)]) ## date, lon, lat, wind speed, and wave height
      data$Date <- as.Date(data$time)
      data <- data%>%group_by(Date, lon, lat)%>% ## aggregate to day
        summarize(speed = mean(wind_spd, na.rm = T),
                  gust = mean(gust, na.rm = T),
                  height = mean(wave_height, na.rm = T),
                  SST = mean(sea_surface_temperature, na.rm = T))
      data$Buoy <- my_buoy
      data$Region <- Region[j]
      Full_Data <- rbind(Full_Data,data) ## bind to dataset
    }
    print(paste("Buoy", my_buoy, "in year", i, "is complete"))
  }
}
Fishable <- Full_Data%>%group_by(Date, Region)%>%
  summarize(Speed = mean(speed, na.rm = T),
            Height = mean(height, na.rm = T))
setwd(DIRECTORY)
write.csv(Fishable, "Fishable_days.csv")
