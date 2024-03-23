### Code to obtain fraction of Fishable days in Hyman et al 2024
library(rnoaa)
library(tidyverse)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)


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
      data <- as.data.frame(data[,c(1,2,3,5,7)]) ## date, lon, lat, wind speed, and wave height
      data$Date <- as.Date(data$time)
      data <- data%>%group_by(Date, lon, lat)%>% ## aggregate to day
        summarize(speed = mean(wind_spd, na.rm = T),
                  height = mean(wave_height, na.rm = T))
      data$Buoy <- my_buoy
      data$Region <- Region[j]
      Full_Data <- rbind(Full_Data,data) ## bind to dataset
    }
    print(paste("Buoy", my_buoy, "in year", i, "is complete"))
  }
}

write.csv(Full_Data, "Fishable_days_all_stations_03_24.csv")

Fishable <- Full_Data%>%group_by(Date, Region)%>%
  summarize(Speed = mean(speed, na.rm = T),
            Height = mean(height, na.rm = T))

write.csv(Fishable, "Fishable_days_MRIP_03_24.csv")
