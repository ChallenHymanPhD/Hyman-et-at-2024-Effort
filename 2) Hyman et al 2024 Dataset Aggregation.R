#------------------------------------------------------------------------------#
################################### Description ################################
#------------------------------------------------------------------------------#
# This file aggregates predictors into a single data frame for use in analysis
# in Hyman et al 2024: Modeling effort in a multispecies recreational fishery; 
# influence of species-specific temporal closures, relative abundance, and 
# seasonality on angler-trips
#
# Required files can be downloaded directly from github.com via read.csv().
#
# The code below is annotated to explain to the user what each component 
# line executes.
#
# This file was written by A. Challen Hyman, PhD, on July 29th, 2024
#------------------------------------------------------------------------------#
##################################### Libraries ################################
#------------------------------------------------------------------------------#
## Syntax packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))

## Visualization packages
suppressMessages(library(ggplot2))
suppressMessages(library(xtable))
suppressMessages(library(grid))
suppressMessages(library(ggpubr))
suppressMessages(library(RColorBrewer))

## File reading
suppressMessages(library(readxl))

## User-defined functions
`%nin%` <- Negate(`%in%`)

#------------------------------------------------------------------------------#
##################################### Data #####################################
#------------------------------------------------------------------------------#
## Load data
Fuel <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/Florida%20gas.csv")                    ## Fuel prices
Days <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/Seasons.csv")                          ## FWC temporal reef fish harvest data
Bags <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/Bag%20limits.csv")                     ## Bag limits be species and year
CPI <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/Consumer%20price%20index.csv")          ## Consumer price index
Income <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/Income.csv")                         ## Florida median income
Buoy <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/NOAABuoyData.csv")                     ## NOAA buoy data
Sales <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/Licsense%20sales.csv", na.strings = 0)## Florida saltwater license sales
Juveniles <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Data-Files/Gag%20juvenile%20indices.csv")    ## Gag Juvenile indices of abundance


# Next, set your directory where the files are stored (all in one folder)
## Below is an example:
DIRECTORY <- "C:\\Users\\ichal\\Documents\\Hyman gag grouper models\\Hyman et al 2024b"
## Set working directory
setwd(DIRECTORY)


### APAIS raw data
APAIS <- read.csv("APAIS.csv")                                                  ## Aggregated APAIS dataset (must be compiled using '1) Hyman et al 2024 APAIS Data Aggregation.R')
for (i in 18:ncol(APAIS)){
  APAIS[which(is.na(APAIS[,i])),i] <- 0
}

## Impute APAIS missing values (can omit, ended up not mattering)
APAIS$party[which(is.na(APAIS$party))] <- 1
APAIS$hrsf[which(is.na(APAIS$hrsf))] <- 1
APAIS$hrsf[which(APAIS$hrsf==0)] <- 1
#------------------------------------------------------------------------------#
## Format data
Complete_data <- APAIS%>%
  filter(area_x  %in% c(3,4),                                                   ## Filter out shore trips
         mode_fx  %in% c(5,7))%>%                                               ## Retain only charter and private modes
  mutate(days = ifelse(as.numeric(substr(id_code,12,13))<15, '01', '01'))%>%
  mutate(Date = as.Date(paste0(year,"-",month,"-",days)),
         Region = ifelse(fl_reg == 1, "Panhandle", "Peninsula"),
         month = month,
         year = year,
         CPUE_gag = ((release_gag+landing_gag)/(party)),                        ## Gag Catch-Per-Unit-Effort
         HPUE_gag = ((landing_gag)/(party)),                                    ## Gag Harvest-Per-Unit-Effort
         DPUE_gag = ((release_gag)/(party)),                                    ## Gag Discards-Per-Unit-Effort
         P_Gag = landing_gag/(release_gag+landing_gag),                         ## Proportion of gag harvested
         CPUE_red.snapper = ((release_red.snapper+landing_red.snapper)/party),  ## Red snapper CPUE
         CPUE_red.grouper = ((release_red.grouper+landing_red.grouper)/party),  ## Red grouper CPUE
         Weight_Gag = wgt_ab1_gag/landing_gag,                                  ## average gag weight
         N = 1,
         N_Gag = ifelse(release_gag+landing_gag >0,1,0)
  )%>%
  group_by(Date, Region)%>%
  summarize(Discard_Gag = sum(release_gag*wp_int, na.rm = T),                   ## Weighted total gag discards
            Harvest_Gag = sum(landing_gag*wp_int, na.rm = T),                   ## Weighted total gag harvest
            Total_Gag = Discard_Gag + Harvest_Gag,                              ## Weighted total gag caught
            P_Gag = weighted.mean(P_Gag, w = wp_int, na.rm = T),                ## Weighted average proportion of gag harvested
            Weight_Gag = weighted.mean(Weight_Gag, w = wp_int, na.rm = T),      ## Weighted average gag weight
            Harvest_Gag_UW = sum(landing_gag),                                  ## Unweighted gag harvested
            Total_Gag_UW = sum(landing_gag + release_gag),                      ## Unweighted total gag caught
            CPUE_Gag = weighted.mean(CPUE_gag, w = wp_int),                     ## Weighted average gag CPUE
            HPUE_Gag = weighted.mean(HPUE_gag, w = wp_int),                     ## Weighted average gag HPUE
            DPUE_Gag = weighted.mean(DPUE_gag, w = wp_int),                     ## Weighted average gag DPUE
            CPUE_RS = mean(CPUE_red.snapper, w = wp_int),                       ## Weighted average red snapper CPUE
            CPUE_RG = mean(CPUE_red.grouper, w = wp_int),                       ## Weighted average red grouper CPUE
            Trips = sum(party*wp_int, na.rm=T),                                 ## Weighted total angler-trips
            year = mean(year),
            month = mean(month),
            N = sum(N),
            N_Gag = sum(N_Gag))%>%
  group_by(year, Region)%>%
  mutate(CPUE_Gag_annual = mean(CPUE_Gag),
         CPUE_RS_annual = mean(CPUE_RS),
         CPUE_RG_annual = mean(CPUE_RG),
         Weight_Gag = mean(Weight_Gag, na.rm = T))


reg_1 <- paste(c(123,129,29,33,37,45,5,91,113,131))                             ## Hyman edits manually specify counties in fl_reg 1
reg_2 <- paste(c(101,103,115,15,17,21,53,71,75,81,57))                          ## Hyman edits manually specify counties in fl_reg 2


### Fishing Seasons
Days$Date <- as.Date(Days$Date)
Days$year <- year(Days$Date)
Days$month <- month(Days$Date)
Days <- Days[-which(Days$Hyman_Region == "Panhandle"),]
Days$Hyman_Region[which(Days$Hyman_Region == "Big Bend")] <- "Panhandle"


### License sales
Sales <- Sales[c(1),]
Sales <- colSums(Sales[,-1], na.rm = T)
Sales <- data.frame(Year = names(Sales),
                    Sales = as.numeric(Sales))
Sales$Year <- 1990:2022

#### Add 2004 (Same as 2005)
Days_2004 <- Days[which(Days$year == 2005),]
Days_2004$year <- 2004
Days_2004$Date <- as.Date(paste(Days_2004$year, Days_2004$month, day(Days_2004$Date), sep = "-"))
Days <- rbind(Days, Days_2004)
Open <- Days %>% group_by(year, month, Hyman_Region)%>%
  summarize(Gag = mean(Gag),
            RS = mean(RS),
            RG = mean(RG))

Season <- Days %>%group_by(year, Hyman_Region)%>%
  summarize(Gag = sum(Gag),
            RS = sum(RS),
            RG = sum(RG))

Season$RG[which(Season$RG >365)] <- 365
Season$RS[which(Season$RS >365)] <- 365
Season$Gag[which(Season$Gag >365)] <- 365

### Bag limit
colnames(Bags)[1] <- c("year")
colnames(Bags)[2:8] <- tolower(gsub("\\.", " ", colnames(Bags)[2:8]))


### FL fuel prices
Fuel <- Fuel[nrow(Fuel):1,]#%>%.[1:276,]
colnames(Fuel)[2] <- "Price"
CPI <- CPI[,-14]%>%pivot_longer(2:13, names_to = "Month", values_to = "CPI")%>%.[1:288,]
CPI$CPI_prev <- c(100, CPI$CPI[-nrow(CPI)])
Fuel$CPI_ratio <- CPI$CPI/CPI$CPI_prev
Fuel$Fuel_CPI <- (Fuel$Price*Fuel$CPI_ratio)
Fuel <- Fuel[which(Fuel$Month == "4-Jan"):nrow(Fuel),]

### FL Income
colnames(Income) <- c("year", "Income")
Income$year <- year(as.Date(Income$year, format = "%d/%m/%Y"))
Income <- rbind(Income, c(2023, 65371))
Income <- Income[which(Income$year %in% 2004:2023),]

### Wind speed
Buoy$Date <- round_date(as.Date(Buoy$Date), unit = 'month')
Buoy <- Buoy%>%group_by(Date, Region)%>%
  mutate(Fishable = ifelse(Gust>=7.75,0,1))%>%
  summarise(Fishable = mean(Fishable),
            Speed = mean(abs(Gust), na.rm = T),
            SST = mean(SST, na.rm=T))
Buoy$Region <- ifelse(Buoy$Region==1, "Panhandle", "Peninsula")
#------------------------------------------------------------------------------#
## Harmonic regression terms
### set periods manually
per <- 365 ## period 1 is annual (365 dyas in a year)
per2 <- 182.5 ## period 2 is semi-annual (182.5 days every six months)

#------------------------------------------------------------------------------#
## Aggregate predictor and response data into single dataset
### Date
Complete_data <- Complete_data[order(Complete_data$Date),]
Complete_data$time <- as.POSIXlt(Complete_data$Date)$yday

### First harmonic
Complete_data$sin1 <- sin(2*pi/per*Complete_data$time)
Complete_data$cos1 <- cos(2*pi/per*Complete_data$time)

### Second harmonic
Complete_data$sin2 <- sin(2*pi/per2*Complete_data$time)
Complete_data$cos2 <- cos(2*pi/per2*Complete_data$time)

### CPUE
Complete_data$CPUE_Gag_lag <- Complete_data$CPUE_Gag[match(paste(Complete_data$year,Complete_data$Region, Complete_data$month),paste(Complete_data$year+1,Complete_data$Region, Complete_data$month))]#%>%round(.,1)
Complete_data$CPUE_RS_lag <- Complete_data$CPUE_RS[match(paste(Complete_data$year,Complete_data$Region, Complete_data$month),paste(Complete_data$year+1,Complete_data$Region, Complete_data$month))]#%>%round(.,1)
Complete_data$CPUE_RG_lag <- Complete_data$CPUE_RG[match(paste(Complete_data$year,Complete_data$Region, Complete_data$month),paste(Complete_data$year+1,Complete_data$Region, Complete_data$month))]#%>%round(.,1)

### CPUE annual
Complete_data$CPUE_Gag_lag_annual <- Complete_data$CPUE_Gag_annual[match(paste(Complete_data$year,Complete_data$Region, Complete_data$month),paste(Complete_data$year+1,Complete_data$Region, Complete_data$month))]#%>%round(.,1)
Complete_data$CPUE_RS_lag_annual <- Complete_data$CPUE_RS_annual[match(paste(Complete_data$year,Complete_data$Region, Complete_data$month),paste(Complete_data$year+1,Complete_data$Region, Complete_data$month))]#%>%round(.,1)
Complete_data$CPUE_RG_lag_annual <- Complete_data$CPUE_RG_annual[match(paste(Complete_data$year,Complete_data$Region, Complete_data$month),paste(Complete_data$year+1,Complete_data$Region, Complete_data$month))]#%>%round(.,1)

## Weight annual
Complete_data$Weight_Gag_lag_annual <- Complete_data$Weight_Gag[match(paste(Complete_data$year,Complete_data$Region, Complete_data$month),paste(Complete_data$year+1,Complete_data$Region, Complete_data$month))]#%>%round(.,1)

### Sales 
Complete_data$Sales <- Sales$Sales[match(Complete_data$year, Sales$Year+1)]/1e6

## Bag limit
Complete_data$Bag_gag <- Bags$gag[match(Complete_data$year, Bags$year)]
Complete_data$Bag_RS <- Bags$`red snapper`[match(Complete_data$year, Bags$year)]
Complete_data$Bag_RG <- Bags$`red grouper`[match(Complete_data$year, Bags$year)]

## Management
Complete_data$Month <- Complete_data$month
Complete_data$M_Gag <- Open$Gag[match(paste(Complete_data$Month, Complete_data$year, Complete_data$Region), paste(Open$month, Open$year, Open$Hyman_Region))]
Complete_data$M_RS <- Open$RS[match(paste(Complete_data$Month, Complete_data$year, Complete_data$Region), paste(Open$month, Open$year, Open$Hyman_Region))]
Complete_data$M_RG <- Open$RG[match(paste(Complete_data$Month, Complete_data$year, Complete_data$Region), paste(Open$month, Open$year, Open$Hyman_Region))]

## Seasons
Complete_data$S_Gag <- Season$Gag[match(paste(Complete_data$year, Complete_data$Region), paste(Season$year, Season$Hyman_Region))]
Complete_data$S_RG <- Season$RG[match(Complete_data$year, Season$year)]
Complete_data$S_RS <- Season$RS[match(Complete_data$year, Season$year)]

## Income
Complete_data$Income <- Income$Income[match(Complete_data$year, Income$year)]

## Fuel
Fuel$Date <- as.Date(paste0("01-",Fuel$Month), format = '%d-%y-%b')
Fuel$Month <- month(Fuel$Date)
Fuel$year <- year(Fuel$Date)
Complete_data$Fuel <- Fuel$Fuel_CPI[match(paste(Complete_data$month, Complete_data$year),paste(Fuel$Month,Fuel$year))]

## Wind
Complete_data$Wind <- Buoy$Speed[match(paste(Complete_data$Date, Complete_data$Region), paste(Buoy$Date, Buoy$Region))]
Complete_data$SST <- Buoy$SST[match(paste(Complete_data$Date, Complete_data$Region), paste(Buoy$Date, Buoy$Region))]

## Fuel to income ratio
Complete_data$FIR <- Complete_data$Income/1e4/Complete_data$Fuel

##Juvenile Index
Complete_data$Juveniles <- Juveniles$DL.Index[match(Complete_data$year, Juveniles$Survey.Year+1)]

Complete_data$S_GagL <- (Complete_data$M_Gag)*log(Complete_data$S_Gag)
Complete_data$S_RSL <- (Complete_data$M_RS)*log(Complete_data$S_RS)
Complete_data$S_RGL <- (Complete_data$M_RG)*log(Complete_data$S_RG)

write.csv(Complete_data, "MRIP Effort and Catch data.csv", quote = F, row.names = F)
