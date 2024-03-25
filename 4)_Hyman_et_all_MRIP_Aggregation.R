#------------------------------------------------------------------------------#
################################### Description ################################
#------------------------------------------------------------------------------#
# This file aggregated effort from raw MRIP files for fishing effort analysis
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
## Syntax packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

## Analysis
suppressMessages(library(survey))

#------------------------------------------------------------------------------#
## Clear out old files in R
rm(list=ls(all=TRUE)) 


## User-defined functions
`%nin%` <- Negate(`%in%`)

source("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/main/MRIP_Hyman_2024_Effort.R")
source("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/main/MRIP_Hyman_2024_Catch.R")


#------------------------------------------------------------------------------#
################################### Instructions ###############################
#------------------------------------------------------------------------------#
#
# Download MRIP data from the following address:
# https://github.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/tree/main
# or you can download the files directly from MRIP at:
# https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/
#
# Next, set your directory where the files are stored (all in one folder)
## Below is an example:
DIRECTORY <- 'C:/Users/ichal/Documents/Hyman gag grouper models/MRIP data'
#

species <- c("GAG", "RED GROUPER", "RED SNAPPER", "GRAY SNAPPER", "GREATER AMBERJACK", "VERMILION SNAPPER", "YELLOWTAIL SNAPPER", "GRAY TRIGGERFISH", "LANE SNAPPER", "MUTTON SNAPPER", "LESSER AMBERJACK", "ALMACO JACK", "BANDED RUDDERFISH", "HOGFISH", "GOLDEN TILEFISH", "BLUELINE TILEFISH", "GOLDFACE TILEFISH", "SCAMP", "BLACK GROUPER", "YELLOWEDGE GROUPER", "SNOWY GROUPER", "SPECKLED HIND", "YELLOWMOUTH GROUPER", "YELLOWFIN GROUPER", "WARSAW GROUPER",
             "GOLIATH GROUPER", "QUEEN SNAPPER", "BLACKFIN SNAPPER", "CUBERA SNAPPER", "SILK SNAPPER", "WENCHMAN", "grouper genus (epinephelus)", "grouper genus (mycteroperca)", "snapper family" )

dom1 <- list(month = c(paste("0", 1:9, sep = ""), "10", "11", "12"), fl_reg = c(1,2))


Effort_global <- NULL
Gag_global <- NULL
RG_global <- NULL
RS_global <- NULL
for (i in 2004:2023){
  Effort <- MRIP.dirtrips(intdir = DIRECTORY,
                          common = species, st = 12,
                          styr = i, endyr = i, trips = 1,
                          dom = dom1)
  Effort <- Effort%>%
    filter(mode_fx %in% c("5","7","6","8"),
           sub_reg=="7",
           area_x %in% c("3","4"))%>%
    group_by(year, fl_reg, month)%>%
    summarize(Trips = sum(Trips))
  Effort_global <- rbind(Effort_global, Effort)
  print(paste("Year", i, "is done for Effort"))
  
  
  Gag <- MRIP.catch(intdir = 'C:/Users/ichal/Documents/Hyman gag grouper models/MRIP data',
                    common = "GAG", st = 12,
                    styr = i, endyr = i,
                    dom = dom1)
  Gag <- Gag%>%
    filter(mode_fx %in% c("5","7","6","8"),
           sub_reg=="7",
           area_x %in% c("3","4"))%>%
    group_by(year, fl_reg, month)%>%
    summarize(Gag_catch = sum(total.catch),
              Gag_harvest = sum(harvest.A.B1),
              Gag_Discard = sum(released.B2),
              Gag_weight = sum(weight))
  Gag_global <- rbind(Gag_global, Gag)
  print(paste("Year", i, "is done for Gag"))
  
  RG <- MRIP.catch(intdir = 'C:/Users/ichal/Documents/Hyman gag grouper models/MRIP data',
                   common = "RED GROUPER", st = 12,
                   styr = i, endyr = i,
                   dom = dom1)
  RG <- RG%>%
    filter(mode_fx %in% c("5","7","6","8"),
           sub_reg=="7",
           area_x %in% c("3","4"))%>%
    group_by(year, fl_reg, month)%>%
    summarize(RG_catch = sum(total.catch),
              RG_harvest = sum(harvest.A.B1),
              RG_Discard = sum(released.B2),
              RG_weight = sum(weight))
  RG_global <- rbind(RG_global, RG)
  print(paste("Year", i, "is done for RG"))
  
  RS <- MRIP.catch(intdir = 'C:/Users/ichal/Documents/Hyman gag grouper models/MRIP data',
                   common = "RED SNAPPER", st = 12,
                   styr = i, endyr = i,
                   dom = dom1)
  RS <- RS%>%
    filter(mode_fx %in% c("5","7","6","8"),
           sub_reg=="7",
           area_x %in% c("3","4"))%>%
    group_by(year, fl_reg, month)%>%
    summarize(RS_catch = sum(total.catch),
              RS_harvest = sum(harvest.A.B1),
              RS_Discard = sum(released.B2),
              RS_weight = sum(weight))
  RS_global <- rbind(RS_global, RS)
  print(paste("Year", i, "is done for RS"))
  
}

setwd(DIRECTORY)
write.csv(Effort_global, "MRIP_Hyman_2024_Effort.csv", row.names = F, quote = F)
write.csv(Gag_global, "MRIP_Hyman_2024_Gag.csv", row.names = F)
write.csv(RG_global, "MRIP_Hyman_2024_RG.csv", row.names = F)
write.csv(RS_global, "MRIP_Hyman_2024_RS.csv", row.names = F)

