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
# This file was written by A. Challen Hyman, PhD, on March 23rd, 2024
#------------------------------------------------------------------------------#
##################################### Libraries ################################
#------------------------------------------------------------------------------#
## Syntax packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))
suppressMessages(library(lubridate))

## File reading
suppressMessages(library(readxl))

## Mapping
suppressMessages(library(sf))
suppressMessages(library(sp))

#------------------------------------------------------------------------------#
##################################### Housekeeping #############################
#------------------------------------------------------------------------------#
## Clear out old files in R
#rm(list=ls(all=TRUE)) 

## Set working directory
setwd("C:\\Users\\ichal\\Documents\\Hyman gag grouper models\\Hyman et al 2024b")

## User-defined functions
`%nin%` <- Negate(`%in%`)

#------------------------------------------------------------------------------#
## Load data
Days <- read.csv("Days_month_frac_2023.csv")[,-1]                ## Fraction of month open in given year
SSB <- read_excel("SSB2.xlsx")                                   ## Spawning stock biomass from stock assessments
IoA <- read.csv("IoA.csv")                                       ## Index of Abundance from interim assessments
Fuel <- read.csv("Florida gas.csv")                              ## Fuel prices
Season <- read.csv("Season length_2023.csv")                     ## Season lengths for each species by year
Bags <- read.csv("Bag limits.csv")                               ## Bag limits be species and year
CPI <- read_excel("Consumer price index.xlsx")                   ## Consumer price index
GDP <- read.csv("FL_GDP.csv")                                    ## Florida GDP
Income <- read.csv("Income.csv")                                 ## Florida median income
Income_2 <- read.csv("FL_Real_Income.csv")                       ## Florida mean income adjusted to inflation (2021 dollars)
Inflation <- read.csv("Inflation rate.csv")                      ## US inflation rate
Fishable <- read.csv("Fishable_days_MRIP_03_24.csv")             ## Fraction of fishable days by region
County_ID <- read.csv("County_ID.csv")                           ## County ID
counties <- read.csv("counties.csv")                             ## County name and fps
colnames(counties) <- c("Code", "County")
Gag_Searches <- read.csv("Gag_Searches.csv")                     ## Google searches for gag
RG_Searches <- read.csv("RG_Searches.csv")                       ## Google searches for red grouper
RS_Searches <- read.csv("RS_searches.csv")                       ## Google searches for red snapper

### Data
Effort <- read.csv("MRIP_Hyman_2024_Effort.csv")
Gag <- read.csv("MRIP_Hyman_2024_Gag.csv")
RG <- read.csv("MRIP_Hyman_2024_RG.csv")
RS <- read.csv("MRIP_Hyman_2024_RS.csv")
#------------------------------------------------------------------------------#
## Format data
Complete_data <- Effort
Complete_data <- merge(Complete_data, Gag, by = c('year', 'fl_reg', 'month'))
Complete_data <- merge(Complete_data, RG, by = c('year', 'fl_reg', 'month'))
Complete_data <- merge(Complete_data, RS, by = c('year', 'fl_reg', 'month'))

### Bag limit
colnames(Bags)[1] <- c("Year")
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
colnames(Income) <- c("Year", "Income")
Income <- Income[which(Income$Year %in% 2004:2023),]
Annual_CPI <- CPI%>%group_by(Year)%>%summarize(CPI = mean(CPI)) ## aggregate CPI to annual level
Income$IR_CPI <- (Annual_CPI$CPI[which(Annual_CPI$Year %in% 2004:2023)]-Annual_CPI$CPI[which(Annual_CPI$Year %in% 2003:2022)])/Annual_CPI$CPI[which(Annual_CPI$Year %in% 2003:2022)]

Inflation <- Inflation[,-14]%>%pivot_longer(2:13, names_to = "Month", values_to = "IR")
colnames(Inflation) <- c("Year", "Month", "IR")
Annual_IR <- Inflation%>%group_by(Year)%>%summarize(IR = mean(IR)) ## aggregate IR to annual level
Income$IR <- Annual_IR$IR[which(Annual_IR$Year %in% 2004:2023)]
Income$Real <- Income$Income/(1+Income$IR_CPI)
Income$Real2 <- Income$Income/(1+Income$IR/100)
#Income$Real3 <- Income_2$Real

### Relative Abundance
#### Replace any NA's in IoA dataset with SSB data
#### Replace missing indices with SSB proxies from benchmark assessments using SLR
SSB <- SSB[which(SSB$Year >1999),] ## Subset to 2000 and later
Mod <- lm(IoA$Gag~SSB$Gag)
Mod <- lm(log(IoA$RG)~SSB$`Red Grouper`)
Mod <- lm(log(IoA$RS)~SSB$`Red Snapper`)
IoA$RS[which(IoA$Year %in% c(2000,2005,2008))] <- exp(coef(Mod)[1] +coef(Mod)[2]*SSB$`Red Snapper`[which(SSB$Year %in% c(2000,2005, 2008))])
colnames(IoA) <- c("Year", 'gag', 'red grouper', 'red snapper', 'gag2', "rs2", 'gs') ## Rename columns
IoA$`red snapper`[4] <- mean(IoA$`red snapper`[c(3,5)])
IoA[24,] <- c(2023, as.numeric(IoA[23,2:7]))

### Fishable days
Fishable$Height[which(is.na(Fishable$Height))] <- 0
Fishable <- Fishable%>%
  mutate(Fishable = ifelse(Speed > 8.75 | Height > 1.2, 0, 1))%>%
  group_by(year(Date), month(Date), Region)%>%
  summarize(Fishable = mean(Fishable))%>%
  mutate(Date = as.Date(paste0(`year(Date)`, "-", `month(Date)`, "-01")))

#------------------------------------------------------------------------------#
## Harmonic regression terms
### set periods manually
per <- 365 ## period 1 is annual (365 dyas in a year)
per2 <- 182.5 ## period 2 is semi-annual (182.5 days every six months)

#------------------------------------------------------------------------------#
## Aggregate predictors and response data into single dataset
Complete_data$Date <- as.Date(paste0(Complete_data$year,"-", Complete_data$month, "-01"), format = '%Y-%m-%d')

Complete_data <- Complete_data[order(Complete_data$Date),]
Complete_data$time <- as.POSIXlt(Complete_data$Date)$yday

## First harmonic
Complete_data$sin1 <- sin(2*pi/per*Complete_data$time)
Complete_data$cos1 <- cos(2*pi/per*Complete_data$time)

## Second harmonic
Complete_data$sin2 <- sin(2*pi/per2*Complete_data$time)
Complete_data$cos2 <- cos(2*pi/per2*Complete_data$time)

## Relative abundance
### Effort
Complete_data$Year <- Complete_data$year
Complete_data$A_Gag_e <- IoA[match(Complete_data$Year, IoA$Year+1), grep('gag2', tolower(colnames(IoA)))]
Complete_data$A_RS_e <- IoA[match(Complete_data$Year, IoA$Year+1), grep('red snapper', tolower(colnames(IoA)))]
Complete_data$A_RG_e <- IoA[match(Complete_data$Year, IoA$Year+1), grep('red grouper', tolower(colnames(IoA)))]
Complete_data$A_GS_e <- IoA[match(Complete_data$Year, IoA$Year+1), grep('gs', tolower(colnames(IoA)))]

### Catch
Complete_data$A_Gag <- IoA[match(Complete_data$Year, IoA$Year), grep('gag2', tolower(colnames(IoA)))]
Complete_data$A_RS <- IoA[match(Complete_data$Year, IoA$Year), grep('red snapper', tolower(colnames(IoA)))]
Complete_data$A_RG <- IoA[match(Complete_data$Year, IoA$Year), grep('red grouper', tolower(colnames(IoA)))]
#Complete_data$A_GS <- IoA[match(Complete_data$Year, IoA$Year), grep('gray snapper', tolower(colnames(IoA)))]

## Gag bag limit
Complete_data$Bag_gag <- Bags$gag[match(Complete_data$Year, Bags$Year)]
Complete_data$Bag_RS <- Bags$`red snapper`[match(Complete_data$Year, Bags$Year)]
Complete_data$Bag_RG <- Bags$`red grouper`[match(Complete_data$Year, Bags$Year)]

## Management
Complete_data$Month <- Complete_data$month
Complete_data$M_Gag <- Days$Gag[match(paste(Complete_data$Month, Complete_data$Year), paste(Days$Month, Days$Year))]
Complete_data$M_RS <- Days$RED.SNAPPER[match(paste(Complete_data$Month, Complete_data$Year),paste(Days$Month, Days$Year))]
Complete_data$M_RG <- Days$Red.Grouper[match(paste(Complete_data$Month, Complete_data$Year),paste(Days$Month, Days$Year))]

## Seasons
Complete_data$S_Gag <- Season$Gag[match(Complete_data$Year, Season$Year)]
Complete_data$S_RG <- Season$Red.Grouper[match(Complete_data$Year, Season$Year)]
Complete_data$S_RS <- Season$RED.SNAPPER[match(Complete_data$Year, Season$Year)]

##Income
Complete_data$Income <- Income$Real2[match(Complete_data$Year, Income$Year)]

## Fuel
Fuel$Date <- as.Date(paste0("01-",Fuel$Month), format = '%d-%y-%b')
Fuel$Month <- month(Fuel$Date)
Fuel$Year <- year(Fuel$Date)
Complete_data$Fuel <- Fuel$Fuel_CPI[match(paste(Complete_data$Month, Complete_data$Year),paste(Fuel$Month,Fuel$Year))]

## Regional ID
Complete_data <- Complete_data[which(Complete_data$fl_reg %in% c(1,2)),]
Complete_data$Region <- "Panhandle"
Complete_data$Region[which(Complete_data$fl_reg==2)] <- "Peninsula"

## Fishable days
Complete_data$Fishable <- Fishable$Fishable[match(paste(Complete_data$Date, Complete_data$fl_reg), paste(Fishable$Date, Fishable$Region))]
## Fuel to income ratio
Complete_data$FIR <- Complete_data$Income/1e4/Complete_data$Fuel

## Log-season length
Complete_data$S_GagL <- (Complete_data$M_Gag)*log(Complete_data$S_Gag)
Complete_data$S_RSL <- (Complete_data$M_RS)*log(Complete_data$S_RS)
Complete_data$S_RGL <- (Complete_data$M_RG)*log(Complete_data$S_RG)

## Google searches for keywords
Gag_Searches$Date <- as.Date(paste0(Gag_Searches$Month,"-01"))
RS_Searches$Date <- as.Date(paste0(RS_Searches$Month,"-01"))
RG_Searches$Date <- as.Date(paste0(RG_Searches$Month,"-01"))
Grouper_Searches$Date <- as.Date(paste0(Grouper_Searches$Month,"-01"))
Snapper_Searches$Date <- as.Date(paste0(Snapper_Searches$Month,"-01"))
Complete_data$Gag_searches <- Gag_Searches$gag...Florida.[match(Complete_data$Date, Gag_Searches$Date)]
Complete_data$RG_searches <- RG_Searches$red.grouper...Florida.[match(Complete_data$Date, RG_Searches$Date)]
Complete_data$RS_searches <- RS_Searches$red.snapper...Florida.[match(Complete_data$Date, RS_Searches$Date)]
Complete_data$Year <- year(Complete_data$Date)- min(year(Complete_data$Date))+1

## write csv file for analysis
write.csv(Complete_data, "MRIP Effort and Catch data.csv", quote = F, row.names = F)

