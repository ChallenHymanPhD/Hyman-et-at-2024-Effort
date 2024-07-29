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
# This file was written by A. Challen Hyman, PhD, on July 29th, 2024
#------------------------------------------------------------------------------#
##################################### Libraries ################################
#------------------------------------------------------------------------------#
## Syntax packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(tidyverse))

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

## List of reef species
common <- c("GAG", "RED GROUPER", "RED SNAPPER", "GRAY SNAPPER", 
            "GREATER AMBERJACK", "VERMILION SNAPPER", "YELLOWTAIL SNAPPER", 
            "GRAY TRIGGERFISH", "LANE SNAPPER", "MUTTON SNAPPER", 
            "LESSER AMBERJACK", "ALMACO JACK", "BANDED RUDDERFISH", "HOGFISH", 
            "GOLDEN TILEFISH", "BLUELINE TILEFISH", "GOLDFACE TILEFISH", 
            "SCAMP", "BLACK GROUPER", "YELLOWEDGE GROUPER", "SNOWY GROUPER", 
            "SPECKLED HIND", "YELLOWMOUTH GROUPER", "YELLOWFIN GROUPER", 
            "WARSAW GROUPER", "GOLIATH GROUPER", "QUEEN SNAPPER", 
            "BLACKFIN SNAPPER", "CUBERA SNAPPER", "SILK SNAPPER", 
            "WENCHMAN", "grouper genus (epinephelus)", 
            "grouper genus (mycteroperca)", "snapper family")
reg_1 <- paste(c(123,129,29,33,37,45,5,91,113,131))                             ## Hyman edits manually specify counties in fl_reg 1
reg_2 <- paste(c(101,103,115,15,17,21,53,71,75,81,57))                          ## Hyman edits manually specify counties in fl_reg 2
st<-as.character(12)                                                            ## Florida FPS
styr<-as.character(2003)                                                        ## Start year
endyr<-as.character(2023)                                                       ## End year
wave<-as.character(c(1,2,3,4,5,6))                                              ## Waves
common<-tolower(common)

#------------------------------------------------------------------------------#
################################ Data aggregation ##############################
#------------------------------------------------------------------------------# 
dataset<-NULL
temp<-NULL
temp1<-NULL
rbind2<- function(input1, input2){
  if(!is.null(ncol(input1))){
    n.input1 <- ncol(input1)
    n.input2 <- ncol(input2)
    
    if (n.input2 < n.input1) {
      TF.names <- which(names(input2) %in% names(input1))         
      column.names <- names(input2[, TF.names])
    } 
    else {TF.names <- which(names(input1) %in% names(input2))
    column.names <- names(input1[, TF.names])}
    return(rbind(input1[, column.names], input2[, column.names]))
  }
  if(is.null(ncol(input1))) return(rbind(input1,input2))
}
for(yr in styr:endyr){
  for (j in 1:as.numeric(length(wave))){ 
    #Get catch
    wv<-wave[j] 
    t3<-read.csv(paste(DIRECTORY,"/","catch_",yr,wv,".csv",sep=""),
                 colClasses=c("character"),na.strings=".")
    t3<-t3[t3$ST %in% c(st),]
    names(t3)<-tolower(names(t3))
    temp<-rbind2(temp,t3)
    #get trips
    t4<-read.csv(paste(DIRECTORY,"/","trip_",yr,wv,".csv",sep=""),
                 colClasses=c("character"),na.strings=".")
    t4<-t4[t4$ST %in% c(st),]
    names(t4)<-tolower(names(t4))    
    temp1<-rbind2(temp1,t4)
  }
}
convtolow<-function(x){
  for(i in 1:ncol(x)) x[,i]<-tolower(x[,i])
  return(x)
}
temp<-convtolow(temp)
temp1<-convtolow(temp1)
temp<-temp[,c("common","strat_id","psu_id","st","id_code","sp_code",
              "claim","release","harvest","tot_len_a","wgt_a","tot_len_b1","wgt_b1","fl_reg","tot_cat",
              "wgt_ab1","tot_len","landing")]
temp<-temp[order(temp$strat_id,temp$psu_id,temp$id_code),]
temp1<-temp1[order(temp1$strat_id,temp1$psu_id,temp1$id_code),]

dataset<-merge(temp1,temp,by.x=c("strat_id","psu_id","id_code","st"),
               by.y=c("strat_id","psu_id","id_code","st"),all.x=FALSE,all.y=FALSE)
dataset$common<-as.character(dataset$common)
dataset$common<-ifelse(is.na(dataset$common),"",dataset$common)

if(!any(colnames(dataset) == "fl_reg")){                                        ## Hyman edits create fl_reg column for regions before 2013
  dataset$fl_reg <- as.numeric(dataset$fl_reg.x)
}
dataset$fl_reg[which(dataset$cnty %in% c(reg_1))] <- 1                          ## Hyman edits to ensure counties are correctly placed in FL_reg 1
dataset$fl_reg[which(dataset$cnty %in% c(reg_2))] <- 2                          ## Hyman edits to ensure counties are correctly placed in FL_reg 2

## Subset data
dataset_subset <- dataset[which(paste(dataset$st)=="12" &
                                paste(dataset$year) %in% paste(seq(as.numeric(styr),as.numeric(endyr)))&
                                dataset$fl_reg %in% c("1","2")),]

dataset_subset$tot_cat<-as.numeric(dataset_subset$tot_cat)
dataset_subset$landing<-as.numeric(dataset_subset$landing)
dataset_subset$claim<-as.numeric(dataset_subset$claim)
dataset_subset$harvest<-as.numeric(dataset_subset$harvest)
dataset_subset$release<-as.numeric(dataset_subset$release)
dataset_subset$wgt_ab1<-as.numeric(dataset_subset$wgt_ab1)
dataset_subset$wp_int<-as.numeric(dataset_subset$wp_int)
dataset_subset$dcomm<-dataset_subset$common

## Retain all reef trips
dataset_subset$Retain <- 0
## If anglers declared a reef species as a primary or secondary target, retian row
dataset_subset$Retain[which(dataset_subset$prim1_common%in%common | dataset_subset$prim2_common%in%common)] <- 1
## If anglers caught a reef species, also retain row
dataset_subset$Reef_Catch<-ifelse(dataset_subset$common%in%common,dataset_subset$tot_cat,0)
dataset_subset$Retain[which(dataset_subset$Reef_Catch > 0)] <- 1
dataset_subset <- dataset_subset[which(dataset_subset$Retain >0),]
results <- dataset_subset
cols_to_keep <- c("strat_id",
                  "psu_id",
                  "id_code",
                  "st",
                  "fl_reg",
                  "Species",
                  "prim2_common",
                  "prim1_common",
                  "cnty",
                  "county",
                  "year",
                  "month",
                  "sub_reg",
                  "mode_fx",
                  "area_x",
                  "wp_int",
                  "hrsf",
                  "party",
                  "common",
                  "landing",
                  "release",
                  "wgt_ab1")
results <- results[,which(colnames(results) %in% cols_to_keep)]
results <- results[,na.omit(match(cols_to_keep, colnames(results)))]
results$total <- as.numeric(results$landing)+as.numeric(results$release)
output <- results%>% pivot_wider(names_from = common,
                                 values_from = c(total, landing, release, wgt_ab1))
Reef_cols <- NULL
common <- c(common,"epinephelus", "mycteroperca") 
for (i in tolower(common)){
  Spec_cols <- grep(i, colnames(output))
  Reef_cols <- c(Reef_cols, Spec_cols)
}
output <- output[,c(1:17, Reef_cols)]
for (i in 18:ncol(output)){
  output[which(is.na(output[,i])),i] <- 0
}


output$total_gag <- output$total_gag + output$`total_grouper genus (mycteroperca)`
output$landing_gag <- output$landing_gag + output$`landing_grouper genus (mycteroperca)`
output$release_gag <- output$release_gag + output$`release_grouper genus (mycteroperca)`

output$`total_red grouper` <- output$`total_red grouper` + output$`total_grouper genus (epinephelus)`
output$`landing_red grouper` <- output$`landing_red grouper` + output$`landing_grouper genus (epinephelus)`
output$`release_red grouper` <- output$`release_red grouper` + output$`release_grouper genus (epinephelus)`

## Save file
write.csv(output, "APAIS.csv", row.names = F)
