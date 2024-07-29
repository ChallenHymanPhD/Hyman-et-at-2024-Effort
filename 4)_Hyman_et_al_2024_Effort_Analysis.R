#------------------------------------------------------------------------------#
################################### Description ################################
#------------------------------------------------------------------------------#
# This file generates effort analysis and creates all figures and tables found
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
suppressMessages(library(ggrepel))
suppressMessages(library(xtable))
options(width = 80)
suppressMessages(library(grid))
suppressMessages(library(ggpubr))
suppressMessages(library(RColorBrewer))

## Modeling
suppressMessages(library(rstan))
suppressMessages(library(cmdstanr))
suppressMessages(library(forecast))

## Mapping
suppressMessages(library(mapview))
suppressMessages(library(sf))
suppressMessages(library(sp))

## File reading
suppressMessages(library(readxl))

## Clear out old files in R
rm(list=ls(all=TRUE)) 

#------------------------------------------------------------------------------#
##################################### User-defined functions ###################
#------------------------------------------------------------------------------#

## 'Not in' function
`%nin%` <- Negate(`%in%`)

## Density plot function
Density_maping <- function(x, color1, color2,bw = "nrd0", adjust = 1){
  Dens <- density(x, n = 2^12, bw = bw, adjust = adjust)
  Dens_data <- data.frame(x=Dens$x, y = Dens$y)
  Dens_plot <- ggplot(data = Dens_data, aes(x = x, y = y))+
    geom_segment(aes(xend = x, yend = 0, colour = y), show.legend = F) + 
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())+
    geom_line(lwd=0.5)+
    ylab("Density")+
    scale_color_gradient(low = color2, high = color1)
  return(Dens_plot)
}

### Set ggplot themes for plotting
Supplemental_theme <- function(){theme_bw(base_family = 'serif')%+replace%
    theme(axis.text = element_text(size = 12, color = 1, family = 'serif'),
          axis.title = element_text(size = 12, color = 1, family = 'serif'),
          strip.text = element_text(size = 14, margin = margin(0.25,0,0.25,0, "cm")),
          #axis.title.y = element_text(hjust=-5, angle = 90),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14)
    )}


My_theme <- function(){
  theme_bw(base_family = 'serif')%+replace%
    theme(axis.text = element_text(size = 16, color = 1, family = 'serif'),
          axis.title = element_text(size = 18, color = 1, family = 'serif'),
          strip.text = element_text(size = 18, margin = margin(0.25,0,0.25,0, "cm")),
          #axis.title.y = element_text(hjust=-5, angle = 90),
          legend.text = element_text(size = 16),
          legend.title = element_text(size = 18)
    )}
theme_set(My_theme())
#------------------------------------------------------------------------------#
## Load data
All_Data <- read.csv("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/main/MRIP%20Effort%20and%20Catch%20data.csv")

### Format
All_Data$Bag_gag[which(is.na(All_Data$Bag_gag))] <- 2
All_Data$Bag_RS[which(is.na(All_Data$Bag_RS))] <- 2
All_Data$Bag_RG[which(is.na(All_Data$Bag_RG))] <- 2

### For counterfactuals
All_Data$CPUE_Gag_lag_unchanged <- All_Data$CPUE_Gag_lag
All_Data$CPUE_RG_lag_unchanged<- All_Data$CPUE_RG_lag
All_Data$CPUE_RS_lag_unchanged <- All_Data$CPUE_RS_lag

All_Data$CPUE_Gag_lag <- (All_Data$CPUE_Gag_lag)*All_Data$M_Gag
All_Data$CPUE_RG_lag<- (All_Data$CPUE_RG_lag)*All_Data$M_RG
All_Data$CPUE_RS_lag <- (All_Data$CPUE_RS_lag)*All_Data$M_RS

All_Data$Date <- as.Date(All_Data$Date)
All_Data$Income <- All_Data$Income/1e4
All_Data$Vessels <- All_Data$vessels/1e5
    
All_Data$M_Both <- All_Data$M_RS_Gag     
## Set training and testing data
set.seed(1234)
All_Data <- All_Data[which(All_Data$year >2003),]
Test_data <- sample(1:nrow(All_Data), 50, replace = F)
Test <- All_Data
Train <- All_Data[-Test_data,]
Test$Prediction <- "In-sample"
Test$Prediction[Test_data] <- "Out-of-sample"
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
## Run Model
## Note: Gag = Gag Grouper, RG = Red Grouper, RS = Red Snapper
### General Effort
run_model <- F
if(run_model ==T){
  Effort_prior <- c(prior(normal(0,10), class = Intercept),
                    prior(normal(0,10), class = b),
                    prior(normal(0,10), class = Intercept, dpar = shape))
  
  Effort_Model <- brm(bf(Trips ~ 
                           Region*M_Gag +
                           Region*M_RS + 
                           Region*S_GagL +
                           Region*S_RSL +  
                           Region*sin1 + 
                           Region*sin2 + 
                           Region*cos1 + 
                           Region*cos2 +
                           Region*CPUE_Gag_lag + 
                           Region*CPUE_RG_lag +
                           Region*CPUE_RS_lag +
                           Region*FIR +
                           Region*Wind +
                           Region*Sales,
                         shape ~ Region*sin1 + 
                           Region*sin2 + 
                           Region*cos1 +
                           Region*cos2),
                      data = na.omit(Train), chains = 1, prior = Effort_prior, family = Gamma(link = "log"))
  
  saveRDS(Effort_Model, file = "Effort_Model.rds")
} else {
  Effort_Model <- readRDS("Effort_Model.rds")
}

Train <- na.omit(Train)

#------------------------------------------------------------------------------#
## Figure  S3
Effort_diag <- posterior_predict(Effort_Model, Train, ndraws = 1000)%>%t()%>%as.data.frame()%>%cbind(.,Train$Region)%>%
    pivot_longer(., cols = c(1:1000), names_to = "scan", values_to = "value")
  colnames(Effort_diag)[1]<- "Region"
  Train$Effort_diag <- predict(Effort_Model, Train, probs = c(0.1, 0.9), robust = T)%>%  ## Note 80% CI
    as.data.frame()%>%.[,1]
  
  
  colors <- c("Predicted" = "#4575B4", "Observed" = "black")
  Effort_diag$limit <- c(1e7, 1e7)[match(Effort_diag$Region,c("Panhandle", "Peninsula"))]
  #Effort_diag <- Effort_diag%>%group_by(Region)%>%filter((value) < limit)
  
  Effort_diagnostics_dens <- ggplot(Train)+
    geom_line(data = Effort_diag, aes(x =(value), group = scan, col = "Predicted"),stat = 'density', alpha = 0.1, show.legend = F)+
    geom_density(aes(x = Trips, col = 'Observed'), lwd = 1, show.legend = F)+
    scale_color_manual(values = colors)+
    xlab("Angler-trips")+ylab("Kernel density")+
    facet_wrap(~Region, scales = "free", ncol = 2)+Supplemental_theme()+
    scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  Effort_diagnostics_scatter <- ggplot(Train)+
    geom_point(aes(x = (Effort_diag), y = Trips), alpha = 0.4, col = "#4575B4")+
    #geom_smooth(aes(x = exp(Effort_diag), y = Grand_effort), method = "lm", formula = y~0+x)+
    geom_abline(lwd = 1)+
    xlab("Expected angler-trips")+ylab("Observed angler-trips")+
    facet_wrap(~Region, scales = "free", ncol = 2)+Supplemental_theme()+
    scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ## Temporal autocorrelation
  Train$resid <- Train$Trips-(Train$Effort_diag)
  Effort_diagnostics_Pacf <- ggPacf(Train$resid)+ggtitle("")
  
  df_acf <- Train %>% filter(is.na(resid) == F)%>%
    group_by(Region) %>% 
    summarise(list_acf=list(pacf(resid, plot=FALSE))) %>%
    mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
    select(-list_acf) %>% 
    unnest() %>% 
    group_by(Region) %>% 
    mutate(lag=row_number() - 1)
  
  df_ci <- Train %>% 
    group_by(Region) %>% 
    summarise(ci = qnorm((1 + 0.95)/2)/sqrt(n()))
  
  Effort_diagnostics_pacf <- ggplot(df_acf, aes(x=lag, y=acf_vals)) +
    geom_bar(stat="identity", width=.05) +
    geom_hline(yintercept = 0) +
    geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
    geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
    labs(x="Lag", y="PACF") +Supplemental_theme()+
    facet_wrap(~Region, ncol = 2)
  
  
  ggarrange(Effort_diagnostics_scatter,Effort_diagnostics_dens, Effort_diagnostics_pacf, nrow = 3, labels = c("a)", "b)", "c)"))

#------------------------------------------------------------------------------#
## Figure  3 and 4
### Effort
Pred <- predict(Effort_Model, Train, probs = c(0.1, 0.9), robust = T)%>%  ## Note 80% CI
  as.data.frame()
Train$E_mean <- (Pred$Estimate)
Train$E_min  <- (Pred$Q10)
Train$E_max  <- (Pred$Q90)

ggplot(Train)+
  geom_line(aes(x = as.Date(Date), y = (Trips)),show.legend = FALSE)+
  geom_point(aes(x = as.Date(Date), y = (Trips)))+
  geom_ribbon( aes(x = as.Date(Date), ymin = E_min, ymax =E_max), fill = "#4575B4", alpha = 0.3, show.legend = F)+
  geom_line( aes(x = as.Date(Date), y = E_mean),col = "#4575B4",lwd = 1, show.legend = F)+
  facet_wrap(~Region, scales = "free_y", ncol = 1)+ylab("Effort (angler-trips)")+
  guides(size = guide_legend(title = "N interviews", override.aes = list(linetype = NA, fill = NA, alpha = NA)),
         alpha = guide_legend(override.aes = list(alpha = 0)),
         col = guide_legend(override.aes = list(size=2)))+xlab("Date")+
  scale_size(range=c(1,4))+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))
                     
Pred <- predict(Effort_Model, Test, probs = c(0.1, 0.9), robust = T)%>%  ## Note 80% CI
  as.data.frame()

Test$Date <- as.Date(Test$Date)
Test$E_mean <- (Pred$Estimate)
Test$E_min  <- (Pred$Q10)
Test$E_max  <- (Pred$Q90)
Test <- na.omit(Test)
Test$Status <- "Outside CI"
Test$Status[which(Test$Trips >= Test$E_min & Test$Trips <= Test$E_max)] <- "Within CI"
Test_plot <- Test[order(Test$Trips),]
Test_plot <- Test_plot[which(Test$Prediction =="Out-of-sample"),]
Test_plot <-Test_plot%>%group_by(Region)%>%
  mutate(Rank = order(Trips))
ggplot(Test_plot)+
  geom_errorbar(aes(x = Rank, ymin = E_min, ymax =E_max, col = Status), show.legend = T, lwd = 1)+
  geom_point(aes(x = Rank, y = E_mean, col = Status), size = 3)+
  geom_point(aes(x = Rank, y = Trips), alpha = 0.5, size = 3)+
  facet_wrap(~Region, scales = "free", ncol = 1)+
  scale_size(range=c(1,4))+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  theme(strip.text.x = element_text(size = 14, 
                                                             margin = margin(0.25,0,0.25,0, "cm")),
                                 strip.text.y = element_text(size = 14, 
                                                             margin = margin(0,0.2,0,0.2, "cm")))+theme(legend.position = "top")+ylab("Angler-trips")+labs(color = "Withheld observation:")

length(which(Test_plot$Status == "Within CI"))/length(Test_plot$Status)
Test_plot%>%
  summarize(perc = length(which(Status == "Within CI"))/length(Status))
#------------------------------------------------------------------------------#
## Effort Tables
# NOTE: Data genertaed here are taken and subsequently inserted into latex 
# tables produced in Overleaf. The code below replicates the raw values but does
# not create the complete data files needed (i.e., it doesn't create the 
# decriptions).
## Effort Tables
Effort_preds <- as_draws_matrix(Effort_Model)%>%as.data.frame()%>%.[,-ncol(.)]%>%.[,-ncol(.)]
Effort_supplemental_tables <-apply(Effort_preds, 2, function(x){quantile(x,c(0.1, 0.5, 0.9))})
mu_terms <- -grep("shape", colnames(Effort_preds))
sigma_terms <- grep("shape", colnames(Effort_preds))

mu_supp <- Effort_supplemental_tables[,mu_terms]
mu_supp <-mu_supp[,c(
  1,2,                                                                       
  grep("CPUE_Gag", colnames(mu_supp)),
  grep("CPUE_RG", colnames(mu_supp)),
  grep("CPUE_RS", colnames(mu_supp)),
  grep("M_Gag", colnames(mu_supp)),
  grep("M_RS", colnames(mu_supp)),
  grep("S_Gag", colnames(mu_supp)),
  grep("S_RS", colnames(mu_supp)),
  grep("Sales", colnames(mu_supp)),
  grep("FIR", colnames(mu_supp)),
  grep("Wind", colnames(mu_supp)),
  grep("sin1", colnames(mu_supp)),
  grep("cos1", colnames(mu_supp)),
  grep("sin2", colnames(mu_supp)),
  grep("cos2", colnames(mu_supp))
)]%>%t()%>%round(.,3)

sigma_supp <- Effort_supplemental_tables[,sigma_terms]
sigma_supp <-sigma_supp[,c(
  1,2,                                                                       
  grep("sin1", colnames(sigma_supp)),
  grep("cos1", colnames(sigma_supp)),
  grep("sin2", colnames(sigma_supp)),
  grep("cos2", colnames(sigma_supp))                                                               
)]%>%t()%>%round(.,3)

mu_names <- c("$PH$", "$PN$",
              "$Index_{Gag}$", 
              "$PN:Index_{Gag}$", 
              "$Index_{RG}$",
              "$PN:Index_{RG}$",
              "$Index_{RS}$",
              "$PN:Index_{RS}$",
              "$Open_{Gag}$",
              "$PN:Open_{Gag}$", 
              "$Open_{RS}$",
              "$PN:Open_{RS}$",
              "$Season_{Gag}$",
              "$PN:Season_{Gag}$",
              "$Season_{RS}$",
              "$PN:Season_{RS}$",
              "$Sales$", 
              "$PN:Sales$",
              "$Ratio$", 
              "$PN:Ratio$",
              "$Wind$", 
              "$PN:Wind$",
              "$sin_{12}$",
              "$PN:sin_{12}$", 
              "$cos_{12}$", 
              "$PN:cos_{12}$",
              "$sin_{6}$",
              "$PN:sin_{6}$",
              "$cos_{6}$",
              "$PN:cos_{6}$")



mu_description <- c("Intercept of mean (i.e. effect of Panhandle)",
                    "Effect of Peninsula relative to reference intercept",
                    "Effect of Gag CPUE in the Panhandle from the prior year",
                    "Effect of Gag CPUE from prior year in the Peninsula relative to the Panhandle",
                    "Effect of RG CPUE from prior year in the Panhandle",
                    "Effect of RG CPUE from prior year in the Peninsula relative to the Panhandle",
                    "Effect of RS CPUE from prior year in the Panhandle",
                    "Effect of RS CPUE from prior year in the Peninsula relative to the Panhandle",
                    "Effect of the fraction of a month open to Gag harvest in the Panhandle",
                    "Effect of the fraction of a month open to Gag harvest in the Peninsula relative to the Panhandle",
                    "Effect of the fraction of a month open to RS harvest in the Panhandle",
                    "Effect of the fraction of a month open to RS harvest in the Peninsula relative to the Panhandle",
                    "Effect of the log-length of the Gag season when Gag is open to harvest in the Panhandle",
                    "Effect of the log-length of the Gag season when Gag is open to harvest in the Peninsula relative to the Panhandle",
                    "Effect of the log-length of the RS season when RS is open to harvest in the Panhandle",
                    "Effect of the log-length of the RS season when RS is open to harvest in the Peninsula relative to the Panhandle",
                    "Effect of the number of recreational 12 month saltwater fishing licenses sold in Florida from prior year in the Panhandle",
                    "Effect of the number of recreational 12 month saltwater fishing licenses sold in the Peninsula relative to the Panhandle",
                    "Effect of the income-fuel ratio (annual scale) in the Panhandle",
                    "Effect of the income-fuel ratio (annual scale) in the Peninsula relative to the Panhandle",
                    "Effect of mean monthly wind speed in the Panhandle",
                    "Effect of mean monthly wind speed in the Peninsula relative to the Panhandle",
                    "Effect of the annual sinusoidal term (12-month  periodicity) in the Panhandle",
                    "Effect of the annual sinusoidal term (12-month  periodicity) in the Peninsula relative to the Panhandle",
                    "Effect of the annual cosinusoidal term (12-month  periodicity) in the Panhandle",
                    "Effect of the annual cosinusoidal term (12-month  periodicity) in the Peninsula relative to the Panhandle",
                    "Effect of the semi-annual sinusoidal term (6-month  periodicity) in the Panhandle",
                    "Effect of the semi-annual sinusoidal term (6-month  periodicity) in the Peninsula relative to the Panhandle",
                    "Effect of the semi-annual cosinusoidal term (6-month  periodicity) in the Panhandle",
                    "Effect of the semi-annual cosinusoidal term (6-month  periodicity) in the Peninsula relative to the Panhandle")


sigma_names <-c("PH", "PN",
                "$sin_{12}$",
                "$PN:sin_{12}$", 
                "$cos_{12}$", 
                "$PN:cos_{12}$",
                "$sin_{6}$",
                "$PN:sin_{6}$",
                "$cos_{6}$",
                "$PN:cos_{6}$")

sigma_description <- c("Intercept of shape parameter (i.e. effect of Panhandle)",
                       "Effect of Peninsula relative to reference intercept",
                       "Effect of the annual sinusoidal term (12-month  periodicity) in the Panhandle",
                       "Effect of the annual sinusoidal term (12-month  periodicity) in the Peninsula relative to the Panhandle",
                       "Effect of the annual cosinusoidal term (12-month  periodicity) in the Panhandle",
                       "Effect of the annual cosinusoidal term (12-month  periodicity) in the Peninsula relative to the Panhandle",
                       "Effect of the semi-annual sinusoidal term (6-month  periodicity) in the Panhandle",
                       "Effect of the semi-annual sinusoidal term (6-month  periodicity) in the Peninsula relative to the Panhandle",
                       "Effect of the semi-annual cosinusoidal term (6-month  periodicity) in the Panhandle",
                       "Effect of the semi-annual cosinusoidal term (6-month  periodicity) in the Peninsula relative to the Panhandle")



mu_params <- paste0("$\\beta_{",0:(nrow(mu_supp)-1),"}$")
sigma_params <- paste0("$\\rho_{",0:(nrow(sigma_supp)-1),"}$")

mu_supp <- cbind(mu_names, mu_params, mu_description, mu_supp)
sigma_supp <- cbind(sigma_names, sigma_params, sigma_description, sigma_supp)
colnames(mu_supp)[1:3] <- colnames(sigma_supp)[1:3] <- c("Predictor", "Regression Coefficient", "Description")

Supplemental_table <- rbind(mu_supp, sigma_supp)
rownames(Supplemental_table) <- NULL
Signif <- ifelse(sign(as.numeric(Supplemental_table[,4])) == sign(as.numeric(Supplemental_table[,6])), "*", "")
Supplemental_table[,2] <- paste0(Supplemental_table[,2], Signif)

print(xtable(Supplemental_table),only.contents=TRUE, include.rownames=FALSE, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      sanitize.text.function = identity)

mu_main <- Effort_preds[,mu_terms]
mu_main <- mu_main[,c(
  1,2,                                                                       
  grep("CPUE_Gag", colnames(mu_main)),
  grep("CPUE_RG", colnames(mu_main)),
  grep("CPUE_RS", colnames(mu_main)),
  grep("M_Gag", colnames(mu_main)),
  grep("M_RS", colnames(mu_main)),
  grep("S_Gag", colnames(mu_main)),
  grep("S_RS", colnames(mu_main)),
  grep("Sales", colnames(mu_main)),
  grep("FIR", colnames(mu_main)),
  grep("Wind", colnames(mu_main)),
  grep("sin1", colnames(mu_main)),
  grep("cos1", colnames(mu_main)),
  grep("sin2", colnames(mu_main)),
  grep("cos2", colnames(mu_main))
)]

for (i in seq(1, (ncol(mu_main)-1), 2)){
  mu_main[,i+1] <- mu_main[,i+1] + mu_main[,i]
}

PH <- c(-grep("Peninsula", colnames(mu_main)))
PN <- grep("Peninsula", colnames(mu_main))

mu_main_PH <- mu_main[,PH]%>%apply(., 2, function(x){quantile(x,c(0.1, 0.5, 0.9))})%>%t()%>%round(.,3)
mu_main_PN <- mu_main[,PN]%>%apply(., 2, function(x){quantile(x,c(0.1, 0.5, 0.9))})%>%t()%>%round(.,3)

Signif_PH <- ifelse(sign(as.numeric(mu_main_PH[,1])) == sign(as.numeric(mu_main_PH[,3])), "*", "")
Signif_PN <- ifelse(sign(as.numeric(mu_main_PN[,1])) == sign(as.numeric(mu_main_PN[,3])), "*", "")

PH_names <- paste0("$\\beta_{", gsub("\\$", "", mu_names[PH]), "}$", Signif_PH)
PN_names <- paste0("$\\beta_{", gsub("\\$", "", mu_names[PN]), "}$", Signif_PN)

mu_main_table <- cbind(PH_names, mu_main_PH,
                       PN_names, mu_main_PN)


sigma_main <- Effort_preds[,sigma_terms]
sigma_main <-sigma_main[,c(
  1,2,                                                                       
  grep("sin1", colnames(sigma_main)),
  grep("cos1", colnames(sigma_main)),
  grep("sin2", colnames(sigma_main)),
  grep("cos2", colnames(sigma_main))                                                                
)]

for (i in seq(1, (ncol(sigma_main)-1), 2)){
  sigma_main[,i+1] <- sigma_main[,i+1] + sigma_main[,i]
}

PH <- c(-grep("Peninsula", colnames(sigma_main)))
PN <- grep("Peninsula", colnames(sigma_main))

sigma_main_PH <- sigma_main[,PH]%>%apply(., 2, function(x){quantile(x,c(0.1, 0.5, 0.9))})%>%t()%>%round(.,3)
sigma_main_PN <- sigma_main[,PN]%>%apply(., 2, function(x){quantile(x,c(0.1, 0.5, 0.9))})%>%t()%>%round(.,3)

Signif_PH <- ifelse(sign(as.numeric(sigma_main_PH[,1])) == sign(as.numeric(sigma_main_PH[,3])), "*", "")
Signif_PN <- ifelse(sign(as.numeric(sigma_main_PN[,1])) == sign(as.numeric(sigma_main_PN[,3])), "*", "")

PH_names <- paste0("$\\rho_{", gsub("\\$", "", sigma_names[PH]), "}$", Signif_PH)
PN_names <- paste0("$\\rho_{", gsub("\\$", "", sigma_names[PN]), "}$", Signif_PN)

sigma_main_table <- cbind(PH_names, sigma_main_PH,
                          PN_names, sigma_main_PN)

Main_Table <- rbind(mu_main_table,
                    sigma_main_table)
colnames(Main_Table)[c(1, 4)] <- c("Panhandle", "Peninsula")

print(xtable(Main_Table),only.contents=TRUE, include.rownames=FALSE, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      sanitize.text.function = identity)
#------------------------------------------------------------------------------#
## Figure 6
Conditional <- All_Data%>%group_by(Region, Month)%>%
  summarize(sin1 = mean(sin1),
            cos1 = mean(cos1),
            sin2 = mean(sin2),
            cos2 = mean(cos2),
            CPUE_Gag_lag_unchanged = mean(CPUE_Gag_lag_unchanged[year == 2023], na.rm = T),
            CPUE_RG_lag_unchanged = mean(CPUE_RG_lag_unchanged[year == 2023], na.rm = T),
            CPUE_RS_lag_unchanged = mean(CPUE_RS_lag_unchanged[year == 2023], na.rm = T),
            Wind = mean(Wind, na.rm = T),
            Sales = mean(Sales[year == 2023], na.rm = T),
            FIR = mean(FIR[year == 2023], na.rm = T),
            M_Gag = 0,
            S_Gag = 1,
            M_RG = 0)
    
Conditional_PH <- Conditional[which(Conditional$Month==6 & Conditional$Region== "Panhandle"),]
Conditional_PN <- Conditional[which(Conditional$Month==6 & Conditional$Region== "Peninsula"),]

Conditional_PH <- expand.grid(sin1 = Conditional_PH$sin1,
                              cos1 = Conditional_PH$cos1,
                              sin2 = Conditional_PH$sin2,
                              cos2 = Conditional_PH$cos2,
                              CPUE_Gag_lag_unchanged = Conditional_PH$CPUE_Gag_lag_unchanged,
                              CPUE_RG_lag_unchanged = Conditional_PH$CPUE_RG_lag_unchanged,
                              CPUE_RS_lag_unchanged =Conditional_PH$CPUE_RS_lag_unchanged,
                              Wind = Conditional_PH$Wind,
                              Sales = Conditional_PH$Sales,
                              FIR = Conditional_PH$FIR,
                              M_Gag = 0,
                              S_Gag = 1,
                              M_RG = 0,
                              M_RS = c(0,0.5,1),
                              S_RS = seq(40,200, by = 20),
                              Region = "Panhandle")

Conditional_PN <- expand.grid(sin1 = Conditional_PN$sin1,
                              cos1 = Conditional_PN$cos1,
                              sin2 = Conditional_PN$sin2,
                              cos2 = Conditional_PN$cos2,
                              CPUE_Gag_lag_unchanged = Conditional_PN$CPUE_Gag_lag_unchanged,
                              CPUE_RG_lag_unchanged = Conditional_PN$CPUE_RG_lag_unchanged,
                              CPUE_RS_lag_unchanged =Conditional_PN$CPUE_RS_lag_unchanged,
                              Wind = Conditional_PN$Wind,
                              Sales = Conditional_PN$Sales,
                              FIR = Conditional_PN$FIR,
                              M_Gag = 0,
                              S_Gag = 1,
                              M_RG = 0,
                              M_RS = c(0,0.5,1),
                              S_RS = seq(40,200, by = 20),
                              Region = "Peninsula")

Conditional <- rbind(Conditional_PH, Conditional_PN)
Conditional$CPUE_Gag_lag <- Conditional$CPUE_Gag_lag_unchanged*Conditional$M_Gag
Conditional$CPUE_RG_lag <- Conditional$CPUE_RG_lag_unchanged*Conditional$M_RG
Conditional$CPUE_RS_lag <- Conditional$CPUE_RS_lag_unchanged*Conditional$M_RS
Conditional$S_GagL <- log(Conditional$S_Gag)*Conditional$M_Gag
Conditional$S_RSL <- log(Conditional$S_RS)*Conditional$M_RS

Pred <- posterior_epred(Effort_Model, Conditional)%>%as.data.frame()
Conditional$Q10 <- apply(Pred, 2, function(x){quantile(x,0.1)})
Conditional$Estimate <- apply(Pred, 2, function(x){quantile(x,0.5)})
Conditional$Q90 <- apply(Pred, 2, function(x){quantile(x,0.9)})

ggplot(Conditional)+
  geom_line(aes(x = S_RS, y = Estimate, col = as.factor(M_RS)), lwd = 1)+
  geom_ribbon(aes(x = S_RS, ymin = Q10, ymax = Q90, fill = as.factor(M_RS)), alpha = 0.5)+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  scale_fill_viridis_d(direction = -1)+
  scale_color_viridis_d(direction = -1)+ylab("Expected effort (angler-trips)")+ xlab("Red snapper season length (days)")+
  facet_wrap(~Region, ncol = 1, scales = "free")+theme(legend.position = "top")+guides(fill = guide_legend("Fraction of month open to harvest red snapper"),
                                                                      col = guide_legend("Fraction of month open to harvest red snapper"))
#------------------------------------------------------------------------------#\
## Figure S4
mu_Standardized <- Effort_preds[,mu_terms]
mu_Standardized <- mu_Standardized[,c(
  grep("CPUE_Gag", colnames(mu_Standardized)),
  grep("CPUE_RG", colnames(mu_Standardized)),
  grep("CPUE_RS", colnames(mu_Standardized)),
  grep("M_Gag", colnames(mu_Standardized)),
  grep("M_RS", colnames(mu_Standardized)),
  grep("S_Gag", colnames(mu_Standardized)),
  grep("S_RS", colnames(mu_Standardized)),
  grep("Sales", colnames(mu_Standardized)),
  grep("FIR", colnames(mu_Standardized)),
  grep("Wind", colnames(mu_Standardized)),
  grep("sin1", colnames(mu_Standardized)),
  grep("cos1", colnames(mu_Standardized)),
  grep("sin2", colnames(mu_Standardized)),
  grep("cos2", colnames(mu_Standardized))
)]

for (i in seq(1, (ncol(mu_Standardized)-1), 2)){
  mu_Standardized[,i+1] <- mu_Standardized[,i+1] + mu_Standardized[,i]
}

Standards <- All_Data%>%group_by(Region)%>%
  summarize(CPUE_Gag_lag = sd(CPUE_Gag_lag, na.rm = T),
            CPUE_RG_lag = sd(CPUE_RG_lag, na.rm = T),
            CPUE_RS_lag = sd(CPUE_RS_lag, na.rm = T),
            M_Gag = sd(M_Gag),
            M_RS = sd(M_RS),
            S_GagL = sd(S_GagL),
            S_RSL = sd(S_RSL),
            Sales = sd(Sales),
            FIR = sd(FIR),
            Wind = sd(Wind),
            sin1 = sd(sin1),
            cos1 = sd(cos1),
            sin2 = sd(sin2),
            cos2 = sd(cos2)
  )%>%pivot_longer(., 2:ncol(.), names_to = "Pred", values_to = "Standard")
Standards <- Standards[c(grep("CPUE_Gag", Standards$Pred),
                         grep("CPUE_RG", Standards$Pred),
                         grep("CPUE_RS", Standards$Pred),
                         grep("M_Gag", Standards$Pred),
                         grep("M_RS", Standards$Pred),
                         grep("S_Gag", Standards$Pred),
                         grep("S_RS", Standards$Pred),
                         grep("Sales", Standards$Pred),
                         grep("FIR", Standards$Pred),
                         grep("Wind", Standards$Pred),
                         grep("sin1", Standards$Pred),
                         grep("cos1", Standards$Pred),
                         grep("sin2", Standards$Pred),
                         grep("cos2", Standards$Pred)),]
for (i in 1:ncol(mu_Standardized)){
  mu_Standardized[,i] <- mu_Standardized[,i]*Standards$Standard[i]
}

mu_Standard <- mu_Standardized%>%
  apply(., 2, function(x){quantile(x, c(0.1, 0.5, 0.9))})%>%t()
Standards <- cbind(Standards, mu_Standard)

Standards$Predictor <- c("$Index_{Gag}$", 
"$Index_{Gag}$", 
"$Index_{RG}$",
"$Index_{RG}$",
"$Index_{RS}$",
"$Index_{RS}$",
"$Open_{Gag}$",
"$Open_{Gag}$", 
"$Open_{RS}$",
"$Open_{RS}$",
"$Season_{Gag}$",
"$Season_{Gag}$",
"$Season_{RS}$",
"$Season_{RS}$",
"$Sales$", 
"$Sales$",
"$Ratio$", 
"$Ratio$",
"$Wind$", 
"$Wind$",
"$sin_{12}$",
"$sin_{12}$", 
"$cos_{12}$", 
"$cos_{12}$",
"$sin_{6}$",
"$sin_{6}$",
"$cos_{6}$",
"$cos_{6}$")

Standards$Predictor <- as.factor(Standards$Predictor)%>%factor(., levels = rev(unique(Standards$Predictor)))
Standards$Signif <- ifelse(sign(Standards$`10%`) == sign(Standards$`90%`), "Excludes", "Includes")

ggplot(Standards[which(Standards$Signif == "Excludes"),])+
  geom_segment(aes(y = Predictor, x = `10%`, xend = `90%`), lwd = 2.5, alpha = 0.5)+
  geom_point(aes(y = Predictor, x = `50%`), size = 3.5)+
  scale_y_discrete(labels = TeX(paste(Standards$Predictor)))+
  geom_vline(aes(xintercept = 0), lwd = 1)+
  facet_wrap(~Region, ncol = 1, scales = "free_y")+xlab("Standardized regression coefficient")                   
