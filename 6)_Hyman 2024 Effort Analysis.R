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
# This file was written by A. Challen Hyman, PhD, on March 23rd, 2024
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
All_Data$Gag_searches <- All_Data$Gag_searches/100
All_Data$RG_searches <- All_Data$RG_searches/100
All_Data$RS_searches <- All_Data$RS_searches/100

All_Data$A_Gag_e <- All_Data$A_Gag_e*All_Data$M_Gag
All_Data$A_RG_e<- All_Data$A_RG_e*All_Data$M_RG
All_Data$A_RS_e <- All_Data$A_RS_e*All_Data$M_RS

All_Data <- All_Data[,c("Region",
                        "Trips",
                        "A_Gag_e",
                        "A_RG_e",
                        "A_RS_e",
                        "M_Gag",
                        "M_RG",
                        "M_RS",
                        "S_GagL",
                        "S_RGL",
                        "S_RSL",
                        "Fishable",
                        "FIR",
                        "sin1",
                        "sin2",
                        "cos1",
                        "cos2",
                        "Gag_searches",
                        "RG_searches",
                        "RS_searches",
                        "Grouper_searches",
                        "Snapper_searches)]

## Set training and testing data
set.seed(1234)
Test_data <- sample(1:nrow(All_Data), 50, replace = F)
Test <- All_Data
Train <- All_Data[-Test_data,]
Test$Prediction <- "In-sample"
Test$Prediction[Test_data] <- "Out-of-sample"
#------------------------------------------------------------------------------#
## Run Model
## Note: Gag = Gag Grouper, RG = Red Grouper, RS = Red Snapper
### General Effort
Train_mu_matrix <- model.matrix(~ Region*A_Gag_e + Region*A_RG_e +
                                  Region*A_RS_e +
                                  Region*M_Gag + Region*M_RG + 
                                  Region*M_RS + Region*S_GagL + 
                                  Region*S_RGL + Region*S_RSL + 
                                  Region*Fishable + 
                                  Region*sin1 + Region*sin2 + 
                                  Region*cos1 + Region*cos2 + 
                                  Region*Grouper_searches + 
                                  Region*Snapper_searches + 
                                  Region*FIR, data = na.omit(Train))

Test_mu_matrix <- model.matrix(~ Region*A_Gag_e + Region*A_RG_e +
                                 Region*A_RS_e +
                                 Region*M_Gag + Region*M_RG + 
                                 Region*M_RS + Region*S_GagL + 
                                 Region*S_RGL + Region*S_RSL + 
                                 Region*Fishable + 
                                 Region*sin1 + Region*sin2 + 
                                 Region*cos1 + Region*cos2 + 
                                 Region*Grouper_searches + 
                                 Region*Snapper_searches + 
                                 Region*FIR, data = na.omit(Test))
                                 Region*FIR, data = na.omit(Test))

Train_sigma_matrix <- model.matrix(~ Region, data = na.omit(Train))
Test_sigma_matrix <- model.matrix(~ Region, data = na.omit(Test))

## Complete data frame
DataList <- list(
  "T" = nrow(Train_mu_matrix),
  "OOS" = nrow(Test_mu_matrix),
  "S" = ncol(Train_sigma_matrix),
  "E" = ncol(Train_mu_matrix),
  "Design_effort" = Train_mu_matrix,
  "Pred_effort" = Test_mu_matrix,
  "S_eff" = Train_sigma_matrix,
  "S_pred" = Test_sigma_matrix,
  "Effort" = log(na.omit(Train)[,2])
)
N <- 2000
Effort_Model_URL <- url("https://raw.githubusercontent.com/ChallenHymanPhD/Hyman-et-at-2024-Effort/Stan-files/Effort%20Complete%20Model.stan")
Effort_Model_txt <- readLines(Effort_Model_URL)
Effort_Model_tmpf <- write_stan_file(Effort_Model_txt)

Effort_Model <- stan(Effort_Model_tmpf, data = DataList, chains = 4, iter = N, control = list(adapt_delta = 0.99), refresh = max(N/100, 5)) 
Effort_Model@stanmodel@dso <- new("cxxdso")
saveRDS(Effort_Model, file = "Effort_Model.rds")  


Train <- na.omit(Train)

## Extract model scans and parameter estimates
Mod <- rstan::extract(Effort_Model)
Train$E_mean <- exp(apply(Mod$pred_Effort, 2, median))
Train$E_min  <- exp(apply(Mod$pred_Effort, 2, function(x){quantile(x, 0.1)}))
Train$E_max  <- exp(apply(Mod$pred_Effort, 2, function(x){quantile(x, 0.9)}))

#------------------------------------------------------------------------------#
## Figure  S1
len <- dim(Mod$pred_Effort)[1]
Effort_diag <- Mod$pred_Effort%>%t()%>%as.data.frame()%>%cbind(.,Train$Region)%>%
  pivot_longer(., cols = c(1:len), names_to = "scan", values_to = "value")
colnames(Effort_diag)[1]<- "Region"

colors <- c("Predicted" = "#4575B4", "Observed" = "black")
Effort_diag$limit <- c(2e7, 2.5e7)[match(Effort_diag$Region,c("Panhandle", "Peninsula"))]
Effort_diag <- Effort_diag%>%group_by(Region)%>%filter(exp(value) < limit)

Effort_diagnostics_dens <- ggplot(Train)+
  geom_line(data = Effort_diag, aes(x = exp(value), group = scan, col = "Predicted"),stat = 'density', alpha = 0.1, show.legend = F)+
  geom_density(aes(x = Trips, col = 'Observed'), lwd = 1, show.legend = F)+
  scale_color_manual(values = colors)+
  xlab("Effort (angler-trips)")+ylab("")+
  facet_wrap(~Region, scales = "free", ncol = 1)+Supplemental_theme()+
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Effort_diagnostics_scatter <- ggplot(Train)+
  geom_point(aes(x = E_mean, y = Trips), alpha = 0.4, col = "#4575B4")+
  #geom_smooth(aes(x = exp(Effort_diag), y = Trips), method = "lm", formula = y~0+x)+
  geom_abline(lwd = 1)+
  xlab("Effort (angler-trips)")+ylab("")+
  facet_wrap(~Region, scales = "free", ncol = 1)+Supplemental_theme()+
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Temporal autocorrelation
Train$resid <- Train$Trips-Train$E_mean
Effort_diagnostics_Pacf <- ggPacf(Train$resid)+ggtitle("")
Train <- na.omit(Train)
df_acf <- Train %>% 
  group_by(Region) %>% 
  summarise(list_acf=list(pacf(resid, plot=FALSE))) %>%
  mutate(acf_vals=purrr::map(list_acf, ~as.numeric(.x$acf))) %>% 
  select(-list_acf) %>% 
  unnest() %>% 
  group_by(Region) %>% 
  mutate(lag=row_number() - 1)

df_ci <- Train %>% 
  group_by(Region) %>% 
  summarise(ci = qnorm((1 + 0.9)/2)/sqrt(n()))

Effort_diagnostics_pacf <- ggplot(df_acf, aes(x=lag, y=acf_vals)) +
  geom_bar(stat="identity", width=.05) +
  geom_hline(yintercept = 0) +
  geom_hline(data = df_ci, aes(yintercept = -ci), color="blue", linetype="dotted") +
  geom_hline(data = df_ci, aes(yintercept = ci), color="blue", linetype="dotted") +
  labs(x="Lag", y="PACF") +
  facet_wrap(~Region, ncol = 1)+Supplemental_theme()


ggarrange(Effort_diagnostics_scatter,Effort_diagnostics_dens, Effort_diagnostics_pacf, ncol = 3, labels = c("a)", "b)", "c)"))
ggsave("Hyman Figure S1.png", dpi = 600, device = "png")

#------------------------------------------------------------------------------#
## Figures  4 and 5
### Effort
ggplot(Train)+
  geom_line(aes(x = as.Date(Date), y = (Trips)),show.legend = FALSE)+
  geom_point(aes(x = as.Date(Date), y = (Trips)))+
  geom_ribbon( aes(x = as.Date(Date), ymin = E_min, ymax =E_max), fill = "#4575B4", alpha = 0.3, show.legend = F)+
  geom_line( aes(x = as.Date(Date), y = E_mean),col = "#4575B4",lwd = 1, show.legend = F)+
  facet_wrap(~Region, scales = "free_y", ncol = 1)+ylab("Effort(angler-trips)")+
  guides(size = guide_legend(title = "N interviews", override.aes = list(linetype = NA, fill = NA, alpha = NA)),
         alpha = guide_legend(override.aes = list(alpha = 0)),
         col = guide_legend(override.aes = list(size=2)))+xlab("Date")+
  scale_size(range=c(1,4))+
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
  Supplemental_theme()
ggsave("Hyman Figure 4 2024.png", device = "png", dpi = 600)

Test <- na.omit(Test)
Test$Date <- as.Date(Test$Date)
Test$E_mean <- exp(apply(Mod$oos_Effort, 2, median))
Test$E_min  <- exp(apply(Mod$oos_Effort, 2, function(x){quantile(x, 0.1)}))
Test$E_max  <- exp(apply(Mod$oos_Effort, 2, function(x){quantile(x, 0.9)}))

Test$Status <- "Outside"
Test$Status[which(Test$Trips >= Test$E_min & Test$Trips <= Test$E_max)] <- "Within"
Test_plot <- Test[order(Test$Trips),]
Test_plot <- Test_plot[which(Test$Prediction =="Out-of-sample"),]
Test_plot <-Test_plot%>%group_by(Region)%>%
  mutate(Rank = order(Trips))
ggplot(Test_plot)+
  geom_point(aes(x = Rank, y = Trips), alpha = 0.5)+
  geom_point(aes(x = Rank, y = E_mean, col = Status), alpha = 0.5)+
  geom_errorbar(aes(x = Rank, ymin = E_min, ymax =E_max, col = Status), show.legend = T, lwd = 0.5)+
  facet_wrap(~Region, scales = "free_y", ncol = 1)+
  scale_size(range=c(1,4))+theme(strip.text.x = element_text(size = 14, 
                                                             margin = margin(0.25,0,0.25,0, "cm")),
                                 strip.text.y = element_text(size = 14, 
                                                             margin = margin(0,0.2,0,0.2, "cm")))+theme(legend.position = "top")+ylab("Observed angler-trips")


Test_plot%>%
  summarize(perc = length(which(Status == "Within"))/length(Status))
ggsave("Hyman Figure 5 2024.png", device = "png", dpi = 600)


Test$Good <- ifelse(Test$Trips >= Test$E_min & Test$Trips <= Test$E_max, 1, 0)

Test%>%group_by(Prediction, Region)%>%
  summarise(nominal = mean(Good))
#------------------------------------------------------------------------------#
## Effort Tables
# NOTE: Data genertaed here are taken and subsequently inserted into latex 
# tables produced in Overleaf. The code below replicates the raw values but does
# not create the complete data files needed (i.e., it doesn't create the 
# decriptions).
Train_mu_matrix <- model.matrix(~ Region*A_Gag_e + Region*A_RG_e +
                                  Region*A_RS_e +
                                  Region*M_Gag + Region*M_RG + 
                                  Region*M_RS + Region*S_GagL + 
                                  Region*S_RGL + Region*S_RSL + 
                                  Region*Fishable + 
                                  Region*sin1 + Region*sin2 + 
                                  Region*cos1 + Region*cos2 + 
                                  Region*Grouper_searches + 
                                  Region*Snapper_searches + 
                                  Region*FIR, data = na.omit(Train))

#stan_dens(Effort_Model, pars = 'beta')
betas <- Mod$beta%>%as.matrix()%>%as.data.frame()
colnames(betas) <- colnames(Train_mu_matrix)
Betas <- apply(betas, 2, function(x){
  quantile(x, c(0.1, 0.5, 0.9))
})%>%.[,c(
  1,2,                                                                       
  grep("A_Gag_e", colnames(betas)),
  grep("A_RG_e", colnames(betas)),
  grep("A_RS_e", colnames(betas)),
  grep("M_Gag", colnames(betas)),
  grep("M_RG", colnames(betas)),
  grep("M_RS", colnames(betas)),
  grep("S_Gag", colnames(betas)),
  grep("S_RG", colnames(betas)),
  grep("S_RS", colnames(betas)),
  grep("Grouper_searches", colnames(betas)),
  grep("Snapper_searches", colnames(betas)),
  grep("FIR", colnames(betas)),
  grep("Fishable", colnames(betas)),
  grep("sin1", colnames(betas)),
  grep("cos1", colnames(betas)),
  grep("sin2", colnames(betas)),
  grep("cos2", colnames(betas))
)]%>%t()%>%round(.,3)


Effort_comp_beta <- c("$\\mu$", rep("", 37))
Effort_names_beta <- c("$PH$", "$PN$",
                       "$A_{Gag_{e}}$", 
                       "$PN:A_{Gag_{e}}$", 
                       
                       "$A_{RG_{e}}$",
                       "$PN:A_{RG_{e}}$",
                       
                       "$A_{RS_{e}}$",
                       "$PN:A_{RS_{e}}$",
                       
                       "$M_{Gag}$",
                       "$PN:M_{Gag}$", 
                       
                       "$PH:M_{RG}$", 
                       "$PN:M_{RG}$",
                       
                       "$M_{RS}$",
                       "$PN:M_{RS}$",
                       
                       "$S_{Gag}$",
                       "$PN:S_{Gag}$",
                       
                       "$S_{RG}$", 
                       "$PH:S_{RG}$", 
                       
                       "$S_{RS}$",
                       "$PN:S_{RS}$",
                       
                       "$G_{Grouper}$", 
                       "$PN:G_{Gouper}$",
                  
                       "$G_{Snapper}$",  
                       "$PN:G_{Snapper}$",
                       
                       "$R$", 
                       "$PN:R$",
                       
                       "$Fish$", 
                       "$PN:Fish$", 
                       
                       "$sin_{12}$",
                       "$PN:sin_{12}$", 
                       
                       "$cos_{12}$", 
                       "$PN:cos_{12}$",
                       
                       "$sin_{6}$",
                       "$PN:sin_{6}$",
                       
                       "$cos_{6}$",
                       "$PN:cos_{6}$")
Effort_params_beta <- c(paste0("$\\beta_{",0:37,"}$"))

Effort_supplemental_table1 <- cbind(Effort_names_beta, Effort_params_beta, Betas)
colnames(Effort_supplemental_table1)[1:2] <- c("Predictor", "Regression Coefficient")
rownames(Effort_supplemental_table1) <- NULL

Signif <- ifelse(sign(as.numeric(Effort_supplemental_table1[,3])) == sign(as.numeric(Effort_supplemental_table1[,5])), "*", "")
Effort_supplemental_table1[,2] <- paste0(Effort_supplemental_table1[,2], Signif)

## Table S1
### Betas coefficients for mu (raw)         
print(xtable(Effort_supplemental_table1),only.contents=TRUE, include.rownames=FALSE, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      sanitize.text.function = identity)

## rho and omega coefficients for sigma (raw) 
rhos <- Mod$tau_effort%>%as.matrix()%>%as.data.frame()
colnames(rhos) <- c("PH", "PN")
rhos <- apply(rhos, 2, function(x){
  quantile(x, c(0.1, 0.5, 0.9))
})%>%t()%>%round(.,3)

omegas <- Mod$omega%>%as.matrix()%>%as.data.frame()
colnames(omegas) <- c("PH", "PN")
omegas <- apply(omegas, 2, function(x){
  quantile(x, c(0.1, 0.5, 0.9))
})%>%t()%>%round(.,3)


betas <- Mod$beta%>%as.matrix()%>%as.data.frame()
colnames(betas) <- colnames(Train_mu_matrix)
Betas_main <- betas[,c(
  1,2,                                                                       
  grep("A_Gag_e", colnames(betas)),
  grep("A_RG_e", colnames(betas)),
  grep("A_RS_e", colnames(betas)),
  grep("M_Gag", colnames(betas)),
  grep("M_RG", colnames(betas)),
  grep("M_RS", colnames(betas)),
  grep("S_Gag", colnames(betas)),
  grep("S_RG", colnames(betas)),
  grep("S_RS", colnames(betas)),
  grep("Grouper_searches", colnames(betas)),
  grep("Snapper_searches", colnames(betas)),
  grep("FIR", colnames(betas)),
  grep("Fishable", colnames(betas)),
  grep("sin1", colnames(betas)),
  grep("cos1", colnames(betas)),
  grep("sin2", colnames(betas)),
  grep("cos2", colnames(betas))
)]

for (i in seq(1, 37, 2)){
  Betas_main[,i+1] <- Betas[,i+1] + Betas_main[,i]
}

PH <- c(-grep("Peninsula", colnames(Betas_main)))
PN <- grep("Peninsula", colnames(Betas_main))

Betas_main_PH <- Betas_main[,PH]%>%apply(., 2, function(x){quantile(x,c(0.1, 0.5, 0.9))})%>%t()%>%round(.,3)
Betas_main_PN <- Betas_main[,PN]%>%apply(., 2, function(x){quantile(x,c(0.1, 0.5, 0.9))})%>%t()%>%round(.,3)

Signif_PH <- ifelse(sign(as.numeric(Betas_main_PH[,1])) == sign(as.numeric(Betas_main_PH[,3])), "*", "")
Signif_PN <- ifelse(sign(as.numeric(Betas_main_PN[,1])) == sign(as.numeric(Betas_main_PN[,3])), "*", "")

PH_Beta_names <- paste0("$\\beta_{","PH:", gsub("\\$", "", Effort_names_beta[PH]), "}$", Signif_PH)
PN_Beta_names <- paste0("$\\beta_{", gsub("\\$", "", Effort_names_beta[PN]), "}$", Signif_PN)

Beta_main_table <- cbind(PH_Beta_names, Betas_main_PH,
                         PN_Beta_names, Betas_main_PN)

## Table 2
### Betas coefficients for mu by region   
print(xtable(Beta_main_table),only.contents=TRUE, include.rownames=FALSE, 
      include.colnames=T, floating=F, sanitize.rownames.function = identity,
      sanitize.text.function = identity)

### rho and omega coefficients for sigma by region
rhos <- Mod$tau_effort%>%as.matrix()%>%as.data.frame()
colnames(rhos) <- c("PH", "PN")
rhos[,2] <- rhos[,1]+rhos[,2]
rhos <- apply(rhos, 2, function(x){
  quantile(x, c(0.1, 0.5, 0.9))
})%>%t()%>%round(.,3)

omegas <- Mod$omega%>%as.matrix()%>%as.data.frame()
colnames(omegas) <- c("PH", "PN")
omegas[,2] <- omegas[,1]+omegas[,2]
omegas <- apply(omegas, 2, function(x){
  quantile(x, c(0.1, 0.5, 0.9))
})%>%t()%>%round(.,3)


