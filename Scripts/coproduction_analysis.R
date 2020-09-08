
library(interactions)
library(car)
library(MuMIn)
library(ggplot2)
library(GGally)
library(rgdal)
library(plyr)
library(grid)
library(rgeos)
library(ggpubr)

############################################################################################
###################              DATA               ########################################
############################################################################################

#### data all observations across all countries

data <- read.csv("data_EU_IFA_sel.csv")[,-1]
data$country <- as.character(data$country)

# define income classes according to Worldbank
data$income <- NA
low_middle <- c("Albania","Algeria","Azerbaijan","Bangladesh","Belarus","Bolivia_(Plurinational_State_of)",
                "Brazil","Cambodia","China,_mainland","Colombia", "Costa_Rica","Dominican_Republic",
                "Egypt","Ecuador","El_Salvador","Ethiopia","Guatemala","Guinea","Honduras",
                "India","Indonesia","Jordan","Kenya","Lao_People's_Democratic_Republic","Lebanon","Madagascar","Malawi",
                "Malaysia","Mauritania","Mexico","Morocco","Myanmar","Nicaragua","Nigeria","Pakistan","Paraguay",
                "Philippines","Republic_of_Moldova","South_Africa","Sri_Lanka","Syrian_Arab_Republic","Thailand",
                "United_Republic_of_Tanzania","Togo","Turkey","Viet_Nam","Zambia","Zimbabwe","Iran_(Islamic_Republic_of)",
                "Uzbekistan","Ukraine")
high <- c("EU27","Argentina","Australia","Canada","Chile","Israel","Japan","Kuwait","New_Zealand","Norway","Republic_of_Korea",
          "Saudi_Arabia","Switzerland","United_States_of_America","Uruguay","Russian_Federation","Venezuela_(Bolivarian_Republic_of)")

for(i in 1:length(data[,1])){
  if(is.element(data$country[i],low_middle)){
    data$income[i]<-"LM"}}

for(i in 1:length(data[,1])){
  if(is.element(data$country[i],high)){
    data$income[i]<-"H"}}

data$income <-factor(data$income)

# create dataframe with logged, centred and scaled measurements
log_data <- data
log_data[,3:11] <- log(log_data[,3:11])
log_data$year <- factor(log_data$year)
log_data$country <- as.character(log_data$country)

#subtract mean and divide by SD
log_data_centred <- log_data
log_data_centred[,3:11] <- scale(log_data_centred[,3:11], center=TRUE, scale=TRUE)

#### data lower-upper-middle-income countries
data_LM <-data[which(data$income=="LM"),]

# create dataframe with logged, centred and scaled measurements
log_data_LM <- data_LM
log_data_LM[,3:11] <- log(log_data_LM[,3:11])
log_data_LM$year <- factor(log_data_LM$year)

#subtract mean and divide by SD
log_data_centred_LM <- log_data_LM
log_data_centred_LM[,3:11] <- scale(log_data_centred_LM[,3:11], center=TRUE, scale=TRUE)

#### data across high-income countries
data_H <-data[which(data$income=="H"),]

# create dataframe with logged, centred and scaled measurements
log_data_H <- data_H
log_data_H[,3:11] <- log(log_data_H[,3:11])
log_data_H$year <- factor(log_data_H$year)

#subtract mean and divide by SD
log_data_centred_H <- log_data_H
log_data_centred_H[,3:11] <- scale(log_data_centred_H[,3:11], center=TRUE, scale=TRUE)


############################################################################################
###################              MODEL               #######################################
############################################################################################

### Model for all countries

# check for correlation among predictors
ggpairs(log_data[,c("Nt_area","cropsuit","manure_area","netcap14_area","market")])

# fit the model with interaction terms
mod <- lm(yield ~ Nt_area + cropsuit + netcap14_area + manure_area + market
          + cropsuit:Nt_area + cropsuit:netcap14_area + cropsuit:manure_area + cropsuit:market, data = log_data_centred)
vif(mod)

# exclude interaction term cropsuit:netcap14_area because of high VIF
mod <- lm(yield ~ Nt_area + cropsuit + netcap14_area + manure_area + market
          + cropsuit:Nt_area + cropsuit:manure_area + cropsuit:market, data = log_data_centred)
vif(mod)

# check that residuals are not correlated to the predictors
log_data_centred$res <- residuals(mod)
layout(1)
plot(res ~ Nt_area + cropsuit + netcap14_area + manure_area + market, 
     data=log_data_centred, pch=21, col=grey(0,0.6), bg=grey(0,0.2))

# check interactions

plot(cropsuit ~ Nt_area, cex=exp(log_data_centred$res), col=grey(0,0.5), data=log_data_centred)
plot(cropsuit ~ Nt_area, cex=exp(log_data_centred$res), col= ifelse(log_data_centred$res < 0,'red','green'), data=log_data_centred)

plot(cropsuit ~ manure_area, cex=exp(log_data_centred$res), col=grey(0,0.5), data=log_data_centred)
plot(cropsuit ~ manure_area, cex=exp(log_data_centred$res), col= ifelse(log_data_centred$res < 0,'red','green'), data=log_data_centred)

plot(cropsuit ~ market, cex=exp(log_data_centred$res), col=grey(0,0.5), data=log_data_centred)
plot(cropsuit ~ market, cex=exp(log_data_centred$res), col= ifelse(log_data_centred$res < 0,'red','green'), data=log_data_centred)

# model selection
options(na.action = "na.fail")

dd_mod <- dredge(mod, beta ="sd", extra = "R^2")

# model averaging
model.avg(dd_mod, subset = delta < 5)

# create table with coeeficients and estimated p values

coefTable_dd_mod <- coefTable(model.avg(dd_mod, subset = delta < 5))
coefTable_dd_mod_round <- round(coefTable_dd_mod[,1:2], digits = 2)

# p <= 0.05 i.e. +- 1.96*Standard Error
coefTable_dd_mod_round_n <-  as.data.frame(coefTable_dd_mod_round)
coefTable_dd_mod_round_n$minSE95 <- NA
coefTable_dd_mod_round_n$maxSE95 <- NA
coefTable_dd_mod_round_n$minSE95 <- round(coefTable_dd_mod_round_n$Estimate - 1.96*coefTable_dd_mod_round_n$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n$maxSE95 <- round(coefTable_dd_mod_round_n$Estimate + 1.96*coefTable_dd_mod_round_n$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n$sigp005 <- NA
coefTable_dd_mod_round_n$sigp005 <- ifelse(sign(coefTable_dd_mod_round_n$minSE95)==sign(coefTable_dd_mod_round_n$maxSE95), "s", "ns")

# p <= 0.10 i.e. +- 1.645*Standard Error
coefTable_dd_mod_round_n$minSE90 <- NA
coefTable_dd_mod_round_n$maxSE90 <- NA
coefTable_dd_mod_round_n$minSE90 <- round(coefTable_dd_mod_round_n$Estimate - 1.645*coefTable_dd_mod_round_n$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n$maxSE90 <- round(coefTable_dd_mod_round_n$Estimate + 1.645*coefTable_dd_mod_round_n$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n$sigp010 <- ifelse(sign(coefTable_dd_mod_round_n$minSE90)==sign(coefTable_dd_mod_round_n$maxSE90), "s", "ns")

# p <= 0.01 i.e. +- 2.576*Standard Error
coefTable_dd_mod_round_n$minSE99 <- NA
coefTable_dd_mod_round_n$maxSE99 <- NA
coefTable_dd_mod_round_n$minSE99 <- round(coefTable_dd_mod_round_n$Estimate - 2.576*coefTable_dd_mod_round_n$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n$maxSE99 <- round(coefTable_dd_mod_round_n$Estimate + 2.576*coefTable_dd_mod_round_n$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n$sigp001 <- ifelse(sign(coefTable_dd_mod_round_n$minSE99)==sign(coefTable_dd_mod_round_n$maxSE99), "s", "ns")

# importance values as table
importance_dd_mod <- as.data.frame(importance(dd_mod))
importance_dd_mod <- round(importance_dd_mod, digits = 2)

##range of R2 of the best performing models (delta AIC < 5)
dd_mod_sel <- subset(dd_mod, delta <= 5)
round(range(dd_mod_sel$`R^2`),digits = 2)

# best performing model for visualising interaction effect
bestmod <- get.models(dd_mod, 1)
bestmod

bestmod1 <-  lm(yield ~ cropsuit + manure_area + market + Nt_area + 
                  cropsuit:Nt_area, data = log_data_centred)
summary(bestmod1)

# Interaction plot for best performing model
interact_plot(bestmod1, pred = cropsuit, modx = Nt_area, interval = TRUE, 
              x.label = "Agricultural suitability", y.label = "Yield",
              legend.main = "Fertilizer use intensity", plot.points = TRUE)


### Model for low/lower and upper middle-income countries
# check for correlation among predictors
ggpairs(log_data_LM[,c("Nt_area","cropsuit","manure_area","netcap14_area","market")])

# fit the model with interaction terms
mod_LM <- lm(yield ~ Nt_area + cropsuit + netcap14_area + manure_area + market
          + cropsuit:Nt_area + cropsuit:netcap14_area + cropsuit:manure_area + cropsuit:market, data = log_data_centred_LM)
vif(mod_LM)

# check that residuals are not correlated to the predictors
log_data_centred_LM$res <- residuals(mod_LM)
layout(1)
plot(res ~ Nt_area + cropsuit + netcap14_area + manure_area + market, 
     data=log_data_centred_LM, pch=21, col=grey(0,0.6), bg=grey(0,0.2))

# check interactions
plot(cropsuit ~ Nt_area, cex=exp(log_data_centred_LM$res), col=grey(0,0.5), data=log_data_centred_LM)
plot(cropsuit ~ Nt_area, cex=exp(log_data_centred_LM$res), col= ifelse(log_data_centred_LM$res < 0,'red','green'), data=log_data_centred_LM)

plot(cropsuit ~ netcap14_area, cex=exp(log_data_centred_LM$res), col=grey(0,0.5), data=log_data_centred_LM)
plot(cropsuit ~ netcap14_area, cex=exp(log_data_centred_LM$res), col= ifelse(log_data_centred_LM$res < 0,'red','green'), data=log_data_centred_LM)

plot(cropsuit ~ market, cex=exp(log_data_centred_LM$res), col=grey(0,0.5), data=log_data_centred_LM)
plot(cropsuit ~ market, cex=exp(log_data_centred_LM$res), col= ifelse(log_data_centred_LM$res < 0,'red','green'), data=log_data_centred_LM)

plot(cropsuit ~ manure_area, cex=exp(log_data_centred_LM$res), col=grey(0,0.5), data=log_data_centred_LM)
plot(cropsuit ~ manure_area, cex=exp(log_data_centred_LM$res), col= ifelse(log_data_centred_LM$res < 0,'red','green'), data=log_data_centred_LM)

# model selection
options(na.action = "na.fail")
dd_mod_LM <- dredge(mod_LM, beta ="sd", extra = "R^2")

#model averaging
model.avg(dd_mod_LM, subset = delta < 5)

# create table with coeeficients and estimated p values

coefTable_dd_mod_LM <- coefTable(model.avg(dd_mod_LM, subset = delta < 5))
coefTable_dd_mod_round_LM <- round(coefTable_dd_mod_LM[,1:2], digits = 2)

# p <= 0.05 i.e. +- 1.96*Standard Error
coefTable_dd_mod_round_n_LM <-  as.data.frame(coefTable_dd_mod_round_LM)
coefTable_dd_mod_round_n_LM$minSE95 <- NA
coefTable_dd_mod_round_n_LM$maxSE95 <- NA
coefTable_dd_mod_round_n_LM$minSE95 <- round(coefTable_dd_mod_round_n_LM$Estimate - 1.96*coefTable_dd_mod_round_n_LM$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n_LM$maxSE95 <- round(coefTable_dd_mod_round_n_LM$Estimate + 1.96*coefTable_dd_mod_round_n_LM$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n_LM$sigp005 <- NA
coefTable_dd_mod_round_n_LM$sigp005 <- ifelse(sign(coefTable_dd_mod_round_n_LM$minSE95)==sign(coefTable_dd_mod_round_n_LM$maxSE95), "s", "ns")

# p <= 0.10 i.e. +- 1.645*Standard Error
coefTable_dd_mod_round_n_LM$minSE90 <- NA
coefTable_dd_mod_round_n_LM$maxSE90 <- NA
coefTable_dd_mod_round_n_LM$minSE90 <- round(coefTable_dd_mod_round_n_LM$Estimate - 1.645*coefTable_dd_mod_round_n_LM$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n_LM$maxSE90 <- round(coefTable_dd_mod_round_n_LM$Estimate + 1.645*coefTable_dd_mod_round_n_LM$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n_LM$sigp010 <- ifelse(sign(coefTable_dd_mod_round_n_LM$minSE90)==sign(coefTable_dd_mod_round_n_LM$maxSE90), "s", "ns")

# p <= 0.01 i.e. +- 2.576*Standard Error
coefTable_dd_mod_round_n_LM$minSE99 <- NA
coefTable_dd_mod_round_n_LM$maxSE99 <- NA
coefTable_dd_mod_round_n_LM$minSE99 <- round(coefTable_dd_mod_round_n_LM$Estimate - 2.576*coefTable_dd_mod_round_n_LM$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n_LM$maxSE99 <- round(coefTable_dd_mod_round_n_LM$Estimate + 2.576*coefTable_dd_mod_round_n_LM$`Std. Error`, digits = 3)
coefTable_dd_mod_round_n_LM$sigp001 <- ifelse(sign(coefTable_dd_mod_round_n_LM$minSE99)==sign(coefTable_dd_mod_round_n_LM$maxSE99), "s", "ns")

# importance values as table
importance_dd_mod_LM <- as.data.frame(importance(dd_mod_LM))
importance_dd_mod_LM <- round(importance_dd_mod_LM, digits = 2)

# range of R2 of the best performing models (delta AIC < 5)

dd_mod_sel_LM <- subset(dd_mod_LM, delta <= 5)
round(range(dd_mod_sel_LM$`R^2`),digits = 2)


### Model for high-income countries

# fit the model only with variables that turned out significant in model selection for all countries, to reduce 
#potential overfitting due to a relatively low number of observations for high income countries
# these variables are: Nt_area + cropsuit + netcap14_area + manure_area + cropsuit:Nt_area

# check for correlation among predictors
ggpairs(log_data_H[,c("Nt_area","cropsuit","manure_area","netcap14_area","market")], scale=TRUE)

# exclude manure_area as it highly correlates with netcap14_area and Nt_area
mod_H <- lm(yield ~ Nt_area + cropsuit + netcap14_area + market + cropsuit:Nt_area, data = log_data_centred_H)
vif(mod_H)
summary(mod_H)

# check that residuals are not correlated to the predictors
log_data_centred_H$res <- residuals(mod_H)
layout(1)
plot(res ~ Nt_area + cropsuit + netcap14_area + market + cropsuit:Nt_area, 
     data=log_data_centred_H, pch=21, col=grey(0,0.6), bg=grey(0,0.2))

# check interactions
plot(cropsuit ~ Nt_area, cex=exp(log_data_centred_H$res), col=grey(0,0.5), data=log_data_centred_H)
plot(cropsuit ~ Nt_area, cex=exp(log_data_centred_H$res), col= ifelse(log_data_centred_H$res < 0,'red','green'), data=log_data_centred_H)

# confidence intervals for predictors
## 95% confidence interval
#Nt_area
#minSE
0.60139 - 1.96*0.10903
#maxSE
0.60139 + 1.96*0.10903

#cropsuit
#minSE
0.53752 - 1.96*0.11096
#maxSE
0.53752 + 1.96*0.11096  

#netcap14_area 
#minSE
0.40762 - 1.96*0.12563 
#maxSE
0.40762 + 1.96*0.12563 

#market
#minSE
-0.05679 - 1.96*0.09928    
#maxSE
-0.05679 + 1.96*0.09928

#Nt_area:cropsuit 
#minSE
-0.81055  - 1.96*0.17506
#minSE
-0.81055  + 1.96*0.17506

############################################################################################
###################              TRAJECTORIES               ################################
############################################################################################

# Visualise trends of yield vs. anthropogenic coproduction factors: fertilizer_area, manure_area and netcap_area
# load data

data_traject<-read.csv("crops_16Lx_clean.csv",sep=",",header=T)[,-1]

data_traject$Country <- as.character(data_traject$Country)

# the following is a list of countries for which IFA data is available for each year

IFA_countries_00_14 <- c("Austria","Belgium","Bulgaria","Cyprus","Denmark","Estonia","Finland","France","Germany","Greece",
                      "Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Romania",
                      "Portugal","Slovakia","Slovenia","Spain","Sweden","United_Kingdom","Hungary","Czechia",
                      "Argentina","Australia","Bangladesh","Belarus","Brazil","Canada",
                      "China,_mainland","Chile","Egypt","India","Indonesia",
                      "Malaysia","Mexico","Morocco","Pakistan","Philippines","South_Africa","Thailand",
                      "Turkey","United_States_of_America","Viet_Nam")

data_traject$IFA_Region <- NA

for(i in 1:length(data_traject[,1])){
  if(is.element(data_traject$Country[i],IFA_countries_00_14)){
    data_traject$IFA_Region[i]<-"IFA"}}

data_traject_IFA <- data_traject[which(data_traject$IFA_Region=="IFA"),]
data_traject_IFA <- data_traject_IFA[,c(1:6,12:13)]

#exclude NA
data_traject <- na.exclude(data_traject)

data_traject_IFA$Income <- NA

lower_middle <- c("Bangladesh","Egypt","India","Indonesia","Morocco","Pakistan","Philippines","Viet_Nam")
upper_middle <- c("Belarus", "Bulgaria","Romania","Brazil","China,_mainland","Malaysia","Mexico","South_Africa","Thailand","Turkey")
high <- c("Austria","Belgium","Cyprus","Denmark","Estonia","Finland","France","Germany","Greece",
          "Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal",
          "Slovakia","Slovenia","Spain","Sweden","United_Kingdom","Hungary","Czechia","Argentina",
          "Australia","Canada","Chile","United_States_of_America")

for(i in 1:length(data_traject_IFA[,1])){
  if(is.element(data_traject_IFA$Country[i],lower_middle)){
    data_traject_IFA$Income[i]<-"lower_middle"} 
  if(is.element(data_traject_IFA$Country[i],upper_middle)){
    data_traject_IFA$Income[i]<-"upper_middle"}
  if(is.element(data_traject_IFA$Country[i],high)){
    data_traject_IFA$Income[i]<-"high"}}

crops_lower_middle <- data_traject_IFA[which(data_traject_IFA$Income=="lower_middle"),]
crops_upper_middle <- data_traject_IFA[which(data_traject_IFA$Income=="upper_middle"),]
crops_high <- data_traject_IFA[which(data_traject_IFA$Income=="high"),]

# split dataset by year and income group

crops_lower_middle_00 <- crops_lower_middle[which(crops_lower_middle$Year=="2000"),]
crops_upper_middle_00 <- crops_upper_middle[which(crops_upper_middle$Year=="2000"),]
crops_high_00 <- crops_high[which(crops_high$Year=="2000"),]

crops_lower_middle_06 <- crops_lower_middle[which(crops_lower_middle$Year=="2006"),]  
crops_upper_middle_06 <- crops_upper_middle[which(crops_upper_middle$Year=="2006"),]
crops_high_06 <- crops_high[which(crops_high$Year=="2006"),]

crops_lower_middle_10 <- crops_lower_middle[which(crops_lower_middle$Year=="2010"),]
crops_upper_middle_10 <- crops_upper_middle[which(crops_upper_middle$Year=="2010"),]
crops_high_10 <- crops_high[which(crops_high$Year=="2010"),]

crops_lower_middle_14 <- crops_lower_middle[which(crops_lower_middle$Year=="2014"),]
crops_upper_middle_14 <- crops_upper_middle[which(crops_upper_middle$Year=="2014"),]
crops_high_14 <- crops_high[which(crops_high$Year=="2014"),]

# create new data frame with aggregated data

aggreg_frame <- data.frame("region" = c("lower_middle","lower_middle","lower_middle","lower_middle","upper_middle","upper_middle","upper_middle","upper_middle","high","high","high","high"),"year" = c("2000","2006","2010","2014","2000","2006","2010","2014","2000","2006","2010","2014"))

aggreg_frame$N_total <- NA
aggreg_frame$prod_total <- NA
aggreg_frame$area_total <- NA
aggreg_frame$manure_total <- NA
aggreg_frame$netcap14_total <- NA

aggreg_frame[1,3] <- sum(crops_lower_middle_00$N_total_t)
aggreg_frame[2,3] <- sum(crops_lower_middle_06$N_total_t)
aggreg_frame[3,3] <- sum(crops_lower_middle_10$N_total_t)
aggreg_frame[4,3] <- sum(crops_lower_middle_14$N_total_t)
aggreg_frame[1,4] <- sum(crops_lower_middle_00$production_kj)
aggreg_frame[2,4] <- sum(crops_lower_middle_06$production_kj)
aggreg_frame[3,4] <- sum(crops_lower_middle_10$production_kj)
aggreg_frame[4,4] <- sum(crops_lower_middle_14$production_kj)
aggreg_frame[1,5] <- sum(crops_lower_middle_00$Area_harvested_ha)
aggreg_frame[2,5] <- sum(crops_lower_middle_06$Area_harvested_ha)
aggreg_frame[3,5] <- sum(crops_lower_middle_10$Area_harvested_ha)
aggreg_frame[4,5] <- sum(crops_lower_middle_14$Area_harvested_ha)
aggreg_frame[1,6] <- sum(crops_lower_middle_00$Manure_kg)
aggreg_frame[2,6] <- sum(crops_lower_middle_06$Manure_kg)
aggreg_frame[3,6] <- sum(crops_lower_middle_10$Manure_kg)
aggreg_frame[4,6] <- sum(crops_lower_middle_14$Manure_kg)
aggreg_frame[1,7] <- sum(crops_lower_middle_00$net_capital_stocks14)
aggreg_frame[2,7] <- sum(crops_lower_middle_06$net_capital_stocks14)
aggreg_frame[3,7] <- sum(crops_lower_middle_10$net_capital_stocks14)
aggreg_frame[4,7] <- sum(crops_lower_middle_14$net_capital_stocks14)

aggreg_frame[5,3] <- sum(crops_upper_middle_00$N_total_t)
aggreg_frame[6,3] <- sum(crops_upper_middle_06$N_total_t)
aggreg_frame[7,3] <- sum(crops_upper_middle_10$N_total_t)
aggreg_frame[8,3] <- sum(crops_upper_middle_14$N_total_t)
aggreg_frame[5,4] <- sum(crops_upper_middle_00$production_kj)
aggreg_frame[6,4] <- sum(crops_upper_middle_06$production_kj)
aggreg_frame[7,4] <- sum(crops_upper_middle_10$production_kj)
aggreg_frame[8,4] <- sum(crops_upper_middle_14$production_kj)
aggreg_frame[5,5] <- sum(crops_upper_middle_00$Area_harvested_ha)
aggreg_frame[6,5] <- sum(crops_upper_middle_06$Area_harvested_ha)
aggreg_frame[7,5] <- sum(crops_upper_middle_10$Area_harvested_ha)
aggreg_frame[8,5] <- sum(crops_upper_middle_14$Area_harvested_ha)
aggreg_frame[5,6] <- sum(crops_upper_middle_00$Manure_kg)
aggreg_frame[6,6] <- sum(crops_upper_middle_06$Manure_kg)
aggreg_frame[7,6] <- sum(crops_upper_middle_10$Manure_kg)
aggreg_frame[8,6] <- sum(crops_upper_middle_14$Manure_kg)
aggreg_frame[5,7] <- sum(crops_upper_middle_00$net_capital_stocks14)
aggreg_frame[6,7] <- sum(crops_upper_middle_06$net_capital_stocks14)
aggreg_frame[7,7] <- sum(crops_upper_middle_10$net_capital_stocks14)
aggreg_frame[8,7] <- sum(crops_upper_middle_14$net_capital_stocks14)

aggreg_frame[9,3] <- sum(crops_high_00$N_total_t)
aggreg_frame[10,3] <- sum(crops_high_06$N_total_t)
aggreg_frame[11,3] <- sum(crops_high_10$N_total_t)
aggreg_frame[12,3] <- sum(crops_high_14$N_total_t)
aggreg_frame[9,4] <- sum(crops_high_00$production_kj)
aggreg_frame[10,4] <- sum(crops_high_06$production_kj)
aggreg_frame[11,4] <- sum(crops_high_10$production_kj)
aggreg_frame[12,4] <- sum(crops_high_14$production_kj)
aggreg_frame[9,5] <- sum(crops_high_00$Area_harvested_ha)
aggreg_frame[10,5] <- sum(crops_high_06$Area_harvested_ha)
aggreg_frame[11,5] <- sum(crops_high_10$Area_harvested_ha)
aggreg_frame[12,5] <- sum(crops_high_14$Area_harvested_ha)
aggreg_frame[9,6] <- sum(crops_high_00$Manure_kg)
aggreg_frame[10,6] <- sum(crops_high_06$Manure_kg)
aggreg_frame[11,6] <- sum(crops_high_10$Manure_kg)
aggreg_frame[12,6] <- sum(crops_high_14$Manure_kg)
aggreg_frame[9,7] <- sum(crops_high_00$net_capital_stocks14)
aggreg_frame[10,7] <- sum(crops_high_06$net_capital_stocks14)
aggreg_frame[11,7] <- sum(crops_high_10$net_capital_stocks14)
aggreg_frame[12,7] <- sum(crops_high_14$net_capital_stocks14)

aggreg_frame$N_area <- NA
aggreg_frame$yield <- NA
aggreg_frame$manure_area <- NA
aggreg_frame$netcap14_area <- NA

aggreg_frame$N_area <- aggreg_frame$N_total/aggreg_frame$area_total
aggreg_frame$yield <- aggreg_frame$prod_total/aggreg_frame$area_total
aggreg_frame$manure_area <- aggreg_frame$manure_total/aggreg_frame$area_total
aggreg_frame$netcap14_area <- aggreg_frame$netcap14_total/aggreg_frame$area_total

# calculate values relative to the year 2000
aggreg_frame$N_area_100 <- NA
aggreg_frame$yield_100 <- NA
aggreg_frame$manure_area_100 <- NA
aggreg_frame$netcap14_area_100 <- NA

aggreg_frame[1,12] <- (aggreg_frame[1,8]/aggreg_frame[1,8])
aggreg_frame[2,12] <- (aggreg_frame[2,8]/aggreg_frame[1,8])
aggreg_frame[3,12] <- (aggreg_frame[3,8]/aggreg_frame[1,8])
aggreg_frame[4,12] <- (aggreg_frame[4,8]/aggreg_frame[1,8])
aggreg_frame[5,12] <- (aggreg_frame[5,8]/aggreg_frame[5,8])
aggreg_frame[6,12] <- (aggreg_frame[6,8]/aggreg_frame[5,8])
aggreg_frame[7,12] <- (aggreg_frame[7,8]/aggreg_frame[5,8])
aggreg_frame[8,12] <- (aggreg_frame[8,8]/aggreg_frame[5,8])
aggreg_frame[9,12] <- (aggreg_frame[9,8]/aggreg_frame[9,8])
aggreg_frame[10,12] <- (aggreg_frame[10,8]/aggreg_frame[9,8])
aggreg_frame[11,12] <- (aggreg_frame[11,8]/aggreg_frame[9,8])
aggreg_frame[12,12] <- (aggreg_frame[12,8]/aggreg_frame[9,8])

aggreg_frame[1,13] <- (aggreg_frame[1,9]/aggreg_frame[1,9])
aggreg_frame[2,13] <- (aggreg_frame[2,9]/aggreg_frame[1,9])
aggreg_frame[3,13] <- (aggreg_frame[3,9]/aggreg_frame[1,9])
aggreg_frame[4,13] <- (aggreg_frame[4,9]/aggreg_frame[1,9])
aggreg_frame[5,13] <- (aggreg_frame[5,9]/aggreg_frame[5,9])
aggreg_frame[6,13] <- (aggreg_frame[6,9]/aggreg_frame[5,9])
aggreg_frame[7,13] <- (aggreg_frame[7,9]/aggreg_frame[5,9])
aggreg_frame[8,13] <- (aggreg_frame[8,9]/aggreg_frame[5,9])
aggreg_frame[9,13] <- (aggreg_frame[9,9]/aggreg_frame[9,9])
aggreg_frame[10,13] <- (aggreg_frame[10,9]/aggreg_frame[9,9])
aggreg_frame[11,13] <- (aggreg_frame[11,9]/aggreg_frame[9,9])
aggreg_frame[12,13] <- (aggreg_frame[12,9]/aggreg_frame[9,9])

aggreg_frame[1,14] <- (aggreg_frame[1,10]/aggreg_frame[1,10])
aggreg_frame[2,14] <- (aggreg_frame[2,10]/aggreg_frame[1,10])
aggreg_frame[3,14] <- (aggreg_frame[3,10]/aggreg_frame[1,10])
aggreg_frame[4,14] <- (aggreg_frame[4,10]/aggreg_frame[1,10])
aggreg_frame[5,14] <- (aggreg_frame[5,10]/aggreg_frame[5,10])
aggreg_frame[6,14] <- (aggreg_frame[6,10]/aggreg_frame[5,10])
aggreg_frame[7,14] <- (aggreg_frame[7,10]/aggreg_frame[5,10])
aggreg_frame[8,14] <- (aggreg_frame[8,10]/aggreg_frame[5,10])
aggreg_frame[9,14] <- (aggreg_frame[9,10]/aggreg_frame[9,10])
aggreg_frame[10,14] <- (aggreg_frame[10,10]/aggreg_frame[9,10])
aggreg_frame[11,14] <- (aggreg_frame[11,10]/aggreg_frame[9,10])
aggreg_frame[12,14] <- (aggreg_frame[12,10]/aggreg_frame[9,10])

aggreg_frame[1,15] <- (aggreg_frame[1,11]/aggreg_frame[1,11])
aggreg_frame[2,15] <- (aggreg_frame[2,11]/aggreg_frame[1,11])
aggreg_frame[3,15] <- (aggreg_frame[3,11]/aggreg_frame[1,11])
aggreg_frame[4,15] <- (aggreg_frame[4,11]/aggreg_frame[1,11])
aggreg_frame[5,15] <- (aggreg_frame[5,11]/aggreg_frame[5,11])
aggreg_frame[6,15] <- (aggreg_frame[6,11]/aggreg_frame[5,11])
aggreg_frame[7,15] <- (aggreg_frame[7,11]/aggreg_frame[5,11])
aggreg_frame[8,15] <- (aggreg_frame[8,11]/aggreg_frame[5,11])
aggreg_frame[9,15] <- (aggreg_frame[9,11]/aggreg_frame[9,11])
aggreg_frame[10,15] <- (aggreg_frame[10,11]/aggreg_frame[9,11])
aggreg_frame[11,15] <- (aggreg_frame[11,11]/aggreg_frame[9,11])
aggreg_frame[12,15] <- (aggreg_frame[12,11]/aggreg_frame[9,11])

aggreg_frame[,12:15] <- aggreg_frame[,12:15]*100
aggreg_frame[,12:15] <- round(aggreg_frame[,12:15], digits = 1)

# plot Figure 1

par(mfrow = c(1,3))
plot(aggreg_frame$N_area_100[aggreg_frame$region=="lower_middle"],aggreg_frame$yield_100[aggreg_frame$region=="lower_middle"],
     pch = 16, col = "black", xlab = "Fertilizer use intensity (%, 100=2000)", ylab = "Yield (%, 100=2000)", cex.axis = 1.4, cex.lab = 1.4)
points(aggreg_frame$N_area_100[aggreg_frame$region=="upper_middle"],aggreg_frame$yield_100[aggreg_frame$region=="upper_middle"], pch = 16, col = "black")
points(aggreg_frame$N_area_100[aggreg_frame$region=="high"],aggreg_frame$yield_100[aggreg_frame$region=="high"], pch = 16, col = "black")
legend("topleft",legend = c("Lower-middle","Upper-middle","High"),
       col=c("orange","blue4","orangered3"),bty="n",pch=c("-","-","-"),pt.cex=2,cex = 1.4)

arrows(aggreg_frame[1,12], aggreg_frame[1,13], aggreg_frame[2,12],aggreg_frame[2,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)
arrows(aggreg_frame[2,12], aggreg_frame[2,13], aggreg_frame[3,12],aggreg_frame[3,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)
arrows(aggreg_frame[3,12], aggreg_frame[3,13], aggreg_frame[4,12],aggreg_frame[4,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)

arrows(aggreg_frame[5,12], aggreg_frame[5,13], aggreg_frame[6,12],aggreg_frame[6,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)
arrows(aggreg_frame[6,12], aggreg_frame[6,13], aggreg_frame[7,12],aggreg_frame[7,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)
arrows(aggreg_frame[7,12], aggreg_frame[7,13], aggreg_frame[8,12],aggreg_frame[8,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)

arrows(aggreg_frame[9,12], aggreg_frame[9,13], aggreg_frame[10,12],aggreg_frame[10,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)
arrows(aggreg_frame[10,12], aggreg_frame[10,13], aggreg_frame[11,12],aggreg_frame[11,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)
arrows(aggreg_frame[11,12], aggreg_frame[11,13], aggreg_frame[12,12],aggreg_frame[12,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)

plot(aggreg_frame$manure_area_100[aggreg_frame$region=="lower_middle"],aggreg_frame$yield_100[aggreg_frame$region=="lower_middle"],
     pch = 16, col = "black", xlab = "Manure use intensity (%, 100=2000)", ylab = "Yield (%, 100=2000)", xlim = c(90,125), cex.axis = 1.4, cex.lab = 1.4)
points(aggreg_frame$manure_area_100[aggreg_frame$region=="upper_middle"],aggreg_frame$yield_100[aggreg_frame$region=="upper_middle"], pch = 16, col = "black")
points(aggreg_frame$manure_area_100[aggreg_frame$region=="high"],aggreg_frame$yield_100[aggreg_frame$region=="high"], pch = 16, col = "black")

arrows(aggreg_frame[1,14], aggreg_frame[1,13], aggreg_frame[2,14],aggreg_frame[2,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)
arrows(aggreg_frame[2,14], aggreg_frame[2,13], aggreg_frame[3,14],aggreg_frame[3,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)
arrows(aggreg_frame[3,14], aggreg_frame[3,13], aggreg_frame[4,14],aggreg_frame[4,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)

arrows(aggreg_frame[5,14], aggreg_frame[5,13], aggreg_frame[6,14],aggreg_frame[6,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)
arrows(aggreg_frame[6,14], aggreg_frame[6,13], aggreg_frame[7,14],aggreg_frame[7,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)
arrows(aggreg_frame[7,14], aggreg_frame[7,13], aggreg_frame[8,14],aggreg_frame[8,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)

arrows(aggreg_frame[9,14], aggreg_frame[9,13], aggreg_frame[10,14],aggreg_frame[10,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)
arrows(aggreg_frame[10,14], aggreg_frame[10,13], aggreg_frame[11,14],aggreg_frame[11,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)
arrows(aggreg_frame[11,14], aggreg_frame[11,13], aggreg_frame[12,14],aggreg_frame[12,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)

plot(aggreg_frame$netcap14_area_100[aggreg_frame$region=="lower_middle"],aggreg_frame$yield_100[aggreg_frame$region=="lower_middle"],
     pch = 16, col = "black", xlab = "Net capital stock intensity (%, 100=2000)", ylab = "Yield (%, 100=2000)", xlim = c(100,270), cex.axis = 1.4, cex.lab = 1.4)
points(aggreg_frame$netcap14_area_100[aggreg_frame$region=="upper_middle"],aggreg_frame$yield_100[aggreg_frame$region=="upper_middle"], pch = 16, col = "black")
points(aggreg_frame$netcap14_area_100[aggreg_frame$region=="high"],aggreg_frame$yield_100[aggreg_frame$region=="high"], pch = 16, col = "black")

arrows(aggreg_frame[1,15], aggreg_frame[1,13], aggreg_frame[2,15],aggreg_frame[2,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)
arrows(aggreg_frame[2,15], aggreg_frame[2,13], aggreg_frame[3,15],aggreg_frame[3,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)
arrows(aggreg_frame[3,15], aggreg_frame[3,13], aggreg_frame[4,15],aggreg_frame[4,13], length = 0.08, angle = 25,
       code = 2, col = "orange", lwd=2)

arrows(aggreg_frame[5,15], aggreg_frame[5,13], aggreg_frame[6,15],aggreg_frame[6,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)
arrows(aggreg_frame[6,15], aggreg_frame[6,13], aggreg_frame[7,15],aggreg_frame[7,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)
arrows(aggreg_frame[7,15], aggreg_frame[7,13], aggreg_frame[8,15],aggreg_frame[8,13], length = 0.08, angle = 25,
       code = 2, col = "blue4", lwd=2)

arrows(aggreg_frame[9,15], aggreg_frame[9,13], aggreg_frame[10,15],aggreg_frame[10,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)
arrows(aggreg_frame[10,15], aggreg_frame[10,13], aggreg_frame[11,15],aggreg_frame[11,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)
arrows(aggreg_frame[11,15], aggreg_frame[11,13], aggreg_frame[12,15],aggreg_frame[12,13], length = 0.08, angle = 25,
       code = 2, col = "orangered3", lwd=2)


############################################################################################
###################              VISUALIZE DATA               ##############################
############################################################################################

### Bivariate choropleth maps for the year 2014 (Figure 2)

data_14 <- data[which(data$year=="2014"),]

# read shape
ctryMapOriginal <- readOGR("Countries_copr_EU.shp")
ctryMapOriginal$NAME_SORT <- as.character(ctryMapOriginal$NAME_SORT)

grd <- rbind(data.frame(dim2=3,dim1=3,color="#3F2949"),
             data.frame(dim2=2,dim1=3,color="#435786"),
             data.frame(dim2=1,dim1=3,color="#4885C1"),
             data.frame(dim2=3,dim1=2,color="#77324C"),
             data.frame(dim2=2,dim1=2,color="#806A8A"),
             data.frame(dim2=1,dim1=2,color="#89A1C8"),
             data.frame(dim2=3,dim1=1,color="#AE3A4E"),
             data.frame(dim2=2,dim1=1,color="#BC7C8F"),
             data.frame(dim2=1,dim1=1,color="#CABED0"))

grd$color <- as.character(grd$color)

# plot all coproduction factors against yield for 2014

Agricultural_suitability <- as.numeric(quantile(data_14$cropsuit,probs=seq(0,1,length.out = 4)))
Yield <- as.numeric(quantile(data_14$yield,probs=seq(0,1,length.out = 4)))
data_14$dim1 <-car::recode(data_14$cropsuit,"Agricultural_suitability[1]:Agricultural_suitability[2]=1; Agricultural_suitability[2]:Agricultural_suitability[3]=2; Agricultural_suitability[3]:Agricultural_suitability[4]=3;")
data_14$dim2 <-car::recode(data_14$yield,"Yield[1]:Yield[2]=1; Yield[2]:Yield[3]=2; Yield[3]:Yield[4]=3;")

## join to map
data_14_final <- merge(data_14[,c("country", "dim1","dim2")],grd)
head(data_14_final)
data_14_final$id <- as.character(data_14_final$country)

mapsBivariate <- fortify(ctryMapOriginal,region="NAME_SORT")
mapsBivariate = plyr::join(mapsBivariate, data_14_final[,c("id","color")], by="id")

a1 <- ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")


Fertilizer_use <- as.numeric(quantile(data_14$Nt_area,probs=seq(0,1,length.out = 4)))
Yield <- as.numeric(quantile(data_14$yield,probs=seq(0,1,length.out = 4)))
data_14$dim1 <-car::recode(data_14$Nt_area,"Fertilizer_use[1]:Fertilizer_use[2]=1; Fertilizer_use[2]:Fertilizer_use[3]=2; Fertilizer_use[3]:Fertilizer_use[4]=3;")
data_14$dim2 <-car::recode(data_14$yield,"Yield[1]:Yield[2]=1; Yield[2]:Yield[3]=2; Yield[3]:Yield[4]=3;")

## join to map
data_14_final <- merge(data_14[,c("country", "dim1","dim2")],grd)
head(data_14_final)
data_14_final$id <- as.character(data_14_final$country)

mapsBivariate <- fortify(ctryMapOriginal,region="NAME_SORT")
mapsBivariate = plyr::join(mapsBivariate, data_14_final[,c("id","color")], by="id")

b1 <- ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")


Netcap_use <- as.numeric(quantile(data_14$netcap14_area,probs=seq(0,1,length.out = 4)))
Yield <- as.numeric(quantile(data_14$yield,probs=seq(0,1,length.out = 4)))
data_14$dim1 <-car::recode(data_14$netcap14_area,"Netcap_use[1]:Netcap_use[2]=1; Netcap_use[2]:Netcap_use[3]=2; Netcap_use[3]:Netcap_use[4]=3;")
data_14$dim2 <-car::recode(data_14$yield,"Yield[1]:Yield[2]=1; Yield[2]:Yield[3]=2; Yield[3]:Yield[4]=3;")

## join to map
data_14_final <- merge(data_14[,c("country", "dim1","dim2")],grd)
head(data_14_final)
data_14_final$id <- as.character(data_14_final$country)

mapsBivariate <- fortify(ctryMapOriginal,region="NAME_SORT")
mapsBivariate = plyr::join(mapsBivariate, data_14_final[,c("id","color")], by="id")

c1 <- ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

Manure_use <- as.numeric(quantile(data_14$manure_area,probs=seq(0,1,length.out = 4)))
Yield <- as.numeric(quantile(data_14$yield,probs=seq(0,1,length.out = 4)))
data_14$dim1 <-car::recode(data_14$manure_area,"Manure_use[1]:Manure_use[2]=1; Manure_use[2]:Manure_use[3]=2; Manure_use[3]:Manure_use[4]=3;")
data_14$dim2 <-car::recode(data_14$yield,"Yield[1]:Yield[2]=1; Yield[2]:Yield[3]=2; Yield[3]:Yield[4]=3;")

## join to map
data_14_final <- merge(data_14[,c("country", "dim1","dim2")],grd)
head(data_14_final)
data_14_final$id <- as.character(data_14_final$country)

mapsBivariate <- fortify(ctryMapOriginal,region="NAME_SORT")
mapsBivariate = plyr::join(mapsBivariate, data_14_final[,c("id","color")], by="id")

d1 <- ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")

Market <- as.numeric(quantile(data_14$market,probs=seq(0,1,length.out = 4)))
Yield <- as.numeric(quantile(data_14$yield,probs=seq(0,1,length.out = 4)))
data_14$dim1 <-car::recode(data_14$market,"Market[1]:Market[2]=1; Market[2]:Market[3]=2; Market[3]:Market[4]=3;")
data_14$dim2 <-car::recode(data_14$yield,"Yield[1]:Yield[2]=1; Yield[2]:Yield[3]=2; Yield[3]:Yield[4]=3;")

## join to map
data_14_final <- merge(data_14[,c("country", "dim1","dim2")],grd)
head(data_14_final)
data_14_final$id <- as.character(data_14_final$country)

mapsBivariate <- fortify(ctryMapOriginal,region="NAME_SORT")
mapsBivariate = plyr::join(mapsBivariate, data_14_final[,c("id","color")], by="id")

e1 <- ggplot() +
  geom_map(data = mapsBivariate, map = mapsBivariate,
           aes(x = long, y = lat,  map_id=id, fill=factor(color)),
           colour = "#7f7f7f", size=0.05) +
  scale_fill_identity()+
  theme_void()+
  theme(legend.position="none")


## create legend
g.legend <- ggplot(grd, aes(dim1,dim2,fill=factor(1:9)))+
  geom_tile()+
  scale_fill_manual(values=grd$color)+
  #theme_void()+
  theme(legend.position="none",axis.title=element_text(size=10),
        panel.background=element_blank(),plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black",size=10),
        axis.title.y = element_text(angle = 90),axis.text.y=element_text(angle=90))+
  labs(x="Coproduction factors a-e",y="Yield") +
  theme(axis.title=element_text(size=10),axis.text=element_text(size=10),
        axis.ticks = element_blank(),axis.text.x = element_text(vjust = 3),axis.text.y = element_text(vjust = -1))+
  scale_x_continuous(breaks=c(1,3),labels=c("low","high"))+
  scale_y_continuous(breaks=c(1,3),labels=c("low","high"))

vp<-viewport(width=0.24,height=0.4,x=0.12,y=0.3)


## plot
jpeg("2014_copr_yield_maps.jpeg", width = 15, height = 15, units = 'cm', res = 600)

ggarrange(a1,b1,c1,d1,e1,g.legend,
          labels = letters[1:5],font.label=list(size=8),
          ncol=2,nrow = 3)

dev.off()