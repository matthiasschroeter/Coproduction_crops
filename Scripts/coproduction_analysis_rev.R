
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
library(olsrr)

############################################################################################
###################              DATA               ########################################
############################################################################################

#### data all observations across all countries

data <- read.csv("data_mean.csv")[,-1]
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
log_data[,2:7] <- log(log_data[,2:7])
#log_data$year <- factor(log_data$year)
log_data$country <- as.character(log_data$country)

#subtract mean and divide by SD
log_data_centred <- log_data
log_data_centred[,2:7] <- scale(log_data_centred[,2:7], center=TRUE, scale=TRUE)


############################################################################################
###################              MODEL               #######################################
############################################################################################

### Model for all countries

# check for correlation among predictors
ggpairs(log_data[,c("Nt_area","cropsuit","manure_area","netcap14_area")])

# fit the model with interaction terms
mod <- lm(yield ~ Nt_area + cropsuit + netcap14_area + manure_area 
          + cropsuit:Nt_area + cropsuit:netcap14_area + cropsuit:manure_area, data = log_data_centred, weights=sqrt(log_data_centred$weight))
vif(mod)

# exclude interaction term cropsuit:netcap14_area because of high VIF
mod <- lm(yield ~ Nt_area + cropsuit + netcap14_area + manure_area
          + cropsuit:Nt_area + cropsuit:manure_area , data = log_data_centred, weights=sqrt(log_data_centred$weight))
vif(mod)

summary(mod)
plot(mod)

ols_plot_cooksd_bar(mod)

# Exclude 5,17,29,34 Azerbaijan, Egypt, Kuwait, and Malaysia
log_data_centred_excl <- log_data_centred[-c(5,17,29,34),]

# check for correlation among predictors
ggpairs(log_data_centred_excl[,c("Nt_area","cropsuit","manure_area","netcap14_area")])

# fit the model with interaction terms
mod <- lm(yield ~ Nt_area + cropsuit + netcap14_area + manure_area 
          + cropsuit:Nt_area + cropsuit:netcap14_area + cropsuit:manure_area, data = log_data_centred_excl, weights=sqrt(log_data_centred_excl$weight))
vif(mod)

# check that residuals are not correlated to the predictors
log_data_centred_excl$res <- residuals(mod)
layout(1)
plot(res ~ Nt_area + cropsuit + netcap14_area + manure_area, 
     data=log_data_centred_excl, pch=21, col=grey(0,0.6), bg=grey(0,0.2))

# check interactions

plot(cropsuit ~ Nt_area, cex=exp(log_data_centred_excl$res), col=grey(0,0.5), data=log_data_centred_excl)
plot(cropsuit ~ Nt_area, cex=exp(log_data_centred_excl$res), col= ifelse(log_data_centred$res < 0,'red','green'), data=log_data_centred_excl)

plot(cropsuit ~ manure_area, cex=exp(log_data_centred_excl$res), col=grey(0,0.5), data=log_data_centred_excl)
plot(cropsuit ~ manure_area, cex=exp(log_data_centred_excl$res), col= ifelse(log_data_centred_excl$res < 0,'red','green'), data=log_data_centred_excl)

plot(cropsuit ~ netcap14_area, cex=exp(log_data_centred_excl$res), col=grey(0,0.5), data=log_data_centred_excl)
plot(cropsuit ~ netcap14_area, cex=exp(log_data_centred_excl$res), col= ifelse(log_data_centred_excl$res < 0,'red','green'), data=log_data_centred_excl)

summary(mod)
plot(mod)


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
                      "Argentina","Australia","Bangladesh","Brazil","Canada",
                      "China,_mainland","Chile","Egypt","India","Indonesia",
                      "Malaysia","Mexico","Morocco","Pakistan","Philippines","South_Africa","Thailand",
                      "Turkey","United_States_of_America","Viet_Nam")

data_traject$IFA_Region <- NA

for(i in 1:length(data_traject[,1])){
  if(is.element(data_traject$Country[i],IFA_countries_00_14)){
    data_traject$IFA_Region[i]<-"IFA"}}

data_traject_IFA <- data_traject[which(data_traject$IFA_Region=="IFA"),]
data_traject_IFA <- data_traject_IFA[,c(1:6,11:12)]

#exclude NA
data_traject_IFA <- na.exclude(data_traject_IFA)

## Create EU27 data and keep it separate

EU27 <- c("Austria","Belgium","Bulgaria","Cyprus","Denmark","Estonia","Finland","France","Germany","Greece",
          "Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Romania",
          "Portugal","Slovakia","Slovenia","Spain","Sweden","United_Kingdom","Hungary","Czechia") 

data_traject_IFA$EU27 <- NA 
for(i in 1:length(data_traject_IFA[,1])){
  if(is.element(data_traject_IFA$Country[i],EU27)){
    data_traject_IFA$EU27[i]<-"EU27"}}

# load data for 2000
data_traject_00<-data_traject_IFA[which(data_traject_IFA$Year=="2000"),]
# load data for 2006
data_traject_06<-data_traject_IFA[which(data_traject_IFA$Year=="2006"),]
# load data for 2010
data_traject_10<-data_traject_IFA[which(data_traject_IFA$Year=="2010"),]
# load data for 2014
data_traject_14<-data_traject_IFA[which(data_traject_IFA$Year=="2014"),]

data_traject_00_EU27<-data_traject_00[which(data_traject_00$EU27=="EU27"),]
data_traject_06_EU27<-data_traject_06[which(data_traject_06$EU27=="EU27"),]
data_traject_10_EU27<-data_traject_10[which(data_traject_10$EU27=="EU27"),]
data_traject_14_EU27<-data_traject_14[which(data_traject_14$EU27=="EU27"),]

# one dataframe for EU27 per year
data_traject_EU27_00 <- data.frame("Country"="EU27","Year"=2000)
data_traject_EU27_00$Area_harvested_ha <- sum(data_traject_00_EU27$Area_harvested_ha)
data_traject_EU27_00$N_total_kj <- sum(data_traject_00_EU27$N_total_kj)
data_traject_EU27_00$N_total_t <- sum(data_traject_00_EU27$N_total_t)
data_traject_EU27_00$production_kj <- sum(data_traject_00_EU27$production_kj)
data_traject_EU27_00$net_capital_stocks14 <- sum(data_traject_00_EU27$net_capital_stocks14)
data_traject_EU27_00$Manure_kg <- sum(data_traject_00_EU27$Manure_kg)
data_traject_EU27_00$EU27 <- "EU27"

data_traject_EU27_06 <- data.frame("Country"="EU27","Year"=2006)
data_traject_EU27_06$Area_harvested_ha <- sum(data_traject_06_EU27$Area_harvested_ha)
data_traject_EU27_06$N_total_kj <- sum(data_traject_06_EU27$N_total_kj)
data_traject_EU27_06$N_total_t <- sum(data_traject_06_EU27$N_total_t)
data_traject_EU27_06$production_kj <- sum(data_traject_06_EU27$production_kj)
data_traject_EU27_06$net_capital_stocks14 <- sum(data_traject_06_EU27$net_capital_stocks14)
data_traject_EU27_06$Manure_kg <- sum(data_traject_06_EU27$Manure_kg)
data_traject_EU27_06$EU27 <- "EU27"

data_traject_EU27_10 <- data.frame("Country"="EU27","Year"=2010)
data_traject_EU27_10$Area_harvested_ha <- sum(data_traject_10_EU27$Area_harvested_ha)
data_traject_EU27_10$N_total_kj <- sum(data_traject_10_EU27$N_total_kj)
data_traject_EU27_10$N_total_t <- sum(data_traject_10_EU27$N_total_t)
data_traject_EU27_10$production_kj <- sum(data_traject_10_EU27$production_kj)
data_traject_EU27_10$net_capital_stocks14 <- sum(data_traject_10_EU27$net_capital_stocks14)
data_traject_EU27_10$Manure_kg <- sum(data_traject_10_EU27$Manure_kg)
data_traject_EU27_10$EU27 <- "EU27"

data_traject_EU27_14 <- data.frame("Country"="EU27","Year"=2014)
data_traject_EU27_14$Area_harvested_ha <- sum(data_traject_14_EU27$Area_harvested_ha)
data_traject_EU27_14$N_total_kj <- sum(data_traject_14_EU27$N_total_kj)
data_traject_EU27_14$N_total_t <- sum(data_traject_14_EU27$N_total_t)
data_traject_EU27_14$production_kj <- sum(data_traject_14_EU27$production_kj)
data_traject_EU27_14$net_capital_stocks14 <- sum(data_traject_14_EU27$net_capital_stocks14)
data_traject_EU27_14$Manure_kg <- sum(data_traject_14_EU27$Manure_kg)
data_traject_EU27_14$EU27 <- "EU27"

data_traject_EU_00 <- rbind(data_traject_00,data_traject_EU27_00)
data_traject_EU_06 <- rbind(data_traject_06,data_traject_EU27_06)
data_traject_EU_10 <- rbind(data_traject_10,data_traject_EU27_10)
data_traject_EU_14 <- rbind(data_traject_14,data_traject_EU27_14)

data_traject_all <- rbind(data_traject_EU_00,data_traject_EU_06,data_traject_EU_10,data_traject_EU_14)

data_traject_all$Income <- NA

lower_middle <- c("Bangladesh","Egypt","India","Indonesia","Morocco","Pakistan","Philippines","Viet_Nam")
upper_middle <- c("Brazil","China,_mainland","Malaysia","Mexico","South_Africa","Thailand","Turkey")
high <- c("EU27","Argentina",
          "Australia","Canada","Chile","United_States_of_America")

for(i in 1:length(data_traject_all[,1])){
  if(is.element(data_traject_all$Country[i],lower_middle)){
    data_traject_all$Income[i]<-"lower_middle"} 
  if(is.element(data_traject_all$Country[i],upper_middle)){
    data_traject_all$Income[i]<-"upper_middle"}
  if(is.element(data_traject_all$Country[i],high)){
    data_traject_all$Income[i]<-"high"}}

crops_lower_middle <- data_traject_all[which(data_traject_all$Income=="lower_middle"),]
crops_upper_middle <- data_traject_all[which(data_traject_all$Income=="upper_middle"),]
crops_high <- data_traject_all[which(data_traject_all$Income=="high"),]

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

# increase 2014 compared to 2000 per country

crops_lower_middle_14$yield_inc <- ((crops_lower_middle_14$production_kj/crops_lower_middle_14$Area_harvested_ha)/(crops_lower_middle_00$production_kj/crops_lower_middle_00$Area_harvested_ha)-1.0)*100
crops_lower_middle_14$fert_inc <- ((crops_lower_middle_14$N_total_t/crops_lower_middle_14$Area_harvested_ha)/(crops_lower_middle_00$N_total_t/crops_lower_middle_00$Area_harvested_ha)-1.0)*100
crops_lower_middle_14$man_inc <- ((crops_lower_middle_14$Manure_kg/crops_lower_middle_14$Area_harvested_ha)/(crops_lower_middle_00$Manure_kg/crops_lower_middle_00$Area_harvested_ha)-1.0)*100
crops_lower_middle_14$netcap_inc <- ((crops_lower_middle_14$net_capital_stocks14/crops_lower_middle_14$Area_harvested_ha)/(crops_lower_middle_00$net_capital_stocks14/crops_lower_middle_00$Area_harvested_ha)-1.0)*100

mean(crops_lower_middle_14$yield_inc)
range(crops_lower_middle_14$yield_inc)

mean(crops_lower_middle_14$fert_inc)
range(crops_lower_middle_14$fert_inc)

mean(crops_lower_middle_14$man_inc)
range(crops_lower_middle_14$man_inc)

mean(crops_lower_middle_14$netcap_inc)
range(crops_lower_middle_14$netcap_inc)


crops_upper_middle_14$yield_inc <- ((crops_upper_middle_14$production_kj/crops_upper_middle_14$Area_harvested_ha)/(crops_upper_middle_00$production_kj/crops_upper_middle_00$Area_harvested_ha)-1.0)*100
crops_upper_middle_14$fert_inc <- ((crops_upper_middle_14$N_total_t/crops_upper_middle_14$Area_harvested_ha)/(crops_upper_middle_00$N_total_t/crops_upper_middle_00$Area_harvested_ha)-1.0)*100
crops_upper_middle_14$man_inc <- ((crops_upper_middle_14$Manure_kg/crops_upper_middle_14$Area_harvested_ha)/(crops_upper_middle_00$Manure_kg/crops_upper_middle_00$Area_harvested_ha)-1.0)*100
crops_upper_middle_14$netcap_inc <- ((crops_upper_middle_14$net_capital_stocks14/crops_upper_middle_14$Area_harvested_ha)/(crops_upper_middle_00$net_capital_stocks14/crops_upper_middle_00$Area_harvested_ha)-1.0)*100

mean(crops_upper_middle_14$yield_inc)
range(crops_upper_middle_14$yield_inc)

mean(crops_upper_middle_14$fert_inc)
range(crops_upper_middle_14$fert_inc)

mean(crops_upper_middle_14$man_inc)
range(crops_upper_middle_14$man_inc)

mean(crops_upper_middle_14$netcap_inc)
range(crops_upper_middle_14$netcap_inc)

crops_high_14$yield_inc <- ((crops_high_14$production_kj/crops_high_14$Area_harvested_ha)/(crops_high_00$production_kj/crops_high_00$Area_harvested_ha)-1.0)*100
crops_high_14$fert_inc <- ((crops_high_14$N_total_t/crops_high_14$Area_harvested_ha)/(crops_high_00$N_total_t/crops_high_00$Area_harvested_ha)-1.0)*100
crops_high_14$man_inc <- ((crops_high_14$Manure_kg/crops_high_14$Area_harvested_ha)/(crops_high_00$Manure_kg/crops_high_00$Area_harvested_ha)-1.0)*100
crops_high_14$netcap_inc <- ((crops_high_14$net_capital_stocks14/crops_high_14$Area_harvested_ha)/(crops_high_00$net_capital_stocks14/crops_high_00$Area_harvested_ha)-1.0)*100

mean(crops_high_14$yield_inc)
range(crops_high_14$yield_inc)

mean(crops_high_14$fert_inc)
range(crops_high_14$fert_inc)

mean(crops_high_14$man_inc)
range(crops_high_14$man_inc)

mean(crops_high_14$netcap_inc)
range(crops_high_14$netcap_inc)

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
     pch = 16, col = "black", xlab = "Manure use intensity (%, 100=2000)", ylab = "Yield (%, 100=2000)", xlim = c(90,130), cex.axis = 1.4, cex.lab = 1.4)
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