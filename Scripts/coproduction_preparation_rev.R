
## DATA PREPARATION: INTEGRATE FAO DATA, IFA DATA AND OTHER DATA SOURCES

############################################################################################################
#####                            First Table, 11 crop groups                                           #####
############################################################################################################
##### load data for table 1 #####
Crops<-read.csv("Production_Crops_E_All_Data_(Normalized).csv",sep=",",header=T)

# cut the different elements out of the table "crops" and create two tables (Area_havested and Production)
# rename the Value columns into area_havested/Production (do the same with the Unit and Flag columns)
# reduce the tables to the needed columns 
# merge the three tabels 

Area_harvested<- Crops[which(Crops$Element =='Area harvested'),]
colnames(Area_harvested)[colnames(Area_harvested)=="Value"] <- "Area_harvested_ha"
Area_harvested_k<-Area_harvested[,c("Area","Item","Year","Area_harvested_ha")]

Production<- Crops[which(Crops$Element =='Production'),]
colnames(Production)[colnames(Production)=="Value"] <- "Production_t"
Production_k<-Production[,c("Area","Item","Year","Production_t")]

# merge the first two tabels
Area_Production <- merge(Area_harvested_k,Production_k, by=c("Area","Item","Year"),all=T)

################ save table for a shortcut ######################################################
# write.csv(Area_Production,file="Area_Production.csv")
Area_Production<-read.csv("Area_Production.csv",sep=",",header=T)
#####################################################################################################

#  select the years for which we have fertilizer data (2000, 2006, 2010 and 2014)
Area_Production_years<-Area_Production[Area_Production$Year %in% c("2000","2006","2010","2014"), ]

################ save table for a shortcut ######################################################
write.csv(Area_Production_years,file="Area_Production_years.csv")
Area_Production_years<-read.csv("Area_Production_years.csv",sep=",",header=T)
#####################################################################################################
Area_Production_years$Area<-paste(gsub("[ ]","_",Area_Production_years$Area))

#  select the countries for which fertilizer data is available
countries_IFA<-Area_Production_years[Area_Production_years$Area %in% c(
  "Albania", "Algeria", "Argentina", "Australia", "Azerbaijan", "Bangladesh", "Belarus", 
  "Bolivia_(Plurinational_State_of)","Brazil", "Cambodia", "Canada", "Chile", "China,_mainland",
  "Colombia", "Costa_Rica", "Dominican_Republic", "Ecuador", "Egypt", "El_Salvador", "Ethiopia", 
  "Guatemala", "Guinea", "Honduras", "India", "Indonesia", "Israel", "Japan", "Jordan", "Kenya", "Kuwait",  
  "Lao_People's_Democratic_Republic", "Lebanon" ,"Madagascar" ,"Malawi" ,"Malaysia" ,"Mauritania" ,"Mexico", 
  "Morocco", "Myanmar", "New_Zealand", "Nicaragua", "Nigeria", "Norway", "Pakistan", "Paraguay",
  "Philippines", "Republic_of_Korea", "Republic_of_Moldova", "Saudi_Arabia", "South_Africa", "Sri_Lanka", 
  "Switzerland", "Syrian_Arab_Republic", "Thailand", "Togo", "Turkey", "United_Republic_of_Tanzania", 
  "United_States_of_America", "Uruguay", "Venezuela_(Bolivarian_Republic_of)", "Viet_Nam", "Zambia", 
  "Zimbabwe", "Iran_(Islamic_Republic_of)", "Russian_Federation", "Ukraine", "Uzbekistan",
  "Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
  "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
  "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United Kingdom","Croatia"), ] 

unique(countries_IFA$Area)

################ save the table for a shortcut ######################################################
write.csv(countries_IFA,file="countries_IFA.csv")
countries_IFA<-read.csv("countries_IFA.csv",sep=",",header=T)
#####################################################################################################

#####################################################################################################
#####                             Crop selection                                                #####
#####################################################################################################
# we need this 11 crop group table later, because fertilizer use is provided for crop groups

# unique Items
unique(countries_IFA$Item)

# Crop selection
crops_11CG_20<-countries_IFA[countries_IFA$Item %in% c(
  "Wheat","Bran, wheat","Bulgur","Flour, wheat",
  "Rice, paddy","Oil, rice bran","Cake, rice bran",
  "Maize","Maize, green","Bran, maize","Cake, maize","Flour, maize","Germ, maize","Oil, maize","Popcorn",
  "Sweet corn frozen","Sweet corn prep or preserved",
  "Barley","Barley, pearled","Sorghum","Bran, sorghum","Oats", "Oats rolled","Rye","Triticale","Millet",
  "Bran, millet","Buckwheat", "Fonio",  "Flour, fonio", "Quinoa",
  "Grain, mixed","Cereal preparations, nes","Flour, cereals","Cereals, nes",
  "Soybeans","Cake, soybeans","Oil, soybean",
  "Oil palm fruit","Oil, palm","Palm kernels","Oil, palm kernel","Cake, palm kernel",
  "Rapeseed","Cake, rapeseed","Oil, rapeseed","Mustard seed","Cake, mustard","Flour, mustard","Sunflower seed",
  "Cake, sunflower", "Oil, sunflower", "Groundnuts, with shell","Groundnuts, shelled","Peanut butter",
  "Cake, groundnuts","Oil, groundnut","Coconuts","Coconuts, desiccated","Castor oil seed","	Oil, castor beans",
  "Hempseed","Cake, hempseed","	Karite nuts (sheanuts)","Butter of karite nuts","Linseed","Cake, linseed",
  "Oil, linseed","Olives","Oil, olive, virgin","Olives preserved","	Poppy seed","Oil, poppy","Sesame seed",
  "Cake, sesame seed","Oil, sesame",
  "Sugar beet","Beet pulp","Sugar cane","Sugar non-centrifugal","Cane tops","Molasses",
  "Cassava","Cassava leaves","Cassava dried","Starch, cassava","Potatoes",
  "Flour, potatoes","Potatoes, frozen","Potato offals","Sweet potatoes","Yams","Taro (cocoyam)",
  "Yautia (cocoyam)","Flour, roots and tubers nes","Roots and tubers, nes",
  
  "Artichokes","Asparagus","Beans, green","	Cabbages and other brassicas","	Carrots and turnips",
  "Cauliflowers and broccoli","Chillies and peppers, green","Cucumbers and gherkins","Eggplants (aubergines)",
  "Garlic","Leeks, other alliaceous vegetables","Lettuce and chicory","	Mushrooms and truffles","Okra",
  "Onions, dry",	"Onions, shallots, green","Peas, green","Pumpkins, squash and gourds","Spinach",
  "String beans","Tomatoes","Tomatoes, paste","Tomatoes, peeled","Turnips for fodder",
  "Mushrooms, canned","Juice, tomato",
  
  "Apples","Apricots","Apricots, dry","Avocados","Bananas","Blueberries","Carobs","Cashewapple",
  "Cherries","Cherries, sour","Cranberries","Currants", "Dates", "Figs","	Figs dried", "Gooseberries", 
  "Grapefruit (inc. pomelos)","Grapes","Raisins", "Kiwi fruit", "Lemons and limes","Mangoes, mangosteens, guavas",
  "Melons, other (inc.cantaloupes)","Oranges", "Papayas", "Peaches and nectarines","Pears","Persimmons",
  "Pineapples","Pineapples canned",	"Plantains and others","Plums and sloes","Plums dried (prunes)",
  "Quinces","Raspberries", "Strawberries","Tangerines, mandarins, clementines, satsumas","Watermelons",
  "Fruit, citrus nes","Fruit, dried nes","Juice, grapefruit","Juice, grapefruit, concentrated",
  "Juice, orange, concentrated","Juice, orange, single strength","Juice, pineapple","Juice, pineapple, concentrated",
  "Juice, plum, concentrated","Juice, plum, single strength","Juice, citrus, concentrated",
  "Juice, citrus, single strength","Juice, fruit nes","Juice, grape","Juice, lemon, concentrated"), ] 

################ save the table for a short cut ######################################################
write.csv(crops_11CG_20,file="crops_11CG_20.csv")
crops_11CG_20<-read.csv("crops_11CG_20.csv",sep=",",header=T)
############################################################################################################

############################################################################################################
#####                            Second Table, 15 crops                                                #####
############################################################################################################
### load data, table 1
crops_11CG_20<-read.csv("crops_11CG_20.csv",sep=",",header=T)[,-1]

############################################################################################################
############################      Crop selection                   #########################################
############################################################################################################

# now we would like to create a list which contanines only the 15 crops from the Zabel et al. (2014) paper:
# Barley, Cassava, Groundnut, Maize, Millet, Oil_Palm, Potato, Rapeseed/canola, Rice, Rye, Sorghum, Soybean,
# Sugarcane, Sunflower, Wheat (incl. summer and winter wheat)

crops_16x<-crops_11CG_20[crops_11CG_20$Item %in% c(
  "Wheat",
  "Rice, paddy",
  "Maize","Maize, green",
  "Barley","Sorghum","Rye","Millet",
  "Soybeans",
  "Oil palm fruit","Palm kernels",
  "Rapeseed","Sunflower seed", "Groundnuts, with shell",
  "Sugar cane",
  "Cassava","Cassava leaves","Potatoes"
), ] 

# units: Area_harvested=ha, Production = tonnes
################save table for a shortcut #######################################################
write.csv(crops_16x,file="crops_16x.csv")
crops_16x<-read.csv("crops_16x.csv",sep=",",header=T)[,-1]
############################################################################################################

############################################################################################################
#####   join kJ data                                                                                    ####
############################################################################################################

# some crops have a different name in the kJ list than in FAO  data -> need to correct this
# convert kcal into KJ
# convert KJ/100g into Kj/t

# load data
kcal_1<-read.csv("kcal.csv",sep=";",header=T)
colnames(kcal_1)<-c("ITEM", "kcal_100g")
crops_16ax<-crops_16x

# check whether the crops have the same name in both lists
crops_16ax_U<-unique(crops_16ax$Item)
crops_16ax$Item<-toupper(crops_16ax$Item) 
crops_kcal_1U<-unique(kcal_1$ITEM)

# identify crops that exist in the 15 crops list but not in the kcal list (eg. have different name in kcal list)
toupper(crops_16ax_U[which(is.element(toupper(unique(crops_16ax$Item)),unique(toupper(kcal_1$ITEM)))==F)])

kcal_1$ITEM<-as.character(kcal_1$ITEM)
#correct the names in the kcal list
kcal_1$ITEM[which(kcal_1$ITEM=="GREEN CORN")]<-"MAIZE, GREEN"
kcal_1$ITEM[which(kcal_1$ITEM=="RICE PADDY")]<-"RICE, PADDY"
kcal_1$ITEM[which(kcal_1$ITEM=="GROUNDNUTS IN SHELL")]<-"GROUNDNUTS, WITH SHELL"

# convert kcal into KJ
#1 kcal =  4,1868 kJ
kcal_1$kj_100g<-NA
kcal_1$kj_100g<-kcal_1$kcal_100g*4.1868
#per tonne
kcal_1$kj_t<-NA
kcal_1$kj_t<-kcal_1$kj_100g*1e+4

################save table for a shortcut ########################################################
write.csv(kcal_1,file="kcal_1.csv")
kcal_1<-read.csv("kcal_1.csv",sep=",",header=T)
#############################################################################################################

# integrate the kcal data in our current table
# kcal_1 unit: kJ per tonne

#kj per tonne into 15 crop list
crops_16ax$Item<-toupper(crops_16ax$Item)
crops_16ax$kj_t_crop<-NA

for(i in 1:length(crops_16ax[,1])){
  crops_16ax$kj_t_crop[i]<-kcal_1$kj_t[which(kcal_1$ITEM==crops_16ax$Item[i])]}

################save table for a shortcut ########################################################
write.csv(crops_16ax,file="crops_16ax.csv")
crops_16ax<-read.csv("crops_16ax.csv",sep=",",header=T)
#############################################################################################################

# multiply the kJ value of each crop with the respective production of each country and year
crops_16bx<-crops_16ax
# new row 
crops_16bx$production_kj<-NA
# calculated, t * kj/t -> kj 
crops_16bx$production_kj<-crops_16bx$Production_t*crops_16bx$kj_t_crop

################save table for a shortcut ########################################################
write.csv(crops_16bx,file="crops_16bx.csv")
crops_16bx<-read.csv("crops_16bx.csv",sep=",",header=T)[,-1]
#############################################################################################################

crops_16cx<-crops_16bx
# aggregate kJ values in order to get only one value per year, country and column
# new rownames 
row.names(crops_16cx)<-1:length(crops_16cx[,1])

# create a code for the aggregation
crops_16cx$code_Area_Year<-NA
crops_16cx$code_Area_Year<-paste(gsub("[ ]","_",crops_16cx$Area),crops_16cx$Year)

# aggregate kJ values for each column separately
kJ_total_production <- aggregate(production_kj~code_Area_Year, data=crops_16cx,sum)
Area_H<- aggregate(Area_harvested_ha~code_Area_Year, data=crops_16cx,sum)

# merge them again into one table
total_kJ <- merge(kJ_total_production,Area_H, by="code_Area_Year",all=T)

# split up the code
y<-unlist(strsplit(total_kJ[,1], " "))
y1<-paste(y[seq(2,length(y),2)])
total_kJ[,4]<-y1
y2<-paste(y[seq(1,length(y),2)])
total_kJ[,5]<-y2
# new column names
colnames(total_kJ)[colnames(total_kJ)=="V5"] <- "Country"
colnames(total_kJ)[colnames(total_kJ)=="V4"] <- "Year"

# new column order
crops_16cx_agg<-total_kJ[,c("Country","Year","Area_harvested_ha","production_kj","code_Area_Year")]

################save table for a shortcut ########################################################
write.csv(crops_16cx_agg,file="crops_16cx_agg.csv")
crops_16cx_agg<-read.csv("crops_16cx_agg.csv",sep=",",header=T)[,-1]
#############################################################################################################

#############################################################################################################
### integrate crop-specific fertilizer data 
### Refer to scripts Fertilizer_2000.R, Fertilizer_2006.R, Fertilizer_2010.R, Fertilizer_2014.R
#############################################################################################################
crops_16ex<-crops_16cx_agg

# load data
Fertilizer_2000b<-read.csv("Fertilizer_2000b.csv",sep=",",header=T)[,c("Area","N_total_kj","sum_t")]
Fertilizer_2000b$Year<-"2000"
colnames(Fertilizer_2000b)[colnames(Fertilizer_2000b)=="sum_t"] <- "N_total_t"

Fertilizer_2006d<-read.csv("Fertilizer_2006d.csv",sep=",",header=T)[,c("Area","N_total_kj","sum_t")]
Fertilizer_2006d$Year<-"2006"
colnames(Fertilizer_2006d)[colnames(Fertilizer_2006d)=="sum_t"] <- "N_total_t"

Fertilizer_2010c<-read.csv("Fertilizer_2010c.csv",sep=",",header=T)[,c("Area","N_total_kj","sum_t")]
Fertilizer_2010c$Year<-"2010"
colnames(Fertilizer_2010c)[colnames(Fertilizer_2010c)=="sum_t"] <- "N_total_t"

Fertilizer_2014c<-read.csv("Fertilizer_2014c.csv",sep=",",header=T)[,c("Area","N_total_kj","sum_t")]
Fertilizer_2014c$Year<-"2014"    
colnames(Fertilizer_2014c)[colnames(Fertilizer_2014c)=="sum_t"] <- "N_total_t"

# bind the four tables into one
Fertilizer_all<-rbind(Fertilizer_2000b,Fertilizer_2006d,Fertilizer_2010c,Fertilizer_2014c)

# create a code 
Fertilizer_all$code_Area_Year<-NA
Fertilizer_all$code_Area_Year<-paste(gsub("[ ]","_",Fertilizer_all$Area),Fertilizer_all$Year)
Fertilizer_all$code_Area_Year<-as.character(Fertilizer_all$code_Area_Year)
crops_16ex$code_Area_Year<-as.character(crops_16ex$code_Area_Year)

code2<-unique(Fertilizer_all$code_Area_Year)

# new column for the total energy consumption  production of total fertilizer used per country and year
crops_16ex$N_total_kj<-NA
#fill column
for(i in 1:length(crops_16ex[,1])){
  if(is.element(crops_16ex$code_Area_Year[i],code2)){
    crops_16ex$N_total_kj[i]<-Fertilizer_all$N_total_kj[which(Fertilizer_all$code_Area_Year==crops_16ex$code_Area_Year[i])]} 
  else {crops_16ex$N_total_kj[i]<-NA}}

# new column for the total fertilizer used in t per country and year
crops_16ex$N_total_t<-NA
#fill column
for(i in 1:length(crops_16ex[,1])){
  if(is.element(crops_16ex$code_Area_Year[i],code2)){
    crops_16ex$N_total_t[i]<-Fertilizer_all$N_total_t[which(Fertilizer_all$code_Area_Year==crops_16ex$code_Area_Year[i])]} 
  else {crops_16ex$N_total_t[i]<-NA}}

################save the table for a shortcut ########################################################
write.csv(crops_16ex,file="crops_16ex.csv")
crops_16ex<-read.csv("crops_16ex.csv",sep=",",header=T)[,-1]
#############################################################################################################

# new colum order
crops_16fx<-crops_16ex[,c("Country","Year","Area_harvested_ha","N_total_kj","N_total_t","production_kj","code_Area_Year")]

#############################################################################################################
###  crop suitability data                                                                              #####
#############################################################################################################
# area-weighted suitability values for whole country, based on cropland extent of Monfreda et al. (2008)

crop_suit_zabel<-read.csv("cropsuit_overall.csv",sep=";",header=T)
crop_suit_zabel$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel$NAME_LONG))

code_k<-unique(crops_16fx$Country)
code_k[which(is.element(unique(crops_16fx$Country),unique(crop_suit_zabel$NAME_LONG))==F)]

# correct names of countries
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="China")]<-"China,_mainland"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Iran")]<-"Iran_(Islamic_Republic_of)"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Moldova")]<-"Republic_of_Moldova"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Syria")]<-"Syrian_Arab_Republic"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Tanzania")]<-"United_Republic_of_Tanzania"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="United_States")]<-"United_States_of_America"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
crop_suit_zabel$NAME_LONG[which(crop_suit_zabel$NAME_LONG=="Vietnam")]<-"Viet_Nam"

crop_suit_zabel$code_Area_Year<-NA
crop_suit_zabel$code_Area_Year<-paste(gsub("[ ]","_",crop_suit_zabel$NAME_LONG),"2010")

code_suit<-unique(crops_16fx$Country)
crops_16fx$suitability<-NA
for(i in 1:length(crops_16fx[,1])){
  if((is.element(crops_16fx$Country[i],code_suit))&(crops_16fx$Year[i]=="2010")){
    crops_16fx$suitability[i]<-crop_suit_zabel$SUM[which(crop_suit_zabel$NAME_LONG==crops_16fx$Country[i])]} 
  else {crops_16fx$suitability[i]<-NA}}

# suitability of 2010 for each year
years<-c("2000","2006","2010","2014")

code_suit<-unique(crops_16fx$Country)
crops_16fx$suitability<-NA
for(i in 1:length(crops_16fx[,1])){
  if((is.element(crops_16fx$Country[i],code_suit))&(is.element(crops_16fx$Year[i],years))){
    crops_16fx$suitability[i]<-crop_suit_zabel$SUM[which(crop_suit_zabel$NAME_LONG==crops_16fx$Country[i])]} 
  else {crops_16fx$suitability[i]<-NA}}

## crop suitability data only on cropland as indicated by ESA CCI land cover data,
#not as an average for the country as a whole

#crop_suit_zabel_00<-read.csv("cropsuit_Zabel_00.csv",sep=";",header=T)
#crop_suit_zabel_00$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_00$NAME_LONG))

#crop_suit_zabel_06<-read.csv("cropsuit_Zabel_06.csv",sep=";",header=T)
#crop_suit_zabel_06$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_06$NAME_LONG))

#crop_suit_zabel_10<-read.csv("cropsuit_Zabel_10.csv",sep=";",header=T)
#crop_suit_zabel_10$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_10$NAME_LONG))

#crop_suit_zabel_14<-read.csv("cropsuit_Zabel_14.csv",sep=";",header=T)
#crop_suit_zabel_14$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_14$NAME_LONG))

#code_k<-unique(crops_16fx$Country)
#code_k[which(is.element(unique(crops_16fx$Country),unique(crop_suit_zabel_14$NAME_LONG))==F)]

#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Czech_Republic")]<-"Czechia"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="China")]<-"China,_mainland"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Czech_Republic")]<-"Czechia"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Iran")]<-"Iran_(Islamic_Republic_of)"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Moldova")]<-"Republic_of_Moldova"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Syria")]<-"Syrian_Arab_Republic"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Tanzania")]<-"United_Republic_of_Tanzania"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="United_States")]<-"United_States_of_America"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
#crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Vietnam")]<-"Viet_Nam"

crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="China")]<-"China,_mainland"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Iran")]<-"Iran_(Islamic_Republic_of)"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Moldova")]<-"Republic_of_Moldova"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Syria")]<-"Syrian_Arab_Republic"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Tanzania")]<-"United_Republic_of_Tanzania"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="United_States")]<-"United_States_of_America"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
crop_suit_zabel_06$NAME_LONG[which(crop_suit_zabel_06$NAME_LONG=="Vietnam")]<-"Viet_Nam"

crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="China")]<-"China,_mainland"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Iran")]<-"Iran_(Islamic_Republic_of)"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Moldova")]<-"Republic_of_Moldova"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Syria")]<-"Syrian_Arab_Republic"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Tanzania")]<-"United_Republic_of_Tanzania"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="United_States")]<-"United_States_of_America"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
crop_suit_zabel_10$NAME_LONG[which(crop_suit_zabel_10$NAME_LONG=="Vietnam")]<-"Viet_Nam"

crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="China")]<-"China,_mainland"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Iran")]<-"Iran_(Islamic_Republic_of)"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Moldova")]<-"Republic_of_Moldova"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Syria")]<-"Syrian_Arab_Republic"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Tanzania")]<-"United_Republic_of_Tanzania"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="United_States")]<-"United_States_of_America"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
crop_suit_zabel_14$NAME_LONG[which(crop_suit_zabel_14$NAME_LONG=="Vietnam")]<-"Viet_Nam"

#crops_16fx$cropsuit<-NA
#for(i in 1:length(crops_16fx[,1])){
#  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2000")){
#    crops_16fx$cropsuit[i]<-crop_suit_zabel_00$MEAN[which(crop_suit_zabel_00$NAME_LONG==crops_16fx$Country[i])]} 
#  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2006")){
#    crops_16fx$cropsuit[i]<-crop_suit_zabel_06$MEAN[which(crop_suit_zabel_06$NAME_LONG==crops_16fx$Country[i])]}
#  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2010")){
#    crops_16fx$cropsuit[i]<-crop_suit_zabel_10$MEAN[which(crop_suit_zabel_10$NAME_LONG==crops_16fx$Country[i])]}
#  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2014")){
#    crops_16fx$cropsuit[i]<-crop_suit_zabel_14$MEAN[which(crop_suit_zabel_14$NAME_LONG==crops_16fx$Country[i])]}
#}

#############################################################################################################
### market influence data                                                                               #####
#############################################################################################################
# market influence data on cropland extent based on Monfreda et al. (2008)

market <- read.csv("market_15crops.csv", sep = ";", header = T)
colnames(market)<-c("Country","Mean")
market$Country<-paste(gsub("[ ]","_",market$Country))
code_k[which(is.element(unique(crops_16fx$Country),unique(market$Country))==F)]
market$Country[which(market$Country=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
market$Country[which(market$Country=="China")]<-"China,_mainland"
market$Country[which(market$Country=="Czech_Republic")]<-"Czechia"
market$Country[which(market$Country=="Iran")]<-"Iran_(Islamic_Republic_of)"
market$Country[which(market$Country=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
market$Country[which(market$Country=="Moldova")]<-"Republic_of_Moldova"
market$Country[which(market$Country=="Syria")]<-"Syrian_Arab_Republic"
market$Country[which(market$Country=="Tanzania")]<-"United_Republic_of_Tanzania"
market$Country[which(market$Country=="United_States")]<-"United_States_of_America"
market$Country[which(market$Country=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
market$Country[which(market$Country=="Vietnam")]<-"Viet_Nam"

# market influence value for each year
years<-c("2000","2006","2010","2014")

code_k<-unique(crops_16fx$Country)
crops_16fx$market<-NA
for(i in 1:length(crops_16fx[,1])){
  if((is.element(crops_16fx$Country[i],code_market))&(is.element(crops_16fx$Year[i],years))){
    crops_16fx$market[i]<-market$Mean[which(market$Country==crops_16fx$Country[i])]} 
  else {crops_16fx$market[i]<-NA}}



#############################################################################################################
### net capital stock                                                                                   #####
#############################################################################################################
net_cap<-read.csv("FAOSTAT_data_12-12-2018.csv",sep=",",header=T)
net_cap$Area<-paste(gsub("[ ]","_",net_cap$Area))
code_k<-unique(crops_16fx$Country)
code_k[which(is.element(unique(crops_16fx$Country),unique(net_cap$Area))==F)]
net_cap$Area[which(net_cap$Area=="")]<-"Croatia"
net_cap$code_Area_Year<-NA
net_cap$code_Area_Year<-paste(gsub("[ ]","_",net_cap$Area),net_cap$Year)

crops_16fx$net_capital_stocks<-NA
code_nk<-unique(net_cap$code_Area_Year)
for(i in 1:length(crops_16fx[,1])){
  if(is.element(crops_16fx$code_Area_Year[i],code_nk)){
    crops_16fx$net_capital_stocks[i]<-net_cap$Value[which(net_cap$code_Area_Year==crops_16fx$code_Area_Year[i])]} 
  else {crops_16fx$net_capital_stocks[i]<-NA}}

# correct values for Belarus for net_capital_stocks
crops_16fx[which(crops_16fx$Country=="Belarus"&crops_16fx$Year=="2000"),"net_capital_stocks"]<-16379.36
crops_16fx[which(crops_16fx$Country=="Belarus"&crops_16fx$Year=="2006"),"net_capital_stocks"]<-20439.1
crops_16fx[which(crops_16fx$Country=="Belarus"&crops_16fx$Year=="2010"),"net_capital_stocks"]<-26802.53
crops_16fx[which(crops_16fx$Country=="Belarus"&crops_16fx$Year=="2014"),"net_capital_stocks"]<-26658.14

#correct net capital stock prices to 2014 values with CPI data https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=1.00&year1=200001&year2=201401
crops_16fx$net_capital_stocks14 <- NA 
for(i in 1:length(crops_16fx[,1])){
  if(crops_16fx$Year[i]=="2000"){
    crops_16fx$net_capital_stocks14[i]<-crops_16fx$net_capital_stocks[i]*1.39} 
  if(crops_16fx$Year[i]=="2006"){
    crops_16fx$net_capital_stocks14[i]<-crops_16fx$net_capital_stocks[i]*1.18}
  if(crops_16fx$Year[i]=="2010"){
    crops_16fx$net_capital_stocks14[i]<-crops_16fx$net_capital_stocks[i]*1.08}
  if(crops_16fx$Year[i]=="2014"){
    crops_16fx$net_capital_stocks14[i]<-crops_16fx$net_capital_stocks[i]*1.00}
}

#############################################################################################################
### Manure use  data                                                                                    #####
#############################################################################################################
Manure<-read.csv("manure_FAO.csv",sep=";",header=T)
Manure$Area<-paste(gsub("[ ]","_",Manure$Area))
code_k<-unique(crops_16fx$Country)
code_k[which(is.element(unique(crops_16fx$Country),unique(Manure$Area))==F)]

Manure<-Manure[which(Manure$Element=="Manure applied to soils (N content)"),]

Manure$code_Area_Year<-NA
Manure$code_Area_Year<-paste(gsub("[ ]","_",Manure$Area),Manure$Year)

Manure$code_Area_Year<-as.character(Manure$code_Area_Year)
crops_16fx$code_Area_Year<-as.character(crops_16fx$code_Area_Year)

crops_16fx$Manure_kg<-NA
code_man <-unique(Manure$code_Area_Year)
for(i in 1:length(crops_16fx[,1])){
  if(is.element(crops_16fx$code_Area_Year[i],code_man)){
    crops_16fx$Manure_kg[i]<-Manure$Value[which(Manure$code_Area_Year==crops_16fx$code_Area_Year[i])]} 
  else {crops_16fx$Manure_kg[i]<-NA}}

################ save table for a shortcut #######################################################
write.csv(crops_16fx,file="crops_16fx.csv")
crops_16fx<-read.csv("crops_16fx.csv",sep=",",header=T)[,-1]
##############################################################################################################
##Clean the data
crops_16Lx_clean<-crops_16fx

# input/ area harvested
crops_16Lx_clean$N_div_AreaH_kj_ha<-NA
crops_16Lx_clean$N_div_AreaH_kj_ha<-crops_16Lx_clean$N_total_kj/crops_16Lx_clean$Area_harvested_ha
# output/ area harvested
crops_16Lx_clean$production_div_AreaH_kj_ha<-NA
crops_16Lx_clean$production_div_AreaH_kj_ha<-crops_16Lx_clean$production_kj/crops_16Lx_clean$Area_harvested_ha
# Manure/area harvested
crops_16Lx_clean$manure_area<-crops_16Lx_clean$Manure_kg/crops_16Lx_clean$Area_harvested_ha
# net_capital_stocks/area harvested
crops_16Lx_clean$netcap_area<-crops_16Lx_clean$net_capital_stocks/crops_16Lx_clean$Area_harvested_ha
#net_capital_stocks14/area harvested
crops_16Lx_clean$netcap14_area<-crops_16Lx_clean$net_capital_stocks14/crops_16Lx_clean$Area_harvested_ha
#production_kJ/N_total_kj
crops_16Lx_clean$TFP <- crops_16Lx_clean$production_kj/crops_16Lx_clean$N_total_kj
#Nitrogen in tonnes per ha
crops_16Lx_clean$Nt_area <- NA
crops_16Lx_clean$Nt_area <- crops_16Lx_clean$N_total_t/crops_16Lx_clean$Area_harvested_ha


################ save table for a short cut #######################################################
write.csv(crops_16Lx_clean,file="crops_16Lx_clean.csv")
crops_16Lx_clean<-read.csv("crops_16Lx_clean.csv",sep=",",header=T)[,-1]
#############################################################################################################

# subset of data with renamed variables
raw_data<-crops_16Lx_clean[,c("Country","Year","N_div_AreaH_kj_ha","Nt_area","production_div_AreaH_kj_ha","suitability","manure_area","netcap_area", "netcap14_area" ,"market")]

names(raw_data)<-c("country","year","N_area","Nt_area","yield","cropsuit","manure_area","netcap_area","netcap14_area","market")

################ save table for a short cut #######################################################
write.csv(raw_data,file="raw_data.csv")
raw_data<-read.csv("raw_data.csv",sep=",",header=T)[,-1]
#############################################################################################################

### Aggregate EU27 data and merge with subset of data

crops_16L_clean<-read.csv("crops_16Lx_clean.csv",sep=",",header=T)[,-1]

## Create EU27 data and keep it separate
# no net capital stock data for Croatia, hence EU27 without Croatia

EU27 <- c("Austria","Belgium","Bulgaria","Cyprus","Denmark","Estonia","Finland","France","Germany","Greece",
          "Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Romania",
          "Portugal","Slovakia","Slovenia","Spain","Sweden","United_Kingdom","Hungary","Czechia") 

crops_16L_clean$EU27 <- NA 
for(i in 1:length(crops_16L_clean[,1])){
  if(is.element(crops_16L_clean$Country[i],EU27)){
    crops_16L_clean$EU27[i]<-"EU27"}}


# load data for 2000
crops_16L_00<-crops_16L_clean[which(crops_16L_clean$Year=="2000"),]
# load data for 2006
crops_16L_06<-crops_16L_clean[which(crops_16L_clean$Year=="2006"),]
# load data for 2010
crops_16L_10<-crops_16L_clean[which(crops_16L_clean$Year=="2010"),]
# load data for 2014
crops_16L_14<-crops_16L_clean[which(crops_16L_clean$Year=="2014"),]

crops_16L_00_EU27<-crops_16L_00[which(crops_16L_00$EU27=="EU27"),]
crops_16L_06_EU27<-crops_16L_06[which(crops_16L_06$EU27=="EU27"),]
crops_16L_10_EU27<-crops_16L_10[which(crops_16L_10$EU27=="EU27"),]
crops_16L_14_EU27<-crops_16L_14[which(crops_16L_14$EU27=="EU27"),]

#sum production EU27
prod_sum_00_EU <- sum(crops_16L_00_EU27$production_kj)
prod_sum_06_EU <- sum(crops_16L_06_EU27$production_kj)
prod_sum_10_EU <- sum(crops_16L_10_EU27$production_kj)
prod_sum_14_EU <- sum(crops_16L_14_EU27$production_kj)

# one dataframe for EU27 per year

data_EU27_00 <- data.frame("country"="EU27","year"=2000)
data_EU27_00$N_area<-NA
data_EU27_00$Nt_area<-NA
data_EU27_00$yield<-NA
data_EU27_00$cropsuit<-NA
data_EU27_00$manure_area<-NA
data_EU27_00$netcap_area<-NA
data_EU27_00$netcap14_area<-NA
data_EU27_00$market<-NA
data_EU27_00$N_area <- sum(crops_16L_00_EU27$N_total_kj)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$Nt_area <- sum(crops_16L_00_EU27$N_total_t)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$yield <- sum(crops_16L_00_EU27$production_kj)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$cropsuit <- 47.0027606
data_EU27_00$manure_area <- sum(crops_16L_00_EU27$Manure_kg)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$netcap_area <- sum(crops_16L_00_EU27$net_capital_stocks)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$netcap14_area <- sum(crops_16L_00_EU27$net_capital_stocks14)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$market <- 14327.100380

data_EU27_06 <- data.frame("country"="EU27","year"=2006)
data_EU27_06$N_area<-NA
data_EU27_06$Nt_area<-NA
data_EU27_06$yield<-NA
data_EU27_06$cropsuit<-NA
data_EU27_06$manure_area<-NA
data_EU27_06$netcap_area<-NA
data_EU27_06$netcap14_area<-NA
data_EU27_06$market<-NA
data_EU27_06$N_area <- sum(crops_16L_06_EU27$N_total_kj)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$Nt_area <- sum(crops_16L_06_EU27$N_total_t)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$yield <- sum(crops_16L_06_EU27$production_kj)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$cropsuit <- 47.0027606
data_EU27_06$manure_area <- sum(crops_16L_06_EU27$Manure_kg)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$netcap_area <- sum(crops_16L_06_EU27$net_capital_stocks)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$netcap14_area <- sum(crops_16L_06_EU27$net_capital_stocks14)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$market <- 14327.100380

data_EU27_10 <- data.frame("country"="EU27","year"=2010)
data_EU27_10$N_area<-NA
data_EU27_10$Nt_area<-NA
data_EU27_10$yield<-NA
data_EU27_10$cropsuit<-NA
data_EU27_10$manure_area<-NA
data_EU27_10$netcap_area<-NA
data_EU27_10$netcap14_area<-NA
data_EU27_10$market<-NA
data_EU27_10$N_area <- sum(crops_16L_10_EU27$N_total_kj)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$Nt_area <- sum(crops_16L_10_EU27$N_total_t)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$yield <- sum(crops_16L_10_EU27$production_kj)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$cropsuit <- 47.0027606
data_EU27_10$manure_area <- sum(crops_16L_10_EU27$Manure_kg)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$netcap_area <- sum(crops_16L_10_EU27$net_capital_stocks)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$netcap14_area <- sum(crops_16L_10_EU27$net_capital_stocks14)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$market <- 14327.100380

data_EU27_14 <- data.frame("country"="EU27","year"=2014)
data_EU27_14$N_area<-NA
data_EU27_14$Nt_area<-NA
data_EU27_14$yield<-NA
data_EU27_14$cropsuit<-NA
data_EU27_14$manure_area<-NA
data_EU27_14$netcap_area<-NA
data_EU27_14$netcap14_area<-NA
data_EU27_14$market<-NA
data_EU27_14$N_area <- sum(crops_16L_14_EU27$N_total_kj)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$Nt_area <- sum(crops_16L_14_EU27$N_total_t)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$yield <- sum(crops_16L_14_EU27$production_kj)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$cropsuit <- 47.0027606
data_EU27_14$manure_area <- sum(crops_16L_14_EU27$Manure_kg)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$netcap_area <- sum(crops_16L_14_EU27$net_capital_stocks)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$netcap14_area <- sum(crops_16L_14_EU27$net_capital_stocks14)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$market <- 14327.100380

write.csv(data_EU27_00, file="data_EU27_00.csv")
write.csv(data_EU27_06, file="data_EU27_06.csv")
write.csv(data_EU27_10, file="data_EU27_10.csv")
write.csv(data_EU27_14, file="data_EU27_14.csv")

data<-read.csv("raw_data.csv",sep=",",header=T)[,-1]

data <- na.exclude(data)
data <- data[which(data$market!=0),]
data <- data[which(data$cropsuit!=0),]
data_00<-data[which(data$year=="2000"),]
data_06<-data[which(data$year=="2006"),]
data_10<-data[which(data$year=="2010"),]
data_14<-data[which(data$year=="2014"),]

# one dataframe for each year
data_EU_00 <- rbind(data_00,data_EU27_00)
data_EU_06 <- rbind(data_06,data_EU27_06)
data_EU_10 <- rbind(data_10,data_EU27_10)
data_EU_14 <- rbind(data_14,data_EU27_14)

#extract IFA countries, Belarus for 2000 removed because of incoherent value
IFA_14 <- c("EU27","Argentina","Australia","Bangladesh", "Belarus","Brazil","Canada",
            "China,_mainland","Chile","Egypt","India","Indonesia","Iran_(Islamic_Republic_of)","Japan",
            "Malaysia","Mexico","Morocco","New_Zealand","Pakistan","Philippines","Russian_Federation",
            "South_Africa","Thailand","Turkey","Ukraine","United_States_of_America","Uzbekistan","Viet_Nam")

IFA_10 <- c("EU27","Argentina","Australia","Bangladesh", "Belarus","Brazil","Canada",
            "China,_mainland","Chile","Egypt","India","Indonesia","Iran_(Islamic_Republic_of)","Japan",
            "Malaysia","Mexico","Morocco","Pakistan","Philippines","Russian_Federation",
            "South_Africa","Thailand","Turkey","Ukraine","United_States_of_America","Uzbekistan","Viet_Nam")

IFA_06 <- c("EU27","Argentina","Australia","Bangladesh","Brazil","Canada",
            "China,_mainland","Chile","Egypt","India","Indonesia","Iran_(Islamic_Republic_of)","Japan",
            "Malaysia","Mexico","Morocco","Pakistan","Philippines","Russian_Federation",
            "South_Africa","Thailand","Turkey","Ukraine","United_States_of_America","Uzbekistan","Viet_Nam")

IFA_00 <- c("EU27","Albania", "Australia", "Azerbaijan", "Bangladesh", "Canada", "China,_Taiwan_Province_of",
            "Croatia", "El_Salvador", "Ethiopia", "Fiji", "Israel", "Japan", "Republic_of_Korea", "Kuwait", "Republic_of_Moldova", 
            "Morocco","New_Zealand", "Nigeria", "Norway", "Pakistan", "Saudi_Arabia", "South_Africa", "Sri_Lanka",
            "Switzerland", "Syrian_Arab_Republic", "Zambia", 
            "Algeria", "Argentina", "Bolivia_(Plurinational_State_of)", "Brazil", "Cambodia", "Chile", "China,_mainland",
            "Colombia", "Costa_Rica", "Dominican_Republic", "Ecuador", "Egypt", "Guatemala", "Guinea", "Honduras",
            "India", "Indonesia", "Jordan", "Kenya", "Lao_People's_Democratic_Republic", "Lebanon", "Madagascar",
            "Malawi", "Malaysia", "Mauritania", "Mexico", "Myanmar", "Nicaragua", "Paraguay", "Philippines", "United_Republic_of_Tanzania",
            "Thailand", "Togo", "Turkey","United_States_of_America", "Uruguay", "Venezuela_(Bolivarian_Republic_of)",
            "Viet_Nam","Zimbabwe")

data_EU_00$IFA_Region <- NA
data_EU_06$IFA_Region <- NA
data_EU_10$IFA_Region <- NA
data_EU_14$IFA_Region <- NA

data_EU_00$country <- as.character(data_EU_00$country)
data_EU_06$country <- as.character(data_EU_06$country)
data_EU_10$country <- as.character(data_EU_10$country)
data_EU_14$country <- as.character(data_EU_14$country)

for(i in 1:length(data_EU_00[,1])){
  if(is.element(data_EU_00$country[i],IFA_00)){
    data_EU_00$IFA_Region[i]<-"IFA"}}

for(i in 1:length(data_EU_06[,1])){
  if(is.element(data_EU_06$country[i],IFA_06)){
    data_EU_06$IFA_Region[i]<-"IFA"}}

for(i in 1:length(data_EU_10[,1])){
  if(is.element(data_EU_10$country[i],IFA_10)){
    data_EU_10$IFA_Region[i]<-"IFA"}}

for(i in 1:length(data_EU_14[,1])){
  if(is.element(data_EU_14$country[i],IFA_14)){
    data_EU_14$IFA_Region[i]<-"IFA"}}

data_EU_IFA <- rbind(data_EU_00,data_EU_06,data_EU_10,data_EU_14)

data_EU_IFA_sel <- data_EU_IFA[which(data_EU_IFA$IFA_Region=="IFA"),]

data_EU_IFA_sel$country <- as.character(data_EU_IFA_sel$country)

write.csv(data_EU_IFA_sel,file="data_EU_IFA_sel.csv")

data <- read.csv("data_EU_IFA_sel.csv")[,-1]


data_mean <- data.frame("country"=unique(data$country))

data_mean$cropsuit <- NA
code_suit<-unique(data_mean$country)


for(i in 1:length(data_mean[,1])){
  if((is.element(data_mean$country[i],code_suit))){
    data_mean$cropsuit[i]<-data$cropsuit[which(data$country==data_mean$country[i])]} 
  else {data_mean$cropsuit[i]<-NA}}

data_mean$market <- NA
for(i in 1:length(data_mean[,1])){
  if((is.element(data_mean$country[i],code_suit))){
    data_mean$market[i]<-data$market[which(data$country==data_mean$country[i])]} 
  else {data_mean$cropsuit[i]<-NA}}


data_mean$Nt_area <- NA
data_mean$Nt_area[which(data_mean$country=="Albania")] <- mean(data$Nt_area[which(data$country=="Albania")])
data_mean$Nt_area[which(data_mean$country=="Algeria")] <- mean(data$Nt_area[which(data$country=="Algeria")])
data_mean$Nt_area[which(data_mean$country=="Argentina")] <- mean(data$Nt_area[which(data$country=="Argentina")])
data_mean$Nt_area[which(data_mean$country=="Australia")] <- mean(data$Nt_area[which(data$country=="Australia")])
data_mean$Nt_area[which(data_mean$country=="Azerbaijan")] <- mean(data$Nt_area[which(data$country=="Azerbaijan")])
data_mean$Nt_area[which(data_mean$country=="Bangladesh")] <- mean(data$Nt_area[which(data$country=="Bangladesh")])
data_mean$Nt_area[which(data_mean$country=="Bolivia_(Plurinational_State_of)")] <- mean(data$Nt_area[which(data$country=="Bolivia_(Plurinational_State_of)")])
data_mean$Nt_area[which(data_mean$country=="Brazil")] <- mean(data$Nt_area[which(data$country=="Brazil")])
data_mean$Nt_area[which(data_mean$country=="Cambodia")] <- mean(data$Nt_area[which(data$country=="Cambodia")])
data_mean$Nt_area[which(data_mean$country=="Canada")] <- mean(data$Nt_area[which(data$country=="Canada")])
data_mean$Nt_area[which(data_mean$country=="Chile")] <- mean(data$Nt_area[which(data$country=="Chile")])
data_mean$Nt_area[which(data_mean$country=="China,_mainland")] <- mean(data$Nt_area[which(data$country=="China,_mainland")])
data_mean$Nt_area[which(data_mean$country=="Colombia")] <- mean(data$Nt_area[which(data$country=="Colombia")])
data_mean$Nt_area[which(data_mean$country=="Costa_Rica")] <- mean(data$Nt_area[which(data$country=="Costa_Rica")])
data_mean$Nt_area[which(data_mean$country=="Dominican_Republic")] <- mean(data$Nt_area[which(data$country=="Dominican_Republic")])
data_mean$Nt_area[which(data_mean$country=="Ecuador")] <- mean(data$Nt_area[which(data$country=="Ecuador")])
data_mean$Nt_area[which(data_mean$country=="Egypt")] <- mean(data$Nt_area[which(data$country=="Egypt")])
data_mean$Nt_area[which(data_mean$country=="El_Salvador")] <- mean(data$Nt_area[which(data$country=="El_Salvador")])
data_mean$Nt_area[which(data_mean$country=="Ethiopia")] <- mean(data$Nt_area[which(data$country=="Ethiopia")])
data_mean$Nt_area[which(data_mean$country=="Guatemala")] <- mean(data$Nt_area[which(data$country=="Guatemala")])
data_mean$Nt_area[which(data_mean$country=="Guinea")] <- mean(data$Nt_area[which(data$country=="Guinea")])
data_mean$Nt_area[which(data_mean$country=="Honduras")] <- mean(data$Nt_area[which(data$country=="Honduras")])
data_mean$Nt_area[which(data_mean$country=="India")] <- mean(data$Nt_area[which(data$country=="India")])
data_mean$Nt_area[which(data_mean$country=="Indonesia")] <- mean(data$Nt_area[which(data$country=="Indonesia")])
data_mean$Nt_area[which(data_mean$country=="Israel")] <- mean(data$Nt_area[which(data$country=="Israel")])
data_mean$Nt_area[which(data_mean$country=="Japan")] <- mean(data$Nt_area[which(data$country=="Japan")])
data_mean$Nt_area[which(data_mean$country=="Jordan")] <- mean(data$Nt_area[which(data$country=="Jordan")])
data_mean$Nt_area[which(data_mean$country=="Kenya")] <- mean(data$Nt_area[which(data$country=="Kenya")])
data_mean$Nt_area[which(data_mean$country=="Kuwait")] <- mean(data$Nt_area[which(data$country=="Kuwait")])
data_mean$Nt_area[which(data_mean$country=="Lao_People's_Democratic_Republic")] <- mean(data$Nt_area[which(data$country=="Lao_People's_Democratic_Republic")])
data_mean$Nt_area[which(data_mean$country=="Lebanon")] <- mean(data$Nt_area[which(data$country=="Lebanon")])
data_mean$Nt_area[which(data_mean$country=="Madagascar")] <- mean(data$Nt_area[which(data$country=="Madagascar")])
data_mean$Nt_area[which(data_mean$country=="Malawi")] <- mean(data$Nt_area[which(data$country=="Malawi")])
data_mean$Nt_area[which(data_mean$country=="Malaysia")] <- mean(data$Nt_area[which(data$country=="Malaysia")])
data_mean$Nt_area[which(data_mean$country=="Mauritania")] <- mean(data$Nt_area[which(data$country=="Mauritania")])
data_mean$Nt_area[which(data_mean$country=="Mexico")] <- mean(data$Nt_area[which(data$country=="Mexico")])
data_mean$Nt_area[which(data_mean$country=="Morocco")] <- mean(data$Nt_area[which(data$country=="Morocco")])
data_mean$Nt_area[which(data_mean$country=="Myanmar")] <- mean(data$Nt_area[which(data$country=="Myanmar")])
data_mean$Nt_area[which(data_mean$country=="New_Zealand")] <- mean(data$Nt_area[which(data$country=="New_Zealand")])
data_mean$Nt_area[which(data_mean$country=="Nicaragua")] <- mean(data$Nt_area[which(data$country=="Nicaragua")])
data_mean$Nt_area[which(data_mean$country=="Nigeria")] <- mean(data$Nt_area[which(data$country=="Nigeria")])
data_mean$Nt_area[which(data_mean$country=="Norway")] <- mean(data$Nt_area[which(data$country=="Norway")])
data_mean$Nt_area[which(data_mean$country=="Pakistan")] <- mean(data$Nt_area[which(data$country=="Pakistan")])
data_mean$Nt_area[which(data_mean$country=="Paraguay")] <- mean(data$Nt_area[which(data$country=="Paraguay")])
data_mean$Nt_area[which(data_mean$country=="Philippines")] <- mean(data$Nt_area[which(data$country=="Philippines")])
data_mean$Nt_area[which(data_mean$country=="Republic_of_Korea")] <- mean(data$Nt_area[which(data$country=="Republic_of_Korea")])
data_mean$Nt_area[which(data_mean$country=="Republic_of_Moldova")] <- mean(data$Nt_area[which(data$country=="Republic_of_Moldova")])
data_mean$Nt_area[which(data_mean$country=="Saudi_Arabia")] <- mean(data$Nt_area[which(data$country=="Saudi_Arabia")])
data_mean$Nt_area[which(data_mean$country=="South_Africa")] <- mean(data$Nt_area[which(data$country=="South_Africa")])
data_mean$Nt_area[which(data_mean$country=="Sri_Lanka")] <- mean(data$Nt_area[which(data$country=="Sri_Lanka")])
data_mean$Nt_area[which(data_mean$country=="Switzerland")] <- mean(data$Nt_area[which(data$country=="Switzerland")])
data_mean$Nt_area[which(data_mean$country=="Syrian_Arab_Republic")] <- mean(data$Nt_area[which(data$country=="Syrian_Arab_Republic")])
data_mean$Nt_area[which(data_mean$country=="Thailand")] <- mean(data$Nt_area[which(data$country=="Thailand")])
data_mean$Nt_area[which(data_mean$country=="Togo")] <- mean(data$Nt_area[which(data$country=="Togo")])
data_mean$Nt_area[which(data_mean$country=="Turkey")] <- mean(data$Nt_area[which(data$country=="Turkey")])
data_mean$Nt_area[which(data_mean$country=="United_Republic_of_Tanzania")] <- mean(data$Nt_area[which(data$country=="United_Republic_of_Tanzania")])
data_mean$Nt_area[which(data_mean$country=="United_States_of_America")] <- mean(data$Nt_area[which(data$country=="United_States_of_America")])
data_mean$Nt_area[which(data_mean$country=="Uruguay")] <- mean(data$Nt_area[which(data$country=="Uruguay")])
data_mean$Nt_area[which(data_mean$country=="Venezuela_(Bolivarian_Republic_of)")] <- mean(data$Nt_area[which(data$country=="Venezuela_(Bolivarian_Republic_of)")])
data_mean$Nt_area[which(data_mean$country=="Viet_Nam")] <- mean(data$Nt_area[which(data$country=="Viet_Nam")])
data_mean$Nt_area[which(data_mean$country=="Zambia")] <- mean(data$Nt_area[which(data$country=="Zambia")])
data_mean$Nt_area[which(data_mean$country=="Zimbabwe")] <- mean(data$Nt_area[which(data$country=="Zimbabwe")])
data_mean$Nt_area[which(data_mean$country=="EU27")] <- mean(data$Nt_area[which(data$country=="EU27")])
data_mean$Nt_area[which(data_mean$country=="Iran_(Islamic_Republic_of)")] <- mean(data$Nt_area[which(data$country=="Iran_(Islamic_Republic_of)")])
data_mean$Nt_area[which(data_mean$country=="Russian_Federation")] <- mean(data$Nt_area[which(data$country=="Russian_Federation")])
data_mean$Nt_area[which(data_mean$country=="Ukraine")] <- mean(data$Nt_area[which(data$country=="Ukraine")])
data_mean$Nt_area[which(data_mean$country=="Uzbekistan")] <- mean(data$Nt_area[which(data$country=="Uzbekistan")])
data_mean$Nt_area[which(data_mean$country=="Belarus")] <- mean(data$Nt_area[which(data$country=="Belarus")])


data_mean$yield <- NA
data_mean$yield[which(data_mean$country=="Albania")] <- mean(data$yield[which(data$country=="Albania")])
data_mean$yield[which(data_mean$country=="Algeria")] <- mean(data$yield[which(data$country=="Algeria")])
data_mean$yield[which(data_mean$country=="Argentina")] <- mean(data$yield[which(data$country=="Argentina")])
data_mean$yield[which(data_mean$country=="Australia")] <- mean(data$yield[which(data$country=="Australia")])
data_mean$yield[which(data_mean$country=="Azerbaijan")] <- mean(data$yield[which(data$country=="Azerbaijan")])
data_mean$yield[which(data_mean$country=="Bangladesh")] <- mean(data$yield[which(data$country=="Bangladesh")])
data_mean$yield[which(data_mean$country=="Bolivia_(Plurinational_State_of)")] <- mean(data$yield[which(data$country=="Bolivia_(Plurinational_State_of)")])
data_mean$yield[which(data_mean$country=="Brazil")] <- mean(data$yield[which(data$country=="Brazil")])
data_mean$yield[which(data_mean$country=="Cambodia")] <- mean(data$yield[which(data$country=="Cambodia")])
data_mean$yield[which(data_mean$country=="Canada")] <- mean(data$yield[which(data$country=="Canada")])
data_mean$yield[which(data_mean$country=="Chile")] <- mean(data$yield[which(data$country=="Chile")])
data_mean$yield[which(data_mean$country=="China,_mainland")] <- mean(data$yield[which(data$country=="China,_mainland")])
data_mean$yield[which(data_mean$country=="Colombia")] <- mean(data$yield[which(data$country=="Colombia")])
data_mean$yield[which(data_mean$country=="Costa_Rica")] <- mean(data$yield[which(data$country=="Costa_Rica")])
data_mean$yield[which(data_mean$country=="Dominican_Republic")] <- mean(data$yield[which(data$country=="Dominican_Republic")])
data_mean$yield[which(data_mean$country=="Ecuador")] <- mean(data$yield[which(data$country=="Ecuador")])
data_mean$yield[which(data_mean$country=="Egypt")] <- mean(data$yield[which(data$country=="Egypt")])
data_mean$yield[which(data_mean$country=="El_Salvador")] <- mean(data$yield[which(data$country=="El_Salvador")])
data_mean$yield[which(data_mean$country=="Ethiopia")] <- mean(data$yield[which(data$country=="Ethiopia")])
data_mean$yield[which(data_mean$country=="Guatemala")] <- mean(data$yield[which(data$country=="Guatemala")])
data_mean$yield[which(data_mean$country=="Guinea")] <- mean(data$yield[which(data$country=="Guinea")])
data_mean$yield[which(data_mean$country=="Honduras")] <- mean(data$yield[which(data$country=="Honduras")])
data_mean$yield[which(data_mean$country=="India")] <- mean(data$yield[which(data$country=="India")])
data_mean$yield[which(data_mean$country=="Indonesia")] <- mean(data$yield[which(data$country=="Indonesia")])
data_mean$yield[which(data_mean$country=="Israel")] <- mean(data$yield[which(data$country=="Israel")])
data_mean$yield[which(data_mean$country=="Japan")] <- mean(data$yield[which(data$country=="Japan")])
data_mean$yield[which(data_mean$country=="Jordan")] <- mean(data$yield[which(data$country=="Jordan")])
data_mean$yield[which(data_mean$country=="Kenya")] <- mean(data$yield[which(data$country=="Kenya")])
data_mean$yield[which(data_mean$country=="Kuwait")] <- mean(data$yield[which(data$country=="Kuwait")])
data_mean$yield[which(data_mean$country=="Lao_People's_Democratic_Republic")] <- mean(data$yield[which(data$country=="Lao_People's_Democratic_Republic")])
data_mean$yield[which(data_mean$country=="Lebanon")] <- mean(data$yield[which(data$country=="Lebanon")])
data_mean$yield[which(data_mean$country=="Madagascar")] <- mean(data$yield[which(data$country=="Madagascar")])
data_mean$yield[which(data_mean$country=="Malawi")] <- mean(data$yield[which(data$country=="Malawi")])
data_mean$yield[which(data_mean$country=="Malaysia")] <- mean(data$yield[which(data$country=="Malaysia")])
data_mean$yield[which(data_mean$country=="Mauritania")] <- mean(data$yield[which(data$country=="Mauritania")])
data_mean$yield[which(data_mean$country=="Mexico")] <- mean(data$yield[which(data$country=="Mexico")])
data_mean$yield[which(data_mean$country=="Morocco")] <- mean(data$yield[which(data$country=="Morocco")])
data_mean$yield[which(data_mean$country=="Myanmar")] <- mean(data$yield[which(data$country=="Myanmar")])
data_mean$yield[which(data_mean$country=="New_Zealand")] <- mean(data$yield[which(data$country=="New_Zealand")])
data_mean$yield[which(data_mean$country=="Nicaragua")] <- mean(data$yield[which(data$country=="Nicaragua")])
data_mean$yield[which(data_mean$country=="Nigeria")] <- mean(data$yield[which(data$country=="Nigeria")])
data_mean$yield[which(data_mean$country=="Norway")] <- mean(data$yield[which(data$country=="Norway")])
data_mean$yield[which(data_mean$country=="Pakistan")] <- mean(data$yield[which(data$country=="Pakistan")])
data_mean$yield[which(data_mean$country=="Paraguay")] <- mean(data$yield[which(data$country=="Paraguay")])
data_mean$yield[which(data_mean$country=="Philippines")] <- mean(data$yield[which(data$country=="Philippines")])
data_mean$yield[which(data_mean$country=="Republic_of_Korea")] <- mean(data$yield[which(data$country=="Republic_of_Korea")])
data_mean$yield[which(data_mean$country=="Republic_of_Moldova")] <- mean(data$yield[which(data$country=="Republic_of_Moldova")])
data_mean$yield[which(data_mean$country=="Saudi_Arabia")] <- mean(data$yield[which(data$country=="Saudi_Arabia")])
data_mean$yield[which(data_mean$country=="South_Africa")] <- mean(data$yield[which(data$country=="South_Africa")])
data_mean$yield[which(data_mean$country=="Sri_Lanka")] <- mean(data$yield[which(data$country=="Sri_Lanka")])
data_mean$yield[which(data_mean$country=="Switzerland")] <- mean(data$yield[which(data$country=="Switzerland")])
data_mean$yield[which(data_mean$country=="Syrian_Arab_Republic")] <- mean(data$yield[which(data$country=="Syrian_Arab_Republic")])
data_mean$yield[which(data_mean$country=="Thailand")] <- mean(data$yield[which(data$country=="Thailand")])
data_mean$yield[which(data_mean$country=="Togo")] <- mean(data$yield[which(data$country=="Togo")])
data_mean$yield[which(data_mean$country=="Turkey")] <- mean(data$yield[which(data$country=="Turkey")])
data_mean$yield[which(data_mean$country=="United_Republic_of_Tanzania")] <- mean(data$yield[which(data$country=="United_Republic_of_Tanzania")])
data_mean$yield[which(data_mean$country=="United_States_of_America")] <- mean(data$yield[which(data$country=="United_States_of_America")])
data_mean$yield[which(data_mean$country=="Uruguay")] <- mean(data$yield[which(data$country=="Uruguay")])
data_mean$yield[which(data_mean$country=="Venezuela_(Bolivarian_Republic_of)")] <- mean(data$yield[which(data$country=="Venezuela_(Bolivarian_Republic_of)")])
data_mean$yield[which(data_mean$country=="Viet_Nam")] <- mean(data$yield[which(data$country=="Viet_Nam")])
data_mean$yield[which(data_mean$country=="Zambia")] <- mean(data$yield[which(data$country=="Zambia")])
data_mean$yield[which(data_mean$country=="Zimbabwe")] <- mean(data$yield[which(data$country=="Zimbabwe")])
data_mean$yield[which(data_mean$country=="EU27")] <- mean(data$yield[which(data$country=="EU27")])
data_mean$yield[which(data_mean$country=="Iran_(Islamic_Republic_of)")] <- mean(data$yield[which(data$country=="Iran_(Islamic_Republic_of)")])
data_mean$yield[which(data_mean$country=="Russian_Federation")] <- mean(data$yield[which(data$country=="Russian_Federation")])
data_mean$yield[which(data_mean$country=="Ukraine")] <- mean(data$yield[which(data$country=="Ukraine")])
data_mean$yield[which(data_mean$country=="Uzbekistan")] <- mean(data$yield[which(data$country=="Uzbekistan")])
data_mean$yield[which(data_mean$country=="Belarus")] <- mean(data$yield[which(data$country=="Belarus")])

data_mean$manure_area <- NA
data_mean$manure_area[which(data_mean$country=="Albania")] <- mean(data$manure_area[which(data$country=="Albania")])
data_mean$manure_area[which(data_mean$country=="Algeria")] <- mean(data$manure_area[which(data$country=="Algeria")])
data_mean$manure_area[which(data_mean$country=="Argentina")] <- mean(data$manure_area[which(data$country=="Argentina")])
data_mean$manure_area[which(data_mean$country=="Australia")] <- mean(data$manure_area[which(data$country=="Australia")])
data_mean$manure_area[which(data_mean$country=="Azerbaijan")] <- mean(data$manure_area[which(data$country=="Azerbaijan")])
data_mean$manure_area[which(data_mean$country=="Bangladesh")] <- mean(data$manure_area[which(data$country=="Bangladesh")])
data_mean$manure_area[which(data_mean$country=="Bolivia_(Plurinational_State_of)")] <- mean(data$manure_area[which(data$country=="Bolivia_(Plurinational_State_of)")])
data_mean$manure_area[which(data_mean$country=="Brazil")] <- mean(data$manure_area[which(data$country=="Brazil")])
data_mean$manure_area[which(data_mean$country=="Cambodia")] <- mean(data$manure_area[which(data$country=="Cambodia")])
data_mean$manure_area[which(data_mean$country=="Canada")] <- mean(data$manure_area[which(data$country=="Canada")])
data_mean$manure_area[which(data_mean$country=="Chile")] <- mean(data$manure_area[which(data$country=="Chile")])
data_mean$manure_area[which(data_mean$country=="China,_mainland")] <- mean(data$manure_area[which(data$country=="China,_mainland")])
data_mean$manure_area[which(data_mean$country=="Colombia")] <- mean(data$manure_area[which(data$country=="Colombia")])
data_mean$manure_area[which(data_mean$country=="Costa_Rica")] <- mean(data$manure_area[which(data$country=="Costa_Rica")])
data_mean$manure_area[which(data_mean$country=="Dominican_Republic")] <- mean(data$manure_area[which(data$country=="Dominican_Republic")])
data_mean$manure_area[which(data_mean$country=="Ecuador")] <- mean(data$manure_area[which(data$country=="Ecuador")])
data_mean$manure_area[which(data_mean$country=="Egypt")] <- mean(data$manure_area[which(data$country=="Egypt")])
data_mean$manure_area[which(data_mean$country=="El_Salvador")] <- mean(data$manure_area[which(data$country=="El_Salvador")])
data_mean$manure_area[which(data_mean$country=="Ethiopia")] <- mean(data$manure_area[which(data$country=="Ethiopia")])
data_mean$manure_area[which(data_mean$country=="Guatemala")] <- mean(data$manure_area[which(data$country=="Guatemala")])
data_mean$manure_area[which(data_mean$country=="Guinea")] <- mean(data$manure_area[which(data$country=="Guinea")])
data_mean$manure_area[which(data_mean$country=="Honduras")] <- mean(data$manure_area[which(data$country=="Honduras")])
data_mean$manure_area[which(data_mean$country=="India")] <- mean(data$manure_area[which(data$country=="India")])
data_mean$manure_area[which(data_mean$country=="Indonesia")] <- mean(data$manure_area[which(data$country=="Indonesia")])
data_mean$manure_area[which(data_mean$country=="Israel")] <- mean(data$manure_area[which(data$country=="Israel")])
data_mean$manure_area[which(data_mean$country=="Japan")] <- mean(data$manure_area[which(data$country=="Japan")])
data_mean$manure_area[which(data_mean$country=="Jordan")] <- mean(data$manure_area[which(data$country=="Jordan")])
data_mean$manure_area[which(data_mean$country=="Kenya")] <- mean(data$manure_area[which(data$country=="Kenya")])
data_mean$manure_area[which(data_mean$country=="Kuwait")] <- mean(data$manure_area[which(data$country=="Kuwait")])
data_mean$manure_area[which(data_mean$country=="Lao_People's_Democratic_Republic")] <- mean(data$manure_area[which(data$country=="Lao_People's_Democratic_Republic")])
data_mean$manure_area[which(data_mean$country=="Lebanon")] <- mean(data$manure_area[which(data$country=="Lebanon")])
data_mean$manure_area[which(data_mean$country=="Madagascar")] <- mean(data$manure_area[which(data$country=="Madagascar")])
data_mean$manure_area[which(data_mean$country=="Malawi")] <- mean(data$manure_area[which(data$country=="Malawi")])
data_mean$manure_area[which(data_mean$country=="Malaysia")] <- mean(data$manure_area[which(data$country=="Malaysia")])
data_mean$manure_area[which(data_mean$country=="Mauritania")] <- mean(data$manure_area[which(data$country=="Mauritania")])
data_mean$manure_area[which(data_mean$country=="Mexico")] <- mean(data$manure_area[which(data$country=="Mexico")])
data_mean$manure_area[which(data_mean$country=="Morocco")] <- mean(data$manure_area[which(data$country=="Morocco")])
data_mean$manure_area[which(data_mean$country=="Myanmar")] <- mean(data$manure_area[which(data$country=="Myanmar")])
data_mean$manure_area[which(data_mean$country=="New_Zealand")] <- mean(data$manure_area[which(data$country=="New_Zealand")])
data_mean$manure_area[which(data_mean$country=="Nicaragua")] <- mean(data$manure_area[which(data$country=="Nicaragua")])
data_mean$manure_area[which(data_mean$country=="Nigeria")] <- mean(data$manure_area[which(data$country=="Nigeria")])
data_mean$manure_area[which(data_mean$country=="Norway")] <- mean(data$manure_area[which(data$country=="Norway")])
data_mean$manure_area[which(data_mean$country=="Pakistan")] <- mean(data$manure_area[which(data$country=="Pakistan")])
data_mean$manure_area[which(data_mean$country=="Paraguay")] <- mean(data$manure_area[which(data$country=="Paraguay")])
data_mean$manure_area[which(data_mean$country=="Philippines")] <- mean(data$manure_area[which(data$country=="Philippines")])
data_mean$manure_area[which(data_mean$country=="Republic_of_Korea")] <- mean(data$manure_area[which(data$country=="Republic_of_Korea")])
data_mean$manure_area[which(data_mean$country=="Republic_of_Moldova")] <- mean(data$manure_area[which(data$country=="Republic_of_Moldova")])
data_mean$manure_area[which(data_mean$country=="Saudi_Arabia")] <- mean(data$manure_area[which(data$country=="Saudi_Arabia")])
data_mean$manure_area[which(data_mean$country=="South_Africa")] <- mean(data$manure_area[which(data$country=="South_Africa")])
data_mean$manure_area[which(data_mean$country=="Sri_Lanka")] <- mean(data$manure_area[which(data$country=="Sri_Lanka")])
data_mean$manure_area[which(data_mean$country=="Switzerland")] <- mean(data$manure_area[which(data$country=="Switzerland")])
data_mean$manure_area[which(data_mean$country=="Syrian_Arab_Republic")] <- mean(data$manure_area[which(data$country=="Syrian_Arab_Republic")])
data_mean$manure_area[which(data_mean$country=="Thailand")] <- mean(data$manure_area[which(data$country=="Thailand")])
data_mean$manure_area[which(data_mean$country=="Togo")] <- mean(data$manure_area[which(data$country=="Togo")])
data_mean$manure_area[which(data_mean$country=="Turkey")] <- mean(data$manure_area[which(data$country=="Turkey")])
data_mean$manure_area[which(data_mean$country=="United_Republic_of_Tanzania")] <- mean(data$manure_area[which(data$country=="United_Republic_of_Tanzania")])
data_mean$manure_area[which(data_mean$country=="United_States_of_America")] <- mean(data$manure_area[which(data$country=="United_States_of_America")])
data_mean$manure_area[which(data_mean$country=="Uruguay")] <- mean(data$manure_area[which(data$country=="Uruguay")])
data_mean$manure_area[which(data_mean$country=="Venezuela_(Bolivarian_Republic_of)")] <- mean(data$manure_area[which(data$country=="Venezuela_(Bolivarian_Republic_of)")])
data_mean$manure_area[which(data_mean$country=="Viet_Nam")] <- mean(data$manure_area[which(data$country=="Viet_Nam")])
data_mean$manure_area[which(data_mean$country=="Zambia")] <- mean(data$manure_area[which(data$country=="Zambia")])
data_mean$manure_area[which(data_mean$country=="Zimbabwe")] <- mean(data$manure_area[which(data$country=="Zimbabwe")])
data_mean$manure_area[which(data_mean$country=="EU27")] <- mean(data$manure_area[which(data$country=="EU27")])
data_mean$manure_area[which(data_mean$country=="Iran_(Islamic_Republic_of)")] <- mean(data$manure_area[which(data$country=="Iran_(Islamic_Republic_of)")])
data_mean$manure_area[which(data_mean$country=="Russian_Federation")] <- mean(data$manure_area[which(data$country=="Russian_Federation")])
data_mean$manure_area[which(data_mean$country=="Ukraine")] <- mean(data$manure_area[which(data$country=="Ukraine")])
data_mean$manure_area[which(data_mean$country=="Uzbekistan")] <- mean(data$manure_area[which(data$country=="Uzbekistan")])
data_mean$manure_area[which(data_mean$country=="Belarus")] <- mean(data$manure_area[which(data$country=="Belarus")])

data_mean$netcap14_area <- NA
data_mean$netcap14_area[which(data_mean$country=="Albania")] <- mean(data$netcap14_area[which(data$country=="Albania")])
data_mean$netcap14_area[which(data_mean$country=="Algeria")] <- mean(data$netcap14_area[which(data$country=="Algeria")])
data_mean$netcap14_area[which(data_mean$country=="Argentina")] <- mean(data$netcap14_area[which(data$country=="Argentina")])
data_mean$netcap14_area[which(data_mean$country=="Australia")] <- mean(data$netcap14_area[which(data$country=="Australia")])
data_mean$netcap14_area[which(data_mean$country=="Azerbaijan")] <- mean(data$netcap14_area[which(data$country=="Azerbaijan")])
data_mean$netcap14_area[which(data_mean$country=="Bangladesh")] <- mean(data$netcap14_area[which(data$country=="Bangladesh")])
data_mean$netcap14_area[which(data_mean$country=="Bolivia_(Plurinational_State_of)")] <- mean(data$netcap14_area[which(data$country=="Bolivia_(Plurinational_State_of)")])
data_mean$netcap14_area[which(data_mean$country=="Brazil")] <- mean(data$netcap14_area[which(data$country=="Brazil")])
data_mean$netcap14_area[which(data_mean$country=="Cambodia")] <- mean(data$netcap14_area[which(data$country=="Cambodia")])
data_mean$netcap14_area[which(data_mean$country=="Canada")] <- mean(data$netcap14_area[which(data$country=="Canada")])
data_mean$netcap14_area[which(data_mean$country=="Chile")] <- mean(data$netcap14_area[which(data$country=="Chile")])
data_mean$netcap14_area[which(data_mean$country=="China,_mainland")] <- mean(data$netcap14_area[which(data$country=="China,_mainland")])
data_mean$netcap14_area[which(data_mean$country=="Colombia")] <- mean(data$netcap14_area[which(data$country=="Colombia")])
data_mean$netcap14_area[which(data_mean$country=="Costa_Rica")] <- mean(data$netcap14_area[which(data$country=="Costa_Rica")])
data_mean$netcap14_area[which(data_mean$country=="Dominican_Republic")] <- mean(data$netcap14_area[which(data$country=="Dominican_Republic")])
data_mean$netcap14_area[which(data_mean$country=="Ecuador")] <- mean(data$netcap14_area[which(data$country=="Ecuador")])
data_mean$netcap14_area[which(data_mean$country=="Egypt")] <- mean(data$netcap14_area[which(data$country=="Egypt")])
data_mean$netcap14_area[which(data_mean$country=="El_Salvador")] <- mean(data$netcap14_area[which(data$country=="El_Salvador")])
data_mean$netcap14_area[which(data_mean$country=="Ethiopia")] <- mean(data$netcap14_area[which(data$country=="Ethiopia")])
data_mean$netcap14_area[which(data_mean$country=="Guatemala")] <- mean(data$netcap14_area[which(data$country=="Guatemala")])
data_mean$netcap14_area[which(data_mean$country=="Guinea")] <- mean(data$netcap14_area[which(data$country=="Guinea")])
data_mean$netcap14_area[which(data_mean$country=="Honduras")] <- mean(data$netcap14_area[which(data$country=="Honduras")])
data_mean$netcap14_area[which(data_mean$country=="India")] <- mean(data$netcap14_area[which(data$country=="India")])
data_mean$netcap14_area[which(data_mean$country=="Indonesia")] <- mean(data$netcap14_area[which(data$country=="Indonesia")])
data_mean$netcap14_area[which(data_mean$country=="Israel")] <- mean(data$netcap14_area[which(data$country=="Israel")])
data_mean$netcap14_area[which(data_mean$country=="Japan")] <- mean(data$netcap14_area[which(data$country=="Japan")])
data_mean$netcap14_area[which(data_mean$country=="Jordan")] <- mean(data$netcap14_area[which(data$country=="Jordan")])
data_mean$netcap14_area[which(data_mean$country=="Kenya")] <- mean(data$netcap14_area[which(data$country=="Kenya")])
data_mean$netcap14_area[which(data_mean$country=="Kuwait")] <- mean(data$netcap14_area[which(data$country=="Kuwait")])
data_mean$netcap14_area[which(data_mean$country=="Lao_People's_Democratic_Republic")] <- mean(data$netcap14_area[which(data$country=="Lao_People's_Democratic_Republic")])
data_mean$netcap14_area[which(data_mean$country=="Lebanon")] <- mean(data$netcap14_area[which(data$country=="Lebanon")])
data_mean$netcap14_area[which(data_mean$country=="Madagascar")] <- mean(data$netcap14_area[which(data$country=="Madagascar")])
data_mean$netcap14_area[which(data_mean$country=="Malawi")] <- mean(data$netcap14_area[which(data$country=="Malawi")])
data_mean$netcap14_area[which(data_mean$country=="Malaysia")] <- mean(data$netcap14_area[which(data$country=="Malaysia")])
data_mean$netcap14_area[which(data_mean$country=="Mauritania")] <- mean(data$netcap14_area[which(data$country=="Mauritania")])
data_mean$netcap14_area[which(data_mean$country=="Mexico")] <- mean(data$netcap14_area[which(data$country=="Mexico")])
data_mean$netcap14_area[which(data_mean$country=="Morocco")] <- mean(data$netcap14_area[which(data$country=="Morocco")])
data_mean$netcap14_area[which(data_mean$country=="Myanmar")] <- mean(data$netcap14_area[which(data$country=="Myanmar")])
data_mean$netcap14_area[which(data_mean$country=="New_Zealand")] <- mean(data$netcap14_area[which(data$country=="New_Zealand")])
data_mean$netcap14_area[which(data_mean$country=="Nicaragua")] <- mean(data$netcap14_area[which(data$country=="Nicaragua")])
data_mean$netcap14_area[which(data_mean$country=="Nigeria")] <- mean(data$netcap14_area[which(data$country=="Nigeria")])
data_mean$netcap14_area[which(data_mean$country=="Norway")] <- mean(data$netcap14_area[which(data$country=="Norway")])
data_mean$netcap14_area[which(data_mean$country=="Pakistan")] <- mean(data$netcap14_area[which(data$country=="Pakistan")])
data_mean$netcap14_area[which(data_mean$country=="Paraguay")] <- mean(data$netcap14_area[which(data$country=="Paraguay")])
data_mean$netcap14_area[which(data_mean$country=="Philippines")] <- mean(data$netcap14_area[which(data$country=="Philippines")])
data_mean$netcap14_area[which(data_mean$country=="Republic_of_Korea")] <- mean(data$netcap14_area[which(data$country=="Republic_of_Korea")])
data_mean$netcap14_area[which(data_mean$country=="Republic_of_Moldova")] <- mean(data$netcap14_area[which(data$country=="Republic_of_Moldova")])
data_mean$netcap14_area[which(data_mean$country=="Saudi_Arabia")] <- mean(data$netcap14_area[which(data$country=="Saudi_Arabia")])
data_mean$netcap14_area[which(data_mean$country=="South_Africa")] <- mean(data$netcap14_area[which(data$country=="South_Africa")])
data_mean$netcap14_area[which(data_mean$country=="Sri_Lanka")] <- mean(data$netcap14_area[which(data$country=="Sri_Lanka")])
data_mean$netcap14_area[which(data_mean$country=="Switzerland")] <- mean(data$netcap14_area[which(data$country=="Switzerland")])
data_mean$netcap14_area[which(data_mean$country=="Syrian_Arab_Republic")] <- mean(data$netcap14_area[which(data$country=="Syrian_Arab_Republic")])
data_mean$netcap14_area[which(data_mean$country=="Thailand")] <- mean(data$netcap14_area[which(data$country=="Thailand")])
data_mean$netcap14_area[which(data_mean$country=="Togo")] <- mean(data$netcap14_area[which(data$country=="Togo")])
data_mean$netcap14_area[which(data_mean$country=="Turkey")] <- mean(data$netcap14_area[which(data$country=="Turkey")])
data_mean$netcap14_area[which(data_mean$country=="United_Republic_of_Tanzania")] <- mean(data$netcap14_area[which(data$country=="United_Republic_of_Tanzania")])
data_mean$netcap14_area[which(data_mean$country=="United_States_of_America")] <- mean(data$netcap14_area[which(data$country=="United_States_of_America")])
data_mean$netcap14_area[which(data_mean$country=="Uruguay")] <- mean(data$netcap14_area[which(data$country=="Uruguay")])
data_mean$netcap14_area[which(data_mean$country=="Venezuela_(Bolivarian_Republic_of)")] <- mean(data$netcap14_area[which(data$country=="Venezuela_(Bolivarian_Republic_of)")])
data_mean$netcap14_area[which(data_mean$country=="Viet_Nam")] <- mean(data$netcap14_area[which(data$country=="Viet_Nam")])
data_mean$netcap14_area[which(data_mean$country=="Zambia")] <- mean(data$netcap14_area[which(data$country=="Zambia")])
data_mean$netcap14_area[which(data_mean$country=="Zimbabwe")] <- mean(data$netcap14_area[which(data$country=="Zimbabwe")])
data_mean$netcap14_area[which(data_mean$country=="EU27")] <- mean(data$netcap14_area[which(data$country=="EU27")])
data_mean$netcap14_area[which(data_mean$country=="Iran_(Islamic_Republic_of)")] <- mean(data$netcap14_area[which(data$country=="Iran_(Islamic_Republic_of)")])
data_mean$netcap14_area[which(data_mean$country=="Russian_Federation")] <- mean(data$netcap14_area[which(data$country=="Russian_Federation")])
data_mean$netcap14_area[which(data_mean$country=="Ukraine")] <- mean(data$netcap14_area[which(data$country=="Ukraine")])
data_mean$netcap14_area[which(data_mean$country=="Uzbekistan")] <- mean(data$netcap14_area[which(data$country=="Uzbekistan")])
data_mean$netcap14_area[which(data_mean$country=="Belarus")] <- mean(data$netcap14_area[which(data$country=="Belarus")])
count(data$country)

data_mean$weight <- NA
data_mean$weight[which(data_mean$country=="Albania")] <- "1"
data_mean$weight[which(data_mean$country=="Algeria")] <- "1"
data_mean$weight[which(data_mean$country=="Argentina")] <- "4"
data_mean$weight[which(data_mean$country=="Australia")] <- "4"
data_mean$weight[which(data_mean$country=="Azerbaijan")] <- "1"
data_mean$weight[which(data_mean$country=="Bangladesh")] <- "4"
data_mean$weight[which(data_mean$country=="Bolivia_(Plurinational_State_of)")] <- "1"
data_mean$weight[which(data_mean$country=="Brazil")] <- "4"
data_mean$weight[which(data_mean$country=="Cambodia")] <- "1"
data_mean$weight[which(data_mean$country=="Canada")] <- "4"
data_mean$weight[which(data_mean$country=="Chile")] <- "4"
data_mean$weight[which(data_mean$country=="China,_mainland")] <- "4"
data_mean$weight[which(data_mean$country=="Colombia")] <- "1"
data_mean$weight[which(data_mean$country=="Costa_Rica")] <- "1"
data_mean$weight[which(data_mean$country=="Dominican_Republic")] <- "1"
data_mean$weight[which(data_mean$country=="Ecuador")] <- "1"
data_mean$weight[which(data_mean$country=="Egypt")] <- "4"
data_mean$weight[which(data_mean$country=="El_Salvador")] <- "1"
data_mean$weight[which(data_mean$country=="Ethiopia")] <- "1"
data_mean$weight[which(data_mean$country=="Guatemala")] <- "1"
data_mean$weight[which(data_mean$country=="Guinea")] <- "1"
data_mean$weight[which(data_mean$country=="Honduras")] <- "1"
data_mean$weight[which(data_mean$country=="India")] <- "4"
data_mean$weight[which(data_mean$country=="Indonesia")] <- "4"
data_mean$weight[which(data_mean$country=="Israel")] <- "3"
data_mean$weight[which(data_mean$country=="Japan")] <- "4"
data_mean$weight[which(data_mean$country=="Jordan")] <- "1"
data_mean$weight[which(data_mean$country=="Kenya")] <- "1"
data_mean$weight[which(data_mean$country=="Kuwait")] <- "1"
data_mean$weight[which(data_mean$country=="Lao_People's_Democratic_Republic")] <- "1"
data_mean$weight[which(data_mean$country=="Lebanon")] <- "1"
data_mean$weight[which(data_mean$country=="Madagascar")] <- "1"
data_mean$weight[which(data_mean$country=="Malawi")] <- "1"
data_mean$weight[which(data_mean$country=="Malaysia")] <- "4"
data_mean$weight[which(data_mean$country=="Mauritania")] <- "1"
data_mean$weight[which(data_mean$country=="Mexico")] <- "4"
data_mean$weight[which(data_mean$country=="Morocco")] <- "4"
data_mean$weight[which(data_mean$country=="Myanmar")] <- "1"
data_mean$weight[which(data_mean$country=="New_Zealand")] <- "2"
data_mean$weight[which(data_mean$country=="Nicaragua")] <- "1"
data_mean$weight[which(data_mean$country=="Nigeria")] <- "1"
data_mean$weight[which(data_mean$country=="Norway")] <- "1"
data_mean$weight[which(data_mean$country=="Pakistan")] <- "4"
data_mean$weight[which(data_mean$country=="Paraguay")] <- "1"
data_mean$weight[which(data_mean$country=="Philippines")] <- "4"
data_mean$weight[which(data_mean$country=="Republic_of_Korea")] <- "1"
data_mean$weight[which(data_mean$country=="Republic_of_Moldova")] <- "1"
data_mean$weight[which(data_mean$country=="Saudi_Arabia")] <- "1" 
data_mean$weight[which(data_mean$country=="South_Africa")] <- "4"
data_mean$weight[which(data_mean$country=="Sri_Lanka")] <- "1"
data_mean$weight[which(data_mean$country=="Switzerland")] <- "1"
data_mean$weight[which(data_mean$country=="Syrian_Arab_Republic")] <- "1"
data_mean$weight[which(data_mean$country=="Thailand")] <- "4"
data_mean$weight[which(data_mean$country=="Togo")] <- "1"
data_mean$weight[which(data_mean$country=="Turkey")] <- "4"
data_mean$weight[which(data_mean$country=="United_Republic_of_Tanzania")] <- "1"
data_mean$weight[which(data_mean$country=="United_States_of_America")] <- "4"
data_mean$weight[which(data_mean$country=="Uruguay")] <- "1"
data_mean$weight[which(data_mean$country=="Venezuela_(Bolivarian_Republic_of)")] <- "1"
data_mean$weight[which(data_mean$country=="Viet_Nam")] <- "4"
data_mean$weight[which(data_mean$country=="Zambia")] <- "1"
data_mean$weight[which(data_mean$country=="Zimbabwe")] <- "1"
data_mean$weight[which(data_mean$country=="EU27")] <- "4"
data_mean$weight[which(data_mean$country=="Iran_(Islamic_Republic_of)")] <- "3"
data_mean$weight[which(data_mean$country=="Russian_Federation")] <- "3"
data_mean$weight[which(data_mean$country=="Ukraine")] <- "3"
data_mean$weight[which(data_mean$country=="Uzbekistan")] <- "3"
data_mean$weight[which(data_mean$country=="Belarus")] <- "2"

write.csv(data_mean,file="data_mean.csv")
