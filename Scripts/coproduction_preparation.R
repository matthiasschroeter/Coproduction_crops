
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
# average values for whole country (data not used in final analysis)
crop_suit_zabel<-read.csv("crop_suit_zabel_81-10.csv",sep=";",header=T)
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
    crops_16fx$suitability[i]<-crop_suit_zabel$MEAN[which(crop_suit_zabel$NAME_LONG==crops_16fx$Country[i])]} 
  else {crops_16fx$suitability[i]<-NA}}

# suitability of 2010 for each year
years<-c("2000","2006","2010","2014")

code_suit<-unique(crops_16fx$Country)
crops_16fx$suitability<-NA
for(i in 1:length(crops_16fx[,1])){
  if((is.element(crops_16fx$Country[i],code_suit))&(is.element(crops_16fx$Year[i],years))){
    crops_16fx$suitability[i]<-crop_suit_zabel$MEAN[which(crop_suit_zabel$NAME_LONG==crops_16fx$Country[i])]} 
  else {crops_16fx$suitability[i]<-NA}}

## crop suitability data only on cropland as indicated by ESA CCI land cover data,
#not as an average for the country as a whole

crop_suit_zabel_00<-read.csv("cropsuit_Zabel_00.csv",sep=";",header=T)
crop_suit_zabel_00$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_00$NAME_LONG))

crop_suit_zabel_06<-read.csv("cropsuit_Zabel_06.csv",sep=";",header=T)
crop_suit_zabel_06$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_06$NAME_LONG))

crop_suit_zabel_10<-read.csv("cropsuit_Zabel_10.csv",sep=";",header=T)
crop_suit_zabel_10$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_10$NAME_LONG))

crop_suit_zabel_14<-read.csv("cropsuit_Zabel_14.csv",sep=";",header=T)
crop_suit_zabel_14$NAME_LONG<-paste(gsub("[ ]","_",crop_suit_zabel_14$NAME_LONG))

code_k<-unique(crops_16fx$Country)
code_k[which(is.element(unique(crops_16fx$Country),unique(crop_suit_zabel_14$NAME_LONG))==F)]

crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="China")]<-"China,_mainland"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Czech_Republic")]<-"Czechia"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Iran")]<-"Iran_(Islamic_Republic_of)"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Moldova")]<-"Republic_of_Moldova"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Syria")]<-"Syrian_Arab_Republic"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Tanzania")]<-"United_Republic_of_Tanzania"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="United_States")]<-"United_States_of_America"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
crop_suit_zabel_00$NAME_LONG[which(crop_suit_zabel_00$NAME_LONG=="Vietnam")]<-"Viet_Nam"

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

crops_16fx$cropsuit<-NA
for(i in 1:length(crops_16fx[,1])){
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2000")){
    crops_16fx$cropsuit[i]<-crop_suit_zabel_00$MEAN[which(crop_suit_zabel_00$NAME_LONG==crops_16fx$Country[i])]} 
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2006")){
    crops_16fx$cropsuit[i]<-crop_suit_zabel_06$MEAN[which(crop_suit_zabel_06$NAME_LONG==crops_16fx$Country[i])]}
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2010")){
    crops_16fx$cropsuit[i]<-crop_suit_zabel_10$MEAN[which(crop_suit_zabel_10$NAME_LONG==crops_16fx$Country[i])]}
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2014")){
    crops_16fx$cropsuit[i]<-crop_suit_zabel_14$MEAN[which(crop_suit_zabel_14$NAME_LONG==crops_16fx$Country[i])]}
}

#############################################################################################################
### market influence data                                                                               #####
#############################################################################################################
# market influence data on cropland for each year as indicated by ESA CCI land cover data
market00 <- read.csv("market00n.csv", sep = ";", header = T)
colnames(market00)<-c("Country","Mean")
market00$Country<-paste(gsub("[ ]","_",market00$Country))
code_k[which(is.element(unique(crops_16fx$Country),unique(market00$Country))==F)]
market00$Country[which(market00$Country=="Czech_Republic")]<-"Czechia"
market00$Country[which(market00$Country=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
market00$Country[which(market00$Country=="China")]<-"China,_mainland"
market00$Country[which(market00$Country=="Czech_Republic")]<-"Czechia"
market00$Country[which(market00$Country=="Iran")]<-"Iran_(Islamic_Republic_of)"
market00$Country[which(market00$Country=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
market00$Country[which(market00$Country=="Moldova")]<-"Republic_of_Moldova"
market00$Country[which(market00$Country=="Syria")]<-"Syrian_Arab_Republic"
market00$Country[which(market00$Country=="Tanzania")]<-"United_Republic_of_Tanzania"
market00$Country[which(market00$Country=="United_States")]<-"United_States_of_America"
market00$Country[which(market00$Country=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
market00$Country[which(market00$Country=="Vietnam")]<-"Viet_Nam"

market06 <- read.csv("market06n.csv", sep = ";", header = T)
colnames(market06)<-c("Country","Mean")
market06$Country<-paste(gsub("[ ]","_",market06$Country))
code_k[which(is.element(unique(crops_16fx$Country),unique(market06$Country))==F)]
market06$Country[which(market06$Country=="Czech_Republic")]<-"Czechia"
market06$Country[which(market06$Country=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
market06$Country[which(market06$Country=="China")]<-"China,_mainland"
market06$Country[which(market06$Country=="Czech_Republic")]<-"Czechia"
market06$Country[which(market06$Country=="Iran")]<-"Iran_(Islamic_Republic_of)"
market06$Country[which(market06$Country=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
market06$Country[which(market06$Country=="Moldova")]<-"Republic_of_Moldova"
market06$Country[which(market06$Country=="Syria")]<-"Syrian_Arab_Republic"
market06$Country[which(market06$Country=="Tanzania")]<-"United_Republic_of_Tanzania"
market06$Country[which(market06$Country=="United_States")]<-"United_States_of_America"
market06$Country[which(market06$Country=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
market06$Country[which(market06$Country=="Vietnam")]<-"Viet_Nam"

market10 <- read.csv("market10n.csv", sep = ";", header = T)
colnames(market10)<-c("Country","Mean")
market10$Country<-paste(gsub("[ ]","_",market10$Country))
code_k[which(is.element(unique(crops_16fx$Country),unique(market10$Country))==F)]
market10$Country[which(market10$Country=="Czech_Republic")]<-"Czechia"
market10$Country[which(market10$Country=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
market10$Country[which(market10$Country=="China")]<-"China,_mainland"
market10$Country[which(market10$Country=="Czech_Republic")]<-"Czechia"
market10$Country[which(market10$Country=="Iran")]<-"Iran_(Islamic_Republic_of)"
market10$Country[which(market10$Country=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
market10$Country[which(market10$Country=="Moldova")]<-"Republic_of_Moldova"
market10$Country[which(market10$Country=="Syria")]<-"Syrian_Arab_Republic"
market10$Country[which(market10$Country=="Tanzania")]<-"United_Republic_of_Tanzania"
market10$Country[which(market10$Country=="United_States")]<-"United_States_of_America"
market10$Country[which(market10$Country=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
market10$Country[which(market10$Country=="Vietnam")]<-"Viet_Nam"

market14 <- read.csv("market14n.csv", sep = ";", header = T)
colnames(market14)<-c("Country","Mean")
market14$Country<-paste(gsub("[ ]","_",market14$Country))
code_k[which(is.element(unique(crops_16fx$Country),unique(market14$Country))==F)]
market14$Country[which(market14$Country=="Czech_Republic")]<-"Czechia"
market14$Country[which(market14$Country=="Bolivia")]<-"Bolivia_(Plurinational_State_of)" 
market14$Country[which(market14$Country=="China")]<-"China,_mainland"
market14$Country[which(market14$Country=="Czech_Republic")]<-"Czechia"
market14$Country[which(market14$Country=="Iran")]<-"Iran_(Islamic_Republic_of)"
market14$Country[which(market14$Country=="Lao_PDR")]<-"Lao_People's_Democratic_Republic"
market14$Country[which(market14$Country=="Moldova")]<-"Republic_of_Moldova"
market14$Country[which(market14$Country=="Syria")]<-"Syrian_Arab_Republic"
market14$Country[which(market14$Country=="Tanzania")]<-"United_Republic_of_Tanzania"
market14$Country[which(market14$Country=="United_States")]<-"United_States_of_America"
market14$Country[which(market14$Country=="Venezuela")]<-"Venezuela_(Bolivarian_Republic_of)"
market14$Country[which(market14$Country=="Vietnam")]<-"Viet_Nam"

crops_16fx$market_influence<-NA
for(i in 1:length(crops_16fx[,1])){
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2000")){
    crops_16fx$market_influence[i]<-market00$Mean[which(market00$Country==crops_16fx$Country[i])]} 
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2006")){
    crops_16fx$market_influence[i]<-market06$Mean[which(market06$Country==crops_16fx$Country[i])]}
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2010")){
    crops_16fx$market_influence[i]<-market10$Mean[which(market10$Country==crops_16fx$Country[i])]}
  if((is.element(crops_16fx$Country[i],code_k))&(crops_16fx$Year[i]=="2014")){
    crops_16fx$market_influence[i]<-market14$Mean[which(market14$Country==crops_16fx$Country[i])]}
}


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
raw_data<-crops_16Lx_clean[,c("Country","Year","N_div_AreaH_kj_ha","Nt_area","production_div_AreaH_kj_ha","suitability","cropsuit","manure_area","netcap_area", "netcap14_area" ,"market_influence")]

names(raw_data)<-c("country","year","N_area","Nt_area","yield","suit","cropsuit","manure_area","netcap_area","netcap14_area","market")

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
data_EU27_00$suit<-NA
data_EU27_00$cropsuit<-NA
data_EU27_00$manure_area<-NA
data_EU27_00$netcap_area<-NA
data_EU27_00$netcap14_area<-NA
data_EU27_00$market<-NA
data_EU27_00$N_area <- sum(crops_16L_00_EU27$N_total_kj)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$Nt_area <- sum(crops_16L_00_EU27$N_total_t)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$yield <- sum(crops_16L_00_EU27$production_kj)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$suit <- 32.626161
data_EU27_00$cropsuit <- 46.498199
data_EU27_00$manure_area <- sum(crops_16L_00_EU27$Manure_kg)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$netcap_area <- sum(crops_16L_00_EU27$net_capital_stocks)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$netcap14_area <- sum(crops_16L_00_EU27$net_capital_stocks14)/sum(crops_16L_00_EU27$Area_harvested_ha)
data_EU27_00$market <- 14960.937951

data_EU27_06 <- data.frame("country"="EU27","year"=2006)
data_EU27_06$N_area<-NA
data_EU27_06$Nt_area<-NA
data_EU27_06$yield<-NA
data_EU27_06$suit<-NA
data_EU27_06$cropsuit<-NA
data_EU27_06$manure_area<-NA
data_EU27_06$netcap_area<-NA
data_EU27_06$netcap14_area<-NA
data_EU27_06$market<-NA
data_EU27_06$N_area <- sum(crops_16L_06_EU27$N_total_kj)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$Nt_area <- sum(crops_16L_06_EU27$N_total_t)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$yield <- sum(crops_16L_06_EU27$production_kj)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$suit <- 32.626161
data_EU27_06$cropsuit <- 46.378118
data_EU27_06$manure_area <- sum(crops_16L_06_EU27$Manure_kg)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$netcap_area <- sum(crops_16L_06_EU27$net_capital_stocks)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$netcap14_area <- sum(crops_16L_06_EU27$net_capital_stocks14)/sum(crops_16L_06_EU27$Area_harvested_ha)
data_EU27_06$market <- 14849.753819

data_EU27_10 <- data.frame("country"="EU27","year"=2010)
data_EU27_10$N_area<-NA
data_EU27_10$Nt_area<-NA
data_EU27_10$yield<-NA
data_EU27_10$suit<-NA
data_EU27_10$cropsuit<-NA
data_EU27_10$manure_area<-NA
data_EU27_10$netcap_area<-NA
data_EU27_10$netcap14_area<-NA
data_EU27_10$market<-NA
data_EU27_10$N_area <- sum(crops_16L_10_EU27$N_total_kj)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$Nt_area <- sum(crops_16L_10_EU27$N_total_t)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$yield <- sum(crops_16L_10_EU27$production_kj)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$suit <- 32.626161
data_EU27_10$cropsuit <- 46.310018
data_EU27_10$manure_area <- sum(crops_16L_10_EU27$Manure_kg)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$netcap_area <- sum(crops_16L_10_EU27$net_capital_stocks)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$netcap14_area <- sum(crops_16L_10_EU27$net_capital_stocks14)/sum(crops_16L_10_EU27$Area_harvested_ha)
data_EU27_10$market <- 14852.176754

data_EU27_14 <- data.frame("country"="EU27","year"=2014)
data_EU27_14$N_area<-NA
data_EU27_14$Nt_area<-NA
data_EU27_14$yield<-NA
data_EU27_14$suit<-NA
data_EU27_14$cropsuit<-NA
data_EU27_14$manure_area<-NA
data_EU27_14$netcap_area<-NA
data_EU27_14$netcap14_area<-NA
data_EU27_14$market<-NA
data_EU27_14$N_area <- sum(crops_16L_14_EU27$N_total_kj)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$Nt_area <- sum(crops_16L_14_EU27$N_total_t)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$yield <- sum(crops_16L_14_EU27$production_kj)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$suit <- 32.626161
data_EU27_14$cropsuit <- 46.258896
data_EU27_14$manure_area <- sum(crops_16L_14_EU27$Manure_kg)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$netcap_area <- sum(crops_16L_14_EU27$net_capital_stocks)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$netcap14_area <- sum(crops_16L_14_EU27$net_capital_stocks14)/sum(crops_16L_14_EU27$Area_harvested_ha)
data_EU27_14$market <- 14847.963576

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

#extract IFA countries
IFA_14 <- c("EU27","Argentina","Australia","Bangladesh", "Belarus","Brazil","Canada",
            "China,_mainland","Chile","Egypt","India","Indonesia","Iran_(Islamic_Republic_of)","Japan",
            "Malaysia","Mexico","Morocco","New_Zealand","Pakistan","Philippines","Russian_Federation",
            "South_Africa","Thailand","Turkey","Ukraine","United_States_of_America","Uzbekistan","Viet_Nam")

IFA_10 <- c("EU27","Argentina","Australia","Bangladesh", "Belarus","Brazil","Canada",
            "China,_mainland","Chile","Egypt","India","Indonesia","Iran_(Islamic_Republic_of)","Japan",
            "Malaysia","Mexico","Morocco","Pakistan","Philippines","Russian_Federation",
            "South_Africa","Thailand","Turkey","Ukraine","United_States_of_America","Uzbekistan","Viet_Nam")

IFA_06 <- c("EU27","Argentina","Australia","Bangladesh","Belarus","Brazil","Canada",
            "China,_mainland","Chile","Egypt","India","Indonesia","Iran_(Islamic_Republic_of)","Japan",
            "Malaysia","Mexico","Morocco","Pakistan","Philippines","Russian_Federation",
            "South_Africa","Thailand","Turkey","Ukraine","United_States_of_America","Uzbekistan","Viet_Nam")

IFA_00 <- c("EU27","Albania", "Australia", "Azerbaijan", "Bangladesh", "Belarus", "Canada", "China,_Taiwan_Province_of",
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
