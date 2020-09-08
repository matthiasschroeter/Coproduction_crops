
###############################################################################################################
#####                  prepare the data of energy use for fertilizer production                            #####
###############################################################################################################
# load data
IFA_2006<-read.csv("IFA_2006.csv",sep=";",header=T)

# replace NA by "0"
IFA_2006[is.na(IFA_2006)] <- 0

# change the unit kilotonnes into tonnes
IFA_2006[,c(3:13)]<-IFA_2006[,c(3:13)]*1000
###############################################################################################################
##### Split crops, then aggregate EU27                                                                    #####
###############################################################################################################
# in the IFA fertilizer data some crops are grouped together, 
# as in some cases we just want to take some crops out of the group, we have to split up the value according to the area harvested
#crops_food<-read.csv("Tabel_1.3.csv",sep=",",header=T)
#crops_food<-crops_food[,c(3,4,5,9)]
crops_food_11d<-read.csv("crops_food_11d.csv",sep=",",header=T)[,-1]
crops_food_11d$Area<-paste(gsub("[ ]","_",crops_food_11d$Area))
# 2006
# first we have to join the EU27 countries to one 
Eu27<-c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
        "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
        "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United Kingdom")
Eu27_2006<- crops_food_11d[which(is.element(crops_food_11d$Area,Eu27)),c(1:4)]
Eu27_2006[is.na(Eu27_2006)] <- 0
Eu27_2006_agg<- aggregate(Area_harvested_ha~Year+Item, data=Eu27_2006,sum)
Eu27_2006_agg$Area<-"Eu-27-2006"

# now same aggregated list for the rest of the countries in the IFA 2006 report
counties_2006<-c("Argentina","Australia","Bangladesh","Brazil","Canada","Chile","China,_mainland","Egypt","India",       
                 "Indonesia","Iran_(Islamic_Republic_of)","Malaysia","Mexico","Morocco","Pakistan","Philippines",
                 "Russian_Federation","South_Africa","Thailand","Turkey","United_States_of_America","Viet_Nam",
                 "Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
                 "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
                 "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United Kingdom")
IFA_counties_2006<-crops_food_11d[which(is.element(crops_food_11d$Area,counties_2006)),c(1:4)]
IFA_counties_2006[is.na(IFA_counties_2006)] <- 0

# now the same for all countries not existing in the IFA 2006 report
ROW_2006<-crops_food_11d[!(crops_food_11d$Area %in% c("Argentina","Australia","Bangladesh","Brazil","Canada","Chile","China,_mainland","Egypt","India",       
         "Indonesia","Iran_(Islamic_Republic_of)","Malaysia","Mexico","Morocco","Pakistan","Philippines",
         "Russian_Federation","South_Africa","Thailand","Turkey","United_States_of_America","Viet_Nam",
         "Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
         "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
         "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United Kingdom")),c(1:4)]
ROW_2006[is.na(ROW_2006)] <- 0
ROW_2006_agg<- aggregate(Area_harvested_ha~Year+Item, data=ROW_2006,sum)
ROW_2006_agg$Area<-"ROW-2006"

all_2006<-rbind(Eu27_2006_agg,IFA_counties_2006,ROW_2006_agg)

all_2006$Item<-paste(gsub("[ ]","_",all_2006$Item))
all_2006$Item<-paste(gsub("[,]","",all_2006$Item))
all_2006$Item<-toupper(all_2006$Item)

CG<-toupper(c("Barley","Barley_pearled","Sorghum","Bran_sorghum","Oats", "Oats_rolled","Rye",
              "Triticale","Millet","Bran_millet","Buckwheat", "Fonio",  "Flour_fonio", "Quinoa",
              "Grain_mixed","Cereal_preparations_nes","Flour_cereals","Cereals_nes"))

OS<-toupper(c("Rapeseed","Cake_rapeseed","Oil_rapeseed","Mustard_seed","Cake_mustard",
              "Flour_mustard","Sunflower_seed",
              "Cake_sunflower", "Oil_sunflower", "Groundnuts_with_shell","Groundnuts_shelled","Peanut_butter",
              "Cake_groundnuts","Oil_groundnut","Coconuts","Coconuts_desiccated","Castor_oil_seed","	Oil_castor_beans",
              "Hempseed","Cake_hempseed","Karite_nuts_(sheanuts)","Butter_of_karite_nuts","Linseed","Cake_linseed",
              "Oil_linseed","Olives","Oil_olive_virgin","Olives_preserved","Poppy_seed","Oil_poppy","Sesame_seed",
              "Cake_sesame_seed","Oil_sesame"))

SC<-toupper(c("Sugar_beet","Beet_pulp","Sugar_cane","Sugar_non-centrifugal","Cane_tops","Molasses"))

all_2006$Crop_Group<-NA
for(i in 1:length(all_2006[,1])){
  if(is.element(all_2006$Item[i],CG)){
    all_2006$Crop_Group[i]<-"CG"} 
  if(is.element(all_2006$Item[i],OS)){
    all_2006$Crop_Group[i]<-"OS"} 
  if(is.element(all_2006$Item[i],SC)){
    all_2006$Crop_Group[i]<-"SC"}}
#else {all_2006$Crop_Group[i]<-"NA"}}

##############################################################################################################################################################################################################################################
#aggregate area harvested by crop group, year and country
all_2006[is.na(all_2006)] <- "non"
all_2006_groups<-all_2006[which(all_2006$Crop_Group!="non"),]

all_2006_groups_agg<-aggregate(Area_harvested_ha~Year+Area+Crop_Group, data=all_2006_groups,sum)
colnames(all_2006_groups_agg)[colnames(all_2006_groups_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"
all_2006_groups_agg$code_Area_Year_group<-NA
all_2006_groups_agg$code_Area_Year_group<-paste(gsub("[ ]","_",all_2006_groups_agg$Area),all_2006_groups_agg$Year,all_2006_groups_agg$Crop_Group)

#######################################################################################################################
# merge the aggregated crop groups with the original list in order to have the area harvested for the single item and the group in one table
all_2006_groups$code_Area_Year_group<-NA
all_2006_groups$code_Area_Year_group<-paste(gsub("[ ]","_",all_2006_groups$Area),all_2006_groups$Year,all_2006_groups$Crop_Group)

all_2006b <- merge(all_2006_groups,all_2006_groups_agg, by="code_Area_Year_group",all=T)

all_2006c<-all_2006b[,c(1:6,10)]

#######################################################################################################################
# join the fertilizer data
IFA_2006$Area<-as.character(IFA_2006$Area)
IFA_2006[7,1]<-"China,_mainland"
IFA_2006[11,1]<-"Iran_(Islamic_Republic_of)"
IFA_2006[17,1]<-"Russian_Federation"
IFA_2006[18,1]<-"South_Africa"
IFA_2006[21,1]<-"United_States_of_America"
IFA_2006[22,1]<-"Viet_Nam"
IFA_2006[23,1]<-"Eu-27-2006"
IFA_2006[24,1]<-"ROW-2006"

Code_IFA_2006<-unique(IFA_2006$Area)
all_2006c$N_t_group<-NA
all_2006c_2<-all_2006c[which(is.element(all_2006c$Area.x,Code_IFA_2006)),]
all_2006c_2<-all_2006c_2[which(all_2006c_2$Year.x=="2006"),]

###########################
row.names(all_2006c_2)<-1:length(all_2006c_2[,1])
for(i in 1:length(all_2006c_2[,1])){
    if(all_2006c_2$Crop_Group.x[i]=="CG"){
      all_2006c_2$N_t_group[i]<-IFA_2006$Other.CG[which(IFA_2006$Area==all_2006c_2$Area.x[i])]}
    if(all_2006c_2$Crop_Group.x[i]=="OS"){
      all_2006c_2$N_t_group[i]<-IFA_2006$Other.OS[which(IFA_2006$Area==all_2006c_2$Area.x[i])]}
    if(all_2006c_2$Crop_Group.x[i]=="SC"){
      all_2006c_2$N_t_group[i]<-IFA_2006$Sugar.Crops[which(IFA_2006$Area==all_2006c_2$Area.x[i])]}
    if(all_2006c_2$Crop_Group.x[i]=="other"){
      all_2006c_2$N_t_group[i]<-"0"}}

all_2006c_3<-all_2006c_2[which(all_2006c_2$Area_harvested_ha!=0),]

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
all_2006c_3$N_t_Item<-NA
all_2006c_3$N_t_Item<-all_2006c_3$N_t_group*all_2006c_3$Area_harvested_ha/all_2006c_3$Area_H_group_ha

#######################################################################################################################
code_CG_OS_SC<-toupper(c("Barley","Barley_pearled","Sorghum","Bran_sorghum","Rye","Millet","Bran_millet",
                 "Rapeseed","Cake_rapeseed","Oil_rapeseed","Sunflower_seed","Cake_sunflower", "Oil_sunflower",
                 "Groundnuts_with_shell","Groundnuts_shelled","Peanut_butter","Cake_groundnuts","Oil_groundnut",
                 "Sugar_cane","Cane_tops"))
# we took out "Triticale"
all_2006g<-all_2006c_3[which(is.element(all_2006c_3$Item,code_CG_OS_SC)),]

all_2006g_2<-aggregate(N_t_Item~code_Area_Year_group, data=all_2006g,sum)

y<-unlist(strsplit(all_2006g_2[,1], " "))
y1<-paste(y[seq(2,length(y),3)])
all_2006g_2[,3]<-y1
y2<-paste(y[seq(1,length(y),3)])
all_2006g_2[,4]<-y2
y3<-paste(y[seq(3,length(y),3)])
all_2006g_2[,5]<-y3

all_2006g_2<-all_2006g_2[,c(4,5,2)]
colnames(all_2006g_2)<-c("Area","Item","N_t_Item")

###################################################################################
# Fill the new groups into the IFA 2006 table
library(reshape2)
all_2006g_2$Area<-as.factor(all_2006g_2$Area)
all_2006g_3 <- dcast(all_2006g_2, Area ~ Item, value.var ="N_t_Item")
IFA_2006_16<-merge(all_2006g_3,IFA_2006,by="Area",all=T)
IFA_2006_16<-IFA_2006_16[,c(1,5:8,10,11,2:4)]

# the categorie CG contains: "Barley","Sorghum","Rye","Triticale","Millet"
# the categorie OS contains:"Rapeseed","Sunflower","Groundnuts"
# the categorie SC contains:"Sugar_cane"

################save the acual tabel for a short cut ###########################################################
#write.csv(IFA_2006_16,file="IFA_2006_16a.csv")
IFA_2006_16<-read.csv("IFA_2006_16a.csv",sep=",",header=T)[,-1]
################################################################################################################

#######################################################################################################################
##### separate EZ27                                                                                          #####
#######################################################################################################################

IFA_2006_16b<-IFA_2006_16

crops_16<-read.csv("crops_16.csv",sep=",",header=T)[,-1]
crops_16$Area<-paste(gsub("[ ]","_",crops_16$Area))
# 2006
# first we have to identfy area harvested for each of the EU 27 countries
Eu27<-c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
        "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
        "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United_Kingdom")
Eu27_2006b<- crops_16[which(is.element(crops_16$Area,Eu27)),c(1:4)]
Eu27_2006b[is.na(Eu27_2006b)] <- 0
Eu27_2006b<-Eu27_2006b[which(Eu27_2006b$Area_harvested_ha!=0),]
Eu27_2006b$Crop_Group<-NA

# create codes
wheat<-c("Wheat","Bran, wheat","Bulgur","Flour, wheat")
rice<-c( "Rice, paddy","Oil, rice bran","Cake, rice bran")
maize<-c("Maize","Maize, green","Bran, maize","Cake, maize","Flour, maize","Germ, maize","Oil, maize","Popcorn",
        "Sweet corn frozen","Sweet corn prep or preserved")
other_cg<-c("Barley","Barley, pearled","Sorghum","Bran, sorghum","Rye","Millet","Bran, millet")
other_os<-c("Rapeseed","Cake, rapeseed","Oil, rapeseed","Sunflower seed","Cake, sunflower", "Oil, sunflower", "Groundnuts, with shell",
             "Groundnuts, shelled","Peanut butter","Cake, groundnuts","Oil, groundnut")
palm<-c("Oil palm fruit","Oil, palm","Palm kernels","Oil, palm kernel","Cake, palm kernel")
S_C<-c("Sugar cane","Cane tops")
soy<-c("Soybeans","Cake, soybeans","Oil, soybean")

# we took out "Triticale"

for(i in 1:length(Eu27_2006b[,1])){
  if(is.element(Eu27_2006b$Item[i],wheat)){
    Eu27_2006b$Crop_Group[i]<-"Wheat"} 
  if(is.element(Eu27_2006b$Item[i],rice)){
    Eu27_2006b$Crop_Group[i]<-"Rice"} 
  if(is.element(Eu27_2006b$Item[i],maize)){
    Eu27_2006b$Crop_Group[i]<-"Maize"}
  if(is.element(Eu27_2006b$Item[i],other_cg)){
    Eu27_2006b$Crop_Group[i]<-"Cereals"}
  if(is.element(Eu27_2006b$Item[i],other_os)){
   Eu27_2006b$Crop_Group[i]<-"Oil_crops"}
  if(is.element(Eu27_2006b$Item[i],palm)){
    Eu27_2006b$Crop_Group[i]<-"Oilpalm"}
  if(is.element(Eu27_2006b$Item[i],S_C)){
    Eu27_2006b$Crop_Group[i]<-"sugarcane"}
  if(is.element(Eu27_2006b$Item[i],soy)){
   Eu27_2006b$Crop_Group[i]<-"soya"}}

Eu27_2006b<-Eu27_2006b[which(Eu27_2006b$Year==c("2006")),]

Eu27_2006b_agg<- aggregate(Area_harvested_ha~Area+Crop_Group, data=Eu27_2006b,sum)
Eu27_all_agg<- aggregate(Area_harvested_ha~Crop_Group, data=Eu27_2006b,sum)

Eu27_2006b_agg$Area_harvested_EU_ha<-NA

for(i in 1:length(Eu27_2006b_agg[,1])){
  Eu27_2006b_agg$Area_harvested_EU_ha[i]<-Eu27_all_agg$Area_harvested_ha[which(Eu27_all_agg$Crop_Group==
                                                                           Eu27_2006b_agg$Crop_Group[i])]} 

# now integrate fertilizer data
fert_2006_EU<-IFA_2006_16b[which(IFA_2006_16b$Area=="Eu-27-2006"),]
fert_2006_EU<-t(fert_2006_EU)

Eu27_2006b_agg$N_t_group<-NA
for(i in 1:length(Eu27_2006b_agg[,1])){
  if(Eu27_2006b_agg$Crop_Group[i]=="Wheat"){
    Eu27_2006b_agg$N_t_group[i]<-fert_2006_EU[3,1]} 
  if(Eu27_2006b_agg$Crop_Group[i]=="Rice"){
    Eu27_2006b_agg$N_t_group[i]<-fert_2006_EU[4,1]} 
  if(Eu27_2006b_agg$Crop_Group[i]=="Maize"){
    Eu27_2006b_agg$N_t_group[i]<-fert_2006_EU[5,1]}
  if(Eu27_2006b_agg$Crop_Group[i]=="Cereals"){
    Eu27_2006b_agg$N_t_group[i]<-fert_2006_EU[8,1]}
  if(Eu27_2006b_agg$Crop_Group[i]=="Oil_crops"){
    Eu27_2006b_agg$N_t_group[i]<-fert_2006_EU[9,1]}
  if(Eu27_2006b_agg$Crop_Group[i]=="sugarcane"){
    Eu27_2006b_agg$N_t_group[i]<-fert_2006_EU[10,1]}
  if(Eu27_2006b_agg$Crop_Group[i]=="soya"){
    Eu27_2006b_agg$N_t_group[i]<-fert_2006_EU[6,1]}}

Eu27_2006b_agg$N_t_Item<-NA
Eu27_2006b_agg$N_t_group<-as.numeric(Eu27_2006b_agg$N_t_group)
Eu27_2006b_agg$N_t_Item<-Eu27_2006b_agg$N_t_group*Eu27_2006b_agg$Area_harvested_ha/Eu27_2006b_agg$Area_harvested_EU_ha

Eu27_2006c<-Eu27_2006b_agg[,c(1,2,6)]

################save the table for a short cut ###########################################################
#write.csv(Eu27_2006c,file="Eu27_2006c.csv")
Eu27_2006c<-read.csv("Eu27_2006c.csv",sep=",",header=T)[,-1]
################################################################################################################

# bring the Eu27_2006c in the same order like the IFA_2006_16b
library(reshape2)

Eu27_2006c$Area<-as.factor(Eu27_2006c$Area)
Eu27_2006d <- dcast(Eu27_2006c, Area ~ Crop_Group, value.var ="N_t_Item")
Eu27_2006d$Oil_Palm<-NA
Eu27_2006d<-Eu27_2006d[,c(1,8,5,3,6,9,2,4,7)]

# bind the Eu27_2006d and IFA_2006_16b
IFA_2006_16b<-IFA_2006_16b[,-2]
colnames(IFA_2006_16b)<-c("Area","Wheat","Rice","Maize","soya","Oil_Palm","Cereals","Oil_crops","sugarcane")
Fertilizer_2006<-rbind(Eu27_2006d,IFA_2006_16b)

################save the acual tabel for a short cut ###########################################################
#write.csv(Fertilizer_2006,file="Fertilizer_2006.csv")
Fertilizer_2006<-read.csv("Fertilizer_2006.csv",sep=",",header=T)[,-1]
################################################################################################################

######################################################################################################
#####    ROW 2006                                                                                #####
######################################################################################################
crops_16<-read.csv("crops_16.csv",sep=",",header=T)[,-1]
crops_16$Area<-paste(gsub("[ ]","_",crops_16$Area))
ROW_counties<-crops_16[!(crops_16$Area %in% c("Austria","Belgium","Bulgaria","Cyprus","Czechia","Denmark","Estonia",
      "Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
      "Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United_Kingdom","Argentina"                 
      ,"Australia","Bangladesh","Brazil","Canada","Chile","China,_mainland","Egypt","Eu-27-2006","India","Indonesia",
      "Iran_(Islamic_Republic_of)","Malaysia","Mexico","Morocco","Pakistan","Philippines","ROW-2006","Russian_Federation",
      "South_Africa","Thailand","Turkey","United_States_of_America","Viet_Nam")),c(1:4)]
  
ROW_counties_2006 <-ROW_counties[(ROW_counties$Year==2006),]
ROW_counties_2006[is.na(ROW_counties_2006)] <- 0
ROW_counties_2006<-ROW_counties_2006[which(ROW_counties_2006$Area_harvested_ha!=0),]
ROW_counties_2006$Crop_Group<-NA

# create codes
wheat<-c("Wheat","Bran, wheat","Bulgur","Flour, wheat")
rice<-c( "Rice, paddy","Oil, rice bran","Cake, rice bran")
maize<-c("Maize","Maize, green","Bran, maize","Cake, maize","Flour, maize","Germ, maize","Oil, maize","Popcorn",
         "Sweet corn frozen","Sweet corn prep or preserved")
other_cg<-c("Barley","Barley, pearled","Sorghum","Bran, sorghum","Rye","Millet","Bran, millet")
other_os<-c("Rapeseed","Cake, rapeseed","Oil, rapeseed","Sunflower seed","Cake, sunflower", "Oil, sunflower", "Groundnuts, with shell",
            "Groundnuts, shelled","Peanut butter","Cake, groundnuts","Oil, groundnut")
palm<-c("Oil palm fruit","Oil, palm","Palm kernels","Oil, palm kernel","Cake, palm kernel")
S_C<-c("Sugar cane","Cane tops")
soy<-c("Soybeans","Cake, soybeans","Oil, soybean")
roots<-c("Cassava","Cassava leaves","Cassava dried","Starch, cassava","Potatoes","Flour, potatoes","Potatoes, frozen",
         "Potato offals")

# we took "Triticale" out

for(i in 1:length(ROW_counties_2006[,1])){
  if(is.element(ROW_counties_2006$Item[i],wheat)){
    ROW_counties_2006$Crop_Group[i]<-"Wheat"} 
  if(is.element(ROW_counties_2006$Item[i],rice)){
    ROW_counties_2006$Crop_Group[i]<-"Rice"} 
  if(is.element(ROW_counties_2006$Item[i],maize)){
    ROW_counties_2006$Crop_Group[i]<-"Maize"}
  if(is.element(ROW_counties_2006$Item[i],other_cg)){
    ROW_counties_2006$Crop_Group[i]<-"Cereals"}
  if(is.element(ROW_counties_2006$Item[i],other_os)){
    ROW_counties_2006$Crop_Group[i]<-"Oil_crops"}
  if(is.element(ROW_counties_2006$Item[i],palm)){
    ROW_counties_2006$Crop_Group[i]<-"Oilpalm"}
  if(is.element(ROW_counties_2006$Item[i],S_C)){
    ROW_counties_2006$Crop_Group[i]<-"sugarcane"}
  if(is.element(ROW_counties_2006$Item[i],soy)){
    ROW_counties_2006$Crop_Group[i]<-"soya"}
  if(is.element(ROW_counties_2006$Item[i],roots)){
    ROW_counties_2006$Crop_Group[i]<-"Roots"}
  if(is.element(ROW_counties_2006$Item[i],palm)){
    ROW_counties_2006$Crop_Group[i]<-"Oil_Palm"}}
#aggregate the Area harvested per country and year
ROW_counties_2006_agg <- aggregate(Area_harvested_ha~Area+Crop_Group, data=ROW_counties_2006,sum)

ROW_groups<- aggregate(Area_harvested_ha~Crop_Group, data=ROW_counties_2006,sum)

ROW_counties_2006_agg$Area_harvested_ROW_ha<-NA

for(i in 1:length(ROW_counties_2006_agg[,1])){
  ROW_counties_2006_agg$Area_harvested_ROW_ha[i]<-ROW_groups$Area_harvested_ha[which(ROW_groups$Crop_Group==
                                                                                 ROW_counties_2006_agg$Crop_Group[i])]} 
# now integrate fertilizer data
fert_2006_ROW<-Fertilizer_2006[which(Fertilizer_2006$Area=="ROW-2006"),]
fert_2006_ROW<-t(fert_2006_ROW)

ROW_counties_2006_agg$N_t_group<-NA
for(i in 1:length(ROW_counties_2006_agg[,1])){
  if(ROW_counties_2006_agg$Crop_Group[i]=="Wheat"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[2,1]} 
  if(ROW_counties_2006_agg$Crop_Group[i]=="Rice"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[3,1]} 
  if(ROW_counties_2006_agg$Crop_Group[i]=="Maize"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[4,1]}
  if(ROW_counties_2006_agg$Crop_Group[i]=="Cereals"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[7,1]}
  if(ROW_counties_2006_agg$Crop_Group[i]=="Oil_crops"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[8,1]}
  if(ROW_counties_2006_agg$Crop_Group[i]=="sugarcane"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[9,1]}
  if(ROW_counties_2006_agg$Crop_Group[i]=="soya"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[5,1]}
  if(ROW_counties_2006_agg$Crop_Group[i]=="Oil_Palm"){
    ROW_counties_2006_agg$N_t_group[i]<-fert_2006_ROW[6,1]}
  if(ROW_counties_2006_agg$Crop_Group[i]=="Roots"){
    ROW_counties_2006_agg$N_t_group[i]<-0}}

ROW_counties_2006_agg$N_t_group_country<-NA
ROW_counties_2006_agg$N_t_group<-as.numeric(ROW_counties_2006_agg$N_t_group)
ROW_counties_2006_agg$N_t_group_country<-ROW_counties_2006_agg$N_t_group*ROW_counties_2006_agg$Area_harvested_ha/ROW_counties_2006_agg$Area_harvested_ROW_ha

ROW_counties_2006_agg<-ROW_counties_2006_agg[,c(1,2,6)]

# bring the ROW_counties_2006_agg in the same order like the Fertilizer_2006
library(reshape2)

ROW_counties_2006_agg$Area<-as.factor(ROW_counties_2006_agg$Area)
ROW_2006b <- dcast(ROW_counties_2006_agg, Area ~ Crop_Group, value.var ="N_t_group_country")
ROW_2006b<-ROW_2006b[,c(1,10,6,3,8,5,2,4,9,7)]

Fertilizer_2006$Roots<-NA
# bind the ROW_2006b and fertilizer data
Fertilizer_2006a<-rbind(Fertilizer_2006,ROW_2006b)

################save table for a short cut ###########################################################
#write.csv(Fertilizer_2006a,file="Fertilizer_2006a.csv")
Fertilizer_2006a<-read.csv("Fertilizer_2006a.csv",sep=",",header=T)[,-1]
################################################################################################################

######################################################################################################
#####    potato - cassava distinction 2006                                                      #####
######################################################################################################
Fertilizer_2010a<-read.csv("Fertilizer_2010a.csv",sep=",",header=T)[,-1]
crops_16<-read.csv("crops_16.csv",sep=",",header=T)[,-1]
crops_16$Area<-paste(gsub("[ ]","_",crops_16$Area))

#  Fert_2006= Fert_2010*Area_H_2006/Area_H_2010
RT<-c("Cassava","Cassava_leaves","Cassava_dried","Starch_cassava","Potatoes",
      "Flour_potatoes","Potatoes_frozen","Potato_offals")

crops_RT<-crops_16[which(is.element(crops_16$Item,RT)),c(1:4)]

crops_RT_06<-crops_RT[which(crops_RT$Year=="2006"),]
crops_RT_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_RT_06,sum)
colnames(crops_RT_06_agg)[colnames(crops_RT_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_RT_10<-crops_RT[which(crops_RT$Year=="2010"),]
crops_RT_10_agg<-aggregate(Area_harvested_ha~Area, data=crops_RT_10,sum)
colnames(crops_RT_10_agg)[colnames(crops_RT_10_agg)=="Area_harvested_ha"] <- "Area_H_10"

crops_RT_2 <- merge(crops_RT_06_agg,crops_RT_10_agg, by="Area",all=T)
crops_RT_2$N_t_roots_10<-NA

# integrate fertilizer data
code_area_fert10<-unique(Fertilizer_2010a$Area)
for(i in 1:length(crops_RT_2[,1])){
  if(is.element(crops_RT_2$Area[i],code_area_fert10)){
  crops_RT_2$N_t_roots_10[i]<-Fertilizer_2010a$Roots[which(Fertilizer_2010a$Area==
                                                           crops_RT_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_RT_2$N_t_roots_06<-NA
crops_RT_2$N_t_roots_06<-crops_RT_2$N_t_roots_10*crops_RT_2$Area_H_06/crops_RT_2$Area_H_10

### now integrate it into the Fertilizer_2006$Roots 
code_area_RT<-unique(crops_RT_2$Area)
for(i in 1:length(Fertilizer_2006a[,1])){
  if(is.element(Fertilizer_2006a$Area[i],code_area_RT)){
    Fertilizer_2006a$Roots[i]<-crops_RT_2$N_t_roots_06[which(crops_RT_2$Area==
                                                                   Fertilizer_2006a$Area[i])]}
  else{}}

Fertilizer_2006b<-Fertilizer_2006a
################save table for a shortcut ###########################################################
#write.csv(Fertilizer_2006b,file="Fertilizer_2006b.csv")
Fertilizer_2006b<-read.csv("Fertilizer_2006b.csv",sep=",",header=T)[,-1]
################################################################################################################


######################################################################################################
######## Fertilizer production              ##########################################################
######################################################################################################
# load data
energy_fertelizer<-read.csv("energy_fertilizer.csv",sep=";",header=T)
energy_fertelizer$Region<-as.character(energy_fertelizer$Region)
# we asume that Japan has a value between "Western Europe" an "North America"
energy_fertelizer[13,1]<-"Japan"
energy_fertelizer[13,2]<-(energy_fertelizer[1,2]+energy_fertelizer[2,2])/2

# change the unit from Gj/t to Kj/t
energy_fertelizer$kJ_t_Ammonia<-NA
energy_fertelizer$kJ_t_Ammonia<-energy_fertelizer$GJ_t_Ammonia*1000000
# now only for N (not Ammonia)
energy_fertelizer$kJ_t_N<-NA
energy_fertelizer$kJ_t_N<-energy_fertelizer$kJ_t_Ammonia/1.21589

# create a code for the regions
energy_fertelizer$Region<-paste(gsub("[ ]","_",energy_fertelizer$Region))
Western_Europe<-c("Austria","Belgium","Denmark","Finland","France","Germany","Greece",
                  "Ireland","Italy","Luxembourg","Malta","Netherlands","Portugal","Spain","Sweden","United_Kingdom",
                  "Norway","Iceland","Switzerland","Faroe_Islands","Turkey","Hungary","Czechia")

#IPBES classification Central Europe
Central_European_countries<-c("Albania","Bosnia_and_Herzegovina","Bulgaria","Croatia","Cyprus","Estonia",
                              "Latvia","Lithuania","Montenegro","Poland","Romania","Serbia","Slovakia","Slovenia",
                              "The_former_Yugoslav_Republic_of_Macedonia")

North_America<-c("Canada","United_States_of_America")  
CIS<-c("Russian_Federation","Armenia","Azerbaijan","Belarus","Kazakhstan","Kyrgyzstan","Republic_of_Moldova","Tajikistan",
       "Ukraine","Uzbekistan","Georgia","Turkmenistan")


China_<-c("China,_mainland","China,_Taiwan_Province_of")
India<-c("India")         
Other_Asia<-c("Bangladesh","Indonesia","Malaysia","Pakistan","Philippines","Thailand","Viet_Nam","Afghanistan","Bhutan",
              "Brunei_Darussalam","Democratic_People's_Republic_of_Korea","Fiji","French_Polynesia","Maldives","Mongolia",
              "Myanmar","Nepal","New_Caledonia","Papua_New_Guinea","Samoa","Solomon_Islands","Sri_Lanka","Vanuatu",
              "American_Samoa","Cambodia","Cook_Islands","Guam","Lao_People's_Democratic_Republic",
              "Micronesia_(Federated_States_of)","Niue","Republic_of_Korea","Timor-Leste","Tonga","Wallis_and_Futuna_Islands")           

Latin_America <-c("Argentina","Chile","Brazil","Mexico","Antigua_and_Barbuda","Bahamas","Barbados","Belize","Bermuda",
                  "Bolivia_(Plurinational_State_of)","Colombia","Costa_Rica","Cuba","Dominica","Dominican_Republic",
                  "Ecuador","El_Salvador","French_Guiana","Grenada","Guadeloupe","Guatemala","Guyana","Haiti","Honduras",
                  "Jamaica","Martinique","Nicaragua","Panama","Paraguay","Peru","Saint_Kitts_and_Nevis","Saint_Lucia",
                  "Saint_Vincent_and_the_Grenadines","Suriname","Trinidad_and_Tobago","Uruguay","Venezuela_(Bolivarian_Republic_of)",
                  "Cayman_Islands","Montserrat","Puerto_Rico")

Africa   <-c("Egypt","Morocco","South_Africa","Algeria","Angola","Benin","Botswana","Burkina_Faso","Burundi","Cabo_Verde",
             "Cameroon","Central_African_Republic","Chad","Congo","Côte_d'Ivoire","Democratic_Republic_of_the_Congo",
             "Djibouti","Equatorial_Guinea","Eritrea","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya",
             "Liberia","Libya","Madagascar","Malawi","Mali","Mauritania","Mauritius","Mozambique","Niger","Nigeria","Rwanda",
             "Sao_Tome_and_Principe","Senegal","Seychelles","Sierra_Leone","Somalia","Swaziland","Togo","Tunisia","Uganda",
             "United_Republic_of_Tanzania","Zambia","Zimbabwe","Comoros","Lesotho","Namibia","Réunion","Western_Sahara",
             "Sudan","South_Sudan")

Middle_East <-c("Iran_(Islamic_Republic_of)","Bahrain","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Qatar","Saudi_Arabia",
                "United_Arab_Emirates","Yemen","Occupied_Palestinian_Territory","Syrian_Arab_Republic")

Oceania    <-c("Australia","New_Zealand")
Japan <-c("Japan")

# aggregate fertilizer used per country
Fertilizer_2006c<-Fertilizer_2006b
Fertilizer_2006c[is.na(Fertilizer_2006c)] <- 0
#sum of fertilizer used for all crops
Fertilizer_2006c$sum_t<-NA
for(i in 1:length(Fertilizer_2006c[,1])){
  Fertilizer_2006c$sum_t[i]<-sum(Fertilizer_2006c[i,2:10])}

# integrate the region code
Fertilizer_2006c$Region<-NA
for(i in 1:length(Fertilizer_2006c[,1])){
  if(is.element(Fertilizer_2006c$Area[i],Western_Europe)){
    Fertilizer_2006c$Region[i]<-"Western_Europe"} 
  if(is.element(Fertilizer_2006c$Area[i],Central_European_countries)){
    Fertilizer_2006c$Region[i]<-"Central_European_countries"}
  if(is.element(Fertilizer_2006c$Area[i],North_America)){
    Fertilizer_2006c$Region[i]<-"North_America"} 
  if(is.element(Fertilizer_2006c$Area[i],CIS)){
    Fertilizer_2006c$Region[i]<-"CIS"}
  if(is.element(Fertilizer_2006c$Area[i],China_)){
    Fertilizer_2006c$Region[i]<-"China_"}
  if(is.element(Fertilizer_2006c$Area[i],India)){
    Fertilizer_2006c$Region[i]<-"India"}
  if(is.element(Fertilizer_2006c$Area[i],Other_Asia)){
    Fertilizer_2006c$Region[i]<-"Other_Asia"}
  if(is.element(Fertilizer_2006c$Area[i],Latin_America)){
    Fertilizer_2006c$Region[i]<-"Latin_America"}
  if(is.element(Fertilizer_2006c$Area[i],Africa)){
    Fertilizer_2006c$Region[i]<-"Africa"}
  if(is.element(Fertilizer_2006c$Area[i],Middle_East)){
    Fertilizer_2006c$Region[i]<-"Middle_East"}
  if(is.element(Fertilizer_2006c$Area[i],Oceania)){
    Fertilizer_2006c$Region[i]<-"Oceania"}
  if(is.element(Fertilizer_2006c$Area[i],Japan)){
    Fertilizer_2006c$Region[i]<-"Japan"}}

# integrate the engergy use per tonne 
Fertilizer_2006c$kJ_t_N<-NA
for(i in 1:length(Fertilizer_2006c[,1])){
  if(is.element(Fertilizer_2006c$Area[i],Western_Europe)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]} 
  if(is.element(Fertilizer_2006c$Area[i],Central_European_countries)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],North_America)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]} 
  if(is.element(Fertilizer_2006c$Area[i],CIS)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],China_)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],India)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],Other_Asia)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],Latin_America)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],Africa)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],Middle_East)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],Oceania)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}
  if(is.element(Fertilizer_2006c$Area[i],Japan)){
    Fertilizer_2006c$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2006c$Region[i])]}}

# multiply fertilizer use times energy for fertilizer production 
Fertilizer_2006c$N_total_kj<-NA
Fertilizer_2006c$N_total_kj<-Fertilizer_2006c$sum_t*Fertilizer_2006c$kJ_t_N

Fertilizer_2006d<-Fertilizer_2006c[!(Fertilizer_2006c$Area %in% c("Eu-27-2006", "ROW-2006")),]
Fertilizer_2006d$Area<-as.character(Fertilizer_2006d$Area)

################save table for a shortcut ###########################################################
#write.csv(Fertilizer_2006d,file="Fertilizer_2006d.csv")
Fertilizer_2006d<-read.csv("Fertilizer_2006d.csv",sep=",",header=T)[,-1]
################################################################################################################
