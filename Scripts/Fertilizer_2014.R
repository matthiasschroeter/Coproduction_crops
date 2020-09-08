
###############################################################################################################
#####                  prepare the data of energy use for fertilizer production                          #####
###############################################################################################################
# load data
IFA_2014<-read.csv("IFA_2014.csv",sep=";",header=T)[1:30,]

# replace NA by "0"
IFA_2014[is.na(IFA_2014)] <- 0

# we want to change the unit kilotonnes into tonnes
IFA_2014[,c(3:16)]<-IFA_2014[,c(3:16)]*1000

###############################################################################################################
##### Split crops, then aggregate EU27                                                                    #####
###############################################################################################################
# in the IFA fertilizer data some crops are grouped together, 
# as in some cases we just want to take some crops out of the group, we have to split up the value according to the area harvested

crops_food_11d<-read.csv("crops_food_11d.csv",sep=",",header=T)[,-1]
crops_food_11d$Area<-paste(gsub("[ ]","_",crops_food_11d$Area))
# 2014
# first we have to join the EU28 countries to one 
Eu28<-c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
        "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
        "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United Kingdom")
Eu28_2014<- crops_food_11d[which(is.element(crops_food_11d$Area,Eu28)),c(1:4)]
Eu28_2014[is.na(Eu28_2014)] <- 0
Eu28_2014_agg<- aggregate(Area_harvested_ha~Year+Item, data=Eu28_2014,sum)
Eu28_2014_agg$Area<-"Eu-28-2014"
Eu28_2014_agg<-Eu28_2014_agg[,c(4,2,1,3)]

# same aggregated list for the rest of the countries in the IFA 2006 report
counties_2014<-c("Argentina","Australia","Bangladesh", "Belarus","Brazil","Canada","China,_mainland","Chile","Egypt","India",       
                 "Indonesia","Iran_(Islamic_Republic_of)","Japan" ,"Malaysia","Mexico","Morocco","New_Zealand","Pakistan",
                 "Philippines","Russian_Federation","South_Africa","Thailand","Turkey","Ukraine","United_States_of_America",
                 "Uzbekistan","Viet_Nam",
                 "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
                 "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
                 "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United_Kingdom")
IFA_counties_2014<-crops_food_11d[which(is.element(crops_food_11d$Area,counties_2014)),c(1:4)]
IFA_counties_2014[is.na(IFA_counties_2014)] <- 0

# now the same for all countries not existing in the IFA 2006 report
ROW_2014<-crops_food_11d[!(crops_food_11d$Area %in% c("Argentina","Australia","Bangladesh", "Belarus","Brazil","Canada",
                  "China,_mainland","Chile","Egypt","India",       
                  "Indonesia","Iran_(Islamic_Republic_of)","Japan" ,"Malaysia","Mexico","Morocco","New_Zealand","Pakistan",
                  "Philippines","Russian_Federation","South_Africa","Thailand","Turkey","Ukraine","United_States_of_America",
                  "Uzbekistan","Viet_Nam",
                  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
                  "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
                  "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United_Kingdom")),c(1:4)]
ROW_2014[is.na(ROW_2014)] <- 0
ROW_2014_agg<- aggregate(Area_harvested_ha~Year+Item, data=ROW_2014,sum)
ROW_2014_agg$Area<-"ROW-2014"
ROW_2014_agg<-ROW_2014_agg[,c(4,2,1,3)]

all_2014<-rbind(Eu28_2014_agg,IFA_counties_2014,ROW_2014_agg)

all_2014$Item<-paste(gsub("[ ]","_",all_2014$Item))
all_2014$Item<-paste(gsub("[,]","",all_2014$Item))

all_2014$Item<-toupper(all_2014$Item)

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

RT<-toupper(c("Cassava","Cassava_leaves","Cassava_dried","Starch_cassava","Potatoes",
              "Flour_potatoes","Potatoes_frozen","Potato_offals","Sweet_potatoes","Yams","Taro_(cocoyam)",
              "Yautia_(cocoyam)","Flour_roots_and_tubers_nes","Roots_and_tubers_nes"))

all_2014$Crop_Group<-NA
for(i in 1:length(all_2014[,1])){
  if(is.element(all_2014$Item[i],CG)){
    all_2014$Crop_Group[i]<-"CG"} 
  if(is.element(all_2014$Item[i],OS)){
    all_2014$Crop_Group[i]<-"OS"} 
  if(is.element(all_2014$Item[i],SC)){
    all_2014$Crop_Group[i]<-"SC"}
  if(is.element(all_2014$Item[i],RT)){
    all_2014$Crop_Group[i]<-"RT"}}
# else {all_2014$Crop_Group[i]<-"NA"}}
#######################################################################################################################
#aggregate area harvested by crop group, year and country
all_2014[is.na(all_2014)] <- "non"
all_2014_groups<-all_2014[which(all_2014$Crop_Group!="non"),]

all_2014_groups_agg<-aggregate(Area_harvested_ha~Year+Area+Crop_Group, data=all_2014_groups,sum)
colnames(all_2014_groups_agg)[colnames(all_2014_groups_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"
all_2014_groups_agg$code_Area_Year_group<-NA
all_2014_groups_agg$code_Area_Year_group<-paste(gsub("[ ]","_",all_2014_groups_agg$Area),all_2014_groups_agg$Year,all_2014_groups_agg$Crop_Group)

#######################################################################################################################
# now merge the aggregated crop groups with the original list in order to have the area harvested for the single item and the group in one table
all_2014$code_Area_Year_group<-NA
all_2014$code_Area_Year_group<-paste(gsub("[ ]","_",all_2014$Area),all_2014$Year,all_2014$Crop_Group)

all_2014b <- merge(all_2014,all_2014_groups_agg, by="code_Area_Year_group",all=T)
all_2014b<-all_2014b[which(all_2014b$Crop_Group.x!="non"),]


all_2014c<-all_2014b[,c(1:6,10)]

#######################################################################################################################
# now join the fertilizer data
IFA_2014$Area<-as.character(IFA_2014$Area)
IFA_2014[7,1]<-"China,_mainland"
IFA_2014[12,1]<-"Iran_(Islamic_Republic_of)"
IFA_2014[20,1]<-"Russian_Federation"
IFA_2014[21,1]<-"South_Africa"
IFA_2014[25,1]<-"United_States_of_America"
IFA_2014[27,1]<-"Viet_Nam"
IFA_2014[28,1]<-"Eu-28-2014"
IFA_2014[29,1]<-"ROW-2014"
IFA_2014[17,1]<-"New_Zealand"

Code_IFA_2014<-unique(IFA_2014$Area)
all_2014c$N_t_group<-NA
### cut out only IFA 2014 countries
all_2014c_2<-all_2014c[which(is.element(all_2014c$Area.x,Code_IFA_2014)),]
all_2014c_2<-all_2014c_2[which(all_2014c_2$Year.x=="2014"),]

###########################
row.names(all_2014c_2)<-1:length(all_2014c_2[,1])
for(i in 1:length(all_2014c_2[,1])){
  if(all_2014c_2$Crop_Group.x[i]=="CG"){
    all_2014c_2$N_t_group[i]<-IFA_2014$Oth.Ce[which(IFA_2014$Area==all_2014c_2$Area.x[i])]}
  else if(all_2014c_2$Crop_Group.x[i]=="OS"){
    all_2014c_2$N_t_group[i]<-IFA_2014$Oth.OS[which(IFA_2014$Area==all_2014c_2$Area.x[i])]}
  else if(all_2014c_2$Crop_Group.x[i]=="SC"){
    all_2014c_2$N_t_group[i]<-IFA_2014$Sugar[which(IFA_2014$Area==all_2014c_2$Area.x[i])]}
  else if(all_2014c_2$Crop_Group.x[i]=="RT"){
    all_2014c_2$N_t_group[i]<-IFA_2014$R.T[which(IFA_2014$Area==all_2014c_2$Area.x[i])]}
  else {}}

all_2014c_2<-all_2014c_2[which(all_2014c_2$Area_harvested_ha!=0),]

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
all_2014c_2$N_t_Item<-NA
all_2014c_2$N_t_Item<-all_2014c_2$N_t_group*all_2014c_2$Area_harvested_ha/all_2014c_2$Area_H_group_ha


#######################################################################################################################
code_CG_OS_SC_RT<-toupper(c("Barley","Barley_pearled","Sorghum","Bran_sorghum","Rye","Millet","Bran_millet",
                            "Rapeseed","Cake_rapeseed","Oil_rapeseed","Sunflower_seed","Cake_sunflower", "Oil_sunflower",
                            "Groundnuts_with_shell","Groundnuts_shelled","Peanut_butter","Cake_groundnuts","Oil_groundnut",
                            "Sugar_cane","Cane_tops","Cassava","Cassava leaves","Cassava dried","Starch, cassava","Potatoes",
                            "Flour, potatoes","Potatoes, frozen","Potato offals"))
# wee took out "Triticale"
all_2014d<-all_2014c_2[which(is.element(all_2014c_2$Item,code_CG_OS_SC_RT)),]

all_2014d_2<-aggregate(N_t_Item~code_Area_Year_group, data=all_2014d,sum)

y<-unlist(strsplit(all_2014d_2[,1], " "))
y1<-paste(y[seq(2,length(y),3)])
all_2014d_2[,3]<-y1
y2<-paste(y[seq(1,length(y),3)])
all_2014d_2[,4]<-y2
y3<-paste(y[seq(3,length(y),3)])
all_2014d_2[,5]<-y3

all_2014d_2<-all_2014d_2[,c(4,5,2)]
colnames(all_2014d_2)<-c("Area","Item","N_t_Item")

###################################################################################
# Fill the new groups into the IFA 2014 table
library(reshape2)
all_2014d_2$Area<-as.factor(all_2014d_2$Area)
all_2014d_3 <- dcast(all_2014d_2, Area ~ Item, value.var ="N_t_Item")
IFA_2014_16<-merge(all_2014d_3,IFA_2014,by="Area",all=T)
IFA_2014_16<-IFA_2014_16[-30,c(1,6:9,11,12,2:5)]

# the categorie CG contains: "Barley","Sorghum","Rye","Triticale","Millet"
# the categorie OS contains:"Rapeseed","Sunflower","Groundnuts"
# the categorie SC contains:"Sugar_cane"
# the categorie RT contains:"cassava","potatoes"
################save the acual tabel for a short cut ###########################################################
#write.csv(IFA_2014_16,file="IFA_2014_16.csv")
IFA_2014_16<-read.csv("IFA_2014_16.csv",sep=",",header=T)[,-1]
################################################################################################################

#######################################################################################################################
##### separate EU28                                                                                               #####
#######################################################################################################################
IFA_2014_16b<-IFA_2014_16

crops_16<-read.csv("crops_16.csv",sep=",",header=T)[,-1]
crops_16$Area<-paste(gsub("[ ]","_",crops_16$Area))
# 2014
# first we have to area harvested for each of the Eu 28 countries
Eu28<-c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany",
        "Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands",
        "Poland","Portugal","Romania", "Slovakia","Slovenia","Spain","Sweden","United_Kingdom")
Eu28_2014b<- crops_16[which(is.element(crops_16$Area,Eu28)),c(1:4)]
Eu28_2014b[is.na(Eu28_2014b)] <- 0
Eu28_2014b<-Eu28_2014b[which(Eu28_2014b$Area_harvested_ha!=0),]
Eu28_2014b$Crop_Group<-NA

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
#we took out "Triticale"

for(i in 1:length(Eu28_2014b[,1])){
  if(is.element(Eu28_2014b$Item[i],wheat)){
    Eu28_2014b$Crop_Group[i]<-"Wheat"} 
  if(is.element(Eu28_2014b$Item[i],rice)){
    Eu28_2014b$Crop_Group[i]<-"Rice"} 
  if(is.element(Eu28_2014b$Item[i],maize)){
    Eu28_2014b$Crop_Group[i]<-"Maize"}
  if(is.element(Eu28_2014b$Item[i],other_cg)){
    Eu28_2014b$Crop_Group[i]<-"Cereals"}
  if(is.element(Eu28_2014b$Item[i],other_os)){
    Eu28_2014b$Crop_Group[i]<-"Oil_crops"}
  if(is.element(Eu28_2014b$Item[i],palm)){
    Eu28_2014b$Crop_Group[i]<-"Oilpalm"}
  if(is.element(Eu28_2014b$Item[i],S_C)){
    Eu28_2014b$Crop_Group[i]<-"sugarcane"}
  if(is.element(Eu28_2014b$Item[i],soy)){
    Eu28_2014b$Crop_Group[i]<-"soya"}
  if(is.element(Eu28_2014b$Item[i],roots)){
    Eu28_2014b$Crop_Group[i]<-"Roots"}}

Eu28_2014b<-Eu28_2014b[which(Eu28_2014b$Year==c("2014")),]

Eu28_2014b_agg<- aggregate(Area_harvested_ha~Area+Crop_Group, data=Eu28_2014b,sum)
Eu28_all_agg.2<- aggregate(Area_harvested_ha~Crop_Group, data=Eu28_2014b,sum)

Eu28_2014b_agg$Area_harvested_EU_ha<-NA

for(i in 1:length(Eu28_2014b_agg[,1])){
  Eu28_2014b_agg$Area_harvested_EU_ha[i]<-Eu28_all_agg.2$Area_harvested_ha[which(Eu28_all_agg.2$Crop_Group==
                                                                             Eu28_2014b_agg$Crop_Group[i])]} 

# integrate fertilizer data
fert_2014_EU<-IFA_2014_16b[which(IFA_2014_16b$Area=="Eu-28-2014"),]
fert_2014_EU<-t(fert_2014_EU)

Eu28_2014b_agg$N_t_group<-NA
for(i in 1:length(Eu28_2014b_agg[,1])){
  if(Eu28_2014b_agg$Crop_Group[i]=="Wheat"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[3,1]} 
  if(Eu28_2014b_agg$Crop_Group[i]=="Rice"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[4,1]} 
  if(Eu28_2014b_agg$Crop_Group[i]=="Maize"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[5,1]}
  if(Eu28_2014b_agg$Crop_Group[i]=="Cereals"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[8,1]}
  if(Eu28_2014b_agg$Crop_Group[i]=="Oil_crops"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[9,1]}
  if(Eu28_2014b_agg$Crop_Group[i]=="sugarcane"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[11,1]}
  if(Eu28_2014b_agg$Crop_Group[i]=="soya"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[6,1]}
  if(Eu28_2014b_agg$Crop_Group[i]=="Roots"){
    Eu28_2014b_agg$N_t_group[i]<-fert_2014_EU[10,1]}}


Eu28_2014b_agg$N_t_group<-as.numeric(Eu28_2014b_agg$N_t_group)
Eu28_2014b_agg$N_t_Item<-NA
Eu28_2014b_agg$N_t_Item<-Eu28_2014b_agg$N_t_group*Eu28_2014b_agg$Area_harvested_ha/Eu28_2014b_agg$Area_harvested_EU_ha

Eu28_2014c<-Eu28_2014b_agg[,c(1,2,6)]

################save the acual tabel for a short cut ###########################################################
#write.csv(Eu28_2014c,file="Eu28_2014c.csv")
Eu28_2014c<-read.csv("Eu28_2014c.csv",sep=",",header=T)[,-1]
################################################################################################################

# bring the Eu28_2014c in the same order like the IFA_2014_16b
library(reshape2)

Eu28_2014c$Area<-as.factor(Eu28_2014c$Area)
Eu28_2014d <- dcast(Eu28_2014c, Area ~ Crop_Group, value.var ="N_t_Item")
Eu28_2014d$Oil_Palm<-NA
Eu28_2014d<-Eu28_2014d[,c(1,9,5,3,7,10,2,4,6,8)]

# bind the Eu27_2006d and IFA_2006_16b
IFA_2014_16b<-IFA_2014_16b[,-2]
colnames(IFA_2014_16b)<-c("Area","Wheat","Rice","Maize","soya","Oil_Palm","Cereals","Oil_crops","Roots","sugarcane")
Fertilizer_2014<-rbind(Eu28_2014d,IFA_2014_16b)

################save the acual tabel for a short cut ###########################################################
#write.csv(Fertilizer_2014,file="Fertilizer_2014.csv")
Fertilizer_2014<-read.csv("Fertilizer_2014.csv",sep=",",header=T)[,-1]
################################################################################################################

######################################################################################################
#####    ROW 2014                                                                               #####
######################################################################################################
crops_16<-read.csv("crops_16.csv",sep=",",header=T)[,-1]
crops_16$Area<-paste(gsub("[ ]","_",crops_16$Area))
ROW_counties3<-crops_16[!(crops_16$Area %in% c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia",
         "Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
         "Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","United_Kingdom","Argentina"                 
         ,"Australia","Bangladesh","Belarus","Brazil","Canada","Chile","China,_mainland","Egypt","Eu-28-2014","India","Indonesia",
         "Iran_(Islamic_Republic_of)","Japan","Malaysia","Mexico","Morocco","New_Zealand","Pakistan","Philippines","ROW-2014","Russian_Federation",
         "South_Africa","Thailand","Turkey","Ukraine","United_States_of_America","Uzbekistan","Viet_Nam")),c(1:4)]

ROW_counties_2014 <-ROW_counties3[(ROW_counties3$Year==2014),]
ROW_counties_2014[is.na(ROW_counties_2014)] <- 0
ROW_counties_2014<-ROW_counties_2014[which(ROW_counties_2014$Area_harvested_ha!=0),]
ROW_counties_2014$Crop_Group<-NA

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
# we took out "Triticale"

for(i in 1:length(ROW_counties_2014[,1])){
  if(is.element(ROW_counties_2014$Item[i],wheat)){
    ROW_counties_2014$Crop_Group[i]<-"Wheat"} 
  if(is.element(ROW_counties_2014$Item[i],rice)){
    ROW_counties_2014$Crop_Group[i]<-"Rice"} 
  if(is.element(ROW_counties_2014$Item[i],maize)){
    ROW_counties_2014$Crop_Group[i]<-"Maize"}
  if(is.element(ROW_counties_2014$Item[i],other_cg)){
    ROW_counties_2014$Crop_Group[i]<-"Cereals"}
  if(is.element(ROW_counties_2014$Item[i],other_os)){
    ROW_counties_2014$Crop_Group[i]<-"Oil_crops"}
  if(is.element(ROW_counties_2014$Item[i],palm)){
    ROW_counties_2014$Crop_Group[i]<-"Oilpalm"}
  if(is.element(ROW_counties_2014$Item[i],S_C)){
    ROW_counties_2014$Crop_Group[i]<-"sugarcane"}
  if(is.element(ROW_counties_2014$Item[i],soy)){
    ROW_counties_2014$Crop_Group[i]<-"soya"}
  if(is.element(ROW_counties_2014$Item[i],roots)){
    ROW_counties_2014$Crop_Group[i]<-"Roots"}
  if(is.element(ROW_counties_2014$Item[i],palm)){
    ROW_counties_2014$Crop_Group[i]<-"Oil_Palm"}}
#aggregate the area harvested per country and year
ROW_counties_2014_agg <- aggregate(Area_harvested_ha~Area+Crop_Group, data=ROW_counties_2014,sum)

ROW_groups3<- aggregate(Area_harvested_ha~Crop_Group, data=ROW_counties_2014,sum)

ROW_counties_2014_agg$Area_harvested_ROW_ha<-NA

for(i in 1:length(ROW_counties_2014_agg[,1])){
  ROW_counties_2014_agg$Area_harvested_ROW_ha[i]<-ROW_groups3$Area_harvested_ha[which(ROW_groups3$Crop_Group==
                                                                                  ROW_counties_2014_agg$Crop_Group[i])]} 
#  integrate fertilizer data
fert_2014_ROW<-Fertilizer_2014[which(Fertilizer_2014$Area=="ROW-2014"),]
fert_2014_ROW<-t(fert_2014_ROW)

ROW_counties_2014_agg$N_t_group<-NA
for(i in 1:length(ROW_counties_2014_agg[,1])){
  if(ROW_counties_2014_agg$Crop_Group[i]=="Wheat"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[2,1]} 
  if(ROW_counties_2014_agg$Crop_Group[i]=="Rice"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[3,1]} 
  if(ROW_counties_2014_agg$Crop_Group[i]=="Maize"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[4,1]}
  if(ROW_counties_2014_agg$Crop_Group[i]=="Cereals"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[7,1]}
  if(ROW_counties_2014_agg$Crop_Group[i]=="Oil_crops"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[8,1]}
  if(ROW_counties_2014_agg$Crop_Group[i]=="sugarcane"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[10,1]}
  if(ROW_counties_2014_agg$Crop_Group[i]=="soya"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[5,1]}
  if(ROW_counties_2014_agg$Crop_Group[i]=="Oil_Palm"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[6,1]}
  if(ROW_counties_2014_agg$Crop_Group[i]=="Roots"){
    ROW_counties_2014_agg$N_t_group[i]<-fert_2014_ROW[9,1]}}

ROW_counties_2014_agg$N_t_group_country<-NA
ROW_counties_2014_agg$N_t_group<-as.numeric(ROW_counties_2014_agg$N_t_group)
ROW_counties_2014_agg$N_t_group_country<-ROW_counties_2014_agg$N_t_group*ROW_counties_2014_agg$Area_harvested_ha/ROW_counties_2014_agg$Area_harvested_ROW_ha

ROW_counties_2014_agg<-ROW_counties_2014_agg[,c(1,2,6)]

# bring the ROW_counties_2006_agg in the same order like the Fertilizer_2006
library(reshape2)

ROW_counties_2014_agg$Area<-as.factor(ROW_counties_2014_agg$Area)
ROW_2014b <- dcast(ROW_counties_2014_agg, Area ~ Crop_Group, value.var ="N_t_group_country")
ROW_2014b<-ROW_2014b[,c(1,10,6,3,8,5,2,4,7,9)]

# bind ROW_2006b and fertilizer
Fertilizer_2014a<-rbind(Fertilizer_2014,ROW_2014b)

################save the acual tabel for a short cut ###########################################################
#write.csv(Fertilizer_2014a,file="Fertilizer_2014a.csv")
Fertilizer_2014a<-read.csv("Fertilizer_2014a.csv",sep=",",header=T)[,-1]
################################################################################################################

#######################################################################################################################
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

# creat a code for the regions
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
Fertilizer_2014b<-Fertilizer_2014a
Fertilizer_2014b[is.na(Fertilizer_2014b)] <- 0
#sum of fertilizer used for all crops
Fertilizer_2014b$sum_t<-NA
for(i in 1:length(Fertilizer_2014b[,1])){
  Fertilizer_2014b$sum_t[i]<-sum(Fertilizer_2014b[i,2:10])}

# integrate the region code
Fertilizer_2014b$Region<-NA
for(i in 1:length(Fertilizer_2014b[,1])){
  if(is.element(Fertilizer_2014b$Area[i],Western_Europe)){
    Fertilizer_2014b$Region[i]<-"Western_Europe"} 
  if(is.element(Fertilizer_2014b$Area[i],Central_European_countries)){
    Fertilizer_2014b$Region[i]<-"Central_European_countries"}
  if(is.element(Fertilizer_2014b$Area[i],North_America)){
    Fertilizer_2014b$Region[i]<-"North_America"} 
  if(is.element(Fertilizer_2014b$Area[i],CIS)){
    Fertilizer_2014b$Region[i]<-"CIS"}
  if(is.element(Fertilizer_2014b$Area[i],China_)){
    Fertilizer_2014b$Region[i]<-"China_"}
  if(is.element(Fertilizer_2014b$Area[i],India)){
    Fertilizer_2014b$Region[i]<-"India"}
  if(is.element(Fertilizer_2014b$Area[i],Other_Asia)){
    Fertilizer_2014b$Region[i]<-"Other_Asia"}
  if(is.element(Fertilizer_2014b$Area[i],Latin_America)){
    Fertilizer_2014b$Region[i]<-"Latin_America"}
  if(is.element(Fertilizer_2014b$Area[i],Africa)){
    Fertilizer_2014b$Region[i]<-"Africa"}
  if(is.element(Fertilizer_2014b$Area[i],Middle_East)){
    Fertilizer_2014b$Region[i]<-"Middle_East"}
  if(is.element(Fertilizer_2014b$Area[i],Oceania)){
    Fertilizer_2014b$Region[i]<-"Oceania"}
  if(is.element(Fertilizer_2014b$Area[i],Japan)){
    Fertilizer_2014b$Region[i]<-"Japan"}}

# integrate the energy use per tonne  
Fertilizer_2014b$kJ_t_N<-NA
for(i in 1:length(Fertilizer_2014b[,1])){
  if(is.element(Fertilizer_2014b$Area[i],Western_Europe)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]} 
  if(is.element(Fertilizer_2014b$Area[i],Central_European_countries)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],North_America)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]} 
  if(is.element(Fertilizer_2014b$Area[i],CIS)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],China_)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],India)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],Other_Asia)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],Latin_America)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],Africa)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],Middle_East)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],Oceania)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}
  if(is.element(Fertilizer_2014b$Area[i],Japan)){
    Fertilizer_2014b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2014b$Region[i])]}}

# multiply fertilizer use times energy for fertilizer production 
Fertilizer_2014b$N_total_kj<-NA
Fertilizer_2014b$N_total_kj<-Fertilizer_2014b$sum_t*Fertilizer_2014b$kJ_t_N

Fertilizer_2014c<-Fertilizer_2014b[!(Fertilizer_2014b$Area %in% c("Eu-28-2014", "ROW-2014")),]
Fertilizer_2014c$Area<-as.character(Fertilizer_2014c$Area)

################save tablr for a short cut ###########################################################
#write.csv(Fertilizer_2014c,file="Fertilizer_2014c.csv")
Fertilizer_2014c<-read.csv("Fertilizer_2014c.csv",sep=",",header=T)[,-1]
################################################################################################################
