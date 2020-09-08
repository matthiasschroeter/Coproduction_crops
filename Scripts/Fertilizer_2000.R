
###############################################################################################################
#####                  prepare the data of energy use for fertilizer production                           #####
###############################################################################################################
# load data
IFA_2000<-read.csv("IFA_2000_16x.csv",sep=";",header=T)

# replace NA by "0"
IFA_2000[is.na(IFA_2000)] <- 0

#change the unit kilotonnes into tonnes
IFA_2000a<-IFA_2000
IFA_2000a[,c(4:32)]<-IFA_2000[,c(4:32)]*1000

################save the table for a shortcut ###########################################################
#write.csv(IFA_2000a,file="IFA_2000a.csv")
IFA_2000a<-read.csv("IFA_2000a.csv",sep=",",header=T)[,-1]
################################################################################################################

# separate the table into one for single crops and one for the crop group which have to be separated
IFA_2000_special<-IFA_2000a[,c(1,19:26,28:32)]
IFA_2000_final<-IFA_2000a[,c(1:18,27)]

# for the group separation we need some crops which are not listed in the 11CG 
countries<-read.csv("countries.csv",sep=",",header=T)[,-1]
countries$Area_harvested_ha[is.na(countries$Area_harvested_ha)] <- 0
crops_all<-countries[which(countries$Area_harvested_ha!=0),]

library(reshape2)
###############################################################################################################
##### First split up the special crop groups                                                              #####
###############################################################################################################
# first special group "Oil_crops_Nuts"
# we include soy because it is listed as oil crop in the IFA lists. We exclude sunflower, as Bulgaria has a value for this crop

Oil_crops_Nuts<-c( "Oil palm fruit","Oil, palm","Palm kernels","Oil, palm kernel","Cake, palm kernel",
                     "Rapeseed","Cake, rapeseed","Oil, rapeseed","Mustard seed","Cake, mustard","Flour, mustard"
                     , "Groundnuts, with shell","Groundnuts, shelled","Peanut butter",
                     "Cake, groundnuts","Oil, groundnut","Coconuts","Coconuts, desiccated","Castor oil seed","	Oil, castor beans",
                     "Hempseed","Cake, hempseed","	Karite nuts (sheanuts)","Butter of karite nuts","Linseed","Cake, linseed",
                     "Oil, linseed","Olives","Oil, olive, virgin","Olives preserved","	Poppy seed","Oil, poppy","Sesame seed",
                     "Cake, sesame seed","Oil, sesame",
                     "Brazil nuts, with shell","Cashew nuts, with shell","Chestnut","Coconuts",
                     "Hazelnuts, with shell","Nuts, nes","	Walnuts, with shell",
                     "Soybeans","Cake, soybeans","Oil, soybean")

# select all crops from the "crops_all" table which could be part of this group
crops_oil_Nuts<-crops_all[which(is.element(crops_all$Item,Oil_crops_Nuts)),c(1:4)]
# select year 2000
crops_oil_Nuts<-crops_oil_Nuts[which(crops_oil_Nuts$Year=="2000"),]

# aggregate the area harvested of all crops belonging to this group
crops_oil_Nuts_agg<-aggregate(Area_harvested_ha~Area, data=crops_oil_Nuts,sum)
colnames(crops_oil_Nuts_agg)[colnames(crops_oil_Nuts_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"
# merge the two tables in order to have one table with the area harvested for each crop and for the respective crop group
crops_oil_Nuts_2 <- merge(crops_oil_Nuts,crops_oil_Nuts_agg, by="Area",all=T)

#cut out the countries for which this category exist
countries<-c("Bulgaria","Ethiopia")
crops_oil_Nuts_2<-crops_oil_Nuts_2[which(is.element(crops_oil_Nuts_2$Area,countries)),]

# integrate the fertilizer data given for this group
crops_oil_Nuts_2$N_t_group<-NA
row.names(crops_oil_Nuts_2)<-1:length(crops_oil_Nuts_2[,1])
for(i in 1:length(crops_oil_Nuts_2[,1])){
  if(crops_oil_Nuts_2$Area[i]=="Bulgaria"){
    crops_oil_Nuts_2$N_t_group[i]<-6000}
  else if(crops_oil_Nuts_2$Area[i]=="Ethiopia"){
    crops_oil_Nuts_2$N_t_group[i]<-1000}
  else {}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_oil_Nuts_2$N_t_Item<-NA
crops_oil_Nuts_2$N_t_Item<-crops_oil_Nuts_2$N_t_group*crops_oil_Nuts_2$Area_harvested_ha/crops_oil_Nuts_2$Area_H_group_ha

# sort oil crops that are part of the 15 crops 
co_new<-c("Groundnuts, with shell","Rapeseed","Soybeans")
crops_oil_new<-crops_oil_Nuts_2[which(is.element(crops_oil_Nuts_2$Item,co_new)),c(1,2,7)]

#dcast
crops_oil_new_2 <- dcast(crops_oil_new, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
#Bulgaria
IFA_2000_final[5,6]<-crops_oil_new_2[1,2]
IFA_2000_final[5,11]<-crops_oil_new_2[1,3]
IFA_2000_final[5,15]<-crops_oil_new_2[1,4]
#Ethiopia
IFA_2000_final[57,6]<-crops_oil_new_2[2,2]
IFA_2000_final[57,11]<-crops_oil_new_2[2,3]
IFA_2000_final[57,15]<-crops_oil_new_2[2,4]

###############################################################################################################
# second special group "Rapeseed_Mustard"
Rapeseed_Mustard<-c("Rapeseed","Cake, rapeseed","Oil, rapeseed","Mustard seed","Cake, mustard","Flour, mustard")

crops_Rapeseed_Mustard<-crops_all[which(is.element(crops_all$Item,Rapeseed_Mustard)),c(1:4)]
crops_Rapeseed_Mustard<-crops_Rapeseed_Mustard[which(crops_Rapeseed_Mustard$Year=="2000"),]

crops_Rapeseed_Mustard_agg<-aggregate(Area_harvested_ha~Area, data=crops_Rapeseed_Mustard,sum)
colnames(crops_Rapeseed_Mustard_agg)[colnames(crops_Rapeseed_Mustard_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Rap_Must_2 <- merge(crops_Rapeseed_Mustard,crops_Rapeseed_Mustard_agg, by="Area",all=T)

countries2<-c("India","Slovakia")
crops_Rap_Must_2<-crops_Rap_Must_2[which(is.element(crops_Rap_Must_2$Area,countries2)),]

crops_Rap_Must_2$N_t_group<-NA
row.names(crops_Rap_Must_2)<-1:length(crops_Rap_Must_2[,1])
for(i in 1:length(crops_Rap_Must_2[,1])){
  if(crops_Rap_Must_2$Area[i]=="India"){
    crops_Rap_Must_2$N_t_group[i]<-322500}
  else if(crops_Rap_Must_2$Area[i]=="Slovakia"){
    crops_Rap_Must_2$N_t_group[i]<-8300}
  else {}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Rap_Must_2$N_t_Item<-NA
crops_Rap_Must_2$N_t_Item<-crops_Rap_Must_2$N_t_group*crops_Rap_Must_2$Area_harvested_ha/crops_Rap_Must_2$Area_H_group_ha

crops_Rap_Must_3 <- dcast(crops_Rap_Must_2, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
#India
IFA_2000_final[20,11]<-crops_Rap_Must_3[1,3]
#Slovakia
IFA_2000_final[81,11]<-crops_Rap_Must_3[2,3]

###############################################################################################################
# special group "Roots_Tubers_other_than_potato_Yam."

Other_RT<-c("Cassava","Cassava leaves","Cassava dried","Starch, cassava","Sweet potatoes","Taro (cocoyam)",
                     "Yautia (cocoyam)","Roots and tubers, nes")

crops_other_RT<-crops_all[which(is.element(crops_all$Item,Other_RT)),c(1:4)]
crops_other_RT<-crops_other_RT[which(crops_other_RT$Year=="2000"),]

crops_other_RT_agg<-aggregate(Area_harvested_ha~Area, data=crops_other_RT,sum)
colnames(crops_other_RT_agg)[colnames(crops_other_RT_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_other_RT_2 <- merge(crops_other_RT,crops_other_RT_agg, by="Area",all=T)

countries3<-c("Japan")
crops_other_RT_2<-crops_other_RT_2[which(is.element(crops_other_RT_2$Area,countries3)),]
# we stop here, because there is no cassava in Japan

###############################################################################################################
# special group "Roots_tubers"

Roots_tubers<-c("Cassava","Cassava leaves","Cassava dried","Starch, cassava","Potatoes",
                 "Flour, potatoes","Potatoes, frozen","Potato offals","Sweet potatoes","Yams","Taro (cocoyam)",
                 "Yautia (cocoyam)","Flour, roots and tubers nes","Roots and tubers, nes")

crops_RT<-crops_all[which(is.element(crops_all$Item,Roots_tubers)),c(1:4)]
crops_RT<-crops_RT[which(crops_RT$Year=="2000"),]

crops_RT_agg<-aggregate(Area_harvested_ha~Area, data=crops_RT,sum)
colnames(crops_RT_agg)[colnames(crops_RT_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_RT_2 <- merge(crops_RT,crops_RT_agg, by="Area",all=T)

countries4<-c("Jordan")
crops_RT_2<-crops_RT_2[which(is.element(crops_RT_2$Area,countries4)),]

# just potatoes in Jordan 
IFA_2000_final[22,10]<-800

###############################################################################################################
# special group "Cereals_incl_pulses"

Cereals_pulses<-c("Wheat","Bran, wheat","Bulgur","Flour, wheat",
                    "Rice, paddy","Oil, rice bran","Cake, rice bran",
                    "Maize","Maize, green","Bran, maize","Cake, maize","Flour, maize","Germ, maize","Oil, maize","Popcorn",
                    "Sweet corn frozen","Sweet corn prep or preserved",
                    "Barley","Barley, pearled","Sorghum","Bran, sorghum","Oats", "Oats rolled","Rye","Triticale","Millet",
                    "Bran, millet","Buckwheat", "Fonio",  "Flour, fonio", "Quinoa",
                    "Grain, mixed","Cereal preparations, nes","Flour, cereals","Cereals, nes",
                  "Pulses, nes","	Pulses,Total")

crops_Cereals_pulses<-crops_all[which(is.element(crops_all$Item,Cereals_pulses)),c(1:4)]
crops_Cereals_pulses<-crops_Cereals_pulses[which(crops_Cereals_pulses$Year=="2000"),]

crops_Cereals_pulses_agg<-aggregate(Area_harvested_ha~Area, data=crops_Cereals_pulses,sum)
colnames(crops_Cereals_pulses_agg)[colnames(crops_Cereals_pulses_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Cereals_pulses_2 <- merge(crops_Cereals_pulses,crops_Cereals_pulses_agg, by="Area",all=T)

countries5<-c("Latvia")
crops_Cereals_pulses_2<-crops_Cereals_pulses_2[which(is.element(crops_Cereals_pulses_2$Area,countries5)),]
crops_Cereals_pulses_2$N_t_group<-300

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Cereals_pulses_2$N_t_Item<-NA
crops_Cereals_pulses_2$N_t_Item<-crops_Cereals_pulses_2$N_t_group*crops_Cereals_pulses_2$Area_harvested_ha/crops_Cereals_pulses_2$Area_H_group_ha

crops_Cereals_pulses_3 <- dcast(crops_Cereals_pulses_2, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
IFA_2000_final[25,4]<-crops_Cereals_pulses_3[1,2]
IFA_2000_final[25,13]<-crops_Cereals_pulses_3[1,7]
IFA_2000_final[25,19]<-crops_Cereals_pulses_3[1,8]
IFA_2000_final[25,18]<-crops_Cereals_pulses_3[1,9]

###############################################################################################################
# special group "Cereals"

Cereals<-c("Wheat","Bran, wheat","Bulgur","Flour, wheat",
           "Rice, paddy","Oil, rice bran","Cake, rice bran",
           "Maize","Maize, green","Bran, maize","Cake, maize","Flour, maize","Germ, maize","Oil, maize","Popcorn",
           "Sweet corn frozen","Sweet corn prep or preserved",
           "Barley","Barley, pearled","Sorghum","Bran, sorghum","Oats", "Oats rolled","Rye","Triticale","Millet",
           "Bran, millet","Buckwheat", "Fonio",  "Flour, fonio", "Quinoa",
           "Grain, mixed","Cereal preparations, nes","Flour, cereals","Cereals, nes")

crops_Cereals<-crops_all[which(is.element(crops_all$Item,Cereals)),c(1:4)]
crops_Cereals<-crops_Cereals[which(crops_Cereals$Year=="2000"),]

crops_Cereals_agg<-aggregate(Area_harvested_ha~Area, data=crops_Cereals,sum)
colnames(crops_Cereals_agg)[colnames(crops_Cereals_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Cereals_2 <- merge(crops_Cereals,crops_Cereals_agg, by="Area",all=T)

countries6<-c("Turkey","Australia","Belarus","Morocco")
crops_Cereals_2<-crops_Cereals_2[which(is.element(crops_Cereals_2$Area,countries6)),]

crops_Cereals_2$N_t_group<-NA
row.names(crops_Cereals_2)<-1:length(crops_Cereals_2[,1])
for(i in 1:length(crops_Cereals_2[,1])){
  if(crops_Cereals_2$Area[i]=="Turkey"){
    crops_Cereals_2$N_t_group[i]<-25000}
  else if(crops_Cereals_2$Area[i]=="Australia"){
    crops_Cereals_2$N_t_group[i]<-701700}
  else if(crops_Cereals_2$Area[i]=="Belarus"){
    crops_Cereals_2$N_t_group[i]<-115000}
  else if(crops_Cereals_2$Area[i]=="Morocco"){
    crops_Cereals_2$N_t_group[i]<-57600}
  else {}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Cereals_2$N_t_Item<-NA
crops_Cereals_2$N_t_Item<-crops_Cereals_2$N_t_group*crops_Cereals_2$Area_harvested_ha/crops_Cereals_2$Area_H_group_ha

co_new<-c("Wheat","Bran, wheat","Bulgur","Flour, wheat",
          "Rice, paddy","Oil, rice bran","Cake, rice bran",
          "Maize","Maize, green","Bran, maize","Cake, maize","Flour, maize","Germ, maize","Oil, maize","Popcorn",
          "Sweet corn frozen","Sweet corn prep or preserved",
          "Barley","Barley, pearled","Sorghum","Bran, sorghum","Rye","Triticale","Millet",
          "Bran, millet")
crops_Cereals_3<-crops_Cereals_2[which(is.element(crops_Cereals_2$Item,co_new)),c(1,2,7)]

crops_Cereals_4 <- dcast(crops_Cereals_3, Area ~ Item, value.var ="N_t_Item")
crops_Cereals_4$Maize[1]<-crops_Cereals_4[1,3]+crops_Cereals_4[1,4] #  "maize green", included in "maize"

### integrate it into the IFA_2000_final table
#Australia
IFA_2000_final[46,4]<-crops_Cereals_4[1,2]
IFA_2000_final[46,7]<-crops_Cereals_4[1,3]
IFA_2000_final[46,8]<-crops_Cereals_4[1,5]
IFA_2000_final[46,12]<-crops_Cereals_4[1,6]
IFA_2000_final[46,13]<-crops_Cereals_4[1,7]
IFA_2000_final[46,14]<-crops_Cereals_4[1,8]
IFA_2000_final[46,19]<-crops_Cereals_4[1,9]
IFA_2000_final[46,18]<-crops_Cereals_4[1,10]
#Belarus
IFA_2000_final[50,4]<-crops_Cereals_4[2,2]
IFA_2000_final[50,7]<-crops_Cereals_4[2,3]
IFA_2000_final[50,8]<-crops_Cereals_4[2,5]
IFA_2000_final[50,12]<-crops_Cereals_4[2,6]
IFA_2000_final[50,13]<-crops_Cereals_4[2,7]
IFA_2000_final[50,14]<-crops_Cereals_4[2,8]
IFA_2000_final[50,19]<-crops_Cereals_4[2,9]
IFA_2000_final[50,18]<-crops_Cereals_4[2,10]
#Morocco
IFA_2000_final[72,4]<-crops_Cereals_4[3,2]
IFA_2000_final[72,7]<-crops_Cereals_4[3,3]
IFA_2000_final[72,8]<-crops_Cereals_4[3,5]
IFA_2000_final[72,12]<-crops_Cereals_4[3,6]
IFA_2000_final[72,13]<-crops_Cereals_4[3,7]
IFA_2000_final[72,14]<-crops_Cereals_4[3,8]
IFA_2000_final[72,19]<-crops_Cereals_4[3,9]
IFA_2000_final[72,18]<-crops_Cereals_4[3,10]
#Turkey
IFA_2000_final[39,4]<-IFA_2000_final[39,4]+crops_Cereals_4[4,2]
IFA_2000_final[39,7]<-IFA_2000_final[39,7]+crops_Cereals_4[4,3]
IFA_2000_final[39,8]<-crops_Cereals_4[4,5]
IFA_2000_final[39,12]<-crops_Cereals_4[4,6]
IFA_2000_final[39,13]<-crops_Cereals_4[4,7]
IFA_2000_final[39,14]<-crops_Cereals_4[4,8]
IFA_2000_final[39,19]<-crops_Cereals_4[4,9]
IFA_2000_final[39,18]<-IFA_2000_final[39,18]+crops_Cereals_4[4,10]

###############################################################################################################
# special group "Oilseeds"
# including soy
Oilseeds<-c("Oil palm fruit","Oil, palm","Palm kernels","Oil, palm kernel","Cake, palm kernel",
            "Rapeseed","Cake, rapeseed","Oil, rapeseed","Mustard seed","Cake, mustard","Flour, mustard","Sunflower seed",
            "Cake, sunflower", "Oil, sunflower", "Groundnuts, with shell","Groundnuts, shelled","Peanut butter",
            "Cake, groundnuts","Oil, groundnut","Coconuts","Coconuts, desiccated","Castor oil seed","	Oil, castor beans",
            "Hempseed","Cake, hempseed","	Karite nuts (sheanuts)","Butter of karite nuts","Linseed","Cake, linseed",
            "Oil, linseed","Olives","Oil, olive, virgin","Olives preserved","	Poppy seed","Oil, poppy","Sesame seed",
            "Cake, sesame seed","Oil, sesame",
            "Soybeans","Cake, soybeans","Oil, soybean")

crops_Oilseeds<-crops_all[which(is.element(crops_all$Item,Oilseeds)),c(1:4)]
crops_Oilseeds<-crops_Oilseeds[which(crops_Oilseeds$Year=="2000"),]

crops_Oilseeds_agg<-aggregate(Area_harvested_ha~Area, data=crops_Oilseeds,sum)
colnames(crops_Oilseeds_agg)[colnames(crops_Oilseeds_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Oilseeds_2 <- merge(crops_Oilseeds,crops_Oilseeds_agg, by="Area",all=T)

countries7<-c("Australia","Morocco")
crops_Oilseeds_2<-crops_Oilseeds_2[which(is.element(crops_Oilseeds_2$Area,countries7)),]

crops_Oilseeds_2$N_t_group<-NA
row.names(crops_Oilseeds_2)<-1:length(crops_Oilseeds_2[,1])
for(i in 1:length(crops_Oilseeds_2[,1])){
  if(crops_Oilseeds_2$Area[i]=="Australia"){
    crops_Oilseeds_2$N_t_group[i]<-55000}
  else if(crops_Oilseeds_2$Area[i]=="Morocco"){
    crops_Oilseeds_2$N_t_group[i]<-200}
  else {}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Oilseeds_2$N_t_Item<-NA
crops_Oilseeds_2$N_t_Item<-crops_Oilseeds_2$N_t_group*crops_Oilseeds_2$Area_harvested_ha/crops_Oilseeds_2$Area_H_group_ha

co_new<-c("Groundnuts, with shell","Sunflower seed","Rapeseed","Soybeans")
crops_Oilseeds_2_new<-crops_Oilseeds_2[which(is.element(crops_Oilseeds_2$Item,co_new)),c(1,2,7)]

crops_Oilseeds_3_new <- dcast(crops_Oilseeds_2_new, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
#Australia
IFA_2000_final[46,6]<-crops_Oilseeds_3_new[1,2]
IFA_2000_final[46,11]<-crops_Oilseeds_3_new[1,3]
IFA_2000_final[46,15]<-crops_Oilseeds_3_new[1,4]
IFA_2000_final[46,17]<-crops_Oilseeds_3_new[1,5]
#Morocco
IFA_2000_final[72,6]<-crops_Oilseeds_3_new[2,2]
IFA_2000_final[72,11]<-crops_Oilseeds_3_new[2,3]
IFA_2000_final[72,15]<-crops_Oilseeds_3_new[2,4]
IFA_2000_final[72,17]<-IFA_2000_final[72,17]+crops_Oilseeds_3_new[2,5]

###############################################################################################################
# special group ""Rye_oat_rice""
RyeOatRice<-c("Rice, paddy","Oil, rice bran","Cake, rice bran","Oats", "Oats rolled","Rye")

crops_RyeOatRice<-crops_all[which(is.element(crops_all$Item,RyeOatRice)),c(1:4)]
crops_RyeOatRice<-crops_RyeOatRice[which(crops_RyeOatRice$Year=="2000"),]

crops_RyeOatRice_agg<-aggregate(Area_harvested_ha~Area, data=crops_RyeOatRice,sum)
colnames(crops_RyeOatRice_agg)[colnames(crops_RyeOatRice_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_RyeOatRices_2 <- merge(crops_RyeOatRice,crops_RyeOatRice_agg, by="Area",all=T)

countries8<-c("Belgium","Denmark","Finland","Germany","Greece","Ireland","Italy","Netherlands","Norway","Portugal","Spain",
              "Sweden","Switzerland","United Kingdom")
crops_RyeOatRices_2<-crops_RyeOatRices_2[which(is.element(crops_RyeOatRices_2$Area,countries8)),]

crops_RyeOatRices_2$N_t_group<-NA
row.names(crops_RyeOatRices_2)<-1:length(crops_RyeOatRices_2[,1])
for(i in 1:length(crops_RyeOatRices_2[,1])){
  if(crops_RyeOatRices_2$Area[i]=="Belgium"){
    crops_RyeOatRices_2$N_t_group[i]<-3000}
  else if(crops_RyeOatRices_2$Area[i]=="Denmark"){
    crops_RyeOatRices_2$N_t_group[i]<-13000}
  else if(crops_RyeOatRices_2$Area[i]=="Finland"){
    crops_RyeOatRices_2$N_t_group[i]<-29000}
  else if(crops_RyeOatRices_2$Area[i]=="Germany"){
    crops_RyeOatRices_2$N_t_group[i]<-179000}
  else if(crops_RyeOatRices_2$Area[i]=="Greece"){
    crops_RyeOatRices_2$N_t_group[i]<-7000}
  else if(crops_RyeOatRices_2$Area[i]=="Ireland"){
    crops_RyeOatRices_2$N_t_group[i]<-2000}
  else if(crops_RyeOatRices_2$Area[i]=="Italy"){
    crops_RyeOatRices_2$N_t_group[i]<-38000}
  else if(crops_RyeOatRices_2$Area[i]=="Netherlands"){
    crops_RyeOatRices_2$N_t_group[i]<-1000}
  else if(crops_RyeOatRices_2$Area[i]=="Norway"){
    crops_RyeOatRices_2$N_t_group[i]<-9000}
  else if(crops_RyeOatRices_2$Area[i]=="Portugal"){
    crops_RyeOatRices_2$N_t_group[i]<-8000}
  else if(crops_RyeOatRices_2$Area[i]=="Spain"){
    crops_RyeOatRices_2$N_t_group[i]<-51000}
  else if(crops_RyeOatRices_2$Area[i]=="Sweden"){
    crops_RyeOatRices_2$N_t_group[i]<-28000}
  else if(crops_RyeOatRices_2$Area[i]=="Switzerland"){
    crops_RyeOatRices_2$N_t_group[i]<-2000}
  else if(crops_RyeOatRices_2$Area[i]=="United Kingdom"){
    crops_RyeOatRices_2$N_t_group[i]<-12000}
  else {}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_RyeOatRices_2$N_t_Item<-NA
crops_RyeOatRices_2$N_t_Item<-crops_RyeOatRices_2$N_t_group*crops_RyeOatRices_2$Area_harvested_ha/crops_RyeOatRices_2$Area_H_group_ha

co_new<-c("Rye","Rice, paddy")
crops_RyeOatRices_2_new<-crops_RyeOatRices_2[which(is.element(crops_RyeOatRices_2$Item,co_new)),c(1,2,7)]

crops_RyeOatRices_3_new <- dcast(crops_RyeOatRices_2_new, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
IFA_2000_final[51,13]<-crops_RyeOatRices_3_new[1,3]
IFA_2000_final[55,13]<-crops_RyeOatRices_3_new[2,3]
IFA_2000_final[59,13]<-crops_RyeOatRices_3_new[3,3]
IFA_2000_final[61,13]<-crops_RyeOatRices_3_new[4,3]
IFA_2000_final[62,13]<-crops_RyeOatRices_3_new[5,3]
IFA_2000_final[64,13]<-crops_RyeOatRices_3_new[6,3]
IFA_2000_final[66,13]<-crops_RyeOatRices_3_new[7,3]
IFA_2000_final[73,13]<-crops_RyeOatRices_3_new[8,3]
IFA_2000_final[76,13]<-crops_RyeOatRices_3_new[9,3]
IFA_2000_final[79,13]<-crops_RyeOatRices_3_new[10,3]
IFA_2000_final[83,13]<-crops_RyeOatRices_3_new[11,3]
IFA_2000_final[85,13]<-crops_RyeOatRices_3_new[12,3]
IFA_2000_final[86,13]<-crops_RyeOatRices_3_new[13,3]
IFA_2000_final[88,13]<-crops_RyeOatRices_3_new[14,3]


IFA_2000_final[62,12]<-crops_RyeOatRices_3_new[5,2]
IFA_2000_final[66,12]<-crops_RyeOatRices_3_new[7,2]
IFA_2000_final[79,12]<-crops_RyeOatRices_3_new[10,2]
IFA_2000_final[83,12]<-crops_RyeOatRices_3_new[11,2]

###############################################################################################################
# special group "Cereals_other"

Cereals_other<-c("Sorghum","Bran, sorghum","Oats", "Oats rolled","Rye","Triticale","Millet",
                 "Bran, millet","Buckwheat", "Fonio",  "Flour, fonio", "Quinoa",
                 "Grain, mixed","Cereal preparations, nes","Flour, cereals","Cereals, nes")

crops_Cereals_other<-crops_all[which(is.element(crops_all$Item,Cereals_other)),c(1:4)]
crops_Cereals_other<-crops_Cereals_other[which(crops_Cereals_other$Year=="2000"),]

crops_Cereals_other_agg<-aggregate(Area_harvested_ha~Area, data=crops_Cereals_other,sum)
colnames(crops_Cereals_other_agg)[colnames(crops_Cereals_other_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Cereals_other_2 <- merge(crops_Cereals_other,crops_Cereals_other_agg, by="Area",all=T)

countries9<-c("Japan","Republic of Korea","Republic of Moldova")
crops_Cereals_other_2<-crops_Cereals_other_2[which(is.element(crops_Cereals_other_2$Area,countries9)),]

crops_Cereals_other_2$N_t_group<-NA
row.names(crops_Cereals_other_2)<-1:length(crops_Cereals_other_2[,1])
for(i in 1:length(crops_Cereals_other_2[,1])){
  if(crops_Cereals_other_2$Area[i]=="Japan"){
    crops_Cereals_other_2$N_t_group[i]<-1400}
  else if(crops_Cereals_other_2$Area[i]=="Republic of Korea"){
    crops_Cereals_other_2$N_t_group[i]<-4900}
  else if(crops_Cereals_other_2$Area[i]=="Republic of Moldova"){
    crops_Cereals_other_2$N_t_group[i]<-100}
  else {}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Cereals_other_2$N_t_Item<-NA
crops_Cereals_other_2$N_t_Item<-crops_Cereals_other_2$N_t_group*crops_Cereals_other_2$Area_harvested_ha/crops_Cereals_other_2$Area_H_group_ha

co_new<-c("Millet","Sorghum","Rye")
crops_Cereals_other_2_new<-crops_Cereals_other_2[which(is.element(crops_Cereals_other_2$Item,co_new)),c(1,2,7)]

crops_Cereals_other_3_new <- dcast(crops_Cereals_other_2_new, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
#Japan
IFA_2000_final[67,8]<-crops_Cereals_other_3_new[1,2]
#Korea Republic
IFA_2000_final[68,8]<-crops_Cereals_other_3_new[2,2]
IFA_2000_final[68,13]<-crops_Cereals_other_3_new[2,3]
IFA_2000_final[68,14]<-crops_Cereals_other_3_new[2,4]
#Moldova
IFA_2000_final[71,8]<-crops_Cereals_other_3_new[3,2]
IFA_2000_final[71,13]<-crops_Cereals_other_3_new[3,3]
IFA_2000_final[71,14]<-crops_Cereals_other_3_new[3,4]

###############################################################################################################
# special group "Rye_Oat"

Rye_Oat<-c("Oats", "Oats rolled","Rye")

crops_Rye_Oat<-crops_all[which(is.element(crops_all$Item,Rye_Oat)),c(1:4)]
crops_Rye_Oat<-crops_Rye_Oat[which(crops_Rye_Oat$Year=="2000"),]

crops_Rye_Oat_agg<-aggregate(Area_harvested_ha~Area, data=crops_Rye_Oat,sum)
colnames(crops_Rye_Oat_agg)[colnames(crops_Rye_Oat_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Rye_Oat_2 <- merge(crops_Rye_Oat,crops_Rye_Oat_agg, by="Area",all=T)

countries10<-c("Lithuania")
crops_Rye_Oat_2<-crops_Rye_Oat_2[which(is.element(crops_Rye_Oat_2$Area,countries10)),]
crops_Rye_Oat_2$N_t_group<-4500
row.names(crops_Rye_Oat_2)<-1:length(crops_Rye_Oat_2[,1])

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Rye_Oat_2$N_t_Item<-NA
crops_Rye_Oat_2$N_t_Item<-crops_Rye_Oat_2$N_t_group*crops_Rye_Oat_2$Area_harvested_ha/crops_Rye_Oat_2$Area_H_group_ha

crops_Rye_Oat_2<-crops_Rye_Oat_2[,c(1,2,7)]

crops_Rye_Oat_3 <- dcast(crops_Rye_Oat_2, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
IFA_2000_final[70,13]<-crops_Rye_Oat_3[1,3]


###############################################################################################################
# special group "Buckwheat.millet"

Buckwheat_millet<-c("Millet","Bran, millet","Buckwheat","Sorghum","Bran, sorghum")

crops_Buckwheat_millet<-crops_all[which(is.element(crops_all$Item,Buckwheat_millet)),c(1:4)]
crops_Buckwheat_millet<-crops_Buckwheat_millet[which(crops_Buckwheat_millet$Year=="2000"),]

crops_Buckwheat_millet_agg<-aggregate(Area_harvested_ha~Area, data=crops_Buckwheat_millet,sum)
colnames(crops_Buckwheat_millet_agg)[colnames(crops_Buckwheat_millet_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Buckwheat_millet_2 <- merge(crops_Buckwheat_millet,crops_Buckwheat_millet_agg, by="Area",all=T)

countries11<-c("Poland")
crops_Buckwheat_millet_2<-crops_Buckwheat_millet_2[which(is.element(crops_Buckwheat_millet_2$Area,countries11)),]
crops_Buckwheat_millet_2$N_t_group<-1500
row.names(crops_Buckwheat_millet_2)<-1:length(crops_Buckwheat_millet_2[,1])

# Poland just has Buckwheat, which is not part of the 15 crops

###############################################################################################################
# special group "Cereals_mixture"
# just Poland, as has fertilizer data vor barley, maize, rye, wheat triticale and the mixture of buckweat and millet we took
# these cereals out

Cereals_mixture<-c("Rice, paddy","Oil, rice bran","Cake, rice bran",
                 "Oats", "Oats rolled", "Fonio",  "Flour, fonio", "Quinoa",
                 "Grain, mixed","Cereal preparations, nes","Flour, cereals","Cereals, nes")

crops_Cereals_mixture<-crops_all[which(is.element(crops_all$Item,Cereals_mixture)),c(1:4)]
crops_Cereals_mixture<-crops_Cereals_mixture[which(crops_Cereals_mixture$Year=="2000"),]

crops_Cereals_mixture_agg<-aggregate(Area_harvested_ha~Area, data=crops_Cereals_mixture,sum)
colnames(crops_Cereals_mixture_agg)[colnames(crops_Cereals_mixture_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Cereals_mixture_2 <- merge(crops_Cereals_mixture,crops_Cereals_mixture_agg, by="Area",all=T)

countries12<-c("Poland")
crops_Cereals_mixture_2<-crops_Cereals_mixture_2[which(is.element(crops_Cereals_mixture_2$Area,countries12)),]
crops_Cereals_mixture_2$N_t_group<-66100
row.names(crops_Cereals_mixture_2)<-1:length(crops_Cereals_mixture_2[,1])

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Cereals_mixture_2$N_t_Item<-NA
crops_Cereals_mixture_2$N_t_Item<-crops_Cereals_mixture_2$N_t_group*crops_Cereals_mixture_2$Area_harvested_ha/crops_Cereals_mixture_2$Area_H_group_ha

crops_Cereals_mixture_2_new<-crops_Cereals_mixture_2[,c(1,2,7)]

crops_Cereals_mixture_3_new <- dcast(crops_Cereals_mixture_2_new, Area ~ Item, value.var ="N_t_Item")
# nothing of interest

###############################################################################################################
# special group ""Sunflower_soya_linseed""

Sun_soya_lin<-c("Sunflower seed","Cake, sunflower", "Oil, sunflower","Soybeans","Cake, soybeans","Oil, soybean",
                "Linseed","Cake, linseed","Oil, linseed")

crops_Sun_soya_lin<-crops_all[which(is.element(crops_all$Item,Sun_soya_lin)),c(1:4)]
crops_Sun_soya_lin<-crops_Sun_soya_lin[which(crops_Sun_soya_lin$Year=="2000"),]

crops_Sun_soya_lin_agg<-aggregate(Area_harvested_ha~Area, data=crops_Sun_soya_lin,sum)
colnames(crops_Sun_soya_lin_agg)[colnames(crops_Sun_soya_lin_agg)=="Area_harvested_ha"] <- "Area_H_group_ha"

crops_Sun_soya_lin_2 <- merge(crops_Sun_soya_lin,crops_Sun_soya_lin_agg, by="Area",all=T)

countries13<-c("Austria","France","Germany","Greece","Italy","Spain","Sweden","United Kingdom")
crops_Sun_soya_lin_2<-crops_Sun_soya_lin_2[which(is.element(crops_Sun_soya_lin_2$Area,countries13)),]

crops_Sun_soya_lin_2$N_t_group<-NA
row.names(crops_Sun_soya_lin_2)<-1:length(crops_Sun_soya_lin_2[,1])
for(i in 1:length(crops_Sun_soya_lin_2[,1])){
  if(crops_Sun_soya_lin_2$Area[i]=="Austria"){
    crops_Sun_soya_lin_2$N_t_group[i]<-2000}
  else if(crops_Sun_soya_lin_2$Area[i]=="France"){
    crops_Sun_soya_lin_2$N_t_group[i]<-55000}
  else if(crops_Sun_soya_lin_2$Area[i]=="Germany"){
    crops_Sun_soya_lin_2$N_t_group[i]<-10000}
  else if(crops_Sun_soya_lin_2$Area[i]=="Greece"){
    crops_Sun_soya_lin_2$N_t_group[i]<-2000}
  else if(crops_Sun_soya_lin_2$Area[i]=="Italy"){
    crops_Sun_soya_lin_2$N_t_group[i]<-19000}
  else if(crops_Sun_soya_lin_2$Area[i]=="Spain"){
    crops_Sun_soya_lin_2$N_t_group[i]<-12000}
  else if(crops_Sun_soya_lin_2$Area[i]=="Sweden"){
    crops_Sun_soya_lin_2$N_t_group[i]<-1000}
  else if(crops_Sun_soya_lin_2$Area[i]=="United Kingdom"){
    crops_Sun_soya_lin_2$N_t_group[i]<-5000}
  else {}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_Sun_soya_lin_2$N_t_Item<-NA
crops_Sun_soya_lin_2$N_t_Item<-crops_Sun_soya_lin_2$N_t_group*crops_Sun_soya_lin_2$Area_harvested_ha/crops_Sun_soya_lin_2$Area_H_group_ha

crops_Sun_soya_lin_2<-crops_Sun_soya_lin_2[,c(1,2,7)]

crops_Sun_soya_lin_3 <- dcast(crops_Sun_soya_lin_2, Area ~ Item, value.var ="N_t_Item")

### integrate it into the IFA_2000_final table
#Austria
IFA_2000_final[47,15]<-crops_Sun_soya_lin_3[1,3]
IFA_2000_final[47,17]<-crops_Sun_soya_lin_3[1,4]
#France
IFA_2000_final[60,15]<-crops_Sun_soya_lin_3[2,3]
IFA_2000_final[60,17]<-crops_Sun_soya_lin_3[2,4]
#Germany
IFA_2000_final[61,15]<-crops_Sun_soya_lin_3[3,3]
IFA_2000_final[61,17]<-crops_Sun_soya_lin_3[3,4]
#Greece
IFA_2000_final[62,15]<-crops_Sun_soya_lin_3[4,3]
IFA_2000_final[62,17]<-crops_Sun_soya_lin_3[4,4]
#Italy
IFA_2000_final[66,15]<-crops_Sun_soya_lin_3[5,3]
IFA_2000_final[66,17]<-crops_Sun_soya_lin_3[5,4]
#Spain
IFA_2000_final[83,15]<-crops_Sun_soya_lin_3[6,3]
IFA_2000_final[83,17]<-crops_Sun_soya_lin_3[6,4]

# 1)"Oil.crops...Nuts"    2) "Rapeseed...Mustard"       3)"Roots..Tubers..other.than.potato.and.Yam."
# 4)"Roots..tubers"       5) "Cereals..incl..pulses."   6)"Cereals"           7)"Oilseeds"                                 
# 8)"Rye..oat..rice"      9) "Cereals..other."          10)"Rye..Oat"         11)"Buckwheat..millet"                        
# 12)"Cereals.mixture"    13)"Sunflower..soya..linseed"       

IFA_2000_final$Area<-paste(gsub("[ ]","_",IFA_2000_final$Area))
################save the acual tabel for a short cut ###########################################################
#write.csv(IFA_2000_final,file="IFA_2000_final.csv")
IFA_2000_final<-read.csv("IFA_2000_final.csv",sep=",",header=T)[,-1]
################################################################################################################
# all special groups are splitted up

#######################
####   check        ###
#######################
# Find out whether data on all countries per year
Fertilizer_2006b<-read.csv("Fertilizer_2006b.csv",sep=",",header=T)[,-1]
Fertilizer_2010a<-read.csv("Fertilizer_2010a.csv",sep=",",header=T)[,-1]
Fertilizer_2014a<-read.csv("Fertilizer_2014a.csv",sep=",",header=T)[,-1]

counties_14<- Fertilizer_2014a$Area
counties_14[which(is.element(unique(Fertilizer_2014a$Area),unique(Fertilizer_2010a$Area))==F)]# in 2014 but not in 2010 
# Eu-28-2014  ROW-2014    
counties_10<- Fertilizer_2010a$Area
counties_10[which(is.element(unique(Fertilizer_2010a$Area),unique(Fertilizer_2006b$Area))==F)]# in 2010 but not in 2006 
# Eu-27-2010 ROW-2010  
counties_06<- Fertilizer_2006b$Area
counties_06[which(is.element(unique(Fertilizer_2006b$Area),unique(IFA_2000_final$Area))==F)]# in 2006 but not in 2000 

# rename countries
IFA_2000_final$Area<-as.character(IFA_2000_final$Area)
IFA_2000_final[54,1]<-"Czechia"
IFA_2000_final[4,1]<-"Brazil"
IFA_2000_final[8,1]<-"China,_mainland"
IFA_2000_final[40,1]<-"United_States_of_America"
IFA_2000_final[43,1]<-"Viet_Nam"
IFA_2000_final[1,1]<-"Algeria"
IFA_2000_final[3,1]<-"Bolivia_(Plurinational_State_of)"
IFA_2000_final[9,1]<-"China,_Taiwan_Province_of"
IFA_2000_final[24,1]<-"Lao_People's_Democratic_Republic"
IFA_2000_final[68,1]<-"Republic_of_Korea"
IFA_2000_final[71,1]<-"Republic_of_Moldova"
IFA_2000_final[42,1]<-"Venezuela_(Bolivarian_Republic_of)"
IFA_2000_final[36,1]<-"United_Republic_of_Tanzania"
IFA_2000_final[87,1]<-"Syrian_Arab_Republic"

counties_00<- IFA_2000_final$Area
counties_00[which(is.element(unique(IFA_2000_final$Area),unique(Fertilizer_2006b$Area))==F)]# in 2000 but not in 2006 

IFA_2000_final$Cereals<-IFA_2000_final$Barley+IFA_2000_final$Millet+IFA_2000_final$Rye+IFA_2000_final$Sorghum
IFA_2000_final$Oil_crops<-IFA_2000_final$Groundnut+IFA_2000_final$Rapeseed+IFA_2000_final$Sunflower
IFA_2000_final$Roots<-IFA_2000_final$Cassava+IFA_2000_final$Potato

Fertilizer_2000<-IFA_2000_final[!(IFA_2000_final$Area %in% "China_(incl._Taiwan)"),c(1,18,12,7,15,9,20,21,16,22)]
colnames(Fertilizer_2000)<-c("Area","Wheat","Rice","Maize","soya","Oil_Palm","Cereals","Oil_crops","sugarcane","Roots")

################save table for a shortcut ###########################################################
#write.csv(Fertilizer_2000,file="Fertilizer_2000.csv")
Fertilizer_2000<-read.csv("Fertilizer_2000.csv",sep=",",header=T)[,-1]
################################################################################################################

################################################################################################################
### Missing countries (ROW) in Fertilizer_2000                                                              ####
################################################################################################################
# as for 2000 we do not have a ROW value, we have to calculate the fertilizer for the ROW countries by Area_H from the 
# value from 2006
counties_06[which(is.element(unique(Fertilizer_2006b$Area),unique(Fertilizer_2000$Area))==F)]# in 2006 but not in 2000
missing_2000<-c("Cyprus", "Luxembourg", "Malta", "Romania", "Slovenia", "Iran_(Islamic_Republic_of)", "Russian_Federation",
       "Afghanistan", "American_Samoa","Angola", "Antigua_and_Barbuda", "Armenia", "Bahamas","Bahrain","Barbados",
       "Belize","Benin","Bermuda","Bhutan", "Bosnia_and_Herzegovina","Botswana", "Brunei_Darussalam","Burkina_Faso",
       "Burundi","Cabo_Verde","Cameroon","Cayman_Islands","Central_African_Republic","Chad","Comoros","Congo",
       "Cook_Islands","Côte_d'Ivoire","Cuba","Democratic_People's_Republic_of_Korea", "Democratic_Republic_of_the_Congo",
       "Djibouti","Dominica","Equatorial_Guinea","Eritrea","Faroe_Islands","French_Guiana","French_Polynesia","Gabon",
       "Gambia","Georgia","Ghana","Grenada","Guadeloupe","Guam","Guinea-Bissau","Guyana","Haiti","Iceland","Iraq",
       "Jamaica","Kazakhstan","Kyrgyzstan", "Lesotho","Liberia","Libya","Maldives","Mali","Martinique","Mauritius",
       "Micronesia_(Federated_States_of)","Mongolia","Montenegro","Montserrat","Mozambique", "Namibia","Nepal",
       "New_Caledonia","Niger","Niue","Occupied_Palestinian_Territory","Oman", "Panama","Papua_New_Guinea","Peru",
       "Puerto_Rico","Qatar","Réunion","Rwanda","Saint_Kitts_and_Nevis","Saint_Lucia","Saint_Vincent_and_the_Grenadines",
       "Samoa","Sao_Tome_and_Principe","Senegal","Serbia","Seychelles","Sierra_Leone","Solomon_Islands","Somalia","South_Sudan",
       "Sudan","Suriname","Swaziland","Tajikistan","The_former_Yugoslav_Republic_of_Macedonia","Timor-Leste","Tonga",
       "Trinidad_and_Tobago","Tunisia","Turkmenistan","Uganda","Ukraine","United_Arab_Emirates","Uzbekistan","Vanuatu",
       "Wallis_and_Futuna_Islands","Western_Sahara","Yemen")

# load data
Fertilizer_2006b<-read.csv("Fertilizer_2006b.csv",sep=",",header=T)[,-1]
crops_16<-read.csv("crops_16.csv",sep=",",header=T)[,-1]
crops_16$Area<-paste(gsub("[ ]","_",crops_16$Area))

# select all countries missing in 2000 fertilizer data
crops_16_mis<-crops_16[which(is.element(crops_16$Area,missing_2000)),c(1:4)]
Fert_06_mis<-Fertilizer_2006b[which(is.element(Fertilizer_2006b$Area,missing_2000)),]

# we have to seperate each group seperately

# Fert_2000= Fert_2006*Area_H_2000/Area_H_2006 -> for ROW
### Wheat ###
Wheat<-c("Wheat","Bran, wheat","Bulgur","Flour, wheat")
# table with all crops belonging to this group 
crops_WT<-crops_16_mis[which(is.element(crops_16_mis$Item,Wheat)),]
# only year 2006
crops_WT_06<-crops_WT[which(crops_WT$Year=="2006"),]
# aggregate Area harvested for the group
crops_WT_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_WT_06,sum)
colnames(crops_WT_06_agg)[colnames(crops_WT_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

# only 2000
crops_WT_00<-crops_WT[which(crops_WT$Year=="2000"),]
# aggregate Area harvested for the group
crops_WT_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_WT_00,sum)
colnames(crops_WT_00_agg)[colnames(crops_WT_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

# combine the two tables, in order to have the area harvested of this group for 2000 and 2006
crops_WT_2 <- merge(crops_WT_00_agg,crops_WT_06_agg, by="Area",all=T)
crops_WT_2$N_t_group<-NA

# integrate fertilizer data
code_area_fert06<-unique(Fert_06_mis$Area)
for(i in 1:length(crops_WT_2[,1])){
  if(is.element(crops_WT_2$Area[i],code_area_fert06)){
    crops_WT_2$N_t_group[i]<-Fert_06_mis$Wheat[which(Fert_06_mis$Area==
                                                             crops_WT_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_WT_2$N_t_Wheat<-NA
crops_WT_2$N_t_Wheat<-crops_WT_2$N_t_group*crops_WT_2$Area_H_00/crops_WT_2$Area_H_06

##  new table ## fill this with all groups #################################################################
Fert2000_missing<-crops_WT_2[,c(1,5)]
###
### Rice ###
Rice<-c("Rice, paddy","Oil, rice bran","Cake, rice bran")

crops_RI<-crops_16_mis[which(is.element(crops_16_mis$Item,Rice)),]

crops_RI_06<-crops_RI[which(crops_RI$Year=="2006"),]
crops_RI_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_RI_06,sum)
colnames(crops_RI_06_agg)[colnames(crops_RI_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_RI_00<-crops_RI[which(crops_RI$Year=="2000"),]
crops_RI_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_RI_00,sum)
colnames(crops_RI_00_agg)[colnames(crops_RI_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_RI_2 <- merge(crops_RI_00_agg,crops_RI_06_agg, by="Area",all=T)
crops_RI_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_RI_2[,1])){
  if(is.element(crops_RI_2$Area[i],code_area_fert06)){
    crops_RI_2$N_t_group[i]<-Fert_06_mis$Rice[which(Fert_06_mis$Area==
                                                        crops_RI_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_RI_2$N_t_Rice<-NA
crops_RI_2$N_t_Rice<-crops_RI_2$N_t_group*crops_RI_2$Area_H_00/crops_RI_2$Area_H_06
crops_RI_3<-crops_RI_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_RI_3, by="Area",all=T)

######################### ###############
### Maize ###
Maize<-c("Maize","Maize, green","Bran, maize","Cake, maize","Flour, maize","Germ, maize","Oil, maize","Popcorn",
         "Sweet corn frozen","Sweet corn prep or preserved")

crops_MI<-crops_16_mis[which(is.element(crops_16_mis$Item,Maize)),]

crops_MI_06<-crops_MI[which(crops_MI$Year=="2006"),]
crops_MI_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_MI_06,sum)
colnames(crops_MI_06_agg)[colnames(crops_MI_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_MI_00<-crops_MI[which(crops_MI$Year=="2000"),]
crops_MI_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_MI_00,sum)
colnames(crops_MI_00_agg)[colnames(crops_MI_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_MI_2 <- merge(crops_MI_00_agg,crops_MI_06_agg, by="Area",all=T)
crops_MI_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_MI_2[,1])){
  if(is.element(crops_MI_2$Area[i],code_area_fert06)){
    crops_MI_2$N_t_group[i]<-Fert_06_mis$Maize[which(Fert_06_mis$Area==
                                                       crops_MI_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_MI_2$N_t_Maize<-NA
crops_MI_2$N_t_Maize<-crops_MI_2$N_t_group*crops_MI_2$Area_H_00/crops_MI_2$Area_H_06
crops_MI_3<-crops_MI_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_MI_3, by="Area",all=T)

########################################
### soya ###
soya<-c("Soybeans","Cake, soybeans","Oil, soybean")

crops_SO<-crops_16_mis[which(is.element(crops_16_mis$Item,soya)),]

crops_SO_06<-crops_SO[which(crops_SO$Year=="2006"),]
crops_SO_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_SO_06,sum)
colnames(crops_SO_06_agg)[colnames(crops_SO_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_SO_00<-crops_SO[which(crops_SO$Year=="2000"),]
crops_SO_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_SO_00,sum)
colnames(crops_SO_00_agg)[colnames(crops_SO_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_SO_2 <- merge(crops_SO_00_agg,crops_SO_06_agg, by="Area",all=T)
crops_SO_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_SO_2[,1])){
  if(is.element(crops_SO_2$Area[i],code_area_fert06)){
    crops_SO_2$N_t_group[i]<-Fert_06_mis$soya[which(Fert_06_mis$Area==
                                                        crops_SO_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_SO_2$N_t_soya<-NA
crops_SO_2$N_t_soya<-crops_SO_2$N_t_group*crops_SO_2$Area_H_00/crops_SO_2$Area_H_06
crops_SO_3<-crops_SO_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_SO_3, by="Area",all=T)

######################### ###############
### Oil_palm ###
palm<-c("Oil palm fruit","Oil, palm","Palm kernels","Oil, palm kernel","Cake, palm kernel")

crops_PA<-crops_16_mis[which(is.element(crops_16_mis$Item,palm)),]

crops_PA_06<-crops_PA[which(crops_PA$Year=="2006"),]
crops_PA_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_PA_06,sum)
colnames(crops_PA_06_agg)[colnames(crops_PA_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_PA_00<-crops_PA[which(crops_PA$Year=="2000"),]
crops_PA_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_PA_00,sum)
colnames(crops_PA_00_agg)[colnames(crops_PA_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_PA_2 <- merge(crops_PA_00_agg,crops_PA_06_agg, by="Area",all=T)
crops_PA_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_PA_2[,1])){
  if(is.element(crops_PA_2$Area[i],code_area_fert06)){
    crops_PA_2$N_t_group[i]<-Fert_06_mis$Oil_Palm[which(Fert_06_mis$Area==
                                                       crops_PA_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_PA_2$N_t_palm<-NA
crops_PA_2$N_t_palm<-crops_PA_2$N_t_group*crops_PA_2$Area_H_00/crops_PA_2$Area_H_06
crops_PA_3<-crops_PA_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_PA_3, by="Area",all=T)

######################### ###############
### Cereals ###
Cereals<-c("Barley","Barley, pearled","Sorghum","Bran, sorghum","Rye","Millet","Bran, millet")

crops_CE<-crops_16_mis[which(is.element(crops_16_mis$Item,Cereals)),]

crops_CE_06<-crops_CE[which(crops_CE$Year=="2006"),]
crops_CE_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_CE_06,sum)
colnames(crops_CE_06_agg)[colnames(crops_CE_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_CE_00<-crops_CE[which(crops_CE$Year=="2000"),]
crops_CE_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_CE_00,sum)
colnames(crops_CE_00_agg)[colnames(crops_CE_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_CE_2 <- merge(crops_CE_00_agg,crops_CE_06_agg, by="Area",all=T)
crops_CE_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_CE_2[,1])){
  if(is.element(crops_CE_2$Area[i],code_area_fert06)){
    crops_CE_2$N_t_group[i]<-Fert_06_mis$Cereals[which(Fert_06_mis$Area==
                                                           crops_CE_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_CE_2$N_t_Cereals<-NA
crops_CE_2$N_t_Cereals<-crops_CE_2$N_t_group*crops_CE_2$Area_H_00/crops_CE_2$Area_H_06
crops_CE_3<-crops_CE_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_CE_3, by="Area",all=T)

######################### ###############
### Oil_crops ###
Oil<-c("Rapeseed","Cake, rapeseed","Oil, rapeseed","Sunflower seed","Cake, sunflower", "Oil, sunflower", "Groundnuts, with shell",
       "Groundnuts, shelled","Peanut butter","Cake, groundnuts","Oil, groundnut")

crops_OI<-crops_16_mis[which(is.element(crops_16_mis$Item,Oil)),]

crops_OI_06<-crops_OI[which(crops_OI$Year=="2006"),]
crops_OI_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_OI_06,sum)
colnames(crops_OI_06_agg)[colnames(crops_OI_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_OI_00<-crops_OI[which(crops_OI$Year=="2000"),]
crops_OI_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_OI_00,sum)
colnames(crops_OI_00_agg)[colnames(crops_OI_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_OI_2 <- merge(crops_OI_00_agg,crops_OI_06_agg, by="Area",all=T)
crops_OI_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_OI_2[,1])){
  if(is.element(crops_OI_2$Area[i],code_area_fert06)){
    crops_OI_2$N_t_group[i]<-Fert_06_mis$Oil_crops[which(Fert_06_mis$Area==
                                                          crops_OI_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_OI_2$N_t_oil_crops<-NA
crops_OI_2$N_t_oil_crops<-crops_OI_2$N_t_group*crops_OI_2$Area_H_00/crops_OI_2$Area_H_06
crops_OI_3<-crops_OI_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_OI_3, by="Area",all=T)

######################### ###############
### sugarcane ###
sugarcane<-c("Sugar cane","Cane tops","Molasses")

crops_SC<-crops_16_mis[which(is.element(crops_16_mis$Item,sugarcane)),]

crops_SC_06<-crops_SC[which(crops_SC$Year=="2006"),]
crops_SC_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_SC_06,sum)
colnames(crops_SC_06_agg)[colnames(crops_SC_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_SC_00<-crops_SC[which(crops_SC$Year=="2000"),]
crops_SC_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_SC_00,sum)
colnames(crops_SC_00_agg)[colnames(crops_SC_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_SC_2 <- merge(crops_SC_00_agg,crops_SC_06_agg, by="Area",all=T)
crops_SC_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_SC_2[,1])){
  if(is.element(crops_SC_2$Area[i],code_area_fert06)){
    crops_SC_2$N_t_group[i]<-Fert_06_mis$sugarcane[which(Fert_06_mis$Area==
                                                            crops_SC_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_SC_2$N_t_sugarcane<-NA
crops_SC_2$N_t_sugarcane<-crops_SC_2$N_t_group*crops_SC_2$Area_H_00/crops_SC_2$Area_H_06
crops_SC_3<-crops_SC_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_SC_3, by="Area",all=T)

######################### ###############
### Roots ###
Roots<-c("Cassava","Cassava leaves","Cassava dried","Starch, cassava","Potatoes",
         "Flour, potatoes","Potatoes, frozen","Potato offals")

crops_RO<-crops_16_mis[which(is.element(crops_16_mis$Item,Roots)),]

crops_RO_06<-crops_RO[which(crops_RO$Year=="2006"),]
crops_RO_06_agg<-aggregate(Area_harvested_ha~Area, data=crops_RO_06,sum)
colnames(crops_RO_06_agg)[colnames(crops_RO_06_agg)=="Area_harvested_ha"] <- "Area_H_06"

crops_RO_00<-crops_RO[which(crops_RO$Year=="2000"),]
crops_RO_00_agg<-aggregate(Area_harvested_ha~Area, data=crops_RO_00,sum)
colnames(crops_RO_00_agg)[colnames(crops_RO_00_agg)=="Area_harvested_ha"] <- "Area_H_00"

crops_RO_2 <- merge(crops_RO_00_agg,crops_RO_06_agg, by="Area",all=T)
crops_RO_2$N_t_group<-NA

# integrate fertilizer data
for(i in 1:length(crops_RO_2[,1])){
  if(is.element(crops_RO_2$Area[i],code_area_fert06)){
    crops_RO_2$N_t_group[i]<-Fert_06_mis$Roots[which(Fert_06_mis$Area==
                                                            crops_RO_2$Area[i])]}
  else{}}

# Fertilizer one crop = Fertilizer group* AreaH one crop / AreaH group
crops_RO_2$N_t_Roots<-NA
crops_RO_2$N_t_Roots<-crops_RO_2$N_t_group*crops_RO_2$Area_H_00/crops_RO_2$Area_H_06
crops_RO_3<-crops_RO_2[,c(1,5)]

### integrate in Fert2000_missing
Fert2000_missing<-merge(Fert2000_missing,crops_RO_3, by="Area",all=T)

################save the acual tabel for a short cut ###########################################################
#write.csv(Fert2000_missing,file="Fert2000_missing.csv")
Fert2000_missing<-read.csv("Fert2000_missing.csv",sep=",",header=T)[,-1]
################################################################################################################
colnames(Fertilizer_2000)<-c("Area","Wheat","Rice","Maize","soya","Oil_Palm","Cereals","Oil_crops","sugarcane","Roots")
colnames(Fert2000_missing)<-c("Area","Wheat","Rice","Maize","soya","Oil_Palm","Cereals","Oil_crops","sugarcane","Roots")

# combine the Fertilizer_2000 with the new ROW table
Fertilizer_2000a<-rbind(Fertilizer_2000,Fert2000_missing)

################save the acual tabel for a short cut ###########################################################
#write.csv(Fertilizer_2000a,file="Fertilizer_2000a.csv")
Fertilizer_2000a<-read.csv("Fertilizer_2000a.csv",sep=",",header=T)[,-1]
################################################################################################################

######################################################################################################
######## Fertilizer production in energy terms (not used in final analysis)  #########################
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
              "Micronesia_(Federated_States_of)","Niue","Republic_of_Korea","Timor-Leste","Tonga",
              "Wallis_and_Futuna_Islands")           

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

# aggregate fertilizer used  per country
Fertilizer_2000b<-Fertilizer_2000a
Fertilizer_2000b[is.na(Fertilizer_2000b)] <- 0
#sum of fertilizer used for all crops
Fertilizer_2000b$sum_t<-NA
for(i in 1:length(Fertilizer_2000b[,1])){
  Fertilizer_2000b$sum_t[i]<-sum(Fertilizer_2000b[i,2:10])}

# integrate the region code
Fertilizer_2000b$Region<-NA
for(i in 1:length(Fertilizer_2000b[,1])){
  if(is.element(Fertilizer_2000b$Area[i],Western_Europe)){
    Fertilizer_2000b$Region[i]<-"Western_Europe"} 
  if(is.element(Fertilizer_2000b$Area[i],Central_European_countries)){
    Fertilizer_2000b$Region[i]<-"Central_European_countries"}
  if(is.element(Fertilizer_2000b$Area[i],North_America)){
    Fertilizer_2000b$Region[i]<-"North_America"} 
  if(is.element(Fertilizer_2000b$Area[i],CIS)){
    Fertilizer_2000b$Region[i]<-"CIS"}
  if(is.element(Fertilizer_2000b$Area[i],China_)){
    Fertilizer_2000b$Region[i]<-"China_"}
  if(is.element(Fertilizer_2000b$Area[i],India)){
    Fertilizer_2000b$Region[i]<-"India"}
  if(is.element(Fertilizer_2000b$Area[i],Other_Asia)){
    Fertilizer_2000b$Region[i]<-"Other_Asia"}
  if(is.element(Fertilizer_2000b$Area[i],Latin_America)){
    Fertilizer_2000b$Region[i]<-"Latin_America"}
  if(is.element(Fertilizer_2000b$Area[i],Africa)){
    Fertilizer_2000b$Region[i]<-"Africa"}
  if(is.element(Fertilizer_2000b$Area[i],Middle_East)){
    Fertilizer_2000b$Region[i]<-"Middle_East"}
  if(is.element(Fertilizer_2000b$Area[i],Oceania)){
    Fertilizer_2000b$Region[i]<-"Oceania"}
  if(is.element(Fertilizer_2000b$Area[i],Japan)){
    Fertilizer_2000b$Region[i]<-"Japan"}}

# integrate the energy use per tonne  
Fertilizer_2000b$kJ_t_N<-NA
for(i in 1:length(Fertilizer_2000b[,1])){
  if(is.element(Fertilizer_2000b$Area[i],Western_Europe)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]} 
  if(is.element(Fertilizer_2000b$Area[i],Central_European_countries)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],North_America)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]} 
  if(is.element(Fertilizer_2000b$Area[i],CIS)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],China_)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],India)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],Other_Asia)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],Latin_America)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],Africa)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],Middle_East)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],Oceania)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}
  if(is.element(Fertilizer_2000b$Area[i],Japan)){
    Fertilizer_2000b$kJ_t_N[i]<-energy_fertelizer$kJ_t_N[which(energy_fertelizer$Region==Fertilizer_2000b$Region[i])]}}

# multiply fertilizer use with energy for fertilizer production 
Fertilizer_2000b$N_total_kj<-NA
Fertilizer_2000b$N_total_kj<-Fertilizer_2000b$sum_t*Fertilizer_2000b$kJ_t_N

################save table for a short cut ###########################################################
#write.csv(Fertilizer_2000b,file="Fertilizer_2000b.csv")
Fertilizer_2000b<-read.csv("Fertilizer_2000b.csv",sep=",",header=T)[,-1]
################################################################################################################
