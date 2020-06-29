

library(dplyr)

table<-read.csv("/Volumes/GoogleDrive/My Drive/SESYNC_Agbirds_Processing/inputs/Crop_Data_modified_20200425.csv")

crop_list=c("Corn", "Soybeans", "Winter Wheat")

crop_calendar<-filter(table, Crop %in% crop_list)

states_list<-c("Ohio","Tennessee","Wisconsin","North Dakota","Montana","Colorado","New Mexico","Wyoming","Arkansas","Iowa","Kansas","Missouri","Nebraska","Oklahoma","South Dakota","Louisiana","Texas","Alabama","Mississippi","Illinois","Indiana","Kentucky","Michigan","Minnesota")



###PLANTING
planting_not_WW<-filter(crop_calendar, Plant_Harvest == "Planting", Crop != "Winter Wheat", State %in% states_list)
nrow(planting_not_WW)
ncol(planting_not_WW)


#GOING BACKWARDS THRROUGH THE WEEKS, SET ALL VALUES OF 0 to 3, for GROWING

#NOT DOING WINTER WHEAT BECAUSE IT GROWS THROUGH WINTER
for (i in 1:48){
  for (t in 55:4){
    if (planting_not_WW[i,t] == 0) {
      #print(planting_not_WW[t,i])
      planting_not_WW[i,t]<-3
    }
    else{
      #print(planting_not_WW[t,i])
      break;
    }
  }
}


#####HARVESTING
harvesting_not_WW<-filter(crop_calendar, Plant_Harvest == "Harvesting", Crop != "Winter Wheat", State %in% states_list)
nrow(harvesting_not_WW)
ncol(harvesting_not_WW)

for (i in 1:48){
  for (t in 4:55){
    if (harvesting_not_WW[i,t] == 0) {
 
      harvesting_not_WW[i,t]<-3
    }
    else{
      #print(planting_not_WW[t,i])
      break;
    }
  }
}


for (i in 1:48){
  for (t in 4:55){
    if (harvesting_not_WW[i,t] == 1) {
      harvesting_not_WW[i,t]<-4
    }
    if (harvesting_not_WW[i,t] == 2){
      harvesting_not_WW[i,t]<-5
     }
  }
}

nrow(harvesting_not_WW)
nrow(planting_not_WW)
rbind(harvesting_not_WW, planting_not_WW)


newtable<-data.frame()
for (i in 1:48) {
  count<-0
  for (t in 4:55){
    if (harvesting_not_WW[i,t] == 3){
      count<- count + 1
    }
    if (harvesting_not_WW[i,t] == 4){
      break;
    }
  }
    a<-planting_not_WW[i,1:count]
    b<-harvesting_not_WW[i,(56-(55-count)):55] #using 56 to start one up from the position
    c<-cbind(a,b)
    newtable<-rbind(newtable, c)
    print(paste(as.character(harvesting_not_WW[i,1]),as.character(harvesting_not_WW[i,2])))
  }

nrow(newtable)
newtable


################################################################################################################################################################
################################################################################################################################################################################################
################################################################################################################################################################
################################################################
################################################################
################################################################
################################################################WINTER WHEAT

###PLANTING
planting_WW<-filter(crop_calendar, Plant_Harvest == "Planting", Crop == "Winter Wheat", State %in% states_list)
nrow(planting_WW)
ncol(planting_WW)

#PLANTING
for (i in 1:24){
  for (t in 55:4){
    if (planting_WW[i,t] == 0) {
      planting_WW[i,t]<-3
    }
    else{
      break;
    }
  }
}


#####HARVESTING
harvesting_WW<-filter(crop_calendar, Plant_Harvest == "Harvesting", Crop == "Winter Wheat", State %in% states_list)
nrow(harvesting_WW)
ncol(harvesting_WW)


#SWITCH VALUES OF 0 UNTIL HARVEST TIME TO 3
for (i in 1:24){
  for (t in 4:55){
    if (harvesting_WW[i,t] == 0) {
      
      harvesting_WW[i,t]<-3
    }
    else{
      #print(planting_not_WW[t,i])
      break;
    }
  }
}

#SWITCH START AND MOST ACTIVE VALUES TO 4 AND 5
for (i in 1:24){
  for (t in 4:55){
    if (harvesting_WW[i,t] == 1) {
      harvesting_WW[i,t]<-4
    }
    if (harvesting_WW[i,t] == 2){
      harvesting_WW[i,t]<-5
    }
  }
}

nrow(harvesting_WW)
nrow(planting_WW)
rbind(harvesting_WW, planting_WW)


##STITCH TWO TABLES TOGETHER

WW_newtable<-data.frame()
for (i in 1:24) {
  count<-0
  for (t in 4:55){
    if (planting_WW[i,t] == 0){
      count<- count + 1
    }
    if (planting_WW[i,t] == 1 | planting_WW[i,t] == 2){
      break;
    }
  }
  a<-planting_WW[i,(56-(55-count)):55]
  b<-harvesting_WW[i,1:count] #using 56 to start one up from the position
  c<-cbind(b, a)
  WW_newtable<-rbind(WW_newtable, c)
  print(paste(as.character(harvesting_WW[i,1]),as.character(harvesting_WW[i,2])))
}


ThreeCropsCal<-rbind(newtable, WW_newtable)

ThreeCropsCal$Plant_Harvest<-"FullCropCal"

write.csv(ThreeCropsCal, "/Volumes/GoogleDrive/My Drive/SESYNC_Agbirds_Processing/inputs/Full_Crop_Calendar_3Crops_20200621.csv")
