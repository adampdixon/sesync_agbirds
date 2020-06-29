#This code is to generate all merged states for each crop and week
#Part 3 of the code process. There are no external functions.

######Runs faster in parallel maybe?

parallel_vrt<-function(week){
  library(gdalUtils)
  crop<-"WinterWheat" #char vector to search in folder for approp. files and to put in output file name. Note WinterWheat with no space in name and capital firt letters.
  #Full file path
  cropstates<-list.files("G:\\My Drive\\SESYNC_Agbirds_Processing\\outputs", full.names = T)
  #Corn, Soybeans, or WinterWheat
  states_at_week<-grep(paste0(crop,"_week_",week,"\\.tif$"),cropstates,value=TRUE, perl=TRUE)#set input folder location here
  #Just file name for naming output
  cropstates_name<-list.files("G:\\My Drive\\SESYNC_Agbirds_Processing\\outputs", full.names = F)
  states_at_week_name<-grep(paste0(crop,"_week_",week,"\\.tif$"),cropstates_name,value=TRUE, perl=TRUE)#set input folder location here
  #this builds a vector of the states with the week identified suitable for gdal input
  #The grep function searches the outputs folder and get all the "Corn_week" files. Change to Soybeans when ready for that.
  gdalbuildvrt(states_at_week, output.vrt = paste0("G:\\My Drive\\SESYNC_Agbirds_Processing\\outputs_vrt\\all_states_",crop,"_week_",week,".vrt")) #set output location here.
}

###########PARALLEL
library(parallel)

#Setting to use as many cores as states running. Each state should process at one core.

ncores<-detectCores(logical = F)
cl<-makeCluster(ncores-1)
week_order<-seq(1,52,1) #!!!!!!!!make sure and change seq based on number of states being analyzed!!!!!!!!
clusterApply(cl, week_order, parallel_vrt)
stopCluster(cl)
######################################
######################################

#see the output
library(raster)
crop_ras<-raster("/nfs/agbirds-data/June2020/outputs_vrt/all_states_soybeans_week_20.vrt")
plot(crop_ras)