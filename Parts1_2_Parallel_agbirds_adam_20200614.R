#This is all the functions and code within another function for parallel processing. Merging into VRTS is not included.

##################SETTING UP FOR PARALLEL
run_parallel<-function(state_order){
  library(raster)
  library(sp) #not sure needed yet
  library(foreign) #to import dbf
  library(dplyr) #for the pipes
  library(cdlTools) #to get cdl data. It's easier than cropping the national dataset (bc it's an .img file and causes crop/mask problem)
  ########################################################
  ############################DATA INPUTS
  ########################################################
  
  #source("/nfs/agbirds-data/AdamAprilAnalysis/agbirds_adam_functions_20200420.R")
  states_list<-c("Texas","Ohio","Tennessee","Wisconsin","North Dakota","Montana","Kentucky","Michigan","Minnesota","Colorado","New Mexico","Wyoming","Arkansas","Iowa","Kansas","South Dakota") #,"Missouri","Nebraska","Oklahoma",,"Louisiana","Alabama","Mississippi","Illinois","Indiana")
  

  
  ##,
  #crops to get
  crops_list<-c("Corn", "Soybeans","Winter Wheat")
  crops_id<-c(1,5,24) #24, ##MAKE sure and get the ID right, get from cdl attribute table

  #use attribute table to look up state abbreviation
  states<-shapefile("C:\\AdamDeleteAfterJuly2020\\cb_2016_us_state_500k.shp")
  
  #load original cropstate table
  crop_table<-read.csv("G:\\My Drive\\SESYNC_Agbirds_Processing\\inputs\\Crop_Data_modified_20200425.csv")
  
  outputs_folder<-"G:\\My Drive\\SESYNC_Agbirds_Processing\\outputs\\"
  
  ############################
  ############################END OF DATA INPUTS
  ############################
  ########################################################
  ########################START OF FUNCTIONS
  ########################################################
  
  ############################FUNCTION 1
  #crop state from national cdl, reclassify to output crop desired as 1 or 0 for everthing else
  getcdl<-function(state){
    #this downloads CDL for state, needs lowercase state name for lookup. It places it in a list object, so subset it with brackets to access
    cdl_download<-getCDL(tolower(state),2019)#, location="/nfs/agbirds-data/June2020/inputs/cdl")
    state_cdl_values2<-unique(cdl_download[[1]])
    reclass_list<-c()
    for (i in state_cdl_values2) {
      if(i %in% crops_id[t]){
        reclass_value<-1
      } else {
        reclass_value<-0
      }
      reclass_list<-c(reclass_list, reclass_value)
    }
    #reclass doesn't accept NA for some reason   
    #put matrix together
    reclass_matrix<-matrix(cbind(state_cdl_values2,reclass_list), nrow= length(state_cdl_values2), ncol = 2)
    #Turn NA value of 0 officially to NA
    reclass_matrix<-rbind(c(0, NA), reclass_matrix[2:length(state_cdl_values2),])
    #crop state of interest, export as 1 bit
    crp_rcls<-reclassify(cdl_download[[1]], rcl = reclass_matrix, format = "GTiff", datatype = "INT1U")
    plot(crp_rcls)
    #NAvalue(crp_rcls)<-0
    return(crp_rcls)
  }
  
  #Working now to generate 52 weeks of crop state from orig data table
  #Values are
  #1 - bare or till
  #2 - planting
  #3 - growing
  #4 - harvest
  #In PLANTING, change all values to either 0 or 2, 0 for bare/til, 2 for most farmers planting
  
  ############################FUNCTION 2
  #extract values from crop calendar table. Change to above coding scheme. Output list with 52 codes
  cropstate_value<-function(get_rows){
    planting_recode<-get_rows%>%
      filter(Plant_Harvest == "Planting")
    
    planting_recode[planting_recode == 1] <- 0
    
    #In HARVETING, change all values to 0 or 4, 4 for most farmers harvesting
    
    harvesting_recode<-get_rows%>%
      filter(Plant_Harvest == "Harvesting")
    
    harvesting_recode[harvesting_recode == 1] <- 0
    harvesting_recode[harvesting_recode == 2] <- 4
    
    #combine both rows, the largest value being the value that takes the place
    #transpose because it was hard to find a way to operate on two rows
    both<-rbind(planting_recode,harvesting_recode)%>%
      select(X1:X52) %>%
      t()
    #both<-both[4:55,]
    #this combines the max value of both rows
    both$comb<-apply(both,1,max)
    #View(both)
    #grab just the combined column
    combined_plant_harvest<-t(both$comb) #removed as.data.frame bc I think it was causing "level" problem
    
    
    #do a while statement for the first set of zero until planting. while value is 0 set to 1
    first_set<-c()
    for (i in 1:52){
      if (combined_plant_harvest[[i]] == 0) {
        combined_plant_harvest[[i]]<-1
        first_set<-c(first_set,combined_plant_harvest[[i]])
      }
      else{
        break; #This breaks after the first set of zeros are changed to 1
      }
    }
    
    #do a while statement going backwards in the list, while value is 0 set to 1
    last_set<-c()
    for (i in 52:1){
      if (combined_plant_harvest[[i]] == 0) {
        combined_plant_harvest[[i]]<-1
        last_set<-c(last_set,combined_plant_harvest[[i]])
      }
      else{
        break;
      }
    }
    
    
    #Then if the value is 0 still, set to 3 for growing
    for (i in 1:52){
      if (combined_plant_harvest[[i]] == 0) {
        combined_plant_harvest[[i]]<-3
      }
    }
    return(as.vector(combined_plant_harvest))
  }
  
  ############################FUNCTION 3  
  #export 52 rasters, change value of crop from 1 to 4 using results from cropstate_value
  #!!!!set output directory!!!!
  #outdir<-"/nfs/agbirds-data/AdamAprilAnalysis/outputs"
  
  
  create_52weeks_cropstate<-function(ras, weeks) {
    for (i in 1:52){
      rcls_mat<-matrix(c(1,weeks[[i]]), nrow= 1, ncol = 2)
      #crop state of interest, export as 1 bit
      #to make sure there are no spaces
      searchString <- ' '
      replacementString <- ''
      crp_rcls<-reclassify(ras, rcl = rcls_mat, format = "GTiff", datatype = "INT1U", filename = paste0(outputs_folder,gsub(searchString,replacementString,state),"_",cropname,"_week_",i))
    }
  }
  ###########################
  ###########################END OF FUNCTIONS
  ###########################
  
  
  
  ###########################
  ###########################EXECUTING FUNCTIONS
  ########################### 
  
  
  #changing to 2:2 to only do soybeans
  for(t in 1:length(crops_list)) {#MAKE SURE TO CHANGE THIS BASED ON HOW MANY CROPS BEING PROCESSED
    state<-states_list[state_order]
    searchString <- ' '
    replacementString <- ''
    cropname<-gsub(searchString,replacementString,crops_list[t]) #"WinterWheat" #needsto be one word for final output file
    crop_id_to_process<-crops_id[t]
    #To get just the Spring Wheat
    get_rows<-crop_table%>%
      filter(State == states_list[state_order], Crop == crops_list[t])
    print(get_rows)
    ##Run the two functions made so far
    print(states_list[state_order])
    #workaround for winter wheat since it is planted in fall, grows through winter and is harvested in summer/early fall
    cs<-if (crop_id_to_process <= 5) {
      cropstate_value(get_rows)
    } else {
      #for winter wheat
      recode(cropstate_value(get_rows), `1` = 3L, `2` = 2L, `3` = 1L,  `4` = 4L)
    }
    #crop and mask the state
    crop_n_state_of_interest<-getcdl(state)
    create_52weeks_cropstate(crop_n_state_of_interest, cs)
  }
  
}

###########PARALLEL
library(parallel)

#Setting to use as many cores as states running. Each state should process at one core.

ncores<-detectCores(logical = F)
cl<-makeCluster(ncores-1)
state_order<-as.list(seq(1,16,1)) 
clusterApply(cl, state_order, run_parallel)
stopCluster(cl)
######################################
######################################
#to run all the states even with parallel took 9.5 hours

