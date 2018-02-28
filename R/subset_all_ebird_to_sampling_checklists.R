### R script for loading all R data files of all eBird data and subsetting
### to just checklists for assigning 'urban values'
subset_ebird_data <- function () {
  ### packages
  library(dplyr)
  
  ### setwd
  setwd("Data/eBird data/All eBird data split")
  
  ### create a list of all filenames in the folder of data
  fileNames <- Sys.glob("*.RData")
  
  ### create a matrix for each file
  for (fileName in fileNames) {
    
    load(fileName)
    
  }
  
  ### combine to one file
  all_eBird_data <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9)
  
  ### clean up column names
  colnames(all_eBird_data) <- gsub(" ", "_", colnames(all_eBird_data))
  
  ### subset to unique checklists (SAMPLING_EVENT_IDENTIFIER)
  sampling_checklists <- all_eBird_data %>%
    dplyr::select(SAMPLING_EVENT_IDENTIFIER, LATITUDE, LONGITUDE, 
                  EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVATION_DATE) %>%
    distinct(SAMPLING_EVENT_IDENTIFIER, .keep_all=TRUE)
  
  ### save sampling data as a RData file for matching with landsat stuff
  save(sampling_checklists, file="Data/eBird data/All eBird checklist data/sampling_checklists.RData")
}
