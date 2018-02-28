## R script to read in Australia eBird data from my local machine and subset it and save as .RData file
## I didn't build this into the remake workflow, as we should only have to do this once
## Although, will do it twice, since the newest data dump will take place March 15th
## The raw text file is approximately 5 gb

split_ebird_data <- function () {
  ## packages
  library(readr)
  library(dplyr)
  
  ## Split the eBird dataframe into 6 different dataframes and save them as .RData files
  ## This makes it so it will be able to upload to github smoothly
  
  ## 1
  data <- read_delim("ebd_AU_relNov-2017.txt", delim="\t", quote="", col_names=TRUE, trim_ws=TRUE)
  
  
  ## split dataframe and keep dataframes in environment
  split <- split(data, (seq(nrow(data))-1) %/% 1500000)
  
  list2env(setNames(split,paste0("df",1:9)),environment())
  
  rm(split)
  
  save(df1, file=paste0("all_eBird_data1.RData"))
  save(df2, file=paste0("all_eBird_data2.RData"))
  save(df3, file=paste0("all_eBird_data3.RData"))
  save(df4, file=paste0("all_eBird_data4.RData"))
  save(df5, file=paste0("all_eBird_data5.RData"))
  save(df6, file=paste0("all_eBird_data6.RData"))
  save(df7, file=paste0("all_eBird_data7.RData"))
  save(df8, file=paste0("all_eBird_data8.RData"))
  save(df9, file=paste0("all_eBird_data9.RData"))
}
