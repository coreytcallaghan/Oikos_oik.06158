## This is an R script to dredge the model for the non
## phylo models and then save it as an object
## to save headaches with remake workflow
## build the dredged model and export it
## done outside of the remake workflow as 
## the environment issue was very difficult to navigate!

build_dredged_model <- function() {
  remake::dump_environment()
  
  library(snow)
  clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
  clust <- try(makeCluster(getOption("cl.cores", 10), type = clusterType))
  
  clusterExport(clust, "analysis_data", envir = environment())
  
  stdz.model <- standardize(global_model)
  
  model.set <- pdredge(global_model, m.lim=c(0, 11), cluster=clust, extra="R^2") 
  
  split <- split(model.set, (seq(nrow(model.set))-1) %/% 200000)
  
  list2env(setNames(split,paste0("df",1:4)),environment())
  
  rm(split)
  
  saveRDS(df1, file="Data/dredged_models_1.rds")
  saveRDS(df2, file="Data/dredged_models_2.rds")
  saveRDS(df3, file="Data/dredged_models_3.rds")
  saveRDS(df4, file="Data/dredged_models_4.rds")
}

build_dredged_model()
