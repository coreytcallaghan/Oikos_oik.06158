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
  
  saveRDS(model.set, file="Data/dredged_model.rds")
  
}

build_dredged_model()
