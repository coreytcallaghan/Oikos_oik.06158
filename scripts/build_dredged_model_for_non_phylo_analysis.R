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
  
  #### selects all models with deltaAic < 4
  top.models <- get.models(model.set, subset=delta<4) 
  
  #### how many top models
  length(top.models)
  
  #### Ranks these models based on AICc
  my.models <- model.sel(top.models, rank="AICc") 
  
  Averaged_models <- model.avg(top.models)
  
  summary <- summary(Averaged_models)
  
  a <- as.data.frame(coefficients(Averaged_models, full=TRUE))
  b <- as.data.frame(confint(Averaged_models, full=TRUE))
  model_results <- cbind(a, b)
  names(model_results)[1] <- "estimate"
  names(model_results)[2] <- "lwr"
  names(model_results)[3] <- "upr"
  model_results$variable <- row.names(model_results)
  row.names(model_results) <- NULL
  
  saveRDS(model_results, file="Data/dredged_model_averaged_param_est.rds")
  saveRDS(summary, file="Data/dredged_model_summary_results.rds")

}

build_dredged_model()
