## This is an R script to dredge the model for the
## phylo models and then save it as an object
## to save headaches with remake workflow
## build the dredged model and export it
## done outside of the remake workflow as 
## the environment issue was very difficult to navigate!

## arm package is really tricky and won't be recognized to load
## into the cluster export. A similar issue to what we had in the
## remake workflow. Here, I pulled the arm::rescale function out and put
## it into the environment and then load it onto the cluserexport function
rescale <- function (x, binary.inputs = "center") 
{
  if (!is.numeric(x)) {
    x <- as.numeric(factor(x))
    x.obs <- x[!is.na(x)]
  }
  x.obs <- x[!is.na(x)]
  if (length(unique(x.obs)) == 2) {
    if (binary.inputs == "0/1") {
      x <- (x - min(x.obs))/(max(x.obs) - min(x.obs))
      return(x)
    }
    else if (binary.inputs == "-0.5,0.5") {
      return(x - 0.5)
    }
    else if (binary.inputs == "center") {
      return(x - mean(x.obs))
    }
    else if (binary.inputs == "full") {
      return((x - mean(x.obs))/(2 * sd(x.obs)))
    }
  }
  else {
    return((x - mean(x.obs))/(2 * sd(x.obs)))
  }
}

build_dredged_model <- function() {
  remake::dump_environment()
  
  library(snow)
  clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
  clust <- try(makeCluster(getOption("cl.cores", 10), type = clusterType))
  
  clusterExport(clust, c("analysis_data", "phylolm", "rescale", "aus_bird_tree"), envir = .GlobalEnv)
  
  model.set <- pdredge(phy_mod_rescaled, m.lim=c(0, 11), cluster=clust)
  
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
  
  saveRDS(model_results, file="Data/PHYLO_dredged_model_averaged_param_est.rds")
  saveRDS(summary, file="Data/PHYLO_dredged_model_summary_results.rds")
  
}

build_dredged_model()



