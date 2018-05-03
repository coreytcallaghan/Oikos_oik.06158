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
  
  ## run phylo model once
  ## standardized version
  row.names(analysis_data) <- analysis_data$binom
  
  phy_mod_rescaled <- phylolm(response ~ rescale(body_size_logged) + rescale(clutch_size_logged) + 
                                rescale(feeding_habitat_generalism) + rescale(brain_residual) + 
                                rescale(Habitat_agricultural) + rescale(breeding_habitat_generalism) + 
                                rescale(granivore) + rescale(insectivore) + 
                                rescale(carrion_eater) + rescale(plant_eater) + rescale(diet_generalism) + 
                                rescale(migrate) + rescale(nomadic_irruptive) +
                                rescale(ground_nesting) + rescale(hollow_nesting) + 
                                rescale(nest_generalism) + rescale(breeding) + 
                                nest_aggregation + feeding_aggregation + 
                                rescale(Habitat_grass_shrubland) + rescale(range_size) +
                                rescale(Habitat_tree_forest), data=analysis_data, phy=aus_bird_tree,
                                na.action = "na.fail", full.matrix=FALSE,
                                weights=(analysis_data$N/analysis_data$unique_localities))
  
  
  library(snow)
  clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
  clust <- try(makeCluster(getOption("cl.cores", 10), type = clusterType))
  
  clusterExport(clust, c("analysis_data", "phylolm", "rescale", "aus_bird_tree"), envir = .GlobalEnv)
  
  model.set <- pdredge(phy_mod_rescaled, m.lim=c(0, 11), cluster=clust)
  
  saveRDS(model.set, file="Data/PHYLO_dredged_model_model.set.rds")
  
  #### selects all models with deltaAic < 4
  top.models <- get.models(model.set, subset=delta<4) 
  
  #### how many top models
  length(top.models)
  
  #### Ranks these models based on AICc
  my.models <- model.sel(top.models, rank="AICc") 
  
  Averaged_models <- model.avg(my.models)
  
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



