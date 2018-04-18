## Script with functions for the results of the
## Non-phylo models


## create df for the model fitting
matched_df <- function(response_variables, traits){
  
  response_variables$binom <- response_variables$SCIENTIFIC_NAME_tree
  
  analysis_data <- left_join(traits, response_variables, by="binom") %>%
    filter(SCIENTIFIC_NAME_tree != "not_treated_as_species")
  
  # clean data to final dataset
  analysis_data <- analysis_data %>%
    # drop any columns that we aren't interested in
    dplyr::select(-brain_size, -iucn_status) %>%
    # filter for only complete variables
    filter_all(all_vars(!is.na(.))) %>%
    # rename columns so that they are logged
    # necessary for the model selection step
    mutate(response = log(urban_median)) %>%
    mutate(body_size_logged = log(mean_body_size)) %>%
    mutate(clutch_size_logged = log(clutch_size))
  
  analysis_data
}

# make coorplot of continuous variables
corrplot_figure <- function(analysis_data){
  
  library(corrplot)
  
 continuous_df <- select_if(analysis_data, is.numeric) %>%
    dplyr::select(-urban_mean, -clutch_size, -mean_body_size, -urban_median, -response) %>%
    rename(`Brain residual` = brain_residual) %>%
    rename(`Feeding habitat generalism` = feeding_habitat_generalism) %>%
    rename(`Breeding habitat generalism` = breeding_habitat_generalism) %>%
    rename(`Diet generalism` = diet_generalism) %>%
    rename(`Nest generalism` = nest_generalism) %>%
    rename(`Number of records` = N) %>%
    rename(`Unique localitieis observed` = unique_localities) %>%
    rename(`log(Body size)` = body_size_logged) %>%
    rename(`log(Clutch size)` = clutch_size_logged) %>%
    cor(use="pairwise.complete.obs")
  
  pdf("figures/corrplot_of_continuous_variables.pdf")
  
  print(
  corrplot::corrplot(continuous_df)
  )
  
  dev.off()
}

## run one large model and standardize the model
## so that effect sizes are comparable

get_global_model <- function(analysis_data) {
  
  glob.mod <- lm(response ~ body_size_logged + clutch_size_logged + feeding_habitat_generalism + brain_residual + 
                   Habitat_agricultural + breeding_habitat_generalism + granivore + insectivore + 
                   carrion_eater + plant_eater + diet_generalism + movement_class +
                   ground_nesting + hollow_nesting + nest_generalism + breeding + 
                   nest_aggregation + feeding_aggregation + Habitat_grass_shrubland +
                   Habitat_tree_forest, data=analysis_data, 
                   na.action = "na.fail", weights=(analysis_data$N/analysis_data$unique_localities))
  
  glob.mod

}



## make table of variance inflation factor to export
collinearity_investigation_function <- function(global_model) {
  
  library(car)
  library(gridExtra)
  collinearity_investigation <- as.data.frame(vif(global_model))
  
  row.names(collinearity_investigation) <- c("log(Body size)", "log(Clutch size)", "Feeding habitat generalism",
                      "Brain residual", "Habitat - agricultural", "Breeding habitat generalism",
                      "Granivore", "Insectivore", "Carrion eater", "Plant eater", "Diet generalism",
                      "Migratory type", "Ground-nesting", "Hollow-nesting", "Nest generalism",
                      "Cooperative breeding", "Nest aggregation", "Feeding aggregation",
                      "Habitat - grass/shrubland", "Habitat - tree/forest")
  
  colnames(collinearity_investigation) <- c("Generalized VIF", "Degrees of \nfreedom", "Adjusted \nGeneralized VIF")
  
  is.num <- sapply(collinearity_investigation, is.numeric)
  collinearity_investigation[is.num] <- lapply(collinearity_investigation[is.num], round, 2)
  
  pdf("tables/collinearity_investigation.pdf")
  grid.table(collinearity_investigation)
  dev.off()
}



## plot parameter estimates of standardized global model

plot_params_globmod <- function(global_model) {
  
  library(sjPlot)
  library(forcats)
  
  pdf("figures/param_plot_global_model.pdf", height=11, width=9)
  
  print(
  get_model_data(global_model, type="std2") %>%
    arrange(estimate) %>%
    mutate(term2 = c("log(Clutch size)",
                     "Feeding habitat generalism", 
                     "Migratory type \n (partial migrant)",
                     "Migratory type \n (dispersal, partial migrant, & nomadic/irruptive",
                     "Diet generalism", 
                     "Breeding habitat generalism", 
                     "Migratory type \n (dispersal & nomadic/irruptive)",
                     "Migratory type \n (total migrant)", 
                     "Migratory type \n (none)", 
                     "Habitat - agricultural", 
                     "Migratory type \n (nomadic/irruptive)",
                     "log(Body size)", 
                     "Brain residual", 
                     "Migratory type \n (dispersal & partial migrant)",
                     "Nest generalism", 
                     "Feeding aggregation \n (solitary & flocks)",
                     "Feeding aggregation \n (solitary, pairs, & flocks)", 
                     "Plant eater", 
                     "Ground-nesting", 
                     "Migratory type \n (total migrant & nomadic/irruptive)", 
                     "Cooperative breeding",
                     "Hollow-nesting", 
                     "Feeding aggregation \n (solitary & pairs)", 
                     "Nest aggregation \n (solitary)",
                     "Habitat - tree/forest", 
                     "Nest aggregation \n (none)", 
                     "Nest aggregation \n (colonial)",
                     "Feeding aggregation \n (pairs)", 
                     "Granivore", 
                     "Carrion eater", 
                     "Feeding aggregation \n (pairs & flocks)", 
                     "Habitat - grass/shrubland", 
                     "Insectivore",
                     "Feeding aggregation \n (solitary)",
                     "Migratory type \n (partial migrant & nomadic/irruptive)")) %>%
    mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
    ggplot(., aes(x=fct_inorder(term2), y=estimate, color=trend))+
    geom_point()+
    geom_errorbar(aes(ymin=conf.low, ymax=conf.high, color=trend))+
    ylab("Parameter estimates")+
    xlab("")+
    coord_flip()+
    theme_classic()+
    guides(color=FALSE)+
    geom_hline(yintercept=0, color="black")
  )
  
  dev.off()
}


## do a model averaging approach using dredging
model_averaging <- function(analysis_data, global_model) {
  
  library(snow)
  clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
  clust <- try(makeCluster(getOption("cl.cores", 10), type = clusterType))
  
  clusterExport(clust, "analysis_data")
  
  library(arm)
  
  stdz.model <- standardize(glob.mod)
  
  model.set <- pdredge(stdz.model, m.lim=c(0, 2), cluster=clust, extra="R^2") 
  
  return(model.set)
  
}


model_averaging_results <- function(model_averaging) {
  
  #### selects all models with deltaAic < 4
  top.models <- get.models(model.set, subset=delta<4) 
  
  #### how many top models
  length(top.models)
  
  #### Ranks these models based on AICc
  my.models <- model.sel(top.models, rank="AICc") 
  
  Averaged_models <- model.avg(top.models)
  
  summary(Averaged_models)
  confidence_intervals <- confint(Averaged_models, full=TRUE)
  
  library(gridExtra)
  
  pdf("tables/Averaged_models.pdf")
  grid.table(confidence_intervals)
  dev.off()
}










