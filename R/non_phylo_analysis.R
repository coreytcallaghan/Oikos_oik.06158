## Script with functions for the results of the
## Non-phylo models


## create df for the model fitting
## read in range size file here
## subset data based on complete variables as well
matched_df <- function(response_variables, traits){
  
  response_variables$binom <- response_variables$SCIENTIFIC_NAME_tree
  
  analysis_data <- left_join(traits, response_variables, by="binom") %>%
    filter(SCIENTIFIC_NAME_tree != "not_treated_as_species")
  
  # clean data
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
  
  # filter data based on range size availability
  range_size <- read_csv("Data/Raw trait data/temp_range_size.csv")
  
  # filter one more time to final dataset
  analysis_data <- analysis_data %>%
    inner_join(., range_size, by=c("COMMON_NAME_traits", "SCIENTIFIC_NAME_traits")) %>%
    filter_all(all_vars(!is.na(.)))
  
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
    rename(`Range size (1000s km2)` = range_size) %>%
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
                   carrion_eater + plant_eater + diet_generalism + migrate + nomadic_irruptive +
                   ground_nesting + hollow_nesting + nest_generalism + breeding + 
                   nest_aggregation + feeding_aggregation + Habitat_grass_shrubland + range_size +
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
                      "Movement - migratory", "Movement - nomadic/irruptive", "Ground-nesting", "Hollow-nesting", 
                      "Nest generalism", "Cooperative breeding", "Nest aggregation", "Feeding aggregation",
                      "Habitat - grass/shrubland", "Range size (1000s km2)", "Habitat - tree/forest")
  
  colnames(collinearity_investigation) <- c("Generalized VIF", "Degrees of \nfreedom", "Adjusted \nGeneralized VIF")
  
  is.num <- sapply(collinearity_investigation, is.numeric)
  collinearity_investigation[is.num] <- lapply(collinearity_investigation[is.num], round, 2)
  
  pdf("tables/collinearity_investigation.pdf")
  grid.table(collinearity_investigation)
  dev.off()
}



## plot parameter estimates of standardized global model
plot_params_globmod <- function(global_model) {
  
    library(forcats)
    remake::dump_environment()
    std.mod <- standardize(global_model) 
    lwr <- data.frame(lwr=confint(std.mod)[,1])
    upr <- data.frame(upr=confint(std.mod)[,2])
    
    pdf("figures/param_plot_global_model.pdf", height=11, width=9)
    
    print( 
    tidy(std.mod) %>%
      mutate(lwr=lwr$lwr) %>%
      mutate(upr=upr$upr) %>%
      filter(term != "(Intercept)") %>%
      arrange(desc(estimate)) %>%
      mutate(term2 = c("Feeding habitat generalism",
                       "Breeding habitat generalism",
                       "log(Clutch size)",
                       "Diet generalism",
                       "Habitat - agricultural",
                       "Movement - migratory",
                       "log(Body size)",
                       "Movement - nomadic/irruptive",
                       "Brain residual",
                       "Feeding aggregation \n (solitary & flocks)",
                       "Ground-nesting",
                       "Feeding aggregation \n (solitary, pairs, & flocks)",
                       "Plant eater",
                       "Nest generalism",
                       "Feeding aggregation \n (solitary & pairs)",
                       "Range size (1000s km2)",
                       "Hollow-nesting",
                       "Cooperative breeding",
                       "Nest aggregation \n (colonial)",
                       "Feeding aggregation \n (pairs)",
                       "Nest aggregation \n (solitary)",
                       "Carrion eater",
                       "Nest aggregation \n (none)",
                       "Feeding aggregation \n (pairs & flocks)",
                       "Granivore",
                       "Habitat - tree/forest",
                       "Habitat - grass/shrubland",
                       "Insectivore",
                       "Feeding aggregation \n (solitary)")) %>%
      arrange(estimate) %>%
      mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
      ggplot(., aes(x=fct_inorder(term2), y=estimate, color=trend))+
      geom_point()+
      geom_errorbar(aes(ymin=lwr, ymax=upr, color=trend))+
      ylab("Parameter estimates")+
      xlab("")+
      coord_flip()+
      theme_classic()+
      guides(color=FALSE)+
      geom_hline(yintercept=0, color="black")
  )
  
  dev.off()
  
  rm(list = ls())
}


## this pulls in the dredged model results which was
## done out of the workflow
## the data was chunked in order to get around github file size limits
## will likely have to alter this once we have a finalized
## set of predictor variables
get_dredged_model <- function() {
  
  df1 <- readRDS("Data/dredged_models_1.rds")
  df2 <- readRDS("Data/dredged_models_2.rds")
  df3 <- readRDS("Data/dredged_models_3.rds")
  df4 <- readRDS("Data/dredged_models_4.rds")
  
  model.set <- bind_rows(df1, df2, df3, df4)
  
  return(model.set)
}


model_averaging_results <- function() {
  
  model_results <- readRDS("Data/dredged_model_averaged_param_est.rds")
  
  pdf("figures/param_plot_averaged_results.pdf")
  print(
  model_results %>%
    filter(variable != "(Intercept)") %>%
    droplevels() %>%
    arrange(desc(estimate)) %>%
    mutate(variable2 = c("log(Clutch size)",
                         "Feeding habitat generalism",
                         "Diet generalism",
                         "Habitat - agricultural",
                         "Breeding habitat generalism",
                         "Feeding aggregation \n (solitary & flocks)",
                         "log(Body size)",
                         "Nest generalism",
                         "Brain residual",
                         "Ground-nesting",
                         "Plant eater",
                         "Hollow-nesting",
                         "Cooperative breeding",
                         "Feeding aggregation \n (solitary, pairs, & flocks)",
                         "Nest aggregation \n (solitary)",
                         "Carrion eater",
                         "Nest aggregation \n (none)",
                         "Nest aggregation \n (colonial)",
                         "Habitat - tree/forest",
                         "Feeding aggregation \n (solitary & pairs)",
                         "Habitat - grass/shrubland",
                         "Feeding aggregation \n (pairs)", 
                         "Granivore", 
                         "Feeding aggregation \n (pairs & flocks)",
                         "Feeding aggregation \n (solitary)",
                         "Insectivore")) %>%
    arrange(estimate) %>%
    mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
    ggplot(., aes(x=fct_inorder(variable2), y=estimate, color=trend))+
    geom_point()+
    geom_errorbar(aes(ymin=lwr, ymax=upr, color=trend))+
    ylab("Parameter estimates")+
    xlab("")+
    coord_flip()+
    theme_classic()+
    guides(color=FALSE)+
    geom_hline(yintercept=0, color="black")
  )

dev.off()

  
  rm(list = ls())
}










