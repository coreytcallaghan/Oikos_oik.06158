


download_phylo <- function() {
  require(ape)
  if (!exists("Data/phylo/"))
    dir.create("Data/phylo/", showWarnings = FALSE)
  download.file(
    "https://data.vertlife.org/birdtree/Stage2/EricsonStage2_0001_1000.zip",
    "Data/phylo/EricsonStage2_0001_1000.zip"
  )
  
}

read_one_tree<-function(path,x=1){
  unzip(path,overwrite=TRUE,exdir="Data/phylo")
  one_bird_tree <- ape::read.tree(file = "Data/phylo/mnt/data/projects/birdphylo/Tree_sets/Stage2_full_data/CombinedTrees/AllBirdsEricson1.tre")[[x]]
  #drop tips to get down to Aus species here
  return(one_bird_tree)
}

read_all_trees<-function(path){
  unzip(path,overwrite=TRUE,exdir="Data/phylo")
  ape::read.tree(file = "Data/phylo/mnt/data/projects/birdphylo/Tree_sets/Stage2_full_data/CombinedTrees/AllBirdsEricson1.tre")
}





plot_bird_tree <- function(aus_bird_tree) {
  pdf("figures/bird_plot.pdf")
  plot(
    aus_bird_tree,
    type = "r",
    show.tip.label = TRUE,
    cex = 0.4,
    show.node.label = TRUE
  )
  dev.off()
}


subset_tree <- function(bird_trees, traits) {
  non_aus_sp <- bird_trees$tip.label[!bird_trees$tip.label %in% traits$binom]
  aus_bird_tree <- drop.tip(bird_trees, non_aus_sp)
  return(aus_bird_tree)
}


plot_bird_tree_traits <-
  function(aus_bird_tree,
           ms,
           response_variables) {
    trait <- as.array(ms$mean_body_size)
    row.names(trait) <- row.names(ms)
    trait <- subset(trait, trait != "NaN")
    trait <- subset(trait, names(trait) %in% aus_bird_tree$tip.label)
    tree_plotting <-
      drop.tip(aus_bird_tree, aus_bird_tree$tip.label[!aus_bird_tree$tip.label %in%
                                                        row.names(trait)])
    
    rv <-
      filter(response_variables,
             SCIENTIFIC_NAME_tree %in% tree_plotting$tip.label)
    median_rv <- as.array(rv$urban_median)
    row.names(median_rv) <- rv$SCIENTIFIC_NAME_tree
    median_rv2 <- median_rv #- mean(median_rv)
    
    
    tree_plotting_2 <-
      drop.tip(tree_plotting, tree_plotting$tip.label[!tree_plotting$tip.label %in%
                                                        row.names(median_rv2)])
    trait <- subset(trait, names(trait) %in% tree_plotting_2$tip.label)
    
    obj <-
      contMap(
        tree_plotting_2,
        median_rv2,
        fsize = c(0.6, 1),
        outline = FALSE,
        plot = FALSE,
        type = "fan"
      )
    
    pdf("figures/bird_urbanness_phylo.pdf",width=8.5,height=8.5)
    plotTree.wBars(
      obj$tree,
      median_rv2,
      method = "plotSimmap",
      colors = obj$cols,
      type = "fan",
      scale = 5,
      tip.labels = FALSE
    )
    add.color.bar(100, obj$cols, title = "trait value", lims = obj$lims, prompt = FALSE,x = 0.9 * par()$usr[1], y = 0.9 * par()$usr[3])
    dev.off()
    pdf("figures/ref_tree.pdf",width=8.5,height=8.5)
    plot(tree_plotting_2, type = "f", cex = 0.2)
    dev.off()
  }


run_many_phylo_models<-function(analysis_data,list_bird_trees,n=1000){
  bird_trees_ss<-list_bird_trees[1:n]
  ss_trees<-lapply(bird_trees_ss,subset_tree,analysis_data)
  list_o<-lapply(ss_trees,run_one_phylo_model,analysis_data=analysis_data)
  return(list_o)
}

extract_brain<-function(mod){
  nn<-names(mod$coefficients)
  return(mod$coefficients[nn=="brain_residual"])
}

plot_dist_parameter<-function(list_of_models){
  df<-data.frame(coef_brain=sapply(list_of_models,extract_brain))
  p<-ggplot(df,aes(x=coef_brain))+geom_histogram()
  pdf("figures/brains.pdf")
  print(p)
  dev.off()
}


run_one_phylo_model<-function(aus_bird_tree, analysis_data){
  #non_aus_sp <- aus_bird_tree$tip.label[!aus_bird_tree$tip.label %in% analysis_data$binom]
  #aus_bird_tree_ss <- diversitree:::drop.tip.fixed(aus_bird_tree, non_aus_sp)
  row.names(analysis_data) <- analysis_data$binom
  
  phy_mod <- phylolm(response ~ body_size_logged + clutch_size_logged + feeding_habitat_generalism + brain_residual + 
                     Habitat_agricultural + breeding_habitat_generalism + granivore + insectivore + 
                     carrion_eater + plant_eater + diet_generalism + migrate + nomadic_irruptive +
                     ground_nesting + hollow_nesting + nest_generalism + breeding + 
                     nest_aggregation + feeding_aggregation + Habitat_grass_shrubland + range_size +
                     Habitat_tree_forest, data=analysis_data, phy=aus_bird_tree,
                     na.action = "na.fail", weights=(analysis_data$N/analysis_data$unique_localities))
  
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
                                na.action = "na.fail", 
                                weights=(analysis_data$N/analysis_data$unique_localities))
  
  return(phy_mod)
}

standard_phylo_model <- function(aus_bird_tree, analysis_data) {
  
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
                                na.action = "na.fail", 
                                weights=(analysis_data$N/analysis_data$unique_localities))
  
  return(phy_mod_rescaled)
  
}


plot_params_phymod <- function(phy_mod_rescaled) {
  
  results <- data.frame(estimate = phy_mod_rescaled$coefficients, 
                        lwr = confint(phy_mod_rescaled)[,1],
                        upr = confint(phy_mod_rescaled)[,2],
                        p_value = summary(phy_mod_rescaled)$coefficients[,4],
                        stringsAsFactors = FALSE)
  
  summary(phy_mod_rescaled)$coefficients[,4]
  
  pdf("figures/param_plot_phylo_model.pdf", height=11, width=9)
  
  print( 
    results %>%
      rownames_to_column("term") %>%
      filter(term != "(Intercept)") %>%
      arrange(desc(estimate)) %>%
      mutate(term2 = c("Feeding habitat generalism",
                       "Diet generalism",
                       "log(Clutch size)",
                       "Habitat - agricultural",
                       "Brain residual",
                       "Breeding habitat generalism",
                       "log(Body size)",
                       "Plant eater",
                       "Nest generalism",
                       "Feeding aggregation \n (solitary, pairs, & flocks)",
                       "Movement - nomadic/irruptive",
                       "Movement - migratory",
                       "Feeding aggregation \n (pairs & flocks)",
                       "Ground-nesting",
                       "Feeding aggregation \n (solitary & pairs)",
                       "Habitat - tree/forest",
                       "Nest aggregation \n (solitary)",
                       "Range size (1000s km2)",
                       "Cooperative breeding",
                       "Carrion eater",
                       "Nest aggregation \n (colonial)",
                       "Hollow-nesting",
                       "Feeding aggregation \n (solitary & flocks)",
                       "Granivore",
                       "Feeding aggregation \n (solitary)",
                       "Nest aggregation \n (none)",
                       "Insectivore",
                       "Habitat - grass/shrubland",
                       "Feeding aggregation \n (pairs)")) %>%
      arrange(estimate) %>%
      mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
      mutate(significance=ifelse(.$p_value <= 0.05, "Significant", "Non-significant")) %>%
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

phy_v_non_phy<-function(global_model,phy_mod_rescaled){
  library(ggplot2)
  cc <- data.frame(phy_mod_coefs=coef(phy_mod_rescaled), 
                   non_phy_mod=coef(global_model)) %>%
    mutate(parameter = c("Intercept",
                         "log(Body size)",
                         "log(Clutch size)",
                         "Feeding habitat generalism",
                         "Brain residual", 
                         "Habitat - agricultural", 
                         "Breeding habitat generalism",
                         "Granivore", 
                         "Insectivore", 
                         "Carrion eater", 
                         "Plant eater", 
                         "Diet generalism",
                         "Movement - migratory", 
                         "Movement - nomadic/irruptive", 
                         "Ground-nesting", 
                         "Hollow-nesting", 
                         "Nest generalism", 
                         "Cooperative breeding", 
                         "Nest aggregation \n (colonial)",
                         "Nest aggregation \n (none)",
                         "Nest aggregation  \n (solitary)",
                         "Feeding aggregation \n (pairs)",
                         "Feeding aggregation \n (pairs & flocks)",
                         "Feeding aggregation \n (solitary)",
                         "Feeding aggregation \n (solitary & flocks)",
                         "Feeding aggregation \n (solitary & pairs)",
                         "Feeding aggregation \n (solitary, pairs, & flocks)",
                         "Habitat - grass/shrubland", 
                         "Range size (1000s km2)", 
                         "Habitat - tree/forest"))
  
  p <- ggplot(cc,aes(x=non_phy_mod,y=phy_mod_coefs))+
    geom_point(color="royalblue4")+
    theme_bw()+
    geom_abline(slope=1,intercept=0)+
    xlab("Non phylogenetic model")+
    ylab("Phylogenetic model")+
    geom_text_repel(aes(label = parameter), 
                    box.padding = unit(0.45, "lines"))
  
  pdf("figures/phy_v_non_phy.pdf", height=10, width=12)
  print(p)
  dev.off()
}