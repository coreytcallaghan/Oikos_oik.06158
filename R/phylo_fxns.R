


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
  unzip(path,overwrite=TRUE,exdir="Data/phylo/")
  one_bird_tree <- ape::read.tree(file = "/Users/wcornwell/Documents/bird_urbanness/Data/phylo/mnt/data/projects/birdphylo/Tree_sets/Stage2_full_data/CombinedTrees/AllBirdsEricson1.tre")[[x]]
  #drop tips to get down to Aus species here
  return(one_bird_tree)
}

read_all_trees<-function(path){
  unzip(path,overwrite=TRUE,exdir="Data/phylo/")
  ape::read.tree(file = "/Users/wcornwell/Documents/bird_urbanness/Data/phylo/mnt/data/projects/birdphylo/Tree_sets/Stage2_full_data/CombinedTrees/AllBirdsEricson1.tre")
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


run_one_phylo_model<-function(aus_bird_tree,analysis_data){
  #non_aus_sp <- aus_bird_tree$tip.label[!aus_bird_tree$tip.label %in% analysis_data$binom]
  #aus_bird_tree_ss <- diversitree:::drop.tip.fixed(aus_bird_tree, non_aus_sp)
  row.names(analysis_data)<-analysis_data$binom
  phy_mod<-phylolm(response~body_size_logged + clutch_size_logged + feeding_habitat_generalism +   
                     brain_residual + Habitat_agricultural + breeding_habitat_generalism + 
                     granivore + insectivore + 
                     carrion_eater + plant_eater + diet_generalism + movement_class +
                     ground_nesting + hollow_nesting + nest_generalism + breeding + 
                     nest_aggregation + feeding_aggregation + Habitat_grass_shrubland +
                     Habitat_tree_forest,data=analysis_data,phy=aus_bird_tree,
                   na.action = "na.fail", weights=(analysis_data$N/analysis_data$unique_localities))
  
  return(phy_mod)
}


phy_v_non_phy<-function(glob.mod,phy_mod){
  library(ggplot2)
  cc<-data.frame(phy_mod_coefs=coef(phy_mod),non_phy_mod=coef(glob.mod))
  cc$parameter<-substr(row.names(cc), start = 1, stop = 7)
  
  p <- ggplot(cc,aes(x=non_phy_mod,y=phy_mod_coefs))+geom_point()+geom_text(aes(label=parameter),hjust=0, vjust=0)+theme_bw()+geom_abline(slope=1,intercept=0)
  pdf("figures/phy_v_non_phy.pdf")
  print(p)
  dev.off()
}