


read_bird_tree <- function() {
  require(ape)
  if (!exists("Data/phylo/"))
    dir.create("Data/phylo/", showWarnings = FALSE)
  download.file(
    "https://www.dropbox.com/s/gfackoogv6n0bv0/AllBirdsEricson1.tre?raw=1",
    "Data/phylo/phy.tre"
  )
  bird_trees <- ape::read.tree(file = "Data/phylo/phy.tre")[[1]]
  #drop tips to get down to Aus species here
  return(bird_trees)
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


subset_tree <- function(trees, traits) {
  non_aus_sp <- trees$tip.label[!trees$tip.label %in% traits$binom]
  aus_bird_tree <- drop.tip(trees, non_aus_sp)
  return(aus_bird_tree)
}


plot_bird_tree_traits <-
  function(aus_bird_tree,
           traits,
           response_variables) {
    trait <- as.array(traits$mean_body_size)
    row.names(trait) <- row.names(traits)
    trait <- subset(trait, trait != "NaN")
    trait <- subset(trait, names(trait) %in% aus_bird_tree$tip.label)
    tree_plotting <-
      drop.tip(aus_bird_tree, aus_bird_tree$tip.label[!aus_bird_tree$tip.label %in%
                                                        row.names(trait)])
    
    response_variables$SCIENTIFIC_NAME <-
      gsub(" ", "_", response_variables$SCIENTIFIC_NAME)
    rv <-
      filter(response_variables,
             SCIENTIFIC_NAME %in% tree_plotting$tip.label)
    median_rv <- as.array(rv$urban_median)
    row.names(median_rv) <- rv$SCIENTIFIC_NAME
    median_rv2 <- median_rv - mean(median_rv)
    
    
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
    
    pdf("figures/bird_urbanness_phylo.pdf")
    plotTree.wBars(
      obj$tree,
      median_rv2,
      method = "plotSimmap",
      colors = obj$cols,
      type = "fan",
      scale = 5,
      tip.labels = FALSE
    )
    dev.off()
    pdf("figures/ref_tree.pdf")
    plot(tree_plotting_2, type = "f", cex = 0.2)
    dev.off()
  }


run_phylo_lm <- function(traits,
                         response_variables,
                         aus_bird_tree) {

  #
  # match and subset to tree.
  # this is annoying because the phylo packages all use row.names and the tidyverse hates rownames.
  # first create a new data.frame (not tibble) with just the traits of interest
  #
  trait <-
    data.frame(
      mean_body_size = traits$mean_body_size,
      clutch_size = traits$clutch_size,
      gregariousness = traits$gregariousness
    )
  row.names(trait) <- row.names(traits)
  trait <-
    subset(
      trait,
      trait$mean_body_size != "NaN" &
        trait$clutch_size != "NaN" & trait$gregariousness != "NaN"
    )
  trait <- subset(trait, row.names(trait) %in% aus_bird_tree$tip.label)
  # subset that dataframe to those names that are both in the trait data base AND the tree
  trait<-subset(trait,trait$mean_body_size!="NaN"&trait$clutch_size!="NaN"&trait$gregariousness!="NaN")
  trait<-subset(trait,row.names(trait)%in%aus_bird_tree$tip.label)
  # subset the tree
  tree_plotting<-drop.tip(aus_bird_tree,aus_bird_tree$tip.label[!aus_bird_tree$tip.label%in%row.names(trait)])
  response_variables$SCIENTIFIC_NAME<-gsub(" ","_",response_variables$SCIENTIFIC_NAME)
  rv<-filter(response_variables,SCIENTIFIC_NAME%in%tree_plotting$tip.label)
  median_rv<-as.array(rv$urban_median)
  row.names(median_rv)<-rv$SCIENTIFIC_NAME
  median_rv2<-median_rv-mean(median_rv)
  
  
  tree_plotting_2<-drop.tip(tree_plotting,tree_plotting$tip.label[!tree_plotting$tip.label%in%row.names(median_rv2)])
  trait<-subset(trait,row.names(trait)%in%tree_plotting_2$tip.label)
  
  dd <- data.frame(urb=median_rv2)
  
  #match traits into the right dataframe  
  dd$body_size<-trait$mean_body_size[match(row.names(dd),row.names(trait))]
  dd$clutch_size<-trait$clutch_size[match(row.names(dd),row.names(trait))]
  dd$gregariousness<-trait$gregariousness[match(row.names(dd),row.names(trait))]
  # run the model
  mod1<-phylolm(urb~log10(body_size)+clutch_size+gregariousness,data=dd,phy=tree_plotting_2)
  return(mod1)
}


run_phylo_lme4 <- function(traits,
                         response_variables,
                         aus_bird_tree) {
  
  #
  # match and subset to tree.
  # this is annoying because the phylo packages all use row.names and the tidyverse hates rownames.
  # first create a new data.frame (not tibble) with just the traits of interest
  #
  trait <-
    data.frame(
      mean_body_size = traits$mean_body_size,
      clutch_size = traits$clutch_size,
      gregariousness = traits$gregariousness
    )
  row.names(trait) <- row.names(traits)
  trait <-
    subset(
      trait,
      trait$mean_body_size != "NaN" &
        trait$clutch_size != "NaN" & trait$gregariousness != "NaN"
    )
  trait <- subset(trait, row.names(trait) %in% aus_bird_tree$tip.label)
  # subset that dataframe to those names that are both in the trait data base AND the tree
  trait<-subset(trait,trait$mean_body_size!="NaN"&trait$clutch_size!="NaN"&trait$gregariousness!="NaN")
  trait<-subset(trait,row.names(trait)%in%aus_bird_tree$tip.label)
  # subset the tree
  tree_plotting<-drop.tip(aus_bird_tree,aus_bird_tree$tip.label[!aus_bird_tree$tip.label%in%row.names(trait)])
  response_variables$SCIENTIFIC_NAME<-gsub(" ","_",response_variables$SCIENTIFIC_NAME)
  rv<-filter(response_variables,SCIENTIFIC_NAME%in%tree_plotting$tip.label)
  median_rv<-as.array(rv$urban_median)
  row.names(median_rv)<-rv$SCIENTIFIC_NAME
  median_rv2<-median_rv-mean(median_rv)
  
  
  tree_plotting_2<-drop.tip(tree_plotting,tree_plotting$tip.label[!tree_plotting$tip.label%in%row.names(median_rv2)])
  trait<-subset(trait,row.names(trait)%in%tree_plotting_2$tip.label)
  
  dd <- data.frame(urb=median_rv2)
  
  #match traits into the right dataframe  
  dd$body_size<-trait$mean_body_size[match(row.names(dd),row.names(trait))]
  dd$clutch_size<-trait$clutch_size[match(row.names(dd),row.names(trait))]
  dd$gregariousness<-trait$gregariousness[match(row.names(dd),row.names(trait))]
  # run the model
  phylo<-tree_plotting_2
#  birdZ <- phylo.to.Z(tree_plotting_2)
  dd$phylo <- row.names(dd)
  dd$obs <- factor(seq(nrow(dd)))
  dd$urb_cat<-as.integer(dd$urb>0)
  dd$obs <- factor(seq(nrow(dd)))
#  +(1|obs)
  
#  phylo_lme4_fit <- phylo_lmm(urb~log10(body_size)+(1|phylo),sp=dd$phylo,
#                              data=dd,phylo=phylo,phylonm="phylo",
#                              control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore"),
#                              phyloZ=birdZ)
  
  basic_gaus_fit <- lmer(urb~clutch_size+log10(body_size)+(1|phylo),
                         data=dd,
                         control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore"))

  return(basic_gaus_fit)
}




phylolm_r2 <- function(phylo_lme4_fit) {
  #stuff
  Fixed<-fixef(phylo_lme4_fit)[2]*phylo_lme4_fit@frame$clutch_size+fixef(phylo_lme4_fit)[3]*phylo_lme4_fit@frame$`log10(body_size)`
    varF<-var(Fixed)
    r2<-varF/(varF + VarCorr(phylo_lme4_fit)$phylo[1]  + attr(VarCorr(phylo_lme4_fit), "sc")^2)
    return(r2)  
  }
