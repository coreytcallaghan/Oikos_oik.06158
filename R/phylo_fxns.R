read_bird_tree<-function(){
  require(ape)
  if(!exists("Data/phylo/")) dir.create("Data/phylo/", showWarnings = FALSE)
  download.file("https://www.dropbox.com/s/gfackoogv6n0bv0/AllBirdsEricson1.tre?raw=1","Data/phylo/phy.tre")
  bird_trees<-ape::read.tree(file="Data/phylo/phy.tre")[[1]]
  #drop tips to get down to Aus species here
  return(bird_trees)
}


plot_bird_tree<-function(aus_bird_tree){
  pdf("figures/bird_plot.pdf")
  plot(aus_bird_tree,type="r",show.tip.label = TRUE,cex=0.4,show.node.label=TRUE)
  dev.off()
}


subset_tree<-function(trees,traits){
  non_aus_sp<-trees$tip.label[!trees$tip.label %in% traits$binom]
  aus_bird_tree<-drop.tip(trees,non_aus_sp)
  return(aus_bird_tree)
  }


plot_bird_tree_traits<-function(aus_bird_tree,traits){
  trait<-as.array(traits$mean_body_size)
  row.names(trait)<-row.names(traits)
  trait<-subset(trait,trait!="NaN")
  trait<-subset(trait,names(trait)%in%aus_bird_tree$tip.label)
  tree_plotting<-drop.tip(aus_bird_tree,aus_bird_tree$tip.label[!aus_bird_tree$tip.label%in%row.names(trait)])
  
  obj <- contMap(tree_plotting, log10(trait), fsize = c(0.6, 1), outline = FALSE)
  
  pdf("figures/bird_plot.pdf")
  plotTree.wBars(obj$tree, log10(trait), method = "plotSimmap", colors = obj$cols,
                 type = "fan", scale = 5)
  dev.off()
}

