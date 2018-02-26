read_bird_tree<-function(){
  require(ape)
  download.file("https://www.dropbox.com/s/gfackoogv6n0bv0/AllBirdsEricson1.tre?raw=1","Data/phylo/phy.tre")
  bird_tree<-read.tree(file="Data/phylo/phy.tre")  
  #drop tips to get down to Aus species here
  return(bird_tree)
}


plot_bird_tree<-function(bird_tree){
  pdf("figures/bird_plot.pdf")
  plot(bird_tree,type="r")
  dev.off()
}