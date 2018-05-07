


library(ksi)

non_aus_sp <- aus_bird_tree$tip.label[!aus_bird_tree$tip.label %in% analysis_data$binom]
aus_bird_tree_ss <- drop.tip(aus_bird_tree, non_aus_sp)


row.names(analysis_data)<-analysis_data$binom

rr<-as.array(analysis_data$response)
names(rr)<-row.names(analysis_data)

aus_bird_tree_ss$nodel.label
aus_bird_tree_ss$node.label<-1:548

ksi(aus_bird_tree_ss, rr, depth=10, test=NULL, verbose=TRUE,
    multicore=FALSE, multicore.args=list())

plot(aus_bird_tree_ss,show.tip.label=FALSE,type="r",show.node.label=TRUE)
write.tree(aus_bird_tree_ss,"with_node_labels.tre")


aus_bird_tree_ss$tip.label[get.descendants(173,aus_bird_tree_ss,tips.only=TRUE)]
analysis_data$in173<-analysis_data$binom %in%aus_bird_tree_ss$tip.label[get.descendants(173,aus_bird_tree_ss,tips.only=TRUE)]
ggplot(analysis_data,aes(x=response,fill=in173))+geom_density(alpha=0.5)

aus_bird_tree_ss$tip.label[get.descendants(47,aus_bird_tree_ss,tips.only=TRUE)]
analysis_data$in47<-analysis_data$binom %in%aus_bird_tree_ss$tip.label[get.descendants(47,aus_bird_tree_ss,tips.only=TRUE)]
ggplot(analysis_data,aes(x=response,fill=in47))+geom_density(alpha=0.5)

aus_bird_tree_ss$tip.label[get.descendants(67,aus_bird_tree_ss,tips.only=TRUE)]
analysis_data$in47<-analysis_data$binom %in%aus_bird_tree_ss$tip.label[get.descendants(47,aus_bird_tree_ss,tips.only=TRUE)]
ggplot(analysis_data,aes(x=response,fill=in47))+geom_density(alpha=0.5)



library(phylosignal)

dd<-select_if(analysis_data,is.numeric)
tree4d<-phylo4d(aus_bird_tree_ss,tip.data=dd)
ps<-phyloSignal(tree4d)

ps2<-summarize_if(analysis_data,is.numeric,phylosig)

