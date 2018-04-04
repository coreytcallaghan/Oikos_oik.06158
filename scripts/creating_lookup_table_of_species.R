### R script to create a look-up table of all the birds in the analysis, using eBird as the starting point
### This is a bit challenging because of the different taxonomies used

### packages
library(readr)
library(dplyr)
library(tidyr)


### Load eBird data
load("Data/eBird data/Final eBird data for analysis/species_urban.RData")

### extract unique common and scientific names
eBird_species <- species_urban %>%
  select(COMMON_NAME, SCIENTIFIC_NAME) %>%
  distinct()

rm(species_urban)


### Load trait data species
traits<-read_csv("Data/Raw trait data/Australian_Bird_Data_Version_1.csv")
traits$binom<- paste(traits$`4_Genus_name_2`, traits$`5_Species_name_2`,sep="_")

traits_species <- traits %>%
  dplyr::select(binom, `3_Taxon_common_name_2`) %>%
  distinct(binom, .keep_all=TRUE) %>%
  rename(COMMON_NAME_traits = `3_Taxon_common_name_2`) %>%
  mutate(in_traits=TRUE) %>%
  rename(SCIENTIFIC_NAME = binom) %>%
  mutate(SCIENTIFIC_NAME_traits = SCIENTIFIC_NAME)


### Load tree data species
read_bird_tree<-function(){
  require(ape)
  if(!exists("Data/phylo/")) dir.create("Data/phylo/", showWarnings = FALSE)
  download.file("https://www.dropbox.com/s/gfackoogv6n0bv0/AllBirdsEricson1.tre?raw=1","Data/phylo/phy.tre")
  bird_trees<-ape::read.tree(file="Data/phylo/phy.tre")[[1]]
  #drop tips to get down to Aus species here
  return(bird_trees)
}

trees <- read_bird_tree()


subset_tree<-function(trees,traits){
  non_aus_sp<-trees$tip.label[!trees$tip.label %in% traits$binom]
  aus_bird_tree<-drop.tip(trees,non_aus_sp)
  return(aus_bird_tree)
}

AUS_tree <- subset_tree(trees, traits)


AUS_tree_birds <- data.frame(SCIENTIFIC_NAME_traits=AUS_tree$tip.label, in_tree = "TRUE", stringsAsFactors = FALSE)

## Now start merging together
## since the eBird data is the actual main source of data
## we will start with that

species_joined <- eBird_species %>%
  mutate(SCIENTIFIC_NAME = gsub(" ", "_", .$SCIENTIFIC_NAME)) %>%
  full_join(., traits_species, by="SCIENTIFIC_NAME") %>%
  replace_na(list(COMMON_NAME = "Not in eBird data")) %>%
  left_join(., AUS_tree_birds, by="SCIENTIFIC_NAME_traits") %>%
  replace_na(list(in_tree="Missing")) %>%
  rename(SCIENTIFIC_NAME_ebird = SCIENTIFIC_NAME) %>%
  rename(COMMON_NAME_ebird = COMMON_NAME)



## list of species in eBird but not in tree
missing_species <- species_joined %>%
  filter(COMMON_NAME != "Not in eBird data") %>%
  filter()





  
  