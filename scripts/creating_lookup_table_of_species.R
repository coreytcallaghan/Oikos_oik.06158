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
  select(-SCIENTIFIC_NAME) %>%
  left_join(., eBird_species, by="COMMON_NAME") %>%
  rename(SCIENTIFIC_NAME_ebird = SCIENTIFIC_NAME) %>%
  rename(COMMON_NAME_ebird = COMMON_NAME) %>%
  replace_na(list(SCIENTIFIC_NAME_ebird = "Not in eBird data"))



## list of species in eBird but not in tree
missing_species <- species_joined %>%
  filter(COMMON_NAME_ebird != "Not in eBird data") %>%
  filter(in_tree == "Missing") %>%
  dplyr::select(COMMON_NAME_ebird, SCIENTIFIC_NAME_ebird) %>%
  rename(COMMON_NAME_traits = COMMON_NAME_ebird) %>%
  left_join(., traits_species, by="COMMON_NAME_traits") %>%
  dplyr::select(-SCIENTIFIC_NAME, -in_traits) %>%
  rename(COMMON_NAME_ebird = COMMON_NAME_traits) 


species_joined_2 <- eBird_species %>%
  mutate(SCIENTIFIC_NAME = gsub(" ", "_", .$SCIENTIFIC_NAME)) %>%
  rename(COMMON_NAME_ebird = COMMON_NAME) %>%
  left_join(., missing_species, by="COMMON_NAME_ebird") %>%
  dplyr::select(-SCIENTIFIC_NAME_ebird) %>%
  full_join(., traits_species, by="SCIENTIFIC_NAME") %>%
  replace_na(list(SCIENTIFIC_NAME_traits.y = "different_scientific_name")) %>%
  mutate(SCIENTIFIC_NAME_traits = ifelse(.$SCIENTIFIC_NAME_traits.y == "different_scientific_name", 
                                         .$SCIENTIFIC_NAME_traits.x, .$SCIENTIFIC_NAME_traits.y)) %>%
  dplyr::select(-SCIENTIFIC_NAME_traits.x, -SCIENTIFIC_NAME_traits.y, -SCIENTIFIC_NAME) %>%
  replace_na(list(COMMON_NAME = "Not in eBird data")) %>%
  left_join(., AUS_tree_birds, by="SCIENTIFIC_NAME_traits") %>%
  replace_na(list(in_tree="Missing")) %>%
  rename(COMMON_NAME = COMMON_NAME_ebird) %>%
  left_join(., eBird_species, by="COMMON_NAME") %>%
  rename(COMMON_NAME_ebird = COMMON_NAME) %>%
  rename(SCIENTIFIC_NAME_ebird = SCIENTIFIC_NAME) %>%
  replace_na(list(SCIENTIFIC_NAME_ebird = "Not in eBird data")) %>%
  filter(COMMON_NAME_ebird != "Not in eBird data") %>%
  replace_na(list(SCIENTIFIC_NAME_traits = "Not matched by common or scientific name")) %>%
  replace_na(list(COMMON_NAME_traits = "Not matched by common or scientific name")) %>%
  mutate(COMMON_NAME_traits = ifelse(.$COMMON_NAME_traits == "Not matched by common or scientific name",
                                     .$COMMON_NAME_ebird, .$COMMON_NAME_traits)) %>%
  mutate(COMMON_NAME_traits = ifelse(.$SCIENTIFIC_NAME_traits == "Not matched by common or scientific name",
                                     "Not matched by common or scientific name", .$COMMON_NAME_traits))

## Now we know which species aren't matched by sorting for not matched species
not_matched <- species_joined_2 %>%
  filter(SCIENTIFIC_NAME_traits == "Not matched by common or scientific name")
  
              
## Now make a new df which matches with the current missing species
df_fixing <- data.frame(COMMON_NAME_ebird = not_matched$COMMON_NAME_ebird,
                        SCIENTIFIC_NAME_ebird = not_matched$SCIENTIFIC_NAME_ebird,
                        COMMON_NAME_traits = c("Grey Fantail", "Purple Swamphen", "Australian White Ibis",
                                               "Cicadabird", "Collared Kingfisher", "not_treated_as_species", "Beach Stone-curlew",
                                               "Grey-fronted Honeyeater", "Grey-headed Robin", "not_treated_as_species",
                                               "not_treated_as_species", "not_treated_as_species", "not_treated_as_species",
                                               "Red-backed Button-quail", "Major Mitchell's Cockatoo", "Frilled Monarch", 
                                               "Yellow Wagtail", "not_treated_as_species", "new_to_australia"), 
                        stringsAsFactors = FALSE) %>%
  left_join(., traits_species, by="COMMON_NAME_traits") %>%
  dplyr::select(-SCIENTIFIC_NAME, -in_traits)



species_joined_final <- species_joined_2 %>%
  dplyr::select(-in_tree, -in_traits) %>%
  left_join(., df_fixing, by=c("COMMON_NAME_ebird", "SCIENTIFIC_NAME_ebird")) %>%
  replace_na(list(COMMON_NAME_traits.x = "Not matched by common or scientific name")) %>%
  mutate(COMMON_NAME_traits = ifelse(.$COMMON_NAME_traits.x == "Not matched by common or scientific name", 
                                     .$COMMON_NAME_traits.y, .$COMMON_NAME_traits.x)) %>%
  mutate(SCIENTIFIC_NAME_traits = ifelse(.$SCIENTIFIC_NAME_traits.x == "Not matched by common or scientific name",
                                         .$SCIENTIFIC_NAME_traits.y, .$SCIENTIFIC_NAME_traits.x)) %>%
  dplyr::select(COMMON_NAME_ebird, SCIENTIFIC_NAME_ebird, COMMON_NAME_traits, SCIENTIFIC_NAME_traits) %>%
  left_join(., AUS_tree_birds, by="SCIENTIFIC_NAME_traits") %>%
  replace_na(list(in_tree="Missing")) %>%
  replace_na(list(SCIENTIFIC_NAME_traits = "replace")) %>%
  mutate(SCIENTIFIC_NAME_traits = ifelse(.$SCIENTIFIC_NAME_traits == "replace",
                                         .$COMMON_NAME_traits, .$SCIENTIFIC_NAME_traits))



### Now we know all the species in the analysis (from the eBird data)
### and the associated trait information
### The last step is to assess which species have different names in the Jetz tree
### which is the basis of the phylo portion of the models
### I downloaded the Master taxonomy from Jetz's Nature paper which has the common names 
### So we'll read that in and look for matches for the file which was calculated above

## first clean up workspace and remove all but the currently joined version above
rm(list=setdiff(ls(), "species_joined_final"))

## read master taxonomy in from Jetz
tree_taxonomy <- read_csv("Data/phylo/2012-03-04206D-master_taxonomy.csv")
  
tree_taxonomy <- tree_taxonomy %>%
  dplyr::select(Scientific, TipLabel, English)

## first find which species are in the Jetz tree based on
## traits data common names but didn't match up with
## scientific names
tree_join_missing <- species_joined_final %>%
  filter(in_tree == "Missing") %>%
  rename(English = COMMON_NAME_traits) %>%
  left_join(., tree_taxonomy, by="English") %>%
  rename(COMMON_NAME_traits = English) %>%
  dplyr::select(-in_tree, -Scientific) %>%
  left_join(., tree_taxonomy, by="TipLabel") %>%
  rename(SCIENTIFIC_NAME_tree = TipLabel) %>%
  dplyr::select(-Scientific) %>%
  rename(COMMON_NAME_tree = English)


tree_join_not_missing <- species_joined_final %>%
  filter(in_tree != "Missing") %>%
  rename(TipLabel = SCIENTIFIC_NAME_traits) %>%
  left_join(., tree_taxonomy, by="TipLabel") %>%
  rename(SCIENTIFIC_NAME_traits = TipLabel) %>%
  dplyr::select(-in_tree) %>%
  rename(SCIENTIFIC_NAME_tree = Scientific) %>%
  rename(COMMON_NAME_tree = English)


## Now can join these two df
## and see how many didn't join by either 
## common or scientific name
tree_join_full <- bind_rows(tree_join_missing, tree_join_not_missing) %>%
  replace_na(list(COMMON_NAME_tree = "Missing", SCIENTIFIC_NAME_tree = "Missing"))


## now extract the missing ones only
missing_from_tree <- tree_join_full %>%
  filter(COMMON_NAME_tree=="Missing")

## looks like 22 species are missing from the tree
df_fixing_tree <- data.frame(COMMON_NAME_ebird=missing_from_tree$COMMON_NAME_ebird,
                             SCIENTIFIC_NAME_ebird=missing_from_tree$SCIENTIFIC_NAME_ebird,
                             COMMON_NAME_traits=missing_from_tree$COMMON_NAME_traits,
                             SCIENTIFIC_NAME_traits=missing_from_tree$SCIENTIFIC_NAME_traits,
                             COMMON_NAME_tree = c("Great Crested Tern", 
                                                  "Australian Sacred Ibis", 
                                                  "Yellow-tailed Black-cockatoo",
                                                  "Horsfield's Bronze-cuckoo", 
                                                  "European Greenfinch", 
                                                  "Slender-billed Cicadabird",
                                                  "Gull-billed Tern", 
                                                  "Shining Bronze-cuckoo", 
                                                  "Emerald Dove", 
                                                  "not_treated_as_species",
                                                  "Little Bronze-cuckoo", 
                                                  "not_treated_as_species", 
                                                  "not_treated_as_species",
                                                  "not_treated_as_species", 
                                                  "not_treated_as_species", 
                                                  "not_treated_as_species",
                                                  "Baudin<U+0092>s Black-Cockatoo", 
                                                  "Blue Quail",  
                                                  "Red-backed Buttonquail",
                                                  "not_treated_as_species", 
                                                  "not_treated_as_species",
                                                  "Aleutian Tern"),
                             SCIENTIFIC_NAME_tree = c("Sterna bergii", 
                                                      "Threskiornis molucca",  
                                                      "Calyptorhynchus funereus",
                                                      "Chrysococcyx basalis", 
                                                      "Carduelis chloris", 
                                                      "Coracina tenuirostris",
                                                      "Sterna nilotica", 
                                                      "Chrysococcyx lucidus", 
                                                      "Chalcophaps indica",
                                                      "not_treated_as_species", 
                                                      "Chrysococcyx minutillus",
                                                      "not_treated_as_species", 
                                                      "not_treated_as_species",
                                                      "not_treated_as_species", 
                                                      "not_treated_as_species", 
                                                      "not_treated_as_species",
                                                      "Calyptorhynchus baudinii",
                                                      "Coturnix chinensis", 
                                                      "Turnix maculosus",
                                                      "not_treated_as_species", 
                                                      "not_treated_as_species", 
                                                      "Sterna aleutica"), stringsAsFactors = FALSE)


### pull apart complete from joined tree file above
### then merge this with the df fixed manually for a final lookup table
tree_complete <- tree_join_full %>%
  filter(COMMON_NAME_tree!="Missing")


final_lookup <- bind_rows(tree_complete, df_fixing_tree)





















  
  