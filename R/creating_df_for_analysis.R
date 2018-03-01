### This is an R script to assign histograms of urbanness to each bird species in Australia
### General process is to load the urbanness scale for each checklist
### and then load all the eBird data and join by checklist
### Then need to account for sampling somehow
### Then each species should have a vector of urbanness which will be collapsed to a single value on a continuous scale


### Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


### load checklist data
source("R/urbanness_functions.R")
checklists <- get_urbanness_table()


### load all eBird data
### setwd
setwd("Data/eBird data/All eBird data split")

### create a list of all filenames in the folder of data
fileNames <- Sys.glob("*.RData")

### create a matrix for each file
for (fileName in fileNames) {
  
  load(fileName)
  
}

### combine to one file
all_eBird_data <- bind_rows(df1, df2, df3, df4, df5, df6, df7, df8, df9)

### clean up column names
colnames(all_eBird_data) <- gsub(" ", "_", colnames(all_eBird_data))


### remove individual files
rm(list=setdiff(ls(), c("checklists", "all_eBird_data")))


### Merge all eBird data with response variables file
### subset the sampling data to what is necessary and join each species with its own urbanness value
species_urban <- checklists %>%
  replace_na(list(EFFORT_DISTANCE_KM=0, EFFORT_AREA_HA=0)) %>%
  ## filter out any checklists > 5 km in distance travelled
  filter(EFFORT_DISTANCE_KM <= 5) %>%
  ## filter out any checklists > 500 ha in area searched
  filter(EFFORT_AREA_HA <= 500) %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, EVI, accessibility, avg_rad, cf_cvg, landcover, `population-density`) %>%
  inner_join(., all_eBird_data, by="SAMPLING_EVENT_IDENTIFIER") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_DATE, LOCALITY, LOCALITY_ID,
                LATITUDE, LONGITUDE, EFFORT_AREA_HA, EFFORT_DISTANCE_KM, OBSERVATION_COUNT, GROUP_IDENTIFIER,
                TAXONOMIC_ORDER, PROTOCOL_TYPE, ALL_SPECIES_REPORTED, accessibility, EVI, DURATION_MINUTES,
                avg_rad, cf_cvg, landcover, `population-density`, CATEGORY) %>%
  rename(population_density = `population-density`)

rm(all_eBird_data)

################################
### clean up the eBird data ####
################################

# Apply some additional filtering criteria
species_urban <- species_urban %>%
  ## select only complete checklists
  filter(ALL_SPECIES_REPORTED==1) %>%
  ## select only stationary, exhaustive area, and travelling protocols
  filter(PROTOCOL_TYPE %in% c("Traveling", "Random", "Stationary", "Area",
                              "BirdLife Australia 20min-2ha survey", "BirdLife Australia 5 km radius search",
                              "BirdLife AUstralia 500m radius search"))

## Extract all the final variables we will want to keep
sampling_event_info <- species_urban %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, LOCALITY, LOCALITY_ID, OBSERVATION_DATE,
                PROTOCOL_TYPE, ALL_SPECIES_REPORTED, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, 
                DURATION_MINUTES, OBSERVATION_DATE, GROUP_IDENTIFIER, EVI, accessibility, 
                avg_rad, cf_cvg, landcover, population_density, LATITUDE, LONGITUDE) %>%
  distinct()

## want to keep the taxonomic order for each species
common_by_taxonomic <- species_urban %>%
  filter(CATEGORY %in% c("species","issf")) %>%
  dplyr::select(COMMON_NAME, TAXONOMIC_ORDER) %>%
  distinct(COMMON_NAME, .keep_all=TRUE)

# combines species and subspecies into one count
species_urban <- species_urban %>%
  filter(CATEGORY %in% c("species","issf")) %>% 
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>%
  rename(OBSERVATION_COUNT = COUNT_SPP) %>% 
  inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., common_by_taxonomic, by="COMMON_NAME") %>%
  ungroup()


# filter out group_identifier data to eliminate duplicated checklists
duplicated <- species_urban %>%
  drop_na(GROUP_IDENTIFIER) %>%
  dplyr::select(GROUP_IDENTIFIER, SAMPLING_EVENT_IDENTIFIER) %>%
  distinct(.keep_all=TRUE) %>%
  group_by(GROUP_IDENTIFIER) %>%
  # randomly sample one checklist for each group_identifier
  sample_n(., 1) %>%
  .$SAMPLING_EVENT_IDENTIFIER

duplicated_data <- species_urban %>%
  filter(SAMPLING_EVENT_IDENTIFIER %in% duplicated)

## now, append the selected checklists for each group_identifier
## with the non group_identifier checklists from the data

# first select all data without a group_identifier
species_urban <- species_urban %>%
  filter(!grepl("G", GROUP_IDENTIFIER)) %>%
  bind_rows(., duplicated_data)

## filter out seabirds based on taxonomic order
species_urban <- species_urban %>%
  filter(TAXONOMIC_ORDER <= 1650 | TAXONOMIC_ORDER >= 1966)

## remove everything but the final subsetted eBird dataframe
rm(list=setdiff(ls(), "species_urban"))

## save as RData file
save.image("Data/eBird data/Final eBird data for analysis/species_urban.RData")
