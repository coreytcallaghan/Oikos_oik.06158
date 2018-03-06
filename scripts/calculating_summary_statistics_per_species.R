### Calculating summary statistics for each species

### load data
load("Data/eBird data/Final eBird data for analysis/species_urban.RData")

### load checklist data
source("R/urbanness_functions.R")
checklists <- get_urbanness_table()

### packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)


### select one species as an example
AUPI <- species_urban %>%
  filter(COMMON_NAME=="Australasian Pipit")

### density plot of avg_radiance
ggplot(AUPI, aes(avg_rad))+
  geom_density()

### 90th percentile
quantile(AUPI$avg_rad, .90)


### Attempt to account for sampling biases
n <- nrow(AUPI)


resample_lists <- function(data, reps, variable, species) {
  
  checklist_df <- data %>%
    select(variable)
  
  response_vector <- species_urban %>%
    filter(COMMON_NAME==species) %>%
    select(variable)
  
  n <- nrow(response_vector)
  
  list_of_resamps <- replicate(n = reps, expr = {sample_n(checklist_df, n, replace = T)}, simplify = F)
  
  resamps_df <- data.frame(matrix(unlist(list_of_resamps), nrow=n, byrow=T), stringsAsFactors=FALSE)
  
  regression <- resamps_df %>%
    map(~fitted.values(lm(.~response_vector[[1]]))) %>%
    as.data.frame()
  
  quantile_results <- regression %>%
    map(quantile, .90) %>%
    as.data.frame() %>%
    gather()
  
  out_df <- data.frame(raw_data=quantile(response_vector[[1]], .90),
                       mean_sampled=mean(quantile_results$value),
                       sd=sd(quantile_results$value),
                       COMMON_NAME=species)
  return(out_df)
}

AUPI_gam <- gam(avg_rad ~ s(LATITUDE, LONGITUDE), data=AUPI, family=gaussian)

AUPI_gam_values <- predict(AUPI_gam)

anova(AUPI_gam)

## COMY
COMY <- species_urban %>%
  filter(COMMON_NAME == "Common Myna")

quantile(COMY$avg_rad, .90)

COMY_gam <- gam(avg_rad ~ s(LATITUDE, LONGITUDE), data=COMY, family=gaussian)

COMY_predicted <- predict(COMY_gam)

quantile(COMY_predicted, .90)


## SOEU
SOEU <- species_urban %>%
  filter(COMMON_NAME == "Southern Emuwren")

quantile(SOEU$avg_rad, .90)

SOEU_gam <- gam(avg_rad ~ s(LATITUDE, LONGITUDE), data=SOEU, family=gaussian)

SOEU_predicted <- predict(SOEU_gam)

quantile(SOEU_predicted, .90)




test_species <- c("Australasian Pipit", "Willie-wagtail", "Common Myna", "Great Egret",
                  "White-faced Heron", "Pied Currawong", "Southern Emuwren", "Masked Woodswallow",
                  "Painted Honeyeater", "Red Wattlebird", "Regent Honeyeater", "Southern Emuwren")


test_data <- species_urban %>%
  filter(COMMON_NAME %in% test_species)







species_urban %>% 
  group_by(COMMON_NAME)  %>% 
  summarise(obs = n()) %>% 
  filter(obs > 1000)

sp <- c("Common Myna", "Southern Emuwren", "Australian Raven",
        "House Sparrow", "Wedge-tailed Eagle")
  
species_urban %>% 
  filter(COMMON_NAME %in% sp) -> cool_birds
  
ggplot(cool_birds, aes(avg_rad, fill = COMMON_NAME)) +
    geom_density(alpha = 0.4) + 
    scale_x_log10() + theme_bw()
  
ggplot(cool_birds, aes(avg_rad, fill = COMMON_NAME)) +
  geom_histogram(bins = 1000) + 
  scale_x_log10() +
  facet_grid(COMMON_NAME ~ .) + theme_bw()


