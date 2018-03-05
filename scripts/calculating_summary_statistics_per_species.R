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
    map(~residuals(lm(.~response_vector[[1]]))) %>%
    as.data.frame()
  
  quantile_results <- regression %>%
    map(quantile, .90) %>%
    as.data.frame() %>%
    gather()
  
  out_df <- data.frame(mean=mean(quantile_results$value),
                       sd=sd(quantile_results$value),
                       COMMON_NAME="Australasian Pipit")
  return(out_df)
}



