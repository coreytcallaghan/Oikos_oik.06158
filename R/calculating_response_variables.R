### Calculating response variables for each species
### This script is used to create the response variables for all species included in the analysis


### load eBird data
load_ebird_data <- function() {
  load('Data/eBird data/Final eBird data for analysis/species_urban.RData')
  species_urban
}

### calculate response variable df
make_response_variables <- function(species_urban) {
  
  response_variables <- species_urban %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(adapter= quantile(log(avg_rad), .90,na.rm=T),
            exploiter = quantile(log(avg_rad), .50,na.rm=T),
            avoider = quantile(log(avg_rad), .10,na.rm=T),
            N = n(),
            unique_localities = length(unique(LOCALITY_ID)))
  
  response_variables
  
}


### Plot the histogram of the response variables
distribution_response_variables <- function(response_variables) {
  
  pdf("figures/distribution_response_variables.pdf")
  
  print(response_variables %>%
    gather(key = "response_level", value = "value", adapter:avoider) %>%
    ggplot(., aes(value))+
    geom_histogram(bins = 50, 
                   col="black", 
                   fill="green", 
                   alpha = .2)+
    theme_bw()+
    xlab("Value")+
    ylab("Count")+
    facet_wrap(~response_level, scales="free"))
  
  dev.off()
}


