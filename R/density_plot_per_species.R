### load eBird data
load_ebird_data <- function() {
  load('Data/eBird data/Final eBird data for analysis/species_urban.RData')
  species_urban
}

### plot a bunch of pdfs for all species
avg_rad_density_for_each_species <- function (species_urban) {
  
  ## find how many pages are necessary
  n_pages <- length(unique(species_urban$COMMON_NAME))/20
  
  pdf("figures/density_plot_per_species.pdf")
  
  for (i in 1:n_pages) {
    
    print(ggplot(species_urban, aes(avg_rad)) +
      geom_density(alpha = 0.4, fill="blue") + 
      scale_x_log10() + 
      theme_classic() +
      xlab("Average radiance") +
      ylab("Density") +
      facet_wrap_paginate(~COMMON_NAME, ncol = 4, nrow = 5, scales="free_y", page = i))
  
  }
  
  dev.off()
  
  pdf("figures/histogram_plot_per_species.pdf")
  
  for (i in 1:n_pages) {
    
    print(ggplot(species_urban, aes(avg_rad)) +
            geom_histogram(bins = 50, 
                           col="black", 
                           fill="green", 
                           alpha = .2) + 
            scale_x_log10() + 
            theme_classic() +
            xlab("Average radiance") +
            ylab("Count") +
            facet_wrap_paginate(~COMMON_NAME, ncol = 4, nrow = 5, scales="free_y", page = i))
    
  }
  
  dev.off()
  
  
}





















