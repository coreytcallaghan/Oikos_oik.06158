
  ### Packages
load_ebird_data <- function() {
  load('Data/eBird data/Final eBird data for analysis/species_urban.RData')
  species_urban
}

avg_rad_density_pdf <- function (cleaned_ebird_data) {
  sp <- c("Common Myna", "Southern Emuwren", "Australian Raven",
        "House Sparrow", "Wedge-tailed Eagle")

  cleaned_ebird_data %>% 
    filter(COMMON_NAME %in% sp) -> cool_birds

  ggplot(cool_birds, aes(avg_rad, fill = COMMON_NAME)) +
      geom_density(alpha = 0.4) + 
      scale_x_log10() + theme_bw()
  ggsave('figures/avg_rad_density.pdf')
}
