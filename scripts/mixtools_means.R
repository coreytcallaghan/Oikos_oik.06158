library(dplyr)
library(mixtools)
# https://www.r-bloggers.com/fitting-mixture-distributions-with-the-r-package-mixtools/
load('Data/eBird data/Final eBird data for analysis/species_urban.RData')

test_species <- c("Common Myna", "Australian Ibis", "Southern Emuwren", "Noisy Miner", 
                  "Dusky Grasswren", "Spinifex Pigeon", "Spotted Dove", "Spotted Bowerbird",
                  "Varied Lorikeet", "Western Rosella")




species_urban %>% group_by(COMMON_NAME) %>% 
  summarise(
    nobs = n(),
    log_mean = log(mean(avg_rad))
  ) %>% arrange(desc(log_mean)) -> sp_summary

sp_summary %>% 
  ggplot(aes(log_mean)) + geom_histogram()

sp_summary$mean %>% log %>% density(bw = 0.2) %>% plot
mixtools::normalmixEM(sp_summary$mean %>% log, k = 3) -> mxdist3
mixtools::normalmixEM(sp_summary$mean %>% log, k = 2) -> mxdist2
mixtools::normalmixEM(sp_summary$mean %>% log, k = 4) -> mxdist4

mxdist2 %>% summary
mxdist3 %>% summary
mxdist4 %>% summary
# ok
plot(mxdist2, which = 2)
# good
plot(mxdist3, which = 2)
# bad
plot(mxdist4, which = 2)

plot(mxdist3, which = 2)
favs <- sp_summary %>% filter(COMMON_NAME %in% test_species)
abline(v = favs$log_mean)


