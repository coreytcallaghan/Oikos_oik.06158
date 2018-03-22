## packages
library(ggplot2)
library(dplyr)
library(fitdistrplus)


## These would be the theoretical groups of birds
## based on the beta distribution
avoider <- data.frame(values=rbeta(10000, 0.5, 2),
                      type="avoider", stringsAsFactors = FALSE)
adapter <- data.frame(values=rbeta(10000, 2, 0.5),
                      type="adapter", stringsAsFactors = FALSE)
exploiter <- data.frame(values=rbeta(10000, 2, 2),
                      type="exploiter", stringsAsFactors = FALSE)
stork <- bind_rows(avoider, adapter, exploiter)

## plot them
ggplot(stork, aes(x=values, fill=type))+
  geom_histogram()+
  facet_wrap(~type)


## The theoretical distributions are made by alpha and beta
## So, if a species is a typical exploiter it should have a value of 1
## avoider should have 4 and adapter should have 0.25, etc.
# avoider <- 0.5/2
# adapter <- 2/0.5
# exploiter <- 2/2

## Showing this with simulated data
## avoider
beta_fit <- qmedist(avoider$values, "beta", probs=c(1/3, 2/3))
beta_fit_estimate <- as.data.frame(beta_fit$estimate)
beta_fit_estimate[1,1]/beta_fit_estimate[2,1]

## adapter
beta_fit <- qmedist(adapter$values, "beta", probs=c(1/3, 2/3))
beta_fit_estimate <- as.data.frame(beta_fit$estimate)
beta_fit_estimate[1,1]/beta_fit_estimate[2,1]

## exploiter
beta_fit <- qmedist(exploiter$values, "beta", probs=c(1/3, 2/3))
beta_fit_estimate <- as.data.frame(beta_fit$estimate)
beta_fit_estimate[1,1]/beta_fit_estimate[2,1]



## pick a couple species and test this theory!
## load data
load('Data/eBird data/Final eBird data for analysis/species_urban.RData')

species_urban$avg_rad_log <- log(species_urban$avg_rad)

## Dusky Moorhen
DUMO <- species_urban %>%
  filter(COMMON_NAME == "Dusky Moorhen") %>%
  dplyr::select(avg_rad) %>%
  mutate(avg_rad_log = log(avg_rad))

## histogram plot
## Looks like it should belong to the adapter grouping
ggplot(DUMO, aes(x=avg_rad_log))+
  geom_histogram()


# Need to put values between 0-1
# function for this
range01 <- function(x){(x-min(species_urban$avg_rad_log))/(max(species_urban$avg_rad_log)-min(species_urban$avg_rad_log))}

# transform data
DUMO$avg_rad_log_transform <- range01(DUMO$avg_rad_log)

# plot histogram again
ggplot(DUMO, aes(avg_rad_log_transform))+
  geom_histogram()

# fit beta distribution, as above
beta_fit <- qmedist(DUMO$avg_rad_log_transform, "beta", probs=c(1/3, 2/3))
beta_fit_estimate <- as.data.frame(beta_fit$estimate)
beta_fit_estimate[1,1]/beta_fit_estimate[2,1]


## now try Dusky Grasswren, which should be very much an avoider
DUGR <- species_urban %>%
  filter(COMMON_NAME == "Dusky Grasswren") %>%
  dplyr::select(avg_rad) %>%
  mutate(avg_rad_log = log(avg_rad))

## histogram plot
ggplot(DUGR, aes(x=avg_rad_log))+
  geom_histogram()

# transform data
DUGR$avg_rad_log_transform <- range01(DUGR$avg_rad_log)

# plot histogram again
ggplot(DUGR, aes(avg_rad_log_transform))+
  geom_histogram()

# fit beta distribution, as above
beta_fit <- qmedist(DUGR$avg_rad_log_transform, "beta", probs=c(1/3, 2/3))
beta_fit_estimate <- as.data.frame(beta_fit$estimate)
beta_fit_estimate[1,1]/beta_fit_estimate[2,1]

## Still not convinced. Need to test it

## write function to calculate "value" to test it a little bit
rv <- function(species) {
  
  data <- species_urban %>%
    filter(COMMON_NAME == species) %>%
    dplyr::select(avg_rad) %>%
    mutate(avg_rad_log = log(avg_rad))
  
  # transform data
 data$avg_rad_log_transform <- range01(data$avg_rad_log)
  
 # fit beta distribution
 beta_fit <- qmedist(data$avg_rad_log_transform, "beta", probs=c(1/3, 2/3))
 beta_fit_estimate <- as.data.frame(beta_fit$estimate)
 return(beta_fit_estimate[1,1]/beta_fit_estimate[2,1])
  
}

# test function
rv("Common Myna")
rv("Australian Ibis")
rv("Southern Emuwren")
rv("Noisy Miner")
rv("Dusky Grasswren")
rv("Spinifex Pigeon")
rv("Spotted Dove")
rv("Spotted Bowerbird")
rv("Varied Lorikeet")
rv("Western Rosella")


## Looks like it might work
## Want to see about using bootstrap to 
## account for sample size of the distibrutions it is estimating

# Want to see about using bootstrap
# need to use fitdist function instead of qmedist
# does the same thing though, but returns different object
beta_fit <- fitdist(DUGR$avg_rad_log_transform, "beta", method="qme", probs=c(1/3, 2/3))
beta_fit_estimate <- as.data.frame(beta_fit$estimate)
beta_fit_estimate[1,1]/beta_fit_estimate[2,1]

# function to run bootstraps
# can automatically parallelize it!
boot_results <- bootdist(beta_fit, niter=1000, bootmethod="nonparam", parallel="snow", ncpus=10)

boot_results_df <- boot_results[['estim']]

estimate <- boot_results_df %>%
  mutate(rv=shape1/shape2) %>%
  summarise(mean_rv = mean(rv),
            sd_rv = sd(rv))

beta_fit_estimate[1,1]/beta_fit_estimate[2,1]
estimate

## That seems to work alright
## Test it against Dusky Moorhen now
# does the same thing though, but returns different object
beta_fit <- fitdist(DUMO$avg_rad_log_transform, "beta", method="qme", probs=c(1/3, 2/3))
beta_fit_estimate <- as.data.frame(beta_fit$estimate)
beta_fit_estimate[1,1]/beta_fit_estimate[2,1]

# function to run bootstraps
# can automatically parallelize it!
boot_results <- bootdist(beta_fit, niter=1000, bootmethod="nonparam", parallel="snow", ncpus=10)

boot_results_df <- boot_results[['estim']]

estimate <- boot_results_df %>%
  mutate(rv=shape1/shape2) %>%
  summarise(mean_rv = mean(rv),
            sd_rv = sd(rv))

estimate

# Or another way would be to use the summary and CI they provide?
test_df <- as.data.frame(t(boot_results[["CI"]]))

test_df %>%
  mutate(rv=shape1/shape2)

# that doesn't seem to work

## rewrite function to calculate the bootstrapped estimates
rv <- function(species) {
  
  data <- species_urban %>%
    filter(COMMON_NAME == species) %>%
    dplyr::select(avg_rad) %>%
    mutate(avg_rad_log = log(avg_rad))
  
  # transform data
  data$avg_rad_log_transform <- range01(data$avg_rad_log)
  
  # fit beta distribution
  beta_fit <- fitdist(data$avg_rad_log_transform, "beta", method="qme", probs=c(1/3, 2/3))
  beta_fit_estimate <- as.data.frame(beta_fit$estimate)
  beta_fit_estimate[1,1]/beta_fit_estimate[2,1]
  
  boot_results <- bootdist(beta_fit, niter=1000, bootmethod="nonparam", parallel="snow", ncpus=10)
  
  boot_results_df <- boot_results[['estim']]
  
  estimate <- boot_results_df %>%
    mutate(rv=shape1/shape2) %>%
    summarise(mean_rv = mean(rv),
              sd_rv = sd(rv))
  
  return(estimate)
  
}


rv("Common Myna")
rv("Australian Ibis")
rv("Southern Emuwren")
rv("Noisy Miner")
rv("Dusky Grasswren")
rv("Spinifex Pigeon")
rv("Spotted Dove")
rv("Spotted Bowerbird")
rv("Varied Lorikeet")
rv("Western Rosella")














