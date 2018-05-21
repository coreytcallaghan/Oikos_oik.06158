### A script for summary of the results and to create final figures/tables using the final set of species

## first dump the remake environment
remake::dump_environment()

## total number of species included in models
nrow(analysis_data)

## summary of urbanization index
summary(analysis_data$urban_median)

sd(analysis_data$urban_median)

min(analysis_data$urban_median)

max(analysis_data$urban_median)

## get total number of checklists and observations used for the analysis, based on the 477 species only
## need to subset the eBird data to only those species in the analysis data

## get list of species
species <- analysis_data %>%
  dplyr::select(COMMON_NAME_ebird) %>%
  rename(COMMON_NAME = COMMON_NAME_ebird) %>%
  .$COMMON_NAME

## subset eBird data based on this list
eBird_final_data <- ebird_data %>%
  filter(COMMON_NAME %in% species)

## number of checklists
length(unique(eBird_final_data$SAMPLING_EVENT_IDENTIFIER))

## number of observations
nrow(eBird_final_data)

## Figure 1
## A figure showing the distribution of 5 different species included in the analysis
## Need to load the species_urban .RData file first
load('Data/eBird data/Final eBird data for analysis/species_urban.RData')

species_to_plot <- c("Dusky Moorhen",
                     "Yellow-tailed Black-Cockatoo",
                     "Hooded Robin",
                     "Noisy Friarbird",
                     "Squatter Pigeon")
library(scales)
Figure1 <- species_urban %>%
  filter(COMMON_NAME %in% species_to_plot) %>%
  ggplot(., aes(avg_rad, fill=COMMON_NAME)) +
  geom_density(alpha = 0.7) + 
  scale_x_continuous(trans='log10', breaks=c(0.0001, 0.0100, 1.0000, 100.0000), labels=c("0.0001", "0.01", "1.0", "100.0"))+ 
  theme_classic() +
  xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
  ylab("Density")+
  guides(fill=guide_legend(title="  Common Name")) +
  theme(legend.position = c(0.26, 0.8))+
  theme(axis.text.x=element_text(hjust=0.6))

pdf("finalFigs/Figure1.pdf", width=5.5, height=4.5)
print(Figure1)
dev.off()


## Figure 2
## Figure 2 is the distribution of response variable for those species included in the analyses
## Use the df2 dataframe from above
## I want to add a line for each of the example species on the distribution

species_to_plot <- c("Dusky Moorhen",
                     "Yellow-tailed Black-Cockatoo",
                     "Hooded Robin",
                     "Noisy Friarbird",
                     "Squatter Pigeon")


species_values <- analysis_data %>%
  filter(COMMON_NAME_ebird %in% species_to_plot) %>%
  dplyr::select(COMMON_NAME_ebird, urban_median)

# I manually add arrows here using the species_values df above
Figure2 <- ggplot(data=analysis_data, aes(urban_median)) +
  geom_histogram(bins = 70, 
                 col="black", 
                 fill="green", 
                 alpha = 0.3)+
  scale_x_continuous(trans='log10', breaks=c(0.005, 0.1, 1.0, 12.0), labels=c("0.005", "0.1", "1.0", "12.0"))+
  theme_classic()+
  xlab("Urbanization index")+
  ylab("Count")+
  geom_segment(aes(x=0.025, xend=0.025, y=23, yend=16), color='firebrick4', size=1.2, arrow=arrow(length=unit(0.5, "cm")))+
  geom_segment(aes(x=0.034, xend=0.034, y=25, yend=18), color='firebrick4', size=1.2, arrow=arrow(length=unit(0.5, "cm")))+
  geom_segment(aes(x=0.27505, xend=0.27507, y=24, yend=17), color='firebrick4', size=1.2, arrow=arrow(length=unit(0.5, "cm")))+
  geom_segment(aes(x=0.68820, xend=0.68820, y=19, yend=12), color='firebrick4', size=1.2, arrow=arrow(length=unit(0.5, "cm")))+
  geom_segment(aes(x=10, xend=10, y=11, yend=4), color='firebrick4', size=1.2, arrow=arrow(length=unit(0.5, "cm")))

pdf("finalFigs/Figure2.pdf", width=5.5, height=4.5)
print(Figure2)
dev.off()


## Figure 3
## The phylogenetic tree. Will be able to simply move 
## this over from the figures folder and rename it so it is Figure3
## lazy way, but easiest for me at this point in time
my.file.copy <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.copy(from = from,  to = to)
}

my.file.copy(from = "figures/bird_urbanness_phylo.pdf",
               to = "finalFigs/Figure3.pdf")



## Figure 4
## All parameter estimates
## A script which plots results for each of the four 'models'

## first make dfs for each separate model approach
### Non -phylo global model
std.mod <- standardize(global_model) 
lwr <- data.frame(lwr=confint(std.mod)[,1])
upr <- data.frame(upr=confint(std.mod)[,2])

estimates_non_phylo <- tidy(std.mod) %>%
  mutate(lwr=lwr$lwr) %>%
  mutate(upr=upr$upr) %>%
  mutate(significance = ifelse(p.value <=0.05, "Significant", "Non-significant")) %>%
  filter(term != "(Intercept)") %>%
  arrange(desc(estimate)) %>%
  mutate(term2 = c("Feeding habitat generalism",
                   "Breeding habitat generalism",
                   "log(Clutch size)",
                   "Diet generalism",
                   "Habitat - agricultural",
                   "Movement - migratory",
                   "log(Body size)",
                   "Movement - nomadic/irruptive",
                   "Brain residual",
                   "Feeding aggregation \n (solitary & flocks)",
                   "Ground-nesting",
                   "Feeding aggregation \n (solitary, pairs, & flocks)",
                   "Plant eater",
                   "Nest generalism",
                   "Feeding aggregation \n (solitary & pairs)",
                   "Range size (1000s km2)",
                   "Hollow-nesting",
                   "Cooperative breeding",
                   "Nest aggregation \n (colonial)",
                   "Feeding aggregation \n (pairs)",
                   "Nest aggregation \n (solitary)",
                   "Carrion eater",
                   "Nest aggregation \n (none)",
                   "Feeding aggregation \n (pairs & flocks)",
                   "Granivore",
                   "Habitat - tree/forest",
                   "Habitat - grass/shrubland",
                   "Insectivore",
                   "Feeding aggregation \n (solitary)")) %>%
  arrange(estimate) %>%
  mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
  dplyr::select(term2, estimate, lwr, upr, statistic, p.value, significance, trend) %>%
  rename(variable=term2) %>%
  rename(p_value=p.value) %>%
  mutate(model = "Non-phylo global model")

## Non-phylo model-averaged
model_results <- readRDS("Data/dredged_model_averaged_param_est.rds")
summary <- readRDS("Data/dredged_model_summary_results.rds")

p_values <- data.frame(p_value=summary$coefmat.full[,5]) %>%
  rownames_to_column("variable")

statistic <- data.frame(statistic=summary$coefmat.full[,4]) %>%
  rownames_to_column("variable")

estimates_non_phylo_averaged <- model_results %>%
    inner_join(., p_values, by="variable") %>%
    inner_join(., statistic, by="variable") %>%
    filter(variable != "(Intercept)") %>%
    droplevels() %>%
    arrange(desc(estimate)) %>%
    mutate(variable2 = c("log(Clutch size)",
                         "Habitat - agricultural",
                         "Diet generalism",
                         "Feeding aggregation \n (solitary & flocks)",
                         "Breeding habitat generalism",
                         "Feeding habitat generalism",
                         "Movement - migratory",
                         "Movement - nomadic/irruptive",
                         "log(Body size)",
                         "Ground-nesting",
                         "Cooperative breeding",
                         "Carrion eater",
                         "Nest aggregation \n (colonial)",
                         "Nest aggregation \n (solitary)",
                         "Nest aggregation \n (none)",
                         "Feeding aggregation \n (solitary, pairs, & flocks)",
                         "Feeding aggregation \n (solitary & pairs)",
                         "Feeding aggregation \n (pairs)",
                         "Habitat - tree/forest",
                         "Granivore", 
                         "Feeding aggregation \n (pairs & flocks)",
                         "Habitat - grass/shrubland",
                         "Insectivore",
                         "Feeding aggregation \n (solitary)")) %>%
    arrange(estimate) %>%
    mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
    mutate(significance=ifelse(.$p_value <= 0.05, "Significant", "Non-significant")) %>%
    dplyr::select(variable2, estimate, lwr, upr, statistic, p_value, significance, trend) %>%
    rename(variable=variable2) %>%
    mutate(model = "Non-phylo model-averaged")

## Phylo model global
results <- data.frame(estimate = phy_mod_rescaled$coefficients, 
                      lwr = confint(phy_mod_rescaled)[,1],
                      upr = confint(phy_mod_rescaled)[,2],
                      p_value = summary(phy_mod_rescaled)$coefficients[,4],
                      statistic = summary(phy_mod_rescaled)$coefficients[,3],
                      stringsAsFactors = FALSE)


estimates_phylo <- results %>%
    rownames_to_column("term") %>%
    filter(term != "(Intercept)") %>%
    arrange(desc(estimate)) %>%
    mutate(term2 = c("Brain residual",
                     "Feeding habitat generalism",
                     "log(Clutch size)",
                     "log(Body size)",
                     "Diet generalism",
                     "Breeding habitat generalism",
                     "Feeding aggregation \n (solitary, pairs, & flocks)",
                     "Feeding aggregation \n (solitary & pairs)",
                     "Ground-nesting",
                     "Nest aggregation \n (colonial)",
                     "Plant eater",
                     "Habitat - tree/forest",
                     "Habitat - agricultural",
                     "Movement - migratory",
                     "Movement - nomadic/irruptive",
                     "Nest aggregation \n (solitary)",
                     "Insectivore",
                     "Feeding aggregation \n (solitary)",
                     "Feeding aggregation \n (pairs & flocks)",
                     "Habitat - grass/shrubland",
                     "Feeding aggregation \n (solitary & flocks)",
                     "Feeding aggregation \n (pairs)",
                     "Hollow-nesting",
                     "Nest generalism",
                     "Granivore",
                     "Range size (1000s km2)",
                     "Cooperative breeding",
                     "Nest aggregation \n (none)",
                     "Carrion eater")) %>%
    arrange(estimate) %>%
    mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
    mutate(significance=ifelse(.$p_value <= 0.05, "Significant", "Non-significant")) %>%
    dplyr::select(term2, estimate, lwr, upr, statistic, p_value, significance, trend) %>%
    rename(variable=term2) %>%
    mutate(model = "Phylo global model")

# Phylo model-averaged results
model_results <- readRDS("Data/PHYLO_dredged_model_averaged_param_est.rds")
summary <- readRDS("Data/PHYLO_dredged_model_summary_results.rds")

p_values <- data.frame(p_value=summary$coefmat.full[,4]) %>%
  rownames_to_column("variable")

statistic <- data.frame(statistic=summary$coefmat.full[,3]) %>%
  rownames_to_column("variable")

estimates_phylo_averaged <- model_results %>%
    inner_join(., p_values, by="variable") %>%
    inner_join(., statistic, by="variable") %>%
    filter(variable != "(Intercept)") %>%
    droplevels() %>%
    arrange(desc(estimate)) %>%
    mutate(variable2 = c("Feeding habitat generalism",
                         "log(Clutch size)",
                         "log(Body size)",
                         "Brain residual",
                         "Nest generalism",
                         "Breeding habitat generalism",
                         "Carrion eater",
                         "Diet generalism",
                         "Movement - migratory",
                         "Habitat - agricultural",
                         "Plant eater",
                         "Habitat - tree/forest",
                         "Movement - nomadic/irruptive",
                         "Ground-nesting",
                         "Insectivore",
                         "Hollow-nesting",
                         "Granivore",
                         "Range size (1000s km2)",
                         "Nest aggregation \n (solitary)",
                         "Nest aggregation \n (colonial)",
                         "Cooperative breeding",
                         "Habitat - grass/shrubland",
                         "Nest aggregation \n (none)")) %>%
    arrange(estimate) %>%
    mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
    mutate(significance=ifelse(.$p_value <= 0.05, "Significant", "Non-significant")) %>%
    mutate(model = "Phylo model-averaged") %>%
    dplyr::select(variable2, estimate, lwr, upr, statistic, p_value, significance, trend, model) %>%
    rename(variable = variable2)

## Now need to merge the dfs together
param_estimates <- bind_rows(estimates_non_phylo, estimates_phylo, estimates_non_phylo_averaged, estimates_phylo_averaged)

## write model results for an appendix
write_csv(param_estimates, "H:/Dissertation/Dissertation Chapters/Data Chapters/Adaptations to urban living in birds/Appendices/Appendix 5/Appendix5.csv")

## Make figure which combines all model results into one
p <- param_estimates %>%
  group_by(variable) %>%
  summarise(mean_estimate=mean(estimate)) %>%
  arrange(desc(mean_estimate)) %>%
  inner_join(., param_estimates, by="variable") %>%
  arrange(model, mean_estimate)

Figure4 <- ggplot(p, aes(x=fct_inorder(variable), y=estimate, group=model, color=model))+
    geom_hline(yintercept=0, color="black")+
    geom_point(aes(shape=significance), size=3, position=position_dodge(width=0.6))+
    scale_shape_manual(values=c(19, 8))+
    geom_errorbar(aes(ymin=lwr, ymax=upr, color=model), position=position_dodge(width=0.6))+
    coord_flip()+
    ylab("Parameter estimates")+
    xlab("")+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank())+
    guides(shape=FALSE)+
    geom_vline(xintercept=seq(1.5, length(unique(p$variable))-0.5, 1), 
               lwd=0.2, colour="gray80")+
    theme(legend.background = element_rect(fill="white", color="black"))+
    theme(legend.position=c(0.16, 0.88))+
    guides(colour = guide_legend(title="                  Model",
                                 override.aes = list(size=2, shape=NA)))+
    theme(axis.text.x=element_text(size=12))+
    theme(axis.title.y=element_text(size=16))+
    theme(axis.text.y=element_text(size=7.5))
    
  pdf("finalFigs/Figure4.pdf", width=9.45, height=10)
  print(Figure4)
  dev.off()

  
  
## Figure 5
## Assessing differences between phylo and non-phylo global models
my.file.copy <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.copy(from = from,  to = to)
}

my.file.copy(from = "figures/phy_v_non_phy.pdf",
             to = "finalFigs/Figure5.pdf")
  
  
##########################################
###### Supplementary Figures #############
##########################################

## Figure S1
## made using google earth engine, outside of R

## Figure S2
my.file.copy(from = "figures/corrplot_of_continuous_variables.pdf",
             to = "finalFigs/FigureS2.pdf")


## Figure S3
my.file.copy(from = "figures/ref_tree.pdf",
             to = "finalFigs/FigureS3.pdf")


## Figure S4
my.file.copy(from = "figures/accounting_for_phylo_uncertainty.pdf",
             to = "finalFigs/FigureS4.pdf")



#######################################
######## Appendices ###################
#######################################

## want to remake appendix 3 so it only 
## includes the 477 species and not the 580 species
### plot a bunch of pdfs for all species
library(ggforce)

  ## find how many pages are necessary
  n_pages <- length(unique(eBird_final_data$COMMON_NAME))/15
  
  pdf("H:/Dissertation/Dissertation Chapters/Data Chapters/Adaptations to urban living in birds/Appendices/Appendix 3/Appendix 3.pdf")
  
  for (i in 1:n_pages) {
    
    print(ggplot(eBird_final_data, aes(avg_rad)) +
            geom_density(alpha = 0.4, fill="blue") + 
            scale_x_continuous(trans='log10', breaks=c(0.0001, 0.0100, 1.0000, 100.0000), labels=c("0.0001", "0.01", "1.0", "100.0")) + 
            theme_classic() +
            xlab(bquote('Average radiance ('* 'nW' ~cm^-2~sr^-1*')'))+
            ylab("Density") +
            theme(axis.text.x=element_text(hjust=0.8)) +
            facet_wrap_paginate(~COMMON_NAME, ncol = 3, nrow = 5, scales="free_y", page = i))
    
  }
  
  dev.off()
  






  
