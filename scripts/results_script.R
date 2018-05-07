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


## Figure 1
## A figure showing the distribution of 5 different species included in the analysis
## Need to load the species_urban .RData file first
load('Data/eBird data/Final eBird data for analysis/species_urban.RData')

species_to_plot <- c("Dusky Moorhen",
                     "Yellow-tailed Black-Cockatoo",
                     "Hooded Robin",
                     "Noisy Friarbird",
                     "Squatter Pigeon")

Figure1 <- species_urban %>%
  filter(COMMON_NAME %in% species_to_plot) %>%
  ggplot(., aes(avg_rad, fill=COMMON_NAME)) +
  geom_density(alpha = 0.7) + 
  scale_x_log10() + 
  theme_classic() +
  xlab("log(Average radiance)") +
  ylab("Density")+
  guides(fill=guide_legend(title="  Common Name")) +
  theme(legend.position = c(0.26, 0.8))+
  theme(axis.text.x=element_text(hjust=0.7))

pdf("final figures & tables for ms/Figure1.pdf", width=5.5, height=4.5)
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
  dplyr::select(COMMON_NAME_ebird, urban_median) %>%
  mutate(urban_median = log(urban_median))

Figure2 <- ggplot(data=analysis_data, aes(log(urban_median))) +
  geom_histogram(bins = 70, 
                 col="black", 
                 fill="green", 
                 alpha = 0.3)+
  theme_classic()+
  xlab("log(Urbanization index)")+
  ylab("Count")+
  geom_vline(data=species_values, aes(xintercept=urban_median), color='red', size=1.62, alpha=0.9)

pdf("final figures & tables for ms/Figure2.pdf", width=5.5, height=4.5)
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
               to = "final figures & tables for ms/Figure3.pdf")



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
  dplyr::select(term2, estimate, lwr, upr, p.value, significance, trend) %>%
  rename(variable=term2) %>%
  rename(p_value=p.value) %>%
  mutate(model = "Non-phylo global model")

## Non-phylo model-averaged
model_results <- readRDS("Data/dredged_model_averaged_param_est.rds")
summary <- readRDS("Data/dredged_model_summary_results.rds")

p_values <- data.frame(p_value=summary$coefmat.full[,5]) %>%
  rownames_to_column("variable")

estimates_non_phylo_averaged <- model_results %>%
    inner_join(., p_values, by="variable") %>%
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
    dplyr::select(variable2, estimate, lwr, upr, p_value, significance, trend) %>%
    rename(variable=variable2) %>%
    mutate(model = "Non-phylo model-averaged")

## Phylo model global
results <- data.frame(estimate = phy_mod_rescaled$coefficients, 
                      lwr = confint(phy_mod_rescaled)[,1],
                      upr = confint(phy_mod_rescaled)[,2],
                      p_value = summary(phy_mod_rescaled)$coefficients[,4],
                      stringsAsFactors = FALSE)


estimates_phylo <- results %>%
    rownames_to_column("term") %>%
    filter(term != "(Intercept)") %>%
    arrange(desc(estimate)) %>%
    mutate(term2 = c("Feeding habitat generalism",
                     "Diet generalism",
                     "log(Clutch size)",
                     "Habitat - agricultural",
                     "Brain residual",
                     "Breeding habitat generalism",
                     "log(Body size)",
                     "Plant eater",
                     "Nest generalism",
                     "Feeding aggregation \n (solitary, pairs, & flocks)",
                     "Movement - nomadic/irruptive",
                     "Movement - migratory",
                     "Feeding aggregation \n (pairs & flocks)",
                     "Ground-nesting",
                     "Feeding aggregation \n (solitary & pairs)",
                     "Habitat - tree/forest",
                     "Nest aggregation \n (solitary)",
                     "Range size (1000s km2)",
                     "Cooperative breeding",
                     "Carrion eater",
                     "Nest aggregation \n (colonial)",
                     "Hollow-nesting",
                     "Feeding aggregation \n (solitary & flocks)",
                     "Granivore",
                     "Feeding aggregation \n (solitary)",
                     "Nest aggregation \n (none)",
                     "Insectivore",
                     "Habitat - grass/shrubland",
                     "Feeding aggregation \n (pairs)")) %>%
    arrange(estimate) %>%
    mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
    mutate(significance=ifelse(.$p_value <= 0.05, "Significant", "Non-significant")) %>%
    dplyr::select(term2, estimate, lwr, upr, p_value, significance, trend) %>%
    rename(variable=term2) %>%
    mutate(model = "Phylo global model")

# Phylo model-averaged results
model_results <- readRDS("Data/PHYLO_dredged_model_averaged_param_est.rds")
summary <- readRDS("Data/PHYLO_dredged_model_summary_results.rds")

p_values <- data.frame(p_value=summary$coefmat.full[,4]) %>%
  rownames_to_column("variable")

estimates_phylo_averaged <- model_results %>%
    inner_join(., p_values, by="variable") %>%
    filter(variable != "(Intercept)") %>%
    droplevels() %>%
    arrange(desc(estimate)) %>%
    mutate(variable2 = c("Feeding habitat generalism",
                         "Brain residual",
                         "log(Body size)",
                         "log(Clutch size)",
                         "Breeding habitat generalism",
                         "Nest aggregation \n (colonial)",
                         "Diet generalism",
                         "Plant eater",
                         "Habitat - agricultural",
                         "Insectivore",
                         "Nest aggregation \n (solitary)",
                         "Movement - migratory",
                         "Habitat - tree/forest",
                         "Ground-nesting",
                         "Movement - nomadic/irruptive",
                         "Habitat - grass/shrubland",
                         "Hollow-nesting",
                         "Granivore",
                         "Nest generalism",
                         "Cooperative breeding",
                         "Carrion eater",
                         "Range size (1000s km2)",
                         "Nest aggregation \n (none)")) %>%
    arrange(estimate) %>%
    mutate(trend=ifelse(.$estimate >0, "positive", "negative")) %>%
    mutate(significance=ifelse(.$p_value <= 0.05, "Significant", "Non-significant")) %>%
    mutate(model = "Phylo model-averaged") %>%
    dplyr::select(variable2, estimate, lwr, upr, p_value, significance, trend, model) %>%
    rename(variable = variable2)



## Now need to merge the dfs together
param_estimates <- bind_rows(estimates_non_phylo, estimates_phylo, estimates_non_phylo_averaged, estimates_phylo_averaged)


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
    theme(legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"))+
    theme(legend.position=c(0.16, 0.88))+
    guides(colour = guide_legend(title="                  Model",
                                 override.aes = list(size=2, shape=NA)))+
    theme(axis.text.x=element_text(size=12))+
    theme(axis.title.y=element_text(size=16))+
    theme(axis.text.y=element_text(size=7.5))
    
  pdf("final figures & tables for ms/Figure4.pdf", width=9.45, height=6.4)
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
             to = "final figures & tables for ms/Figure5.pdf")
  
  
##########################################
###### Supplementary Figures #############
##########################################

## Figure S1
my.file.copy(from = "figures/corrplot_of_continuous_variables.pdf",
             to = "final figures & tables for ms/FigureS1.pdf")


## Figure S2
my.file.copy(from = "figures/accounting_for_phylo_uncertainty.pdf",
             to = "final figures & tables for ms/FigureS2.pdf")



##########################################
###### Supplementary Tables ##############
##########################################

## Table S1
my.file.copy(from = "tables/collinearity_investigation.pdf",
             to = "final figures & tables for ms/TableS1.pdf")
  
  
