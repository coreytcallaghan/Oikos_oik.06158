### A script for summary of the results and to create final figures using the final set of species

## first dump the remake environment
remake::dump_environment()

## get the final dataframe which was used for analysis
## code copies from the non_phylo_model.Rmd
response_variables$binom <- response_variables$SCIENTIFIC_NAME_tree

df <- left_join(traits,response_variables, by="binom") %>%
  filter(SCIENTIFIC_NAME_tree != "not_treated_as_species") %>%
  # drop columns that we aren't inerested in
  dplyr::select(-brain_size)

df2 <- df %>%
  filter_all(all_vars(!is.na(.)))

## total number of species included in models
length(unique(df2))

## summary of urbanization index
summary(df2$urban_median)

sd(df2$urban_median)

min(df2$urban_median)

max(df2$urban_median)


## Figure 1
## A figure showing the distribution of 5 different species included in the analysis
## Need to load the species_urban .RData file first
load('Data/eBird data/Final eBird data for analysis/species_urban.RData')

species_to_plot <- c("Common Myna", "Scaly-breasted Lorikeet", "Welcome Swallow", 
                     "Banded Honeyeater", "White-winged Fairywren")

Figure1 <- species_urban %>%
  filter(COMMON_NAME %in% species_to_plot) %>%
  ggplot(., aes(avg_rad, fill=COMMON_NAME)) +
  geom_density(alpha = 0.7) + 
  scale_x_log10() + 
  theme_classic() +
  xlab("Average radiance") +
  ylab("Density")+
  guides(fill=guide_legend(title="  Common Name"))

pdf("final figures for paper/Figure1.pdf", width=5.5, height=4.5)
print(Figure1)
dev.off()


## Figure 2
## Figure 2 is the distribution of response variable for those species included in the analyses
## Use the df2 dataframe from above
## I want to add a line for each of the example species on the distribution

species_to_plot <- c("Common Myna", "Scaly-breasted Lorikeet", "Welcome Swallow", 
                     "Banded Honeyeater", "White-winged Fairywren")

species_values <- df2 %>%
  filter(COMMON_NAME_ebird %in% species_to_plot) %>%
  dplyr::select(COMMON_NAME_ebird, urban_median) %>%
  mutate(urban_median = log(urban_median))

Figure2 <- ggplot(data=df2, aes(log(urban_median))) +
  geom_histogram(bins = 70, 
                 col="black", 
                 fill="green", 
                 alpha = 0.3)+
  theme_classic()+
  xlab("Value")+
  ylab("Count")+
  geom_vline(data=species_values, aes(xintercept=urban_median), color='red', size=1.62, alpha=0.9)

pdf("final figures for paper/Figure2.pdf", width=5.5, height=4.5)
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
               to = "final figures for paper/Figure3.pdf")









