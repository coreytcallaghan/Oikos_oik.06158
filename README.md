# Phylogenetically controlled models of adaptations in urban birds of Australia

This repository contains code needed to reproduce the analysis for the article: Callaghan et al. 2019

## Instructions

### Install relevant software

All analyses were done in `R`. You need to download this repository, and then open an R session with working directory set to the root of the project.

To compile the paper, we use the [remake](https://github.com/richfitz/remake) package for R. You can install remake using the `devtools` package (run `install.packages("devtools")` to install devtools if needed):

```r
devtools::install_github("richfitz/remake", dependencies=TRUE)
```

### Recreating the figures and paper

To generate all figures and analyses simply do:

```r
remake::make()
```
