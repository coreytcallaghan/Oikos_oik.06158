Traits that affect urbanness in birds
================

This repository contains code needed to reproduce the analysis for the article: Callaghan et al. 2019: https://onlinelibrary.wiley.com/doi/abs/10.1111/oik.06158

Instructions
------------

### Install relevant software

All analyses were done in `R`. You need to download this repository, and then open an R session with working directory set to the root of the project.

To compile the paper, we use the [remake](https://github.com/richfitz/remake) package for R. You can install remake using the `devtools` package (run `install.packages("devtools")` to install devtools if needed):

``` r
devtools::install_github("richfitz/remake", dependencies=TRUE)
devtools::install_github("liamrevell/phytools", dependencies=TRUE)
```

### Recreating the figures and paper

To generate all figures and analyses simply do:

``` r
remake::make()
```

    ## [  LOAD ]

    ## [  READ ]                                              |  # loading sou...

    ## <  MAKE > all

    ## [    OK ] Data/phylo/EricsonStage2_0001_1000.zip

    ## [    OK ] traits

    ## [    OK ] ebird_data

    ## [    OK ] urbanness_table

    ## [    OK ] figures/param_plot_averaged_results.pdf

    ## [    OK ] bird_tree

    ## [    OK ] list_of_bird_trees

    ## [    OK ] response_variables

    ## [    OK ] analysis_data

    ## [    OK ] aus_bird_tree

    ## [    OK ] global_model

    ## [    OK ] phy_mod_rescaled

    ## [    OK ] list_phy_models

    ## [    OK ] figures/bird_urbanness_phylo.pdf

    ## [    OK ] figures/urbanness.pdf

    ## [    OK ] figures/corrplot_of_continuous_variables.pdf

    ## [    OK ] figures/density_plot_per_species.pdf

    ## [    OK ] figures/distribution_response_variables.pdf

    ## [    OK ] figures/histogram_plot_per_species.pdf

    ## [    OK ] tables/collinearity_investigation.pdf

    ## [    OK ] figures/param_plot_global_model.pdf

    ## [    OK ] figures/phy_v_non_phy.pdf

    ## [    OK ] figures/brains.pdf

    ## [    OK ] figures/param_plot_phylo_model.pdf

    ## [    OK ] reports/phylo_report.pdf

    ## [ ----- ] all

### Understanding the workflow of the analysis

Here is a figure that describes the workflow from the cannonical data files to the final figures and tables. In the following figure, internal data steps (intermediate files) are shown in blue and the final outputs are shown in red. Each arrow is performed by a function written in R and contained in this repository.

``` r
remake::diagram(remake::make())
```

![](workflow.png)
