# Simulations for SARS-CoV-2 

This repository contains scripts to simulate phylogenetic trees and DNA sequences similar to observed for SARS-CoV-2.

You can find a script to simulate phylogenetic trees [here](https://github.com/thednainus/sarscov2simulations/blob/master/Coalescent_simulations/Tree_Simulations.R).

You can find a script to simulate DNA sequences based on the simulated phylogenetic tree [here](https://github.com/thednainus/sarscov2simulations/blob/master/Coalescent_simulations/DNA_seqali_simulations.R)

The script that contains the epidemiological model used to simulate phyloegentic trees are [here](https://github.com/thednainus/sarscov2simulations/blob/master/Coalescent_simulations/seijrRmodel.R). You will need the [phydynR](https://github.com/emvolz-phylodynamics/phydynR) package to use these scripts.

## How to install it?

First you will need to install package **devtools** if you don't have it installed already.

```r
devtools::install_github(repo = "thednainus/sarscov2simulations")

# After installing the package:
library(senegalHIVmodel)
```
