# Simulations for SARS-CoV-2 

This repository contains scripts to simulate phylogenetic trees and DNA sequence alignment similar to those observed for SARS-CoV-2.

You can find a script to simulate phylogenetic trees [here](https://github.com/thednainus/sarscov2simulations/blob/master/Coalescent_simulations/Tree_Simulations.R).

You can find a script to simulate DNA sequence alignments using a simulated phylogenetic tree [here](https://github.com/thednainus/sarscov2simulations/blob/master/Coalescent_simulations/DNA_seqali_simulations.R)

The script that contains the epidemiological model used to simulate phyloegentic trees are [here](https://github.com/thednainus/sarscov2simulations/blob/master/Coalescent_simulations/seijrRmodel.R).


## System Requirement

### Operating System
This R package was developed and tested on a macOS High Sierra version 10.13.6


### Additional R Packages Required

Note these are the version we used, however the scripts should work with other R package versions.

* **devtools** version 2.3.2 
* **lhs** version 1.1.1
* **phyclust** version 0.1.29
* **ape** version 5.4.1
* [**phydynR** version 0.2.0](https://github.com/emvolz-phylodynamics/phydynR)


## How to install it?

You will need to install all abovementioned R packages as below (if not alreay installed in your computer):

```r
install.packages("devtools")
install.packages("lhs")
install.packages("phyclust")
install.packages("ape")

library(devtools)
# to install phydynR
devtools::install_github("emvolz-phylodynamics/phydynR")
# If installing phydynR from install_github does not work, and you get the 
#"#error "*** C++11 compiler required; enable C++11 mode in your compiler, or use an earlier version of Armadillo"
# then type type in R the following (but first remove the comment symbol #) and then try install phydynR again
# Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
# if you still get an error, you may need to try
# devtools::install_github("emvolz-phylodynamics/phydynR", force = TRUE)


# to install this R package
devtools::install_github(repo = "thednainus/sarscov2simulations")

# Alternatively, you can download the zip file from GitHub
# Unzip the file and run the following command on your computer (don't forget to remove the comment symbol #)
# install.packages(path_to_file, repos = NULL, type="source")
# where path_to_file is the path to the unziped directory that you downloaded from GitHub


# After installing the package:
library(sarscov2simulations)
```

# Output Example

The aim of these scprit was to generate DNA sequence alignment consistent to a epidemiological model and a phylogenetic tree. This sequences alignment was used in BEAST to 
estimate the parameters of the epidemiological model (see [paper](https://www.medrxiv.org/content/10.1101/2020.09.15.20194258v1)).

After running script [DNA_seqali_simulations.R](https://github.com/thednainus/sarscov2simulations/blob/master/Coalescent_simulations/DNA_seqali_simulations.R), you should get sequence alignment as exemplified [here](https://github.com/thednainus/sarscov2simulations/tree/master/Output_Example).
Note that sequence name is in the form of **simSeq99|2020.248|_Il**
where **simSeq99** is the sequence name; 
**2020.248** is sampling date in decimal form and; 
**Il** is the compartment the "individual" is classified, in this case it is infected with low rate of transmission.

