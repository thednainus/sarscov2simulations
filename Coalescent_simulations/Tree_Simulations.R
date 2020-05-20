library(phydynR)
library(lhs)
library(sarscov2simulations )
library(ggplot2)
library(reshape2)

#setwd("Coalescent_simulations/")
source("seijrRmodel.R")

#set.seed(25011979)
# sample parameters (default is set to 100)
# parameters that varies
# for the simulation test that Erik set up I used n=30
# and I set seed to set.seed(25011979)
# for simulation test done for ABC analysis test I used seed = 17091975
set.seed(17091975)
#parameters <- sampler(n = 30)
parameters <- sampler()
#parameters that are fixed
parameters["tau"] <- 74
parameters["p_h"] <- 0.2
parameters["exogGrowthRate"] <- 25
parameters["gamma0"] <- 73.0
parameters["gamma1"] <- 121.667
parameters["gammaExog"] <-44.0


# crate tip names
all_data <- create_tip_names()

# create sample states
sampleStates <- createSample_states(state_info = all_data)


# This is not a good function as it is using a lot of global variables
sim_trees <- function(parameters){

  sampleTimes <- createSample_times(time_region = parameters[["st"]],
                                    time_exog = 2020.0,
                                    state_info = all_data)

  # split the tip names into two to add information of time to tip name
  new_names <- strsplit(names(sampleTimes), split = "_")

  # new modfied names
  mod_names <- mapply(rename_tips, new_names, sampleTimes)

  # change names to sampleTimes and sampleStates
  names(sampleTimes) <- mod_names
  rownames(sampleStates) <- mod_names

  # initial conditions
	x0 <- c( E = parameters[['E']] , Il = 1.0E-8, Ih = 1.0E-8, exog = 1.75 , #6.5846e-3,
           R = 0.0, S = parameters[["S"]], infections = 0.0)

  #simulate trees
  tre <- sim.co.tree(as.list(parameters[c("b", "importRate", "tau", "p_h", "exogGrowthRate", "gamma0", "gamma1", "gammaExog")]),
                     demographic.process.model = dm,
                     x0 = x0,
                     t0 = 2019.92,
                     sampleTimes = sampleTimes,
                     sampleStates = sampleStates,
                     res = 1000)
}

# create a list of simulated phylogenetic trees
#tres <- apply(parameters, 1, FUN = sim_trees)
# seed set.seed(25011979) was used for Erik simulations
# for ABC simulations I used seed 17091975
set.seed(17091975)
# the code line below was used for simulations set up by Erik
#tres <- apply(parameters[c(3,5,8,9,17),], 1, FUN = sim_trees)
tres <- apply(parameters, 1, FUN = sim_trees)
#saveRDS(tres, file = "simulated_trees25011979.rds")


