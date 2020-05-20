library(lhs)

#' Sampler to sample parameters from a uniform distribution
#'
#' Funtion that will sample parameter values from a uniform distribution based
#' on latin hypercube sampling.
#'
#' @param n numeric for the number of samples. Default is set to 100.
#'
#' @return dataframe for parameters sampled.
#'
#' @details This function will sample from a uniform distribution parameter
#'  values for b, initS (initial size of susceptibles), import (importation rate),
#'   st (start time) and E (initial size of exposed individuals).
#'  Boundaries for each parameter:
#'  b: min = 15, max = 25
#'  initS: min = 1e4, max = 1e6
#'  import: min = 1, max = 10
#'  st: min = 2020.10, max = 2020.15
#'  E: min = 1, max = 30
#' @export
#'
#' @examples
#' #the example below will sample 100 parameter values for each b, initS, import,
#' and st from a uniform ditribution
#' parameters <- sampler()
sampler <- function(n = 100){

  d = 5 # number of dimention (the number of parameters that we will estimate)
  params <- randomLHS(n, d)
  params[,1] <- qunif(params[,1], min = 15, max = 25) # b
  params[,2] <- qunif(params[,2], min = 1e4, max = 1e6) # initS
  params[,3] <- qunif(params[,3], min = 1, max = 10) # import rate
  params[,4] <- qunif(params[,4], min = 2020.10, max = 2020.15) # st
  params[,5] <- qunif(params[,5], min = 1, max = 30) # E


  params <- data.frame(params)
  colnames(params) <- c("b", "S", "importRate", "st", 'E')


  return (params)

}


#' Create tip names
#'
#' This function will create names for terminal branches (tip) in the
#' phylogenetic tree. It will also associate each tip to a deme from the
#' structured coalescent model.
#'
#' @param region_n numeric which is defined as the number of tips for the region
#'  data. Region data are data for the region of interest. Default is set to 100.
#' @param exog_n numeric which is defined as the number of tips for exogenous
#'  data. Exogenous data are data for other regions that are not the region of
#'  interest. Default is set to 50.
#'
#' @return dataframe for tip names and states associated to each tip name
#'
#' @export
#'
#' @examples
#' tip_data <- create_tip_names()
create_tip_names <- function(region_n = 100, exog_n = 50){

  region_tip <- seq(1:region_n)
  region_tip <- paste("Seq", region_tip, sep = "")
  region_tip <- paste(region_tip, "Il", sep = "_")

  region_tip <- data.frame(tip.names = region_tip)
  region_tip["States"] <- "Il"


  exog_tip <- seq(1:exog_n)
  exog_tip <- paste("Seq", exog_tip, sep = "")
  exog_tip <- paste(exog_tip, "exog", sep = "_")

  exog_tip <- data.frame(tip.names = exog_tip)
  exog_tip["States"] <- "exog"

  all_data <- rbind(region_tip, exog_tip)
  all_data$tip.names <- as.character(all_data$tip.names)

  return(all_data)

}


#' Create sample state matrix
#'
#' Function that creates a matrix with the probability of each tip of the
#' phylogenetic tree be associate to a deme. Each row of the matrix should sum
#' to 1.
#'
#' @param state_info dataframe containing the information of tip names and state.
#' State is a character associating each tip to a deme of the structured
#' coalescent model.
#' #'
#' @return matrix in which each column represent a state (or deme name) and each
#' row is a name for the tip of the phylogenetic tree. Each row should sum to 1.
#' @export
#'
#' @examples
#' tip_data <- create_tip_names()
#' sampleStates <- createSample_states(tip_data)
createSample_states <- function(state_info){

  E <-  Il <- Ih <- rep(0, 150)
  exog <- rep(0, 150)


  # sample states to be used in the tree simulation function
  E[state_info$States == "E"] <- 1
  Il[state_info$States == "Il"] <- 1
  Ih[state_info$States == "Ih"] <- 1
  exog[state_info$States == "exog"] <- 1

  sampleStates <- cbind(E, Il, Ih, exog)
  rownames(sampleStates) <- state_info$tip.name

  return(sampleStates)
}


#' Create time for terminal branches in the phylogenetic tree
#'
#' Function to sample time for each terminal branch in a phylogenetic tree.
#' This sampling will be stratified through time, which is equal time increments
#' between samples. Samples here is each terminal branch in the phylogenetic tree.
#'
#' @param time_region numeric which is the start time for sampling
#' @param time_exog numeric which is the maximum time for sampling
#' @inheritParams createSample_states
#'
#' @return numeric vector for sampling times. Each value in the vector is
#' named using the name used for the tips in the phylogenetic tree.
#'
#' @export
#'
#'
#' @examples
#' tip_data <- create_tip_names()
#' sampleTimes <- createSample_times(time_region = 2020.15,
#'                                   time_exog = 2020.0,
#'                                   state_info = tip_data)
createSample_times <- function(time_region, time_exog, state_info){

  # sample times
  Times_region <- seq(time_region, 2020.249, length.out = 100)

  Times_exog <- seq(time_exog, 2020.249, length.out = 50)

  sampleTimes <- c(Times_region, Times_exog)
  names(sampleTimes) <- state_info$tip.names

  return(sampleTimes)
}

#' Rename tip names
#'
#' Function to rename tip names to include information of sample times
#'
#' @param tip_name tip name after spliting the name into two: first part contains
#' the tip name, second part contains the state name. For example: if tip name is
#' "Seq1_exog", the splitted form will be "Seq1" "exog".
#' @param tip_time time for correspondent tip name
#'
#' @return new tip name
#' @export
#'
#' @examples
#' TO DO
rename_tips <- function(tip_name, tip_time){
  name <- tip_name
  seq_name <- name[[1]][1]
  state <- name[2]
  time <- round(tip_time[[1]], 3)
  paste(paste(seq_name, time, sep = "|"), state, sep="|_")

}


# parameter for seqgen analyses
# seqgen reference (http://tree.bio.ed.ac.uk/software/seqgen/)

#' Simulate DNA sequence alignment
#'
#' Function to simulate DNA sequence alignment based on the HKY DNA substitution
#' model
#'
#' @param tree object of class phylo
#'
#' @return DNA sequence alignment in PHYLIP format
#' @details This function will simulate DNA sequence alignment using seq-gen
#' \url{http://tree.bio.ed.ac.uk/software/seqgen/}.
#' Parameters used in this function are:
#' -mHKY: selects the model. Here we select HKY DNA substitution model
#' -l29500: selects the length of the DNA sequence alignment to simulate. Here
#'   alignments will have 29500 base pairs.
#' -t5.5: selects the transition/tranversion rate
#' -f0.3,0.2,0.2,0.3 selects the frequency of each nucleotide in the sequence
#'   alignment in the order of A, C, G, T.
#'  Parameters -t and -f must be set when simulating HKY substitution models
#' -s0.001 scale branch lengths in order to make then equal to the expected
#'   number of substitution per site for each branch.
#' -or: Return alignment as a relaxed PHYLIP format. With this option sequence
#'   names can have more than 10 characters.
#' @export
#'
#' @examples
#' TO DO
simulate_dna <- function(tree){

  # convert tree topology into newick tree format
  newick <- write.tree(tree)
  # simulate DNA sequence alignment using seq-gen
  dna <- seqgen(opts = "-mHKY -l29500 -t5.5 -f0.3,0.2,0.2,0.3 -s0.001 -or", newick.tree = newick)

  return(dna)


}

#' Save DNA sequence alignment in a file
#'
#' Funtion to save seq-gen DNA sequence alignment into a file.
#'
#' @param seqgen_ali object of class seqgen
#' @param filename name for the file to save sequence alignment. Note that
#'  this name should NOT have the extention .phy, for example.
#'
#' @return
#' @export
#'
#' @examples
#' TO DO
write_ali <- function(seqgen_ali, filename){

  filename0 = paste(filename, "fasta", sep = ".")
  filename = paste(filename, "phy", sep = ".")
  cat(seqgen_ali, file = filename, sep = "\n")
  d = read.dna(  file = filename, format = 'sequential'  )
  write.dna( d, file = filename0 , format = 'fasta' )
}
