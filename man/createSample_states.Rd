% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulations.R
\name{createSample_states}
\alias{createSample_states}
\title{Create sample state matrix}
\usage{
createSample_states(state_info)
}
\arguments{
\item{state_info}{dataframe containing the information of tip names and state.
State is a character associating each tip to a deme of the structured
coalescent model.
#'}
}
\value{
matrix in which each column represent a state (or deme name) and each
row is a name for the tip of the phylogenetic tree. Each row should sum to 1.
}
\description{
Function that creates a matrix with the probability of each tip of the
phylogenetic tree be associate to a deme. Each row of the matrix should sum
to 1.
}
\examples{
tip_data <- create_tip_names()
sampleStates <- createSample_states(tip_data)
}
