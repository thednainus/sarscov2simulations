# Script to simulate DNA sequence alignment for simulated phylogenetic trees

library(phyclust)
library(ape)
library(sarscov2simulations)

# The line below will source the R script that will generate the
# structured coalescent model and simulate phylogenetic trees
#setwd("Coalescent_simulations/")
source("Tree_Simulations.R")


# Simulate DNA sequence alignment
set.seed(29052019)
sim_dna <- lapply(tres, function(x) simulate_dna(x))

size <- length(sim_dna)
names(sim_dna) <- paste("sim", 1:size, sep="")

# function that will save each seg-gen alignment to a different file
lapply(names(sim_dna), function(x) write_ali(sim_dna[[x]], x))

# Function to remove duplicated sequences from alignment
dedup <- function(fastafn ){
  d = read.dna( fastafn, 'fasta')
  D <- as.matrix( dist.dna( d, 'F84' ) )
  sids = rownames(D)
  sts <- sapply( strsplit( sids, '\\|' ), function(x) tail(x,2)[1] )
  sts <- setNames( as.numeric( sts ), sids )

  {
    drop <- c()
    keep <- rownames(D)
    ct = cutree( hclust( as.dist(D) ) , h = 1e-6  )
    for ( k in unique( ct )){
      s = names( ct )[ ct == k ]
      if ( length( s ) > 1 ){
        u = s[ which.max(sts[s])[1]  ]
        drop <- c( drop , setdiff( s, u ) )
      }
    }
    if ( length( drop  ) > 1 ){
      keep <- setdiff( rownames(D), drop )
      D <- D[keep, keep]
      sts <- sts[ rownames(D)]
    }
    cat( paste( 'Removed', length( drop ), 'identical sequences\n' ) )
  }
  x = strsplit( fastafn, '\\.')[[1]]
  ofn = paste(sep='.', head(x,length(x)-1), '_dedup.fasta')
  #~ browser()
  write.dna( d[keep, ],file= ofn, format= 'fasta' )
  invisible(d[keep,])
}


files = paste0( 'sim', 1:size, '.fasta')

ds = lapply( files, dedup )


