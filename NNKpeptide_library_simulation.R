"""
This function creates a library of size n, with sequences of length c, composed of randomized codons with NNK structure,
where N=A,C,T,G and K=A or G
"""
peptide_sim <- function(n,c) { 
  library("seqinr")
  #n=numeric(length=$) # size of your library
  #c=numeric(length=$) #length of peptides in your library
  peptide_library <- NULL
  for(i in 1:(n)){
    peptide <- NULL
    pep_count=0
    codon <- NULL
    for(x in 1:(c)){
      N <- sample(c("A","C","G","T"),2, replace=TRUE)
      K <- sample(c("G","T"),1)
      codon <- list(c(N,K))
      peptide[x] <- codon
    }
    peptide<- unlist(peptide)
    pepstring <- c2s(peptide)
    peptide_library[i] <-pepstring
    pep_count= pep_count+1
  }
  peptide_library
  
  write.csv(peptide_library, "sim_pep_lib.csv")}


