### functions for calculating number of unique particles per subject from PMwG samples
#### Contributed by Niek Stevenson and Reilly Innes

pmwg_particlesPerSub <- function(samples){
  x<-apply(samples$samples$alpha[1,,-1]!=sampled$samples$alpha[1,,-(sampled$samples$idx)],1,sum)
  x[order(x)]
}

## This function checks the number of new particles sampled for each subject given the stage
#requires dplyr package
pmwg_particlesPerSub_stage <- function(samples, stage){
  x<- apply(tmp$samples$subj_ll[,tmp$samples$stage==stage],1,n_distinct)
  x[order(x)]
}