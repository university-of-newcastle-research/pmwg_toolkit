### Plotting function for paramater histograms
#### Contributed by Niek Stevenson 

#requires 'bayesplot' package
pmwg_parHist <- function(samples){
  chains <- as.array(as_mcmc(samples))
  mcmc_hist(chains)
}

