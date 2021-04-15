### Plotting function for paramater histograms
#### Contributed by Niek Stevenson 
library(bayesplot)
library(pmwg)
library(tidyr)
library(dplyr)
library(ggplot2)

#requires 'bayesplot' package
pmwg_parHist <- function(samples){
  chains <- as.array(as_mcmc(samples))
  mcmc_hist(chains)
}

pmwg_parHistPrior<- function(samples){
  theta <- t(sampled$samples$theta_mu[sampled$samples$stage=="sample"])
  theta<-as.data.frame(theta)
  theta <- pivot_longer(theta, cols = everything(), names_to = "pars", values_to = "estimate" )
  prior_mean <- sampled$prior$theta_mu_mean
  prior_var <- diag(sampled$prior$theta_mu_var)
  priors <- as.data.frame(cbind(prior_mean,prior_var))
  priors$pars <- sampled$par_names
  theta <- left_join(theta, priors, by="pars")
  
  ggplot(theta, aes(estimate))+
    geom_histogram(aes(y =..density..))+
    stat_function(fun = dnorm, args = list(mean = prior_mean, sd = prior_var))+
    facet_wrap(~pars, scales = "free_y")+
    theme_bw()
}
