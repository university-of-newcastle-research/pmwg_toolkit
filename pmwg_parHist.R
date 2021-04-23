### Plotting function for paramater histograms
#### Contributed by Niek Stevenson 
library(bayesplot)
library(pmwg)
library(tidyr)
library(dplyr)
library(ggplot2)

#requires 'bayesplot' package
pmwg_parHist <- function(samples, bins =30, prior = FALSE ){
  if (!prior){
    chains <- as.array(as_mcmc(samples))
    mcmc_hist(chains)
  } else{
    theta <- t(sampled$samples$theta_mu)
    theta<-as.data.frame(theta)
    long <- sum(sampled$samples$stage=="sample")
    theta <- theta[c((length(theta[,1])-long+1):length(theta[,1])),]
    theta <- pivot_longer(theta, cols = everything(), names_to = "pars", values_to = "estimate" )
    prior_mean <- sampled$prior$theta_mu_mean
    prior_var <- diag(sampled$prior$theta_mu_var)
    priors = NULL
    for (i in 1:sampled$n_pars){
      tmp <- rnorm(n=long, mean=prior_mean[i], sd=prior_var[i])
      tmp <- as.data.frame(tmp)      
      priors<-  c(priors, tmp[1:long,])
    }
    priors<-as.data.frame(priors)
    y <- as.factor(sampled$par_names)
    theta<-theta[order(factor(theta$pars, levels = y)),]
    theta$prior <- priors$priors
    theta$pars<- as.factor(theta$pars)
    
    
    ggplot(theta, aes(estimate))+
      geom_histogram(aes(y =..density..), bins = bins)+
      geom_density(aes(prior))+
      facet_wrap(~pars, scales = "free_y")+
      theme_bw()
  }
}

# pmwg_parHist(sampled, prior=TRUE)
# else if (var(sampled$prior$theta_mu_mean)==0 && var(diag(sampled$prior$theta_mu_var))==0){
#   theta <- t(sampled$samples$theta_mu[sampled$samples$stage=="sample"])
#   theta<-as.data.frame(theta)
#   theta <- pivot_longer(theta, cols = everything(), names_to = "pars", values_to = "estimate" )
#   prior_mean <- sampled$prior$theta_mu_mean
#   prior_var <- diag(sampled$prior$theta_mu_var)
#   priors <- as.data.frame(cbind(prior_mean,prior_var))
#   priors$pars <- sampled$par_names
#   theta <- left_join(theta, priors, by="pars")
#   
#   ggplot(theta, aes(estimate))+
#     geom_histogram(aes(y =..density..))+
#     stat_function(fun = dnorm, args = list(mean = prior_mean[1], sd = prior_var[1]))+
#     facet_wrap(~pars, scales = "free_y")+
#     theme_bw()
#   
# } 

