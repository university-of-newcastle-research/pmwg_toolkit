### functions to return operations on theta and alpha values of PMwG output
#### Contributed by Niek Stevenson and Reilly Innes

## returns parameter estimates for the group level (theta) - method defaults to mean
pmwg_thetaParValues <- function(sampled, method = "mean"){
  tmp <- apply(sampled$samples$theta_mu[,sampled$samples$stage=="sample"],1,method)
  round(tmp,3)
}

## returns random effects for each subject - method defaults to mean
pmwg_alphaParValues <- function(sampled, method = "mean"){
  tmp <- apply(sampled$samples$alpha[,,sampled$samples$stage=="sample"],1:2, method)
  round(tmp,3)
}

