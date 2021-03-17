### Functions to return and plot the covariance/correlation matrix
#### Contributed by Niek Stevenson and Reilly Innes


pmwg_covariance <- function(samples, cor = T, plot = T){
  #### Covariance matrix
  cov<-apply(samples$samples$theta_sig[,,samples$samples$idx-1000:samples$samples$idx] ,1:2, mean)
  colnames(cov)<-samples$par_names
  rownames(cov)<-samples$par_names
  if (plot){
    diagonal<-apply(samples$samples$theta_sig,3,diag)
    matplot(log(t(diagonal)), type="l", main = "covariance plot", xlab = "samples", ylab = "covariance")
    #tbh not really sure how to interpret this plot
  }
  
  if (cor) {
    cor<-cov2cor(cov) #correlation matrix
    return(list(correlation = cor, covariance = cov))
  }
}
