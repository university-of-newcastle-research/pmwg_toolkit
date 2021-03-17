### Functions to return and plot the variance over time.
#### Contributed by Niek Stevenson and Reilly Innes


pmwg_variance <- function(samples, plot = T){
  diagonal<-apply(tmp$samples$theta_sig,3,diag)
  if (plot){
    matplot(log(t(diagonal)), type="l")
  }
  x<-diag(tmp$samples$theta_sig[,,samples$samples$idx])
  names(x)<-par_names
  return(x)
  
}
