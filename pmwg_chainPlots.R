### Plotting function for paramater 'chains'
#### Contributed by Niek Stevenson and Reilly Innes


pmwg_chainPlots <- function(samples, subjectParPlot = T, parameterPlot = T, subjectLLPlot = T){
  if (subjectParPlot){
    par(mfrow = c(2, ceiling(samples$n_pars/2)))
    for (par in samples$par_names){
      matplot(t(samples$samples$alpha[par,,]),type="l", main = par, xlab = "samples", ylab = "ParameterValue")
    } 
  }
  par(mfrow=c(1,1))
  if(parameterPlot) matplot(t(samples$samples$theta_mu), type="l", main = "Paramater chains", ylab = "Parameter Value", xlab = "samples")
  if(subjectLLPlot) matplot(t(samples$samples$subj_ll), type="l", main = "LogLikelihood chains per subject", ylab = "LogLikelihood", xlab = "samples")
  if(sum(subjectParPlot, parameterPlot, subjectLLPlot) > 1) print('plots presented behind eachother')
  
}