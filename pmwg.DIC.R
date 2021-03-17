### function for DIC from PMwG samples
#### Contributed by Reilly Innes

pmwg.DIC=function(sampled,pD=FALSE){
  nsubj=length(unique(sampled$data$subject))
  
  # the mean likelihood of the overall (sampled-stage) model, separately for each subject
  mean.like <- apply(sampled$samples$subj_ll[,sampled$samples$stage=="sample"],1,mean)
  
  # the mean of each parameter across iterations. Keep dimensions for parameters and subjects
  mean.params <- t(apply(sampled$samples$alpha[,,sampled$samples$stage=="sample"],1:2,mean))
  
  # i name mean.params here so it can be used by the log_like function
  colnames(mean.params)<-sampled$par_names
  
  # log-likelihood for each subject using their mean parameter vector
  mean.params.like <- numeric(ncol(mean.params))
  data <- transform(sampled$data, subject=match(subject, unique(subject)))
  for (j in 1:nsubj) {
    mean.params.like[j] <- sampled$ll_func(mean.params[j,], data=data[data$subject==j,], sample=FALSE)
  }
  
  # Effective number of parameters
  pD <- sum(-2*mean.like + 2*mean.params.like)
  
  # Deviance Information Criterion
  DIC <- sum(-4*mean.like + 2*mean.params.like)
  
  if (pD){
    return(c("DIC"=DIC,"effective parameters"=pD))
  }else{
    return(DIC)
  }
  
}
