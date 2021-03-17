### function to get the IACT value for each parameter and the covariance off diagonal
#### Contributed by Reilly Innes


library(LaplacesDemon)
pmwg.iact = function(sampled){
  dim = sampled$n_pars
  n.params<- (dim*dim - dim)/2 
  n_subjects = sampled$n_subjects
  theta <- sampled$samples$theta_mu[,sampled$samples$stage=="sample"]
  alpha <- sampled$samples$alpha[,,sampled$samples$stage=="sample"]
  sig <- sampled$samples$theta_sig[,,sampled$samples$stage=="sample"]  
  tmp <- sig[,,1:100]
  
  tmp<-NULL
  for (i in 1:dim){
    tmp[i]<- IAT(theta[i,])
  }
  names(tmp)<-pars
  
  tmp2<-matrix(nrow=dim, ncol = dim)
  for (i in 1:dim){
    for (j in 1:dim){
      tmp2[i,j]<- IAT(sig[i,j,])
    }
  }
  

  colnames(tmp2)<-pars
  rownames(tmp2)<-paste0(pars,"_")
  tmp3 <- tmp2[lower.tri(tmp2)]
  index <-  which(lower.tri(tmp2), arr.ind = T)
  names(tmp3) <- paste(rownames(tmp2)[index[,1]], colnames(tmp2)[index[,2]], sep=", ")
  
  iact.table <- c(tmp,tmp3)
  
  return(iact.table)
} 