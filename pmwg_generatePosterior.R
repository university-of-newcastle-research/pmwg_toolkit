### function to sample from the posterior
#### contributed by Reilly Innes


### from the sampled object, uses the LL (sample =TRUE) to create data. Relies on LL being correct. Can have errors with lists
pmwg_generatePosterior <- function(sampled, n, rbind.data=TRUE){
  n.posterior=n # Number of parameter samples from posterior distribution.
  pp.data=list()
  S = sampled$n_subjects
  data=sampled$data
  sampled_stage = length(sampled$samples$stage[sampled$samples$stage=="sample"])
  for (s in 1:S) {
    print(paste0("subject ", s))
    iterations=round(seq(from=(sampled$samples$idx-sampled_stage) , to=sampled$samples$idx, length.out=n.posterior))
    for (i in 1:length(iterations)) {
      print(i)
      x <- sampled$samples$alpha[,s,iterations[i]]
      names(x) <- sampled$par_names
      tmp=sampled$ll_func(x=x,
                          data= data[data$subject == unique(data$subject)[s], ],
                          sample=TRUE)
      if (i==1) {
        pp.data[[s]]=cbind(pp_iter = i,tmp)
      } else {
        pp.data[[s]]=rbind(pp.data[[s]],cbind(pp_iter = i,tmp))
      }
    }
    
  }
  if (rbind.data){
    tidy_pp_data <- do.call(rbind, pp_data)  
    return(tidy_pp_data)
  }
  else {
    return(pp_data)
  }
}

