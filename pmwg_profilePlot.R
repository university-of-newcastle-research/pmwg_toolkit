#### profile plot function
#### Contributed by Reilly Innes
#### Use pwmg_profilePlot on an initiated PMwG object to ensure your likelihood functions correctly
#### This function takes in the initial theta_mu estimates and simulates a small amount of data based on these
#### plots are returned which show how the likelihood changes as the generating value changes
#### For the different generating values, we use small increments (which can be negative or positive)
#### We expect to see inverse U shaped curves, where the likelihood is most likely at the generating value and falls away as we get further from the generating value

#### NOTE: The likelihood function needs both sample = TRUE and sample = FALSE arguments to function correctly
#### Also, avoid putting protective statements (like if(any(data$rt)<t0) etc) at the beginning of the function
#### These statements should go in the if(sample=FALSE) part
require(ggplot2)
require(tidyr)
require(pmwg)

pmwg_profilePlot = function(sampler, generating_values=NULL){
  if(is.null(generating_values)){
    #create generating values based on theta_mu
    generating_values <- sampler$samples$theta_mu
    names(generating_values)<- sampler$par_names
  } else{
    names(generating_values)<-sampler$par_names
  }
  #make the test data set. here I use a tenth of the total data for speed
  test <- sampler$ll_func(x = generating_values,
                  data = sampler$data[c(1:(nrow(sampler$data)/10)),],
                  sample = TRUE)
  # this is the number of values to test and plot. 
  n_values <- 9
  tmp <- array(dim=c(n_values,sampler$n_pars))
  #here i make the increment, however, you may wish to make this smaller or larger.
  #the increment here goes from theta_mu - .2 to theta_mu + .2, with n_values length
  increment<- seq(from=-.2, to=.2, length.out = n_values)
  
  for (i in 1:sampler$n_pars){
    for (j in 1:n_values){
      #need to use all generating values except the current parameter being tested
      test_values <- generating_values
      #here we change the current parameter by adding the increment
      test_values[i] <- generating_values[i] + increment[j]
      #test the likelihood given these new values and the test data
      tmp[j, i] <- sampler$ll_func(x = test_values, data=test, sample=F)
    }
  }
  #prepare output for plotting
  colnames(tmp)<-sampler$par_names
  tmp<-as.data.frame(tmp)
  tmp <- tidyr::pivot_longer(tmp, everything(), names_to = "pars", values_to = "likelihood")
  tmp$increment <- rep(increment, each=sampler$n_pars)
  ### next, plot these values for each parameter
  ggplot2::ggplot(tmp, aes(x=increment, y= likelihood))+
    geom_point()+
    geom_line()+
    facet_wrap(~pars, scales = "free")+
    theme_bw()+
    geom_vline(xintercept = 0, color="red", alpha = 0.3)
}

# profile_plot(sampler)
# x<- log(c(1,2,2,.1,0.1,3,1,0.05))
# profile_plot(sampler, generating_values = x)

