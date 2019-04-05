NumberOffspring = function(Nt, K, r0, g, l, adultmort, agestage, coffprop, poorenv, fixedenv, lossperct, population){
  if(g >= poorenv & g <= fixedenv){
    K = K - (K * lossperct)
  }
  
  # calculate popsize based on pre-reproduction census
  Ntt = Nt*(1+r0*(1-(Nt/K))) #logistic
  
  # add DI variance
  Nt1 = round(rnorm(1, Ntt, l), 0)
  
  # determine number of offspring to produce in captive and wild poulations 
  adultmortnumber = Nt - round(Nt*adultmort, 0)
  totaloffspring  = Nt1 - adultmortnumber                                 #number that will die due to adult mortality
  
  #set up object to return values
  numberoffspring = matrix(nrow=1, ncol=2)
  colnames(numberoffspring) = c("captiveoffspring", "wildoffspring")
  
  #determine number of captive born offspring needed, if durring captive breeding
  numberoffspring[1,1] = round((coffprop*totaloffspring),0)
  numberoffspring[1,2] = totaloffspring 

  return(numberoffspring)
}
