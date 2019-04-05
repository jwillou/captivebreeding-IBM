RRSMortality = function(population, maturity){
  npopPART = NULL
  npopnew  = NULL
  dead  = population[population[,9] == 0, ,drop=FALSE]                # remove dead indiviudals
  cdead = population[population[,9] ==-99,,drop=FALSE]                # remove captive caught individuals
  population  = population[population[,9] == 1, ]          
  mats      = population[population[,2]==maturity,,drop=FALSE]
  allothers = population[population[,2]!=maturity,,drop=FALSE]

  if(nrow(mats)>0){
    for(rr in 1:nrow(mats)){
      maxvalue = mats[rr,5]
      if(maxvalue > 1){
        maxvalue = 1
      }
      mats[rr,9] = sample(x=c(0,1), size=1, replace=TRUE, prob = c(1-maxvalue, maxvalue))
    }
    population = rbind(mats, allothers)
  }    
  #recombine population parts into single object
  if(length(cdead[,1])>0){
    population = rbind(population,dead,cdead)
  }else {
    population = rbind(population,dead)
  }
  return(population)
}
