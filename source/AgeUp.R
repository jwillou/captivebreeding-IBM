AgeUp = function(population, alldead){
  dead  = population[population[,9] == 0, , drop=FALSE]          # remove dead indiviudals
  cdead = population[population[,9] ==-99, , drop=FALSE]          # remove captive caught individuals
  if(length(population[population[,9] == 1, 1])>0){
    population = population[population[,9] == 1, ]   # isolate remaining alive individuals
    #increase age
    if(!is.null(nrow(population))){
      population[, 2] = population[, 2] + 1          # add 1 year to all alive individuals
    }
  }else{
    alldead <<- 1
  }
  
  #recombine population parts into single object
  if(length(cdead[,1])>0){
    population = rbind(population,dead,cdead)
  }else {
    population = rbind(population,dead)
  }
  
  return(population)
}
