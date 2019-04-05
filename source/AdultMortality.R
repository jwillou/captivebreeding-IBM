AdultMortality = function(population, g, adultmort, killimmat, maturity, parents, lifespan, repro1, alldead){
  dead  = population[population[,9] == 0, ,drop=FALSE]          # remove dead indiviudals
  cdead = population[population[,9] ==-99,,drop=FALSE]          # remove captive caught individuals
  
  #can individuals reproduce multiple times in a lifetime? 1 = yes, 0 = no
  if(repro1==1){
    parents = NULL
  }
  
  #for potentially killing all individuals 
  if(killimmat==1){
    immpopulat = NULL
    immpopulat = population[population[,2]<=0, , drop=FALSE]
    population = population[population[,2]>0, , drop=FALSE]
    if(length(population[population[,9] == 1, 1])>0){
      population = population[population[,9] == 1, ,drop=FALSE]   # isolate remaining alive individuals
      
      #find old individuals, add parents and mark as dead
      oldies = NULL
      oldies = try(population[population[,2]>=lifespan, 1])
      forsuredead = unique(c(parents, oldies))
      if(nrow(population)>1){
        population[population[,1] %in% forsuredead,9]  = 0
        population[population[,1] %in% forsuredead,10] = g
        #kill some more individuals
        nkill = round((nrow(population) * adultmort), 0) - length(forsuredead)
        if(nkill>0){
          kill  = sample(1:length(population[,1]), nkill, replace=FALSE)
          population[kill,9]  = 0
          population[kill,10] = g
        }
      }
    }else{
      alldead <<- 1
    }
    if(nrow(immpopulat)>0){
      population = rbind(immpopulat, population)
    }
  }
  #for killing only mature individuals
  if(killimmat==0){
    immpopulat = NULL
    immpopulat = population[population[,2]<maturity, , drop=FALSE]
    population = population[population[,2]>=maturity, , drop=FALSE]
    if(length(population[population[,9] == 1, 1])>0){
      population = population[population[,9] == 1, ]   # isolate remaining alive individuals
      
      #find old individuals, add parents and mark as dead
      oldies = NULL
      oldies = try(population[population[,2]>=lifespan, 1])
      forsuredead = unique(c(parents, oldies))
      if(nrow(population)>1){
        population[population[,1] %in% forsuredead,9]  = 0
        population[population[,1] %in% forsuredead,10] = g
        #kill some more individuals
        nkill = round((nrow(population) * adultmort), 0) - length(forsuredead)
        if(nkill>0){
          kill  = sample(1:length(population[,1]), nkill, replace=FALSE)
          population[kill,9]  = 0
          population[kill,10] = g
        }
      }
    }else{
      alldead <<- 1
    }
    if(nrow(immpopulat)>0){
      population = rbind(immpopulat, population)
    }
  }
  
  #recombine population parts into single object
  if(length(cdead[,1])>0){
    population = rbind(population,dead,cdead)
  }else {
    population = rbind(population,dead)
  }
  
  return(population)
}
