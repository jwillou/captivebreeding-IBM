SampleFounders = function(cappop, population, capfound, newfound, caplife, Nt, maturity, g, alldead, RRS, RRSvar){ #propnew, 
  #remove old captive individuals
  more = 0
  x = NULL
  x = try(nrow(cappop), silent=TRUE)
  if(!is.null(x)){
    cappop = cappop[cappop[,2]<=caplife,]  
  }
  
  #control how often individuals are sampled from the wild
  #if(propnew[1]==g){
  #  #remove current year from propnew list
  #  propnew <<- propnew[2:length(propnew)]
    
    cutoff = mean(population[population[,9]==1,5,drop=FALSE])
    #isolate wild born individuals
    wildborn = population[population[,9]==1,,drop=FALSE]     # alive
    wildborn = wildborn[wildborn[,6]==1,,drop=FALSE]         # born in wild not hatchery
    wildborn = wildborn[wildborn[,2]>=maturity,,drop=FALSE]  # mature
    wildborn = wildborn[wildborn[,5]>= cutoff,,drop=FALSE] #(1 - RRS - RRSvar*2)
    
    #check to see that there are some wildborn individuals available
    if(is.null(wildborn)){
      print("BAD YEAR - NO WILDBORN")
      alldead  <<- 1
      return(1)
    }  
    
    #if all new founders to be selected, empty captive population object
    if(newfound==1){
      cappop = NULL
    }
    
    #desired size of total captive population
    sizecaptive = round(capfound *nrow(wildborn),0)
    
    if(sizecaptive < 2){
      print("SF - NO CAPTIVE POPULATION")
      #alldead  <<- 1
      #return(1)
    }
    #determine additional number of captive individuals needed, snag them
    more = sizecaptive - length(cappop[,1])
    if(more > 0){
      ncaps   = sample(1:length(wildborn[,1]), nrow(wildborn)-more, replace = FALSE)
      cappop  = rbind(cappop, wildborn[-ncaps, ])
      for(x in 1:length(cappop[,1])){
        population[population[,1]==cappop[x,1], 9] = -99
        population[population[,1]==cappop[x,1],10] = g
      }
    }
  #}
  datasets = list(cappop=cappop, population=population, more=more) 
  return(datasets)
}
