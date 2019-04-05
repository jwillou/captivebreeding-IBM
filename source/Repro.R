Repro = function(repros, wild.pairs, RRS, RRSvar, fecundity, maturity, g, nloci, population, cappop, totalinds, migrants, selection){
  # generation starting ID number
  gstartID = totalinds
  ALLOFFSPRING = NULL
  
  # check that there are at least 2 individuals, one male and one female
  if(is.null(nrow(repros))){            return(ALLOFFSPRING)}
  if(length(repros[repros[,7]==0,1])<1){return(ALLOFFSPRING)} 
  if(length(repros[repros[,7]==1,1])<1){return(ALLOFFSPRING)}
  
  # randomly pair males and females
  size    <- min(table(repros[, 7]))
  males   <- sample(which(repros[, 7] == 1), size, replace = FALSE)
  females <- sample(which(repros[, 7] == 0), size, replace = FALSE)
  pairs   <- as.matrix(cbind(males, females))
  if(nrow(pairs)!=length(wild.pairs)){
    if(length(wild.pairs)==1){
      newpairs = matrix(nrow=1, ncol=2)
      newpairs[1,1] = sample(pairs[,1], 1)
      newpairs[1,2] = sample(pairs[,2], 1)
      pairs = newpairs
    }else{
      pairs   <- pairs[sample(1:length(pairs[, 1]), length(wild.pairs), replace = TRUE), ] #randomly select needed number of pairs; replace = TRUE in case not enough pairs
    }
  }
  
  #look for migrants and sub into list if present
  if(nrow(pairs)>0){
    if(!is.null(migrants)){
      numberIs = nrow(migrants)
      sexes    = migrants[,7]
      for(i in 1:nrow(migrants)){
        if(nrow(pairs)>=nrow(migrants)){
          if(sexes[i]==1){
            pairs[i,1] = nrow(repros) - i + 1
          }
          if(sexes[i]==0){
            pairs[i,2] = nrow(repros) - i + 1
          }
        }else{
          if(sexes[i]==1){
            pairs[1,1] = nrow(repros) - i + 1
          }
          if(sexes[i]==0){
            pairs[1,2] = nrow(repros) - i + 1
          }
        }
      }
    }
  }else{
    numberIs = nrow(migrants)
    sexes    = migrants[,7]
    ms = length(subset(sexes, sexes==1))
    fs = length(subset(sexes, sexes==0))
    pairs   = matrix(nrow=min(ms, fs), ncol = 2)
    if(pairs>0){
      for(i in 1:nrow(pairs)){
        if(sexes[i]==1){
          pairs[i,1] = nrow(repros) - i + 1
        }
        if(sexes[i]==0){
          pairs[i,2] = nrow(repros) - i + 1
        }
      }
    }else{
      pairs = 0
      return(pairs)
    }
  }
  x = NULL
  x = try(length(pairs), silent = TRUE)
  if(x>0){
    pairings = matrix(nrow=length(pairs[,1]), ncol = 3)
    pairings[,1] = pairs[,1]
    pairings[,2] = pairs[,2]
    pairings[,3] = wild.pairs
    times = nrow(pairings)
  }else {
    offspring = NULL
    return(offspring)
  }
  pairs = pairings   #<- cbind(pairs, wild.pairs) # add number of offspring each pair will produce
  
  # set up all offspring matrix
  ALLOFFSPRING      <- matrix(nrow = sum(wild.pairs), ncol = 10 + (nloci * 2)) #IF VARIANCE EXCEEDS 0.25 EXTRA OFFSPRING, WILL GET ERROR
  ALLOFFSPRING[, 1] <- -9
  totaloffs         <- 0

  # generate offspring
  for(p in 1:times) {  #times determined above, with pairing, and indicates the number of pairs
    if(times>1){
      f = pairs[p, 2]
      m = pairs[p, 1]
      noff = pairs[p, 3]
    }
    if(times==1){
      f = pairs[2]
      m = pairs[1]
      noff = pairs[3]
    }
    
    if(noff > 0){
      ids <- seq(from = gstartID + 1, to = gstartID + noff, by = 1)
      
      #population offpsring matrix
      offspring      = matrix(nrow=noff, ncol=10)
      offspring[,1]  = ids                                                                       #ID
      offspring[,2]  = rep(0, noff)                                                              #age at current time
      offspring[,3]  = rep(repros[f,1], noff)                                                    #mom ID
      offspring[,4]  = rep(repros[m,1], noff)                                                    #dad ID
      if(selection == 0){                                                                        #relative repro success
        offspring[,5]  = rnorm(noff, mean(c(repros[f, 5], repros[m, 5])), RRSvar)                
      }
      if(selection != 0){
        offspring[,5]  = rnorm(noff,mean(c(repros[f,5],repros[m,5]))+abs(rnorm(noff,selection,RRSvar)),RRSvar)
      }
      offspring[,6]  = rep(1, noff)                                                              #wild born = 1, captive born = 0
      offspring[,7]  = sample(c(0,1), noff, replace=TRUE, prob=c(0.5, 0.5))                      #male=1, female=0
      offspring[,8]  = rep(g, noff)                                                              #generation born                                               
      offspring[,9]  = rep(1, noff)                                                              #1=alive,0=dead
      offspring[,10] = rep(0, noff)                                                              #generation individual died
      
      #fix RRS values that are greater than 2 or less than zero
      morethan1 = which(offspring[,5]> 2) 
      offspring[morethan1,5] = 2 #- offspring[morethan1,5]
      lessthan0 = which(offspring[,5]< 0) 
      offspring[lessthan0,5] = 0
      
      # genotypes
      # prep parent genotypes
      fg = repros[f, -c(1:10)]
      mg = repros[m, -c(1:10)]
      
      # prep offspring genotype matrix
      offspringG = matrix(nrow=noff, ncol=(nloci*2))
      
      # allele 1 positions
      positions = seq(1, (nloci*2), 2)
      
      # randomly sample either position 1 or 2 (add 0 or 1) to starting position
      fallele  <- positions + sample(0:1, nloci * noff, replace = TRUE)
      fallele2 <- fg[fallele]
      fallele3 <- matrix(fallele2, nrow = noff, ncol = nloci, byrow = TRUE)
      
      mallele  <- positions + sample(0:1, nloci * noff, replace = TRUE)
      mallele2 <- mg[mallele]
      mallele3 <- matrix(mallele2, nrow = noff, ncol = nloci, byrow = TRUE)
      
      offspringG[, positions]     <- fallele3
      offspringG[, positions + 1] <- mallele3
      
      offspring    = cbind(offspring, offspringG)
      gstartID     = gstartID + nrow(offspring)
      
      m1 <- match(-9, ALLOFFSPRING[, 1])
      ALLOFFSPRING[m1:((m1+nrow(offspring))-1), ] <- offspring
    } 
  }

  return(ALLOFFSPRING)
}
