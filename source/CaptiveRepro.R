CaptiveRepro = function(cappop, cap.pairs, RRS, RRSvar, fecundity, maturity, g, nloci, population, totalinds){
  #let captive population reproduce (randomly paired)
  offspring = NULL
  if(!is.null(nrow(cappop))){
    if(nrow(cappop) > 0){
      repros    = cappop[cappop[,2] >= maturity, 1:ncol(population)]
      if(length(repros) <= ncol(population)){
        return(offspring)
      } else {
        gstartID = totalinds
        ALLOFFSPRING = NULL 
        
        # check that there are at least 2 individuals, one male and one female
        if(is.null(nrow(repros))) {return(ALLOFFSPRING)}
        if(length(repros[repros[, 7] == 0, 1]) < 1) {return(ALLOFFSPRING)} 
        if(length(repros[repros[, 7] == 1, 1]) < 1) {return(ALLOFFSPRING)}
        
        # randomly pair males and females
        size    <- min(table(repros[, 7]))
        males   <- sample(which(repros[, 7] == 1), size, replace = FALSE)
        females <- sample(which(repros[, 7] == 0), size, replace = FALSE)
        pairs   <- cbind(males, females)
        pairs   <- pairs[sample(1:length(pairs[, 1]), length(cap.pairs), replace = TRUE), ] #randomly select needed number of pairs; replace = TRUE in case not enough pairs
        
        x = NULL
        x = try(length(pairs), silent = TRUE)
        if(!is.null(x) & x>2){
          pairings = matrix(nrow=length(pairs[,1]), ncol = 3)
          pairings[,1] = pairs[,1]
          pairings[,2] = pairs[,2]
          pairings[,3] = cap.pairs
          times = nrow(pairings)
        }else if(!is.null(x) & x==2){
          pairings = c(pairs[1], pairs[2], cap.pairs)
          times = 1
        }else {
          offspring = NULL
          return(offspring)
        }
        pairs = pairings   #<- cbind(pairs, cap.pairs) # add number of offspring each pair will produce
        
        # set up all offspring matrix
        ALLOFFSPRING      <- matrix(nrow = sum(cap.pairs), ncol = 10 + (nloci * 2)) 
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
            offspring[,1]  = ids                                                                      #ID
            offspring[,2]  = rep(0, noff)                                                             #age at current time
            offspring[,3]  = rep(repros[f,1], noff)                                                   #mom ID
            offspring[,4]  = rep(repros[m,1], noff)                                                   #dad ID
            offspring[,5]  = rnorm(noff, mean(c(repros[f, 5], repros[m, 5])), RRSvar) - RRS           #relative repro success, MAYBE NEED TO ADD VARIANCE???
            offspring[,6]  = rep(1, noff)                                                             #wild born = 1, captive born = 0
            offspring[,7]  = sample(c(0,1), noff, replace=TRUE, prob=c(0.5, 0.5))                     #male=1, female=0
            offspring[,8]  = rep(g, noff)                                                             #generation born                                               
            offspring[,9]  = rep(1, noff)                                                             #1=alive,0=dead
            offspring[,10] = rep(0, noff)                                                             #generation individual died
            
            #fix RRS values that are greater than 1 or less than zero
            morethan1 = which(offspring[,5]> 2) 
            offspring[morethan1,5] = 2
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
        offspring <- ALLOFFSPRING
      } 
      #change origin to captive-born code (0), if any offspring are produced
      if(!is.null(offspring)){
        x = try(nrow(offspring), silent=TRUE)
        if(is.null(x)){return(offspring)}
        if(x>1){offspring[,6] = 0}
        if(x==1){offspring[6] = 0}
      }
    }
  }
  return(offspring)
}
