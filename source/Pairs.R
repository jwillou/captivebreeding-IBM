Pairs = function(n.offs, fecundity){
  #fecundity == 1 - this assumes that exactly 0 or 1 offspring are produced per pair
  if(fecundity==1){
    pairs = rep(1, n.offs)
    return(pairs)
  }
  
  #fecundity == 2 - this assumes that each birth results in two offspring, as in tamarins, but that 1/2 of the time only one survives
  if(fecundity==2){
    twins   = rep(2, (n.offs/4))
    singles = rep(1, n.offs - sum(twins))
    pairs   = c(singles, twins)
    return(pairs)  
  }
  
  # fecundity 3-9
  #if(fecundity>=3 & fecundity<10){
  #  hist(rgamma(n.offs*10, shape = 0.5, rate = 0.5), breaks = 50) # histogram of distribution
  #  mean(rgamma(n.offs*10, shape = 0.5, rate = 0.5))
  #  var(rgamma(n.offs*10, shape = 0.5, rate = 0.5))
  #  g.shape = 0.5 
  #  g.rate = 0.5
  #}  
  
  # fecundity >= 10
  if(fecundity>=10 & fecundity<100){
    #hist(rgamma(n.offs*10, shape = 0.5, rate = 0.85), breaks = 50) # histogram of distribution
    #mean(rgamma(n.offs*10, shape = 0.5, rate = 0.5))
    #var(rgamma(n.offs*10, shape = 0.5, rate = 0.5))
    g.shape = 0.5 
    g.rate = 0.85
  }  
  
  if(fecundity>=100){
    #hist(rgamma(n.offs*10, shape = 0.5, rate = 0.5), breaks = 50) # histogram of distribution
    #mean(rgamma(n.offs*10, shape = 0.5, rate = 0.5))
    #var(rgamma(n.offs*10, shape = 0.5, rate = 0.5))
    g.shape = 0.5 
    g.rate = 0.5
  }  
  # determine repro success using shape/rate parameters determined above
  pair.rs <- rgamma(n.offs*10, shape = g.shape, rate = g.rate)  # sample from distribution
  pair.rs <- round(pair.rs) # round to get whole numbers
  
  # this whole next part is to get us exactly n.offs offspring
  # basic procdure is to get slightly less than n.offs (ocasionally will get exactly n.offs) and 
  # randomly add pairs with 1 offspring until reach n.offs
  pair.rs  <- pair.rs[-which(pair.rs == 0)] # remove all 0s
  pair.rs2 <- cumsum(pair.rs)  # take cumlative sum of pair.rs
  
  #try really hard to get something that does not sum to zero
  loops = 0
  while(sum(pair.rs2)==0){
    loops = loops + 1
    pair.rs <- rgamma(n.offs*10, shape = g.shape, rate = g.rate)  # sample from distribution
    pair.rs <- round(pair.rs) # round to get whole numbers
    pair.rs  <- pair.rs[-which(pair.rs == 0)] # remove all 0s
    pair.rs2 <- cumsum(pair.rs)  # take cumlative sum of pair.rs
    if(loops>999){
      break
    }
  }
  
  #if unlucky with all 0, try again
  if(sum(pair.rs2)==0){
    while(sum(pair.rs2)==0){
      loops = loops + 1
      pair.rs <- rgamma(n.offs*10, shape = g.shape, rate = g.rate)  # sample from distribution
      pair.rs <- round(pair.rs) # round to get whole numbers
      pair.rs  <- pair.rs[-which(pair.rs == 0)] # remove all 0s
      pair.rs2 <- cumsum(pair.rs)  # take cumlative sum of pair.rs
      if(loops>999){
        break
      }
    }
  }
  
  #if first value is larger than the number of offspring needed in total, have one pair produce all offspring
  if(pair.rs2[1]>n.offs){
    pairs = n.offs
    return(pairs)
  }else {
    closest  <- which(abs(pair.rs2-n.offs) == min(abs(pair.rs2-n.offs))) # the first x individuals in pair.rs are cloest to n.offs
    if(length(closest) > 1) {closest <- closest[1]}
    
    n.sampled <- pair.rs2[closest] # are we > or < n.offs
    if(n.sampled > n.offs) {closest <- closest-1} # if > n.offs take one less so that we are always less than n.offs
    
    rs <- pair.rs[1:closest] # take all pairs
    
    add.off <- n.offs-sum(rs)  # how many extra offspring do we need to add
    pairs <- c(rs, rep(1, add.off))
    return(pairs)
  }  
}
