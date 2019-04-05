RunModel = function(runvars, r){
#### Initialize ####
  #set initial age strucutre of population (equal proportion of all ages) 
  agestage   = data.frame(age = (1:runvars$lifespan[r]), num = rep(runvars$N[r]/(runvars$lifespan[r]*runvars$N[r]), runvars$lifespan[r])) 
  population = StartingPop(runvars$N[r], agestage, runvars$nloci[r])
  otherpop   = StartingPop(10000, agestage, runvars$nloci[r])
  otherpop   = otherpop[,11:(runvars$nloci[r]*2+10)]
  remove(agestage)
  
  #write starting population to file 
  write.table(population[0,], paste(runvars$outdir[r], "population_indvs", r, ".csv", sep=""), col.names=TRUE, row.names=FALSE, append=FALSE, sep=",")
  
  #set up variables and ID number counter
  totalinds  = length(population[,1]) + 1 #VariableForUniqueID
  cappop     = NULL
  alldead    <<- 0
   
  #if plotting switch is on, plot population size
  if(runvars$plotit[r]==1){
    Na         = runvars$K[r]
    Nadults    = c(nrow(population[population[,2]>=runvars$maturity[r],,drop=FALSE]))
    plot(-100, -100 , xlab="generation", ylab="population size", xlim=c(0, (runvars$gens[r] + runvars$lifespan[r] + 1)), ylim=c(0, (runvars$K[r]*3)), type="b") 
    abline(runvars$K[r],0,lty=3, col="grey50")
    abline(v=runvars$poorenv[r], lty=2, col="darkorange", lwd=2)
    abline(v=runvars$fixedenv[r], lty=2, col="darkorange", lwd=2)
    abline(v=runvars$startcap[r], lty=4, col="forestgreen", lwd=2)
    abline(v=runvars$endcap[r], lty=4, col="forestgreen", lwd=2)
  }
#### Simulate over years ####
  for(g in 1:(runvars$gens[r] + runvars$lifespan[r] + 1)){ #(runvars$gens[r] + runvars$lifespan[r] + 1)

#### Captive sampling ####
    if(g >= runvars$startcap[r] & g <= runvars$endcap[r] & g %% runvars$propnew[r] == 0){   
      #reset tracking variable
      alldead <<- 0
      
      # sample wild-born from wild population, mark as such in population
      datasets   = SampleFounders(cappop, population, runvars$capfound[r], runvars$newfound[r], runvars$caplife[r], Nt, runvars$maturity[r], g, alldead, runvars$RRS[r], runvars$RRSvar[r])
      cappop     = datasets$cappop
      population = datasets$population
      #captivedie = datasets$more
      remove(datasets)
      # check to be sure population is not extinct
      if(alldead == 1){
        population[,9] = 0
        AllDead(alldead, population, runvars$outdir[r], r)
        break
      }
    } 
#### Some death ####    
    # remove offspring, proportional to RRS values
    nalive     = nrow(population[population[,9]==1,,drop=FALSE])
    population = RRSMortality(population, runvars$maturity[r])
    nkilled    = nalive - nrow(population[population[,9]==1,,drop=FALSE])
    
#### Determine number of offspring to produce ####
    # determine current population size 
    Nt = CalculateNt(population, runvars$countimmat[r], runvars$maturity[r])
    if(is.null(Nt)){
      alldead <<- 2
      population[,9] = 0
      AllDead(alldead, population, runvars$outdir[r], r)
      break
    }
    
    # adjust value to compensate for proportion of adults killed before/after head count
    Nt = Nt + round(nkilled * runvars$propkill[r])
    remove(nalive, nkilled)
    
    # determine future population size/number of offspring needed
    numberoffspring     = NumberOffspring(Nt, runvars$K[r], runvars$r0[r], g, runvars$l[r], runvars$adultmort[r], agestage, runvars$coffprop[r], runvars$poorenv[r], runvars$fixedenv[r], runvars$lossperct[r], population)
    n.captive.offspring = numberoffspring[1,1]
    n.wild.offspring    = numberoffspring[1,2]
    
#### Captive reproduction #####
    coffspring  = NULL
    if(g >= runvars$startcap[r] & g <= runvars$endcap[r] & g %% runvars$propnew[r] == 0){   
      if(n.captive.offspring>0){
        cap.pairs   = CaptivePairs(n.captive.offspring, runvars$fecundity[r], runvars$equalcaptive[r], runvars$allonecaptive[r], runvars$capvariation[r], cappop)
        coffspring  = CaptiveRepro(cappop, cap.pairs, runvars$RRS[r], runvars$RRSvar[r], fecundity, runvars$maturity[r], g, runvars$nloci[r], population, totalinds)
        remove(cap.pairs)
        # add captive born individuals to count (if there were any)
        if(!is.null(nrow(coffspring))){totalinds = totalinds + length(coffspring[,1]) + 1}
      }
    }
#### Wild reproduction ####
    #get migrants if it is the correct year
    migrants = NULL
    if(runvars$nimmigrants[r]>0 & g %% runvars$maturity[r] == 0){
      # add migrants for the year - all are mature adults
      migrants     = Immigrant(runvars$nimmigrants[r], runvars$maturity[r], runvars$lifespan[r], totalinds, runvars$nloci[r], otherpop, g, runvars$RRS[r])
      migrants[,5] = mean(population[population[,9]==1,5,drop=FALSE])
      population   = rbind(population, migrants)
      totalinds    = totalinds + runvars$nimmigrants[r] + 1
    }
    
    offspring = NULL
    parents   = NULL
    repros    = population[population[,9] == 1, ,drop=FALSE] # are alive
    if(nrow(repros)>2){
      repros = repros[repros[,2] >= runvars$maturity[r], ,drop=FALSE]  # are adults
      if(nrow(repros)>2){
        if(n.wild.offspring>0){
          wild.pairs = Pairs(n.offs = n.wild.offspring, runvars$fecundity[r])
          offspring  = Repro(repros, wild.pairs, runvars$RRS[r], runvars$RRSvar[r], runvars$fecundity[r], runvars$maturity[r], g, runvars$nloci[r], population, cappop, totalinds, migrants, runvars$selection[r])
          remove(wild.pairs)
          # add wild born individuals to IDcount (if there were any)
          if(!is.null(nrow(offspring))){
            totalinds = totalinds + length(offspring[,1]) + 1
            parents   = unique(c(offspring[,3], offspring[,4]))
          }
        }
      }
    }
    
    remove(Nt, numberoffspring, n.wild.offspring, n.captive.offspring, repros, migrants)
    
#### Some more death, add new of year ####
    # kill adults that die this year, after reproduction
    population = AdultMortality(population, g, runvars$adultmort[r], runvars$killimmat[r], runvars$maturity[r], parents, runvars$lifespan[r], runvars$repro1[r], alldead)
    
    # add new of year to object
    population = rbind(population, offspring, coffspring)
    remove(parents, offspring, coffspring)
    
##### Age and write to file ####
    # increase age by 1 year
    population = AgeUp(population, alldead)
    
    # determine current population size and plot it
    if(runvars$plotit[r]==1){
      Na      = c(Na, nrow(population[population[,9]==1, ,drop=FALSE]))
      alive   = population[population[,9]==1, ,drop=FALSE]
      Nadults = c(Nadults, nrow(alive[alive[,2] >= runvars$maturity[r],,drop=FALSE]))
      lines(c(0:g), Na , xlab="generation", ylab="population size", cex = 2, lty = 1, col="black", lwd=5)
      lines(c(0:g), Nadults , xlab="generation", ylab="population size", cex = 2, lty = 1, col="blue", lwd=5)
      remove(alive)
    }
    
    # write info for all dead individuals, then remove all dead from population object
    AllDead(alldead, population, runvars$outdir[r], r)
    alldead <<- 3
    population = population[population[,9]==1,,drop=FALSE]
    
    # look for extinct populations (before immigration makes it no longer extinct)
    if(nrow(population[population[,9]==1, ,drop=FALSE])<2){break}
  
  }
#### Return ####
  # write info for all remaining individuals
  alldead <<- 4
  
  # calulate and writeout summary information
  WriteOut(runvars, r)
}
