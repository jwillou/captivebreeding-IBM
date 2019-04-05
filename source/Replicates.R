Replicates = function(repeats, K.V, N.V, gens.V, RRS.V, RRSvar.V, r0.V, l.V, nloci.V, countimmat.V, killimmat.V, adultmort.V, 
                      fecundity.V, maturity.V, lifespan.V, repro1.V,
                      startcap.V, endcap.V, capfound.V, newfound.V, propnew.V, caplife.V, coffprop.V,
                      poorenv.V, fixedenv.V, lossperct.V, 
                      nimmigrants.V, propkill.V, equalcaptive.V, allonecaptive.V, capvariation.V,
                      directory, outdir, plotit, species, selection) {  
  
  replicates = expand.grid(K.V, N.V, gens.V, RRS.V, RRSvar.V, r0.V, l.V, nloci.V, countimmat.V, killimmat.V, adultmort.V, 
                           fecundity.V, maturity.V, lifespan.V, repro1.V,
                           startcap.V, endcap.V, capfound.V, newfound.V, propnew.V, caplife.V, coffprop.V,
                           poorenv.V, fixedenv.V, lossperct.V, 
                           nimmigrants.V, propkill.V, equalcaptive.V, allonecaptive.V, capvariation.V,
                           directory, outdir, plotit, species, selection)
  colnames(replicates) = c("K", "N", "gens", "RRS", "RRSvar", "r0", "l", "nloci", "countimmat", "killimmat", "adultmort", 
                           "fecundity", "maturity", "lifespan", "repro1",
                           "startcap", "endcap", "capfound", "newfound", "propnew", "caplife", "coffprop",
                           "poorenv", "fixedenv", "lossperct", 
                           "nimmigrants", "propkill", "equalcaptive", "allonecaptive", "capvariation",
                           "directory", "outdir", "plotit", "species", "selection")
 
  #remove replicates that include RRS for sims that do not include captive breeding
  replicates$keep = rep(1, length(replicates[,1]))
  for(r in 1:length(replicates[,1])){
    if(replicates$startcap[r]==1000){
      if(replicates$RRS[r]>0){
        replicates$keep[r] = 0
      }
    }
  }
  replicates = replicates[!replicates$keep==0,]
  reps = NULL
  for(r in 1:repeats){
    reps = rbind(reps, replicates)
  }
  replicates = reps
  return(replicates)
}  
