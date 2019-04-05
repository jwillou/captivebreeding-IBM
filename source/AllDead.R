AllDead = function(alldead, population, outdir, r){
  writeout = rbind(population[population[,9]==-99,,drop=FALSE], population[population[,9]==0,,drop=FALSE])
  x = nrow(writeout)
  if(x > 0){
    forwriteout = apply(writeout, 1, function(z){paste(z, collapse = ",")})
    write(forwriteout, paste(outdir, "population_indvs", r, ".csv", sep=""), ncolumns=1, append=TRUE)  
  }
}
