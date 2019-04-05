CalculateNt = function(population, countimmat, maturity){
    dead       = try(population[population[,9]==0,  ], silent=TRUE)
    cdead      = try(population[population[,9]==-99,], silent=TRUE)
    population = try(population[population[,9]==1,  ], silent=TRUE)
    
    #count everybody
    if(countimmat==1){
      Nt = nrow(population)
    }
    #do not count immature individuals
    if(countimmat==0){
      Nt = nrow(population[population[,2]>=maturity,,drop=FALSE])
    }
    return(Nt)
}
