StartingPop = function(N, agestage, nloci){
  population = matrix(nrow=N, ncol=10)
  population[,1]  = c(1:N)                                                     #individualID.momID.dadID
  population[,2]  = sample(agestage$age, N, replace=TRUE, prob=agestage$num)   #age at current time
  population[,3]  = rep(0, N)                                                  #place holder for momID
  population[,4]  = rep(0, N)                                                  #place holder for dadID
  population[,5]  = rep(1, N)                                                  #relative repro success
  population[,6]  = rep(1, N)                                                  #wild born = 1, captive born = 0
  population[,7]  = sample(c(0,1), N, replace=TRUE, prob=c(0.5, 0.5))          #male=1, female=0
  population[,8]  = rep(0,N)                                                   #generation born
  population[,9]  = rep(1,N)                                                   #alive=1, dead=0
  population[,10] = rep(0,N)                                                   #generation died
  
  colnames(population) = c("ID","age","momID","dadID","RRS","HvsW","sex","gen","alive","gendead")
  
  #msatts not initially in HWE
  genos = NULL
  nalleles = 35
  alleles = seq(1,nalleles,1)
  afreqs  = ((sort(c(1:nalleles), decreasing = TRUE)/c(1:nalleles)) / (sum(sort(c(1:nalleles), decreasing = TRUE)/c(1:nalleles))))
  for(g in 1:(nloci)){
    cgenos = as.matrix(sample(alleles, N*2, replace=TRUE, prob=afreqs))
    genos = cbind(genos, cgenos[1:N], cgenos[(N+1):(2*N)])
  }
  
  #SNPS in HWE
  #generate genotypes for starting population
  #genos   = matrix(nrow=N, ncol=nloci*2)
  #columns = seq(1,(nloci*2),2)
  #for(n in 1:nloci){
  #  p = sample(seq(from = 0, to = 1, by = 0.01), 1)
  #  #create pool of genotypes in HWE
  #  pool = c(rep(0, round(N*p*p, 0)),                                    #homozygous p*p
  #           rep(1, round(N*(1-p)*(1-p), 0)),                            #homozygous (1-p)*(1-p)
  #           rep(2, N-(round(N*p*p, 0)+(round(N*(1-p)*(1-p), 0))))       #heterozygotes 
  #           )
  #  gtype = sample(pool, N, replace=FALSE)
  #  for(i in 1:N){
  #    if(gtype[i]==0){                            #homozygous (0,0)
  #      genos[i,columns[n]]   = 0
  #      genos[i,columns[n]+1] = 0
  #      next
  #    }else if(gtype[i]==1){                      #heterozygous (0,1)
  #      genos[i,columns[n]]   = 0
  #      genos[i,columns[n]+1] = 1
  #    }else{                                      #homozygous (1,1)
  #      genos[i,columns[n]]   = 1
  #      genos[i,columns[n]+1] = 1
  #    }
  #  }
  #  pool  = NULL
  #}  
  
  #add genotypes to population data matrix
  population = cbind(population, genos)
  return(population)
} 
(1-sum(0.5, 0.15, 0.1, 0.05))/6
