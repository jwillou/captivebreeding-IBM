Immigrant = function(nimmigrants, maturity, lifespan, totalinds, nloci, otherpop, g, RRS){
  if(nimmigrants==1){
    tnimmigrants=2
  }else{
    tnimmigrants=nimmigrants
  }
  ages   = data.frame(age = (maturity:lifespan), num = rep(tnimmigrants/(lifespan*tnimmigrants), length(maturity:lifespan))) 
  
  migrants = matrix(nrow=tnimmigrants, ncol=10)
  migrants[,1]  = seq(max(totalinds), totalinds+tnimmigrants-1, 1)                      #individualID.momID.dadID
  migrants[,2]  = rep(lifespan, tnimmigrants)                                           #age at current time
  migrants[,3]  = rep(0, tnimmigrants)                                                  #place holder for momID
  migrants[,4]  = rep(0, tnimmigrants)                                                  #place holder for dadID
  migrants[,5]  = rep((1-RRS), tnimmigrants)                                            #relative repro success
  migrants[,6]  = rep(1, tnimmigrants)                                                  #wild born = 1, captive born = 0
  migrants[,7]  = sample(c(0,1), tnimmigrants, replace=TRUE, prob=c(0.5, 0.5))          #male=1, female=0
  migrants[,8]  = rep(g,tnimmigrants)                                                   #generation born
  migrants[,9]  = rep(1,tnimmigrants)                                                   #alive=1, dead=0
  migrants[,10] = rep(0,tnimmigrants)                                                   #generation died
  
  colnames(migrants) = c("ID","age","momID","dadID","RRS","HvsW","sex","gen","alive","gendead")

  rows  = sample(1:nrow(otherpop), tnimmigrants)
  genos = otherpop[rows,]
  
  #add genotypes to population data matrix
  migrants = cbind(migrants, genos)
  
  if(nimmigrants==1){
    migrants = migrants[1,,drop=FALSE]
  }
  return(migrants)
}
