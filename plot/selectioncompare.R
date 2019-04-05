RRSvar = seq(0, 0.01, 0.001)
for(r in 1:length(RRSvar)){
  normal = rnorm(1000, 0.5, 0.005)
  select = rnorm(1000, 0.5+abs(rnorm(1000, RRSvar[r], 0.005 )), 0.005)
  
  par(mfrow=c(3,1))
  hist(normal, xlim=c(0.45,0.65), col="black", ylim=c(1,100), main=paste(RRSvar[r], "difference =", round(mean(select) - mean(normal), 4), sep=" "), xlab="", ylab="", breaks=50)
  abline(v=mean(normal))
  par(new=TRUE)
  hist(select, xlim=c(0.45,0.65), col="red", ylim=c(1,100), main="", xlab="", ylab="", breaks=50, border="red")
  abline(v=mean(select), col="red", lty=2)
  
  hist(normal, xlim=c(0.45,0.65), col="black", ylim=c(1,100), main="", xlab="", ylab="", breaks=50)
  abline(v=mean(normal))
  hist(select, xlim=c(0.45,0.65), col="red", ylim=c(1,100), main="", xlab="", ylab="", breaks=50, border="red")
  abline(v=mean(select), col="red", lty=2)
}
