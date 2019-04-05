library(Hmisc)
#load in data from some other run; as long as has control doesn't matter much which one

#set up from heritabity runs (RRSheritability.R)
par(mfrow=c(2,2))
for(d in 1:length(datasets)){
  data = datasets[[d]]
  data = data[data$startcap==1000,]
  data = data[data$RRSvar==0,]
  #data = data[125,,drop=FALSE]
  ones = subset(1:length(data$year), data$year==1) 
  
  #maxy = 125
  #miny = 0
  #plot(-100, -100, ylim = c(miny, maxy), xlim = c(0,30), xlab = "age", ylab = "number individuals", cex.lab=1, cex.lab=1) #,  main="supplementation") #reintroduction
  toave = NULL
  for(r in 1:length(ones)){
    if(r==length(ones)){
      plotdata = data[ones[r]:nrow(data),]
    }else{
      plotdata = data[ones[r]:(ones[r+1]-1),]
    }
    toave = rbind(toave, plotdata[50,23:53])
    #lines(x=c(0:30), plotdata[25,23:53], xlim=c(25,250), ylim=c(miny,maxy), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1 now all
  }
  yearmeans = apply(toave, 2, mean)
  yearCI = (apply(toave, 2, sd)/sqrt(10)) * 1.96
  yl = 0
  if(d==1){yu = 30}
  if(d==2){yu = 100}
  if(d==3){yu = 75}
  if(d==4){yu = 125}
  barplot(height=yearmeans, width=1, space = 0, names.arg=c(0:30), col=pcolors[d], border="grey60", xlab="age", ylab= "number of individuals", xlim=c(0,30), ylim=c(yl,yu))
  segments(c(0:30)+0.5, yearmeans - yearCI, c(0:30)+0.5, yearmeans + yearCI, lwd = 1.5, col="black")
  arrows(c(0:30)+0.5, yearmeans - yearCI, c(0:30)+0.5, yearmeans + yearCI, lwd = 1.5, angle = 90, code = 3, length = 0.005, col="black")
}
