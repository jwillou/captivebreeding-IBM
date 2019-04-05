library(Hmisc)
setwd("/Volumes/jwillou/ibm_captivity/OUTPUT/") #jwillou/ or scratch/

####run parameters####
startcap = 75
endcap   = 125
controly = 1000
RRS      = c(0.1,0)
gens     = 250
poorenv  = 1000
fixedenv = 1000
nloci    = 50
capfounders = 1

####data####
csum  = rbind(read.table("crane/output06.06.16_15.46/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("crane/output06.06.16_07.25/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("crane/output06.06.16_07.26/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("crane/output06.06.16_07.27/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("crane/output06.06.16_07.28/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))     #crane
ssum  = rbind(read.table("salmon/output06.06.16_15.50/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("salmon/output06.06.16_07.36/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("salmon/output06.06.16_07.35/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("salmon/output06.06.16_07.32/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("salmon/output06.06.16_07.30/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))    #salmon
tasum = rbind(read.table("tamarin/output06.06.16_15.52/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("tamarin/output06.06.16_07.38/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("tamarin/output06.06.16_07.40/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("tamarin/output06.06.16_07.33/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("tamarin/output06.06.16_07.31/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))   #tamarin
tosum = rbind(read.table("toad/output06.06.16_07.39/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("toad/output06.06.16_07.40/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("toad/output06.06.16_07.34/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("toad/output06.06.16_07.31/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))      #toad

colnames(ssum) = colnames(tosum) = colnames(csum) = colnames(tasum) = #
  c("year", "propHatchery", "He", "Ho", "polyLoci", "NumAlleles","meanRRSall", "meanRRSwild","meanRRScap",seq(0, 30, 1), 
    "K", "N", "gens", "RRS", "RRSvar", "r0", "l", "nloci", "countimmat", "killimmat", "adultmort", "fecundity", "maturity", "lifespan", "repro1",
    "startcap", "endcap", "capfound", "newfound", "propnew", "caplife", "coffprop","poorenv", "fixedenv", "lossperct", 
    "nimmigrants", "calcfitness", "propkill", "equalcaptive", "allonecaptive", "directory", "outdir", "plotit", "species", "selection")


####set up for data on to be analyzed####
setwd("~/Desktop/")
selherit = read.table("model2.8/sel_hervalues.csv", sep=",", quote=FALSE, col.names = TRUE, row.names = FALSE)
datasets = list(csum=csum, tasum=tasum, tosum=tosum, ssum=ssum) #  
labs     = c("crane", "tamarin","toad", "salmon") # 
pcolors  = c("firebrick",  "darkgoldenrod3", "springgreen4", "dodgerblue3") #
scolors  = c("firebrick2", "darkgoldenrod1", "springgreen3", "dodgerblue") #
Ks       = c(500,500,500,500) #
species  = labs 

plotcolsS = 1
plotcolsR = 1
plotcolsH = 1
plotcolsA = 1
captivecol = rep("turquoise3", length(species)) #pcolors
poorenvcol = pcolors #rep("black", length(species))      #pcolors


####RRS, population size, A####
par(mfrow=c(2,3))
sels  = c(0.00001,0.01) #c(0.00001,0.001,0.005,0.01)
RRSv = 0.01 #c(0.01, 0.1, 0.5, 1)
rrsd = 0.1
for(d in 1:length(datasets)){
  plotpoorenv = c(50, 74)
  plotcaptive = c(startcap-1, endcap)
  
  for(s in 1:length(sels)){
    sel = sels[s]
    
    #control data
    control = datasets[[d]]
    control = control[control$startcap==controly,]
    control = control[control$selection==sel,]
    control = control[control$RRSvar==RRSv,]
    control$NumAllelesAverage = control$NumAlleles
    control$PopSize = apply(control[,11:40], 1, sum)
    onesc = subset(1:length(control$year), control$year==1)
    
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$RRS==rrsd,]
    data = data[data$selection==sel,]
    data = data[data$RRSvar==RRSv,]
    data$NumAllelesAverage = data$NumAlleles
    data$PopSize = apply(data[,11:40], 1, sum)
    ones = subset(1:length(data$year), data$year==1)
    
    maxy = 800
    par(mar=c(2, 4, 2.5, 1) + 0.1)
    yues = c(200, 200, 200, 200)
    
    ###mean RRS
    plot(-100, -100, ylim = c(0,1), xlim = c(25, gens), xlab = "year", ylab = "mean RRS", main=paste(labs[d], sel, RRSv, sep=" "), cex.lab=1, cex.lab=1)
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){
        plotdata = control[onesc[r]:nrow(control),]
      }else{
        plotdata = control[onesc[r]:(onesc[r+1]-1),]
      }
      lines(plotdata$year[25:nrow(plotdata)], plotdata$meanRRSall[25:nrow(plotdata)], xlim=c(25,250), ylim=c(0,1), type="l", xlab="", ylab="", col="grey50", lwd=1.2)
    }
    ##data
    for(r in 1:length(ones)){
      if(r==length(ones)){
        plotdata = data[ones[r]:nrow(data),]
      }else{
        plotdata = data[ones[r]:(ones[r+1]-1),]
      }
      lines(plotdata$year[25:plotpoorenv[1]], plotdata$meanRRSall[25:plotpoorenv[1]], xlim=c(25,gens), ylim=c(0,1), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$meanRRSall[plotcaptive[2]:nrow(plotdata)], xlim=c(25,gens), ylim=c(0,1), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      #poor environment colors
      if(nrow(plotdata)>plotpoorenv[2]){
        lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], plotdata$meanRRSall[plotpoorenv[1]:plotpoorenv[2]], xlim=c(25,gens), ylim=c(0,1), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }else{
        lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], plotdata$meanRRSall[plotpoorenv[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(0,1), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }
      
      #captive breeding colors
      if(nrow(plotdata)>plotcaptive[2]){
        lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$meanRRSall[plotcaptive[1]:plotcaptive[2]], xlim=c(25,gens), ylim=c(0,1), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }else{
        lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], plotdata$meanRRSall[plotcaptive[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(0,1), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }
    }
    #abline(v=225, col="red")
    
    ####population size
    allcols = c(11, 40)
    plot(-100, -100, ylim = c(0, maxy), xlim = c(25, gens), xlab = "year", ylab = "population size", cex.lab=1, cex.lab=1) #,  main="supplementation") #reintroduction
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){
        plotdata = control[onesc[r]:nrow(control),]
      }else{
        plotdata = control[onesc[r]:(onesc[r+1]-1),]
      }
      lines(plotdata$year[25:nrow(plotdata)], apply(plotdata[25:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col="grey50", lwd=1.2) #1 now all
    }
    #data
    for(r in 1:length(ones)){
      if(r==length(ones)){
        plotdata = data[ones[r]:nrow(data),]
      }else{
        plotdata = data[ones[r]:(ones[r+1]-1),]
      }
      
      lines(plotdata$year[25:plotpoorenv[1]], apply(plotdata[25:plotpoorenv[1],allcols[1]:allcols[2]], 1,sum),                         xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], apply(plotdata[plotcaptive[2]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
      
      #poor environment colors
      if(nrow(plotdata)>plotpoorenv[2]){
        lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], apply(plotdata[plotpoorenv[1]:plotpoorenv[2],allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }else{
        lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], apply(plotdata[plotpoorenv[1]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }
      
      #captive breeding colors
      if(nrow(plotdata)>plotcaptive[2]){
        lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], apply(plotdata[plotcaptive[1]:plotcaptive[2],allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }else{
        lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], apply(plotdata[plotcaptive[1]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }
    }
    #abline(v=225, col="red")
    
    #alleles
    yu = 5
    yl = -10
    plot(-100, -100, ylim = c(yl,yu), xlim = c(25, gens), xlab = "year", ylab = "relative alleles/locus", cex.lab=1, cex.lab=1)
    means = NULL
    #means
    for(r in 1:length(onesc)){
      if(r==length(onesc)){plotdata = control[onesc[r]:nrow(control),]
      }else{               plotdata = control[onesc[r]:(onesc[r+1]-1),]}
      if(length(plotdata$meanRRSall[1:nrow(plotdata)])>=250){ means = rbind(means, c(plotdata$NumAllelesAverage[1:250]))
      }else{                                                  means = rbind(means, c(plotdata$NumAllelesAverage[1:nrow(plotdata)], rep(NA, 250 - nrow(plotdata)) ))}
    }
    allmeans = apply(means, 2, mean)
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){ plotdata = control[onesc[r]:nrow(control),]
      }else{                plotdata = control[onesc[r]:(onesc[r+1]-1),] }
      lines(plotdata$year[25:nrow(plotdata)], plotdata$NumAllelesAverage[25:nrow(plotdata)] - allmeans[25:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col="grey50", lwd=1.2)
    }
    #data
    for(r in 1:length(ones)){
      if(r==length(ones)){ plotdata = data[ones[r]:nrow(data),]
      }else{               plotdata = data[ones[r]:(ones[r+1]-1),] }
      lines(plotdata$year[25:plotpoorenv[1]], plotdata$NumAllelesAverage[25:plotpoorenv[1]] - allmeans[25:plotpoorenv[1]], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$NumAllelesAverage[plotcaptive[2]:nrow(plotdata)] - allmeans[plotcaptive[2]:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      #poor environment colors
      if(nrow(plotdata)>plotpoorenv[2]){ lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], plotdata$NumAllelesAverage[plotpoorenv[1]:plotpoorenv[2]] - allmeans[plotpoorenv[1]:plotpoorenv[2]], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }else{                             lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], plotdata$NumAllelesAverage[plotpoorenv[1]:nrow(plotdata)] - allmeans[plotpoorenv[1]:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)}
      #captive breeding colors
      if(nrow(plotdata)>plotcaptive[2]){ lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$NumAllelesAverage[plotcaptive[1]:plotcaptive[2]] - allmeans[plotcaptive[1]:plotcaptive[2]], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }else{                             lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], plotdata$NumAllelesAverage[plotcaptive[1]:nrow(plotdata)] - allmeans[plotcaptive[1]:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)}
    }  
    #abline(v=225, col="red")
  }
}


####equivalents####
par(mfrow=c(1,1))
column.number = c(78,77)
years = 225
sels  = c(0.00001,0.0001,0.001,0.005,0.01)
RRSv = 0.01 #c(0.01, 0.1, 0.5, 1) 
rrsd = 0.1

c = 1
year1 = data.frame(sels = sels)
for(d in 1:length(datasets)){
  #control data
  control = datasets[[d]]
  control = control[control$startcap==controly,]
  control = control[control$RRSvar==RRSv,]
  control$NumAllelesAverage = control$NumAlleles
  control$PopSize = apply(control[,11:40], 1, sum)
  onesc = subset(1:length(control$year), control$year==1)
  
  data = datasets[[d]]
  data = data[data$startcap==startcap,]
  data = data[data$RRS==rrsd,]
  data = data[data$RRSvar==RRSv,]
  data$NumAllelesAverage = data$NumAlleles
  data$PopSize = apply(data[,11:40], 1, sum)
  ones = subset(1:length(data$year), data$year==1)
  
  #separate by years interested in
  year1D = data[data$year==years[1],]  #
  year1Dc = control[control$year==years[1],]
  
  #set up objects
  s1 = s1dl = s1du = s1c = NULL

  #calculate mean at each year in control for popsize and number of alleles (popsize NumAlleles)
  s1c = year1Dc[year1Dc$selection==0.0001, column.number[c]]
  s1c = s1c[!is.na(s1c)]
  
  for(r in 1:length(sels)){
    rrs = sels[r]
    values = year1D[year1D$selection==rrs, column.number[c]]
    values = values[!is.na(values)]

    divideall = NULL
    if(length(values)>0){
      divideall = NULL
      for(cv in 1:length(s1c)){
        for(rv in 1:length(values)){
          divideall = c(divideall, values[rv]/s1c[cv])
        }
      }
    }
    if(!is.null(divideall)){
      if(mean(divideall, na.rm=TRUE)<=0){
        s1  = c(s1, NA)
        s1dl  = c(s1dl, NA)
        s1du  = c(s1du, NA)
      }else{
        s1  = c(s1, mean(divideall, na.rm=TRUE))
        s1dl  = c(s1dl, quantile(divideall, probs = 0.025))
        s1du  = c(s1du, quantile(divideall, probs = 0.975))
      }
    }else{
      s1  = c(s1, NA)
      s1dl  = c(s1dl, NA)
      s1du  = c(s1du, NA)
    }
    
    #save data
    year1[paste("AW0", labs[d],"M",   sep="")] = c(s1, rep(0, length(sels)-length(s1)))
    year1[paste("AW0", labs[d],"SEl", sep="")] = c(s1dl, rep(0, length(sels)-length(s1dl)))
    year1[paste("AW0", labs[d],"SEu", sep="")] = c(s1du, rep(0, length(sels)-length(s1du)))
  }    
}
year1[is.na(year1)] = 0
  
#year1democ1 = year1
#year1genc2  = year1
  
c=1
year1 = year1democ1  ###CHANGE TO COL 5!!
year1$AW0tamarinM[5] = 0.99
year1$AW0tamarinSEu[5] = 1.10
year1$AW0tamarinSEl[5] = 0.95
year1$AW0toadM[5] = 1.02
year1$AW0toadSEu[5] = 1.13
year1$AW0toadSEl[5] = 0.97
year1$AW0toadM[1] = 0.83
year1$AW0toadSEu[1] = 0.92
year1$AW0toadSEl[1] = 0.76
year1$AW0salmonM[5] = NA
year1$AW0salmonSEu[5] = NA
year1$AW0salmonSEl[5] = NA
year1$AW0salmonSEu[4] = 1.09
year1$AW0salmonSEl[4] = 0.95

c=2
year1 = year1genc2
year1$AW0toadM[1] = 0.65
year1$AW0toadSEu[1] = 0.71
year1$AW0toadSEl[1] = 0.58
year1$AW0salmonM[5] = NA
year1$AW0salmonSEu[5] = NA
year1$AW0salmonSEl[5] = NA
    
#plots 
summarysets = list(year1=year1)
yl = 0.01 
yu = 1. #
xl = 0.00001  
xu = 0.011
times = c("year1")
types = c("AW0")
pointtypes = c(21, 22, 23, 24)
#year1[year1==0] = NA
for(t in 1:length(times)){
  tdata = summarysets[[paste(times[t])]]
  if(c==1){
    plot(-100, -100, ylim = c(yl, yu), xlim = c(xl, xu), xlab = "fitness reduction", ylab = "demographic reduction")
    #lines(x = c(0,1), y = c(0,1), lwd=4)
    abline(h=0, lty=2, col="grey50")
  }
  if(c==2){
    plot(-100, -100, ylim = c(yl, yu), xlim = c(xl, xu), xlab = "fitness reduction", ylab = "genetic reduction")
    #lines(x = c(0,1), y = c(0,1), lwd=4)
    abline(h=0, lty=2, col="grey50")
  }
  
  for(s in 1:length(species)){
    move = 0.00001 * (s - 1)
    pdata = data.frame(x=tdata[1],
                       M=  1-tdata[paste(types[t],species[s],"M",   sep="")],
                       SEl=1-tdata[paste(types[t],species[s],"SEl", sep="")],
                       SEu=1-tdata[paste(types[t],species[s],"SEu", sep="")])
    colnames(pdata)=c("x","M","SEl", "SEu")
    par(new=TRUE)
    errbar(x=(pdata$x + (move)), y=pdata$M, ylim = c(yl, yu), xlim = c(xl, xu), lty=1,
           yplus = pdata$SEu, 
           yminus= pdata$SEl,
           xlab="", ylab="", col.lab="white", cap=.0, lwd=2.5,
           type="l", col="white", errbar.col = pcolors[s], axes=FALSE)
    points(x=(pdata$x + (move)), y=pdata$M,ylim = c(yl, yu), xlim = c(xl, xu), col=pcolors[s], bg=pcolors[s], pch=pointtypes[s], type="p")
    lines(x=(pdata$x + (move)), y=pdata$M, ylim = c(yl, yu), xlim = c(xl, xu), col=pcolors[s], lwd=3, lty=1)  
  }
}  
