library(Hmisc)
setwd("/Volumes/snyder/ibm_captivity/OUTPUT/") #jwillou/ or scratch/

####run parameters####
startcap = c(75,100,115,123,124,1000)
endcap   = 125
controly = 1000
RRS      = c(0.1,0)
gens     = 250
poorenv  = 1000
fixedenv = 1000
nloci    = 50
capfounders = 1

####data####
csum  = rbind(read.table("crane/output06.06.16_17.38/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("crane/output06.06.16_15.58/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("crane/output06.06.16_15.54/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))     #crane
ssum  = rbind(read.table("salmon/output06.06.16_17.40/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("salmon/output06.06.16_17.38/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("salmon/output06.06.16_15.55/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))    #salmon
tasum = rbind(read.table("tamarin/output06.06.16_17.50/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("tamarin/output06.06.16_18.37/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("tamarin/output06.06.16_15.56/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))   #tamarin
tosum = rbind(read.table("toad/output06.06.16_18.16/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("toad/output06.06.16_18.30/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE),
              read.table("toad/output06.06.16_15.57/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE))      #toad

colnames(ssum) = colnames(tosum) = colnames(csum) = colnames(tasum) = #
  c("year", "propHatchery", "He", "Ho", "polyLoci", "NumAlleles","meanRRSall", "meanRRSwild","meanRRScap",seq(0, 30, 1), 
    "K", "N", "gens", "RRS", "RRSvar", "r0", "l", "nloci", "countimmat", "killimmat", "adultmort", "fecundity", "maturity", "lifespan", "repro1",
    "startcap", "endcap", "capfound", "newfound", "propnew", "caplife", "coffprop","poorenv", "fixedenv", "lossperct", 
    "nimmigrants", "calcfitness", "propkill", "equalcaptive", "allonecaptive", "directory", "outdir", "plotit", "species", "selection")

####set up for data on to be analyzed####
setwd("~/Desktop/")
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


####explor RRS, popsize, alleles ####
par(mfrow=c(5,3))
sels = c(0.00001,0.0001,0.001,0.005,0.01)
RRSv = 0.01 
rrsd = 0.1
startcapn = startcap[1]

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
    data = data[data$startcap==startcapn,]
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


####breakeven ####

for(d in 1:length(datasets)){
  
}
