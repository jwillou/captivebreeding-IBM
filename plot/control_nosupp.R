library(Hmisc)
setwd("/Volumes/snyder/ibm_captivity/nosupp/") #jwillou/ or scratch/

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
csum  = read.table("crane/output08.17.16_09.49/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
ssum  = read.table("salmon/output08.17.16_09.52/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
tasum = read.table("tamarin/output08.17.16_09.54/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tosum = read.table("toad/output08.17.16_09.55/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad

colnames(ssum) = colnames(tosum) = colnames(csum) = colnames(tasum) = #
  c("year", "propHatchery", "He", "Ho", "polyLoci", "NumAlleles","meanRRSall", "meanRRSwild","meanRRScap",seq(0, 30, 1), 
    "K", "N", "gens", "RRS", "RRSvar", "r0", "l", "nloci", "countimmat", "killimmat", "adultmort", "fecundity", "maturity", "lifespan", "repro1",
    "startcap", "endcap", "capfound", "newfound", "propnew", "caplife", "coffprop","poorenv", "fixedenv", "lossperct", 
    "nimmigrants", "calcfitness", "propkill", "equalcaptive", "allonecaptive", "capvariation", "directory", "outdir", "plotit", "species")

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


###MODIFIED FOR FIGURE 2!!!! RRS, population size, Ho, A####
#rrstoplot = c(0,0.9) #REMEMBER: THIS IS RRS PENALTY
par(mfrow=c(4,3))
for(d in 1:length(datasets)){
  #par(mfrow=c(3,3))
  #set up color plotting variables
  #poor environment before captive breeding
  plotpoorenv = c(50, 74)
  plotcaptive = c(startcap-1, endcap)
  #control data
  control = datasets[[d]]
  control = control[control$startcap==controly,]
  control$NumAllelesAverage = control$NumAlleles
  onesc = subset(1:length(control$year), control$year==1)
  
  #determine ylim for population size for each species
  maxy = 800#max(apply(maxpop[,23:(23+(ncol(maxpop) - 34))], 1, sum), na.rm=TRUE) + 200
  par(mar=c(2, 4, 2.5, 1) + 0.1)
  #par(mfrow = c(2,2), mar=c(1, 4, 1, 1) + 0.1)#it goes c(bottom, left, top, right) 
  #par(mfrow=c(2,3))
  yues = c(200, 200, 200, 200)
  for(rs in 1:1){ #length(rrstoplot)
    rrsd = 0.1 #rrstoplot[rs]
    
    #dataset
    #data = datasetsRI[[d]]
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$RRS==rrsd,]
    data$NumAllelesAverage = data$NumAlleles
    ones = subset(1:length(data$year), data$year==1)
    
    ###mean RRS
    plot(-100, -100, ylim = c(0,1), xlim = c(25, gens), xlab = "year", ylab = "mean RRS", main=paste(labs[d], sep=" "), cex.lab=1, cex.lab=1)
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
    
    ####population size
    allcols = c(11, 40)
    plot(-100, -100, ylim = c(0, maxy), xlim = c(25, gens), xlab = "year", ylab = "population size", cex.lab=1, cex.lab=1) #,  main="supplementation") #reintroduction
    #plot(-100, -100, ylim = c(0, maxy), xlim = c(25, gens), xlab = "year", ylab = "population size", cex.lab=1, cex.lab=1,  main="reintroduction") #
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){
        plotdata = control[onesc[r]:nrow(control),]
      }else{
        plotdata = control[onesc[r]:(onesc[r+1]-1),]
      }
      #lines(plotdata$year[1:nrow(plotdata)], plotdata[1:gens,23], xlim=c(1,gens), ylim=c(0,2000), type="l", xlab="", ylab="", col="black") #0
      lines(plotdata$year[25:nrow(plotdata)], apply(plotdata[25:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col="grey50", lwd=1.2) #1 now all
      #lines(plotdata$year[25:nrow(plotdata)], apply(plotdata[25:gens,alladds[1]:alladds[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col="grey60") #2 now mature
      #lines(plotdata$year[1:nrow(plotdata)], plotdata[1:gens,26], xlim=c(1,gens), ylim=c(0,2000), type="l", xlab="", ylab="", col="grey70") #3
      #lines(plotdata$year[1:nrow(plotdata)], plotdata[1:gens,27], xlim=c(1,gens), ylim=c(0,2000), type="l", xlab="", ylab="", col="grey80") #4
      #lines(plotdata$year[1:nrow(plotdata)], plotdata[1:gens,28], xlim=c(1,gens), ylim=c(0,2000), type="l", xlab="", ylab="", col="grey90") #5
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
  }
}
remove(d,ones,r,rs, plotdata,data,control,rrsd,onesc)
