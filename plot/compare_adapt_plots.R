library(Hmisc)
setwd("/Volumes/jwillou/ibm_captivity/OUTPUT/") #jwillou/ or scratch/

####run parameters####
startcap = 75
controly = 1000
RRS      = seq(0.1,0)
gens     = 250
poorenv  = 50
fixedenv = 100
nloci    = 50
capfounders = 1

####data####
ssum   = read.table("salmon/output06.10.16_07.18/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
ssumRI = read.table("salmon/output06.10.16_07.10/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon

colnames(ssum) = colnames(ssumRI) = #
  c("year", "propHatchery", "He", "Ho", "polyLoci", "Nalleles","meanRRSall", "meanRRSwild","meanRRScap",seq(0, 30, 1), 
    "K", "N", "gens", "RRS", "RRSvar", "r0", "l", "nloci", "countimmat", "killimmat", "adultmort", "fecundity", "maturity", "lifespan", "repro1",
    "startcap", "endcap", "capfound", "newfound", "propnew", "caplife", "coffprop","poorenv", "fixedenv", "lossperct", 
    "nimmigrants", "calcfitness", "propkill", "equalcaptive", "allonecaptive", "capvariation", "directory", "outdir", "plotit", "species")

####set up for data on to be analyzed####
setwd("~/Desktop/")
datasets = list(ssumRI=ssumRI, ssum=ssum) #  
labs     = c("salmon","salmon", "salmon") # 
pcolors  = c("deepskyblue", "deepskyblue3", "dodgerblue3") #
scolors  = pcolors #
Ks       = c(500,500,500,500) #
species  = labs 

plotcolsS = 1
plotcolsR = 1
plotcolsH = 1
plotcolsA = 1
captivecol = rep("turquoise3", length(species)) #pcolors
poorenvcol = pcolors #rep("black", length(species))      #pcolors

###plotting on top of each other - 10, 25, 50 years
endcap = c(85, 100, 125)
par(mar=c(2, 4, 2.5, 1) + 0.1)
yues = c(200, 200, 200, 200)
par(mfrow=c(1,2))

#popsize
for(d in 1:length(datasets)){
  #control data
  control = datasets[[d]]
  control = control[control$startcap==controly,]
  control$NumAllelesAverage = control$Nalleles
  onesc = subset(1:length(control$year), control$year==1)
  
 ####population size
  maxy = 1000
  allcols = c(11, 40)
  plot(-100, -100, ylim = c(0, maxy), xlim = c(25, gens), xlab = "year", ylab = "population size", cex.lab=1, cex.lab=1) #
  #control
  for(r in 1:100){
    if(r==length(onesc)){
      plotdata = control[onesc[r]:nrow(control),]
    }else{
      plotdata = control[onesc[r]:(onesc[r+1]-1),]
    }
    lines(plotdata$year[25:nrow(plotdata)], apply(plotdata[25:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,maxy), type="l", xlab="", ylab="", col="grey50", lwd=1.2) #1 now all
  }
  
  #add data
  for(rs in 1:length(endcap)){ 
    sc = endcap[rs]
    plotpoorenv = c(50, startcap)
    plotcaptive = c(startcap-1, sc)
    rrsd = 0.1 #rrstoplot[rs]
    
    #dataset
    data = datasets[[d]]
    data = data[data$endcap==sc,]
    data = data[data$RRS==rrsd,]
    data = data[data$propnew==1,]
    data$NumAllelesAverage = data$Nalleles
    ones = subset(1:length(data$year), data$year==1)
    
    #data
    for(r in 1:length(ones)){
      if(r==length(ones)){
        plotdata = data[ones[r]:nrow(data),]
      }else{
        plotdata = data[ones[r]:(ones[r+1]-1),]
      }
      
      #before cb part 1
      lines(plotdata$year[25:plotpoorenv[2]], apply(plotdata[25:plotpoorenv[2],allcols[1]:allcols[2]], 1,sum),                         xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=pcolors[rs], lwd=1.2) #1
      
      #after cv
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], apply(plotdata[plotcaptive[2]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=pcolors[rs], lwd=1.2) #1
      
      #before part 2 cb
      #lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], apply(plotdata[plotpoorenv[1]:plotpoorenv[2],allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=poorenvcol[rs], lwd=1.2)
      
      
      #durring cv 
      lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], apply(plotdata[plotcaptive[1]:plotcaptive[2],allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=captivecol[rs], lwd=1.2)
      
    }
  }
}
remove(d,ones,r,rs, plotdata,data,control,rrsd,onesc)

#alleles
for(d in 1:length(datasets)){
  #control data
  control = datasets[[d]]
  control = control[control$startcap==controly,]
  control$NumAllelesAverage = control$Nalleles
  onesc = subset(1:length(control$year), control$year==1)
  
  yu = 5
  yl = -10
  plot(-100, -100, ylim = c(yl,yu), xlim = c(25, gens), xlab = "year", ylab = "genetic diversity", cex.lab=1, cex.lab=1)
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
  
  #add data
  for(rs in 1:length(endcap)){ 
    sc = endcap[rs]
    plotpoorenv = c(50, startcap)
    plotcaptive = c(startcap-1, sc)
    rrsd = 0.1 #rrstoplot[rs]
    
    #dataset
    data = datasets[[d]]
    data = data[data$endcap==sc,]
    data = data[data$RRS==rrsd,]
    data = data[data$propnew==1,]
    data$NumAllelesAverage = data$Nalleles
    ones = subset(1:length(data$year), data$year==1)
    
    #data
    for(r in 1:length(ones)){
      if(r==length(ones)){
        plotdata = data[ones[r]:nrow(data),]
      }else{
        plotdata = data[ones[r]:(ones[r+1]-1),]
      }
      
      #before cb part 1
      lines(plotdata$year[25:plotpoorenv[2]], plotdata$NumAllelesAverage[25:plotpoorenv[2]]  - allmeans[25:plotpoorenv[2]], xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=pcolors[rs], lwd=1.2) #1
      
      #after cv
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$NumAllelesAverage[plotcaptive[2]:nrow(plotdata)]  - allmeans[plotcaptive[2]:nrow(plotdata)], xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=pcolors[rs], lwd=1.2) #1
      
      #durring cv 
      lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$NumAllelesAverage[plotcaptive[1]:plotcaptive[2]] - allmeans[plotcaptive[1]:plotcaptive[2]], xlim=c(25,gens), ylim=c(0, maxy), type="l", xlab="", ylab="", col=captivecol[rs], lwd=1.2)
      
    }
  }
}
remove(d,ones,r,rs, plotdata,data,control,rrsd,onesc)
