library(Hmisc)
setwd("/Volumes/snyder/ibm_captivity/OUTPUT/") #jwillou/ or scratch/

####run parameters####
startcap = 75
endcap   = 125
controly = 1000
RRS      = c(seq(0,0.275,0.025), seq(0.3,1,0.1)) 
gens     = 250
poorenv  = 1000
fixedenv = 1000
nloci    = 50
capfounders = 1

####data####
csum  = read.table("crane//allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
ssum  = read.table("salmon//allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
tasum = read.table("tamarin//allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tosum = read.table("toad//allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad

colnames(ssum) = colnames(tosum) = colnames(csum) = colnames(tasum) = #
  c("year", "propHatchery", "He", "Ho", "polyLoci", "Nalleles","meanRRSall", "meanRRSwild","meanRRScap",seq(0, 30, 1), 
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
  control[control=="NaN"] = NA
  control$NumAllelesAverage = control$NumAlleles
  onesc = subset(1:length(control$year), control$year==1)
  
  #determine ylim for population size for each species
  #maxpop = datasetsRI[[d]]
  maxy = 800#max(apply(maxpop[,23:(23+(ncol(maxpop) - 34))], 1, sum), na.rm=TRUE) + 200
  par(mar=c(2, 4, 2.5, 1) + 0.1)
  #par(mfrow = c(2,2), mar=c(1, 4, 1, 1) + 0.1)#it goes c(bottom, left, top, right) 
  #par(mfrow=c(2,3))
  yues = c(200, 200, 200, 200)
  for(rs in 1:1){ #length(rrstoplot)
    rrsd = 0.3 #rrstoplot[rs]
    
    #dataset
    #data = datasetsRI[[d]]
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$RRS==rrsd,]
    data[data=="NaN"] = NA
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
    allcols = c(24, 53)
    alladds = c((24+data$maturity[2]),(23+30))
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
remove(d,ones,r,rs, plotdata,data,control,rrsd,rrstoplot,onesc)

####fitness - average number of offspring produced that survived one year, in one year####
cols1 = c(9,11)    #cols1 and cols2 must be the same length
cols2 = c(8,10)   #c(14,16,18)
years = c(100,225)
year1 = year2 = year3 = data.frame(RRS=RRS)
for(c in 1:length(cols1)){
  if(c < 3){
    for(d in 1:length(datasets)){
      #dataset
      data = datasets[[d]]
      data = data[data$startcap==startcap,]
      data = data[data$K==Ks[d],]
      data[data=="NaN"] = NA
      
      #control data
      control = datasets[[d]]
      control = control[control$startcap==controly,]
      control = control[control$K==Ks[d],]
      control[control=="NaN"] = NA
      
      #separate by years interested in
      year1D = data[data$year==years[1],]
      year2D = data[data$year==years[2],]
      year3D = data[data$year==years[3],]
      
      year1Dc = control[control$year==years[1],]
      year2Dc = control[control$year==years[2],]
      year3Dc = control[control$year==years[3],]
      
      #calculate means for each RRS value and fitness estimate of interest
      s1 = s2 = s3 = s1dl = s2dl = s3dl = s1du = s2du = s3du =NULL
      b1 = b2 = b3 = b1dl = b2dl = b3dl = b1du = b2du = b3du = NULL
      s1c = s2c = s3c = s1dcl = s2dcl = s3dcl = s1dcu = s2dcu = s3dcu = NULL
      b1c = b2c = b3c = b1dcl = b2dcl = b3dcl = b1dcu = b2dcu = b3dcu = NULL

      for(r in 1:length(RRS)){
        rrs = RRS[r]
        #change column to change fitness estimate - all offspring produced in one year that survived to next year 14-19
        column.number = cols1[c]
        s1  = c(s1, mean(year1D[year1D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s2  = c(s2, mean(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s3  = c(s3, mean(year3D[year3D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s1dl  = c(s1dl,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        s1du  = c(s1du,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE)) 
        s2dl  = c(s2dl,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        s2du  = c(s2du,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE)) 
        s3dl  = c(s3dl,  quantile(year3D[year3D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        s3du  = c(s3du,  quantile(year3D[year3D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE)) 
        
        s1c  = c(s1c, mean(year1Dc[year1Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s2c  = c(s2c, mean(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s3c  = c(s3c, mean(year3Dc[year3Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s1dcl = c(s1dcl, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        s1dcu = c(s1dcu, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        s2dcl = c(s2dcl, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        s2dcu = c(s2dcu, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        s3dcl = c(s3dcl, quantile(year3Dc[year3Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        s3dcu = c(s3dcu, quantile(year3Dc[year3Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        
        #change column to change fitness estimate - lifetime reproductive success by year 8-13
        column.number = cols2[c]
        b1  = c(b1, mean(year1D[year1D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b2  = c(b2, mean(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b3  = c(b3, mean(year3D[year3D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b1dl  = c(b1dl,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        b1du  = c(b1du,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        b2dl  = c(b2dl,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        b2du  = c(b2du,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        b3dl  = c(b3dl,  quantile(year3D[year3D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        b3du  = c(b3du,  quantile(year3D[year3D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        
        b1c  = c(b1c, mean(year1Dc[year1Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b2c  = c(b2c, mean(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b3c  = c(b3c, mean(year3Dc[year3Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b1dcl = c(b1dcl, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        b1dcu = c(b1dcu, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        b2dcl = c(b2dcl, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        b2dcu = c(b2dcu, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        b3dcl = c(b3dcl, quantile(year3Dc[year3Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        b3dcu = c(b3dcu, quantile(year3Dc[year3Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      }
      year1[paste("AW0", labs[d],"M",   sep="")] = s1
      year1[paste("AW0", labs[d],"SEl", sep="")] = s1dl
      year1[paste("AW0", labs[d],"SEu", sep="")] = s1du
      year2[paste("AW0", labs[d],"M",   sep="")] = s2
      year2[paste("AW0", labs[d],"SEl", sep="")] = s2dl
      year2[paste("AW0", labs[d],"SEu", sep="")] = s2du
      year3[paste("AW0", labs[d],"M",   sep="")] = s3
      year3[paste("AW0", labs[d],"SEl", sep="")] = s3dl
      year3[paste("AW0", labs[d],"SEu", sep="")] = s3du
      
      year1[paste("AW0", "control", labs[d],"M",   sep="")] = s1c
      year1[paste("AW0", "control", labs[d],"SEl", sep="")] = s1dcl
      year1[paste("AW0", "control", labs[d],"SEu", sep="")] = s1dcu
      year2[paste("AW0", "control", labs[d],"M",   sep="")] = s2c
      year2[paste("AW0", "control", labs[d],"SEl", sep="")] = s2dcl
      year2[paste("AW0", "control", labs[d],"SEu", sep="")] = s2dcu
      year3[paste("AW0", "control", labs[d],"M",   sep="")] = s3c
      year3[paste("AW0", "control", labs[d],"SEl", sep="")] = s3dcl
      year3[paste("AW0", "control", labs[d],"SEu", sep="")] = s3dcu
      
      year1[paste("LW0", labs[d],"M",   sep="")] = b1
      year1[paste("LW0", labs[d],"SEl", sep="")] = b1dl
      year1[paste("LW0", labs[d],"SEu", sep="")] = b1du
      year2[paste("LW0", labs[d],"M",   sep="")] = b2
      year2[paste("LW0", labs[d],"SEl", sep="")] = b2dl
      year2[paste("LW0", labs[d],"SEu", sep="")] = b2du
      year3[paste("LW0", labs[d],"M",   sep="")] = b3
      year3[paste("LW0", labs[d],"SEl", sep="")] = b3dl
      year3[paste("LW0", labs[d],"SEu", sep="")] = b3du
      
      year1[paste("LW0", "control", labs[d],"M",   sep="")] = b1c
      year2[paste("LW0", "control", labs[d],"SEl", sep="")] = b1dcl
      year2[paste("LW0", "control", labs[d],"SEu", sep="")] = b1dcu
      year2[paste("LW0", "control", labs[d],"M",   sep="")] = b2c
      year2[paste("LW0", "control", labs[d],"SEl", sep="")] = b2dcl
      year2[paste("LW0", "control", labs[d],"SEu", sep="")] = b2dcu
      year3[paste("LW0", "control", labs[d],"M",   sep="")] = b3c
      year3[paste("LW0", "control", labs[d],"SEl", sep="")] = b3dcl
      year3[paste("LW0", "control", labs[d],"SEu", sep="")] = b3dcu
    }
    
    #plots - all pop and wild born
    summarysets = list(year1=year1,year2=year2,year3=year3)
    yl = c(0,0,0,0,0,0) 
    yu = c(3,3,3,3,3,3) #
    par(mfrow=c(2,3))
    times = c("year1", "year2", "year3",
              "year1", "year2", "year3")
    types = c("LW0","LW0","LW0","AW0","AW0","AW0")
    
    for(t in 1:length(times)){
      tdata = summarysets[[paste(times[t])]]
      plot(-100, -100, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), xlab = "RRS", ylab = "fitness")
      for(s in 1:length(species)){
        move = s - 1
        controlM = tdata[paste(types[t],"control",species[s],"M", sep="")]
        controlM[controlM=="NaN"] = NA
        controlM = controlM[!is.na(controlM)]#nrow(controlM)
        #set control to 1 if it is captive breeders only figures, since nothing to compare to in control
        #if(controlM=="NaN"){controlM=1}
        pdata = data.frame(x=1-tdata[1],
                           M=  tdata[paste(types[t],species[s],"M",   sep="")]/controlM,
                           SEl=tdata[paste(types[t],species[s],"SEl", sep="")]/controlM,
                           SEu=tdata[paste(types[t],species[s],"SEu", sep="")]/controlM)
        colnames(pdata)=c("x","M","SEl", "SEu")
        points(x=(pdata$x + (0.015*move)), y=pdata$M,ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], bg=pcolors[s], pch=21, type="p")
        lines(x=(pdata$x + (0.015*move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], lwd=2)
        par(new=TRUE)
        errbar(x=(pdata$x + (0.015*move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), 
               yplus = pdata$SEu, 
               yminus= pdata$SEl,
               xlab="", ylab="", col.lab="white", cap=.01, lwd=2.5,
               type="l", col="white", errbar.col = pcolors[s], axes=FALSE)
      }
    }
  }
  if(c==3){
    year1 = year2 = year3 = data.frame(RRS=RRS)
    for(d in 1:length(datasets)){
      #dataset
      data = datasets[[d]]
      data = data[data$startcap==startcap,]
      data = data[data$K==Ks[d],]
      data[data=="NaN"] = NA
      
      #control data
      control = datasets[[d]]
      control = control[control$startcap==controly,]
      control = control[control$K==Ks[d],]
      control[control=="NaN"] = NA
      
      #separate by years interested in
      year2D = data[data$year==years[2],]
      year2Dc = control[control$year==years[2],]
      
      #calculate means for each RRS value and fitness estimate of interest
      s2 = s2dl = s2du = NULL
      b2 = b2dl = b2du = NULL
      s2c = s2dcl = s2dcu = NULL
      b2c = b2dcl = b2dcu = NULL
      
      for(r in 1:length(RRS)){
        rrs = RRS[r]
        #change column to change fitness estimate - all offspring produced in one year that survived to next year 14-19
        column.number = cols1[c]
        s2    = c(s2,    mean(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s2dl  = c(s2dl,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        s2du  = c(s2du,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE)) 
        s2c   = c(s2c,   mean(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        s2dcl = c(s2dcl, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        s2dcu = c(s2dcu, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        
        #change column to change fitness estimate - lifetime reproductive success by year 8-13
        column.number = cols2[c]
        b2    = c(b2,    mean(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b2dl  = c(b2dl,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
        b2du  = c(b2du,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
        b2c   = c(b2c,   mean(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
        b2dcl = c(b2dcl, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
        b2dcu = c(b2dcu, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      }
      year2[paste("AW0", labs[d],"M",  sep="")] = s2
      year2[paste("AW0", labs[d],"SEl", sep="")] = s2dl
      year2[paste("AW0", labs[d],"SEu", sep="")] = s2du

      year2[paste("AW0", "control", labs[d],"M",  sep="")] = s2c
      year2[paste("AW0", "control", labs[d],"SEl", sep="")] = s2dcl
      year2[paste("AW0", "control", labs[d],"SEu", sep="")] = s2dcu

      year2[paste("LW0", labs[d],"M",  sep="")] = b2
      year2[paste("LW0", labs[d],"SEl", sep="")] = b2dl
      year2[paste("LW0", labs[d],"SEu", sep="")] = b2du

      year2[paste("LW0", "control", labs[d],"M",  sep="")] = b2c
      year2[paste("LW0", "control", labs[d],"SEl", sep="")] = b2dcl
      year2[paste("LW0", "control", labs[d],"SEu", sep="")] = b2dcu
    }
    #captive born only
    summarysets = list(year2=year2)
    yl = c(0,0,0,0) 
    yu = c(2,2,6,6) #
    par(mfrow=c(2,2))
    times = c("year2", "year2",
              "year2", "year2")
    types = c("LW0","AW0","LW0","AW0")
    
    for(t in 1:length(times)){
      tdata = summarysets[[paste(times[t])]]
      plot(-100, -100, ylim = c(yl[t], yu[t]), xlim = c(0,1.1), xlab = "RRS", ylab = "fitness")
      for(s in 1:length(species)){
        move = s - 1
        tdata = tdata[tdata[,1]!=1,,drop=FALSE]
        pdata = data.frame(x=1-tdata[1],
                           M=  tdata[paste(types[t],species[s],"M",   sep="")],
                           SEl=tdata[paste(types[t],species[s],"SEl", sep="")],
                           SEu=tdata[paste(types[t],species[s],"SEu", sep="")])
        colnames(pdata)=c("x","M","SEl", "SEu")
        lines(x=pdata$x + (0.015*move), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], lwd=2, lty=1)
        points(x=pdata$x + (0.015*move), y=pdata$M,ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], bg=pcolors[s], pch=21, type="p")
        par(new=TRUE)
        errbar(x=pdata$x + (0.015*move), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), 
               yplus = pdata$SEu, 
               yminus= pdata$SEl,
               xlab="", ylab="", col.lab="white", cap=.01, lwd=2.5,
               type="l", col="white", errbar.col = pcolors[s], axes=FALSE)
      }
    }
  }
}
remove(summarysets,move,control,data,pdata,tdata,year1,year1D,year1Dc,year2,year2D,year2Dc,year3,year3D,year3Dc,b1,b1c,b1dcl,b1dcu,b1du,b1dl,b2,b2c,b2dcl,b2dcu,b2dl,b2du,b3,b3c,b3dcl,b3dcu,b3dl,b3du,c,cols1,cols2,column.number,controlM,d,r,rrs,s,s1,s1c,s1dcl,s1dcu,s2,s2c,s2dcl,s2dcu,s2dl,s2du,s1dl,s1du,s3,s3c,s3dcl,s3dcu,s3dl,s3du,t,times,types,years,yl,yu)

####fitness - average number of offspring produced that survived one year, UPDATED 1 COL####
cols1 = c(16)    #cols1 and cols2 must be the same length
#cols2 = c(8,10)   #c(14,16,18)
years = c(100, 225)
year1 = year2 = data.frame(RRS=RRS)
for(c in 1:length(cols1)){
  for(d in 1:length(datasets)){
    #dataset
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$K==Ks[d],]
    data[data=="NaN"] = NA
    
    #control data
    control = datasets[[d]]
    control = control[control$startcap==controly,]
    control = control[control$K==Ks[d],]
    control[control=="NaN"] = NA
    
    #separate by years interested in
    year1D = data[data$year==years[1],]
    year2D = data[data$year==years[2],]
    
    year1Dc = control[control$year==years[1],]
    year2Dc = control[control$year==years[2],]
    
    #calculate means for each RRS value and fitness estimate of interest
    s1 = s2 = s3 = s1dl = s2dl = s3dl = s1du = s2du = s3du =NULL
    b1 = b2 = b3 = b1dl = b2dl = b3dl = b1du = b2du = b3du = NULL
    s1c = s2c = s3c = s1dcl = s2dcl = s3dcl = s1dcu = s2dcu = s3dcu = NULL
    b1c = b2c = b3c = b1dcl = b2dcl = b3dcl = b1dcu = b2dcu = b3dcu = NULL
    
    for(r in 1:length(RRS)){
      rrs = RRS[r]
      #change column to change fitness estimate - lifetime reproductive success by year 8-13
      column.number = cols1[c]
      b1  = c(b1, mean(year1D[year1D$RRS==as.character(rrs), column.number], na.rm=TRUE))
      b2  = c(b2, mean(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE))
      b1dl  = c(b1dl,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
      b1du  = c(b1du,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      b2dl  = c(b2dl,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
      b2du  = c(b2du,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      
      b1c  = c(b1c, mean(year1Dc[year1Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
      b2c  = c(b2c, mean(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
      b1dcl = c(b1dcl, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
      b1dcu = c(b1dcu, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      b2dcl = c(b2dcl, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
      b2dcu = c(b2dcu, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
    }
    year1[paste("LW0", labs[d],"M",   sep="")] = b1
    year1[paste("LW0", labs[d],"SEl", sep="")] = b1dl
    year1[paste("LW0", labs[d],"SEu", sep="")] = b1du
    year2[paste("LW0", labs[d],"M",   sep="")] = b2
    year2[paste("LW0", labs[d],"SEl", sep="")] = b2dl
    year2[paste("LW0", labs[d],"SEu", sep="")] = b2du
    
    year1[paste("LW0", "control", labs[d],"M",   sep="")] = b1c
    year2[paste("LW0", "control", labs[d],"SEl", sep="")] = b1dcl
    year2[paste("LW0", "control", labs[d],"SEu", sep="")] = b1dcu
    year2[paste("LW0", "control", labs[d],"M",   sep="")] = b2c
    year2[paste("LW0", "control", labs[d],"SEl", sep="")] = b2dcl
    year2[paste("LW0", "control", labs[d],"SEu", sep="")] = b2dcu
  }
  
  #plots - all pop and wild born
  summarysets = list(year1=year1,year2=year2)
  yl = c(0,0) 
  yu = c(3,3) #
  par(mfrow=c(1,1))
  times = c("year1", "year2")
  types = c("LW0","LW0")
  for(t in 1:length(times)){
    tdata = summarysets[[paste(times[t])]]
    if(t==1){
      plot(-100, -100, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), xlab = "RRS", ylab = "number of offspring", main="during supplementation")
      abline(h=1, lty=2, col="grey50")
    }
    if(t==2){
      plot(-100, -100, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), xlab = "RRS", ylab = "number of offspring", main="100 years post supplementation")
      abline(h=1, lty=2, col="grey50")
    }
    for(s in 1:length(species)){
      move = s - 1
      controlM = tdata[paste(types[t],"control",species[s],"M", sep="")]
      controlM[controlM=="NaN"] = NA
      controlM = controlM[!is.na(controlM)]#nrow(controlM)
      #set control to 1 if it is captive breeders only figures, since nothing to compare to in control
      #if(controlM=="NaN"){controlM=1}
      pdata = data.frame(x=1-tdata[1],
                         M=  tdata[paste(types[t],species[s],"M",   sep="")]/controlM,
                         SEl=tdata[paste(types[t],species[s],"SEl", sep="")],
                         SEu=tdata[paste(types[t],species[s],"SEu", sep="")])
      colnames(pdata)=c("x","M","SEl", "SEu")
      par(new=TRUE)
      errbar(x=(pdata$x + (0.0035*move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), 
             yplus = pdata$M + pdata$SEu, 
             yminus= pdata$M - pdata$SEl,
             xlab="", ylab="", col.lab="white", cap=.0, lwd=2.5,
             type="l", col="white", errbar.col = pcolors[s], axes=FALSE)
      points(x=(pdata$x + (0.0035*move)), y=pdata$M,ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], bg=pcolors[s], pch=21, type="p")
      lines(x=(pdata$x + (0.0035*move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], lwd=3)
    }
  }
}
remove(summarysets,move,control,data,pdata,tdata,year1,year1D,year1Dc,year2,year2D,year2Dc,year3,year3D,year3Dc,b1,b1c,b1dcl,b1dcu,b1du,b1dl,b2,b2c,b2dcl,b2dcu,b2dl,b2du,b3,b3c,b3dcl,b3dcu,b3dl,b3du,c,cols1,cols2,column.number,controlM,d,r,rrs,s,s1,s1c,s1dcl,s1dcu,s2,s2c,s2dcl,s2dcu,s2dl,s2du,s1dl,s1du,s3,s3c,s3dcl,s3dcu,s3dl,s3du,t,times,types,years,yl,yu)

####fitness -lifetime UPDATED####
cols1 = 10    
years = c(100,225)
year1 = year2 = year3 = data.frame(RRS=RRS)
for(c in 1:length(cols1)){
  for(d in 1:length(datasets)){
    #dataset
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$K==Ks[d],]
    data[data=="NaN"] = NA
    
    #control data
    control = datasets[[d]]
    control = control[control$startcap==controly,]
    control = control[control$K==Ks[d],]
    control[control=="NaN"] = NA
    
    #separate by years interested in
    year1D = data[data$year==years[1],]
    year2D = data[data$year==years[2],]
    year3D = data[data$year==years[3],]
    year1Dc = control[control$year==years[1],]
    year2Dc = control[control$year==years[2],]
    year3Dc = control[control$year==years[3],]
    
    #calculate means for each RRS value and fitness estimate of interest
    s1 = s2 = s3 = s1dl = s2dl = s3dl = s1du = s2du = s3du =NULL
    s1c = s2c = s3c = s1dcl = s2dcl = s3dcl = s1dcu = s2dcu = s3dcu = NULL
    
    for(r in 1:length(RRS)){
      rrs = RRS[r]
      #change column to change fitness estimate - lifetime reproductive success
      column.number = cols1[c]
      s1  = c(s1, mean(year1D[year1D$RRS==as.character(rrs), column.number], na.rm=TRUE))
      s2  = c(s2, mean(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE))
      s3  = c(s3, mean(year3D[year3D$RRS==as.character(rrs), column.number], na.rm=TRUE))
      s1dl  = c(s1dl,  (sd(year1D[year1D$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year1D[year1D$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s1du  = c(s1du,  (sd(year1D[year1D$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year1D[year1D$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s2dl  = c(s2dl,  (sd(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year2D[year2D$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s2du  = c(s2du,  (sd(year2D[year2D$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year2D[year2D$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s3dl  = c(s3dl,  (sd(year3D[year3D$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year3D[year3D$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s3du  = c(s3du,  (sd(year3D[year3D$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year3D[year3D$RRS==as.character(rrs), column.number])) )) * 1.96 )
      
      #s1dl  = c(s1dl,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
      #s1du  = c(s1du,  quantile(year1D[year1D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE)) 
      #s2dl  = c(s2dl,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
      #s2du  = c(s2du,  quantile(year2D[year2D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE)) 
      #s3dl  = c(s3dl,  quantile(year3D[year3D$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE)) 
      #s3du  = c(s3du,  quantile(year3D[year3D$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE)) 
      
      s1c  = c(s1c, mean(year1Dc[year1Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
      s2c  = c(s2c, mean(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
      s3c  = c(s3c, mean(year3Dc[year3Dc$RRS==as.character(rrs), column.number], na.rm=TRUE))
      s1dcl = c(s1dcl, (sd(year1Dc[year1Dc$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year1Dc[year1Dc$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s1dcu = c(s1dcu, (sd(year1Dc[year1Dc$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year1Dc[year1Dc$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s2dcl = c(s2dcl, (sd(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year2Dc[year2Dc$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s2dcu = c(s2dcu, (sd(year2Dc[year2Dc$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year2Dc[year2Dc$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s3dcl = c(s3dcl, (sd(year3Dc[year3Dc$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year3Dc[year3Dc$RRS==as.character(rrs), column.number])) )) * 1.96 )
      s3dcu = c(s3dcu, (sd(year3Dc[year3Dc$RRS==as.character(rrs), column.number], na.rm=TRUE) / (sqrt(length(year3Dc[year3Dc$RRS==as.character(rrs), column.number])) )) * 1.96 )
      
      #s1dcl = c(s1dcl, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
      #s1dcu = c(s1dcu, quantile(year1Dc[year1Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      #s2dcl = c(s2dcl, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
      #s2dcu = c(s2dcu, quantile(year2Dc[year2Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      #s3dcl = c(s3dcl, quantile(year3Dc[year3Dc$RRS==as.character(rrs), column.number], probs = 0.025, na.rm=TRUE))
      #s3dcu = c(s3dcu, quantile(year3Dc[year3Dc$RRS==as.character(rrs), column.number], probs = 0.975, na.rm=TRUE))
      
    }
    year1[paste("AW0", labs[d],"M",   sep="")] = s1
    year1[paste("AW0", labs[d],"SEl", sep="")] = s1dl
    year1[paste("AW0", labs[d],"SEu", sep="")] = s1du
    year2[paste("AW0", labs[d],"M",   sep="")] = s2
    year2[paste("AW0", labs[d],"SEl", sep="")] = s2dl
    year2[paste("AW0", labs[d],"SEu", sep="")] = s2du
    year3[paste("AW0", labs[d],"M",   sep="")] = s3
    year3[paste("AW0", labs[d],"SEl", sep="")] = s3dl
    year3[paste("AW0", labs[d],"SEu", sep="")] = s3du
    
    year1[paste("AW0", "control", labs[d],"M",   sep="")] = s1c
    year1[paste("AW0", "control", labs[d],"SEl", sep="")] = s1dcl
    year1[paste("AW0", "control", labs[d],"SEu", sep="")] = s1dcu
    year2[paste("AW0", "control", labs[d],"M",   sep="")] = s2c
    year2[paste("AW0", "control", labs[d],"SEl", sep="")] = s2dcl
    year2[paste("AW0", "control", labs[d],"SEu", sep="")] = s2dcu
    year3[paste("AW0", "control", labs[d],"M",   sep="")] = s3c
    year3[paste("AW0", "control", labs[d],"SEl", sep="")] = s3dcl
    year3[paste("AW0", "control", labs[d],"SEu", sep="")] = s3dcu
  }
  
  #plots - all pop and wild born
  summarysets = list(year1=year1,year2=year2,year3=year3)
  yl = c(0,0) 
  yu = c(8,8) #
  par(mfrow=c(1,1))
  times = c("year1", "year2")
  types = c("AW0","AW0")
  
  for(t in 1:length(times)){
    tdata = summarysets[[paste(times[t])]]
    plot(-100, -100, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), xlab = "RRS", ylab = "mean lifetime reproductive success", main = "during supplementation")
    abline(h=1, lty=2, col="grey50")
    for(s in 1:length(species)){
      move = s - 1
      controlM = tdata[paste(types[t],"control",species[s],"M", sep="")]
      controlM[controlM=="NaN"] = NA
      controlM = controlM[!is.na(controlM)]#nrow(controlM)
      #set control to 1 if it is captive breeders only figures, since nothing to compare to in control
      #if(controlM=="NaN"){controlM=1}
      pdata = data.frame(x=1-tdata[1],
                         M=  tdata[paste(types[t],species[s],"M",   sep="")]/controlM,
                         SEl=tdata[paste(types[t],species[s],"SEl", sep="")],
                         SEu=tdata[paste(types[t],species[s],"SEu", sep="")])
      colnames(pdata)=c("x","M","SEl", "SEu")
      par(new=TRUE)
      errbar(x=(pdata$x + (0.0035*move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), 
             yplus = pdata$M + pdata$SEu, 
             yminus= pdata$M - pdata$SEl,
             xlab="", ylab="", col.lab="white", cap=.0, lwd=2.5,
             type="l", col="white", errbar.col = pcolors[s], axes=FALSE)
      points(x=(pdata$x + (0.0035*move)), y=pdata$M,ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], bg=pcolors[s], pch=21, type="p")
      lines(x=(pdata$x + (0.0035*move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 1.1), col=pcolors[s], lwd=3)
    }
  }  
}
remove(summarysets,move,control,data,pdata,tdata,year1,year1D,year1Dc,year2,year2D,year2Dc,year3,year3D,year3Dc,c,cols1,column.number,controlM,d,r,rrs,s,s1,s1c,s1dcl,s1dcu,s2,s2c,s2dcl,s2dcu,s2dl,s2du,s1dl,s1du,s3,s3c,s3dcl,s3dcu,s3dl,s3du,t,times,types,years,yl,yu)

####meanRRS compared to fitness--overall####
#meanRRSall meanRRSwild meanRRShatch
years = seq(1,gens,1)
spear = pears = NULL
for(i in 1:length(years)){
  data = datasets[[1]]
  data = subset(data, data$year==years[i])
  spear = c(spear, cor(data$meanRRSall, data[,9], method="spearman"))
  pears = c(pears, cor(data$meanRRSall, data[,9], method="pearson"))
  year = c(years, years[i])
}
summ = cbind(years, spear, pears)
colnames(summ) = c("years", "spearman", "pearson")
par(mfrow=c(1,2))
plot(summ[,1], summ[,2], xlab="year", ylab="spearman r")
plot(summ[,1], summ[,3], xlab="year", ylab="pearson r")

plot(data$meanRRSall, data$fittW0B)
x=lm(meanRRSall~fittN0A, data=data)
summary(x)
plot(data$meanRRSall, data$fittN0A)

###RRS, population size, Ho, A####
rrstoplot = c(0.5,0.7) #REMEMBER: THIS IS RRS PENALTY
#par(mfrow=c(4,3))
for(d in 1:length(datasets)){
  #par(mfrow=c(3,3))
  #set up color plotting variables
  #poor environment before captive breeding
  plotpoorenv = c(50,74)
  plotcaptive = c(startcap-1, endcap)
  #control data
  control = datasets[[d]]
  control = control[control$startcap==controly,]
  control[control=="NaN"] = NA
  control$NumAllelesAverage = control$NumAlleles
  onesc = subset(1:length(control$year), control$year==1)
  
  #determine ylim for population size for each species
  maxy = 1000
  par(mar=c(5.1,5.1,4.1,2.1))
  par(mfrow=c(2,3))
  yues = c(200, 200, 200, 200)
  for(rs in 1:length(rrstoplot)){
    rrsd = rrstoplot[rs]
    
    #dataset
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$RRS==rrsd,]
    data[data=="NaN"] = NA
    data$NumAllelesAverage = data$NumAlleles
    ones = subset(1:length(data$year), data$year==1)
    
    ###mean RRS
    plot(-100, -100, ylim = c(0,1), xlim = c(25, gens), xlab = "year", ylab = "mean RRS", main=paste(labs[d], 1-rrsd, sep=" "), cex.lab=1, cex.lab=1)
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){
        plotdata = control[onesc[r]:nrow(control),]
      }else{
        plotdata = control[onesc[r]:(onesc[r+1]-1),]
      }
      lines(plotdata$year[25:nrow(plotdata)], plotdata$meanRRSall[25:nrow(plotdata)], xlim=c(25,250), ylim=c(0,1), type="l", xlab="", ylab="", col="grey80", lwd=1.2)
    }
    #data
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
    allcols = c(24, 53)
    alladds = c((24+data$maturity[2]),(23+30))
    plot(-100, -100, ylim = c(0, maxy), xlim = c(25, gens), xlab = "year", ylab = "population size", cex.lab=1, cex.lab=1)
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){
        plotdata = control[onesc[r]:nrow(control),]
      }else{
        plotdata = control[onesc[r]:(onesc[r+1]-1),]
      }
      #lines(plotdata$year[1:nrow(plotdata)], plotdata[1:gens,23], xlim=c(1,gens), ylim=c(0,2000), type="l", xlab="", ylab="", col="black") #0
      lines(plotdata$year[25:nrow(plotdata)], apply(plotdata[25:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col="grey60", lwd=1.2) #1 now all
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
      
      lines(plotdata$year[25:plotpoorenv[1]], apply(plotdata[25:plotpoorenv[1],allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
      #lines(plotdata$year[25:nrow(plotdata)], apply(plotdata[25:gens,alladds[1]:alladds[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=scolors[d], lwd=1.2) #2
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], apply(plotdata[plotcaptive[2]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
      
      #poor environment colors
      if(nrow(plotdata)>plotpoorenv[2]){
        lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], apply(plotdata[plotpoorenv[1]:plotpoorenv[2],allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
        #lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], apply(plotdata[plotpoorenv[1]:plotpoorenv[2],alladds[1]:alladds[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=poorenvcol, lwd=1.2)
      }else{
        lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], apply(plotdata[plotpoorenv[1]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
        #lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], apply(plotdata[plotpoorenv[1]:nrow(plotdata),alladds[1]:alladds[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=poorenvcol, lwd=1.2)
      }
      
      #captive breeding colors
      if(nrow(plotdata)>plotcaptive[2]){
        lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], apply(plotdata[plotcaptive[1]:plotcaptive[2],allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
        #lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], apply(plotdata[plotcaptive[1]:plotcaptive[2],alladds[1]:alladds[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=captivecol, lwd=1.2)
      }else{
        lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], apply(plotdata[plotcaptive[1]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
        #lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], apply(plotdata[plotcaptive[1]:nrow(plotdata),alladds[1]:alladds[2]], 1,sum), xlim=c(25,gens), ylim=c(0,5000), type="l", xlab="", ylab="", col=captivecol, lwd=1.2)
      }
    }
    
    
    ###Number of alleles
    yu = 30
    yl = 0
    plot(-100, -100, ylim = c(yl,yu), xlim = c(25, gens), xlab = "year", ylab = "number of alleles/locus", cex.lab=1, cex.lab=1)
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){
        plotdata = control[onesc[r]:nrow(control),]
      }else{
        plotdata = control[onesc[r]:(onesc[r+1]-1),]
      }
      lines(plotdata$year[25:nrow(plotdata)], plotdata$NumAllelesAverage[25:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col="grey80", lwd=1.2)
    }
    #data
    for(r in 1:length(ones)){
      if(r==length(ones)){
        plotdata = data[ones[r]:nrow(data),]
      }else{
        plotdata = data[ones[r]:(ones[r+1]-1),]
      }
      lines(plotdata$year[25:plotpoorenv[1]], plotdata$NumAllelesAverage[25:plotpoorenv[1]], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$NumAllelesAverage[plotcaptive[2]:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      
      #poor environment colors
      if(nrow(plotdata)>plotpoorenv[2]){
        lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], plotdata$NumAllelesAverage[plotpoorenv[1]:plotpoorenv[2]], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }else{
        lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], plotdata$NumAllelesAverage[plotpoorenv[1]:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }
      
      #captive breeding colors
      if(nrow(plotdata)>plotcaptive[2]){
        lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$NumAllelesAverage[plotcaptive[1]:plotcaptive[2]], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }else{
        lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], plotdata$NumAllelesAverage[plotcaptive[1]:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }
    }
  }
}
remove(d,ones,r,rs, plotdata,data,control,rrsd,rrstoplot,onesc)

###RRS, population size, Ho, A -- RELATIVE TO MEAN CONTROL####
rrstoplot = c(0,0.3) #REMEMBER: THIS IS RRS PENALTY
#par(mfrow=c(4,3))
#par(mfrow=c(1,1))
for(d in 1:length(datasets)){
  #par(mfrow=c(3,3))
  #set up color plotting variables
  #poor environment before captive breeding
  if(poorenv < startcap){
    #environment fixed before captive breeding
    if(fixedenv < startcap){
      plotpoorenv = c(poorenv-1, fixedenv)
    }
    #environment never fixed
    if(startcap < fixedenv){
      plotpoorenv = c(poorenv-1, startcap-1)
    }
  }
  plotcaptive = c(startcap-1, endcap)
  #control data
  control = datasets[[d]]
  control = control[control$startcap==controly,]
  control[control=="NaN"] = NA
  control$NumAllelesAverage = control$NumAlleles
  onesc = subset(1:length(control$year), control$year==1)
  
  #determine ylim for population size for each species
  maxpop = datasets[[d]]
  maxy = max(apply(maxpop[,23:(23+(ncol(maxpop) - 34))], 1, sum), na.rm=TRUE) + 200
  par(mar=c(5.1,5.1,4.1,2.1))
  par(mfrow=c(2,3))
  yues = c(200, 200, 200, 200)
  for(rs in 1:length(rrstoplot)){
    rrsd = rrstoplot[rs]
    
    #dataset
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$RRS==rrsd,]
    data[data=="NaN"] = NA
    data$NumAllelesAverage = data$NumAlleles
    ones = subset(1:length(data$year), data$year==1)
    
    ###mean RRS
    plot(-100, -100, ylim = c(-1,1), xlim = c(25, gens), xlab = "year", ylab = "mean RRS", main=paste(labs[d], 1-rrsd, sep=" "), cex.lab=1, cex.lab=1)
    means = NULL
    #means
    for(r in 1:length(onesc)){
      if(r==length(onesc)){plotdata = control[onesc[r]:nrow(control),]
      }else{               plotdata = control[onesc[r]:(onesc[r+1]-1),]}
      if(length(plotdata$meanRRSall[1:nrow(plotdata)])>=250){ means = rbind(means, c(plotdata$meanRRSall[1:250]))
      }else{                                                  means = rbind(means, c(plotdata$meanRRSall[1:nrow(plotdata)], rep(NA, 250 - nrow(plotdata)) ))}
    }
    allmeans = apply(means, 2, mean)
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){ plotdata = control[onesc[r]:nrow(control),]
      }else{                plotdata = control[onesc[r]:(onesc[r+1]-1),] }
      lines(plotdata$year[25:nrow(plotdata)], plotdata$meanRRSall[25:nrow(plotdata)] - allmeans[25:nrow(plotdata)], xlim=c(25,250), ylim=c(-1,1), type="l", xlab="", ylab="", col="grey80", lwd=1.2)
    }
    #data
    for(r in 1:length(ones)){
      if(r==length(ones)){ plotdata = data[ones[r]:nrow(data),]
      }else{               plotdata = data[ones[r]:(ones[r+1]-1),] }
      lines(plotdata$year[25:plotpoorenv[1]],             plotdata$meanRRSall[25:plotpoorenv[1]]             - allmeans[25:plotpoorenv[1]],             xlim=c(25,gens), ylim=c(-1,1), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$meanRRSall[plotcaptive[2]:nrow(plotdata)] - allmeans[plotcaptive[2]:nrow(plotdata)], xlim=c(25,gens), ylim=c(-1,1), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
      
      #poor environment colors
      if(nrow(plotdata)>plotpoorenv[2]){lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], plotdata$meanRRSall[plotpoorenv[1]:plotpoorenv[2]] - allmeans[plotpoorenv[1]:plotpoorenv[2]], xlim=c(25,gens), ylim=c(-1,1), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }else{                            lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], plotdata$meanRRSall[plotpoorenv[1]:nrow(plotdata)] - allmeans[plotpoorenv[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(-1,1), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)}
      #captive breeding colors
      if(nrow(plotdata)>plotcaptive[2]){lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$meanRRSall[plotcaptive[1]:plotcaptive[2]] - allmeans[plotcaptive[1]:plotcaptive[2]], xlim=c(25,gens), ylim=c(-1,1), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }else{                            lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], plotdata$meanRRSall[plotcaptive[1]:nrow(plotdata)] - allmeans[plotcaptive[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(-1,1), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)}
    }
    
    ####population size
    allcols = c(24, 53)
    alladds = c((24+data$maturity[2]),(23+30))
    plot(-100, -100, ylim=c(-500,500), xlim = c(25, gens), xlab = "year", ylab = "population size", cex.lab=1, cex.lab=1)
    means = NULL
    #means
    for(r in 1:length(onesc)){
      if(r==length(onesc)){plotdata = control[onesc[r]:nrow(control),]
      }else{               plotdata = control[onesc[r]:(onesc[r+1]-1),]}
      if(length(plotdata$meanRRSall[1:nrow(plotdata)])>=250){ means = rbind(means, c(apply(plotdata[1:nrow(plotdata),allcols[1]:allcols[2]], 1,sum, na.rm=TRUE)[1:250]))
      }else{                                                  means = rbind(means, c(apply(plotdata[1:nrow(plotdata),allcols[1]:allcols[2]], 1,sum, na.rm=TRUE), rep(NA, 250 - nrow(plotdata)) ))}
    }
    allmeans = apply(means, 2, mean)
    #control
    for(r in 1:length(onesc)){
      if(r==length(onesc)){ plotdata = control[onesc[r]:nrow(control),]
      }else{                plotdata = control[onesc[r]:(onesc[r+1]-1),] }
      lines(plotdata$year[25:nrow(plotdata)], apply(plotdata[25:nrow(plotdata),allcols[1]:allcols[2]], 1,sum) - allmeans[25:nrow(plotdata)], xlim=c(25,gens), ylim=c(-500,500), type="l", xlab="", ylab="", col="grey60", lwd=1.2) #1 now all
    }
    #data
    for(r in 1:length(ones)){
      if(r==length(ones)){ plotdata = data[ones[r]:nrow(data),]
      }else{               plotdata = data[ones[r]:(ones[r+1]-1),]}
      lines(plotdata$year[25:plotpoorenv[1]],             apply(plotdata[25:plotpoorenv[1],allcols[1]:allcols[2]], 1,sum) - allmeans[25:plotpoorenv[1]], xlim=c(25,gens), ylim=c(-500,500), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
      lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], apply(plotdata[plotcaptive[2]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum) - allmeans[plotcaptive[2]:nrow(plotdata)], xlim=c(25,gens), ylim=c(-500,500), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
      #poor environment colors
      if(nrow(plotdata)>plotpoorenv[2]){ lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], apply(plotdata[plotpoorenv[1]:plotpoorenv[2],allcols[1]:allcols[2]], 1,sum) - allmeans[plotpoorenv[1]:plotpoorenv[2]], xlim=c(25,gens), ylim=c(-500,500), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
      }else{                             lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], apply(plotdata[plotpoorenv[1]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum) - allmeans[plotpoorenv[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(-500,500), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)}
      #captive breeding colors
      if(nrow(plotdata)>plotcaptive[2]){ lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], apply(plotdata[plotcaptive[1]:plotcaptive[2],allcols[1]:allcols[2]], 1,sum) - allmeans[plotcaptive[1]:plotcaptive[2]], xlim=c(25,gens), ylim=c(-500,500), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
      }else{                             lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], apply(plotdata[plotcaptive[1]:nrow(plotdata),allcols[1]:allcols[2]], 1,sum) - allmeans[plotcaptive[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(-500,500), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)}
    }
    
    
    ###Number of alleles
    yu = 20
    yl = -20
    plot(-100, -100, ylim = c(yl,yu), xlim = c(25, gens), xlab = "year", ylab = "number of alleles/locus", cex.lab=1, cex.lab=1)
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
      lines(plotdata$year[25:nrow(plotdata)], plotdata$NumAllelesAverage[25:nrow(plotdata)] - allmeans[25:nrow(plotdata)], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col="grey80", lwd=1.2)
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
remove(d,ones,r,rs, plotdata,data,control,rrsd,rrstoplot,onesc,means,allmeans)

####CIs for NE####
rrstoplot = c(0,0.3) #REMEMBER: THIS IS RRS PENALTY. 
for(rs in 1:length(rrstoplot)){
  neCIs = NULL
  neCIs = cbind(neCIs, seq(1, 250, 1))
  for(d in 1:length(datasets)){
    #control data
    control = datasets[[d]]
    control = control[control$startcap==controly,]
    control[control=="NaN"] = NA
    
    #other data
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$RRS==rrstoplot[rs],]
    data[data=="NaN"] = NA
    
    cm = ccil = cciu = dm = dcil = dciu = NULL
    for(y in 1:250){
      cd   = subset(control, control$year==y)
      cm   = c(cm, mean(cd$Ne, na.rm=TRUE))
      ccil = c(ccil, quantile(cd$Ne, probs = 0.025, na.rm=TRUE))
      cciu = c(cciu, quantile(cd$Ne, probs = 0.975, na.rm=TRUE))
      
      dd = subset(data,    data$year==y)
      dm   = c(dm, mean(dd$Ne, na.rm=TRUE))
      dcil = c(dcil, quantile(dd$Ne, probs = 0.025, na.rm=TRUE))
      dciu = c(dciu, quantile(dd$Ne, probs = 0.975, na.rm=TRUE))
    }
    neCIs = cbind(neCIs, cm, ccil, cciu, dm, dcil, dciu)
  }
  colnames(neCIs) = c("years",
                      "craneMC", "craneCIlC", "craneCIuC", "craneDC", "craneCIlD", "craneCIuD",
                      "tamarinMC", "tamarinCIlC", "tamarinCIuC", "tamarinDC", "tamarinCIlD", "tamarinCIuD",
                      "toadMC", "toadCIlC", "toadCIuC", "toadDC", "toadCIlD", "toadCIuD",
                      "salmonMC", "salmonCIlC", "salmonCIuC", "salmonDC", "salmonCIlD", "salmonCIuD")
  rownames(neCIs) = NULL
  neCIs = as.data.frame(neCIs)
  
  par(mfrow=c(1,4))
  #controls
  plot(x=neCIs$years[25:250], y=neCIs$craneCIlC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", main="crane", xlab="year", ylab="Ne")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$craneCIuC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  #data
  plot(x=neCIs$years[25:250], y=neCIs$craneCIlD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$craneCIuD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  
  #controls
  plot(x=neCIs$years[25:250], y=neCIs$salmonCIlC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", main="salmon", xlab="year", ylab="Ne")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$salmonCIuC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  #data
  plot(x=neCIs$years[25:250], y=neCIs$salmonCIlD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$salmonCIuD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  
  #controls
  plot(x=neCIs$years[25:250], y=neCIs$tamarinCIlC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", main="tamarin", xlab="year", ylab="Ne")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$tamarinCIuC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  #data
  plot(x=neCIs$years[25:250], y=neCIs$tamarinCIlD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$tamarinCIuD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  
  #controls
  plot(x=neCIs$years[25:250], y=neCIs$toadCIlC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", main="toad", xlab="year", ylab="Ne")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$toadCIuC[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="grey50", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  #data
  plot(x=neCIs$years[25:250], y=neCIs$toadCIlD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  par(new=TRUE)
  plot(x=neCIs$years[25:250], y=neCIs$toadCIuD[25:250], xlim=c(25,250), ylim=c(0, 2000), lty=1, type="l", col="black", axes=FALSE, xlab="", ylab="")
  
}
remove(cd,control,data,dd,neCIs,ccil,cciu,cm,d,dcil,dciu,dm,rrstoplot,y)

####prophatchery####
yl = 0
yu = 1
phatchery = NULL
par(mfrow=c(1,1))
for(d in 1:length(datasets)){
  data = datasets[[d]]
  data = data[data$startcap==startcap,]
  data = data[data$K==Ks[d],]
  data = data[data$year>=50,]
  data = data[data$year<151,]
  #phatchery[paste(labs[d],"M",sep="")] = c(phatchery[paste(labs[d],"M",sep="")], mean(data$propHatchery, na.rm=TRUE))
  #phatchery[paste(labs[d],"SE",sep="")] = c(phatchery[paste(labs[d],"SE",sep="")], sd(data$propHatchery, na.rm=TRUE)/sqrt(length(data$propHatchery)))
  plot(data$year, data$propHatchery, ylim = c(yl, yu), xlim = c(50,150), xlab = "year", ylab = "proportion hatchery", col=pcolors[d])
}
remove(yl,yu,phatchery,data)

####heterozygosity and alleles panels####
###Heterozygosity 
yu = 1.05
yl = 0.6
plot(-100, -100, ylim = c(yl,yu), xlim = c(25, gens), xlab = "year", ylab = "heterozygosity retained (%)", cex.lab=1, cex.lab=1)
#control
for(r in 1:length(onesc)){
  if(r==length(onesc)){
    plotdata = control[onesc[r]:nrow(control),]
  }else{
    plotdata = control[onesc[r]:(onesc[r+1]-1),]
  }
  lines(plotdata$year[25:nrow(plotdata)], plotdata$Ho[25:gens]/plotdata$Ho[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col="grey80", lwd=1.2)
}
#data
for(r in 1:length(ones)){
  if(r==length(ones)){
    plotdata = data[ones[r]:nrow(data),]
  }else{
    plotdata = data[ones[r]:(ones[r+1]-1),]
  }
  lines(plotdata$year[25:plotpoorenv[1]], plotdata$Ho[25:plotpoorenv[1]]/plotdata$Ho[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
  lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$Ho[plotcaptive[2]:nrow(plotdata)]/plotdata$Ho[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
  
  #poor environment colors
  if(nrow(plotdata)>plotpoorenv[2]){
    lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], plotdata$Ho[plotpoorenv[1]:plotpoorenv[2]]/plotdata$Ho[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
  }else{
    lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], plotdata$Ho[plotpoorenv[1]:nrow(plotdata)]/plotdata$Ho[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
  }
  
  #captive breeding colors
  if(nrow(plotdata)>plotcaptive[2]){
    lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$Ho[plotcaptive[1]:plotcaptive[2]]/plotdata$Ho[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
  }else{
    lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], plotdata$Ho[plotcaptive[1]:nrow(plotdata)]/plotdata$Ho[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
  }
}

###Number of alleles
yu = 1
yl = 0.4
plot(-100, -100, ylim = c(yl,yu), xlim = c(25, gens), xlab = "year", ylab = "alleles retained (%)", cex.lab=1, cex.lab=1)
#control
for(r in 1:length(onesc)){
  if(r==length(onesc)){
    plotdata = control[onesc[r]:nrow(control),]
  }else{
    plotdata = control[onesc[r]:(onesc[r+1]-1),]
  }
  lines(plotdata$year[25:nrow(plotdata)], plotdata$NumAllelesAverage[25:gens]/plotdata$NumAllelesAverage[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col="grey80", lwd=1.2)
}
#data
for(r in 1:length(ones)){
  if(r==length(ones)){
    plotdata = data[ones[r]:nrow(data),]
  }else{
    plotdata = data[ones[r]:(ones[r+1]-1),]
  }
  lines(plotdata$year[25:plotpoorenv[1]], plotdata$NumAllelesAverage[25:plotpoorenv[1]]/plotdata$NumAllelesAverage[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
  lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$NumAllelesAverage[plotcaptive[2]:nrow(plotdata)]/plotdata$NumAllelesAverage[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2)
  #poor environment colors
  if(nrow(plotdata)>plotpoorenv[2]){
    lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], plotdata$NumAllelesAverage[plotpoorenv[1]:plotpoorenv[2]]/plotdata$NumAllelesAverage[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
  }else{
    lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], plotdata$NumAllelesAverage[plotpoorenv[1]:nrow(plotdata)]/plotdata$NumAllelesAverage[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
  }
  
  #captive breeding colors
  if(nrow(plotdata)>plotcaptive[2]){
    lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$NumAllelesAverage[plotcaptive[1]:plotcaptive[2]]/plotdata$NumAllelesAverage[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
  }else{
    lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], plotdata$NumAllelesAverage[plotcaptive[1]:nrow(plotdata)]/plotdata$NumAllelesAverage[25], xlim=c(25,gens), ylim = c(yl,yu), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
  }
}
####effectve population size
allcols = c(23, (23+data$lifespan[1]))
alladds = c((23+data$maturity[2]),(23+data$lifespan[1]))

plot(-100, -100, ylim = c(0, yues[d]), xlim = c(25, gens), xlab = "year", ylab = "effective population size", cex.lab=1, cex.lab=1)
#control
for(r in 1:length(onesc)){
  if(r==length(onesc)){
    plotdata = control[onesc[r]:nrow(control),]
  }else{
    plotdata = control[onesc[r]:(onesc[r+1]-1),]
  }
  lines(plotdata$year[25:nrow(plotdata)], plotdata$Ne[25:nrow(plotdata)], xlim=c(25,gens), ylim=c(0,yues[d]), type="l", xlab="", ylab="", col="grey60", lwd=1.2) #1 now all
}
#data
for(r in 1:length(ones)){
  if(r==length(ones)){
    plotdata = data[ones[r]:nrow(data),]
  }else{
    plotdata = data[ones[r]:(ones[r+1]-1),]
  }
  lines(plotdata$year[25:plotpoorenv[1]], plotdata$Ne[25:plotpoorenv[1]], xlim=c(25,gens), ylim=c(0,yues[d]), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
  lines(plotdata$year[plotcaptive[2]:nrow(plotdata)], plotdata$Ne[plotcaptive[2]:nrow(plotdata)], xlim=c(25,gens), ylim=c(0,yues[d]), type="l", xlab="", ylab="", col=pcolors[d], lwd=1.2) #1
  
  #poor environment colors
  if(nrow(plotdata)>plotpoorenv[2]){
    lines(plotdata$year[plotpoorenv[1]:plotpoorenv[2]], plotdata$Ne[plotpoorenv[1]:plotpoorenv[2]], xlim=c(25,gens), ylim=c(0,yues[d]), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
  }else{
    lines(plotdata$year[plotpoorenv[1]:nrow(plotdata)], plotdata$Ne[plotpoorenv[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(0,yues[d]), type="l", xlab="", ylab="", col=poorenvcol[d], lwd=1.2)
  }
  
  #captive breeding colors
  if(nrow(plotdata)>plotcaptive[2]){
    lines(plotdata$year[plotcaptive[1]:plotcaptive[2]], plotdata$Ne[plotcaptive[1]:plotcaptive[2]], xlim=c(25,gens), ylim=c(0,yues[d]), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
  }else{
    lines(plotdata$year[plotcaptive[1]:nrow(plotdata)], plotdata$Ne[plotcaptive[1]:nrow(plotdata)], xlim=c(25,gens), ylim=c(0,yues[d]), type="l", xlab="", ylab="", col=captivecol[d], lwd=1.2)
  }
}
