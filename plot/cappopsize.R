library(Hmisc)
setwd("/Volumes/jwillou/ibm_captivity/OUTPUT/") #jwillou/ or scratch/

####run parameters####
startcap = 75
endcap   = 125
controly = 1000
RRS      = seq(0,1,0.1)
gens     = 250
poorenv  = 1000
fixedenv = 1000
nloci    = 50
capfounders = 1

####data####
#all pop = 500
csum  = read.table("crane/output05.12.16_08.40/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
ssum  = read.table("salmon/output10.12.16_17.32/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
tasum = read.table("tamarin/output05.12.16_08.43/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tosum = read.table("toad/output05.12.16_08.44/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad
#adults = 500
csum  = read.table("crane/output10.11.16_12.53/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
ssum  = read.table("salmon/output10.12.16_12.54/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
tasum = read.table("tamarin/output10.11.16_16.35/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tosum = read.table("toad/output10.11.16_16.36/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad

colnames(ssum) = colnames(tosum) = colnames(csum) = colnames(tasum) = #
  c("year", "propHatchery", "He", "Ho", "polyLoci", "NumAlleles","meanRRSall", "meanRRSwild","meanRRScap",seq(0, 30, 1), 
    "K", "N", "gens", "RRS", "RRSvar", "r0", "l", "nloci", "countimmat", "killimmat", "adultmort", "fecundity", "maturity", "lifespan", "repro1",
    "startcap", "endcap", "capfound", "newfound", "propnew", "caplife", "coffprop","poorenv", "fixedenv", "lossperct", 
    "nimmigrants", "calcfitness", "propkill", "equalcaptive", "allonecaptive", "directory", "outdir", "plotit", "species")

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


years      = seq(75, 125, 1)
mats       = c(5, 2, 4, 4) #c(1, 1, 1, 1) #
par(mfrow=c(1,1))
yl = 0 
yu = 150
plot(-100, -100, xlim=c(75, 125), ylim = c(yl, yu), xlab = "year", ylab = "captive population size", cex=2)
for(d in 1:length(datasets)){
  #dataset
  data = datasets[[d]]
  data$popsize = apply(data[,11:40], 1, sum, na.rm=TRUE)
  
  #finish with dataset
  data = data[data$startcap==startcap,]
  data[data=="NaN"] = NA
  data = data[data$RRS==0.1,]
  ones = subset(1:length(data$year), data$year==1)
  
  nadults = addsl = addsu = NULL
  for(y in 1:length(years)){
    adds = apply(data[data$year %in% (ones+years[y]),(11+mats[d]-1):40], 1, sum)
    addsl = c(addsl, quantile(adds, probs = 0.025))
    addsu = c(addsu, quantile(adds, probs = 0.975))
    nadults = c(nadults, mean(adds))
  }
  move = 0.1 * (d - 1)
  pdata = data.frame(x=years,
                     M=  nadults * .15,
                     SEl=addsl * .15,
                     SEu=addsu * .15)
  colnames(pdata)=c("x","M","SEl", "SEu")
  par(new=TRUE)
  errbar(x=(pdata$x + (move)), y=pdata$M, ylim = c(yl, yu), xlim = c(75, 125), lty=1,
         yplus = pdata$SEu, 
         yminus= pdata$SEl,
         xlab="", ylab="", col.lab="white", cap=.0, lwd=2.5,
         type="l", col="white", errbar.col = pcolors[d], axes=FALSE)
  #points(x=(pdata$x + (move)), y=pdata$M,ylim = c(yl, yu), xlim = c(75, 125), col=pcolors[d], bg=pcolors[d], pch=21, type="p")
  lines(x=(pdata$x + (move)), y=pdata$M, ylim = c(yl, yu), xlim = c(75, 125), col=pcolors[d], lwd=3, lty=1)  
}
