library(Hmisc)
library(gplots)
library(RColorBrewer)
setwd("/Volumes/snyder/ibm_captivity/OUTPUT/") #jwillou/ or scratch/

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
csum0  = read.table("crane/output09.26.16_10.03/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
csum0  = cbind(csum0, rep(1, nrow(csum0)))
ssum0  = read.table("salmon/output09.26.16_10.04/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
ssum0  = cbind(ssum0, rep(1, nrow(ssum0)))
tasum0 = read.table("tamarin/output09.26.16_10.05/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tasum0 = cbind(tasum0, rep(1, nrow(tasum0)))
tosum0 = read.table("toad/output09.26.16_10.05/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad
tosum0 = cbind(tosum0, rep(1, nrow(tosum0)))

csum1  = read.table("crane/output09.26.16_10.06/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
csum1  = cbind(csum1, rep(2, nrow(csum1)))
ssum1  = read.table("salmon/output09.26.16_10.07/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
ssum1  = cbind(ssum1, rep(2, nrow(ssum1)))
tasum1 = read.table("tamarin/output09.26.16_10.07/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tasum1 = cbind(tasum1, rep(2, nrow(tasum1)))
tosum1 = read.table("toad/output09.26.16_10.08/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad
tosum1 = cbind(tosum1, rep(2, nrow(tosum1)))

csum2  = read.table("crane/output09.26.16_10.08/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
csum2  = cbind(csum2, rep(5, nrow(csum2)))
ssum2  = read.table("salmon/output09.26.16_10.09/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
ssum2  = cbind(ssum2, rep(5, nrow(ssum2)))
tasum2 = read.table("tamarin/output09.26.16_10.09/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tasum2 = cbind(tasum2, rep(5, nrow(tasum2)))
tosum2 = read.table("toad/output09.26.16_10.10/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad
tosum2 = cbind(tosum2, rep(5, nrow(tosum2)))

csum3  = read.table("crane/output09.26.16_10.10/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
csum3  = cbind(csum3, rep(10, nrow(csum3)))
ssum3  = read.table("salmon/output09.26.16_10.11/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
ssum3  = cbind(ssum3, rep(10, nrow(ssum3)))
tasum3 = read.table("tamarin/output09.26.16_10.14/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tasum3 = cbind(tasum3, rep(10, nrow(tasum3)))
tosum3 = read.table("toad/output09.26.16_10.18/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad
tosum3 = cbind(tosum3, rep(10, nrow(tosum3)))

colnames(ssum0) = colnames(tosum0) = colnames(csum0) = colnames(tasum0) = 
  colnames(ssum1) = colnames(tosum1) = colnames(csum1) = colnames(tasum1) = 
  colnames(ssum2) = colnames(tosum2) = colnames(csum2) = colnames(tasum2) =
  colnames(ssum3) = colnames(tosum3) = colnames(csum3) = colnames(tasum3) =
  c("year", "propHatchery", "He", "Ho", "polyLoci", "Nalleles","meanRRSall", "meanRRSwild","meanRRScap",seq(0, 30, 1), 
    "K", "N", "gens", "RRS", "RRSvar", "r0", "l", "nloci", "countimmat", "killimmat", "adultmort", "fecundity", "maturity", "lifespan", "repro1",
    "startcap", "endcap", "capfound", "newfound", "propnew", "caplife", "coffprop","poorenv", "fixedenv", "lossperct", 
    "nimmigrants", "propkill", "equalcaptive", "allonecaptive", "capvariation", "directory", "outdir", "plotit", "species", "extra","column","migfreq")

csum  = rbind(csum1, csum2, csum3, csum0)
ssum  = rbind(ssum1, ssum2, ssum3, ssum0)
tasum = rbind(tasum1, tasum2, tasum3, tasum0)
tosum = rbind(tosum1, tosum2, tosum3, tosum0)
remove(csum0, csum1, csum2, csum3, ssum0, ssum1, ssum2, ssum3, tasum0, tasum1, tasum2, tasum3, tosum0, tosum1, tosum2, tosum3)

####set up for data on to be analyzed####
setwd("~/Desktop/")
datasets = list(csum=csum, tasum=tasum, tosum=tosum, ssum=ssum) #  
labs     = c("crane", "tamarin", "toad", "salmon") # 
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

###Migraion: number and frequency####
par(mfrow=c(1,1))
column.number = c(79,78)
years = 225
allsummaries = NULL
nimmigrants  = c(1,2,5,10)
migfreq      = c(1,2,5,10)

#set c and run
c=2
allsummaries = NULL
for(de in 1:length(migfreq)){
  year1 = data.frame(nimmigrants=nimmigrants)
  plotpoorenv = c(50,74)
  plotcaptive = c(startcap-1, endcap)
  for(d in 1:length(datasets)){
    #control data
    control = datasets[[d]]
    control = control[control$startcap==controly,]
    control = control[control$migfreq==migfreq[de],]
    control$NumAllelesAverage = control$Nalleles
    control$popsize = apply(control[,11:40], 1, sum)
    onesc = subset(1:length(control$year), control$year==1)
    
    #determine ylim for population size for each species
    maxy = 1000
    yues = 200
    
    #dataset
    data = datasets[[d]]
    data = data[data$startcap==startcap,]
    data = data[data$migfreq==migfreq[de],]
    data = data[data$RRS==0.1,]
    data$NumAllelesAverage = data$Nalleles
    data$popsize = apply(data[,11:40], 1, sum)
    ones = subset(1:length(data$year), data$year==1)
    
    #separate by years interested in
    year1D = data[data$year==years[1],]
    year1Dc = control[control$year==years[1],]
    
    #set up objects
    s1 = s1dl = s1du = s1c = NULL
    
    for(r in 1:length(nimmigrants)){
      rrs = nimmigrants[r]
      
      #calculate mean at each year in control for popsize and number of alleles (popsize NumAlleles)
      s1c  = year1Dc[year1Dc$nimmigrants==as.numeric(rrs), column.number[c]]
      s1c  = s1c[!is.na(s1c)]  
      
      values = year1D[year1D$nimmigrants==as.numeric(rrs), column.number[c]]
      values = values[!is.na(values)]
      
      if(length(values)>100){values = values[1:100] }
      divideall = NULL
      if(length(values)>10){
        divideall = NULL
        for(cv in 1:length(s1c)){
          for(rv in 1:length(values)){
            divideall = c(divideall, values[rv]/s1c[cv])
          }
        }
      }
      if(!is.null(divideall)){
        if(mean(divideall, na.rm=TRUE)<=0){
          s1  = c(s1, 1)
          s1dl  = c(s1dl, 1)
          s1du  = c(s1du, 1)
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
    }
    #save data
    year1[paste("AW0", labs[d],"M",   sep="")] = c(s1, rep(0, length(nimmigrants)-length(s1)))
    year1[paste("AW0", labs[d],"SEl", sep="")] = c(s1dl, rep(0, length(nimmigrants)-length(s1dl)))
    year1[paste("AW0", labs[d],"SEu", sep="")] = c(s1du, rep(0, length(nimmigrants)-length(s1du)))
  }    
  holdyear1 = cbind(year1, rep(migfreq[de], nrow(year1)))
  colnames(holdyear1) = c(colnames(year1), "decline")
  allsummaries = rbind(allsummaries, holdyear1)
}
allsummaries[is.na(allsummaries)] = 0

#demosummaries = allsummaries
#gensummaries = allsummaries

#plot setup
#allsummaries = demosummaries
#pdf("~/Desktop/demo_heatmap.pdf", width = 9, height = 9) 
allsummaries = gensummaries
#pdf("~/Desktop/gen_heatmap.pdf", width = 9, height = 9)   
#par(mfrow=c(2,2))

#colors
my_palette = colorRampPalette(c("yellow", "red"))(40)
col_breaks = seq(0,1,0.025)
times = c("year1")
types = c("AW0")
for(s in 1:length(species)){
  hdata = data.frame(y=  allsummaries[ncol(allsummaries)],
                     M=  1 - allsummaries[paste(types[1],species[s],"M",   sep="")])
  colnames(hdata)=c("y","M")
  pdata = cbind(hdata$M[ 1:4], 
                hdata$M[ 5:8],
                hdata$M[ 9:12],
                hdata$M[13:16])
  
  colnames(pdata) = c("1", "2", "5", "10")
  rownames(pdata) =c("1", "2", "5", "10")
  pdata = pdata[order(as.numeric(rownames(pdata)),decreasing=FALSE), ]
  pdata = as.matrix(pdata)
  pdata = t(pdata)
  
  heatmap.2(pdata,
            #cellnote = pdata,             # same data set for cell labels
            #notecol="black",              # change font color of cell labels to black
            main = "",                    # heat map title
            margins =c(12,8),             # widens margins around plot
            col=my_palette,               # use on color palette defined earlier
            breaks=col_breaks,            # enable color transition at specified limits
            dendrogram='none',            # turn of dendrogram
            Rowv=FALSE,
            Colv=FALSE,
            trace='none',
            density.info='none'
  )
  
}
#dev.off()

heatmap.2(
  matrix(rnorm(100*10), nrow=100)
  , dendrogram='none'
  , Colv = F
  , Rowv = F
  , trace='none'
  , col = my_palette
  , breaks=col_breaks
  , labRow=NA
  , labCol=NA
  , density.info='none'
  #, lmat=rbind(c(4, 2), c(1, 3)), lhei=c(2, 8), lwid=c(4, 1)
)
