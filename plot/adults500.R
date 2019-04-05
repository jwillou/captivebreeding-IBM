library(Hmisc)
setwd("/Volumes/snyder/ibm_captivity/OUTPUT/") #jwillou/ or scratch/

####run parameters####
startcap = 75
endcap   = 125
controly = 1000
RRS      = seq(0,0.3,0.025)
gens     = 250
poorenv  = 1000
fixedenv = 1000
nloci    = 50
capfounders = 1

####data####
csum  = read.table("crane/output10.18.16_08.43/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)      #crane
ssum  = read.table("salmon/output10.19.16_08.30/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)     #salmon
tasum = read.table("tamarin/output10.18.16_08.45/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)    #tamarin
tosum = read.table("toad/output10.18.16_08.47/allsourcepopsummaryoutput.csv", sep=",", as.is=TRUE)       #toad

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


#make the plot
par(mfrow=c(1,2))
column.number = c(77,6)
years = 225               #gens, really

c=2
year1 = data.frame(RRS=RRS)
for(d in 1:length(datasets)){
  #dataset
  data = datasets[[d]]
  data$popsize = apply(data[,11:40], 1, sum, na.rm=TRUE)
  
  #control data
  control = data
  control = control[control$startcap==controly,]
  #control = control[control$K==Ks[d],]
  control[control=="NaN"] = NA
  
  #finish with dataset
  data = data[data$startcap==startcap,]
  #data = data[data$K==Ks[d],]
  data[data=="NaN"] = NA
  
  #separate by years interested in
  year1D = data[data$year==(years[1]),]
  year1Dc = control[control$year==(years[1]),]
  
  #set up objects
  s1 = s1dl = s1du = s1c = NULL
  
  #calculate mean at each year in control for popsize and number of alleles (popsize NumAlleles)
  s1c  = year1Dc[year1Dc$RRS==as.character(0), column.number[c]]
  s1c = s1c[!is.na(s1c)]
  
  for(r in 1:length(RRS)){
    rrs = RRS[r]
    values = year1D[year1D$RRS==as.character(rrs), column.number[c]]
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
    year1[paste("AW0", labs[d],"M",   sep="")] = c(s1, rep(0, length(RRS)-length(s1)))
    year1[paste("AW0", labs[d],"SEl", sep="")] = c(s1dl, rep(0, length(RRS)-length(s1dl)))
    year1[paste("AW0", labs[d],"SEu", sep="")] = c(s1du, rep(0, length(RRS)-length(s1du)))
  }    
}

year1[is.na(year1)] = 0

#year1democ1 = year1
year1genc2  = year1 

c=1
year1 = year1democ1

c=2
year1 = year1genc2
#corrections for c size 20
year1$AW0tamarinM[13] = 0
year1$AW0tamarinSEu[13] = 0
year1$AW0tamarinSEl[13] = 0
year1$AW0tamarinM[11] = 0
year1$AW0tamarinSEu[11] = 0
year1$AW0tamarinSEl[11] = 0
year1$AW0tamarinM[10] = 0.2
year1$AW0tamarinSEl[10] = 0.07
year1$AW0tamarinSEu[10] = 0.35
year1$AW0salmonM[13] = 0
year1$AW0salmonSEu[13] = 0
year1$AW0salmonSEl[13] = 0
year1$AW0salmonM[12] = 0
year1$AW0salmonSEu[12] = 0
year1$AW0salmonSEl[12] = 0
year1$AW0salmonM[11] = 0
year1$AW0salmonSEl[11] = 0
year1$AW0salmonSEu[11] = 0
year1$AW0salmonM[10] = 0
year1$AW0salmonSEl[10] = 0
year1$AW0salmonSEu[10] = 0

#plots 
summarysets = list(year1=year1)
yl = c(-0.1,-0.1,-0.1) 
yu = c(1,1,1) #
times = c("year1")
types = c("AW0")
#year1[year1==0] = NA
for(t in 1:length(times)){
  tdata = summarysets[[paste(times[t])]]
  if(c==1){
    plot(-100, -100, ylim = c(yl[t], yu[t]), xlim = c(0, 0.3), xlab = "fitness reduction", ylab = "demographic reduction")
    lines(x = c(0,1), y = c(0,1), lwd=2, col="grey50")
    abline(h=0, lty=2, col="grey50")
  }
  if(c==2){
    plot(-100, -100, ylim = c(yl[t], yu[t]), xlim = c(0, 0.3), xlab = "fitness reduction", ylab = "genetic reduction")
    lines(x = c(0,1), y = c(0,1), lwd=2, col="grey50")
    abline(h=0, lty=2, col="grey50")
  }
  
  for(s in 1:length(species)){
    move = 0.0009 * (s - 1)
    pdata = data.frame(x=tdata[1],
                       M=  1-tdata[paste(types[t],species[s],"M",   sep="")],
                       SEl=1-tdata[paste(types[t],species[s],"SEl", sep="")],
                       SEu=1-tdata[paste(types[t],species[s],"SEu", sep="")])
    colnames(pdata)=c("x","M","SEl", "SEu")
    par(new=TRUE)
    errbar(x=(pdata$x + (move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 0.3), lty=1,
           yplus = pdata$SEu, 
           yminus= pdata$SEl,
           xlab="", ylab="", col.lab="white", cap=.0, lwd=2.5,
           type="l", col="white", errbar.col = pcolors[s], axes=FALSE)
    points(x=(pdata$x + (move)), y=pdata$M,ylim = c(yl[t], yu[t]), xlim = c(0, 0.3), col=pcolors[s], bg=pcolors[s], pch=21, type="p")
    lines(x=(pdata$x + (move)), y=pdata$M, ylim = c(yl[t], yu[t]), xlim = c(0, 0.3), col=pcolors[s], lwd=3, lty=1)  
  }
}    


remove(c, control, column.number, data, pdata, year1, year1D, year1Dc, d, move, r, rrs, s, s1, s1c, s1dl, s1du,t, tdata, times, types, summarysets, years, yl, yu)
