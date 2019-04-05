setwd("")                                                         #set working directory
directory = getwd()
outdir    = paste(directory,"/output/",sep="")                    #directory to save model output  
source(paste(directory, "/source/FunctionSourcer.R", sep = ''))   #source functions and set source directory

repeats = 1                                                       #number of time to repeat simulations
plotit  <<- 1                                                     #toggle population size plotting 0=off, 1=on
species = "crane"                                                 #"crane" "tamarin" "toad" "salmon" "other" (parameters values listed below)

#species parameters
if(species=="crane")  { fecundity.V = 1;   maturity.V  = 5;  lifespan.V  = 23; repro1.V = 1; K.V = 500 }   #fecundity - modified by RRS for each pair
if(species=="tamarin"){ fecundity.V = 2;   maturity.V  = 2;  lifespan.V  = 8;  repro1.V = 1; K.V = 500 }   #maturity  - number of years to reach sexual maturity
if(species=="toad")   { fecundity.V = 30;  maturity.V  = 4;  lifespan.V  = 10; repro1.V = 1; K.V = 500 }   #lifespan  - maximum
if(species=="salmon") { fecundity.V = 500; maturity.V  = 4;  lifespan.V  = 7;  repro1.V = 0; K.V = 500 }   #repro1    - can individuals reproduce multiple times in a lifetime? 1 = yes, 0 = no
if(species=="other")  { fecundity.V = 100; maturity.V  = 2;  lifespan.V  = 5;  repro1.V = 0; K.V = 500 }   #K         - values for setting total pop to 250

#variables
N.V          = K.V                                               #intial population size                                             
gens.V       = 250                                               #number years to simulate plus lifespan                             
RRS.V        = c(seq(0,0.275,0.025), seq(0.3,1,0.1))             #relative reproductive success penelty for captive born individuals 
RRSvar.V     = 0.005                                             #variance in repro success, initiated when RRS is assigned         
r0.V         = 0.1                                               #per capita growth rate                                            
l.V          = 5                                                 #adult density independent mortality (adds variance)               
nloci.V      = 50                                                #number of loci                                                   
countimmat.V = 1                                                 #count immature individuals in Nt? 0 = no, 1 = yes                 
killimmat.V  = 1                                                 #kill immature in adult mortality step? 0 = no, 1 = yes            
adultmort.V  = (1/(lifespan.V-2))                                #proportion of adults that die after reproduction each year

#variables for captive breeding
startcap.V  = c(75,1000)                                         #year to start introduction of captive individuals (1000 = no captive breeding)                
endcap.V    = 125                                                #year to end introduction of captive individuals
capfound.V  = 0.15                                               #proportion of captive founders needed every generation
newfound.V  = 1                                                  #1=all new founders every generation, 0=some new some old
propnew.V   = 1                                                  #frequency to supplement/add individuals to popualtion 
caplife.V   = 1*lifespan.V                                       #lifespan of captive individuals, relative to lifespan.V 
coffprop.V  = 1                                                  #proportion of offspring from cap relative to wild               

#control reproduction in captivity
equalcaptive.V  = 0                                              #reproduction in captivity is divided equally (0=n, 1=y)
allonecaptive.V = 0                                              #reproduction in captivity is all from one pair (0=n, 1=y)
capvariation.V  = 1                                              #variation set up similar to wild, currently modified in code

#environmental variables
poorenv.V   = 75                                                 #generation to start removing individuals 
fixedenv.V  = 125                                                #generation to end removing individuals 
lossperct.V = 0.5                                                #proportion of individuals to remove     

#other
nimmigrants.V = 1                                                #number of effective migrants per generation  
propkill.V    = 0.5                                              #proportion to assume die before Nt count
selection     = 0                                                #switch for adding strong selection (0 off, value = strength) 

runvars = Replicates(repeats, K.V, N.V, gens.V, RRS.V, RRSvar.V, r0.V, l.V, nloci.V, countimmat.V, killimmat.V, adultmort.V, fecundity.V, maturity.V, lifespan.V, repro1.V,
                     startcap.V, endcap.V, capfound.V, newfound.V, propnew.V, caplife.V, coffprop.V,poorenv.V, fixedenv.V, lossperct.V, nimmigrants.V, 
                     propkill.V, equalcaptive.V, allonecaptive.V, capvariation.V, directory, outdir, plotit, species, selection)

remove(repeats, K.V, N.V, gens.V, RRS.V, RRSvar.V, r0.V, l.V, nloci.V, countimmat.V, killimmat.V, adultmort.V, fecundity.V, maturity.V, lifespan.V, repro1.V,
       startcap.V, endcap.V, capfound.V, newfound.V, propnew.V, caplife.V, coffprop.V,poorenv.V, fixedenv.V, lossperct.V, nimmigrants.V,
       propkill.V, equalcaptive.V, allonecaptive.V, capvariation.V,directory, outdir, plotit, species, selection)

##run model iterating over parameters in Replicates
for(r in 1:nrow(runvars)){ 
  RunModel(runvars, r)
}
