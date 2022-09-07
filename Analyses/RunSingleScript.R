
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/Calibration_BayesianNonBayesian.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/Predictions_Bayesian.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/Predictions_nonBayesian.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/global.R")

library(here)

replicates = 100
samples = NULL
ngenerationsBayes = 10000
multicore = FALSE
priors = "Difusse"
name = "S3"
init.values = FALSE

RunSingleFullResults <- function(name="S3",
                                 replicates, 
                                 samples, 
                                 ngenerationsBayes, 
                                 priors){

calData <- read.csv(here::here("Analyses","Datasets", paste0("Dataset_",name, ".csv")))
recData <- read.csv(here::here("Analyses","Datasets", "BayClump_reconstruction_template.csv")) 


multicore = FALSE

calData$T2 <- calData$Temperature
lmcals <- simulateLM_measured(calData, replicates = replicates, samples = samples)
lminversecals <- simulateLM_inverseweights(calData, replicates = replicates, samples = samples)
yorkcals <- simulateYork_measured(calData, replicates = replicates, samples = samples)
demingcals <- simulateDeming(calData, replicates = replicates, samples = samples, multicore=multicore)
bayeslincals <- fitClumpedRegressions(calibrationData = calData, 
                                                 priors = priors,
                                                 n.iter = ngenerationsBayes,
                                                 samples = samples)

nonBayesianParamsComplete <- rbindlist(list("OLS"=lmcals,
     "WOLS"=lminversecals,
     "York"=yorkcals,
     "Deming"=demingcals), idcol = "Model")       
       
BayesianPosteriors <- rbindlist(list("BLM1_fit"=do.call(rbind.data.frame,as.mcmc(bayeslincals$BLM1_fit)),
                                     "BLM1_fit_NoErrors"=do.call(rbind.data.frame,as.mcmc(bayeslincals$BLM1_fit_NoErrors)),
                                     "BLM3_fit"=do.call(rbind.data.frame,as.mcmc(bayeslincals$BLM3_fit))), idcol = "Model", fill = T)  

Params <- rbind.data.frame(nonBayesianParamsComplete, BayesianPosteriors[,1:3])

ParamEstimates <- aggregate(. ~ Model, Params, function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))

#calData$Tc <- sqrt(10^6/(calData$T2))-273.15
#calData$TcE <- abs((sqrt(10^6/(calData$T2))-273.15) - (sqrt(10^6/(calData$T2+abs(calData$TempError)))-273.15))
              

lmrecClassic <-  do.call(rbind,lapply(1:nrow(recData), function(x){
                a <- predictTc(calData, targety=recData$D47[x],obCal=lmcals)
                b <- predictTc(calData, targety=recData$D47[x]+recData$D47error[x], obCal=lmcals)
                cbind.data.frame("D47"=recData$D47[x],"D47se"=recData$D47error[x], "Tc"=a$temp, "se"=a$temp-b$temp)
              } ))
              
            

lminverserecClassic  <-  do.call(rbind,lapply(1:nrow(recData), function(x){
                a <- predictTc(calData, targety=recData$D47[x], obCal=lminversecals)
                b <- predictTc(calData, targety=recData$D47[x]+recData$D47error[x], obCal=lminversecals)
                cbind.data.frame("D47"=recData$D47[x],"D47se"=recData$D47error[x], "Tc"=a$temp, "se"=a$temp-b$temp)
              } ))
            

yorkrecClassic  <-   do.call(rbind,lapply(1:nrow(recData), function(x){
                a <- predictTc(calData, targety=recData$D47[x], obCal=yorkcals)
                b <- predictTc(calData, targety=recData$D47[x]+recData$D47error[x], obCal=yorkcals)
                cbind.data.frame("D47"=recData$D47[x],"D47se"=recData$D47error[x], "Tc"=a$temp, "se"=a$temp-b$temp)
              } ))
              


demingrecClassic <- do.call(rbind,lapply(1:nrow(recData), function(x){
                a <- predictTc(calData, targety=recData$D47[x], obCal=demingcals)
                b <- predictTc(calData, targety=recData$D47[x]+recData$D47error[x], obCal=demingcals)
                cbind.data.frame("D47"=recData$D47[x],"D47se"=recData$D47error[x], "Tc"=a$temp, "se"=a$temp-b$temp)
              }))
            

infTempBayesian <- BayesianPredictions(calibrationData = calData, 
                                       n.iter = ngenerationsBayes, 
                                       priors = priors,
                                       samples=NULL,
                                       init.values = FALSE, 
                                       D47Pred = recData$D47,
                                       materialsPred = as.numeric(as.factor(ifelse(is.na(recData$Material), 1,recData$Material)))
)
            
BayesianRecs <- rbindlist(lapply(infTempBayesian, as.data.frame), idcol = 'Model', fill = TRUE)

##Reconstructions




RecComplete <- rbindlist(list("OLS"=lmrecClassic,
                              "York"=yorkrecClassic,
                              "Deming"=demingrecClassic,
                              "WOLS"=lminverserecClassic
                              ),
                         idcol = "Model", fill = TRUE)


colnames(BayesianRecs)[2] <- "D47"
colnames(RecComplete)[5] <- "sd"

RecComplete <- rbindlist(list(BayesianRecs, RecComplete), fill = TRUE)


toRet <- list("ParameterEstimates"=ParamEstimates,
     "Reconstructions"=RecComplete,
     "RawParams"=Params,
     BayesianRecs,
     bayeslincals
     )

write.csv(ParamEstimates, here::here("Analyses","Results",paste0(name,"Replicates=", replicates,"Samples=",samples,priors ,"_ParameterEstimates.csv")))
write.csv(RecComplete, here::here("Analyses","Results", paste0(name,"Replicates=", replicates,"Samples=",samples,priors ,"_Recs.csv" )))
write.csv(Params, here::here("Analyses","Results",paste0(name,"Replicates=", replicates,"Samples=",samples,priors ,"_ParamsFull.csv" )))

return(toRet)
}

##50
###Informative
a <- RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Informative')

b <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Informative')

c <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Informative')

###Difuse

a1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')

b1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')


c1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')


###NonInformative

RunSingleFullResults(name="S1",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=50000, 
                          priors='NonInformative')

RunSingleFullResults(name="S2",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=50000, 
                          priors='NonInformative')


RunSingleFullResults(name="S3",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=50000, 
                          priors='NonInformative')



##500
###Informative
d <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='Informative')

e <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='Informative')


f <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='Informative')


###Difuse
d1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')

e1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')


f1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')


###NonInformative

RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='NonInformative')

RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='NonInformative')


RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='NonInformative')



##10
###Informative
g <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='Informative')

h <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='Informative')


i <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='Informative')


###Difuse
g1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')

h1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')


i1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=50000, 
                     priors='Difuse')

###NonInformative

RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='NonInformative')

RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='NonInformative')


RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='NonInformative')



