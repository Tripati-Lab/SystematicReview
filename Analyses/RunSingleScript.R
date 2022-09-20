source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/Calibration_BayesianNonBayesian.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/Predictions_Bayesian.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/Functions/Predictions_nonBayesian.R")
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/dev/global.R")

library(here)
library(rstan)

replicates = 100
samples = NULL
ngenerationsBayes = 3000
multicore = FALSE
priors = "Weak"
name = "S1"
init.values = FALSE

RunSingleFullResults <- function(name="S3",
                                 replicates, 
                                 samples, 
                                 ngenerationsBayes, 
                                 priors){

calData <- read.csv(here::here("Analyses","Datasets", paste0("Dataset_",name, ".csv")))
recData <- read.csv(here::here("Analyses","Datasets", "BayClump_reconstruction_template.csv")) 

calData$D47error <- abs(calData$D47error)
calData$TempError <- abs(calData$TempError)

lmcals <- simulateLM_measured(calData, replicates = replicates, samples = samples)
lminversecals <- simulateLM_inverseweights(calData, replicates = replicates, samples = samples)
yorkcals <- simulateYork_measured(calData, replicates = replicates, samples = samples)
demingcals <- simulateDeming(calData, replicates = replicates, samples = samples)
bayeslincals <- fitClumpedRegressions(calibrationData = calData, 
                                      priors = priors,
                                      numSavedSteps = ngenerationsBayes,
                                      samples = samples)

nonBayesianParamsComplete <- rbindlist(list("OLS" = lmcals,
                                            "WOLS" = lminversecals,
                                            "York" = yorkcals,
                                            "Deming" = demingcals),
                                       idcol = "Model")       
     
rbindlist(list("BLM1_fit" = summary(bayeslincals$BLM1_fit)$summary,
"BLM1_fit_NoErrors" = summary(bayeslincals$BLM1_fit_NoErrors)$summary,
"BLM3_fit" = summary(bayeslincals$BLM3_fit)$summary), idcol = )

Params <- rbind.data.frame(nonBayesianParamsComplete)
ParamEstimates <- aggregate(. ~ Model, Params, function(x) c(mean = mean(x), sd = sd(x)) )
ParamEstimates <- as.data.frame(as.matrix(ParamEstimates))


sumBayesian <- rbind.data.frame(
cbind.data.frame(
Model = "BLM1_fit",
alpha.mean = summary(bayeslincals$BLM1_fit)$summary[1,1],
alpha.sd = summary(bayeslincals$BLM1_fit)$summary[1,3],
beta.mean = summary(bayeslincals$BLM1_fit)$summary[2,1],
beta.sd = summary(bayeslincals$BLM1_fit)$summary[2,3]
),

cbind.data.frame(
  Model = "BLM1_fit_NoErrors",
  alpha.mean = summary(bayeslincals$BLM1_fit_NoErrors)$summary[1,1],
  alpha.sd = summary(bayeslincals$BLM1_fit_NoErrors)$summary[1,3],
  beta.mean = summary(bayeslincals$BLM1_fit_NoErrors)$summary[2,1],
  beta.sd = summary(bayeslincals$BLM1_fit_NoErrors)$summary[2,3]
),

cbind.data.frame(
  Model = "BLM3_fit",
  alpha.mean = summary(bayeslincals$BLM3_fit)$summary[1,1],
  alpha.sd = summary(bayeslincals$BLM3_fit)$summary[1,3],
  beta.mean = summary(bayeslincals$BLM3_fit)$summary[2,1],
  beta.sd = summary(bayeslincals$BLM3_fit)$summary[2,3]
)
)

ParamEstimates <- rbind.data.frame(ParamEstimates, sumBayesian)


##Reconstructions
lmrecClassic <-  predictTc(calData = calData,
                           recData = recData,
                           obCal = lmcals,
                           clumpedClassic = TRUE)
lmrecSimple <-  predictTc(calData = calData,
                           recData = recData,
                           obCal = lmcals,
                           clumpedClassic = FALSE)

lminverserecClassic <-  predictTc(calData = calData,
                                  recData = recData,
                                  obCal = lminversecals)


lminverserecSimple <-  predictTc(calData = calData,
                                  recData = recData,
                                  obCal = lminversecals,
                                 clumpedClassic = FALSE)

yorkrecClassic <-  predictTc(calData = calData,
                             recData = recData,
                             obCal = yorkcals)

yorkrecSimple <-  predictTc(calData = calData,
                                 recData = recData,
                                 obCal = yorkcals,
                                 clumpedClassic = FALSE)

demingrecClassic <-  predictTc(calData = calData,
                               recData = recData,
                               obCal = demingcals ,
                               clumpedClassic = TRUE)

demingrecSimple <-  predictTc(calData = calData,
                                   recData = recData,
                                   obCal = demingcals ,
                                   clumpedClassic = FALSE)

infTempBayesianBLM1 <- BayesianPredictions(calModel = bayeslincals$BLM1_fit,
                    calData = calData,
                    recData = recData)

infTempBayesianBLM1_fit_NoErrors <- BayesianPredictions(calModel = bayeslincals$BLM1_fit_NoErrors,
                                       calData = calData,
                                       recData = recData)


RecComplete <- rbindlist(list("OLS"=lmrecClassic,
                              "York"=yorkrecClassic,
                              "Deming"=demingrecClassic,
                              "WOLS"=lminverserecClassic,
                              "BayesianBLM1" = infTempBayesianBLM1,
                              "BayesianBLM1_NoErrors" =infTempBayesianBLM1
                              ),
                         idcol = "Model", fill = TRUE)


colnames(BayesianRecs)[3] <- "D47"

colnames(RecComplete)[c(5,3)] <- c("sd","D47PredErr")

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

###Weak

a1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Weak')

b1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Weak')


c1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=50000, 
                     priors='Weak')


###Uninformative

RunSingleFullResults(name="S1",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=50000, 
                          priors='Uninformative')

RunSingleFullResults(name="S2",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=50000, 
                          priors='Uninformative')


RunSingleFullResults(name="S3",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=50000, 
                          priors='Uninformative')



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


###Weak
d1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=50000, 
                     priors='Weak')

e1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=50000, 
                     priors='Weak')


f1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=50000, 
                     priors='Weak')


###Uninformative

RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='Uninformative')

RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='Uninformative')


RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=50000, 
                     priors='Uninformative')



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


###Weak
g1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=50000, 
                     priors='Weak')

h1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=50000, 
                     priors='Weak')


i1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=50000, 
                     priors='Weak')

###Uninformative

RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='Uninformative')

RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='Uninformative')


RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=50000, 
                     priors='Uninformative')



