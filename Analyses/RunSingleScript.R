library(here)
library(bayclumpr)
library(data.table)
library(rstan)

RunSingleFullResults <- function(name="S3",
                                 replicates, 
                                 samples, 
                                 ngenerationsBayes, 
                                 priors){

calData <- read.csv(here::here("Analyses","Datasets", paste0("Dataset_",name,"_",samples, ".csv")))
recData <- read.csv(here::here("Analyses","Datasets", "BayClump_reconstruction_template.csv")) 

calData$D47error <- abs(calData$D47error)
calData$TempError <- abs(calData$TempError)

lmcals <- cal.ols(calData, replicates = replicates)
lminversecals <- cal.wols(calData, replicates = replicates)
yorkcals <- cal.york(calData, replicates = replicates)
demingcals <- cal.deming(calData, replicates = replicates)
bayeslincals <- cal.bayesian(calibrationData = calData, 
                                      priors = priors,
                                      numSavedSteps = ngenerationsBayes)

nonBayesianParamsComplete <- rbindlist(list("OLS" = lmcals,
                                            "WOLS" = lminversecals,
                                            "York" = yorkcals,
                                            "Deming" = demingcals),
                                       idcol = "Model")       
   
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
ParamEstimates$alpha.mean <- as.numeric(ParamEstimates$alpha.mean)
ParamEstimates$alpha.sd <- as.numeric(ParamEstimates$alpha.sd)
ParamEstimates$beta.mean <- as.numeric(ParamEstimates$beta.mean)
ParamEstimates$beta.sd <- as.numeric(ParamEstimates$beta.sd)

##Reconstructions
lmrecClassic <-  rec.clumped(recData = recData,
                           obCal = lmcals)

lminverserecClassic <-  rec.clumped(recData = recData,
                                  obCal = lminversecals)

yorkrecClassic <-  rec.clumped(recData = recData,
                             obCal = yorkcals)


demingrecClassic <-  rec.clumped(recData = recData,
                               obCal = demingcals)

infTempBayesianBLM1 <- rec.bayesian(calModel = bayeslincals$BLM1_fit,
                                           recData = recData,
                                           priors = if(priors == 'Weak'){"Uninformative"}else{priors},
                                    postcalsamples = 500)

infTempBayesianBLM1_fit_NoErrors <- rec.bayesian(calModel = bayeslincals$BLM1_fit_NoErrors,
                                       recData = recData,
                                       priors = if(priors == 'Weak'){"Uninformative"}else{priors},
                                       postcalsamples = 500)

infTempBayesianBLM3 <- rec.bayesian(calModel = bayeslincals$BLM3_fit,
                                           recData = recData,
                                           mixed = TRUE,
                                           priors = if(priors == 'Weak'){"Uninformative"}else{priors},
                                    postcalsamples = 500)


RecComplete <- rbindlist(list("OLS"=lmrecClassic,
                              "York"=yorkrecClassic,
                              "Deming"=demingrecClassic,
                              "WOLS"=lminverserecClassic,
                              "BayesianBLM1" = infTempBayesianBLM1,
                              "BayesianBLM1_NoErrors" =infTempBayesianBLM1_fit_NoErrors,
                              'infTempBayesianBLM3'= infTempBayesianBLM3
                              ),
                         idcol = "Model", fill = TRUE)


toRet <- list("ParameterEstimates"=ParamEstimates,
     "Reconstructions"=RecComplete,
     "RawParams"=Params
     )

write.csv(ParamEstimates, here::here("Analyses","Results",paste0(samples,"_Obs"),paste0(name,"Replicates=", replicates,"Samples=",samples,priors ,"_ParameterEstimates.csv")))
write.csv(RecComplete, here::here("Analyses","Results",paste0(samples,"_Obs"), paste0(name,"Replicates=", replicates,"Samples=",samples,priors ,"_Recs.csv" )))
write.csv(Params, here::here("Analyses","Results",paste0(samples,"_Obs"),paste0(name,"Replicates=", replicates,"Samples=",samples,priors ,"_ParamsFull.csv" )))

return(toRet)
}

##50
###Informative
a <- RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=3000, 
                     priors='Informative')

b <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=3000, 
                     priors='Informative')

c <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=3000, 
                     priors='Informative')

###Weak

a1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=3000, 
                     priors='Weak')

b1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=3000, 
                     priors='Weak')


c1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=50, 
                     ngenerationsBayes=3000, 
                     priors='Weak')


###Uninformative

RunSingleFullResults(name="S1",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=3000, 
                          priors='Uninformative')

RunSingleFullResults(name="S2",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=3000, 
                          priors='Uninformative')


RunSingleFullResults(name="S3",
                          replicates=1000, 
                          samples=50, 
                          ngenerationsBayes=3000, 
                          priors='Uninformative')



##500
###Informative
d <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=3000, 
                     priors='Informative')

e <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=3000, 
                     priors='Informative')


f <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=3000, 
                     priors='Informative')


###Weak
d1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=3000, 
                     priors='Weak')

e1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=3000, 
                     priors='Weak')


f1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples= 500, 
                     ngenerationsBayes=3000, 
                     priors='Weak')


###Uninformative

RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=3000, 
                     priors='Uninformative')

RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=3000, 
                     priors='Uninformative')


RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=500, 
                     ngenerationsBayes=3000, 
                     priors='Uninformative')



##10
###Informative
g <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=3000, 
                     priors='Informative')

h <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=3000, 
                     priors='Informative')


i <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=3000, 
                     priors='Informative')


###Weak
g1 <-RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=3000, 
                     priors='Weak')

h1 <-RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=3000, 
                     priors='Weak')


i1 <-RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples= 10, 
                     ngenerationsBayes=3000, 
                     priors='Weak')

###Uninformative

RunSingleFullResults(name="S1",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=3000, 
                     priors='Uninformative')

RunSingleFullResults(name="S2",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=3000, 
                     priors='Uninformative')


RunSingleFullResults(name="S3",
                     replicates=1000, 
                     samples=10, 
                     ngenerationsBayes=3000, 
                     priors='Uninformative')



