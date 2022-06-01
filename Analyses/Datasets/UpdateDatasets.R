calData <- read.csv("Dataset_S3.csv")

set.seed(1056)     
nobs = 1000  

#Low 
AddD= 0.0025 #Bernasconi
TrueD47= 0.0125 #Petersen
TempErrC= 0.019

#Intermediate
AddD= 0.0075
TrueD47= 0.0225
TempErrC= 0.077

#High
AddD= 0.0125
TrueD47= 0.0275
TempErrC= 0.155 


truex <- rnorm(nobs,12.02585352, 2.5)
errx <- rnorm(nobs, 0, TempErrC)
obsx <- truex + errx

beta1 <- 0.268
beta2 <- 0.0369
sdy <- TrueD47
sdobsy <- AddD

erry <- rnorm(nobs, 0, sdobsy)
truey <- rnorm(nobs,beta1 + beta2*truex,sdy)
obsy <- truey + erry


calData$x_TRUE <- truex
calData$Temperature <- obsx
calData$TempError <- errx
calData$y_TRUE <- truey
calData$D47error <- erry
calData$D47 <- obsy

