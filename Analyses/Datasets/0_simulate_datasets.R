## Function to simulate the datasets

genDS <- function(error = "S3", nobs = 1000, export = TRUE){
  set.seed(3)     

if(error == 'S1'){
#Low 
AddD = 0.0025 #Bernasconi
TrueD47 = 0.0125 #Petersen
TempErrC = 0.019
  }
  
if(error == 'S2'){
#Intermediate
AddD = 0.0075
TrueD47 = 0.0225
TempErrC = 0.077
}

if(error == 'S3'){
#High
AddD = 0.0125
TrueD47 = 0.0275
TempErrC = 0.155 
}
  
  
  # simulate covariate data
  n <- nobs
  sdx <- 2
  pop_taux <- 1 / (sdx * sdx)
  sdobs <- TempErrC
  taux <- 1 / (sdobs * sdobs)
  truex <- rnorm(n, 12.2, sdx)
  errorx <- rnorm(n, 0, sdobs)
  obsx <- truex + errorx
  
  # simulate response data
  alpha <- 0.268
  beta <- 0.0369
  
  # process error
  p_sdy <-  TrueD47
  p_tauy <- 1 / (p_sdy * p_sdy)
  p_errory <- rnorm(n, 0, p_sdy)
  
  # measurement error
  obs_sdy <- AddD  
  obs_tauy <- 1 / (obs_sdy * obs_sdy)
  obs_errory <- rnorm(n,0,obs_sdy)
  
  truey <- alpha + beta*truex + p_errory
  obsy <- truey + obs_errory
  
#library(IsoplotR)
#york(cbind(obsx,errorx,obsy,obs_errory))
#lm(obsy~obsx)

ds <- cbind.data.frame(
x_TRUE = truex,
Temperature = obsx,
TempError = errorx,
y_TRUE = truey,
D47error = obs_errory,
D47 = obsy,
Material = 1)
write.csv(ds, here::here("Analyses","Datasets",paste0("Dataset_",error,"_",nobs, ".csv")))
return(ds)
}


## Simulating the datasets

a1 <- genDS(error = "S1", nobs = 10)
a2 <- genDS(error = "S1", nobs = 50)
a3 <- genDS(error = "S1", nobs = 500)
a4 <- genDS(error = "S1", nobs = 1000)


b1 <- genDS(error = "S2", nobs = 10)
b2 <- genDS(error = "S2", nobs = 50)
b3 <- genDS(error = "S2", nobs = 500)
b4 <- genDS(error = "S2", nobs = 1000)


c1 <- genDS(error = "S3", nobs = 10)
c2 <- genDS(error = "S3", nobs = 50)
c3 <- genDS(error = "S3", nobs = 500)
c4 <- genDS(error = "S3", nobs = 1000)

