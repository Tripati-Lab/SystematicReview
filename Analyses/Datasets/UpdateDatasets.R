#calData <- read.csv(here::here("Analyses","Datasets","Dataset_S3_1000.csv"))


genDS <- function(error = "S1", nobs = 1000){
  set.seed(3)     

  if(error == 'S1'){
#Low 
AddD= 0.0025 #Bernasconi
TrueD47= 0.0125 #Petersen
TempErrC= 0.019

  }
if(error == 'S2'){
#Intermediate
AddD= 0.0075
TrueD47= 0.0225
TempErrC= 0.077
}

if(error == 'S3'){
#High
AddD= 0.0125
TrueD47= 0.0275
TempErrC= 0.155 
}

truex <- rnorm(nobs,12.02585352, 2.5)
errx <- rnorm(nobs, 0, TempErrC)
obsx <- truex + errx

beta1 <- 0.268
beta2 <- 0.0369
sdy <- TrueD47
sdobsy <- AddD

erry <- rnorm(nobs, 0, sdy)
truey <- rnorm(nobs,beta1 + beta2*truex,sdobsy)
obsy <- truey + erry

ds <- cbind.data.frame(
x_TRUE = truex,
Temperature = obsx,
TempError = errx,
y_TRUE = truey,
D47error = erry,
D47 = obsy,
Material = 1)
write.csv(ds, here::here("Analyses","Datasets",paste0("Dataset_",error,"_",nobs, ".csv")))
ds
}

a1 <- genDS(error = "S1", nobs = 10)
a2 <- genDS(error = "S1", nobs = 50)
a3 <- genDS(error = "S1", nobs = 500)


b1 <- genDS(error = "S2", nobs = 10)
b2 <- genDS(error = "S2", nobs = 50)
b3 <- genDS(error = "S2", nobs = 500)


c1 <- genDS(error = "S3", nobs = 10)
c2 <- genDS(error = "S3", nobs = 50)
c3 <- genDS(error = "S3", nobs = 500)

lm(a1$y_TRUE~a1$x_TRUE)
lm(a1$D47~a1$Temperature)

lm(a2$y_TRUE~a2$x_TRUE)
lm(a2$D47~a2$Temperature)

lm(a3$y_TRUE~a3$x_TRUE)
lm(a3$D47~a3$Temperature)



lm(b1$y_TRUE~b1$x_TRUE)
lm(b1$D47~b1$Temperature)

lm(b2$y_TRUE~b2$x_TRUE)
lm(b2$D47~b2$Temperature)

lm(b3$y_TRUE~b3$x_TRUE)
lm(b3$D47~b3$Temperature)

