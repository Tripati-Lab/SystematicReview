

obCal <- lmcals

predictTc <<- function(calData,
                       recData,
                       obCal,
                       IgnoreParamUncertainty = TRUE){
  
  if(IgnoreParamUncertainty){
    
  samples <- unique(recData$Sample)
  recTemp <- lapply(seq_along(samples), function(x){
    subrecData <- recData[recData$Sample == samples[x],]
    temp <- sqrt((mean(obCal$beta) * 10 ^ 6) / 
                 (subrecData$D47 - mean(obCal$alpha))) - 273.15
    recTempS <- cbind.data.frame(meanD47 = mean(subrecData$D47),
                                 sdD47 = sd(subrecData$D47),
                                 meanTemp = mean(temp), 
                                 sdTemp = sd(temp))
    return(recTempS)
  } )
  
  recTemp <- do.call(rbind, recTemp)
  preds <- cbind.data.frame(Sample = samples, recTemp)
  preds
  }else{
    
  calData <<- calData
  obCal <<- observedCalibration
  std2 <<- function(i) sd(i)/sqrt(length(i))
  mod <<- stats::nls(formula = D47  ~ a + b1*(10^6/(Temperature+273.15)^2),
                     data = calData, 
                     start = list(a = mean(obCal$alpha),
                                  b1 = mean(obCal$beta)),
                     lower = c(a = (mean(obCal$alpha)-std2(obCal$alpha)*1.96),b1 = (mean(obCal$beta)-std2(obCal$beta)*1.96)),
                     upper = c(a = (mean(obCal$alpha)+std2(obCal$alpha)*1.96),b1 = (mean(obCal$beta)+std2(obCal$beta)*1.96)),
                     algorithm = "port") 
  
  samples <- unique(recData$Sample)
  indPreds <- lapply(seq_along(samples), function(x){
    subrecData <- recData[recData$Sample == samples[x],]
    
    temps <- unlist(lapply(subrecData$D47, function(y){
    estimate <<- investr::invest(mod, y0 = y,
                               interval = "percentile",  
                               seed = 3, 
                               nsim = 1,
                               extendInt = "yes", 
                               progress = F, 
                               lower = -100,
                               upper = 100)$estimate
    }))
  
  recTempS <- cbind.data.frame(meanD47 = mean(subrecData$D47),
                               sdD47 = sd(subrecData$D47),
                               meanTemp = mean(temps), 
                               sdTemp = sd(temps))
  })
  
  preds <- do.call(rbind, indPreds)
  preds <- cbind.data.frame(Sample = samples, preds)
  
  }
}
