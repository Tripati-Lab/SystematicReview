

obCal <- lmcals

predictTc <<- function(calData, 
                      targety, 
                      targetyError, 
                      nObs, 
                      obCal,
                      IgnoreParamUncertainty = TRUE){
  
  if(IgnoreParamUncertainty){
  
   t1 <- sqrt((mean(obCal$beta) * 10 ^ 6) / (targety - mean(obCal$alpha))) - 273.15
   t2 <-sqrt((mean(obCal$beta) * 10 ^ 6) / (targety+targetyError - mean(obCal$alpha))) - 273.15
    
   preds <- cbind.data.frame(D47= targety, 
                             D47error=targetyError,
                             temp=t1, 
                             se=t1-t2
                             )

   
  }else{
    
  calData <<- calData
  obCal <<- obCal
  std2 <<- function(i) sd(i)/sqrt(length(i))
  mod <<- stats::nls(formula = D47  ~ a + b1*(10^6/(Temperature+273.15)^2),
                     data = calData, 
                     start = list(a = mean(obCal$alpha),b1 = mean(obCal$beta)),
                     lower = c(a = (mean(obCal$alpha)-std2(obCal$alpha)*1.96),b1 = (mean(obCal$beta)-std2(obCal$beta)*1.96)),
                     upper = c(a = (mean(obCal$alpha)+std2(obCal$alpha)*1.96),b1 = (mean(obCal$beta)+std2(obCal$beta)*1.96)),
                     algorithm = "port") 
  
  indPreds <- lapply(1:length(targety), function(x){
  estimate <<- investr::invest(mod, y0 = rnorm(nObs[x], mean=targety[x], sd=targetyError[x]),
                               interval = "percentile",  seed = 3,  nsim=1000,
                               extendInt="yes", progress=F, 
                               lower=-100,
                               upper=100)
  
  cbind.data.frame(D47= targety[x], 
                   D47error=targetyError[x],
                   temp=estimate$estimate, 
                   se=estimate$se
                   )
  })
  
  preds <-do.call(rbind, indPreds)
  
  }
}
