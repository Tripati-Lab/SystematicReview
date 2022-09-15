recData <- read.csv(here::here("Analyses","Datasets", "BayClump_reconstruction_template.csv")) 


replicateLevelDs <- lapply(1:nrow(recData), function(x){
  vals <- rnorm(recData$N[x], mean=recData$D47[x], sd=recData$D47error[x])
  replicateLevel <- cbind.data.frame(recData[x,], vals)
  colnames(replicateLevel)[c(5, 7)] <- c("TrueD47", "D47")
  replicateLevel
})

replicateLevelDs <- do.call(rbind, replicateLevelDs)

write.csv(replicateLevelDs, here::here("Analyses","Datasets", "BayClump_reconstruction_template_replicates.csv"))
