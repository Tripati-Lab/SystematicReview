library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)
library(dplyr)
library(here)

ggthemr('light')

beta <- 0.0369
alpha <- 0.268

ds <- here("Analyses", "Results", "10_Obs")
Info10 <-list.files(ds, pattern = "Informative_ParamsFull", full.names = T)[1]
ds <- here("Analyses", "Results", "50_Obs")
Info50 <-list.files(ds, pattern = "Informative_ParamsFull", full.names = T)[1]
ds <- here("Analyses", "Results", "500_Obs")
Info500 <-list.files(ds, pattern = "Informative_ParamsFull", full.names = T)[1]

TargetOutputFiles <- c(Info10, Info50, Info500)
datasets <- lapply(TargetOutputFiles, read.csv)

full <- rbind.data.frame(
cbind.data.frame(nobs=10, datasets[[1]]),
cbind.data.frame(nobs=50, datasets[[2]]),
cbind.data.frame(nobs=500, datasets[[3]])
)
nobsRepDataset <- full

## Patterns for regression lines and CI
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/Calibration_BayesianNonBayesian.R")

nobs <- unique(nobsRepDataset$nobs)
models <- unique(nobsRepDataset$Model)

CIs<- do.call(rbind, lapply(nobs, function(x){
  do.call(rbind, lapply(models, function(y){
    #do.call(rbind,lapply(dataset, function(z){
    
a<-RegressionSingleCI(data = nobsRepDataset[nobsRepDataset$nobs ==x &
                                           nobsRepDataset$Model == y,], 
                   from = 5, to = 20)[[1]]
cbind.data.frame(nobs=x,models=y,a)

    #}))
  }))
}))


CIs$models <- factor(CIs$models,
                     levels = c("BLM1_fit_NoErrors", 
                                "BLM1_fit",
                                "BLM3_fit",
                                "OLS",
                                "WOLS",
                                "Deming", 
                                "York") ,
                     labels = 
                       c("B-SL", "B-SL-E", "B-LMM", "OLS", "W-OLS", "D", "Y")
)


CIs$nobs <- factor(CIs$nobs, levels = unique(CIs$nobs),
                   labels = c('n = 10', "n = 50", "n = 500"))

p1 <- ggplot(CIs, aes(x=x, y=median_est)) + 
  #geom_line(aes(y = D47_ci_lower_est)) + 
  #geom_line(aes(y = D47_ci_upper_est)) + 
  geom_ribbon(aes(ymin = ci_lower_est,
                  ymax = ci_upper_est), 
              fill = "blue", alpha = .5) +
  geom_abline(slope = beta, 
              intercept = alpha, col='red')+
  geom_line(aes(x=x, y=median_est), color='black') + 
  facet_rep_wrap(~models+nobs, repeat.tick.labels = 'all', ncol=4) +
  facet_grid(vars(models), vars(nobs))+
  ylab(expression(Delta["47"]*" (‰)" ))+ 
  xlab(expression(paste(10^6, " / T"^2, "(Temperature in °K)")))+  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), #axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  )+ theme(text = element_text(size = 22))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(colour="black"),
        strip.text = element_text(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.x.bottom=element_line(color="black", size=0.1),
        axis.line.y.left=element_line(color="black", size=0.1))


pdf(here("Figures","Plots","Fig4.pdf"), 10, 14)
print(p1)
dev.off()

jpeg(here("Figures","Plots","Fig4.jpg"), 10, 14, units = "in", res=300)
print(p1)
dev.off()


