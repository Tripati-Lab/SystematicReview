library(here)
library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)
library(dplyr)

ggthemr('light')

beta <- 0.0369
alpha <- 0.268

ds <- here("Analyses", "Results", "50_Obs")
TargetOutputFiles<-list.files(ds, pattern = "Informative_ParamsFull", full.names = T)
datasets <- lapply(TargetOutputFiles, read.csv)
  
  full <- rbind.data.frame(
    cbind.data.frame(Dataset='S1', datasets[[1]]),
    cbind.data.frame(Dataset='S2', datasets[[3]]),
    cbind.data.frame(Dataset='S3', datasets[[2]])
  )
  
nobsRepDataset <-  full

## Patterns for regression lines and CI
source("https://raw.githubusercontent.com/Tripati-Lab/BayClump/main/Functions/Calibration_BayesianNonBayesian.R")

dataset <- unique(nobsRepDataset$Dataset)
models <- unique(nobsRepDataset$Model)

CIs<- do.call(rbind, lapply(dataset, function(x){
  do.call(rbind, lapply(models, function(y){
    #do.call(rbind,lapply(dataset, function(z){
    
    a<-RegressionSingleCI(data = nobsRepDataset[nobsRepDataset$Dataset ==x &
                                                  nobsRepDataset$Model == y,], 
                          from = 5, to = 20)[[1]]
    cbind.data.frame(dataset=x,models=y,a)
    
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

CIs$dataset <- factor(CIs$dataset, levels = unique(CIs$dataset), labels = c("Low-error",
                                                                            "Intermediate-error",
                                                                            "High-error"))

alpha = 0.268
beta = 0.0369

p1 <- ggplot(CIs, aes(x=x, y=median_est)) + 
  #geom_line(aes(y = D47_ci_lower_est)) + 
  #geom_line(aes(y = D47_ci_upper_est)) + 
  geom_ribbon(aes(ymin = ci_lower_est,
                  ymax = ci_upper_est), 
              fill = "blue", alpha = .5) +
  geom_abline(slope = beta, 
              intercept = alpha, col='red')+
  geom_line(aes(x=x, y=median_est), color='black') + 
  facet_rep_wrap(~models+dataset, repeat.tick.labels = 'all', ncol=4) +
  facet_grid(vars(models), vars(dataset))+
  ylab(expression(Delta["47"]*" (‰)" ))+ 
  xlab(expression(paste(10^6, " / T"^2, "(Temperature in °K)")))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), #axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)
  )+ theme(text = element_text(size = 22))  +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(colour="black"),
        strip.text = element_text(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.x.bottom=element_line(color="black", size=0.1),
        axis.line.y.left=element_line(color="black", size=0.1))


pdf(here("Figures","Plots","Fig2.pdf"), 10, 14)
print(p1)
dev.off()

jpeg(here("Figures","Plots","Fig2.pdf"), 10, 14, units = "in", res=300)
print(p1)
dev.off()



