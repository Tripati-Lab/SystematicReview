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

ds <- here::here("Analyses", "Results", "50_Obs")
TargetOutputFiles<-list.files(ds, pattern = "Weak_ParameterEstimates", full.names = T)
datasets <- lapply(TargetOutputFiles, read.csv)

full <- rbind.data.frame(
  cbind.data.frame(Dataset='S1', datasets[[1]]),
  cbind.data.frame(Dataset='S2', datasets[[2]]),
  cbind.data.frame(Dataset='S3', datasets[[3]])
)

nobsRepDataset <-  full



nobsRepDataset$Model <- factor(nobsRepDataset$Model,
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

nobsRepDataset$Dataset <- factor(nobsRepDataset$Dataset, levels = unique(nobsRepDataset$Dataset), labels = c("Low-error",
                                                                                                             "Intermediate-error",
                                                                                                             "High-error"))

alpha = 0.268
beta = 0.0369

nobsRepDataset$type <- ifelse(nobsRepDataset$Model %in% c('B-SL', 'B-SL-E', 'B-LMM'), "Bayesian", "Frequentist")

write.csv(nobsRepDataset,  here::here("Analyses", "Results", "50_Obs", "Summary.csv"))

p1 <- 
  ggplot(nobsRepDataset, aes(x = alpha.mean, y = beta.mean, color = Model)) + 
  geom_errorbar(aes(ymin = beta.mean - beta.sd,ymax = beta.mean + beta.sd, lty= type), size = .6) + 
  geom_errorbarh(aes(xmin = alpha.mean - alpha.sd,xmax = alpha.mean + alpha.sd, lty= type), size =.6) +
  geom_point(aes(alpha, beta), color = "black") +
  geom_point()+ 
  facet_wrap(~(Dataset)) +
  ylab(expression(beta))+ 
  xlab(expression(alpha))+ 
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
        axis.line.y.left=element_line(color="black", size=0.1),
        legend.key = element_rect(fill = "white"),
        legend.text=element_text(color="black",size=15),
        legend.key.size = unit(7,"point"), 
        legend.title=element_blank())+ 
  theme(legend.position="bottom") +
  guides(colour = guide_legend(nrow = 1))+ 
  scale_color_brewer(palette = "Dark2")

p1

pdf(here::here("Figures","Plots","Fig3.pdf"), 14, 5)
print(p1)
dev.off()

jpeg(here::here("Figures","Plots","Fig3.jpg"), 14, 5, units = "in", res=300)
print(p1)
dev.off()



