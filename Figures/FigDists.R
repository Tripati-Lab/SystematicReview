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

ds <- here::here("Analyses", "Datasets")
TargetOutputFiles<-list.files(ds, pattern = "Dataset_", full.names = T)
datasets <- lapply(TargetOutputFiles, read.csv)

full <- rbind.data.frame(
  cbind.data.frame(Dataset='S1', datasets[[1]]),
  cbind.data.frame(Dataset='S2', datasets[[2]]),
  cbind.data.frame(Dataset='S3', datasets[[3]])
)


full$Dataset <- factor(full$Dataset, levels = unique(full$Dataset), labels = c("Low-error",
                                                                                                             "Intermediate-error",
                                                                                                             "High-error"))




p1 <- 
  ggplot(full, aes(x = Temperature, y = D47, color= Dataset)) + 
  geom_point()+ 
  geom_errorbarh(aes(xmin = Temperature - TempError,xmax = Temperature + TempError)) + 
  geom_errorbar(aes(ymin = D47 - D47error,ymax = D47 + D47error)) +
    geom_abline(slope = beta, 
                intercept = alpha, col='black')+
  facet_grid(cols = vars(Dataset)) +
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
        axis.line.y.left=element_line(color="black", size=0.1))+
  theme(legend.position="none")


pdf(here::here("Figures","Plots","FigDist.pdf"), 10, 5)
print(p1)
dev.off()

jpeg(here::here("Figures","Plots","FigDist.jpg"), 10, 5, units = "in", res=300)
print(p1)
dev.off()


