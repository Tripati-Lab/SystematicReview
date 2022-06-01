setwd("~/MEGA/Projects/2_BayClump_reviews/May2022/Figures")
library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)


dirList <- list.dirs(".")[-1]
dirList <- list.dirs()[grep("Fig10", list.dirs())]

target <- dirList[1]
TargetOutputFiles<-list.files(target,  full.names = T)
atm <- import_list(TargetOutputFiles[[1]])
full <- import_list(TargetOutputFiles[[2]])
petersen <- import_list(TargetOutputFiles[[3]])

atm <- rbindlist(atm, idcol = "Model", fill = TRUE)
full <- rbindlist(full, idcol = "Model", fill = TRUE)
petersen <- rbindlist(petersen, idcol = "Model", fill = TRUE)

dataset <- rbindlist(list(atm=atm,full=full, petersen=petersen), idcol = "Dataset")

sun <- read.csv(TargetOutputFiles[[4]])

colnames(sun)[c(3,7)] <- c("D47", "Model")
colnames(dataset)[c(3,5,6)] <- c("D47", "Temperature", "Error")

sun1 <- sun
sun2 <- sun
sun3 <- sun

sun1$Dataset <- "atm"
sun2$Dataset <- "full"
sun3$Dataset <- "petersen"

sun1$type <- "sun"
sun2$type <- "sun"
sun3$type <- "sun"

fullDS <- rbindlist(list(dataset, sun1, sun2, sun3 ), fill = T)

fullDS$Type <- ifelse(is.na(fullDS$type), 'Re-analysis', 'Sun et al. (2021)')

fullDS$Age <- rep(c(6.2,0), nrow(fullDS)/2 )

fullDS$Model <- factor(fullDS$Model,
                               levels = c("Bayesian linear model", 
                                          "Bayesian linear model, errors",
                                          "Bayesian linear mixed model",
                                          "Linear",
                                          "Inverse linear",
                                          "Deming", 
                                          "York",
                                          "Tripati et al. (2015)",
                                          "Henkes et al. (2013)",
                                          "Petersen et al. (2019)"
                                          ) ,
                               labels = 
                                 c("B-SL", "B-SL-E", "B-LMM", "OLS", "W-OLS", "D", "Y",
                                   "T-2015", "H-2013", "P-2019")
)

fullDS$Dataset <- factor(fullDS$Dataset, levels = c("petersen", "full", "atm"),
                         labels = c("Petersen", "Anderson - Full" , "Anderson - ATM"))

p1 <- ggplot(data=fullDS) +
  geom_errorbar(aes(x=Model, ymin = Temperature-Error,ymax = Temperature+Error, color=factor(Age), linetype=Type),
                position=position_dodge(width=0.9), width=0.5, size=.7)+
  geom_point(aes(y=Temperature, x=Model, color=factor(Age)), position=position_dodge(width=0.9))+
  facet_grid(Dataset~.) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(colour="black",
                                   angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.x.bottom=element_line(color="black", size=0.1),
        axis.line.y.left=element_line(color="black", size=0.1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  xlab("") + scale_color_manual(values=c("#62bba5", "orange"))+labs(color="Age (Ma)")+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black"),
  axis.ticks = element_line(colour = "black")
  )+ theme(text = element_text(size = 20))+ ylab("Temperature (°C)")


pdf('Plots/Fig11.pdf', 15, 7)
print(p1)
dev.off()

jpeg('Plots/Fig11.jpg', 15, 7, units = "in", res=300)
print(p1)
dev.off()

