setwd("~/MEGA/Projects/2_BayClump_reviews/May2022/Figures")
library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)

ggthemr('light')

beta <- 0.0369
alpha <- 0.268
dirList <- list.dirs(".")[-1]
dirList <- list.dirs()[grep("Fig3", list.dirs())]

    target <- dirList[1]
    TargetOutputFiles<-list.files(target,  full.names = T)
    posterior_diffuse <- import_list(TargetOutputFiles[[1]])
    posterior_informed <- import_list(TargetOutputFiles[[3]])
    
    prior_diffuse <- cbind.data.frame(alpha=rnorm(1000, 0.231,0.065*3 ), 
                                      beta=rnorm(1000, 0.039,0.004*3 ))
    prior_informed <- cbind.data.frame(alpha=rnorm(1000, 0.231,0.065*1 ), 
                                       beta=rnorm(1000, 0.039,0.004*1 ))
    
    prior_informed$Model <- "prior prior_informed"
    prior_diffuse$Model <- "prior prior_diffuse"
    
    posterior_informed <- rbindlist(posterior_informed, idcol = "Model", fill=T)
    posterior_diffuse <- rbindlist(posterior_diffuse, idcol = "Model", fill=T)
    
    
    
   dataset <- rbindlist(list(Informed=posterior_informed,
                             Diffuse=posterior_diffuse,
                             Informed=prior_informed,
                             Diffuse=prior_diffuse
                             ), idcol = "Dist", fill= TRUE)
    
    
   dataset$Model <- factor(dataset$Model,
                        levels = c("Bayesian model no errors", 
                                   "Bayesian model with errors",
                                   "Bayesian mixed w errors",
                                   "prior prior_informed",
                                   "prior prior_diffuse") ,
                        labels = 
                          c("B-SL", "B-SL-E", "B-LMM", "P-I", "P-D")
   )
   
p1 <- ggplot(data = dataset, aes(x=alpha, fill=Model))+
     stat_density()+
     facet_wrap(~Dist)+ 
     geom_vline(xintercept = 0.268, linetype="dashed", color='black')+
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           axis.title.x = element_text(colour = "black"),
           axis.title.y = element_text(colour = "black"),
           axis.text.y = element_text(colour="black"),
           axis.text.x = element_text(colour="black"),
           strip.text = element_text(colour = 'black'),
           panel.border = element_rect(colour = "black", fill=NA),
           axis.line.x.bottom=element_line(color="black", size=0.1),
           axis.line.y.left=element_line(color="black", size=0.1),
           text = element_text(size=15))+
     scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                        expand = c(0, 0))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
                     expand = c(0, 0))+
     labs(fill = "") + xlab("Intercept")+  ylab("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")
  ) + theme(text = element_text(size = 22))  
   
p2 <- ggplot(data = dataset, aes(x=beta, fill=Model))+
  stat_density()+
  facet_wrap(~Dist)+ 
  geom_vline(xintercept = 0.0369, linetype="dashed", color='black')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(colour="black"),
        strip.text = element_text(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.x.bottom=element_line(color="black", size=0.1),
        axis.line.y.left=element_line(color="black", size=0.1),
        text = element_text(size=15))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10),
                     expand = c(0, 0))+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 4),
                     expand = c(0, 0))+
  labs(fill = "") + xlab("Slope")+ ylab("")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")
  ) + theme(text = element_text(size = 22))   

completePlot <- ggarrange(p1, p2)


pdf("Plots/Fig3.pdf", 20, 5)
print(completePlot)
dev.off()

jpeg("Plots/Fig3.jpg", 20, 5, units = "in", res=300)
print(completePlot)
dev.off()

