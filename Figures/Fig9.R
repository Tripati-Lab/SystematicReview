setwd("~/MEGA/Projects/2_BayClump_reviews/May2022/Figures")
library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)


dirList <- list.dirs(".")[-1]
dirList <- list.dirs()[grep("Fig8", list.dirs())]

target <- dirList[1]
TargetOutputFiles<-list.files(target,  full.names = T)
calEst <- import_list(TargetOutputFiles[[1]])
calEst <- calEst[grep( "CI", names(calEst))]
regs <- rbindlist(calEst, idcol = "Model", fill = TRUE)


Petersen <- RegressionSingleCI(data.frame(alpha=rnorm(1000, 0.258, 1.70E-05),
                                          beta=rnorm(1000, 0.0383, 1.70E-06)), 0.8,13.6)

#regs <- rbindlist(list(calEst, cbind.data.frame(Model="Petersen", Petersen)), fill = T)


regs$Model <- factor(regs$Model,
                               levels = c("Bayesian model no errors CI", 
                                          "Bayesian model with errors CI",
                                          "Bayesian mixed w errors CI",
                                          "Linear regression CI",
                                          "Inverse linear regression CI",
                                          "Deming regression CI", 
                                          "York regression CI") ,
                               labels = 
                                 c("B-SL", "B-SL-E", "B-LMM", "OLS", "W-OLS", "D", "Y")
)

colnames(regs)[2] <- "Temperature"

p1 <- ggplot(data=regs) +
   geom_ribbon(data=regs,aes(x=Temperature, y = D47_median_est, ymin = D47_ci_lower_est,
                            ymax = D47_ci_upper_est),color=NA,fill="orange",
              alpha = 0.8)+
  geom_line(data=regs,aes(x=Temperature, y = D47_median_est), color="blue")+
  geom_line(data=Petersen[[1]],
            aes(x=x, y = median_est), color="red", lty="dashed")+
  geom_ribbon(data=Petersen[[1]],aes(x=x, y = median_est, ymin = ci_lower_est,
                                     ymax = ci_upper_est),color=NA,fill="grey",
              alpha = 0.8)+
  facet_grid(.~Model) +
  ylab(expression(Delta["47"]*" (‰)" ))+ 
  xlab(expression(paste(10^6, " / T"^2, "(Temperature in °K)")))+ 
  guides(color="none", fill='none')+ 
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
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")
  )+ theme(text = element_text(size = 17))


pdf('Plots/Fig8.pdf', 15, 4)
print(p1)
dev.off()

jpeg("Plots/Fig8.jpg", 15, 4, units = "in", res=300)
print(p1)
dev.off()


