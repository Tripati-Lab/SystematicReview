setwd("~/MEGA/Projects/2_BayClump_reviews/May2022/Figures")
library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)


dirList <- list.dirs(".")[-1]
dirList <- list.dirs()[grep("Fig9", list.dirs())]

target <- dirList[1]
TargetOutputFiles<-list.files(target,  full.names = T)
atm <- import_list(TargetOutputFiles[[1]])
full <- import_list(TargetOutputFiles[[2]])


atm <- atm[-grep( "CI", names(atm))]
atm <- rbindlist(atm, idcol = "Model", fill = TRUE)

full <- full[-grep( "CI", names(full))]
full <- rbindlist(full, idcol = "Model", fill = TRUE)

models <- unique(full$Model)
CIsFull<- do.call(rbind, lapply(models, function(x){
  print(x)
  if(x != "Bayesian mixed w errors"){
    a<-RegressionSingleCI(data = full[full$Model ==x ,], 
                          from = 0.5, to = 13.3)[[1]]
    cbind.data.frame(models=x, material='full', a)
  }else{
    mat1 <- full[full$Model ==x ,]
    mat1[,c(2,3)] <- mat1[,c(6,7)]
    a1<-RegressionSingleCI(data = mat1, 
                          from = 0.5, to = 13.3)[[1]]
    mat2 <- full[full$Model ==x ,]
    mat2[,c(2,3)] <- mat2[,c(8,9)]
    a2<-RegressionSingleCI(data = mat2, 
                           from = 0.5, to = 13.3)[[1]]
    rbind(cbind.data.frame(models=x, material=1, a1),
    cbind.data.frame(models=x, material=2, a2))
  }
    
}))

CIsatm<- do.call(rbind, lapply(models, function(x){
  print(x)
  if(x != "Bayesian mixed w errors"){
    a<-RegressionSingleCI(data = atm[atm$Model ==x ,], 
                          from = 10, to = 13.3)[[1]]
    cbind.data.frame(models=x, material='full', a)
  }else{
    mat1 <- atm[atm$Model ==x ,]
    mat1[,c(2,3)] <- mat1[,c(6,7)]
    a1<-RegressionSingleCI(data = mat1, 
                           from = 10, to = 13.3)[[1]]
    mat2 <- atm[atm$Model ==x ,]
    mat2[,c(2,3)] <- mat2[,c(8,9)]
    a2<-RegressionSingleCI(data = mat2, 
                           from = 10, to = 13.3)[[1]]
    rbind(cbind.data.frame(models=x, material=1, a1),
          cbind.data.frame(models=x, material=2, a2))
  }
  
}))


Anderson_full <- RegressionSingleCI(data.frame(alpha=rnorm(1000, 0.154, 4.00E-03),
                                          beta=rnorm(1000, 0.0391, 4.00E-04)), 
                               0.5,13.3)

Anderson_atm <- RegressionSingleCI(data.frame(alpha=rnorm(1000, 0.154, 4.00E-03),
                                               beta=rnorm(1000, 0.0391, 4.00E-04)), 
                                    10,13.3)

Anderson <- rbind.data.frame(cbind.data.frame(Dataset='full', Anderson_full[[1]]),
                             cbind.data.frame(Dataset='atm', Anderson_atm[[1]]))

regs <- rbindlist(list(atm=CIsatm,full=CIsFull), idcol = "Dataset")


regs$models <- factor(regs$models,
                               levels = c("Bayesian model no errors", 
                                          "Bayesian model with errors",
                                          "Bayesian mixed w errors",
                                          "Linear regression",
                                          "Inverse linear regression",
                                          "Deming regression", 
                                          "York regression") ,
                               labels = 
                                 c("B-SL", "B-SL-E", "B-LMM", "OLS", "W-OLS", "D", "Y")
)


regs$Dataset <- factor(regs$Dataset, c("full", "atm"), labels = c("Anderson - Full", "Anderson -  ATM"))
Anderson$Dataset <- factor(Anderson$Dataset, c("full", "atm"), labels = c("Anderson - Full", "Anderson -  ATM"))

p1 <- ggplot(data=regs) +
   geom_ribbon(data=regs,aes(x=x, y = median_est, ymin = ci_lower_est,
                            ymax = ci_upper_est, group=material),color=NA,fill="orange",
              alpha = 0.8)+
  geom_line(data=regs,aes(x=x, y = median_est,group=material), color="blue")+
  geom_line(data=Anderson,
            aes(x=x, y = median_est), color="red", lty="dashed")+
  geom_ribbon(data=Anderson,aes(x=x, y = median_est, ymin = ci_lower_est,
                                     ymax = ci_upper_est),color=NA,fill="grey",
              alpha = 0.8)+
  facet_grid(Dataset~models, scales = "free_y") +
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
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")
  )+ theme(text = element_text(size = 17)) 


pdf('Plots/Fig10.pdf', 10, 5)
print(p1)
dev.off()


jpeg('Plots/Fig10.jpg', 15, 5, units = "in", res=300)
print(p1)
dev.off()

