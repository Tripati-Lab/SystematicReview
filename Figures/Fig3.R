library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)
library(bayclumpr)

ggthemr('light')

beta <- 0.0369
alpha <- 0.268


if(!file.exists(here::here("Figures/WS_Fig3.RData"))){

ngenerationsBayes = 3000
name = "S2"

calData <- read.csv(here::here("Analyses","Datasets", paste0("Dataset_",name,"_",samples, ".csv")))
calData$D47error <- abs(calData$D47error)
calData$TempError <- abs(calData$TempError)

priors = "Informative"
bayeslincals_informative <- cal.bayesian(calibrationData = calData, 
                             priors = priors,
                             numSavedSteps = ngenerationsBayes)

priors = "Weak"

bayeslincals_weak <- cal.bayesian(calibrationData = calData, 
                                  priors = priors,
                                  numSavedSteps = ngenerationsBayes)


priors = "Uninformative"

bayeslincals_Uninformative <- cal.bayesian(calibrationData = calData, 
                                           priors = priors,
                                           numSavedSteps = ngenerationsBayes)


save.image(here::here("Figures/WS_Fig3.RData"))

}

load(here::here("Figures/WS_Fig3.RData"))

    #Priors
    prior_weak <- cbind.data.frame(alpha=rnorm(1000, 0.231,1), 
                                      beta=rnorm(1000, 0.039,1 ))
    prior_informed <- cbind.data.frame(alpha=rnorm(1000, 0.231,0.065*1 ), 
                                       beta=rnorm(1000, 0.039,0.004*1 ))
    prior_informed$Model <- "Prior"
    prior_weak$Model <- "Prior"

    
    params_inf <- data.frame(rstan::extract(bayeslincals_informative$BLM1_fit, c('beta', 'alpha')), Model = "Posterior")
    params_weak <- data.frame(rstan::extract(bayeslincals_weak$BLM1_fit, c('beta', 'alpha')), Model = "Posterior")
    params_uninf <- data.frame(rstan::extract(bayeslincals_Uninformative$BLM1_fit, c('beta', 'alpha')), Model = "Posterior")
    
  colnames(params_inf)
  colnames(params_weak)
  colnames(params_uninf)
  
  colnames(prior_informed)
  colnames(prior_weak)
  
    
   dataset <- rbindlist(list(Informed=params_inf,
                             Weak=params_weak,
                             Informed=prior_informed,
                             Weak=prior_weak
                             ), idcol = "Dist", fill= TRUE)
    
   dataset$Model <- factor(dataset$Model, levels = c("Prior", "Posterior"))
   
p1 <- ggplot(data = dataset, aes(x=alpha, fill=Model, group = Model))+
  stat_density(aes( y=..scaled..,color=Model), position="dodge", color = NA)+
  facet_wrap(~Dist, scales = "free") + 
     geom_vline(xintercept = 0.268, linetype="dashed", color='black') +
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
  ) + theme(text = element_text(size = 22),
            legend.text=element_text(color="black",size=15),
            legend.title=element_blank())  +
  theme(legend.position="bottom")
   


p2 <- ggplot(data = dataset, aes(x=beta, fill=Model))+
  stat_density(aes( y=..scaled..,color=Model), position="dodge", color = NA)+
  facet_wrap(~Dist, scales = "free")+ 
  geom_vline(xintercept = 0.0369, linetype="dashed", color='black') +
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
  )  + theme(text = element_text(size = 22),
             legend.text=element_text(color="black",size=15),
             legend.title=element_blank())  +
  theme(legend.position="bottom") 

completePlot <- ggarrange(p1, p2)

pdf(here::here("Figures","Plots","Fig3.pdf"), 20, 5)
print(completePlot)
dev.off()

jpeg(here::here("Figures","Plots","Fig3.jpg"), 20, 5, units = "in", res=300)
print(completePlot)
dev.off()

