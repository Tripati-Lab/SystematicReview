library(rio)
library(data.table)
library(ggplot2)
library(here)

ds <- here::here("Analyses", "Results", "50_Obs")
TargetOutputFiles <-list.files(ds, pattern = "Weak_Recs", full.names = T)

S1 <- read.csv(TargetOutputFiles[[1]])
S2 <- read.csv(TargetOutputFiles[[2]])
S3 <- read.csv(TargetOutputFiles[[3]])

completePreds <- rbindlist(list(S1=S1, S2=S2, S3=S3), idcol = "Dataset")

completePreds$True.Temp <- sqrt((0.0369 * 10 ^ 6) / (completePreds$D47 - 0.268)) - 273.15
completePreds$distance <- (completePreds$Tc - completePreds$True.Temp)
completePreds$distancePer <- (completePreds$Tc - completePreds$True.Temp)/completePreds$True.Temp

completePreds$within95 <- data.table::between(completePreds$True.Temp, completePreds$Tc - 1.96*completePreds$se,
                                              completePreds$Tc + 1.96*completePreds$se)
completePreds$Bayesian <- grepl("BLM", completePreds$Model)
  
completePreds$Model2 <- completePreds$Model

write.csv(completePreds,  here::here("Analyses", "Results", "50_Obs", "Complete.predictions.csv"))

dist=0.5

completePreds2 <- completePreds


completePreds2$Model <- factor(completePreds2$Model,
                               levels = c("BayesianBLM1_NoErrors", "BayesianBLM1","infTempBayesianBLM3",
                                          "OLS","WOLS","Deming", "York") ,
                               labels = 
                               c("B-SL", "B-SL-E", "B-LMM", "OLS", "W-OLS", "D", "Y")
)


completePreds2$D47se <- factor(completePreds2$D47error, levels = unique(completePreds2$D47error),
                   labels = c('Error in target Δ47 = 0.005‰', "0.01‰", "0.02‰"))

completePreds2$Dataset <- factor(completePreds2$Dataset, levels = unique(completePreds2$Dataset), labels = c("Low-error",
                                                                            "Intermediate-error",
                                                                            "High-error"))

#completePreds2 <- completePreds2[completePreds2$Model != "B-LMM",]


library(ggthemr)
ggthemr('light')
##Plots
tdata <- completePreds2[completePreds2$D47 == 0.8,]
tdata$alpVal <- 1#ifelse(tdata$within95, 1, 0.8)

p1 <- 
  ggplot(data=tdata, aes(x=Model, y=meanTemp, color=Model))+
  geom_point( position=position_dodge(dist), size=2)+
  geom_errorbar(aes(ymin=meanTemp-error, 
                    ymax=meanTemp+error), 
                width=0.5,size=0.6,
                position=position_dodge(dist)
                ) +
  geom_hline(aes(yintercept=True.Temp), lty='dashed', color='black')+
  facet_grid(vars(Dataset),vars(factor(D47se))) + ylab("Temperature (°C)")+ xlab("Error in Δ47 (‰)")+ 
  scale_color_brewer(palette="Dark2") + guides(alpha='none', color='none')+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(colour="black",
                                   angle = 45,hjust=1),
        strip.text = element_text(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.x.bottom=element_line(color="black", size=0.1),
        axis.line.y.left=element_line(color="black", size=0.1),
        text = element_text(size=15),
        panel.background = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  xlab("")+
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")
  )

tdata <- completePreds2[completePreds2$D47 == 0.7,]
tdata$alpVal <- 1#ifelse(tdata$within95, 1, 0.8)
p2 <- ggplot(data=tdata, aes(x=Model, y=meanTemp, color=Model))+
  geom_point( position=position_dodge(dist), size=2)+
  geom_errorbar(aes(ymin=meanTemp-error, 
                    ymax=meanTemp+error), 
                width=0.5,size=0.6,
                position=position_dodge(dist)
  ) +
  geom_hline(aes(yintercept=True.Temp), lty='dashed', color='black')+
  facet_grid(vars(Dataset),vars(factor(D47se))) + ylab("Temperature (°C)")+ xlab("Error in Δ47 (‰)")+ 
  scale_color_brewer(palette="Dark2") + guides(alpha='none', color='none')+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(colour="black",
                                   angle = 45,hjust=1),
        strip.text = element_text(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.x.bottom=element_line(color="black", size=0.1),
        axis.line.y.left=element_line(color="black", size=0.1),
        text = element_text(size=15),
        panel.background = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  xlab("")+
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")
  )

tdata <- completePreds2[completePreds2$D47 == 0.6,]
tdata$alpVal <- 1#ifelse(tdata$within95, 1, 0.8)

p3 <- ggplot(data=tdata, aes(x=Model, y=meanTemp, color=Model))+
  geom_point( position=position_dodge(dist), size=2)+
  geom_errorbar(aes(ymin=meanTemp-error*2, 
                    ymax=meanTemp+error*2), 
                width=0.5,size=0.6,
                position=position_dodge(dist)
  ) +
  geom_hline(aes(yintercept=True.Temp), lty='dashed', color='black')+
  facet_grid(vars(Dataset),vars(factor(D47se))) + ylab("Temperature (°C)")+ xlab("Error in Δ47 (‰)")+ 
  scale_color_brewer(palette="Dark2") + guides(alpha='none', color='none')+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(colour="black"),
        axis.text.x = element_text(colour="black",
                                   angle = 45,hjust=1),
        strip.text = element_text(colour = 'black'),
        panel.border = element_rect(colour = "black", fill=NA),
        axis.line.x.bottom=element_line(color="black", size=0.1),
        axis.line.y.left=element_line(color="black", size=0.1),
        text = element_text(size=15),
        panel.background = element_blank())+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  xlab("")+
  theme(axis.text.x = element_text(colour="black"), 
        axis.text.y = element_text(colour="black"))+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black")
  )

ggsave(plot = p1, filename= here::here("Figures","Plots","Fig5.pdf"), device= cairo_pdf, width =  10, height =  8)


jpeg(here::here("Figures","Plots","Fig5.jpg"), 10, 8, units = "in", res=300)
print(p1)
dev.off()

ggsave(plot = p2, filename= here::here("Figures","Plots","Fig6.pdf"), device= cairo_pdf, width =  10, height =  8)


jpeg(here::here("Figures","Plots","Fig6.jpg"), 10, 8, units = "in", res=300)
print(p2)
dev.off()

ggsave(plot = p3, filename= here::here("Figures","Plots","Fig7.pdf"), device= cairo_pdf, width =  10, height =  8)


jpeg(here::here("Figures","Plots","Fig7.jpg"), 10, 8, units = "in", res=300)
print(p3)
dev.off()

