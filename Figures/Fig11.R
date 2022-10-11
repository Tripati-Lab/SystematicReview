library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)
ggthemr('light')

ds <- list.files(here::here("Analyses", "Results", "Sun"), full.names = TRUE)
datasets <- rio::import_list(ds)
datasets <- rbindlist(datasets, idcol = "Model")
colnames(datasets)[6] <- 'SD'


#write.csv(datasets, here::here("Figures", "OriginalSun.csv"))

ds_sun <- read.csv(here::here("Figures", "OriginalSun.csv"), row.names = 1)
colnames(ds_sun) <- colnames(datasets)

fullDS <- rbindlist(list(datasets, ds_sun))


fullDS$Sample <- ifelse(fullDS$`Δ47 (‰)` == 0.706, "~6.2 Ma", "0")
fullDS$Temperature <- fullDS$`Temperature (°C)`


fullDS <- fullDS[fullDS$Model != "Petersen et al. (2019)",]

fullDS$Model <- factor(fullDS$Model,
                         levels = c("Bayesian linear model", "Bayesian linear model, errors", "Bayesian linear mixed model" , "Linear" , "Inverse linear", "Deming", "York" , "Henkes et al. (2013)" , "Tripati et al. (2015)", "Petersen et al. (2019)"   ) ,
                         labels = 
                           c("B-SL", "B-SL-E", "B-LMM", "OLS", "W-OLS", "D", "Y", "H-2013", "T-2015", "P-2019")
)

p1 <- ggplot(data=fullDS) +
  geom_errorbar(aes(x=Model, ymin = Temperature-SD,ymax = Temperature+SD, color=factor(Sample)),
                position=position_dodge(width=0.9), width=0.5, size=.7)+
  geom_point(aes(y=Temperature, x=Model, color=factor(Sample)), position=position_dodge(width=0.9))+
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


ggsave(plot = p1, filename= here::here("Figures","Plots",'Fig11.pdf'), device= cairo_pdf, width =  15, height =  5)

jpeg(here::here("Figures","Plots",'Fig11.jpg'), 10, 5, units = "in", res=300)
print(p1)
dev.off()

