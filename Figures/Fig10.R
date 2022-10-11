library(rio)
library(data.table)
library(ggplot2)
library(ggpubr)
library(lemon)
library(ggthemr)
ggthemr('light')

ds <- here::here("Analyses", "Results", "Anderson_Full")
AndDS <- read.csv(here::here("Analyses", "Datasets", "RW_Anderson_Full.csv"))


datasets <- rio::import_list(list.files(ds, full.names = T))
datasets <- datasets[grep(" CI", names(datasets))]
datasets <- rbindlist(datasets, idcol = "Model", fill = TRUE)


datasets$Model <- factor(datasets$Model,
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

datasets <- datasets[! datasets$Model == "B-LMM",]

datasets$material <- ifelse(is.na(datasets$material), "Full", datasets$material)

#Subset per material

#range1 <- range(AndDS[AndDS$Material == 1,"Temperature"])
#range2 <- range(AndDS[AndDS$Material == 2,"Temperature"])

#Mat1 <- datasets[datasets$material == 1,]
#Mat2 <- datasets[datasets$material == 2,]
#MatFull <- datasets[datasets$material == "Full",]

#Mat1 <- Mat1[between(unlist(Mat1[,2]), range1[1], range1[2] ),]
#Mat2 <- Mat2[between(unlist(Mat2[,2]), range2[1], range2[2] ),]
#datasets <- rbind.data.frame(MatFull, Mat1, Mat2)

##



Anderson <- bayclumpr::cal.ci(data.frame(alpha=rnorm(1000, 0.154, 4.00E-03),
                                          beta=rnorm(1000, 0.0391, 4.00E-04)), 
                               0.5,13.3)
regs <- datasets
colnames(regs)[2] <- "Temperature"


p1 <- 
  ggplot(data=regs) +
  geom_ribbon(data=Anderson[[1]],aes(x=x, y = median_est, ymin = ci_lower_est,
                                     ymax = ci_upper_est), color=NA, fill="darkblue",
              alpha = 0.5)+
  geom_ribbon(data=regs,aes(x=Temperature, y = D47_median_est, ymin = D47_ci_lower_est,
                            ymax = D47_ci_upper_est), fill = "grey",
              alpha = 0.5)+
  geom_line(data=regs,aes(x=Temperature, y = D47_median_est), color = ' black')+
  geom_line(data=Anderson[[1]],
            aes(x=x, y = median_est), color="red", lty="dashed")+

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
  ) + theme(text = element_text(size = 17))



ggsave(plot = p1, filename= here::here("Figures","Plots",'Fig10.pdf'), device= cairo_pdf, width =  15, height =  4)


jpeg(here::here("Figures","Plots",'Fig10.jpg'), 15, 4, units = "in", res=300)
print(p1)
dev.off()

