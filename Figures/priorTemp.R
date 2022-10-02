library(ggthemr)
library(ggplot2)

set.seed(3)
ggthemr('light')
data <- data.frame(Temp = rnorm(1000, 11, 5))
data$TempC <- sqrt(10^6/data$Temp) - 273.15
data <- reshape::melt(data)
qnts <- quantile(data$value[data$variable == "TempC"], c(0.025, 0.975), na.rm = T)

data <- data[-which(data$value >qnts[2] | data$value <qnts[1]),]
data <- na.omit(data)
library(ggplot2)

data$variable <- ifelse(data$variable == "Temp", "1/T (K)", "T (C)")

p1 <- ggplot(data) + geom_histogram(aes(x = value))+
  facet_grid(~variable, scales = "free") +
  xlab("Temperature")+ 
  theme(text = element_text(size = 22))  +
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
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7))

jpeg(here::here("Figures","Plots","FigS3.jpg"), 15, 5, units = "in", res=300)
print(p1)
dev.off()


