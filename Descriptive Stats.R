#import data
new_elnino <- read.csv("new_elnino.csv")
x <- c()
#calculate the buoy with most data
for (i in 0:71){
  x=c(x,sum(new_elnino$buoy==i))}
which(x==max(x))
#data from buoy 2
library(dplyr)
buoy2 = new_elnino %>% filter(buoy == 2)
buoy2 <- subset(buoy2,select = -c(diff,buoy))
buoy2 <- na.omit(buoy2)
buoy2$Sea.Surface.Temp <- as.numeric(buoy2$Sea.Surface.Temp)
#find the mode
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#visulization of buoy 2 data with linear regression between each variable
library(ggplot2)
ggplot(buoy2,aes(y=buoy2$Zonal.Winds)) + 
  geom_boxplot(color="lightblue", notch=TRUE, fill="azure", outlier.color="red", outlier.shape=20,outlier.size=4) +
  ylab("Zonal.Winds") +
  scale_x_discrete() +
  ggtitle("Boxplot of Zonal.Winds")

ggplot(buoy2,aes(x=Zonal.Winds,y=Sea.Surface.Temp)) + 
  geom_point(color = "blue") +
  xlab("Zonal.Winds") +
  ylab("Sea.Surface.Temp") +
  ggtitle("Scatterplot of Zonal.Winds") +
  geom_smooth(method = "lm",color = "red") +
  geom_smooth(color = "orange")

library(pastecs)

getMode(buoy2$Zonal.Winds)
round(stat.desc(buoy2[,5]),2)

ggplot(buoy2,aes(y=buoy2$Meridional.Winds)) + 
  geom_boxplot(color="lightblue", notch=TRUE, fill="azure", outlier.color="red", outlier.shape=20,outlier.size=4) +
  ylab("Meridional.Winds") +
  scale_x_discrete() +
  ggtitle("Boxplot of Meridional.Winds")
  

ggplot(buoy2,aes(x=Meridional.Winds,y=Sea.Surface.Temp)) + 
  geom_point(color = "blue") +
  xlab("Meridional.Winds") +
  ylab("Sea.Surface.Temp") +
  ggtitle("Scatterplot of Meridional.Winds") +
  geom_smooth(method = "lm",color = "red") +
  geom_smooth(color = "orange")

getMode(buoy2$Meridional.Winds)
round(stat.desc(buoy2[,6]),2)

ggplot(buoy2,aes(y=buoy2$Humidity)) + 
  geom_boxplot(color="lightblue", notch=TRUE, fill="azure", outlier.color="red", outlier.shape=20,outlier.size=4) +
  ylab("Humidity") +
  scale_x_discrete() +
  ggtitle("Boxplot of Humidity")

ggplot(buoy2,aes(x=Humidity,y=Sea.Surface.Temp)) + 
  geom_point(color = "blue") +
  xlab("Humidity") +
  ylab("Sea.Surface.Temp") +
  ggtitle("Scatterplot of Humidity") +
  geom_smooth(method = "lm",color = "red") +
  geom_smooth(color = "orange")

getMode(buoy2$Humidity)
round(stat.desc(buoy2[,7]),2)

ggplot(buoy2,aes(y=buoy2$Air.Temp)) + 
  geom_boxplot(color="lightblue", notch=TRUE, fill="azure", outlier.color="red", outlier.shape=20,outlier.size=4) +
  ylab("Air.Temp") +
  scale_x_discrete() +
  ggtitle("Boxplot of Humidity")

ggplot(buoy2,aes(x=Air.Temp,y=Sea.Surface.Temp)) + 
  geom_point(color = "blue") +
  xlab("Air.Temp") +
  ylab("Sea.Surface.Temp") +
  ggtitle("Scatterplot of Air.Temp") +
  geom_smooth(method = "lm",color = "red") +
  geom_smooth(color = "orange")

getMode(buoy2$Air.Temp)
round(stat.desc(buoy2[,8]),2)

ggplot(buoy2,aes(y=buoy2$Sea.Surface.Temp)) + 
  geom_boxplot(color="lightblue", notch=TRUE, fill="azure", outlier.color="red", outlier.shape=20,outlier.size=4) +
  ylab("Sea.Surface.Temp") +
  scale_x_discrete() +
  ggtitle("Boxplot of Sea.Surface.")

getMode(buoy2$Sea.Surface.Temp)
round(stat.desc(buoy2[,9]),2)
