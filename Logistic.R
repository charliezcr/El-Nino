## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(gains)
library(caret)
library(ROCR)


## ------------------------------------------------------------------------
#get buoy 2 and 48
new_elnino <- read.csv("new_elnino.csv")
buoy2 <- new_elnino %>% filter(buoy==2)
buoy48 <- new_elnino %>% filter((buoy==48))
buoy248 <- rbind(buoy2,buoy48)
#choose only the selected variables
buoy248 <- buoy248 %>% select(Zonal.Winds,Meridional.Winds,Humidity,Air.Temp,Sea.Surface.Temp,buoy)
#change buoy2 as 1 and buoy 48 as 0
buoy248$buoy[buoy248$buoy==2] <- 1
buoy248$buoy[buoy248$buoy==48] <- 0
buoy248$Sea.Surface.Temp <- as.double(buoy248$Sea.Surface.Temp)


## ------------------------------------------------------------------------
#get training and testing data
set.seed(9)
trainindex <- sample(c(dim(buoy248)[1]),dim(buoy248)[1]*0.7)
train <- buoy248[trainindex,]
valid <- buoy248[-trainindex,]


## ------------------------------------------------------------------------
#logistic regression
logit2 <- glm(buoy~Sea.Surface.Temp+Meridional.Winds+Zonal.Winds:Humidity+Zonal.Winds:Air.Temp+Meridional.Winds:Humidity+Meridional.Winds:Air.Temp+Humidity:Air.Temp,data = train,family = binomial(link = "logit"))
options(scipen=999)
summary(logit2)


## ------------------------------------------------------------------------
#prediction
logit2pred <- predict(logit2, valid[,-6], type = "response")
predict <- ifelse(logit2pred> 0.5, "1", "0")
data.frame(actual = valid$buoy[100:110],predicted = predict[100:110])
confusionMatrix(as.factor(predict), as.factor(valid$buoy))


## ------------------------------------------------------------------------
#ROC
ROCRpred <- prediction(logit2pred, valid$buoy)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

