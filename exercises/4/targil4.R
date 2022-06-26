setwd("C:/Users/dom/Desktop/r files")
bicup.df<-read.csv("bicup2006.csv")
head(bicup.df)

library(forecast)
library(lubridate)


bicup.ts <- ts(bicup.df$DEMAND, 
               start = c(1, 1), end = c(21, 63), freq = 63)

View(bicup.ts)
plot(bicup.ts, xlab = "Day of month (March 2005)", ylab = "Number of visitors")



#w/o noise
bicup2.ts <- ts(bicup.df$DEMAND[-c(428:428)], 
               start = c(1, 1), end = c(21, 63), freq = 63)

View(bicup2.ts)
plot(bicup2.ts, xlab = "Day of month (March 2005)", ylab = "Number of visitors")

#fit trend linear
bicup2.lm <- tslm(bicup2.ts ~ trend)
lines(bicup2.lm$fitted, lwd = 0.5, col = "green")
summary(bicup2.lm)
accuracy(bicup2.lm)


#fit trend exponential
bicup2.lm <- tslm(bicup2.ts ~ trend, lambda = 0)
lines(bicup2.lm$fitted, lwd = 0.5, col = "black")
summary(bicup2.lm)
accuracy(bicup2.lm)


nValid <- 189
nTrain <- length(bicup2.ts) - nValid


train.ts <- window(bicup2.ts, start = c(1, 1), end = c(1, nTrain))
valid.ts <- window(bicup2.ts, start = c(1, nTrain+1), end = c(1, nTrain + nValid))

train.lm <- tslm(train.ts ~ trend)
summary(train.lm)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)

plot(train.lm.pred, ylab = "Demand", xlab = "Day of month (March 2005)", main = "Forecast from Linear Model")
lines(train.lm$fitted, lwd = 0.5, col="pink")
lines(valid.ts, col = "blue")

accuracy(train.ts, train.lm$fitted) #training
accuracy(valid.ts, train.lm.pred$mean) #validation

#fit seasonality
bicup2.lm <- tslm(bicup2.ts ~ trend+season)
lines(bicup2.lm$fitted, lwd = 0.5, col = "red")


nValid <- 189
nTrain <- length(bicup2.ts) - nValid


train.ts <- window(bicup2.ts, start = c(1, 1), end = c(1, nTrain))
valid.ts <- window(bicup2.ts, start = c(1, nTrain+1), end = c(1, nTrain + nValid))

train.lm <- tslm(train.ts ~ trend+season)
summary(train.lm)
train.lm.pred <- forecast(train.lm, h = nValid, level = 0)

plot(train.lm.pred, ylab = "Demand", xlab = "Day of month (March 2005)", main = "Forecast from Linear Model")
lines(train.lm$fitted, lwd = 0.5, col="pink")
lines(valid.ts, col = "blue")

accuracy(train.ts, train.lm$fitted) #training
accuracy(valid.ts, train.lm.pred$mean) #validation


