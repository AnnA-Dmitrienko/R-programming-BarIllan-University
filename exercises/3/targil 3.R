library(forecast)
library(ggplot2)

setwd("C:/Users/dom/Desktop/r files")

hitex.df<-read.csv("Catalogs.csv")

head(hitex.df)

set.seed(1)  
train.index <- sample(1:1000, 1000*0.6)  
train.df <- hitex.df[train.index, ]
valid.df <- hitex.df[-train.index, ]

ggplot(train.df, aes(x =as.factor( Age), y = AmountSpent)) + 
  geom_bar(stat="identity") +
  ylab("Amount Spent")+xlab("Age")+
  ggtitle("Amount Spent vs. Age")

ggplot(train.df, aes(x =as.factor(Children), y = AmountSpent)) + 
  geom_bar(stat="identity") +
  ylab("Amount Spent")+xlab("Children")+
  ggtitle("Amount Spent vs. Children")


ggplot(train.df, aes(x =as.factor (Catalogs), y = AmountSpent)) + 
  geom_bar(stat="identity") +
  ylab("Amount Spent")+xlab("Catalogs")+
  ggtitle("Amount Spent vs. Catalogs")

ggplot(train.df, aes(x=Salary, y =AmountSpent))+
  geom_point()+ylab("Amount Spent")+xlab("Salary")+
  scale_x_log10()+
  scale_y_log10()+
  ggtitle("Amount Spent vs. Salary")

reg <- lm(log(AmountSpent)~ log (Salary)+as.factor(Age)+
            as.factor(Catalogs)+as.factor(Children)+Gender+Married+Location,data=train.df)
summary(reg)
pred <- predict(reg, newdata = train.df)
accuracy(exp(pred), train.df$AmountSpent)

pred<- predict(reg, newdata = valid.df)
accuracy(exp(pred), valid.df$AmountSpent)







  

