setwd("C:/Users/dom/Desktop/r files")

library(rpart)
library(rpart.plot)

hitex.df<-read.csv("Catalogs.csv")

head(hitex.df)

set.seed(1)  
train.index <- sample(1:1000, 1000*0.6)  
train.df <- hitex.df[train.index, ]
valid.df <- hitex.df[-train.index, ]

tr <- ctree(AmountSpent ~ ., data = train.df)
plot(tr, type = "simple")


tr_new <- ctree(log(AmountSpent)~ log (Salary)+as.factor(Age)+
            as.factor(Catalogs)+as.factor(Children)+Gender+Married+Location,data=train.df)
plot(tr_new, type = "simple")


pred1 <- predict(tr, newdata = valid.df)
View(pred1)
RMSE.rtree <- sqrt(mean((pred1-valid.df$AmountSpent)^2))
View(RMSE.rtree)

pred2 <- predict(tr_new, newdata = valid.df)
View(pred2)
RMSE.rtree2 <- sqrt(mean((exp(pred2)-valid.df$AmountSpent)^2))
View(RMSE.rtree2)
