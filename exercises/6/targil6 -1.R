setwd("C:/Users/annar/Desktop/r files")

bank.df <- read.csv("UniversalBank.csv")
head(bank.df)
bank.df <- bank.df[ , -c(1, 5)] # Drop ID and zip code columns.

### split the data
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

library(rpart)
library(rpart.plot)


library(party)
tr <- ctree(Personal.Loan ~ ., data = train.df)
plot(tr, type = "simple")
pred <- predict(tr, newdata = valid.df)

library(AUC)
r <- roc(pred, as.factor(valid.df$Personal.Loan))
auc(r)
plot(r)

library(forecast)

ComputeAUC <- function(seed){
  set.seed(seed)  
  train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
  train.df <- bank.df[train.index, ]
  valid.df <- bank.df[-train.index, ]
  
 #fit CI tree 
  tr <- ctree(Personal.Loan ~ ., data = train.df)
  
  pred <- predict(tr, newdata = valid.df)
  r <- roc(pred, as.factor(valid.df$Personal.Loan))
return(auc(r))  

}

t1 <- proc.time()
auc_arr = c()
for (seed in c(1:1000)){
  auc_arr <- c(auc_arr, ComputeAUC(seed))
}
t2 <- proc.time()
t2-t1
boxplot(auc_arr)

### do the same - in parallel
library(doParallel)
# find how many cores are available
detectCores()

# create cluster with deired number of cores
cl <- makeCluster(3)
# register cluster
registerDoParallel(cl)
# find how many cores are being userd
getDoParWorkers()

library(foreach)
t1 <- proc.time()
auc_arr <- foreach(seed = 1:1000, .combine = "c", .packages = "forecast") %dopar% ComputeAUC(seed)
t2 <- proc.time()
t2-t1
boxplot(auc_arr)


