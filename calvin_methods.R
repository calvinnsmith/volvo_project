#library(fpc)
library(dbscan)
library(tidyverse)
library(dplyr)
library(factoextra)
library(caret)
library(ROCR)
library(rlist)


classes <- seq(0,9)

labels <- c(1,0)


### training data ####
train_1_0003 <- read.csv("train_1_0003.csv")
colnames(train_1_0003)[2:11] <- classes

train_1_001 <- read.csv("train_1_001.csv")
colnames(train_1_001)[2:11] <- classes

train_1_005 <- read.csv("train_1_005.csv")
colnames(train_1_005)[2:11] <- classes



### validation data ####
valid_1_0003 <- read.csv("valid_1_0003.csv")
colnames(valid_1_0003)[2:11] <- classes

valid_1_001 <- read.csv("valid_1_001.csv")
colnames(valid_1_001)[2:11] <- classes

valid_1_005 <- read.csv("valid_1_005.csv")
colnames(valid_1_005)[2:11] <- classes




plot(train_1_005[,2],train_1_005[,3])



#### DBSCAN Classwise #####

#choose training data
train_data <- train_1_005[1:6000,]

#choose validation data
valid_data <- valid_1_005


# label data as inlier = 1 and outlier = 0
for (i in 1:nrow(train_data)){
  if (max.col(train_data[i,2:11])-1==train_data[i,12]){
    train_data$inlier[i] <- 1 }
  else {
    train_data$inlier[i] <- 0
  }
  
}

train_data$inlier[6001:nrow(train_data)] <- 0


for (i in 1:nrow(valid_data)){
  if (max.col(valid_data[i,2:11])-1==valid_data[i,12]){
    valid_data$inlier[i] <- 1 }
  else {
    valid_data$inlier[i] <- 0
  }
  
}

valid_data$inlier[2001:nrow(valid_data)] <- 0


# finding optimal classwise eps and minpts

eps <- seq(0.001,1.0,length.out = 30)
minpts <- floor(seq(10,100, length.out = 5))

#try different training data
#train_data <- subset(train_data, inlier == 1)

valid_data <- valid_data %>% group_by(class,inlier)
valid_data <- sample_n(valid_data,size = 20)

auc_class <- numeric(length(10))
auc_index <- list(length(10))

for (i in 1:10){
  train <- subset(train_data, class == i-1)
  valid <- subset(valid_data, class == i-1)
  auc_matrix <- matrix(data = NA, nrow = length(eps), ncol = length(minpts))
  for (j in 1:length(eps)){
    for( k in 1:length(minpts)){
      set.seed(123)
      db <- dbscan(train[,2:11], eps = eps[j], minPts = minpts[k])
      pred <- predict(db, newdata = valid[,2:11], data = train[,2:11])
      pred[which(pred != 0)] <- 1
      
      conf <- confusionMatrix(as.factor(pred),as.factor(valid$inlier))
      
      print(conf$table)
      
      #AUROC
      predi <- prediction(pred, valid$inlier)
      perf <- performance(predi,"auc")
      
      auc_matrix[j,k] <- perf@y.values[[1]][1]
      
     
    }
  }
  auc_class[i] <- max(auc_matrix)
  auc_index[[i]] <- which(auc_matrix == max(auc_matrix), arr.ind = TRUE)
}

### Optimal Eps and Minpts for each class ###

optimal <- list(length(10))

for (i in 1:10){
  opt <- auc_index[[i]]
  eps_minpts <- matrix(data = NA, nrow = nrow(opt), ncol = 2)
  
  for (j in 1:nrow(opt)){
    
    optimum <- opt[j,]
    
    eps_minpts[j,] <- c(eps[optimum[1]],minpts[optimum[2]])
    
  }
  optimal[[i]] <- eps_minpts
}



db <- dbscan(train[,2:11], eps = 0.27, minPts = 55)
pred <- predict(db, newdata = valid[,2:11], data = train[,2:11])





