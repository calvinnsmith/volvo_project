library(tidyverse)
library(dplyr)

### Data Preparation ####
classes <- seq(0,9)

# inlier
one_inlier <- read.csv("network_1_inlier_activation_vectors.csv", sep = "," , header = F)
colnames(one_inlier) <- classes
# outlier
one_outlier <-  read.csv("network_1_outlier_activation_vectors.csv", sep = "," , header = F)
colnames(one_outlier) <- classes

# true classes inlier set
true_inlier <- read.csv("inlier_true_labels.csv", sep = ",", header = F)
colnames(true_inlier) <- "class"

one_inlier$class <- true_inlier$class

#### "classifying outlier dataset" ####
pred_classes <- numeric(10000)
for (i in 1:10000){
  pred_classes[i] <- which(one_outlier[i,1:10] == max(one_outlier[i,1:10]))
}
pred_classes <- pred_classes - 1 

one_outlier$class <- pred_classes

#### INLIER ####

## classifying IN_IN and IN_OUT
pred_classes <- numeric(10000)
for (i in 1:10000){
  pred_classes[i] <- which(one_inlier[i,1:10] == max(one_inlier[i,1:10]))
}
pred_classes <- pred_classes - 1 

c <- pred_classes == true_inlier

one_inlier$inlier <- 1
one_inlier$inlier[c != TRUE] <- 0

one_inlier$type <- "x_in"
one_inlier$type[c != TRUE] <- "x_miss"

index <- 1:10000
one_inlier$index <- index
one_inlier <- one_inlier %>% group_by(class)
train_inlier <- sample_n(one_inlier,600)
valid_inlier <- sample_n(one_inlier[-train_inlier$index,],200)
test_inlier <- one_inlier[-c(train_inlier$index,valid_inlier$index),]


#### OUTLIER ####
one_outlier$inlier <- 0
one_outlier$type <- "x_out"
one_outlier$index <- index
one_outlier <- one_outlier %>% group_by(class)
train_outlier <- sample_frac(one_outlier,0.6)
valid_outlier <- sample_frac(one_outlier[-train_outlier$index,],0.3)
test_outlier <- sample_frac(one_outlier[-c(train_outlier$index,valid_outlier$index),],3/7)
  



### FINAL DATASETS ####

X_train <- rbind(train_inlier,train_outlier)
X_valid <- rbind(valid_inlier,valid_outlier)
X_test <- rbind(test_inlier,test_outlier)

x_valid <- X_valid %>% group_by(class,inlier) %>% sample_n(20)
x_test <- X_test %>% group_by(class,inlier) %>% sample_n(20)

write.csv(X_train,"X_train.csv")
write.csv(X_valid,"X_valid.csv")
write.csv(X_test,"X_test.csv")

write.csv(x_valid,"x_valid.csv")
write.csv(x_test,"x_test.csv")









lengths <- numeric(10)
for (i in 1:10){
  lengths[i] <- length(which(x_valid$class == i-1))
}
prop1 <- lengths/6000






