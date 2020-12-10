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

#finding outliers from inlier dataset
#pred_classes <- numeric(10000)
  #for (i in 1:10000){
    #pred_classes[i] <- which(one_inlier[i,] == max(one_inlier[i,]))
#}
#pred_classes <- pred_classes - 1 

#c <- pred_classes == true_inlier

#inlier <- one_inlier[c == TRUE,]
#outlier <- one_inlier[c == FALSE,]

#### Inlier training, validation and testing ####
one_inlier$class <- true_inlier$class
one_inlier <- one_inlier %>% group_by(class) %>% arrange(class)
one_inlier$index <- 1:nrow(one_inlier)

set.seed(123)

train_inlier <- sample_n(one_inlier,600,replace = FALSE)
valid_inlier <- sample_n(one_inlier[-train_inlier$index,],200,replace = FALSE)
test_inlier <- one_inlier[-c(train_inlier$index,valid_inlier$index),]


# check that classes are proportionate
lengths <- numeric(10)
for (i in 1:10){
  lengths[i] <- length(which(one_outlier$class == i-1))
}


#### "classifying outlier dataset" ####

pred_classes <- numeric(10000)
for (i in 1:10000){
  pred_classes[i] <- which(one_outlier[i,] == max(one_outlier[i,]))
}
pred_classes <- pred_classes - 1 

one_outlier$class <- pred_classes
one_outlier <- one_outlier %>% arrange(class)
one_outlier$index <- 1:nrow(one_outlier)


### Proportion of outliers in training data ####

#calculating proportion of classes in outlier data
counts_class <- numeric(10)
for (i in 1:10){
  counts_class[i] <- length(which(one_outlier$class == i-1))
}
prop_class <- counts_class/nrow(one_outlier)

# function that given a proportion "prop", obtains the indices of training, validation
# and testing for the outlier dataset

function_outlier <- function(prop){

#nr of samples to obtain specified % of outliers  
samples_train <- floor(nrow(train_inlier)*prop/(1-prop))
samples_valid <- floor(nrow(valid_inlier)*prop/(1-prop))
samples_test <- floor(nrow(test_inlier)*prop/(1-prop))

index <- vector(mode = "list",length = 3)

for (i in 1:10){
    
    set.seed(1)
    
    class_vec <- one_outlier[which(one_outlier$class == i-1),]
    
    train <- sample_n(class_vec,samples_train*prop_class[i])
    valid <- sample_n(class_vec[-train$index,],samples_valid*prop_class[i])
    test <- sample_n(class_vec[-c(train$index,valid$index),],samples_test*prop_class[i])
    
    index[[1]] <- append(index[[1]],train$index)
    index[[2]] <- append(index[[2]],valid$index)
    index[[3]] <- append(index[[3]],test$index)
    
  }
return(index)
}

outlier_index_1 <- function_outlier(0.003)
outlier_index_2 <- function_outlier(0.01)
outlier_index_3 <- function_outlier(0.05)

train_outlier_1 <- one_outlier[outlier_index_1[[1]],]
valid_outlier_1 <- one_outlier[outlier_index_1[[2]],]
test_outlier_1 <- one_outlier[outlier_index_1[[3]],]

train_outlier_2 <- one_outlier[outlier_index_2[[1]],]
valid_outlier_2 <- one_outlier[outlier_index_2[[2]],]
test_outlier_2 <- one_outlier[outlier_index_2[[3]],]

train_outlier_3 <- one_outlier[outlier_index_3[[1]],]
valid_outlier_3 <- one_outlier[outlier_index_3[[2]],]
test_outlier_3 <- one_outlier[outlier_index_3[[3]],]



#### Final training, validation and testing data ####

X_train_1 <- rbind(train_inlier,train_outlier_1)
X_valid_1 <- rbind(valid_inlier,valid_outlier_1)
X_test_1 <- rbind(test_inlier,test_outlier_1)

X_train_2 <- rbind(train_inlier,train_outlier_2)
X_valid_2 <- rbind(valid_inlier,valid_outlier_2)
X_test_2 <- rbind(test_inlier,test_outlier_2)

X_train_3 <- rbind(train_inlier,train_outlier_3)
X_valid_3 <- rbind(valid_inlier,valid_outlier_3)
X_test_3 <- rbind(test_inlier,test_outlier_3)

write.csv(X_train_1, "train_0003.csv")
write.csv(X_valid_1, "valid_0003.csv")
write.csv(X_test_1, "test_0003.csv")

write.csv(X_train_2, "train_001.csv")
write.csv(X_valid_2, "valid_001.csv")
write.csv(X_test_2, "test_001.csv")

write.csv(X_train_3, "train_005.csv")
write.csv(X_valid_3, "valid_005.csv")
write.csv(X_test_3, "test_005.csv")

