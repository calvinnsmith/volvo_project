library(tidyverse)


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


#finding outliers from inlier dataset
pred_classes <- numeric(10000)
  for (i in 1:10000){
    pred_classes[i] <- which(one_inlier[i,] == max(one_inlier[i,]))
}
pred_classes <- pred_classes - 1 

c <- pred_classes == true_inlier

inlier <- one_inlier[c == TRUE,]
outlier <- one_inlier[c==FALSE,]




