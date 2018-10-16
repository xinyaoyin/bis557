# add new datasets
setwd("~/Desktop/bis557/data-raw")

# read in datasets 
ridge_train <- read.csv("ridge_train.csv")
ridge_test <- read.csv("ridge_test.csv")

# save datasets to data folder
save(ridge_train, file = "../data/ridge_train.rda")
save(ridge_test, file = "../data/ridge_test.rda")