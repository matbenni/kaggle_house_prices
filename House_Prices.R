setwd("C:/Users/mattb/Desktop/Init/Programming/Data Science/Kaggle/House_Prices/Data")

# Load Libraries
library(ggplot2)
library(data.table)
library(corrplot)
library(mice)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)
library(caret)

# Load data into train and test splits
house_train <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)
house_test <- read.csv('test.csv', header = TRUE, stringsAsFactors = FALSE)

# Set the ID variable and Target Variable
ID.VAR <- "Id"
TARGET.VAR <- "SalePrice"

# Convert the data frames to data tables for manipulation/visualization
setDT(house_train)
setDT(house_test)

# Count number of NA values in each variable
vars_with_na <- names(colSums(is.na(house_train))[colSums(is.na(house_train)) > 0])

# Grab categorical variables
cat_vars_train <- names(house_train)[which(sapply(house_train, is.character))]

# Grab numeric variables
num_vars_train <- names(house_train)[which(sapply(house_train, is.numeric))]

# Create new data frame without the id and target vars
candidate_features <- setdiff(names(house_train), c(ID.VAR, TARGET.VAR))

data_type <- sapply(candidate_features, function(x) {class(house_train[[x]])})







