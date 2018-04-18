# Load Libraries
library(ggplot2)
library(corrplot)
library(mice)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)
library(caret)
library(VIM)

# Load data into train and test splits
house_train <- read.csv('train.csv')
house_test <- read.csv('test.csv')

# ---

# Change factors to integers
for(i in 1:81){
  if(is.factor(house_train[, i])){
    house_train[, i] <- as.integer(house_train[, i])
  }
}

# -----------------------------------------------------------------------------

# Dealing with missing values

# Count number of NA values in each variable
count_na <- sapply(house_train, function(x) length(which(is.na(x) == TRUE)))
na_df <- data.frame(Count = count_na)

na_df

# PoolQC, Fence need to have NA replaced with 0 since so many homes do not have a Fence or Pool

house_train$PoolQC[is.na(house_train$PoolQC)] <- 0
house_train$Fence[is.na(house_train$Fence)] <- 0
house_train$MiscFeature[is.na(house_train$MiscFeature)] <- 0
house_train$Alley[is.na(house_train$Alley)] <- 0

# View missingness after cleanup
aggr(house_train, col = c("steelblue", "darkred"), numbers = TRUE, sortVars = TRUE)

# Run mice algorithm - set.seed = 456
incomplete_mice <- mice(house_train, m = 5, method = "pmm", maxit = 50, seed = 456)
summary(incomplete_mice)

# Complete mice algorithm and store data
completed_data <- complete(incomplete_mice, 1)
training <- setdiff(names(completed_data), c(ID.VAR,TARGET.VAR))
response <- completed_data$SalePrice
training <- completed_data[training]

# training - updated data table

# Run Boruta algorithm and store it, set seed 567
set.seed(567)
boruta_outcome <- Boruta(training, response, maxRuns = 101, doTrace = 0)

# View the attributes selected by the Boruta algorithm sorted by status
sel_attributes <- setDT(arrange(cbind(attr = rownames(attStats(boruta_outcome)), attStats(boruta_outcome)),desc(normHits)))
confirmed_attributes <- sel_attributes[sel_attributes[, decision == "Confirmed"], attr]

training <- training[confirmed_attributes]





