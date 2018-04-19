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
library(randomForest)

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

for(i in 1:80){
  if(is.factor(house_test[, i])){
    house_test[, i] <- as.integer(house_test[, i])
  }
}

# ---

# Dealing with missing values

# Count number of NA values in each variable
count_na <- sapply(house_train, function(x) length(which(is.na(x) == TRUE)))
count_na_test <- sapply(house_test, function(x) length(which(is.na(x) == TRUE)))
na_df <- data.frame(Count = count_na)
na_df_test <- data.frame(Count = count_na_test)

na_df
na_df_test

# PoolQC, Fence need to have NA replaced with 0 since so many homes do not have a Fence or Pool

house_train$PoolQC[is.na(house_train$PoolQC)] <- 0
house_train$Fence[is.na(house_train$Fence)] <- 0
house_train$MiscFeature[is.na(house_train$MiscFeature)] <- 0
house_train$Alley[is.na(house_train$Alley)] <- 0

house_test$PoolQC[is.na(house_test$PoolQC)] <- 0
house_test$Fence[is.na(house_test$Fence)] <- 0
house_test$MiscFeature[is.na(house_test$MiscFeature)] <- 0
house_test$Alley[is.na(house_test$Alley)] <- 0

# View missingness after cleanup
aggr(house_train, col = c("steelblue", "darkred"), numbers = TRUE, sortVars = TRUE)
aggr(house_test, col = c("steelblue", "darkred"), numbers = TRUE, sortVars = TRUE)

# Run mice algorithm - set.seed = 456
incomplete_mice <- mice(house_train, m = 5, method = "pmm", maxit = 50, seed = 456)
summary(incomplete_mice)
incomplete_mice_test <- mice(house_test, m = 5, method = "pmm", maxit = 50, seed = 456)

# Complete mice algorithm and store data
completed_data <- complete(incomplete_mice, 1)
completed_data_test <- complete(incomplete_mice_test, 1)
training <- setdiff(names(completed_data), c(ID.VAR,TARGET.VAR))
response <- completed_data$SalePrice
training <- completed_data[training]

# Run Boruta algorithm and store it, set seed 567
set.seed(567)
boruta_outcome <- Boruta(training, response, maxRuns = 101, doTrace = 0)

# View the attributes selected by the Boruta algorithm sorted by status
sel_attributes <- setDT(arrange(cbind(attr = rownames(attStats(boruta_outcome)), attStats(boruta_outcome)),desc(normHits)))
confirmed_attributes <- sel_attributes[sel_attributes[, decision == "Confirmed"], attr]

training <- training[confirmed_attributes]

# Set a random seed
set.seed(789)

# Build the model: randomForest
model_rf <- randomForest(response ~ ., data = training)

# Get importance
importance    <- importance(model_rf)
varImportance <- data.frame(Variables = row.names(importance), Importance = importance[ ,'IncNodePurity'])

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

# Make prediction
predict_prices <- predict(model_rf, completed_data_test)
predict_prices
predicted <- cbind(Id = house_test$Id, SalePrice = predict_prices)

write.csv(predicted, file = "model_rf_solution.csv", row.names = FALSE)


