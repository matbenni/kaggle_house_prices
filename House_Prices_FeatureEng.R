# Load Libraries
library(ggplot2) # for data visualization
library(stringr) #extracting string patterns
library(Matrix) # matrix transformations
library(glmnet) # ridge, lasso & elastinet
library(xgboost) # gbm
library(randomForest)
library(Metrics) # rmse
library(caret) # one hot encoding
library(scales) # plotting $$
library(e1071) # skewness
library(corrplot) # correlation plot
library(data.table)
library(dplyr) # load this in last so plyr doens't overlap it
library(forcats) # help with plotting

# Load data into train and test splits
house_train <- read.csv('train.csv')
house_test <- read.csv('test.csv')
# Train dimensions
dim(house_train)
# Test dimensions
dim(house_test)

combined_df <- rbind(within(house_train, rm("SalePrice")), house_test)
dim(combined_df)

# setDT for manipulation and eda
setDT(combined_df)

# Check missing values sort descending
na_vars <- which(colSums(is.na(combined_df)) > 0)
sort(colSums(sapply(combined_df[, ..na_vars], is.na)), decreasing = TRUE)

# Check for those with pool area but no PoolQC
combined_df[is.na(PoolQC) & PoolArea > 0, c("PoolQC", "PoolArea")]
combined_df[is.na(PoolQC) & PoolArea > 0, which = TRUE]
# These three values need to be changed to "None" 2421, 2504, 2600

# Calculate average area by PoolQC level and the number of each
combined_df[, list(Average = mean(PoolArea), Count = .N), by = PoolQC]

# Now we can assign a level to the NA's based on how close they are to the means
combined_df[2421, "PoolQC"] <- "Ex"
combined_df[2504, "PoolQC"] <- "Ex"
combined_df[2600, "PoolQC"] <- "Fa"
combined_df[is.na(PoolQC) == TRUE, "PoolQC"] <- "None"
summary(combined_df[, "PoolQC"])


# Garage Variables
garage_names <- names(combined_df[, like(names(combined_df), "Gar"), with = FALSE])
garage_names
combined_df[GarageYrBlt == YearBuilt, .N]
combined_df[is.na(GarageYrBlt), .N]
combined_df[GarageYrBlt != YearBuilt, .N]

# Since the majority were built at the same time as the home, we will replace
# the NA's with the YearBuilt
combined_df[is.na(GarageYrBlt), "GarageYrBlt" := "YearBuilt"] #<- combined_df[is.na(GarageYrBlt), "GarageYrBlt" := "YearBuilt"]

# Check logicals/common sense levels of Garage Vars

# Assuming most people build detached garages after the home is built, the number of
# detached garages should be somewhat close to the number of homes with garages built
# at a time different than that of the home
combined_df[GarageType == "Detchd", .N]

# Check if there are garages that are 0 cars and have an area of 0 but have other attributes
View(combined_df[is.na(GarageCond), garage_names, with = FALSE])
View(combined_df[is.na(GarageQual), garage_names, with = FALSE])
View(combined_df[is.na(GarageFinish), garage_names, with = FALSE])
# So 157 of our homes show 0 cars and 0 area.  Therefore, we can assume they have no garage at all
# But we have 1 home with NA for Quality, Finish, and Condition, but had an area greater than 0
# We can replace its values manually but for the home with the 360 and 1 values
# we can replace its values with the most common for each column
combined_df[GarageCars == 1 & GarageArea == 360 & is.na(GarageQual), which = TRUE]
# So observation 2127 is the observation we will manually change the values for
# Get most common values for GarageQual, GarageCond, GarageFinish
combined_df[2127, "GarageQual" := names(which.max(table(combined_df[, "GarageQual"])))]
combined_df[2127, "GarageCond" := names(which.max(table(combined_df[, "GarageCond"])))]
combined_df[2127, "GarageFinish" := names(which.max(table(combined_df[, "GarageFinish"])))]

# Get numeric garage variables
garage_num_vars <- names(which(sapply(combined_df[, garage_names, with = FALSE], is.numeric)))
garage_cat_vars <- names(which(sapply(combined_df[, garage_names, with = FALSE], is.factor)))

# If numeric, change to zero, else change to None
for (g in garage_num_vars) {
  set(combined_df, i = which(is.na(combined_df[[g]])), j = g, value = 0)
}
for (h in garage_cat_vars) {
  set(combined_df, i = which(is.na(combined_df[[h]])), j = h, value = "None")
}

# Kitchen - KitchenQual, KitchenAbvGr
kitchen_names <- names(combined_df[, like(names(combined_df), "Kitchen"), with = FALSE])
combined_df[is.na(KitchenQual), kitchen_names, with = FALSE]

# How many NA's do we have remaining?
count_na <- sapply(combined_df, function(x) length(which(is.na(x) == TRUE)))
na_df <- data.frame(Count = count_na)
na_df

# Only 1 NA value in KitchenQual
frq_kitchen <- names(which.max(table(combined_df[, "KitchenQual"])))
ggplot(combined_df) + geom_bar(aes(x = fct_infreq(KitchenQual)))
# So KitchenQual = TA is the most frequent - replace the NA with TA
combined_df[is.na(KitchenQual), "KitchenQual" := frq_kitchen]

# Electrical NA
electrical_names <- names(combined_df[, like(names(combined_df), "Elec"), with = FALSE])
frq_electrical <- names(which.max(table(combined_df[, "Electrical"])))
ggplot(combined_df) + geom_bar(aes(x = fct_infreq(Electrical)))
# Change NA value in Electrical to most frequent - SBrkr
combined_df[is.na(Electrical), "Electrical" := frq_electrical]

# Check which basement variables have NA's
basement_names <- names(combined_df[, like(names(combined_df), "Bsmt"), with = FALSE])
sapply(combined_df[, basement_names, with = FALSE], function(x) length(which(is.na(x) == TRUE)))
# Check most frequent BsmtExposure level
frq_bsmtexposure <- names(which.max(table(combined_df[, "BsmtExposure"])))
ggplot(combined_df) + geom_bar(aes(x = fct_infreq(BsmtExposure)))
# combined_df[is.na(BsmtExposure), "BsmtExposure" := frq_bsmtexposure]
table(combined_df[, "BsmtExposure"])
View(combined_df[is.na(BsmtExposure), c("Id", basement_names), with = FALSE])
# Observation 949, 1488, 2349 missing BsmtExposure - replace with most freq BsmtExposure
combined_df[c(949, 1488, 2349), "BsmtExposure" := "No"]

# Get numeric and categorical basement variables
basement_num_vars <- names(which(sapply(combined_df[, basement_names, with = FALSE], is.numeric)))
basement_cat_vars <- names(which(sapply(combined_df[, basement_names, with = FALSE], is.factor)))

for (b in basement_num_vars) {
  set(combined_df, i = which(is.na(combined_df[[b]])), j = b, value = 0)
}
for (c in basement_cat_vars) {
  set(combined_df, i = which(is.na(combined_df[[c]])), j = c, value = "None")
}
summary(combined_df[, basement_names, with = FALSE])
