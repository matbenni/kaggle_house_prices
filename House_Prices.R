setwd("C:/Users/mattb/Desktop/Init/Kaggle/House_Prices/Data")

train <- read.csv('train.csv', header = TRUE, stringsAsFactors = FALSE)
test <- read.csv('test.csv', header = TRUE, stringsAsFactors = FALSE)

cor(train$SalePrice, train$YearBuilt)
plot(train$SalePrice, train$YearBuilt)

plot(train$SalePrice, train$TotRmsAbvGrd)
cor(train$SalePrice, train$TotRmsAbvGrd)


#number of bedrooms, square footage, year built, number of bathrooms, basement yes or no, central AC,
model1 <- lm(SalePrice ~ YearBuilt + TotRmsAbvGrd, data = train)
summary(model1)
