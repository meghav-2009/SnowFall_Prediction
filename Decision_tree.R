library(datasets)
library(caTools)
#install.packages("party")
library(party)
library(dplyr)
library(magrittr)
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

setwd("/Users/charan/Documents/Temporary")

Record_DF <- read.csv('Decision_tree_and_Naive_Bayes.csv', stringsAsFactors=TRUE) # all non-numeric columns should be factors
Record_DF <- Record_DF[,-c(1)]

# Handling data imbalance - making sure all the categories in target label are balanced
set.seed(100)
Record_DF_no_snow <- sample_n(Record_DF[Record_DF$snow_or_not == 'no_snow',], 500)
Record_DF_high_snow <- sample_n(Record_DF[Record_DF$snow_or_not == 'chance_of_snow',], 500)

Final_df <- rbind(Record_DF_no_snow, Record_DF_high_snow) # combining data frames

head(Final_df)
str(Final_df)
table(Final_df$snow_or_not)

sample_data = sample.split(Final_df, SplitRatio = 0.8)
train_data <- subset(Final_df, sample_data == TRUE)
test_data <- subset(Final_df, sample_data == FALSE)

# removing labels from test data and storing then in another variable 'TestKnownLabels'
TestKnownLabels <- test_data$snow_or_not
test_data <- test_data[ , -which(names(test_data) %in% c("snow_or_not"))]
head(test_data)

# checking the balance of categories in target labels in training data and testing data
table(train_data$snow_or_not)
table(TestKnownLabels)

fit <- rpart(snow_or_not ~., data = train_data, method = 'class', maxdepth = 2) # all columns and all rows
#fit <- rpart(snow ~., data = train_data, method = 'class', subset = sample(1:nrow(train_data), 20)) # all cols, 20 rows
#fit <- rpart(snow ~ rain + UV, data = train_data, method = 'class') # only rain and UV columns

# In rpart function above -> default split is 'Gini', If we want to use information gain then
#fit <- rpart(snow ~., data = train_data, method = 'class', parms = list(split = 'information'))

rpart.plot(fit, extra = 101) # extra?

predict_unseen <-predict(fit, test_data, type = 'class')

table_mat <- table(TestKnownLabels, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test * 100

write.csv(test_data, '/Users/charan/Documents/Temporary/Decision_tree_testdata.csv', row.names = FALSE)
write.csv(train_data, '/Users/charan/Documents/Temporary/Decision_tree_traindata.csv', row.names = FALSE)
