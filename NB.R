#install.packages('naivebayes')
library(naivebayes)
library(dplyr)
library(datasets)
library(caTools)
library(e1071)

setwd("/Users/charan/Documents/Temporary")

Record_DF <- read.csv('Decision_tree_and_Naive_Bayes.csv', stringsAsFactors=TRUE) # all non-numeric columns should be factors
Record_DF <- Record_DF[,-c(1)]

# Handling data imbalance - making sure all the categories in target label are balanced
set.seed(100)
Record_DF_no_snow <- sample_n(Record_DF[Record_DF$snow_or_not == 'no_snow',], 500)
Record_DF_high_snow <- sample_n(Record_DF[Record_DF$snow_or_not == 'chance_of_snow',], 500)

Final_df <- rbind(Record_DF_no_snow, Record_DF_high_snow) # combining data frames


sample_data = sample.split(Final_df, SplitRatio = 0.8)
train_data <- subset(Final_df, sample_data == TRUE)
test_data <- subset(Final_df, sample_data == FALSE)

# removing labels and storing them seperately for both training and testing data
TrainKnownLabels <- train_data$snow_or_not
train_data <- train_data[ , -which(names(train_data) %in% c("snow_or_not"))]
TestKnownLabels <- test_data$snow_or_not
test_data <- test_data[ , -which(names(test_data) %in% c("snow_or_not"))]

(model_NB <- naiveBayes(train_data, TrainKnownLabels, laplace = 1))
predictions <- predict(model_NB, test_data)

tab_NB <- table(predictions,TestKnownLabels)
tab_NB

accuracy_Test <- sum(diag(tab_NB)) / sum(tab_NB)
accuracy_Test * 100

train_data
test_data

write.csv(test_data, '/Users/charan/Documents/Temporary/Naive_Bayes_testdata.csv', row.names = FALSE)
write.csv(train_data, '/Users/charan/Documents/Temporary/Naive_Bayes_traindata.csv', row.names = FALSE)
