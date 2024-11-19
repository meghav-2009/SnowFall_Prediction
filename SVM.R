library(datasets)
library(caTools)
#install.packages("party")
library(party)
library(dplyr)
library(magrittr)
#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(e1071)

setwd("/Users/charan/Documents/Temporary")

Record_DF <- read.csv('SVM.csv', stringsAsFactors=TRUE)
Record_DF <- Record_DF[,-c(1)]

set.seed(100)
Record_DF_no_snow <- sample_n(Record_DF[Record_DF$snow_or_not == 'no_snow',], 100)
Record_DF_high_snow <- sample_n(Record_DF[Record_DF$snow_or_not == 'chance_of_snow',], 150)

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

svm_fit <- svm(snow_or_not ~ ., data = train_data, kernel = 'linear', cost = 85)
pred <- predict(svm_fit, test_data, type = 'snow_or_not')
plot(svm_fit, data = train_data, Cloud ~ Feels_like_c)

table(pred, TestKnownLabels)
