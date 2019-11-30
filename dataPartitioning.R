data = read.csv(file.choose())
str(data)


library(caret)
set.seed(123)

# trainIndex <- createDataPartition(data$Purchased, p=0.7 , list = TRUE)
trainIndex <- createDataPartition(data$Purchased, p=0.7 , list = FALSE)

trainIndex

dataTrain <- data[trainIndex,]
dataTrain


dataTest <- data[-trainIndex,]
dataTest
