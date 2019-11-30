marathon_data

dim(marathon_data)

str(marathon_data)

attach(marathon_data)
nrow(marathon_data[marathon_data$finished_race == "Yes",]) / nrow(marathon_data)

nrow(marathon_data[marathon_data$finished_race == "No",]) / nrow(marathon_data)



marathon_data$charity <- as.factor(marathon_data$charity)
marathon_data$finished_race <- ifelse(marathon_data$finished_race == "Yes" , 1 , 0)

mean(marathon_data$age)





marathon_dataIndex <-  createDataPartition(marathon_data$finished_race, p=0.5 , list = FALSE)

marathon_dataIndex


trainMarathon <- marathon_data[marathon_dataIndex,]
nrow(trainMarathon)
testMarathon <- marathon_data[-marathon_dataIndex,]
nrow(testMarathon)

trainMarathon[,-4]
testMarathon[,-4]


nrow(testMarathon)

names(marathon_data)
modellm <- glm(trainMarathon$finished_race ~ . , data = trainMarathon[,-4] , family = binomial)

vif(modellm)

logitMod <- glm(trainMarathon$finished_race ~ months_trained + age + charity , data = trainMarathon[,-4] , family = binomial(link="logit"))

#logitMod <- glm(finished_race ~ months_trained + age + charity, data= trainMarathon[,-4]  , family=binomial(link="logit"))

vif(logitMod)

predicted <- plogis(predict(logitMod, testMarathon)) 




summary(modellm)
summary(logitMod)


lrtest(modellm)

lrtest(logitMod)

library(pscl)

pR2(modellm)
pR2(logitMod)

nrow(trainMarathon)


predictionTrain_Marathon <- predict(modellm,type = "response" , data = trainMarathon)
predictionTrain_Marathon
class(predictionTrain_Marathon)

predictionTrain_Marathon

cutoff_Marathon = floor(predictionTrain_Marathon+0.7)
cutoff_Marathon


confmatTrain_Marathon = table(Predicted=cutoff_Marathon,Actual=trainMarathon$finished_race)
confmatTrain_Marathon

#6 Precision, Recall & F1 Score
#install.packages("caret")
library(caret)

confusionMatrix(confmatTrain_Marathon,positive="1",mode="everything")

#Predict for test Data
nrow(testMarathon)
predictionTest_Marathon <- predict(logitMod, newdata = testMarathon, type = "response")
predictionTest_Marathon

cutoff_Marathon = floor(predictionTest_Marathon+0.7)
cutoff_Marathon


confmatTest_Marathon = table(Predicted=cutoff_Marathon,Actual= testMarathon$finished_race)
confmatTest_Marathon

confusionMatrix(confmatTest_Marathon,positive="1",mode="everything")
#7 ROC Curve
install.packages("pROC")
library(pROC)

#plot.roc(data$Purchased,prediction)

plot.roc(trainMarathon$finished_race, predictionTrain_Marathon)

# auc(data$Purchased , prediction)

auc(trainMarathon$finished_race, predictionTrain_Marathon)

auc(testMarathon$finished_race, predictionTest_Marathon)

