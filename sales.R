# logistic regression

data = read.csv(file.choose())
data
str(data)

library(caret)
set.seed(123)

# trainIndex <- createDataPartition(data$Purchased, p=0.7 , list = TRUE)
trainIndex <- createDataPartition(data$Purchased, p=0.7 , list = FALSE)

head(trainIndex)

dataTrain <- data[trainIndex,]
dataTrain

nrow(dataTrain)

dataTest <- data[-trainIndex,]
dataTest

logit = glm(Purchased ~ ., data = dataTrain[,-1],family=binomial)

summary(logit)




#1 Identify overall fitness of model using log likehood Ratio Test
install.packages("lmtest")
library(lmtest)

lrtest(logit)

#2 Calculating McFadden's Rsquared (Minimum McFadden Rsquared considered for model fitness is 10%)
#install.packages("pscl")
library(pscl)
pR2(logit)

#47.11% of the uncertainty in the intercept only model is explained by the full model

#3 Coefficients importance
summary(logit)

# Product and shopping are insignificant in the model
#AIC = -2logLikelihood +2k = -2 * (-137.922) +2*4 = 283.844
#Lower the AIC, better the model - Used for multicomparison of models with different variables



#4 Explanatory power of odds and probability
exp(coef(logit))


#5 Building Confusion Matrix
#Predict for Train Data
#0.5


nrow(dataTrain)

predictionTrain <- predict(logit,type = "response")
predictionTrain

cutoff = floor(predictionTrain+0.7)
cutoff

confmatTrain = table(Predicted=cutoff,Actual=dataTrain$Purchased)
confmatTrain

#6 Precision, Recall & F1 Score
install.packages("caret")
library(caret)

confusionMatrix(confmatTrain,positive="1",mode="everything")

#Predict for test Data
nrow(dataTest)
predictionTest <- predict(logit, newdata = dataTest, type = "response")
predictionTest

cutoff = floor(predictionTest+0.7)
cutoff


confmatTest = table(Predicted=cutoff,Actual=dataTest$Purchased)
confmatTest

confusionMatrix(confmatTest,positive="1",mode="everything")
#7 ROC Curve
install.packages("pROC")
library(pROC)

#plot.roc(data$Purchased,prediction)

plot.roc(dataTrain$Purchased, predictionTrain)

# auc(data$Purchased , prediction)

auc(dataTrain$Purchased, predictionTrain)

auc(dataTest$Purchased, predictionTest)

nrow(data[,1])

length(data$Purchased)


length(prediction)


#*******************************************************
# for original complete Dataset
# Cutoff = 0.5 

# Accuracy : 0.8525          
# 95% CI : (0.8139, 0.8858)
# No Information Rate : 0.6425          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.6692          
# 
# Mcnemar's Test P-Value : 0.01911         
#                                           
#             Sensitivity : 0.7273          
#             Specificity : 0.9222          
#          Pos Pred Value : 0.8387          
#          Neg Pred Value : 0.8587          
#               Precision : 0.8387          
#                  Recall : 0.7273          
#                      F1 : 0.7790          
#              Prevalence : 0.3575          
#          Detection Rate : 0.2600          
#    Detection Prevalence : 0.3100          
#       Balanced Accuracy : 0.8247          
#                                           
#        'Positive' Class : 1             


# cutoff = 0.4
# 
# Accuracy : 0.8525          
# 95% CI : (0.8139, 0.8858)
# No Information Rate : 0.6425          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.6814          
# 
# Mcnemar's Test P-Value : 0.6025          
#                                           
#             Sensitivity : 0.8112          
#             Specificity : 0.8755          
#          Pos Pred Value : 0.7838          
#          Neg Pred Value : 0.8929          
#               Precision : 0.7838          
#                  Recall : 0.8112          
#                      F1 : 0.7973          
#              Prevalence : 0.3575          
#          Detection Rate : 0.2900          
#    Detection Prevalence : 0.3700          
#       Balanced Accuracy : 0.8433          
#                                           
#        'Positive' Class : 1               
#                                  
#        
#*************************************************       

#******************************************************

# Confusion Matrrix on Train Data
# 
# Actual
# Predicted   0   1
# 0 163  26
# 1  14  77
# 
# Accuracy : 0.8571          
# 95% CI : (0.8106, 0.8959)
# No Information Rate : 0.6321          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.6852          
# 
# Mcnemar's Test P-Value : 0.08199         
#                                           
#             Sensitivity : 0.7476          
#             Specificity : 0.9209          
#          Pos Pred Value : 0.8462          
#          Neg Pred Value : 0.8624          
#               Precision : 0.8462          
#                  Recall : 0.7476          
#                      F1 : 0.7938          
#              Prevalence : 0.3679          
#          Detection Rate : 0.2750          
#    Detection Prevalence : 0.3250          
#       Balanced Accuracy : 0.8342          
#                                           
# confusion Matrix on Test Data
# Actual
# Predicted  0  1
# 0 73 10
# 1  7 30
# 
# Accuracy : 0.8583          
# 95% CI : (0.7829, 0.9153)
# No Information Rate : 0.6667          
# P-Value [Acc > NIR] : 1.569e-06       
# 
# Kappa : 0.6752          
# 
# Mcnemar's Test P-Value : 0.6276          
#                                           
#             Sensitivity : 0.7500          
#             Specificity : 0.9125          
#          Pos Pred Value : 0.8108          
#          Neg Pred Value : 0.8795          
#               Precision : 0.8108          
#                  Recall : 0.7500          
#                      F1 : 0.7792          
#              Prevalence : 0.3333          
#          Detection Rate : 0.2500          
#    Detection Prevalence : 0.3083          
#       Balanced Accuracy : 0.8313          
#                                           
#        'Positive' Class : 1               
#                                   
#
# Cut off of 0.3 on Training Data
#
# Actual
# Predicted   0   1
# 0 138  10
# 1  39  93
# 
# Accuracy : 0.825           
# 95% CI : (0.7753, 0.8676)
# No Information Rate : 0.6321          
# P-Value [Acc > NIR] : 1.220e-12       
# 
# Kappa : 0.6446          
# 
# Mcnemar's Test P-Value : 6.334e-05       
#                                           
#             Sensitivity : 0.9029          
#             Specificity : 0.7797          
#          Pos Pred Value : 0.7045          
#          Neg Pred Value : 0.9324          
#               Precision : 0.7045          
#                  Recall : 0.9029          
#                      F1 : 0.7915          
#              Prevalence : 0.3679          
#          Detection Rate : 0.3321          
#    Detection Prevalence : 0.4714          
#       Balanced Accuracy : 0.8413          
#                                           
#        'Positive' Class : 1               
# 
# Actual
# Predicted  0  1
# 0 62  3
# 1 18 37
# 
# Accuracy : 0.825          
# 95% CI : (0.745, 0.8883)
# No Information Rate : 0.6667         
# P-Value [Acc > NIR] : 8.313e-05      
# 
# Kappa : 0.64           
# 
# Mcnemar's Test P-Value : 0.00225        
#                                          
#             Sensitivity : 0.9250         
#             Specificity : 0.7750         
#          Pos Pred Value : 0.6727         
#          Neg Pred Value : 0.9538         
#               Precision : 0.6727         
#                  Recall : 0.9250         
#                      F1 : 0.7789         
#              Prevalence : 0.3333         
#          Detection Rate : 0.3083         
#    Detection Prevalence : 0.4583         
#       Balanced Accuracy : 0.8500         
#                                          
#        'Positive' Class : 1           