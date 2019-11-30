# logistic regression - Naresh Malhotra Example

data = read.csv(file.choose())

logit = glm(Loyalty ~ ., data = data[,-1],family=binomial)

summary(logit)


#1 Identify overall fitness of model using log likehood Ratio Test
install.packages("lmtest")
library(lmtest)

lrtest(logit)

#2 Calculating McFadden's Rsquared (Minimum McFadden Rsquared considered for model fitness is 10%)
install.packages("pscl")
library(pscl)
pR2(logit)

#43.56% of the uncertainty in the intercept only model is explained by the full model

#3 Coefficients importance
summary(logit)

# Product and shopping are insignificant in the model
#AIC = -2logLikelihood +2k = -2 * (-11.7357) +2*4 = 31.471
#Lower the AIC, better the model - Used for multicomparison of models with different variables



#4 Explanatory power of odds and probability
exp(coef(logit))


#5 Building Confusion Matrix

prediction <- predict(logit,type = "response")
prediction
cutoff = floor(prediction+0.75)
cutoff

confmat = table(Predicted=cutoff,Actual=data$Loyalty)
confmat

#6 Precision, Recall & F1 Score
install.packages("caret")
library(caret)

confusionMatrix(confmat,positive="1",mode="everything")

#7 ROC Curve
install.packages("pROC")

library(pROC)

plot.roc(data$Loyalty,prediction)

auc(data$Loyalty,prediction)




