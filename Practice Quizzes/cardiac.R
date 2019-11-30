
z = 0.7 + (0.02*48) + (0.005*60)

dim(cardiac)

is.na(cardiac)

str(cardiac)


summary(cardiac)

attach(cardiac)

sd(AGE_ATWHICH_THE_AILMENT_WAS_DIAGNOSED)
sd(Height)
sd(Weight)
sd(BMI)
sd(BP_Diastolic)
sd(Pulse_Rate)
sd(Stress_Index)


cor(BMI,Weight)

cor(Weight , Height)


?vif

library(MASS)
require(MASS)

library(car)


?vif
vif(BP_Diastolic)


model <- lm(Stress_Index ~., data = cardiac)
vif(model)

a = c(10,10,9,4,11,4,2)
b = c( 12,14 , 13, 5 , 15,7 ,4)

cor(a,b)
