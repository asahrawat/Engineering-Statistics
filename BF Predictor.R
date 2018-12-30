#Group 1 variables: Body fat, Age, Density, Weight, Height, Forearm, Wrist, Bicep


library ('dplyr')
library ('readxl')

BFdata <- BodyFat_1_
#View(BFdata)
#view correlation estimates
plot(BFdata)
#correlation values
correlation <- cor(BFdata)
correlation

model_1 <- lm(BFdata$BODYFAT~BFdata$DENSITY)
#cor(BFdata$BODYFAT,BFdata$DENSITY)
summary(model_1)


model_2 <- lm(BFdata$BODYFAT~BFdata$DENSITY+BFdata$WEIGHT)
#cor(BFdata$BODYFAT,BFdata$DENSITY+BFdata$WEIGHT)
summary(model_2)



model_3 <- lm(BFdata$BODYFAT~BFdata$DENSITY+BFdata$BICEPS)
#cor(BFdata$BODYFAT,BFdata$DENSITY+BFdata$BICEPS)
summary(model_3)



model_4 <- lm(BFdata$BODYFAT~BFdata$DENSITY+BFdata$WEIGHT+BFdata$BICEPS)
#cor(BFdata$BODYFAT,BFdata$DENSITY+BFdata$WEIGHT+BFdata$BICEPS)
summary(model_4)




model_5 <- lm(BFdata$BODYFAT~BFdata$DENSITY+BFdata$WEIGHT+BFdata$BICEPS+BFdata$AGE)
#cor(BFdata$BODYFAT,BFdata$DENSITY+BFdata$WEIGHT)
summary(model_5)

#R^2 = 0.9778

plot(BFdata$BODYFAT, BFdata$DENSITY+BFdata$WEIGHT+BFdata$BICEPS+BFdata$AGE)

plot(model_5)



#R^2 for model with all variables = 0.9777


