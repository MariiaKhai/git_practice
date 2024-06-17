library(dplyr)
library(ggplot2)
library(vcd)

# Считаем данные
titanic <- read.csv('C:/tmp/R/Regression/titanic.csv')
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))
str(titanic)

# Построим мозаичный график
mosaic(~ Sex + Survived | Pclass, data=titanic) 

# Модель без предикторов (Intercept only model)
simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)
table(titanic$Survived)
odds <- 290 / 424
log(odds) 
summary(simple_fit)

# Модель с одним номинативным предиктором
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
coef(fit1)
table(titanic$Survived, titanic$Sex)

odds_male <- 93 / 360
odds_female <- 197 / 64

log(odds_female)
log(odds_male)

odds_ratio <- odds_male / odds_female
log(odds_ratio)

# Модель с двумя категориальными предикторами
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")
coef(fit2)
summary(fit2)

table(titanic$Survived, titanic$Pclass , titanic$Sex)

# (Intercept) 
female_p1_odds <- 82 / 3
log(female_p1_odds)

# Sexmale  
male_p1_odds <- 40  /  61 
log(male_p1_odds)
log(male_p1_odds / female_p1_odds )

# PclassSecond
female_p2_odds <- 68  /  6 
log(female_p2_odds / female_p1_odds )

# PclassThird
female_p3_odds <- 47  /  55 
log(female_p3_odds / female_p1_odds )

# SexMale:PclassSecond
male_p2_odds <- 15 / 84
log(male_p2_odds / female_p2_odds ) - log(male_p1_odds / female_p1_odds )

#Sexmale:factorThird 
male_p3_odds <- 38 / 215
log(male_p3_odds / female_p3_odds ) - log(male_p1_odds / female_p1_odds )


# сравнение моделей
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")

anova(fit1, fit2, test="Chisq")

# предсказание новых данных
new_data <- data.frame(Sex = "Female", Pclass = "Third")
predict(fit2, newdata = new_data, type = "response")


fit3 <- glm(Survived ~ Sex + Pclass, titanic, family = "binomial")
predict(fit3, newdata = new_data)


fit4 <- glm(Survived ~ Sex + Pclass, titanic, family = "binomial")
predSurv<-predict(fit4, newdata = titanic, type = "response")

# Confusion matrix for threshold of 0.5
table(titanic$Survived, predSurv > 0.5)
      FALSE TRUE
No    360   64
Yes    93  197

# Sensitivity
197/(197+93)
# Specificity 
64/(64+360)

# Confusion matrix for threshold of 0.3
table(titanic$Survived, predSurv > 0.3)
FALSE TRUE
No    299  125
Yes    53  237
# Sensitivity 
53/(53+237)
# Specificity
125/(125+299)

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predSurv, titanic$Survived)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")
ROCRperf_acc = performance(ROCRpred, "acc")
ROCRperf_auc = performance(ROCRpred, "auc")

# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Plot Accuracy
plot(ROCRperf_acc)

# AUC
ROCRperf_auc@y.values

#predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
table(titanic$Survived, predSurv > 0.8)


     FALSE TRUE
No    415    9
Yes   140  150


# Accuracy

(415+150)/(415+140+9+150)

table(titanic$Survived, predSurv > 0.1)

FALSE TRUE
No    215  209
Yes    38  252
# Accuracy
(252+215)/(415+140+9+150)

table(titanic$Survived, predSurv > 0.5)
FALSE TRUE
No    360   64
Yes    93  197
# Accuracy
(197+360)/(415+140+9+150)


predSurv_yn<-as.factor(ifelse((predSurv > 0.7), "Yes", "No"))
confusionMatrix(predSurv_yn, titanic$Survived,
                positive = "Yes")
