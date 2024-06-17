library(dplyr)
library(ggplot2)
library(vcd)


# Loading data
titanic<-read.csv('C:/Mariia/tmp/R/Regression/titanic.csv')
titanic <- na.omit(titanic)
glimpse(titanic)
titanic <- mutate(titanic, 
                  Survived = factor(Survived, labels = c("No", "Yes")), 
                  Pclass = factor(Pclass, labels = c("First", "Second", "Third")), 
                  Sex = factor(Sex, labels = c("Female", "Male")))

# Mosaic plot
mosaic(~  Survived + Sex + Pclass, data=titanic, shade=TRUE)
mosaic(~  Survived + Sex, data=titanic, shade=TRUE)
mosaic(~  Survived, data=titanic, shade=TRUE)

# Intercept only model
simple_fit <- glm(Survived ~ 1, titanic, family = "binomial")
coef(simple_fit)
table(titanic$Survived)
odds <- 290 / 424
odds
log(odds) 
p=1/(1+exp(-coef(simple_fit)))
p
summary(simple_fit)

# Pair logistic regression with nominal independent variable
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
summary(fit1)
coef(fit1)
table(titanic$Survived, titanic$Sex)

odds_female <- 197 / 64
odds_female
odds_male <- 93 / 360
odds_male


log(odds_female)
log(odds_male)

odds_ratio <- odds_male / odds_female
log(odds_ratio)

p_female=1/(1+exp(-fit1$coefficients[1]))
p_female
p_male=1/(1+exp(-(fit1$coefficients[1]+fit1$coefficients[2])))
p_male

titanic$predict_Sex<-predict(fit1, titanic, type="response")


# Multiply logistic regression with two nominal independent variable  
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")
coef(fit2)
summary(fit2)

table(titanic$Survived, titanic$Pclass , titanic$Sex)

# (Intercept) 
female_p1_odds <- 82 / 3
female_p1_odds
log(female_p1_odds)

# Sexmale  
male_p1_odds <- 40  /  61 
male_p1_odds
log(male_p1_odds)
log(male_p1_odds / female_p1_odds )

# PclassSecond
female_p2_odds <- 68  /  6 
female_p2_odds
log(female_p2_odds / female_p1_odds )

# PclassThird
female_p3_odds <- 47  /  55 
female_p3_odds
log(female_p3_odds / female_p1_odds )

# SexMale:PclassSecond
male_p2_odds <- 15 / 84
male_p2_odds
log(male_p2_odds / female_p2_odds ) - log(male_p1_odds / female_p1_odds )

#Sexmale:factorThird 
male_p3_odds <- 38 / 215
male_p3_odds
log(male_p3_odds / female_p3_odds ) - log(male_p1_odds / female_p1_odds )

titanic$predict_Sex_Class<-predict(fit2, titanic, type="response")

# Compare models
fit1 <- glm(Survived ~ Sex, titanic, family = "binomial")
fit2 <- glm(Survived ~ Sex * Pclass, titanic, family = "binomial")

anova(fit1, fit2, test="Chisq")

# Predict new values
new_data1 <- data.frame(Sex = "Female", Pclass = "First")
new_data2 <- data.frame(Sex = "Male", Pclass = "Second")
predict(fit1, newdata = new_data1, type = "response")
predict(fit2, newdata = new_data1, type = "response")
predict(fit1, newdata = new_data2, type = "response")
predict(fit2, newdata = new_data2, type = "response")


## ROC, Confusion matrix 
##https://rpubs.com/abhaypadda/logistic-regression-using-titanic-data

predSurv<-predict(fit2, newdata = titanic, type = "response")

# Confusion matrix for threshold of 0.5
table(titanic$Survived, predSurv > 0.5)

# Sensitivity
197/(197+93)
# Specificity 
64/(64+360)

# Confusion matrix for threshold of 0.3
table(titanic$Survived, predSurv > 0.3)

# Sensitivity 
53/(53+237)
# Specificity
125/(125+299)


# Install and load ROCR package
#install.packages("ROCR")
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

# Accuracy

(415+150)/(415+140+9+150)

table(titanic$Survived, predSurv > 0.1)

# Accuracy
(252+215)/(415+140+9+150)

table(titanic$Survived, predSurv > 0.5)

# Accuracy
(197+360)/(415+140+9+150)


# Install and load caret package
#install.packages("caret")
library(caret)
predSurv_yn<-as.factor(ifelse((predSurv > 0.8), "Yes", "No"))
confusionMatrix(predSurv_yn, titanic$Survived,
                positive = "Yes")

