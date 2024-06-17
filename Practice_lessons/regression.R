library(ggplot2)
library(dplyr)

###Pair linear regression
#loading data
income.data<-read.csv("C:\\Mariia\\tmp\\R\\Regression\\income.data.csv")
summary(income.data)

#EDA income.data
hist(income.data$happiness) 
hist(income.data$income)
plot(happiness ~ income, data = income.data)
abline(lm(income.data$happiness ~ income.data$income), col='red')

#regression model
income.happiness.lm <- lm(happiness ~ income, data = income.data)
summary(income.happiness.lm)
par(mfrow=c(2,2))
plot(income.happiness.lm)
par(mfrow=c(1,1))

#Residuals
summary(income.data$happiness - income.happiness.lm$fitted.values)
#Q-Q plot for residuals
qqnorm(income.happiness.lm$residuals, pch = 1, frame = FALSE)
qqline(income.happiness.lm$residuals, col = "steelblue", lwd = 2)
#or
library(car)
qqPlot(income.happiness.lm$residuals)

#normality residuals
library(fBasics) 
jarqueberaTest(income.happiness.lm $resid)
#or
shapiro.test(income.happiness.lm $resid)

#dwtest - independent residuals
library(car) 
durbinWatsonTest(income.happiness.lm)
#or
library(lmtest) 
dwtest(income.happiness.lm)

#Coefficients
income.happiness.lm$coefficients
print(paste("Level of happiness = ", round(income.happiness.lm$coefficients[2],2) ,
            "*income +",round(income.happiness.lm$coefficients[1],2)))
#prediction 
income.happiness.lm$coefficients[2]*5+income.happiness.lm$coefficients[1]

#income.new<-data.frame(income=c(10,11,12,13,14,15))
income.new<-data.frame(income=income.data$income)
income.predict<-predict(income.happiness.lm, newdata = income.new) 
#income.new$predict<-income.predict
income.data$income.predict<-income.predict



### Multiple linear regression
#loading data
heart.data<-read.csv("C:\\Mariia\\tmp\\R\\Regression\\heart.data.csv")
summary(heart.data)

#EDA heart.data
hist(heart.data$heart.disease)
hist(heart.data$biking)
hist(heart.data$smoking)

cor(heart.data$heart.disease, heart.data$smoking, method = 'spearman')
cor(heart.data$heart.disease, heart.data$biking, method = 'spearman')

#Multicollinearity
cor(heart.data$biking, heart.data$smoking, method = 'spearman') 

plot(heart.disease ~ biking, data=heart.data)
abline(lm(heart.data$heart.disease ~ heart.data$biking), col='red')
plot(heart.disease ~ smoking, data=heart.data)
abline(lm(heart.data$heart.disease ~ heart.data$smoking), col='blue')

#regression model
heart.disease.lm<-lm(heart.disease ~ biking + smoking, data = heart.data)
summary(heart.disease.lm)
par(mfrow=c(2,2))
plot(heart.disease.lm)
par(mfrow=c(1,1))
#normality residuals
library(fBasics) 
jarqueberaTest(heart.disease.lm$resid)
#dwtest - independent residuals
library(lmtest) 
dwtest(heart.disease.lm)

income.graph<-ggplot(income.data, aes(x=income, y=happiness))+
  geom_point()+
  geom_smooth(method="lm", col="red")+
  theme_bw() +
  labs(title = "Reported happiness as a function of income",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")+
  annotate(geom="text", x=6, y=1.75, 
           label=paste(" happiness=",round(income.happiness.lm$coefficients[2],2),"*income",
                       "+",round(income.happiness.lm$coefficients[1],2)))
income.graph

plotting.data<-expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out=30),
  smoking=c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking)))

plotting.data$predicted.y <- predict.lm(heart.disease.lm, newdata=plotting.data)
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)
plotting.data$smoking <-as.factor(plotting.data$smoking)

heart.plot <- ggplot(heart.data, aes(x=biking, y=heart.disease)) +
  geom_point()+
  geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking), size=1.25)+
  theme_bw() +
  labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
       x = "Biking to work (% of population)",
       y = "Heart disease (% of population)",
       color = "Smoking \n (% of population)")+
  annotate(geom="text", x=30, y=1.75, 
           label=paste(" Heart disease =", round(heart.disease.lm$coefficients[1],2),
           "+(", round(heart.disease.lm$coefficients[2],2), ")*biking",
           "+(", round(heart.disease.lm$coefficients[3],2), ")*smoking"))

heart.plot

#Stepwise regression
heart.data<-heart.data[,-1]

heart.disease.lm_null<-lm(heart.disease ~ 1, data = heart.data)
heart.disease.lm_all<-lm(heart.disease ~ smoking+biking, data = heart.data)
summary(heart.disease.lm_null)
summary(heart.disease.lm_all)
anova(heart.disease.lm_null, heart.disease.lm_all)

scope = list(lower = heart.disease.lm_null, upper = heart.disease.lm_all)
both <-step(heart.disease.lm_null, scope, direction = "both")


forward<-step(heart.disease.lm_null,direction = "forward",scope = formula(heart.disease.lm_all),trace=0)
forward$anova

backward<-step(heart.disease.lm_all,direction = "backward",scope = formula(heart.disease.lm_null),trace=1)
backward$anova
