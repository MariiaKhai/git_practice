library(ggplot2)
library(dplyr)
library(corrplot)

###Pair linear regression
#loading data
df<-read.csv("C:\\Mariia\\tmp\\R\\Regression\\attitude.csv")
summary(df)

#EDA income.data
hist(df$rating) 
hist(df$complaints)
corrplot(cor(df), method="circle")
plot(rating ~ complaints, data = df)
abline(lm(df$rating ~ df$complaints), col='red')


#regression model
df.lm <- lm(rating ~ complaints, data = df)
summary(df.lm)
par(mfrow=c(2,2))
plot(df.lm)
par(mfrow=c(1,1))

#Residuals
summary(df$rating - df.lm$fitted.values)
#Q-Q plot for residuals
qqnorm(df.lm$residuals, pch = 1, frame = FALSE)
qqline(df.lm$residuals, col = "steelblue", lwd = 2)
#or
library(car)
qqPlot(df.lm$residuals)
shapiro.test(df.lm $resid)

#Coefficients
df.lm$coefficients
print(paste("Rating = ", round(df.lm$coefficients[2],2) ,
            "*complaints +",round(df.lm$coefficients[1],2)))

#regression model
df.lm2 <- lm(rating ~ complaints+learning+raises, data = df)
summary(df.lm2)
par(mfrow=c(2,2))
plot(df.lm2)
par(mfrow=c(1,1))


#Stepwise regression
df.lm_null<-lm(rating ~ 1, data = df)
df.lm_full<-lm(rating ~ complaints+learning, data = df)
summary(df.lm_null)
mean(df$rating)
scope = list(lower = df.lm_null, upper = df.lm_full)
ideal_model <-step(df.lm, scope, direction = "forward")
anova(df.lm_full,df.lm_null)
