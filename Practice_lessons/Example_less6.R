library(ggplot2)
library(sqldf)
library(corrplot)
library(GLMsData)
library(multcomp) # cholesterol dataset
library(gplots) # plotmeans
library(vcd) # Arthritis dataset


help(mtcars)
head(mtcars)
#--------------------------  Covariance and correlation 
ggplot(data=mtcars, mapping=aes(x=mpg, y=qsec))+
  geom_point(size=3, color='slateblue4')

am_code<-mtcars$am+1
ggplot(data=mtcars, mapping=aes(x=mpg, y=qsec))+
  geom_point(size=3, color=am_code)


ggplot(data=mtcars, mapping=aes(x=mpg, y=qsec))+
  geom_point(size=3, color=am_code)+
  geom_smooth(method=lm)

mtcars_metrical<-sqldf('select mpg,disp, hp,drat,wt,qsec from mtcars')

head(mtcars_metrical)
CR<-cor(mtcars_metrical)
corrplot(CR, method="circle")
corrplot(CR, method="pie", type='lower')
corrplot(CR, method="color",type='upper')

cor.test(mtcars$mpg, mtcars$qsec, method='pearson',alt='two.sided')
cor.test(mtcars$mpg, mtcars$qsec, method='pearson', alt='less', conf.level=0.99)
cor.test(mtcars$mpg, mtcars$qsec, method='pearson', alt='greater', conf.level=0.99)

fit<-lm(mpg~wt, mtcars)
summary(fit)

plot(mtcars$qsec, mtcars$mpg)
abline(fit)

fit0<-lm(mpg~0+qsec, mtcars)
summary(fit0)

mtcars_ord<-sqldf('select cyl, gear,carb from mtcars')
head(mtcars_ord)
ggplot(data=mtcars, mapping=aes(x=gear, y=carb))+
  geom_point(size=3, color='slateblue4')

CO<-cor(mtcars_ord, method='spearman')
cor.test(mtcars$cyl, mtcars$carb, method='person',alt='two.sided')

#------------------------------- t test -----------------------------------------
data(blocks)
View(blocks)
head(blocks)
summary(blocks)

# Are the children tended to stack more cubical parts than cylinder parts?

ggplot(data=blocks, mapping=aes(x=Shape, y=Age))+
  geom_boxplot()+
  geom_point(aes( colour=Child))

ggplot(data=blocks, mapping=aes(x=Shape, y=Number))+
  geom_boxplot()+
  geom_point(aes(size=Age, color=Child))+
  coord_flip()+
    theme_bw()

# independent groups
t.test(Number~Shape, data=blocks) #Two independent samples drawn from normal distributed populations
t.test(Number~Shape, data=blocks,alternative='greater') #Two independent samples drawn from normal distributed populations
t.test(blocks$Number, alternative = "less", mu=8) #One-sample t-test

# dependent groups
blocks$Trial <- as.factor(blocks$Trial)

blocks_trial1 <- blocks[blocks$Trial == "1", ]
blocks_trial2 <- blocks[blocks$Trial == "2", ]

if (nrow(blocks_trial1) == nrow(blocks_trial2)) {
  t_test_result <- t.test(blocks_trial1$Number, blocks_trial2$Number, paired = TRUE)
  print(t_test_result)
} else {
  print("Кількість спостережень в групах різна")
}

#-------------------------------- ANOVA ---------------------------------
data("cholesterol")
help(cholesterol)
View(cholesterol)

help("aggregate")
example("aggregate") 

help("attach")
attach(cholesterol)

table(trt) # cross table 
aggregate(response,by=list(trt),FUN=mean) # mean by groups

fit<-aov(cholesterol$response~cholesterol$trt)
summary(fit) # Groups differs

cholesterol$Category <- ifelse(cholesterol$trt %in% c("1time", "2times", "4times"), "Formulation", "Control")

fit_formulation <- aov(response ~ trt, data = subset(cholesterol, Category == "Formulation"))
summary(fit_formulation)

fit_control <- aov(response ~ trt, data = subset(cholesterol, Category == "Control"))
summary(fit_control)

ggplot(cholesterol, aes(x = trt, y = response, fill = trt)) +
  geom_boxplot() +
  ggtitle("Response by Treatment Group") +
  theme_minimal()


ggplot(subset(cholesterol, Category == "Formulation"), aes(x = trt, y = response, fill = trt)) +
  geom_boxplot() +
  ggtitle("Response by Formulation") +
  theme_minimal()


ggplot(subset(cholesterol, Category == "Control"), aes(x = trt, y = response, fill = trt)) +
  geom_boxplot() +
  ggtitle("Response by Control Drug") +
  theme_minimal()

#------------------------ chi-squared for categorial data ----------------

help("Arthritis")

art <- xtabs(~ Treatment + Improved, data = Arthritis, subset = Sex == "Female") # contingency table
art
mosaic(art, gp = shading_Friendly)
mosaic(art, gp = shading_max)
model<-chisq.test(art)
model

model$p.value
model$observed
model$expected
model$residuals # the Pearson residuals, (observed - expected) / sqrt(expected)



chisq_my <- function(tab) {
  m<-chisq.test(tab)
  if (m$p.value<=0.05) {
    print('Difference is significant') 
  } else { print('Difference is not significant')
  }
}

chisq_my(art)
