# Підготовка даних
set.seed(42) # Для відтворюваності результатів
v <- rnorm(120, mean=50, sd=10) # Згенерувати 120 значень з нормального розподілу з середнім 50 та стандартним відхиленням 10
vr<-round(v,0)
# Факторна змінна для демонстрації побудови стовпчастої діаграми
group <- factor(sample(c("Group 1", "Group 2","Group 3"), 120, replace=TRUE))

# Розрахунок описових статистик
mean_v <- mean(v)
median_v <- median(v)
sd_v <- sd(v)
# Вивід результатів
cat("Mean of V:", mean_v, "\n")
cat("Median of V:", median_v, "\n")
cat("Standard Deviation of V:", sd_v, "\n")

# Функція для обрахунку моди
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

mode_v <- getmode(vr)
mode_v
mode_f <- getmode(as.character(group))
mode_f

# Побудова таблиці частот для факторної змінної
freq_table <- table(vr)
print(freq_table)

# Розрахунок відсотків
percentages <- prop.table(freq_table) * 100
print(round(percentages, 2))


# Побудова таблиць частот для метричної змінної
frequency_table <- table(round(v, 0))
print(frequency_table)
frequency_percentage <- round(prop.table(frequency_table) * 100, 2)
print(frequency_percentage)

# Розбиття даних на інтервали
intervals <- cut(v, breaks = seq(20, 80, by = 10))

# Побудова таблиці частот
frequency_table <- table(intervals)
print(frequency_table)

# Побудова таблиці відсотків
percentage_table <- prop.table(frequency_table) * 100
print(percentage_table)


# Гістограма
hist(v, main="Histogram of V", col="blue", xlab="Value", ylab="Frequency", breaks=5)
barplot(percentage_table, main="Bar Plot of Group", col=c("green", "orange","yellow","red","blue", "pink"), xlab="Interval", ylab="Count")

# Побудова стовпчастої діаграми за факторною змінною
barplot(table(group), main="Bar Plot of Group", col=c("green", "orange","red"), xlab="Group", ylab="Count")


# Використання пакетів для розрахунку описових статистик
library(ggplot2)

#our datasets
affairs<-read.csv("C:\\tmp\\R\\Datasets\\Affairs.csv")
data(mpg)

View(affairs)
View(mpg)

#variables
#https://www.statsmodels.org/dev/datasets/generated/fair.html
str(affairs)
str(mpg)

#frequency
table(affairs$gender)
table(affairs[,2])

table(mpg$model)
prop.table(table(mpg$model))*100
prop.table(table(affairs[[2]]))*100

#descriptive statistics
mean(affairs$age)
mean(affairs$education)

#summary statistics
summary(affairs)
summary(mpg)

unique(affairs$education)
affairs$education_factor<-factor(affairs$education, levels=c(9,12,14,16,17,20),
                                labels=c('grade school','high school','some college',
                                         'college graduate','some graduate school',
                                         'advanced degree'))
affairs$gender<-as.factor(affairs$gender)
affairs$children<-as.factor(affairs$children)
affairs$age<-as.numeric(paste(affairs$age))

affairs[is.na(affairs$education_factor),]
mean(affairs$education_factor)   

#sd for sample
sd(affairs$age)

#sd for population
sqrt((length(affairs$age)-1)/length((affairs$age)))*sd(affairs$age)

library(radiant)
sdpop(affairs$age)

#main functions for calculating descriptive statistics
median(affairs$age)
quantile(affairs$age, c(.1, .25, .50, .75, .9))
range(affairs$age)
range_age<-range(affairs$age)[2]-range(affairs$age)[1]

# Libraries for calculation of descriptive statistics
library(psych)
describe(affairs)

s<-describe(affairs$age)
#s[1]
s$mean
d<-describe(affairs)
d$mean[1]

library(jmv)
descriptives(
  data=affairs,
  vars = c("age", "rating", "education_factor"),
  splitBy = "gender"
)

descriptives(
  data=affairs,
  vars = c("age", "rating", "education_factor"),
  splitBy = "gender",
  se = TRUE,
  hist = TRUE,
  boxMean = TRUE,
  pcValues = "25,50,75"
)


#advanced package for calculation of the descriptive statistics
library(janitor)
library(flextable)

affairs %>%
  tabyl(age, gender) %>% 
  adorn_totals(where = "col") %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns(position = "front") %>% 
  adorn_title(
    row_name = "Age Category",
    col_name = "Gender",
    placement = "combined") %>% # this is necessary to print as image
  flextable::flextable() %>%    # convert to pretty image
  flextable::autofit() %>%          # format to one line per row 
  flextable::save_as_docx(path = "C:/tmp/R/tabyl.docx")   # save as Word document to filepath


#addition statistics 
education_by_gender <- affairs %>% 
  tabyl(education_factor, gender, show_na = FALSE) 

chisq.test(education_by_gender)
chisq.test(table(affairs$education_factor, affairs$gender), simulate.p.value = TRUE)


####ggplot2 
############################
library(ggplot2)
#Basic plot
ggplot(mpg, aes(x = displ, y = cty)) + 
  geom_point()

data<-read.csv('C:/tmp/R/Datasets/fibroid.csv')

ggplot(data, aes(x = MP100N, y = MP111N)) + 
  geom_point()


# Basic histogram
ggplot(affairs, aes(x=age)) + 
  geom_histogram()

p<-ggplot(affairs, aes(x=age))
p+
  geom_histogram()

ggplot(mpg, aes(cty, hwy)) + geom_point()
ggplot(diamonds, aes(carat, price)) + geom_point()
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(affairs, aes(age)) + geom_histogram()

#Colour, shape, size
ggplot(affairs, aes(x = age, y = yearsmarried)) + 
  geom_point()
ggplot(mpg, aes(displ, cty)) + 
  geom_point()

ggplot(mpg, aes(displ, cty)) + 
  geom_point(colour = '#41539F',aes(size=year))

ggplot(mpg, aes(displ, cty)) + 
  geom_point(aes(colour = class), shape=6)

#facet
ggplot(affairs, aes(age)) + 
  geom_histogram(aes(fill=gender)) + 
  facet_wrap(~gender)


# Change the width of bins
ggplot(affairs, aes(x=age)) + 
  geom_histogram(binwidth=5)

# Change colors
p<-ggplot(affairs, aes(x=age)) + 
  geom_histogram(binwidth=5, color="black", fill="blue", linetype="dashed")
p

# Add mean line
p+
  geom_vline(aes(xintercept=mean(age)),
              color="red", linetype="dashed", size=1)

#Add smooth
ggplot(mpg, aes(displ, cty)) + 
  geom_point() + 
  geom_smooth()

# Histogram with density plot
ggplot(affairs, aes(x=age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

#https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html#alpha

#Boxplot
ggplot(affairs, aes(gender, age)) + 
  geom_point()

ggplot(affairs, aes(gender, age)) + 
  geom_boxplot()

ggplot(mpg, aes(drv, cty)) + 
  geom_boxplot()

ggplot(mpg, aes(drv, cty, color=drv)) + geom_jitter()
ggplot(mpg, aes(drv, cty, fill=drv)) + geom_boxplot()
ggplot(mpg, aes(drv, cty, fill=drv)) + geom_violin()

#Modifying the axes
ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 5)

#https://ggplot2.tidyverse.org/reference/aes_colour_fill_alpha.html

ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 5, colour="red", size=4) + 
  xlab("city driving (mpg)") + 
  ylab("highway driving (mpg)")

# Remove the axis labels with NULL
ggplot(mpg, aes(cty, hwy)) +
  geom_point(alpha = 1 / 5, colour="red", size=4) + 
  xlab(NULL) + 
  ylab(NULL)

