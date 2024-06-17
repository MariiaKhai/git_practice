library(base)
library(dplyr)
setwd("C:/Mariia/tmp/R/")

# Створення змінних
a <- 45 # ціле число
A <- 56 # інша змінна, R розрізняє регістр
vector <- c(2, 3, 6) # вектор цілих чисел
real <- c(0.5, 0.3, 0.7) # вектор дійсних чисел
string_my <- c("a", 'r', 'y') # вектор рядків
print(vector) # виведення вектора
vector[2]

# Типи даних

# Створення вектора символів
data <- c("1", "2", "three", "4.5")
# Перевірка типу даних
class(data)
# Перетворення символів на числа
numeric_data <- as.numeric(data)
numeric_data

# Створення фактора
factor_data <- factor(c("yes", "no", "yes", "maybe"))
class(factor_data)
levels(factor_data)
# Перетворення фактора на символьний тип
char_data <- as.character(factor_data)
class(char_data)


# Створення dataframe
df <- data.frame(vector, real, string_my)
View(df)
class(df)
str(df)

# Отримання значень з dataframe
df$vector # весь стовпець vector
df$vector[1] # перше значення стовпця vector
df[1, 1] # перше значення першого стовпця
df[[3]] # весь третій стовпець
df[,] # увесь dataframe
df[c(1:2),] # декілька строк
df[,c(1:2)] # декілька стовпців
df[ , c('real', 'vector')] # декілька стовпців


avr <- mean(df$real) # розрахунок середнього значення
avr
print(df$real >= avr) # порівняння значень стовпця з середнім
df$real[df$real > avr] # виведення значень 'real', які більші за середнє
df$vector[df$real == 0.3 & df$string_my %in% c('a','r')] # відбір даних за умовами
df$vector[df$string_my %in% c('a','r')] # відбір даних за умовами


# Аннотація датасету
names(df)[1] <- "Age at hospitalization (in years)"
colnames(df) <- c('A','B','C')
rownames(df)<-c('A','B','C')

attr(df, "description") <- "This data frame contains..."

df$A_factor <- factor(df$A,
                      levels = c(2,3,6),
                      labels = c("male", "female", NA))

# Внутрішня індексація факторної змінної не співпадає з індексацією числового вектору!
ff<-factor(c(5,6,8,4,5,5,5,6,7,8), labels=c('a','b','c','d','e')) %>% print()
fn<-as.numeric(ff) %>% print()
fc<-as.character(ff) %>% print()
ff_first<- as.numeric(as.character(ff)) %>% print()

# Відновити початкові значення числового вектору, який був трасформований у фактор
# можливо тільки тод, коли немає міток
fff <- factor(4:6) %>% print()
as.numeric(fff)
as.numeric(as.character(fff))


library(expss)
df = apply_labels(df,
              	A =  'Study Identifier' ,
              	B  = 'Domain Abbreviation' ,
              	C  =  'Unique Subject Identifier')


# Робота з файлами
file <- read.csv("Affairs.csv", encoding = "UTF-8", header = TRUE, sep = ",")
path = 'C:\\Mariia\\tmp\\R\\Datasets'
file <- read.csv(path, encoding = "UTF-8", header = TRUE, sep = ",")
write.csv(df, "df.csv", row.names = FALSE)


# Перевірка даних
length(vector) # довжина вектора
dim(df)[2] # кількість стовпців в dataframe
str(df) # структура dataframe
tail(file) # перші рядки dataframe
ls()
rm()

# Робота з пакетами та даними
# install.packages('dplyr')
# library(dplyr)
help("dplyr") # допомога по пакету
?dplyr # ще один спосіб отримати допомогу
data()  # вивід доступних датасетів
help(mpg) # допомога по конкретному датасету
data("mtcars") # завантаження датасету
View(mtcars) # відображення датасету
data(CO2)


