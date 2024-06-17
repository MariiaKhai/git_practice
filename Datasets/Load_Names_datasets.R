file<-read.table('C:/tmp/R/Datasets/nursery/nursery.data', sep=',')


library(stringr)

# Читання вмісту файла .names як рядків
name_lines <- readLines('C:/tmp/R/Datasets/nursery/nursery.c45-names')

# Вибірка та очищення рядків, які містять назви атрибутів
attribute_lines <- name_lines[grep(":", name_lines)]
attribute_names <- str_extract(attribute_lines, "^[^:]+")

# Відображення назв атрибутів
print(attribute_names)

names(file)<-attribute_names
colnames(file)[9]<-'class'


