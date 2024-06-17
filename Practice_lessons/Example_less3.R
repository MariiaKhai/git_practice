library(dplyr)
library(ggplot2)
data<- read.csv("C:/tmp/UKR_Center/OpenData2018.csv", sep=';', encoding ='UTF-8' )

#Summary information
summary(data)
glimpse(data)
table(data$SEXTYPENAME)
table(data$REGNAME)


#set.seed function
set.seed(10)

#creating short sample
data_short<-data[1:100000,]
data_short<-subset(data, UkrBall100>120)
data_short<-data[sample(nrow(data), 100000),]
#or
data_short<-sample_n(data, 100000)
df<-data_short
#or
df<-data %>%
  sample_n(., 100000) 


#rename columns
colnames(df)[1]<-"ID"
colnames(df)[3]<-"Gender"
#or
#df<-as_tibble(df)
#df<-rename(df, ID=OUTID)
#df<-rename(df, Gender=SEXTYPENAME)

#unique data
unique(df$Gender)
unique(df$REGNAME)
#or
df$REGNAME[!duplicated(df$REGNAME)]
#remove duplicated based REGNAME or 2:4 variables
df[!duplicated(df$REGNAME),]
df[!duplicated(df[c(2:4)]),]
#or
df %>% 
  distinct(REGNAME, .keep_all=TRUE)


#moves column to a new position
df<-relocate(df, Gender, .after=ID)
df<-relocate(df, ClassProfileNAME, .after=last_col())

#select columnes and create new dataset
df<-select(df, ID, Gender, Birth, REGNAME, REGTYPENAME, ClassProfileNAME, EOTYPENAME, 
           UkrBall100, UkrBall12, UkrBall, mathBall100, mathBall12, mathBall)
#check missing data
colSums(is.na(df))
rowSums(is.na(df))
df[df=='null']<-NA

#divide dataset on 2 datasets
df1<-select(df, Gender, ID, Birth, REGNAME, REGTYPENAME, ClassProfileNAME, EOTYPENAME)
df1$Gender<-as.factor(df1$Gender)
df1$REGNAME<-as.factor(df1$REGNAME)
df1$REGTYPENAME<-as.factor(df1$REGTYPENAME)
df1$ClassProfileNAME<-as.factor(df1$ClassProfileNAME)
df1$EOTYPENAME<-as.factor(df1$EOTYPENAME)
summary(df1)

df2<-select(df, UkrBall100, UkrBall12, UkrBall, mathBall100, mathBall12, mathBall)
df2$UkrBall100<-as.numeric(paste(df2$UkrBall100))

df2<-df2 %>%
  mutate(UkrBall100=as.numeric(paste(UkrBall100)),
         UkrBall12=as.numeric(paste(UkrBall12)),
         UkrBall=as.numeric(paste(UkrBall)),
         mathBall100=as.numeric(paste(mathBall100)),
         mathBall12=as.numeric(paste(mathBall12)),
         mathBall=as.numeric(paste(mathBall))
         )

#combine 2 datasets
df_new<-cbind(df1,df2)
#or
df_new<-bind_cols(df1, df2)

#checkresults
summary(df_new)
glimpse(df_new)
barplot(table(df_new$Birth))

#mutate - Make New Variables

#define bin interval for Birth
df_new<-df_new %>%
  mutate(new_Birth=cut(Birth, breaks=c(1997, 1999, 2001, 2003)))
#check results
table(df_new$new_Birth)
sum(is.na(df_new$new_Birth))
table(df_new$new_Birth, exclude = NULL)
barplot(table(df_new$new_Birth, exclude = NULL))

#create sum of balls
df_new<-df_new %>%
  mutate(sumBall100=UkrBall100+mathBall100)

#df_new$UkrBall100[is.na(df_new$UkrBall100)]<-0
#df_new$mathBall100[is.na(df_new$mathBall100)]<-0

df_new$UkrBall[is.na(df_new$UkrBall)]<-0
df_new$mathBall[is.na(df_new$mathBall)]<-0
df_new<-na.omit(df_new)

df_new<-df_new %>%
  mutate(grade_Ukr=percent_rank(UkrBall))%>%
  mutate(ball100_200=(grade_Ukr*(200-100)+100)) %>%
  arrange(-grade_Ukr)



#summaries + group_by
df_gender <- df_new %>%
  group_by(Gender) %>%
  summarise(meanUkrBall100=mean(UkrBall100, na.rm=TRUE),
            meanUkrBall12=mean(UkrBall12, na.rm=TRUE))

df_new_Birth <- df_new %>%
  group_by(new_Birth) %>%
  summarise(meanUkrBall100=mean(UkrBall100, na.rm=TRUE),
            meanUkrBall12=mean(UkrBall12, na.rm=TRUE),
            ncount=n()) 
###########################################################################

#summaries + group_by + filter
df_reg<-df_new %>%
  dplyr::filter(mathBall100>0) %>%
  dplyr::group_by(REGNAME) %>%
  dplyr::summarise(meanMath=mean(mathBall100, na.rm=TRUE)) %>%
  dplyr::arrange(desc(meanMath)) 

df_excelent_REGName <- df_new %>%
  dplyr::group_by(REGNAME, EOTYPENAME) %>%
  dplyr::summarise(mean_UkrBall100 = mean(UkrBall100, na.rm=TRUE)) %>%
  dplyr::filter(mean_UkrBall100 == max(mean_UkrBall100))

#summaries + group_by + mutate
df_Birth<-df_new %>%
  dplyr::group_by(Birth) %>%
  dplyr::summarise(ncount=n()) %>%
  dplyr::arrange(Birth)  %>%
  dplyr::mutate(per=ncount/sum(ncount)*100) %>%
  dplyr::mutate(cumcount=cumsum(ncount),
         cumper=cumsum(per))

ggplot(df_Birth, aes(Birth, cumper)) +
  geom_line()

#summarise + group_by more 1 variables
df_gender_regtype <-df_new %>%
  dplyr::group_by(Gender,Birth) %>%
  dplyr::summarise(meanUkr100=mean(UkrBall100, na.rm=TRUE)) 
  

#reshaping data
library(tidyr)
df_reshape<-spread(df_gender_regtype, Gender, meanUkr100)
df_reshape_back<-gather(df_reshape, "Sex", "mean", 2:3)

library(reshape)
df_new1<-select(df_new, Birth, Gender, UkrBall100)
df_new1<-na.omit(df_new1)
df_reshape1<- cast(df_new1, Birth~Gender, mean)
df_reshape_back1<-melt(df_reshape1, id=c('жіноча', 'чоловіча'))

#merge data
#creating subsets for merging
df_ukr<-subset(df, !(is.na(df$UkrBall100)))
df_math<-subset(df, !(is.na(df$mathBall100)))
df_ukr<-select(df_ukr, ID, Gender, UkrBall100)
df_math<-select(df_math, ID, Gender, mathBall100)


# merge or...
df_ukr_math<-merge(df_ukr, df_math[,-2], by='ID', all=FALSE)
df_ukr_math_all<-merge(df_ukr, df_math, by='ID', all=TRUE)
df_ukr_math_x<-merge(df_ukr, df_math, by='ID', all.x=TRUE)
df_ukr_math_y<-merge(df_ukr, df_math, by='ID', all.y=TRUE)

# .. join
df_ukr_math<-inner_join(df_ukr, df_math, by='ID')
df_ukr_math<-full_join(df_ukr, df_math, by='ID')
df_ukr_math_x<-left_join(df_ukr, df_math, by='ID')
df_ukr_math_y<-right_join(df_ukr, df_math, by='ID')

df_ukr_math_semi<-semi_join(df_ukr, df_math, by='ID')
df_ukr_math_anti<-anti_join(df_ukr, df_math, by='ID')
df_ukr_math_nest<-nest_join(df_ukr, df_math, by='ID')

#save data 
write.csv(df_new, 'C:/tmp/R/df_DPA.csv', row.names = FALSE)
cc<-read.csv('C:/tmp/R/df_DPA.csv')
saveRDS(df_new, 'C:/tmp/R/df_DPA.rds')
rr<-readRDS('C:/tmp/R/df_DPA.rds')

library(pak)
pak::pak("caret")

#One-Hot Encoding
library(caret)
#define one-hot encoding function
dummy_df_new <- dummyVars(" ~ Gender", data=df_new)
df_dummy <- data.frame(predict(dummy_df_new, newdata = df_new))
#or
library(data.table)
library(mltools)
dummy_df_Gender <- one_hot(as.data.table(df_new$Gender))

#normalization vs standardization
library(BBmisc)
df2_norm <- normalize(df2, method='range', range=c(0,1)) 
summary(df2_norm)
df2_scaled <- scale(df2)         
summary(df2_scaled)


df2$UkrBall_log<-log(df2$UkrBall)
hist(df2$UkrBall)
hist(df2$UkrBall_log)

#discretization
library(recipes)
df_disc<-discretize(df2$UkrBall100, cuts=4)
df2$UkrBall100_bin<-predict(df_disc, df2$UkrBall100)
df2_disc <- df2 %>%
  dplyr::group_by(UkrBall100_bin) %>%
  dplyr::summarise(meanUkrbin=mean(UkrBall100, na.rm=TRUE))

