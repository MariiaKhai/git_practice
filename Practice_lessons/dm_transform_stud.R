library(haven)
library(dplyr)
library(expss)
library(readxl)
library(lubridate)

######
demog <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/demog.sas7bdat")
dm_goal <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/R/dm.sas7bdat")
str(dm_goal)

str(demog)
class(demog)
summary(demog)
unique(demog$SITEID)
unique(demog$STUDYID)
unique(demog$SUBJID)

demog_studyid <- demog %>%
  group_by(SUBJID) %>%
  summarise(STUDYID=last(STUDYID),
            SITEID=last(SITEID),
            DM='DM',
            SEX=first(GENDER),
            BIRTHDTC=first(BIRTHDT))

demog_studyid$USUBJID<-paste0(demog_studyid$STUDYID,"-", demog_studyid$SITEID,"-", sprintf("%06d",demog_studyid$SUBJID))


rdmcode<-read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/rdmcode.sas7bdat")

dm<-merge(demog_studyid, rdmcode[,c(1,4)], by="SUBJID")

invagt<-read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/invagt.sas7bdat")


invagt_filter_first<-subset(invagt, VISID=='DAY1')

invagt_first<-invagt %>%
  group_by(SUBJID) %>%
  summarise(DAYDATE_new=first(as.Date(DAYDATE)))

dm<-merge(dm, invagt_filter_first[,c(1,13,15)], by="SUBJID")
dm$RFXSTDTC<-paste0(dm$DAYDATE, 'T', dm$TIMEADM)

dm$SEX<-replace(dm$SEX,dm$SEX==1, "M")
dm$SEX<-replace(dm$SEX,dm$SEX==2, "F")

colnames(dm)[8]<-'RFSTDTC'
dm$Age<-round((as.numeric(as.Date(dm$RFSTDTC))-as.numeric(as.Date(dm$BIRTHDTC))+1)/365.25,0)



