library(haven)
library(dplyr)
library(expss)
library(readxl)
library(lubridate)

######
demog <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/demog.sas7bdat")

describe(demog)
summary(demog)
unique(demog$STUDYID)
unique(demog$SUBJID)

#dplyr, summarise, last
demog_studyid <- demog %>% 
  group_by(SUBJID) %>%
  summarise(STUDYID=last(STUDYID))

dm <- demog_studyid
dm$DOMAIN <-"DM" 

unique(demog$SITEID)
demog_USUBJID<- demog %>% 
  group_by(SUBJID) %>%
  summarise(SITEID=last(SITEID))

#merge
dm<-merge(dm, demog_USUBJID, by='SUBJID', all = TRUE)

# sprintf
dm$SUBJID <- sprintf("%06d", dm$SUBJID)
# paste0
dm$USUBJID  <- paste0(dm$STUDYID, "-", dm$SITEID, "-", dm$SUBJID)


####
rdmcode <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/rdmcode.sas7bdat")

rdmcode$SUBJID <- sprintf("%06d", rdmcode$SUBJID)
dm<-merge(dm, rdmcode[,c(1,4)], by='SUBJID', all = TRUE)
dm$RFSTDTC<-dm$DATERANDOMIZED
dm$RFSTDTC <- format(dm$RFSTDTC, "%Y-%m-%d")
dm<-dm[,-(length(dm)-1)]

######
endtrial <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/endtrial.sas7bdat")
endtrial$SUBJID <- sprintf("%06d", endtrial$SUBJID)
dm <- merge(dm, endtrial[, c(3,10)], by='SUBJID', all = TRUE)
dm <- dm %>% rename(RFENDTC = LASTVSDT)
dm$RFENDTC <- format(dm$RFENDTC, "%Y-%m-%d")

#####
invagt <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/invagt.sas7bdat")
invagt$SUBJID <- sprintf("%06d", invagt$SUBJID)
invagt_filter_first <- subset(invagt, VISID=='DAY1')

dm <- merge(dm, invagt_filter_first[,c(1,13,15)], by='SUBJID', all = TRUE)

dm$RFXSTDTC <- paste(dm$DAYDATE, dm$TIMEADM)
dm$RFXSTDTC <- as.POSIXct(dm$RFXSTDTC, format = "%Y-%m-%d %H:%M")
dm$RFXSTDTC <- format(dm$RFXSTDTC, "%Y-%m-%dT%H:%M")
dm <- dm [,-c(length(dm)-1,length(dm)-2)]

invagt_filter_last <- invagt %>%
  mutate(VISID_DAY = as.numeric(substring(VISID, 4))) %>%
  group_by(SUBJID) %>%
  slice_max(order_by = VISID_DAY, with_ties = FALSE)

dm <- merge(dm, invagt_filter_last[,c(1,13,15)], by='SUBJID', all = TRUE)

dm$RFXENDTC <- paste(dm$DAYDATE, dm$TIMEADM)
dm$RFXENDTC <- as.POSIXct(dm$RFXENDTC, format = "%Y-%m-%d %H:%M")
dm$RFXENDTC <- format(dm$RFXENDTC, "%Y-%m-%dT%H:%M")
dm <- dm [,-c(length(dm)-1,length(dm)-2)]

dm$RFICDTC <- NA

dm <- merge(dm, endtrial[, c(3,10)], by='SUBJID', all = TRUE)
dm <- dm %>% rename(RFPENDTC = LASTVSDT)
dm$RFPENDTC <- format(dm$RFPENDTC, "%Y-%m-%d")

death <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/death.sas7bdat")
death$SUBJID <- sprintf("%06d", death$SUBJID)
dm <- merge(dm, death[, c(3,10)], by='SUBJID', all = TRUE)
dm <- dm %>% rename(DTHDTC  = DEATHDT)
#dm$DTHDTC <- format(dm$DTHDTC, "%Y-%m-%d")

#ifelse
dm$DTHFL <- ifelse(!is.na(dm$DTHDTC), 'Y', NA)

dm$SITEID <- dm$SITEID

demog_birth <- demog %>% 
  group_by(SUBJID) %>%
  summarise(BIRTHDT=last(BIRTHDT))
demog_birth$SUBJID <- sprintf("%06d", demog_birth$SUBJID)

dm <- merge(dm, demog_birth, by='SUBJID', all.x = TRUE)
dm <- dm %>% rename(BRTHDTC = BIRTHDT)
dm$BRTHDTC <- format(dm$BRTHDTC, "%Y-%m-%d")

dm$AGE <- round((as.numeric(as.Date(dm$RFSTDTC) - as.Date(dm$BRTHDTC)) + 1) / 365.25 ,0)
dm$AGEU <- 'YEARS'

demog_sex <- demog %>% 
  group_by(SUBJID) %>%
  summarise(GENDER=last(GENDER))
demog_sex$SUBJID <- sprintf("%06d", demog_sex$SUBJID)

dm <- merge(dm, demog_sex, by='SUBJID', all=TRUE)
dm <- dm %>% rename(SEX = GENDER)
dm$SEX <- replace(dm$SEX, dm$SEX==1, 'M')
dm$SEX <- replace(dm$SEX, dm$SEX==2, 'F')

SDTM_Specs_updt <- read_excel("E:/Mentors 1 semester/Nataliia Moiseienko/R/SDTM_Specs_updt.xlsx", sheet = "ValueLevel")

subset_race<-subset(SDTM_Specs_updt, VARIABLE=='RACE')
demog_majrrace <- demog %>% 
  group_by(SUBJID) %>%
  summarise(MAJRRACE=last(MAJRRACE))
demog_majrrace$SUBJID <- sprintf("%06d", demog_majrrace$SUBJID)
dm <- merge(dm, demog_majrrace, by='SUBJID', all=TRUE)
dm<-merge(dm, subset_race[,c(4,10)], by.x = "MAJRRACE" , by.y = "CONDITIONAL_VALUE1")
dm<-dm[,c(2:length(dm))]
colnames(dm)[length(dm)]<-'RACE'

subset_ethnic<-subset(SDTM_Specs_updt, VARIABLE=='ETHNIC')
demog_ethnic <- demog %>% 
  group_by(SUBJID) %>%
  summarise(MAJRRACE=last(MAJRRACE))
demog_ethnic$SUBJID <- sprintf("%06d", demog_ethnic$SUBJID)
dm <- merge(dm, demog_ethnic, by='SUBJID', all=TRUE)
dm<-merge(dm, subset_ethnic[,c(4,10)], by.x = "MAJRRACE" , by.y = "CONDITIONAL_VALUE1")
dm<-dm[,c(2:length(dm))]
colnames(dm)[length(dm)]<-'ETHNIC'

Rdmcode <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/Rawdata/Rdmcode.sas7bdat")
Rdmcode$SUBJID <- sprintf("%06d", Rdmcode$SUBJID)
dm <- merge(dm, Rdmcode[, c(1,6)], by='SUBJID', all = TRUE)
dm <- dm %>% rename(ARMCD = TRTMT)
dm$ARMCD  <- ifelse(!is.na(dm$ARMCD), dm$ARMCD, 'NOTASSGN')

dm <- merge(dm, Rdmcode[, c(1,7)], by='SUBJID', all = TRUE)
dm <- dm %>% rename(ARM = XTRTMT)
dm$ARM  <- ifelse(!is.na(dm$ARM), dm$ARM, 'Not Assigned')

dm<- merge(dm, invagt_filter_first[,c(1,13)], by='SUBJID', all.x = TRUE)
dm$ACTARM <- ifelse(!is.na(dm$DAYDATE), dm$ARM, NA)
#dm$ACTARM <- ifelse(!is.na(dm$ARM), 'Not Treated', 'Not Assigned')

dm$ACTARMCD <- ifelse(!is.na(dm$ARMCD), dm$ARMCD, 'NOTASSGN')
#dm$ACTARMCD <- ifelse(dm$ACTARM == dm$ARM, dm$ARM, 'NOTASSGN')
  
dm$COUNTRY <- "UNK"  

demog_visit <- demog %>% 
  group_by(SUBJID) %>%
  summarise(VISITDT=last(VISITDT))

demog_visit$SUBJID <- sprintf("%06d", demog_visit$SUBJID)

dm <- merge(dm, demog_visit, by='SUBJID', all = TRUE)
dm <- dm %>% rename(DMDTC = VISITDT)
dm$DMDTC <- format(dm$DMDTC, "%Y-%m-%d")

dm$DMDY <- ifelse(dm$DMDTC < dm$RFSTDTC, round(as.numeric(as.Date(dm$DMDTC) - as.Date(dm$RFSTDTC)),0), 
                  round((as.numeric(as.Date(dm$DMDTC) - as.Date(dm$RFSTDTC))+1),0) )
dm$DMDY<-as.character(dm$DMDY)

  
dm = apply_labels(dm,
                  STUDYID  =	'Study Identifier' , 
                  DOMAIN  = 'Domain Abbreviation' ,
                  USUBJID  =	'Unique Subject Identifier',  
                  SUBJID  =	'Subject Identifier for the Study',  
                  RFSTDTC  =	'Subject Reference Start Date/Time' , 
                  RFENDTC  =	'Subject Reference End Date/Time',  
                  RFXSTDTC =	"Date/Time of FirstStudy Treatment",
                  RFXENDTC =	"Date/Time of Last Study Treatment",
                  RFICDTC	= 'Date/Time of Informed Consent',
                  RFPENDTC  =	"Date/Time of End of Participation",
                  DTHDTC = 'Date/Time of Death',
                  DTHFL =	'Subject Death Flag',
                  SITEID  =	'Study Site Identifier',
                  BRTHDTC  =	'Date/Time of Birth',  
                  AGE  =	'Age',  
                  AGEU = 	'Age Units',  
                  SEX  =	'Sex',  
                  RACE  =	'Race',  
                  ETHNIC  =	'Ethnicity',  
                  ARMCD = 	'Planned Arm Code',  
                  ARM = 	'Description of Planned Arm',  
                  ACTARM = 'Description of Actual Arm',
                  ACTARMCD =	'Actual Arm Code',
                  COUNTRY  =	'Country',  
                  DMDTC = 	'Date/Time of Collection',  
                  DMDY  =	 'Study Day of Collection' 
)


dm_goal <- read_sas("E:/Mentors 1 semester/Nataliia Moiseienko/R/dm.sas7bdat")
str(dm_goal)
str(dm)

column_order <- names(dm_goal)
dm <- dm[, column_order]


mismatched_rows <- data.frame()
df1<-dm
df2<-dm_goal
df2$RFICDTC<-NA
df2$DTHDTC<-NA
df2$DTHFL<-NA
df1 <- df1[order(df1$SUBJID), ]
df2 <- df2[order(df2$SUBJID), ]

for (col in column_order) {
  mismatched_indices <- which(df1[[col]] != df2[[col]])
  
  if (length(mismatched_indices) > 0) {
    mismatched_rows <- rbind(mismatched_rows, df1[mismatched_indices, ])
  }
}

mismatched_rows


df1 <- df1 %>%
  mutate(across(where(is.labelled), as.character)) 

df2 <- df2 %>%
  mutate(across(where(is.character), as.character))

diff_dm <- anti_join(df1, df2, by = "SUBJID")
diff_dm_goal <- anti_join(df2, df1, by = "SUBJID")

print(diff_dm)
print(diff_dm_goal)

all.equal(df1, df2)
