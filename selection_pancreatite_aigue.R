


# Chargement de la base de donnée

con2<-dbConnect(MySQL(),
                user='root',
                password='new_password',
                host='127.0.0.1',
                dbname='MIMICIII')



#  Extraction des données concernant les patients une pancréatite aigue


sql <- paste("SELECT * FROM DIAGNOSES_ICD", sep=" " )
res <- dbSendQuery(con2, sql)
DIAGNOSES_ICD<-dbFetch(res,n=-1)


subset(DIAGNOSES_ICD, DIAGNOSES_ICD$ICD9_CODE == 5770)->temp0
unique(temp0$HADM_ID)->admid2
length(unique(temp0$HADM_ID))


paste(admid2,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)

unique(ADMISSIONS$SUBJECT_ID)->patient_id
paste(patient_id,collapse = ",")->pids
sql <- paste("SELECT * FROM PATIENTS WHERE SUBJECT_ID IN  (", pids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
PATIENTS<-dbFetch(res,n=-1)


# as.data.frame(table(ICUSTAYS$HADM_ID))->o
# as.numeric(as.character(o$Var1[o$Freq == 1]))->admid2
# 
# 
# as.numeric(as.character(o$Var1[o$Freq == 1]))->admid2
# admid2->admid

#On reextrait les fichiers avec le nouveax ids
paste(admid2,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)

unique(ADMISSIONS$SUBJECT_ID)->patient_id
paste(patient_id,collapse = ",")->pids
sql <- paste("SELECT * FROM PATIENTS WHERE SUBJECT_ID IN  (", pids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
PATIENTS<-dbFetch(res,n=-1)


# On selectionne les 614 patients présnetant une pancréatite avérée. 

library(googlesheets)
url<-"https://docs.google.com/spreadsheets/d/1ljAp-leaGzXuMP7DhlHhET2j-XzMwMeUJQtbJPYg6Wg/edit?usp=sharing"
read_sheet(ss = url, sheet = "Sheet1" )->ty2
ty2->ty

#read.xlsx(file="./Discharge summaries/verification_pancreatite_aigue_v20190722.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$include == 1)->k
k$HADM_ID->admid 

# On selectionne les patients admis depuis les urgences. 

con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")


paste(admid,collapse = ",")->ids
sql <- paste("select * from mimic3.admissions where  hadm_id in (", ids, ") ", sep=" " )
temp0 <- dbGetQuery(con, sql)
dbDisconnect(con)


subset(temp0, temp0$admission_type == "EMERGENCY")->temp1



print("##################################")
print(" L'enseble des identifiants d'interêt sont maintenant dans le vecteur admid")
print("##################################")



