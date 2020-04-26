


# Chargement de la base de donnée

con2<-dbConnect(MySQL(),
                user='root',
                password='Arsenal001!',
                host='127.0.0.1',
                dbname='MIMIC3')



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
read.xlsx(file="./Discharge summaries/verification_pancreatite_aigue_v20190722.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$Pancreatitis == 1)->k
k$HADM_ID->admid 


subset(ICUSTAYS, ICUSTAYS$HADM_ID %in% admid)->icu 


# Chargement du fichier admissions avec nouvelle variables. 
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)

ADMISSIONS$DEAD<-0
ADMISSIONS$DEAD[ADMISSIONS$DEATHTIME != ""]<-1
write.csv(file = "./tables_analyses/death_status.csv",  x = ADMISSIONS[c( "HADM_ID" , "DEAD" )], row.names = FALSE)



