

# Ce fichier nécéssite 
# - Une variable ids qui contient les indentifiants d'hospitalisation d'interêt 
# - Un fichier Auxilliaire_PROCEDUREEVENTS.xlsx qui contient les procédures que l'on souhaite extraire. 


paste(admid,collapse = ",")->ids
sql <- paste("SELECT HADM_ID, INTIME FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)


read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_PROCEDURES.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->temp0
subset(temp0, temp0$script1 == 1)->temp1


paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM  PROCEDURES_ICD WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
PROCEDURES_ICD  <-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM D_ICD_PROCEDURES  ",  " ",  sep=" " )
res <- dbSendQuery(con2, sql)
D_ICD_PROCEDURES  <-dbFetch(res,n=-1)



merge(PROCEDURES_ICD, D_ICD_PROCEDURES, by = "ICD9_CODE")->temp3

temp3[c("ICD9_CODE"  ,  "HADM_ID"  ,  "SEQ_NUM" ,  "SHORT_TITLE" , "LONG_TITLE" )]->temp4

subset(temp4, temp4$ICD9_CODE %in% temp1$ICD9_CODE)->temp5

temp5$SHORT_TITLE[temp5$SHORT_TITLE %in% c("Ven cath renal dialysis","Dialysis arteriovenostom", "Hemodialysis","Peritoneal dialysis")]<-"Dialysis"
temp5$SHORT_TITLE[temp5$SHORT_TITLE %in% c("Cont inv mec ven <96 hrs", "Cont inv mec ven 96+ hrs")]<-"MV"

cast(temp5, HADM_ID  ~ SHORT_TITLE)->temp6

merge(ICUSTAYS[c("HADM_ID")], temp6, by = "HADM_ID", all.x = TRUE)->temp7

temp7$Dialysis[temp7$Dialysis >= 1 & !is.na(temp7$Dialysis)]<-"yes"
temp7$Dialysis[temp7$Dialysis == 0| is.na(temp7$Dialysis)]<-"no"

temp7$MV[temp7$MV >= 1 & !is.na(temp7$MV)]<-"yes"
temp7$MV[temp7$MV == 0| is.na(temp7$MV)]<-"no"

temp7$`Non-invasive mech vent`[temp7$`Non-invasive mech vent` >= 1 & !is.na(temp7$`Non-invasive mech vent`)]<-"yes"
temp7$`Non-invasive mech vent`[temp7$`Non-invasive mech vent` == 0| is.na(temp7$`Non-invasive mech vent`)]<-"no"

write.csv (temp7, file ="./resultats/MecVenAndDialysis.csv")



