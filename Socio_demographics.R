
# Ce script nécéssite l'ouvertyre d'une aray admid. 





paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)



paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)


ADMISSIONS$SUBJECT_ID->pid
paste(pid,collapse = ",")->pid2

sql <- paste("SELECT * FROM PATIENTS WHERE SUBJECT_ID IN  (", pid2, ")", sep=" " )
res <- dbSendQuery(con2, sql)
PATIENTS<-dbFetch(res,n=-1)



# A partir des identifiants fournis ce script génère un fichier


merge(ADMISSIONS[c( "HADM_ID","ADMITTIME" , "DISCHTIME", "DEATHTIME", "EDREGTIME", "EDOUTTIME"    )], ICUSTAYS [c( "HADM_ID"      , "ICUSTAY_ID","INTIME","OUTTIME","LOS" )], by = "HADM_ID")->temp0  
temp0$ADMITTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$ADMITTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp0$DISCHTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$DISCHTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

temp0$INTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

temp0$OUTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$OUTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp0$ADMITTIME, temp0$INTIME ,unit="hours")->temp0$Entre_hospit
as.numeric(temp0$Entre_hospit)->temp0$Entre_hospit


difftime(  temp0$ADMITTIME, temp0$INTIME ,unit="hours")->temp0$Entre_hospit
as.numeric(temp0$Entre_hospit)->temp0$Entre_hospit

difftime( temp0$OUTTIME, temp0$INTIME,unit="hours")->temp0$Sortie_usi
as.numeric(temp0$Sortie_usi)->temp0$Sortie_usi

difftime( temp0$DISCHTIME, temp0$INTIME,unit="hours")->temp0$Sortie_hospit
as.numeric(temp0$Sortie_hospit)->temp0$Sortie_hospit


temp0$Sortie_usi ->temp0$Delta_USI

temp0$Sortie_hospit- temp0$Entre_hospit ->temp0$Delta_Hospit

temp0$DEAD_TIME<-NA

temp0$DEATHTIME[temp0$DEATHTIME != ""]->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
difftime( chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s')), temp0$INTIME[temp0$DEATHTIME != ""],unit="hours")->temp0$DEAD_TIME[temp0$DEATHTIME != ""]


as.numeric(temp0$DEAD_TIME)->temp0$DEAD_TIME


temp0->temp1





# Enregistrement de ces données pour l'analyse. 


merge(temp1, ADMISSIONS[c("HADM_ID", "SUBJECT_ID" , "ADMISSION_TYPE","ADMISSION_LOCATION","DISCHARGE_LOCATION", "INSURANCE","LANGUAGE","RELIGION","MARITAL_STATUS","ETHNICITY")],
      by = "HADM_ID")->temp2
merge(temp2, PATIENTS [c("SUBJECT_ID","GENDER","DOB","DOD","DOD_HOSP","DOD_SSN")] , by = "SUBJECT_ID")->temp3  
difftime(  temp3$INTIME,  temp3$DOB ,unit="days")/365.25->temp3$AGE
as.numeric(temp3$AGE)->temp3$AGE


# On met l'age en variable catégorielle car des patients ont plus de 93 ans. 

temp3$AGE_cat<-NA
temp3$AGE_cat[temp3$AGE < 50]<-"Less than 50"
temp3$AGE_cat[temp3$AGE >= 50 & temp3$AGE < 65 ]<-"Between 50 and 65"
temp3$AGE_cat[temp3$AGE >=65 ]<-"more than  65"



# Modification de la variable ethnicity

temp3$ETHNICITY[temp3$ETHNICITY %in% c( "WHITE" , "WHITE - RUSSIAN" , "MIDDLE EASTERN" ) ]<-"Caucasian"
temp3$ETHNICITY[temp3$ETHNICITY %in%  c("BLACK/AFRICAN AMERICAN" , "BLACK/AFRICAN" , "BLACK/CAPE VERDEAN"    )]<-"African American"
temp3$ETHNICITY[!(temp3$ETHNICITY %in%  c("African American" , "Caucasian"))]<-"Other"

# Modification de la variable marital status

temp3$MARITAL_STATUS[temp3$MARITAL_STATUS %in% c(  "MARRIED"  ) ]<-"Married"
temp3$MARITAL_STATUS[temp3$MARITAL_STATUS %in%  c( "SINGLE"     )]<-"Single"
temp3$MARITAL_STATUS[temp3$MARITAL_STATUS %in%  c( "DIVORCED", "SEPARATED")]<-"Separated"
temp3$MARITAL_STATUS[!(temp3$MARITAL_STATUS %in%  c("Separated", "Single", "Married"))]<-"Other"






# Il y'a deux dossier ou les heures des decès sont mal enregistrée. 
# En cas d'heure de décès négative, on assume que l'heure de décès correspond à l'heure de sortie de réanimation.


write.csv(x = temp3, file = "./resultats/demographics_V3.csv", row.names = FALSE)




print("Le fichier demographics_V3.csv vient d'être créer dans le fichier resultats repertoire local. ")




## Script diagnostic, 
# On vérifie que les heures de décès sont postérieures l'heure d'arrrivée 








