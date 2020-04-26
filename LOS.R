# Length of stay en USI et en réanimation. 






paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)



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

difftime( temp0$OUTTIME, temp0$INTIME,unit="days")->temp0$LOS_USI
as.numeric(temp0$LOS_USI)->temp0$LOS_USI

difftime( temp0$DISCHTIME, temp0$INTIME,unit="days")->temp0$LOS_Hospital
as.numeric(temp0$LOS_Hospital)->temp0$LOS_Hospital

temp0[c("HADM_ID", "Entre_hospit","LOS_USI","LOS_Hospital")]->temp1


write.csv(x = temp1 , file = "./resultats/los.csv", row.names = FALSE)



