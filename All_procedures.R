# Ce script extrait toutes les procédures possible 


# Ce fichier nécéssite 
# - Une variable ids qui contient les indentifiants d'hospitalisation d'interêt 


paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM  PROCEDURES_ICD WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
PROCEDURES_ICD  <-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM D_ICD_PROCEDURES  ",  " ",  sep=" " )
res <- dbSendQuery(con2, sql)
D_ICD_PROCEDURES  <-dbFetch(res,n=-1)


merge(PROCEDURES_ICD, D_ICD_PROCEDURES, by = "ICD9_CODE")->temp3

temp3[c("ICD9_CODE"  ,  "HADM_ID"  ,  "SEQ_NUM" ,  "SHORT_TITLE" , "LONG_TITLE" )]->temp4
temp4$value<-1
cast( data= temp4, HADM_ID ~ LONG_TITLE     )
cast( data= temp4, HADM_ID ~ LONG_TITLE     )->temp5
temp5[, -1] [temp5[, -1] != 0]<-"yes"
temp5[, -1] [temp5[, -1] != "yes"]<-"no"
write.csv (temp5, file ="./tables_analyses/All_procedures.csv")


print("Ce script vient de générer le fichier : All_procedures.csv  ")
print("Contient toutes les données de codage de patients.   ")