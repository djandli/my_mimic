

# Ce fichier nécéssite 
# - Une variable ids qui contient les indentifiants d'hospitalisation d'interêt 
# - Un fichier Auxilliaire_PROCEDUREEVENTS.xlsx qui contient les procédures que l'on souhaite extraire. 


read.xlsx(file="./tables auxilliaires/Auxilliaire_PROCEDUREEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->temp0
subset(temp0, temp0$INCLUDE == 1)->temp1


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
cast(temp5, HADM_ID  ~ SHORT_TITLE)->temp6

as.data.frame(cbind(1:length(admid), admid))->marge
colnames(marge)<-c("ligne", "HADM_ID")

merge(marge, temp6, by = "HADM_ID", all.x = TRUE)->temp7

temp7$`Mec ven`<-"No"

temp7$`Mec ven` [ temp7$`Cont inv mec ven <96 hrs`  >= 1 ]<-"Cont inv mec ven <96 hrs"
temp7$`Mec ven` [ temp7$`Cont inv mec ven 96+ hrs`  >= 1 ]<-"Cont inv mec ven >96 hrs"

melt(temp7, id = "HADM_ID")->temp8
write.csv (temp7, file ="./figures/procedures.csv")


write.csv (temp7, file ="./tables_analyses/procedures.csv")

