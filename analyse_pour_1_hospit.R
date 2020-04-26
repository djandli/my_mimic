# Analyse du patient 191893 


read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$labo_standard == 1)->include

c(include$ITEMID_CV, include$ITEMID_MV, include$ITEMID_CV2, include$ITEMID_CV3)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids
paste(itemids,collapse = ",")->items
paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids
ids<-"191893"
sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ptm <- proc.time()
CHARTEVENTS<-dbFetch(res,n=-1)
proc.time() - ptm ->tmpsec


# write.table(file="./tables_analyses/charteventsBIO.csv", CHARTEVENTS, sep=",")
# 
# 
# 
# read.table(file ="./tables_analyses/charteventsBIO.csv", sep ="," , header = TRUE)->temp0



merge(ADMISSIONS)
subset(ADMISSIONS, ADMISSIONS$HADM_ID == 113780)


subset(ICUSTAYS, ICUSTAYS$HADM_ID == 124045)



merge(ICUSTAYS[c(  "HADM_ID","ICUSTAY_ID","FIRST_CAREUNIT",    "LAST_CAREUNIT"  ,  "INTIME","OUTTIME" )], 
      ADMISSIONS[c("HADM_ID","ADMITTIME","DISCHTIME")], by = c("HADM_ID"))->a


subset( a [c("HADM_ID","ICUSTAY_ID","FIRST_CAREUNIT",  "LAST_CAREUNIT" ,"ADMITTIME", "INTIME","OUTTIME","DISCHTIME" )], 
        a$HADM_ID == 197736) 



