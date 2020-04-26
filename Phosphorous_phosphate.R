# Ce script extrait les élements phosphorous et phosphate pour voir leur chevauchement et la valeur ajouté de l'un par rapport à l'autre. 




# Extraction des paramètres à partir de CHARTEVENTS 

read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$VARIABLE == "Phosphorous")->include
c(include$ITEMID_CV, include$ITEMID_MV)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids
paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ptm <- proc.time()
CHARTEVENTS<-dbFetch(res,n=-1)
proc.time() - ptm ->tmpsec
write.table(file="./resultats/phosphorous.csv", CHARTEVENTS, sep=",")

read.table(file ="./resultats/phosphorous.csv", sep ="," , header = TRUE)->temp0



# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))



ICUSTAYS[c(  "HADM_ID"  ,  "INTIME"   )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS



# Extraction des variables d'interts à la baseline, H24 et H48

subset(temp2, temp2$TEMPS >=0 & temp2$TEMPS <=24)->temp3


# Extraction du nom des variables

include[c("VARIABLE",  "ITEMID_MV" , "ITEMID_CV")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3
merge(temp3, inc3, by.x = "ITEMID" , by.y = "value" )->temp4


cast(temp4, HADM_ID ~ VARIABLE, value = "VALUE", meanNA)->temp5





# Extraction de l'élement d'interêt à partir de 