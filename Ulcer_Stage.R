print("Ce script analyse les données sur le Peptic Ulcer Stage")







quit() 
exit; 




# tout d'abord, on commence par extraire le script R contenant les données de patients à extraire. 

## Le script permet de générer la table d'admission et les patients d'interêt (vecteur : admid)
source("./SCRIPTS/script_introductif.R")

# Pressure Ulcer Stage n'est disponible que pour les patients suivi en utilisant le système MetaVision. 
# Cela correspon aux items "224965" et "224631"



ITEMID<-c("224965", "224631")

ITEMID[!is.na(ITEMID)]->itemids
paste(itemids,collapse = ",")->items
paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ptm <- proc.time()
CHARTEVENTS<-dbFetch(res,n=-1)
proc.time() - ptm ->tmpsec
write.table(file="./tables_analyses/chartevents_PUS.csv", CHARTEVENTS, sep=",")



read.table(file ="./tables_analyses/chartevents_PUS.csv", sep ="," , header = TRUE)->temp0




# Tranformation de la donnée CHARTTIME en une donnée temporelle. 
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


# Création d'une donnée temps relative (comparée à l'heure d'entrée au SI)
ICUSTAYS[c(  "HADM_ID"  ,  "INTIME"   )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS





# Création d'une variable temps catégorielle. 
temp2$CAT_TEMPS<-NA
temp2$CAT_TEMPS[temp2$TEMPS >= 0 & temp2$TEMPS < 7*24]<-"S1"
temp2$CAT_TEMPS[temp2$TEMPS >= 7*24 & temp2$TEMPS < 14*24]<-"S2"
temp2$CAT_TEMPS[temp2$TEMPS >= 14*24 & temp2$TEMPS < 21*24]<-"S3"
temp2$CAT_TEMPS[temp2$TEMPS >= 21*24 & temp2$TEMPS < 28*24]<-"S4"
temp2$CAT_TEMPS[temp2$TEMPS >= 28*24 ]<-"S4+"


# Selection des variables d'interêts
var<-c( "HADM_ID","CHARTTIME","VALUENUM","VALUEUOM","INTIME","TEMPS","CAT_TEMPS")



# Création du fichier de resultats

write.csv (temp2[var], file ="./tables_analyses/PUS.csv")


