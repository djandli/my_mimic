# Extraction des données concernant le score de Glasgow. 



meanNA <- function(x) {
  n <- mean(x, na.rm = TRUE)
  return(n)
}

minNA <- function(x) {
  n <- min(x, na.rm = TRUE)
  return(n)
}





paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)

merge(ADMISSIONS, ICUSTAYS, by = "HADM_ID")->adms


# Chargement des données socio-démographiques
#read.table( file = "./tables_analyses/demographics_V2.csv", header = TRUE, sep = ",")->adms




# Extraction des paramètres d'interêts
read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$Glagow == 1)->include
c(include$ITEMID_CV, include$ITEMID_MV)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids

paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids

if(!file.exists("./resultats/chartevents_glasgow.csv")){
  print("Does not exist, we have to create it : might take few minutes ...")
  
  
  sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
  res <- dbSendQuery(con2, sql)
  ptm <- proc.time()
  CHARTEVENTS<-dbFetch(res,n=-1)
  proc.time() - ptm ->tmpsec
  write.table(file="./resultats/chartevents_glasgow.csv", CHARTEVENTS, sep=",")
  read.table(file ="./resultats/chartevents_glasgow.csv", sep ="," , header = TRUE)->temp0
    }else {
  
    print("No need toexctrac CHARTEVENTS data, file carrying on :  ")
      read.table(file ="./resultats/chartevents_glasgow.csv", sep ="," , header = TRUE)->temp0
    }
  
  


# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))



ICUSTAYS[c(  "HADM_ID"  ,  "INTIME"   )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS



subset(temp2, temp2$TEMPS > 0  &  temp2$TEMPS < 24 )->temp3


# Extraction du nom des variables

include[c("VARIABLE",  "ITEMID_MV" , "ITEMID_CV")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3

merge(temp3, inc3, by.x = "ITEMID" , by.y = "value" )->temp4


cast(temp4, HADM_ID ~  VARIABLE , value = "VALUE", minNA)->temp5

temp5$`Galsgow – réponse motrice` + temp5$`Glasgow – réponse verbale` + temp5$`Glasgow- ouverture des yeux` ->temp5$Glasgow 

write.csv(x = temp5 , file = "./resultats/glasgow.csv", row.names = FALSE)

