# Données biologiques à la baseline, H24 et H48. 


## Certaines données biologiques ne sont pas présentes dans la table LABEVENTS, il est donc pertinent de tenter d'extraire les informations 
# à partir de la table CHARTEVENTS également. Il est important de garder à l'esprit que les informations de données d'hosptialisation multiples
# en réanimation ne sont pas mentionnées. 



# Chargement et utilisation de quelques fonctions 

meanNA <- function(x) {
  n <- mean(x, na.rm = TRUE)
  return(n)
}



# Chargement des données de LABEVENTS. 

## Selection des données de laboratoires à partir de LABEVENTS

read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$Inclure == 1)->temp1
temp1$ITEMID->listeids
paste(listeids,collapse = ",")->items
paste(admid,collapse = ",")->ids




## On selectionne la table ADMISSIONS dont on aura besoin pour les temps. 
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)

## On selectionne la table ICUSTAYS  dont on aura besoin pour les temps. 
sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)

## Selection des données de laboratoires à partir de LABEVENTS
sql <- paste("SELECT * FROM LABEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
LABEVENTS<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM D_LABITEMS "  ,  sep=" " )
res <- dbSendQuery(con2, sql)
D_LABITEMS<-dbFetch(res,n=-1)


# Fusionner avec le fichier auxillaire pour avoir le nom des variables
merge(LABEVENTS, temp1, by = "ITEMID")->temp0



# On retire les paramètres biologiques sans données temporelles 
subset(temp0, !is.na(temp0$CHARTTIME))->temp1


# Ajout de l'entrée en USI. 
merge(temp1, ICUSTAYS[c("HADM_ID", "INTIME")], by = "HADM_ID", all = TRUE)->temp2
temp2$INTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp2$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp2$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp2$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp2$CHARTTIME ,temp2$INTIME, unit="hours") ->temp2$TEMPS
temp2$TABLE<-"LABEVENTS"
subset(temp2, save1$TEMPS>= 0 & save1$TEMPS<24) ->temp2




as.numeric(as.character(temp2$VALUE))->temp2$VALUE 
cast(temp2, HADM_ID ~ LABEL , value = "VALUE", fun.aggregate = meanNA)->temp3


# Fusion avec le fichier ADMISSIONS pour être sur de ne pas avoir de données manquantes 

merge(temp3, ADMISSIONS[c("HADM_ID")], all = TRUE)->temp4
temp4->saved1



# Visualisation des données manquantes
saved1[is.na(saved1)]<-"Missing"
saved1[!(colnames(saved1) %in% c("HADM_ID"))]->temp6
temp6[temp6 != "Missing"]<-"Not missing"
cbind(saved1[c("HADM_ID")] , temp6)->temp7
melt(temp7, id = "HADM_ID")->temp8

temp8->tempA

# ggplot(data = temp8, aes(x = variable , fill = value)) + geom_bar() + coord_flip()






################################################################################"


read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$Labo == 1)->include
c(include$ITEMID_CV, include$ITEMID_MV)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids
paste(itemids,collapse = ",")->items
paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ptm <- proc.time()
CHARTEVENTS<-dbFetch(res,n=-1)
proc.time() - ptm ->tmpsec
write.table(file="./resultats/chartevents_labo.csv", CHARTEVENTS, sep=",")



read.table(file ="./resultats/chartevents_labo.csv", sep ="," , header = TRUE)->temp0



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





# Visualisation des données manquantes
temp5[is.na(temp5)]<-"Missing"
temp5[!(colnames(temp5) %in% c("HADM_ID"))]->temp6
temp6[temp6 != "Missing"]<-"Not missing"
cbind(temp5[c("HADM_ID")] , temp6)->temp7
melt(temp7, id = "HADM_ID")->temp8
temp8->tempB







# Fusion des deux fichiers et visualisation de ce que l'on gagne
tempA$FILE<-"LABEVENTS"
tempB$FILE<-"CHARTEVENTS"

rbind(tempA, tempB)->tempC
# cast(tempC, HADM_ID + variable ~ FILE)->v
cast(tempC, HADM_ID + variable ~ FILE)->tempD
tempD$CHARTEVENTS[is.na(tempD$CHARTEVENTS) ]<-"Missing"
tempD$LABEVENTS[is.na(tempD$LABEVENTS) ]<-"Missing"

tempD$TABLE<-NA

tempD$TABLE[tempD$CHARTEVENTS == "Missing" & tempD$LABEVENTS == "Missing"]<-"Totally missing"
tempD$TABLE[tempD$CHARTEVENTS != "Missing" & tempD$LABEVENTS == "Missing"]<-"CHARTEVENTS"
tempD$TABLE[tempD$CHARTEVENTS == "Missing" & tempD$LABEVENTS != "Missing"]<-"LABEVENTS"
tempD$TABLE[tempD$CHARTEVENTS != "Missing" & tempD$LABEVENTS != "Missing"]<-"Both"



ggplot(data = tempD, aes(x = variable , fill = TABLE)) + geom_bar() + coord_flip() + 
  scale_fill_manual(values=c("darkgreen", "yellow","orange", "red"))->g1


tiff(filename = "./resultats/Diagnostic_chart_VS_lab.tiff", res = 600, width = 10, height = 8 , units = "in") 
g1
dev.off() 


















