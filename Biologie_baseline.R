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

## Selection des données de laboratoires à partir de LABEVENTS
sql <- paste("SELECT * FROM LABEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
LABEVENTS<-dbFetch(res,n=-1)
sql <- paste("SELECT * FROM D_LABITEMS "  ,  sep=" " )
res <- dbSendQuery(con2, sql)
D_LABITEMS<-dbFetch(res,n=-1)
merge(LABEVENTS, D_LABITEMS, by = "ITEMID")->temp0

merge(temp0, ICUSTAYS[c("HADM_ID", "INTIME")], by = "HADM_ID", all = TRUE)->temp1
temp1$INTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp1$CHARTTIME ,temp1$INTIME, unit="hours") ->temp1$TEMPS
temp1$TABLE<-"LABEVENTS"


subset(temp1, temp1$TEMPS>= -6 & temp1$TEMPS<24) ->temp2
as.numeric(as.character(temp2$VALUE))->temp2$VALUE 
cast(temp2, HADM_ID ~ LABEL , value = "VALUE", fun.aggregate = meanNA)->temp3


# Fusion avec le fichier ADMISSIONS pour être sur de ne pas avoir de données manquantes 

merge(temp3, ADMISSIONS[c("HADM_ID")], all = TRUE)->temp4





write.csv(x = temp4 , file = "./resultats/biologie_baseline.csv", row.names = FALSE)




# Génération des figures diagnostiques. 

### Visulation des données manquantes 

temp4[is.na(temp4)]<-"Missing"
temp4[temp4 != "Missing"]<-"Not missing"
melt(temp4, id = "HADM_ID")->temp5
ggplot(data = temp5, aes(x = variable , fill = value)) + geom_bar() + coord_flip()->g1


tiff(filename = "./resultats/Diagnostic_biologie_baseline.tiff", res = 600, width = 10, height = 8 , units = "in") 
g1
dev.off() 





