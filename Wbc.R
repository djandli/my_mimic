# Introduction

#Comparasion des deux variables "White bllod cells" et "WBC" du fichier LABEVENTS. 




# Chargement et utilisation de quelques fonctions 

meanNA <- function(x) {
  n <- mean(x, na.rm = TRUE)
  return(n)
}



# Chargement des données de LABEVENTS. 

## Selection des données de laboratoires à partir de LABEVENTS

read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$wbc == 1)->temp1
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

merge(temp0, ADMISSIONS[c("HADM_ID", "ADMITTIME")], by = "HADM_ID", all = TRUE)->temp1
temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$ADMITTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$ADMITTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp1$CHARTTIME ,temp1$ADMITTIME, unit="hours") ->temp1$TEMPS
temp1$TABLE<-"LABEVENTS"
as.numeric(temp1$VALUE)->temp1$VALUE



cast(data = temp1, HADM_ID ~ LABEL, value = "VALUE" , meanNA)->r



# Si l'on s'interesse aux quelques études qui considérent les deux paramètres :
subset(r, !is.na(r$`WBC Count` & !is.na(r$`White Blood Cells`)))->r1


subset(temp1, temp1$HADM_ID %in% r1$HADM_ID)->temp2

subset(temp2, temp2$LABEL == "WBC Count")->a
subset(temp2, temp2$LABEL == "White Blood Cells")->b

ggplot() + 
  geom_line(data = b, aes (x = TEMPS, y = VALUE, color = LABEL, group = HADM_ID )) + 
  geom_point(data = a, aes (x = TEMPS, y = VALUE, color = LABEL )) + 
  ggtitle("Analyse de la plus value de considerer WBC Count et White Blood Cells dans LABEVENTS")->g1


tiff(filename = "./resultats/Diagnostic_wbc.tiff", res = 600, width = 10, height = 8 , units = "in") 
g1
dev.off() 

























subset(temp1, save1$TEMPS>= 0 & save1$TEMPS<24) ->temp2
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
ggplot(data = temp5, aes(x = variable , fill = value)) + geom_bar() + coord_flip()
