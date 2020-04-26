
# tout d'abord, on commence par extraire le script R contenant les données de patients à extraire. 

## Le script permet de générer la table d'admission et les patients d'interêt (vecteur : admid)


# Fonction meanNA

meanNA <- function(x) {
  n <- mean(x, na.rm = TRUE)
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
read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_D_ITEMS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1 )->ty
subset(ty, ty$Clinique1 == 1)->include
include$ITEMID->ITEMID
ITEMID[!is.na(ITEMID)]->itemids
paste(itemids,collapse = ",")->items
paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ptm <- proc.time()
CHARTEVENTS<-dbFetch(res,n=-1)
proc.time() - ptm ->tmpsec
write.table(file="./resultats/chartevents_clinique1.csv", CHARTEVENTS, sep=",")



read.table(file ="./resultats/chartevents_clinique1.csv", sep ="," , header = TRUE)->temp0



# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))



ICUSTAYS[c(  "HADM_ID"  ,  "INTIME"   )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS


# Extraction des données à la baseline. 
subset(temp2, temp2$TEMPS> -6 & temp2$TEMPS <=24)->temp3




# Extraction du nom des variables

include[c("VARIABLE",  "ITEMID")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "variable",  "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3

merge(temp3, inc3, by.x = "ITEMID" , by.y = "value" )->temp4

subset(temp4, !is.na(temp4$VARIABLE) )->temp4b    # On supprime la ligne vide
temp4b->temp4











# Visualisaiton de la ditribution des paramètres dans Metavision et CareVue.

pdf(file = "./resultats/Diagnostic_clinical_baseline.pdf", height = 5 , width = 10,  onefile = TRUE)


for (j in 1:length(unique(temp4$VARIABLE))){  
  
  print(j)
  
  unique(temp4$VARIABLE)[j]->u 
  
  subset(temp4, temp4$VARIABLE == u)->sub0
  as.character(sub0$ITEMID)->sub0$ITEMID
  gsub(u, pattern = "é", replacement =  "e")->v
  gsub(u, pattern = "l\\’", replacement =  "")->v
  ggplot(data = sub0, aes(x = VALUE, fill = ITEMID, color = ITEMID)) + geom_density()  + facet_grid(. ~ variable) + 
          ggtitle(as.character(v))  + theme(legend.title = element_blank()) ->g1
  
  
  ggplot(data = sub0, aes(y = VALUE, x= ITEMID, fill = ITEMID, color = ITEMID)) + geom_boxplot()  + 
    ggtitle(as.character(v))->g2
  
 print( grid.arrange(g1, g2, nrow = 1 , ncol = 2))
  
}


dev.off() 


# On peut supprimer des données aberrantes si l'on en constate dans la figure diagnostique; 
## On voit que certaines valeurs de NIBP mean sont égales à 9999.99
## Il s'agit probablement de données manquantes. 

temp4$VALUE[temp4$VALUE == "9999.99"]<-NA

# Dans ce script, aucune des valeurs ne doit être égale à 0 !
temp4$VALUE[temp4$VALUE == 0]<-NA

## on peut choisir ici de relancer manuellement le diagnostic; 
temp4$VALUE[temp4$VARIABLE == "Artérielle Diastolique" & temp4$VALUE > 300 & !is.na(temp4$VALUE)]<-NA
temp4$VALUE[temp4$VARIABLE == "Température (F)" & temp4$VALUE  <50 & !is.na(temp4$VALUE)]<-NA
temp4$VALUE[temp4$VARIABLE == "Artérielle Pression artérelle moyenne" & temp4$VALUE <30  & !is.na(temp4$VALUE)]<-NA





# Poursuite de l'extraction 
cast(temp4, HADM_ID ~ VARIABLE , value = "VALUE", meanNA)->temp5





## 2) Si absence de données de presion non-invasive, on recherche la pression invasive; 

#### Systolique

temp5$`Artérielle Systolique`[is.na(temp5$`NIBP systolique`) & !is.na(temp5$`Artérielle Systolique`)]->
  temp5$`NIBP systolique`[is.na(temp5$`NIBP systolique`) & !is.na(temp5$`Artérielle Systolique`)]

#### Diastolique

temp5$`Artérielle Diastolique`[is.na(temp5$`NIBP diastolique`) & !is.na(temp5$`Artérielle Diastolique`)]->
  temp5$`NIBP diastolique`[is.na(temp5$`NIBP diastolique`) & !is.na(temp5$`Artérielle Diastolique`)]

#### Moyenne

temp5$`Artérielle Pression artérelle moyenne`[is.na(temp5$`NIBP mean`) & !is.na(temp5$`Artérielle Pression artérelle moyenne`)]->
  temp5$`NIBP mean`[is.na(temp5$`NIBP mean`) & !is.na(temp5$`Artérielle Pression artérelle moyenne`)]

## 3) Temperature 

temp5$`Température (C)`[temp5$`Température (C)` == 0 & !is.na(temp5$`Température (C)`)]<-NA
(temp5$`Température (F)`[is.na(temp5$`Température (C)`)]-32)*5/9->temp5$`Température (C)`[is.na(temp5$`Température (C)`)]





# Après fusion, on ne selectionne que les paramètres d'interêt

temp5[c(   "HADM_ID","Fréquence cardiaque","Fréquence respiratoire","NIBP diastolique","NIBP mean","NIBP systolique",
           "Température (C)")] ->temp6




write.csv(x = temp6 , file = "./resultats/clinique_baseline.csv", row.names = FALSE)



# Génération d'une  diagnostique  pour analyser les données manquantes. 

temp6[is.na(temp6)]<-"Missing"
temp6[!(colnames(temp6) %in% c("HADM_ID"))]->temp7
temp7[temp7 != "Missing"]<-"Not missing"
cbind(temp6[c("HADM_ID")] , temp7)->temp8


melt(temp8, id = "HADM_ID")->temp9
ggplot(data = temp9, aes(x = variable , fill = value)) + geom_bar() + coord_flip() + 
  scale_fill_manual(values=c("darkgreen", "red"))->g1


tiff(filename = "./resultats/Diagnostic_clinique_baseline.tiff", res = 600, width = 10, height = 8 , units = "in") 
g1
dev.off() 


