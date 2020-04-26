
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

sql <- paste("SELECT * FROM D_ITEMS", sep=" " )
res <- dbSendQuery(con2, sql)
D_ITEMS<-dbFetch(res,n=-1)


# Extraction des paramètres d'interêts
read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_D_ITEMS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1 )->ty
subset(ty, ty$Morpho == 1)->include
include$ITEMID->ITEMID

ITEMID[!is.na(ITEMID)]->itemids

paste(itemids,collapse = ",")->items



if(!file.exists("./resultats/chartevents_morpho.csv")){
  print("Does not exist, we have to create it : might take few minutes ...")
  
  
  paste(admid,collapse = ",")->ids
  sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
  res <- dbSendQuery(con2, sql)
  ptm <- proc.time()
  CHARTEVENTS<-dbFetch(res,n=-1)
  proc.time() - ptm ->tmpsec
  write.table(file="./resultats/chartevents_morpho.csv", CHARTEVENTS, sep=",")
  read.table(file ="./resultats/chartevents_morpho.csv", sep ="," , header = TRUE)->temp0
  
}else {
  
  print("No need to exctrac CHARTEVENTS data, file carrying on :  ")
  read.table(file ="./resultats/chartevents_morpho.csv", sep ="," , header = TRUE)->temp0
}





# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))



ICUSTAYS[c(  "HADM_ID"  ,  "INTIME"   )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS


temp2->temp3


# Extraction du nom des variables

include[c("VARIABLE","ITEMID")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "variable",  "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3

merge(temp3, inc3, by.x = "ITEMID" , by.y = "value" )->temp4












# Visualisaiton de la ditribution des paramètres dans Metavision et CareVue.

pdf(file = "./resultats/Diagnostic_morpho.pdf", height = 5 , width = 10,  onefile = TRUE)


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


# Modification des résultats pour ne garder que le poids en kg et la taille en cm. 

temp4$VARIABLE[temp4$ITEMID == 920]<-"Taille (inches)"


subset(temp4, temp4$VALUE != 0)->temp5
cast(temp5[c("HADM_ID", "VARIABLE" , "VALUE")], HADM_ID ~ VARIABLE, value = "VALUE", fun.aggregate = meanNA) ->temp6


temp6$`Poids (pounds)`[is.na(temp6$`Poids (kg)`) & !is.na(temp6$`Poids (pounds)`)]/ 2.205-> 
  temp6$`Poids (kg)`[is.na(temp6$`Poids (kg)`) & !is.na(temp6$`Poids (pounds)`)]



temp6$`Taille (inches)` [is.na(temp6$`Taille (cm)`) & !is.na(temp6$`Taille (inches)`)]*2.54 ->
  temp6$`Taille (cm)` [is.na(temp6$`Taille (cm)`) & !is.na(temp6$`Taille (inches)`)]




# Après fusion, on ne selectionne que les paramètres d'interêt

temp6[c(  "HADM_ID","Poids (kg)","Taille (cm)")] ->temp7




write.csv(x = temp7 , file = "./resultats/clinique_morpho.csv", row.names = FALSE)
temp7->temp6


# Génération d'une  diagnostique  pour analyser les données manquantes. 

temp6[is.na(temp6)]<-"Missing"
temp6[!(colnames(temp6) %in% c("HADM_ID"))]->temp7
temp7[temp7 != "Missing"]<-"Not missing"
cbind(temp6[c("HADM_ID")] , temp7)->temp8


melt(temp8, id = "HADM_ID")->temp9
ggplot(data = temp9, aes(x = variable , fill = value)) + geom_bar() + coord_flip() + 
  scale_fill_manual(values=c("darkgreen", "red"))->g1


tiff(filename = "./resultats/Diagnostic_morpho.tiff", res = 600, width = 10, height = 8 , units = "in") 
g1
dev.off() 


