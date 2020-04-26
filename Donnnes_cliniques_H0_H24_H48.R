
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
read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$INCLURE.. == 1)->include
c(include$ITEMID_CV, include$ITEMID_MV)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids
# paste(itemids,collapse = ",")->items
# paste(itemids,collapse = ",")->items
# paste(admid,collapse = ",")->ids
# sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
# res <- dbSendQuery(con2, sql)
# ptm <- proc.time()
# CHARTEVENTS<-dbFetch(res,n=-1)
# proc.time() - ptm ->tmpsec
# write.table(file="./resultats/chartevents1.csv", CHARTEVENTS, sep=",")



read.table(file ="./resultats/chartevents1.csv", sep ="," , header = TRUE)->temp0



# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))



ICUSTAYS[c(  "HADM_ID"  ,  "INTIME"   )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS



# Extraction des variables d'interts à la baseline, H24 et H48

temp2$CAT_TEMPS<-NA
temp2$CAT_TEMPS[temp2$TEMPS >-8 & temp2$TEMPS <8]<-"Baseline"
temp2$CAT_TEMPS[temp2$TEMPS >16 & temp2$TEMPS <32]<-"H24"
temp2$CAT_TEMPS[temp2$TEMPS >40 & temp2$TEMPS <56]<-"H48"
subset(temp2, temp2$CAT_TEMPS %in% c("Baseline" , "H24" , "H48"))->temp3


# Extraction du nom des variables

include[c("VARIABLE",  "ITEMID_MV" , "ITEMID_CV")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3

merge(temp3, inc3, by.x = "ITEMID" , by.y = "value" )->temp4


cast(temp4, HADM_ID + VARIABLE ~ CAT_TEMPS, value = "VALUE", meanNA)->temp5



write.csv(x = temp5 , file = "./resultats/chart_3times.csv", row.names = FALSE)




# 
# 
# 
# 
# 
# as.data.frame(cbind(unique(temp4$HADM_ID), 1:length(unique(temp4$HADM_ID))))->ert
# merge(temp4, ert, by.x = "HADM_ID", by.y = "V1")->temp4
# 
# 
# 
# merge(temp4, ADMISSIONS[c("HADM_ID", "ADMITTIME")], by = "HADM_ID" )->temp2
# 
# #head(temp2)
# 
# as.character(temp2$ADMITTIME)->temp2$ADMITTIME
# temp2$ADMITTIME->times
# dtparts = t(as.data.frame(strsplit(times,' ')))
# row.names(dtparts) = NULL
# temp2$ADMITTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
# 
# 
# 
# difftime(temp2$CHARTTIME, temp2$ADMITTIME,unit="hours")->temp2$TEMPS 
# as.numeric(temp2$TEMPS)->temp2$TEMPS
# 
# 
# # On ajoute la source de la base de donnée via notre travail antérieure 
# read.table(file = "./tables_analyses/demographics.csv", sep = ",",header = TRUE ) ->adms
# 
# merge(temp2, adms[c("HADM_ID", "DBSOURCE")], by = "HADM_ID" )->temp3
# 
# 
# 
# read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
# 
# # Visualisation des paramètres extraits 
# 
# 
# # Visualisation des variables catégorielles
# 
# variables <- c(   "Pain Present","Pain Assessment Method","Pain Location","Glasgow – réponse verbale","Galsgow – réponse motrice","Pain Management","Pain Type","Pain Level Acceptable","Pain Cause","Pain Level","Daily Wake Up","Daily Wake Up Deferred","Gaz","Untoward Effect","Pain Level Response","Riker-SAS Scale","Pressure Ulcer Stage \\#1","Pressure Ulcer Stage \\#2" ) 
# 
# 
# 
# temp3->temp4
# temp4$CAT_TEMPS<-NA
# temp4$CAT_TEMPS[temp4$TEMPS < 3]<-"baseline"
# temp4$CAT_TEMPS[temp4$TEMPS < 28 & temp4$TEMPS >= 20]<-"24 hours"
# temp4$CAT_TEMPS[temp4$TEMPS < 52 & temp4$TEMPS >= 44]<-"48 hours"
# 
# subset(temp4, temp4$CAT_TEMPS != "NA")->temp5
# 
# 
# temp5[c("HADM_ID", "VARIABLE",  "VALUE","VALUENUM","VALUEUOM","CAT_TEMPS")]->temp6
# melt(temp6, id = c("HADM_ID", "VARIABLE" , "CAT_TEMPS"))->temp7
# 
# write.csv(temp7 , file = "./tables_analyses/clinical.csv"  , row.names = FALSE)
# 
# 
# 
# variables[5]->j
# subset(temp3, temp3$VARIABLE == j)->po 
# 
# 
# 
# #  png(file = paste("./figures/repartition_acqui/", j, "1.png"), height = 10, width = 10, units = "in", res = 150)
# 
# po[c("TEMPS","DBSOURCE","VALUE","VALUENUM" , "V2")]->po1
# 
# ggplot(data = po1, aes (x = TEMPS, y = V2, col = DBSOURCE)) + geom_point() ->g0
# g0
# as.character(po1$VALUE)->po1$VALUE
# ggplot(data = po1) + geom_bar(aes (x = VALUE, fill = DBSOURCE)) ->g1
# as.character(po1$VALUENUM)->po1$VALUENUM
# ggplot(data = po1) + geom_bar(aes (x = VALUENUM, fill = DBSOURCE)) ->g2
# 
# 
# 
# t <- textGrob(j)
# grid.arrange(g1, g2, ncol=2 )->g3
# grid.arrange(g0, g3, nrow=2 )->g4
# 
# 
# 
# 
# 
# # Extraction des constantes à baseline, 24 et 48 heures. 
# 
# po->temp4
# temp4$CAT_TEMPS<-NA
# temp4$CAT_TEMPS[temp4$TEMPS < 3]<-"baseline"
# temp4$CAT_TEMPS[temp4$TEMPS < 28 & temp4$TEMPS >= 20]<-"24 hours"
# temp4$CAT_TEMPS[temp4$TEMPS < 52 & temp4$TEMPS >= 44]<-"48 hours"
# 
# subset(temp4, temp4$CAT_TEMPS != "NA")->temp5
# 
# 
# 
# # Valeurs moyennes par patient
# 
# cast(temp5,  HADM_ID + CAT_TEMPS ~ VARIABLE , value = "VALUENUM", mean)->variableM
# melt(variableM, id = c("HADM_ID" ,"CAT_TEMPS"))->temp7  
# as.character(temp7$VARIABLE)->temp7$VARIABLE
# 
# read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
# 
# as.character(ty$Type [ty$VARIABLE == unique(temp7$VARIABLE) & !is.na(ty$VARIABLE)])->o
# 
# # if (o == "categorielle ordinale") {   
# # ggplot( data = temp7) +  geom_bar(aes(x = value, fill = CAT_TEMPS)) ->g6
# #  }
# # 
# # if (o == "numerique") {   
# ggplot( data = temp7) +  geom_density(aes(x = value, fill = CAT_TEMPS)) ->g6
# # }
# 
# 
# #Nombre de valeurs par patients 
# 
# cast(temp5,  HADM_ID + CAT_TEMPS ~ VARIABLE , value = "VALUENUM", length)->variableC
# melt(variableC, id = c("HADM_ID" ,"CAT_TEMPS"))->temp7  
# temp7$value [temp7$value >= 3]<-">3"
# as.factor(temp7$value)->temp7$value
# temp7$value = factor(temp7$value,levels(temp7$value)[c(2:4,1)])
# as.factor(temp7$CAT_TEMPS)->temp7$CAT_TEMPS
# temp7$CAT_TEMPS = factor(temp7$CAT_TEMPS,levels(temp7$CAT_TEMPS)[c(3,1,2)])
# ggplot(temp7, aes (x = value)) +  geom_bar(position = "dodge") +  facet_grid( CAT_TEMPS  ~ .)->g5
# 
# 
# 
# 
# # Moyenne des valeurs à chacun des temps
# 
# cast(temp5,  HADM_ID + CAT_TEMPS ~ VARIABLE , value = "VALUENUM", mean)->variableM
# 
# CreateTableOne(data = variableM, strata = "CAT_TEMPS"  )->T1
# kable(print(T1))->f
# write.csv(file = "./figures/clinique.csv", print(T1))
# 
# 
# grid.arrange(g5, g6, nrow=2 )->g7
# grid.arrange(g4, g7, ncol=2 )->g8
# 
# 
# png(file = paste("./figures/repartition_acqui/", j, ".png"), height = 10, width = 10, units = "in", res = 150)
# plot(g8)
# dev.off() 
# 
# 
# # Valeurs uniques
# 
# subset(temp3, temp3$VARIABLE == "Taille (inch)")->po
# ggplot(data = po, aes (x = TEMPS, y = V2, col = DBSOURCE)) + geom_point() + ggtitle( "Taille (inch)")
# 
# 
# subset(temp3, temp3$VARIABLE == "Poids à l’admission (kg)" )->po
# ggplot(data = po, aes (x = TEMPS, y = V2, col = DBSOURCE)) + geom_point() + ggtitle( "Poids  (kg)")
# 
# 
# temp3$TEMPS [temp3$VARIABLE == "Taille (inch)"]<-0
# temp3$TEMPS [temp3$VARIABLE == "Poids à l’admission (kg)"]<-0
