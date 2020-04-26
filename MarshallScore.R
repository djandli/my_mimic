



# ce fichier nécéssite une liste admid d'indentifiant d'interêt

# Exctraction des données à partir du fichier sofa créés par le script d'Alistair Johnson

con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")


paste(admid,collapse = ",")->ids
sql <- paste("select * from mimic3.labevents where  hadm_id in (", ids, ") ", sep=" " )
data <- dbGetQuery(con, sql)



write.csv(x = data , file = "./resultats/apsiii.csv", row.names = FALSE)

print("##################################")
print(" Le fichier apsiii.csv.csv vient d'être créé")
print("##################################")

dbDisconnect(con)





###################### Extraction de quelques données d'interêt ###########

library(googlesheets)
read_sheet(ss = url, sheet = "LABEVENTS" )->ty
#read.csv(text=gsheet2text(url, format='csv', sheetid = 2), stringsAsFactors=FALSE, header = TRUE)->ty
#subset(ty, ty$Marshall == 1)->include


#read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$Marshall == 1)->temp1
temp1$ITEMID->listeids
paste(listeids,collapse = ",")->items
paste(admid,collapse = ",")->ids

## Selection des données de laboratoires à partir de LABEVENTS

dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")

sql <- paste("select * from mimic3.labevents where  hadm_id in (", ids, ") and itemid in (",items, ")",  sep=" " )
data <- dbGetQuery(con, sql)


# sql <- paste("SELECT * FROM LABEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
# res <- dbSendQuery(con2, sql)
# LABEVENTS<-dbFetch(res,n=-1)

sql <- paste("select * from mimic3.d_labitems ",  sep=" " )
D_LABITEMS <- dbGetQuery(con, sql)
# sql <- paste("SELECT * FROM D_LABITEMS "  ,  sep=" " )
# res <- dbSendQuery(con2, sql)
# D_LABITEMS<-dbFetch(res,n=-1)
merge(data, D_LABITEMS, by = "itemid" )->temp0


# Chargement de la table admissions
sql <- paste("select * from mimic3.icustays where  hadm_id in (", ids, ")",  sep=" " )
icustays <- dbGetQuery(con, sql)


merge(temp0, icustays[c("hadm_id", "intime")], by = "hadm_id", all = TRUE)->temp1
temp1$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$intime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$intime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp1$charttime ,temp1$intime, unit="hours") ->temp1$TEMPS
temp1$TABLE<-"LABEVENTS"
temp1->save1


############################# 
## Extraction des données de CHARTEVENTS 
############################# 
library(googlesheets)

read.csv(text=gsheet2text(url, format='csv', sheetid = 0), stringsAsFactors=FALSE, header = TRUE, skip = 1)->ty
subset(ty, ty$Marshall == 1)->include

# 
# # Extraction des paramètres d'interêts
# read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
# subset(ty, ty$INCLURE.. == 1)->include
c(include$ITEMID_CV, include$ITEMID_MV)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids
paste(itemids,collapse = ",")->items
paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("select * from mimic3.chartevents where  hadm_id in (", ids, ") and itemid in (",items, ")",  sep=" " )
ptm <- proc.time()
chartevents <- dbGetQuery(con, sql)
proc.time() - ptm ->tmpsec

write.table(file="./resultats/chartevents2.csv", chartevents, sep=",")

read.table(file ="./resultats/chartevents2.csv", sep ="," , header = TRUE)->temp0



# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


# Chargeemnt de ICUSTAYS
icustays[c(  "hadm_id"  ,  "intime"   )]->temp1
merge(temp0,temp1, by = "hadm_id")->temp2
difftime(temp2$charttime, temp2$intime , unit = "hours")->temp2$TEMPS



# Extraction du nom des variables
include[c("VARIABLE",  "ITEMID_MV" , "ITEMID_CV")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3

merge(temp2, inc3, by.x = "itemid" , by.y = "value" )->temp4






####################### Visualisation des paramètres d'interêt sous forme graphique. 
temp4[c("hadm_id",  "VARIABLE" , "TEMPS", "valueuom" , "value")]->a0
as.numeric(as.character(a0$value))->a0$value 
save1[c("hadm_id",  "label" , "TEMPS", "valueuom" , "valuenum")]->a1
colnames(a1)[c(2,5)]<-c("VARIABLE","value")

rbind(a0,a1)->a2


# Aparté ##########################################
# Pour chaque patient, on génère une planche de données


merge(a2, icustays[c("hadm_id","dbsource")], by  = "hadm_id")->temp5


unique(temp5$VARIABLE)[1]
subset(temp5, temp5$hadm_id == unique(temp5$hadm_id)[9])->temp6

# Extraction temps maximum en réanimation
maxi<-as.numeric(difftime(icustays$outtime[icustays$hadm_id == unique(temp5$hadm_id)[5]], icustays$intime[icustays$hadm_id == unique(temp5$hadm_id)[5]], unit = "hours"))
subset(temp6, temp6$TEMPS < maxi)->temp6b


temp6b[c("value","TEMPS","VARIABLE","dbsource")]->temp7

temp7$quadrant<-"A"
ht<-c("Fréquence respiratoire","O2 delivery device","Intubated", "pO2","Creatinine",)
temp7$quadrant[temp7$VARIABLE %in% ht]<-"B"

bg<-c("pH","PaO2","Fi O2","O2 Flow" )
temp7$quadrant[temp7$VARIABLE %in% bg]<-"Blood gas"

subset(  temp7 , !(temp7$VARIABLE %in% c("O2 delivery device","Fi O2","O2 Flow",
                                         "PaO2","Intubated", "pO2","Creatinine","pH"  )))->temp8
subset(  temp7 , temp7$VARIABLE == "O2 delivery device")->temp9
subset(  temp7 , temp7$VARIABLE == "Fi O2")->temp10
subset(  temp7 , temp7$VARIABLE == "O2 Flow")->temp11
subset(  temp7 , temp7$VARIABLE == "Intubated" )->temp12
subset(  temp7 , temp7$VARIABLE == "PaO2" )->temp13
subset(  temp7 , temp7$VARIABLE == "pO2" )->temp14
subset(  temp7 , temp7$VARIABLE == "Creatinine" )->temp15
subset(  temp7 , temp7$VARIABLE == "pH" )->temp16

as.numeric(as.character(temp8$value))->temp8$value

ggplot(data = temp8,aes( x = TEMPS, y = value, color = VARIABLE)) + xlim(-15, maxi) +
  geom_line()  +  facet_grid( quadrant  ~ . ,scales = "free") + 
#  geom_point(data = temp9, aes (x = TEMPS, y = -15, shape = value)) +
  geom_text(data = temp10, aes (x = TEMPS, y = -5, label = value),angle = 0, cex = 3)+
  geom_text(data = temp11, aes (x = TEMPS, y = -10, label = value),angle = 0, cex = 3)+
  geom_text(data = temp12, aes (x = TEMPS, y = -1, label = value),angle = 0, cex = 3)+
  geom_text(data = temp13, aes (x = TEMPS, y = -1, label = value),angle = 0, cex = 3)+
  geom_text(data = temp14, aes (x = TEMPS, y = -7, label = value),angle = 0, cex = 3)+
  geom_text(data = temp15, aes (x = TEMPS, y = -12, label = value),angle = 0, cex = 3)+
  geom_text(data = temp16, aes (x = TEMPS, y = -14, label = value),angle = 0, cex = 3)+
  theme(legend.position="bottom") 

#library(gridExtra)
#grid.arrange(bp, dp, vp, sc, ncol=2, nrow = 2)




# Temps de sortie de réa

###############
############### Fin de l'aparté



# Analyse des données pour calculer le score Marshall à chaque intervalle

subset(a2, !(a2 %in% c("O2 delivery device","Intubated")))->a3
as.numeric(as.character(a3$value))->a3$value 
cast(a3, hadm_id  + TEMPS ~ VARIABLE, value = "value", fun.aggregate = "mean" )->o2

o2$PaO2 / o2$`Fi O2` ->o2$Index


for (v in colnames(o2)) {
  print(v)
  o2[[v]][is.na(o2[[v]])]<-NA
}


o3<- o2  %>% group_by(hadm_id)  %>%
  fill( `Artérielle Systolique`, .direction = "down") %>%
  fill( Creatinine, .direction = "down") %>%
  fill( `Fi O2`, .direction = "down") %>%
  fill( `Fréquence cardiaque`, .direction = "down") %>%
  fill( `Fréquence respiratoire`, .direction = "down") %>%
  fill( `NIBP systolique`, .direction = "down") %>%
  fill( `Oxygen Saturation`, .direction = "down") %>%
  fill( PaO2, .direction = "down") %>%
  fill( `pH`, .direction = "down") %>%
  fill( `O2 Flow`, .direction = "down") %>%
  fill( `pO2`, .direction = "down") %>%
  fill( `Température (C)`, .direction = "down") %>%
  fill( `Température (F)`, .direction = "down") %>%
  fill( `Index`, .direction = "down")
              
# Calcul du score de Marshall

o3[c( "hadm_id","TEMPS","Fi O2","Oxygen Saturation","PaO2","pH","pO2","Index" )]->o4

# Take the last available score each time




################# To be continued ##################











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
















