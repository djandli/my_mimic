

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
subset(ty, ty$Pancarte == 1)->temp1
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



############################# 
## Extraction des données de traitements
############################# 

dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")

sql <- paste("select * from mimic3.d_items",  sep=" " )
ptm <- proc.time()
d_items <- dbGetQuery(con, sql)
proc.time() - ptm ->tmpsec


############################## Metavision
dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")

sql <- paste("select * from mimic3.inputevents_mv2 where  hadm_id in (", ids, ")",  sep=" " )
ptm <- proc.time()
temp0 <- dbGetQuery(con, sql)
proc.time() - ptm ->tmpsec


# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$starttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$starttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$endtime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$endtime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


# Chargeemnt de ICUSTAYS
icustays[c(  "hadm_id"  ,  "intime"   )]->temp1
merge(temp0,temp1, by = "hadm_id")->temp2

# Creéation nouvelle variable
difftime(temp2$starttime, temp2$intime , unit = "hours")->temp2$START
difftime(temp2$endtime, temp2$intime , unit = "hours")->temp2$END

temp2->inputMV
merge(inputMV, d_items[c("itemid","label")], by = "itemid")->InputMV

############################## CareVue

dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")

sql <- paste("select * from mimic3.inputevents_cv2 where  hadm_id in (", ids, ")",  sep=" " )
ptm <- proc.time()
temp0 <- dbGetQuery(con, sql)
proc.time() - ptm ->tmpsec

# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$storetime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$storetime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


# Chargeemnt de ICUSTAYS
icustays[c(  "hadm_id"  ,  "intime"   )]->temp1
merge(temp0,temp1, by = "hadm_id")->temp2

# Creéation nouvelle variable
difftime(temp2$charttime, temp2$intime , unit = "hours")->temp2$TIME
difftime(temp2$storetime, temp2$intime , unit = "hours")->temp2$validated

temp2->inputCV
merge(inputCV, d_items[c("itemid","label")], by = "itemid")->InputCV

##########################################

# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$starttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$starttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$endtime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$endtime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


# Chargeemnt de ICUSTAYS
icustays[c(  "hadm_id"  ,  "intime"   )]->temp1
merge(temp0,temp1, by = "hadm_id")->temp2

# Creéation nouvelle variable
difftime(temp2$starttime, temp2$intime , unit = "hours")->temp2$START
difftime(temp2$endtime, temp2$intime , unit = "hours")->temp2$END

temp2->inputMV



############################# 
## Extraction des données de NOTEEVENTS 
############################# 

sql <- paste("select * from mimic3.noteevents where  hadm_id in (", ids, ")",  sep=" " )
ptm <- proc.time()
noteevents <- dbGetQuery(con, sql)
proc.time() - ptm ->tmpsec


noteevents$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
noteevents$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


# Chargeemnt de ICUSTAYS
icustays[c(  "hadm_id"  ,  "intime"   )]->temp1
merge(noteevents,temp1, by = "hadm_id")->temp2
difftime(temp2$charttime, temp2$intime , unit = "hours")->temp2$TEMPS

temp2[c("hadm_id",   "TEMPS", "category" , "description")]->a3


############################# 
## Extraction des données de DATETIMEEVENTS
############################# 





################@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@
################@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @ @@@@@@@@@@@@@@@@@@@@@@@@@@
################@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@ @@@@@@@@@@@@@@@@@@@@@@@@
################@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@ @@@@@@@@@@@@@@@@@@@@@@
################@@@@@@@@@@@@@@@@@@@@@@@@@@@@ @@@@@ @@@@@@@@@@@@@@@@@@@@@
####################### Visualisation des paramètres d'interêt sous forme graphique. 
temp4[c("hadm_id",  "VARIABLE" , "TEMPS", "valueuom" , "value")]->a0
as.numeric(as.character(a0$value))->a0$value 
save1[c("hadm_id",  "label" , "TEMPS", "valueuom" , "valuenum")]->a1
colnames(a1)[c(2,5)]<-c("VARIABLE","value")

rbind(a0,a1)->a2
# Aparté #############################################################
# Aparté #############################################################
# Aparté #############################################################







# Pour chaque patient, on génère une planche de données

Numero<-35
merge(a2, icustays[c("hadm_id","dbsource")], by  = "hadm_id")->temp5


#unique(temp5$VARIABLE)[1]
subset(temp5, temp5$hadm_id == unique(temp5$hadm_id)[Numero])->temp6

# Extraction des données de NOTEEVENTS
subset(a3, a3$hadm_id == unique(temp5$hadm_id)[Numero])->ntes

# Extraction des données de ICUSTAYS
unique(temp5$hadm_id)[Numero]

# Extraction des données de prescriptions
subset(prescriptions, prescriptions$hadm_id == unique(temp5$hadm_id)[Numero])->o



# Extraction temps maximum en réanimation
maxi<-as.numeric(difftime(icustays$outtime[icustays$hadm_id == unique(temp5$hadm_id)[Numero]], 
                          icustays$intime[icustays$hadm_id == unique(temp5$hadm_id)[Numero]], unit = "hours"))
subset(temp6, temp6$TEMPS < maxi)->temp6b


temp6b[c("value","TEMPS","VARIABLE","dbsource")]->temp7

temp7$quadrant<-"Scope"
# Remove biochemistry
bioC<-c("Fréquence respiratoire","O2 delivery device","Intubated", "pO2","Creatinine",
        "Calcium, Total","Anion Gap","Urea Nitrogen","Calcium, Total",
        "Bicarbonate", "Potassium", "Lactate","Phosphate","Glucose","Magnesium",
        "Albumin")
temp7$quadrant[temp7$VARIABLE %in% bioC]<-"Biochemistry"
# Remove blood gas
bg<-c("pH","PaO2","Fi O2","O2 Flow" )
temp7$quadrant[temp7$VARIABLE %in% bg]<-"Blood gas"
# Remove liver and pancreatic enzymes
enzymes<-c("Amylase" , "Lipase", "Asparate Aminotransferase (AST)", "Alanine Aminotransferase (ALT)" ,
           "Alkaline Phosphatase", "Bilirubin, Total" ,"Bilirubin, Direct","Bilirubin, Indirect")
temp7$quadrant[temp7$VARIABLE %in% enzymes]<-"Enzymes"
# Hematology
hem<-c("Lymphocytes","White Blood Cells","Platelet Count","PT","Neutrophils"  )
temp7$quadrant[temp7$VARIABLE %in% hem]<-"Hematology"


round(temp7$value[temp7$VARIABLE == "Fi O2"],digits = 2)->temp7$value[temp7$VARIABLE == "Fi O2"]

subset(  temp7 , !(temp7$quadrant %in% c( "Enzymes","Blood gas", "Biochemistry","Hematology" )))->temp8
unique(temp8$VARIABLE)

as.numeric(as.character(temp8$value))->temp8$value

legend<-as.numeric(min(temp6$TEMPS))-8
b1<--10
b2<--40
b3<--70
b4<--100
offset<-5


g<-ggplot() + xlim(as.numeric(min(temp6$TEMPS))-40, maxi) +
  geom_line(data = temp8,aes( x = TEMPS, y = value, color = VARIABLE), alpha=0.6)  +  
  theme(legend.position="bottom") +
  ## Liver enzymes
  geom_rect(aes(xmin=-Inf, xmax=+Inf, ymin=-45,ymax=-5), alpha=0.2, fill="red") +
  geom_text(data = subset(  temp7 , temp7$VARIABLE == "Amylase"), 
            aes (x = TEMPS, y = -10, label = value),angle = 0, cex = 3) + 
  annotate("text", x = legend, y = -10, label = "Amylase",colour = "black", cex = 3, hjust = 1 ) +
  geom_text(data = subset(  temp7 , temp7$VARIABLE == "Lipase"), 
            aes (x = TEMPS, y = -15, label = value),angle = 0, cex = 3) + 
  annotate("text", x = legend, y = -15, label = "Lipase",colour = "black", cex = 3, hjust = 1 ) + 
  geom_text(data = subset(  temp7 , temp7$VARIABLE == "Asparate Aminotransferase (AST)"), 
            aes (x = TEMPS, y = -20, label = value),angle = 0, cex = 3) + 
  annotate("text", x = legend, y = -20, label = "ASAT",colour = "black", cex = 3, hjust = 1 ) +
  geom_text(data = subset(  temp7 , temp7$VARIABLE == "Alanine Aminotransferase (ALT)"), 
            aes (x = TEMPS, y = -25, label = value),angle = 0, cex = 3) + 
  annotate("text", x = legend, y = -25, label = "ALT",colour = "black", cex = 3, hjust = 1 ) +
  geom_text(data = subset(  temp7 , temp7$VARIABLE == "Alkaline Phosphatase"), 
            aes (x = TEMPS, y = -30, label = value),angle = 0, cex = 3) + 
  annotate("text", x = legend, y = -30, label = "PAL",colour = "black", cex = 3, hjust = 1 ) +
  geom_text(data = subset(  temp7 , temp7$VARIABLE == "Bilirubin, Total"), 
            aes (x = TEMPS, y = -35, label = value),angle = 0, cex = 3) + 
  annotate("text", x = legend, y = -35, label = "Bilirubin T",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Bilirubin, Indirect",na.rm = TRUE) >0 )
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Bilirubin, Indirect"), 
              aes (x = TEMPS, y = -40, label = value),angle = 0, cex = 3) } + 
  annotate("text", x = legend, y = -40, label = "Bilirubin, Indirect",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Bilirubin, Direct",na.rm = TRUE) >0 )
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Bilirubin, Direct"), 
              aes (x = TEMPS, y = -45, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -45, label = "Bilirubin, Direct",colour = "black", cex = 3, hjust = 1 ) +
  
  ## Blood gas
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-85,ymax=-45), alpha=0.2, fill="blue") +
  {if(sum(temp7$VARIABLE == "Fi O2" ,na.rm = TRUE) >0 )
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Fi O2"), 
              aes (x = TEMPS, y = 150, label = value),angle = 45, cex = 3) } + 
  annotate("text", x = legend, y = 150, label = "Fi O2",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "O2 delivery device" ,na.rm = TRUE) >0 )
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "O2 delivery device"), 
              aes (x = TEMPS, y = -55, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -55, label = "O2 delivery",colour = "black", cex = 3, hjust = 1 ) + 
  {if(sum(temp7$VARIABLE == "O2 Flow" ,na.rm = TRUE) >0 )
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "O2 Flow"), 
              aes (x = TEMPS, y = -60, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -60, label = "O2 Flow",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Anion Gap" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Anion Gap"), 
              aes (x = TEMPS, y = -65, label = value),angle = 0, cex = 3) } + 
  annotate("text", x = legend, y = -65, label = "Anion gap",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "PaO2" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "PaO2"), 
              aes (x = TEMPS, y = -70, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -70, label = "PaO2",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "pH" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "pH"), 
              aes (x = TEMPS, y = -75, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -75, label = "pH",colour = "black", cex = 3, hjust = 1 ) +
  ## Biochesmitry
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-135,ymax=-85), alpha=0.2, fill="green") +
  {if(sum(temp7$VARIABLE == "Magnesium" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Magnesium"), 
              aes (x = TEMPS, y = -90, label = value),angle = 0, cex = 3) } + 
  annotate("text", x = legend, y = -90, label = "Magnesium",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Potassium" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Potassium"), 
              aes (x = TEMPS, y = -95, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -95, label = "Potassium",colour = "black", cex = 3, hjust = 1 ) + 
  {if(sum(temp7$VARIABLE == "Creatinine" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Creatinine"), 
              aes (x = TEMPS, y = -100, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -100, label = "Creatinine",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Urea Nitrogen" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Urea Nitrogen"), 
              aes (x = TEMPS, y = -105, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -105, label = "Urea Nitrogen",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Calcium, Total" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Calcium, Total"), 
              aes (x = TEMPS, y = -110, label = value),angle = 0, cex = 3)} + 
  annotate("text", x = legend, y = -110, label = "Calcium",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Albumin" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Albumin"), 
              aes (x = TEMPS, y = -115, label = value),angle = 0, cex = 3)} + 
  annotate("text", x = legend, y = -115, label = "Albumin",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Phosphate" ,na.rm = TRUE) >0 ) 
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Phosphate" ), 
              aes (x = TEMPS, y = -120, label = value),angle = 0, cex = 3) } + 
  annotate("text", x = legend, y = -120, label = "Phosphate" ,colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Bicarbonate" ,na.rm = TRUE) >0 )  
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Bicarbonate" ), 
              aes (x = TEMPS, y = -125, label = value),angle = 0, cex = 3)} + 
  annotate("text", x = legend, y = -125, label = "Bicarbonate",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Lactate" ,na.rm = TRUE) >0 )  
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Lactate" ), 
              aes (x = TEMPS, y = -130, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -130, label = "Lactate",colour = "black", cex = 3, hjust = 1 ) +
  ## Hematology
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-170,ymax=-135), alpha=0.2, fill="yellow") +
  {if(sum(temp7$VARIABLE == "Lymphocytes" ,na.rm = TRUE) >0 )  
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Lymphocytes" ), 
              aes (x = TEMPS, y = -140, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -140, label = "Lymphocytes",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "White Blood Cells" ,na.rm = TRUE) >0 )  
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "White Blood Cells"), 
              aes (x = TEMPS, y = -145, label = value),angle = 0, cex = 3)} + 
  annotate("text", x = legend, y = -145, label = "White Blood Cells",colour = "black", cex = 3, hjust = 1 ) + 
  {if(sum(temp7$VARIABLE == "Platelet Count" ,na.rm = TRUE) >0 )  
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Platelet Count"), 
              aes (x = TEMPS, y = -150, label = value),angle = 0, cex = 3) } + 
  annotate("text", x = legend, y = -150, label = "Platelet Count",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "PT" ,na.rm = TRUE) >0 )  
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "PT"), 
              aes (x = TEMPS, y = -155, label = value),angle = 0, cex = 3) }+
  annotate("text", x = legend, y = -155, label = "PT",colour = "black", cex = 3, hjust = 1 ) +
  {if(sum(temp7$VARIABLE == "Neutrophils" ,na.rm = TRUE) >0 )  
    geom_text(data = subset(  temp7 , temp7$VARIABLE == "Neutrophils"), 
              aes (x = TEMPS, y = -160, label = value),angle = 0, cex = 3) }+ 
  annotate("text", x = legend, y = -160, label = "Neutrophils",colour = "black", cex = 3, hjust = 1 ) + 
  ## Add noteeevents
  geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=-5,ymax=20), alpha=0.2, fill="brown") +
  geom_point(data = subset(  ntes , ntes$description %in%  c( "CHEST (PORTABLE AP)" ,"CHEST (SINGLE VIEW)")), 
             aes (x = TEMPS, y = 0)) + 
  annotate("text", x = legend, y = 0, label = "RXP",colour = "black", cex = 3, hjust = 1 ) + 
  {if(sum(ntes$description == "LIVER OR GALLBLADDER US (SINGLE ORGAN)" ,na.rm = TRUE) >0 )
    geom_point(data = subset(  ntes , ntes$description == "LIVER OR GALLBLADDER US (SINGLE ORGAN)"), 
               aes (x = TEMPS, y = 5)) } + 
  annotate("text", x = legend, y = 5, label = "EUS",colour = "black", cex = 3, hjust = 1 )+ 
  {if(sum(ntes$description == "ERCP BILIARY&PANCREAS BY GI UNIT" ,na.rm = TRUE) >0 )
    geom_point(data = subset(  ntes , ntes$description == "ERCP BILIARY&PANCREAS BY GI UNIT"), 
               aes (x = TEMPS, y = 10)) }+ 
  annotate("text", x = legend, y = 10, label = "ERCP",colour = "black", cex = 3, hjust = 1 ) + 
  {if(sum(ntes$category == "Nursing/other" & ntes$description == "Report" ,na.rm = TRUE) >0 )
    geom_point(data = subset(  ntes , ntes$category == "Nursing/other" & ntes$description == "Report"), 
               aes (x = TEMPS, y = 15)) } + 
  annotate("text", x = legend, y = 15, label = "Nurse report",colour = "black", cex = 3, hjust = 1 ) +
  # Add title
  ggtitle(paste(as.character(subset(icustays, icustays$hadm_id == unique(temp5$hadm_id)[Numero])["first_careunit"]), 
                as.character(subset(icustays, icustays$hadm_id == unique(temp5$hadm_id)[Numero])["dbsource"]) , 
                unique(temp5$hadm_id)[Numero],sep = "-")) 






########################## Visualisation des traitements recues. 


subset(InputCV, InputCV$hadm_id == unique(temp5$hadm_id)[Numero])->CareV
subset(InputMV, InputMV$hadm_id == unique(temp5$hadm_id)[Numero])->MetaV


g->g1
z<-50
#subset(  CareV , CareV$label ==  "D5W" & CareV$TIME >= 25 & CareV$TIME < 26 )->v
as.numeric(as.character(v$amount))->v$amount 

p<-80
legend<-as.numeric(min(CareV$TIME))-8


CareV[c("hadm_id", "label", "TIME", "validated",  "amount","amountuom", "rate","rateuom" ) ] ->j
#       "orderid","linkorderid","stopped","newbottle" ) ] ->j



subset(CareV, !is.na(CareV$rate))->fr
as.numeric(as.character(fr$rate))->fr$rate 
paste(fr$label, fr$rateuom, sep = ",")->fr$label2
as.numeric(fr$validated)->fr$validated2
as.character(fr$TIME)->fr$TIME
# Visualisation des traitements recues 
ggplot() + geom_point(data = fr, 
                      aes(y = rate, x = validated2, color = TIME)) + 
  facet_grid( label2 ~ . , scales = "free" ) ->gA


# Create plot with quantities
subset(CareV, is.na(CareV$rate))->qt
as.character(qt$TIME)->qt$TIME

ggplot() + geom_point(data = qt, 
                      aes(y = label, x = validated, color = TIME)) ->gB


grid.arrange(gA,gB)  ->gC



# We select the rates 



for (u in unique(CareV$label)){
  unique(CareV$label)[16]->u  
  subset(  CareV , CareV$label ==  u)->v
  v[c("hadm_id", "label", "TIME", "validated",  "amount","amountuom", "rate","originalrateuom",
      "rateuom", "stopped", "linkorderid" ) ] ->j
  subset(j, !is.na(j$rate))->rate
  subset(j, is.na(j$rate))->quantity
  
  # Ajout des rate
  # g1 + geom_point(data = rate, aes(x = validated, y = 50)) + 
  #  geom_text(data = rate, aes(x = TIME, y = 50, label = rate, angle = 90), cex = 3, hjust = 1, vjust = 1) + 
  #   xlim(c(25,130)) + 
  #   geom_point(data = quantity, aes(x = validated, y = 30)) + 
  #   geom_point(data = quantity, aes(x = TIME, y = 20), color = "red") + 
  #   geom_text(data = quantity, aes(x = TIME, y = 20, label = amount, angle = 90), cex = 3, hjust = 1, vjust = 1) +
  #   
  # 
  
  as.character(quantity$TIME)->quantity$TIME
  as.numeric(as.character(quantity$validated))->quantity$validated
  as.numeric(as.character(quantity$amount))->quantity$amount
  
  as.character(rate$TIME)->rate$TIME
  as.numeric(as.character(rate$validated))->rate$validated2
  as.numeric(as.character(rate$rate))->rate$taux
  
  
  
  # Calcul des quantités à partir des taux 
  library(data.table)
  DT <- data.table(rate)
  DT[ , .SD[which.min(validated)], by = c("TIME", "rate")]->DTmin
  colnames(DTmin)[colnames(DTmin) == "validated"]<-"minimum"
  DT[ , .SD[which.max(validated)], by = c("TIME", "rate")]->DTmax
  colnames(DTmax)[colnames(DTmax) == "validated"]<-"maximum"
  # Autre option prendre le temps miniamle de la validation suivante
  merge(DTmin,DTmax, by = c("TIME", "rate", "hadm_id", "label", "taux", 
                            "amountuom", "amount", "linkorderid", 
                            "rateuom", "originalrateuom","stopped"))->DT2
  #Reorder
  DT2[order(minimum),]->DT3
  # Si on a une seule valeur pour une donnée, on prends la valeur de l'élément suivant
  lead(DT3$minimum, default = "Stopped")->DT3$Next
  DT3$maximum [DT3$minimum == DT3$maximum & !is.na(DT3$Next)] <-DT3$Next[DT3$minimum == DT3$maximum & !is.na(DT3$Next)]
  # Différence de temps
  as.numeric(DT3$maximum) - as.numeric(DT3$minimum)->DT3$DIFF
  # Débit théorique
  as.numeric(DT3$DIFF)* DT3$taux->DT3$calculated_amount
  # Somme théorique
  cast(DT3, label + TIME  ~ hadm_id, value = "calculated_amount",
       fun.aggregate = "somme")->tempA
  colnames(tempA)[3]<-"theoritical_amount"
  
  
  # Calcul des quantités
  # Calcul des quantités totales à chaque temps 
  somme<-function(x){
    sum(x, na.rm = TRUE)->y
    return(y)
  } 
  cast(quantity, label + TIME ~ hadm_id, value= "amount", fun.aggregate = "somme")->temp0
  colnames(temp0)[3]<-"amount"
  
  ggplot() + 
    # geom_point(data = quantity, aes( x = validated, y = amount, color = TIME)) +
    geom_point(data = rate, aes( x = validated, y = taux, color = TIME, pch = linkorderid ),) ->g2
  
  ggplot() + 
    geom_point(data = quantity, aes( x = validated, y = amount, color = TIME,  pch = linkorderid)) -> g3
  
  #library(gridExtra)
  grid.arrange(g2,g3, ti)  ->g4
  
  
  merge(temp0,tempA, by = c( "label", "TIME"))->tempB
  #Reorder
  as.numeric(tempB$TIME)->tempB$TIME
  attach(tempB)
  tempB[order(TIME),]->tempC
  detach(tempB)
  tempC
  
  
  ##############@
  
  
  
  
  
  
  DT1[order(validated),]->DT2
  
  
  lag(DT2$TIME)->DT2$previousTIME  # Considerer different temps si changement ou originel
  
  DT2$START<-NA
  for (i in 1:nrow(DT2)){
    print(i)
    #i<-4
    if(DT2$TIME[i] == DT2$previousTIME[i] & !is.na(DT2$previousTIME[i]) ){ DT2$START[i]  <-DT2$validated2[i] }  else {DT2$START[i] <-DT2$TIME[i] }
  }
  # Calcul du temps de fin
  lead(DT2$START, default = "Stopped")->DT2$STOP
  # Calcul du temps à chaque débit
  as.numeric(DT2$STOP) - as.numeric(DT2$START)->DT2$DIFF
  # Débit théorique
  as.numeric(DT2$DIFF)* DT2$taux->DT2$calculated_amount
  
  # Calcul des quantités totales à chaque temps 
  somme<-function(x){
    sum(x, na.rm = TRUE)->y
    return(y)
  } 
  cast(quantity, label + TIME ~ hadm_id, value= "amount", fun.aggregate = "somme")->temp0
  colnames(temp0)[3]<-"amount"
  
  ggplot() + 
    # geom_point(data = quantity, aes( x = validated, y = amount, color = TIME)) +
    geom_point(data = rate, aes( x = validated, y = rate, color = TIME, pch = linkorderid ),) ->g2
  
  ggplot() + 
    geom_point(data = quantity, aes( x = validated, y = amount, color = TIME,  pch = linkorderid)) -> g3
  
  #library(gridExtra)
  grid.arrange(g2,g3)  ->g4
  
  
  
  # result <- rate   %>% slice(which.min("validated2")) 
  # %>% slice(which.min(validated))
  # 
  # 
  # 
  # group_by(rate, TIME) %>% 
  #   filter(validated2 == min(validated2)) %>% 
  #   filter(1:n() == 1)
  
  
  #   filter(value == min(validated)) %>%
  #  mutate(validated, validated  + 10 )
  # mutate(result, prev  =  lead(TIME, order_by = validated))->p
  #result$stopped<-lead(result$validated, default = "Stopped",  order_by = TIME)
  
  # Calcul de la durée pour chaque dose
  #as.numeric(result$stopped) - result$validated->result$difftimes
  
  # Calcul de la quantité totale à chaque temps
  
  
  somme<-function(x){
    sum(x, na.rm = TRUE)->y
    return(y)
  }
  cast(quantity, label + TIME ~ hadm_id, value= "amount", fun.aggregate = "somme")->temp0
  colnames(temp0)[3]<-"amount"
  
  
  
  #merge(result, temp0, by = "TIME")
  merge(result, temp0, by = "TIME")->temp1
  
  
  # Fin de la fusion
  
  
  
  
  ggplot() + 
    # geom_point(data = quantity, aes( x = validated, y = amount, color = TIME)) +
    geom_point(data = rate, aes( x = validated, y = rate, color = TIME, pch = linkorderid ),) ->g2
  
  ggplot() + 
    geom_point(data = quantity, aes( x = validated, y = amount, color = TIME,  pch = linkorderid)) -> g3
  
  #library(gridExtra)
  grid.arrange(g2,g3)  ->g4
  
  # Ajout des quantités 
  subset(j, is.na(j$rate))->quantity
  
  
  as.numeric(as.character(rate$TIME))->rate$TIME
  
  as.numeric(as.character(v$amount))->v$amount 
  
  
  print(u)
  subset(  CareV , CareV$label ==  u)->v
  print(nrow(v))
  as.numeric(as.character(v$amount))->v$amount 
  cast(v, label + TIME ~ hadm_id, value ="amount", fun.aggregate =  "sum" )->m
  colnames(m)[3]<-"volume"
  round(m$volume, digits = 1)->m$volume
  #nrow(v)->p
  g1<- g1 +
    annotate("text", x = legend, y = p , label =  u ,
             colour = "black", cex = 3, hjust = 1 ) +
    geom_text(data = m,  aes_string(x = "TIME", y = p , label = "volume", angle = 0), cex = 3 , hjust = 0, vjust = 0) + 
    geom_point(data = m,  aes_string(x = "TIME", y = p ), pch = 3)
  
  # z<-z+10
  
  p<-p-10
}



