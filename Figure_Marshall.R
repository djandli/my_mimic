# Données biologiques à la baseline, H24 et H48. 


## Certaines données biologiques ne sont pas présentes dans la table LABEVENTS, il est donc pertinent de tenter d'extraire les informations 
# à partir de la table CHARTEVENTS également. Il est important de garder à l'esprit que les informations de données d'hosptialisation multiples
# en réanimation ne sont pas mentionnées. 



# Chargement des données de LABEVENTS. 

## Selection des données de laboratoires à partir de LABEVENTS (Créatinine, PaO2, FiO2 )

read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$Inclure == 1)->temp1
temp1$ITEMID->listeids
paste(listeids,collapse = ",")->items   # Selection des items d'interêt
paste(admid,collapse = ",")->ids

## Extraction des tables

dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")

sql <- paste("select * from mimic3.labevents where hadm_id in (", ids, ") and itemid in ( ",items, ")", sep=" " )
LABEVENTS <- dbGetQuery(con, sql)

sql <- paste("select * from mimic3.d_labitems", sep=" " )
D_LABITEMS <- dbGetQuery(con, sql)

sql <- paste("select * from mimic3.icustays where  hadm_id in (", ids, ")", sep=" " )
ICUSTAYS <- dbGetQuery(con, sql)



# Début de l'analyse
merge(LABEVENTS, D_LABITEMS, by = "itemid")->temp0
merge(temp0, ICUSTAYS[c("hadm_id", "intime")], by = "hadm_id", all.x = TRUE)->temp1
temp1->BIOS
BIOS[c("hadm_id", "label", "value", "charttime")]->BIOS1


# temp1$charttime->times
# dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
# row.names(dtparts) = NULL
# temp1$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
# 
# 
# temp1$intime->times
# dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
# row.names(dtparts) = NULL
# temp1$intime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
# 
# 
# difftime(  temp1$charttime ,temp1$intime, unit="hours") ->temp1$temps
# 
# 
# # Extracttion des paramètres  à l'entrée en USI. 
# subset(temp1, temp1$temps > 0 & temp1$temps <72)->temp2
# as.numeric(as.character(temp2$value))->temp2$value 
# as.numeric(as.character(temp2$temps))->temp2$temps 
# temp2->bio


#########################################
# Exctraction des variables cliniques 
#######################################

read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$Clinique1 == 1)->include
c(include$ITEMID_CV, include$ITEMID_MV)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids


paste(itemids,collapse = ",")->items   # Selection des items d'interêt
paste(admid,collapse = ",")->ids

## Selection des données de laboratoires à partir de LABEVENTS
dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")
sql <- paste("select * from mimic3.chartevents where  hadm_id in (", ids, ") and itemid in ( ",items, ")", sep=" " )
start_time <- Sys.time()
CHARTEVENTS <- dbGetQuery(con, sql)
end_time <- Sys.time()
end_time - start_time
# Extraction du nom des variables correspondantes
sql <- paste("select * from mimic3.d_items", sep=" " )
D_ITEMS <- dbGetQuery(con, sql)
# Fusion des deux fichiers
merge(CHARTEVENTS, D_ITEMS, by = "itemid")->temp0
temp0[c("hadm_id", "label", "value", "charttime")]->CHARTS

# ===========================

# On fusionne les éléments extraits à partir de CHARTEVENTS ET LABEVENTS

# Extraction des compte rendu de radiologique. 
dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")
sql <- paste("select * from mimic3.noteevents where  hadm_id in (", ids, ") ", sep=" " )
NOTEEVENTS <- dbGetQuery(con, sql)
NOTEEVENTS[c("hadm_id", "description",  "text" , "charttime")]->NOTES
# merge(NOTEEVENTS, ICUSTAYS[c("hadm_id", "intime")], by = "hadm_id", all.x = TRUE)->temp1



# as.character(bios$charttime)->bios$charttime
# gsub(x = bios$charttime, pattern = ")|\\(", replacement ="")->bios$charttime
rbind(CHARTS, BIOS1)->FUSION



# Prise en compte des donnes temporelles : fusion avec ICUSTAYS

merge(FUSION, ICUSTAYS[c("hadm_id", "intime")], by = "hadm_id", all.x = TRUE)->temp1
temp1$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
temp1$intime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$intime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
difftime(  time1 = temp1$charttime ,time2 = temp1$intime, unit="hours") ->temp1$temps


as.numeric(as.character(temp1$value))->temp1$value
# Visualisation des données temporelles. 




# Le fichier temp1 viens d'être créé avec les élements d'interêts; 
## Modication du nom de certaines variables. 
temp1$label[temp1$label %in% c( "Arterial BP [Systolic]"  ,"NBP [Systolic]" , 
            "Non Invasive Blood Pressure systolic" , "Arterial Blood Pressure systolic" ) ] <-"Systolic blood pressure"

temp1$label[temp1$label %in% c( "Temperature Fahrenheit"  ,"Temperature F"  ) ] <-"Temperature Fahrenheit"


subset(temp1, !(temp1 %in% c("Temperature C", "Temperature Celsius"  )))->temp1b


#


subset(temp1, temp1$hadm_id == 189836 & temp1$temps > -30 & temp1$temps <200)->tempX

# On sous-selectionnel les température 

var<-c("Systolic blood pressure" , "Temperature C", 
       "Respiratory Rate", "Heart Rate","Temperature Fahrenheit", 
       "FiO2 Set", "ART BP mean" ,"Arterial BP [Diastolic]", "NBP Mean", "NBP [Diastolic]", 
       "Arterial BP Mean" )
subset(tempX, tempX$label %in% var)->tempX1
subset(tempX, !(tempX$label %in% var))->tempX2
ggplot(data = tempX1, aes (x = temps, y = value, color = label)) + geom_line() +
  geom_point()->a

ggplot(data = tempX2, aes (x = temps, y = label, label = value)) + geom_text() ->b


grid.arrange(a,a,b,a,widths = c(2,1), heights = c(1,2))



# On sous-selectopnnel les éléments de NOTEEVENTS


# subset(NOTEEVENTS, NOTEEVENTS$hadm_id == 189836)->subA
# subA[c("hadm_id" , "category","description" , "charttime")]->subB
merge(ICUSTAYS [c("hadm_id", "intime")], NOTES, by = "hadm_id")->subC

unique(subC$description)[grep(unique(subC$description), pattern = "CT")]->u

subset(subC, subC$description %in% u)->subD 


subD->temp1
temp1$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
temp1$intime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$intime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
difftime(  time1 = temp1$charttime ,time2 = temp1$intime, unit="hours") ->temp1$temps

w


## Moyenne des valeurs à 24 heures. 
subset(temp1b, temp1b$temps > 12 & temp1$temps <36)->temp2


cast(temp2, hadm_id ~label,  value = "value", fun.aggregate = "length" )->length 
as.numeric(as.character(temp2$value))->temp2$value 
cast(temp2, hadm_id ~label,  value = "value", fun.aggregate = "mean", na.rm = TRUE)->mean 

melt(length, id = c("hadm_id", "label") )->l
melt(mean, id = c("hadm_id", "label"))->u

merge(l,u, by = c("hadm_id", "label"))->j








# Extracttion des paramètres  à l'entrée en USI. 
subset(temp1, temp1$temps > 0 & temp1$temps <72)->temp2


as.numeric(as.character(temp2$value))->temp2$value 
as.numeric(as.character(temp2$temps))->temp2$temps 
temp2->clinik



# Fusion des deux fichiers. 

clinik[c( "hadm_id", "itemid", "label","value",  "temps" )]->clinik1
bio[c("hadm_id","itemid", "label", "value",  "temps" )]->bio1

rbind(clinik1, bio1)->dataAll



subset(dataAll, dataAll$hadm_id == 195451)->temp3

# 
# 199779->u
# 
# ICUSTAYS$outtime[ICUSTAYS$hadm_id == u]
# ICUSTAYS$intime[ICUSTAYS$hadm_id == u]
# 
# 
# difftime(  ICUSTAYS$outtime[ICUSTAYS$hadm_id == u] ,ICUSTAYS$intime[ICUSTAYS$hadm_id == u], unit="hours") ->x


temp3$label2<-"Other"
temp3$label2[temp3$label %in% c("Creatinine" )]<-"Creatinine"
temp3$label2[temp3$label %in% c("pH"  )]<-"pH" 



# Fusion des deux fichiers d'interêts


# Selection des scanner à partir du fichier NOTEEVENTS
paste(admid,collapse = ",")->ids
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")
sql <- paste("select * from mimic3.noteevents where  hadm_id in (", ids, ") ", sep=" " )
NOTEEVENTS<- dbGetQuery(con, sql)
dbDisconnect(con)


temp1$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
temp1$intime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$intime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
difftime(  temp1$charttime ,temp1$intime, unit="hours") ->temp1$temps
temp

# Discharge summary
subset(temp1,  temp1$hadm_id == 196303)->i

ggplot(data = i)+ geom_point(aes(x=temps, y = description))->w




ggplot()+ geom_text(size = 3,
                    label = i$text, aes(x=0, y = 0))



