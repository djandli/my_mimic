# 
# #calcul des scores APACHE II
# 
# # Score APACHE II 
# 
# ## définition des comorbidités
# 
# * Maladie hépatique: Cirrhose prouvée par biopsie, hypertension portale documentée, épisodes d'hémorragie gastro-intestinale haute par HTP, épisodes d'encéphalopathie ou de coma hépatique.
# * Maladie cardio-vasculaire: Maladie classée Classe 4 de la NYHA.
# * Maladie respiratoire: Maladie restrictive, obstructive ou vasculaire réduisant sévèrement l'activité physique; Hypoxie ou hypercapnie chronique documentée, polycythémie secondaire, HTAP sévère ou dépendance respiratoire.
# * Maladie rénale: Hémodialyse chronique.
# * Immuno-dépression:Patient sous traitement immuno suppresseur, chimiothérapie, radiothérapie, stéroïdes au long cours à haute dose, maladie préalable telle que leucémie, lymphome, SIDA.



source("./script_introductif.R")



# Extraction des donnes administratives : Age 



paste(admid2,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)

sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)

unique(ICUSTAYS$SUBJECT_ID)->patient_id
paste(patient_id,collapse = ",")->pids
sql <- paste("SELECT * FROM PATIENTS WHERE SUBJECT_ID IN  (", pids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
PATIENTS<-dbFetch(res,n=-1)


merge(ICUSTAYS[c( "HADM_ID", "SUBJECT_ID" ,  "ICUSTAY_ID" ,  "INTIME"     )], PATIENTS [c(  "SUBJECT_ID", "DOB" )], by = "SUBJECT_ID")->temp0  

# Transformation en variable temporelle. 
temp0$INTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp0$DOB->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$DOB= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp0$INTIME, temp0$DOB ,unit="days") / 365.25 ->temp0$Age_years
as.numeric(temp0$Age_years)->temp0$Age_years
temp0->save0


###########################################################################################################
###########################################################################################################
# Extraction des données cliniques.



## !!  Comme la lecture du fichier CHARTEVENTS est longue, 
## il vaut mieux extraire les données une fois et les analyser dan un second temps une fois qu'elles sont extraites. 

read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$INCLURE.. == 1)->include
c(include$ITEMID_CV, include$ITEMID_MV)->ITEMID
# # # ITEMID[!is.na(ITEMID)]->itemids
# # # paste(itemids,collapse = ",")->items
# # # paste(itemids,collapse = ",")->items
# # # paste(admid,collapse = ",")->ids
# # # sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
# # # res <- dbSendQuery(con2, sql)
# # # ptm <- proc.time()
# # # CHARTEVENTS<-dbFetch(res,n=-1)
# # # proc.time() - ptm ->tmpsec
# # # write.table(file="./tables_analyses/chartevents1.csv", CHARTEVENTS, sep=",")
# # 
#   


read.table(file ="./resultats/chartevents_clinique1.csv", sep ="," , header = TRUE)->temp0



# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))



save0[c(  "HADM_ID"  ,   "INTIME"  )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS

subset(temp2, temp2$TEMPS <= 24 & temp2$TEMPS > 0 )->temp3


temp3 [c("HADM_ID","ICUSTAY_ID","ITEMID","VALUE","VALUENUM","VALUEUOM","TEMPS" )]->temp4





########################################

# Selection du nom des variables à partir du fichier include. 

include[c("VARIABLE",  "ITEMID_MV" , "ITEMID_CV")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3


merge(temp4, inc3, by.x = "ITEMID" , by.y = "value" )->temp5


 
# Calcul du score de Glasgow total
var<-c("Galsgow – réponse motrice","Glasgow – réponse verbale","Glasgow- ouverture des yeux" )
subset(temp5, temp5$VARIABLE %in% var)->tempZ
cast(tempZ ,  HADM_ID + ICUSTAY_ID    + TEMPS      ~ VARIABLE, value = "VALUE")->tempZ1
tempZ1$`Galsgow – réponse motrice`+ tempZ1$`Glasgow – réponse verbale` + tempZ1$`Glasgow- ouverture des yeux`->tempZ1$GLASGOW 
tempZ1 %>% group_by(ICUSTAY_ID) %>% slice(which.min(GLASGOW))  %>% select (HADM_ID, ICUSTAY_ID , GLASGOW) ->glasgow  





# Fréquence respiratoire
temp5 %>%
  filter(VARIABLE == "Fréquence respiratoire" ) %>%
  mutate(fr = ifelse(VALUE >= 50 , 4,
                    ifelse(VALUE >= 35 & VALUE  < 50, 3,
                                  ifelse(VALUE >= 25 & VALUE  < 35, 1,
                                         ifelse(VALUE >= 12 & VALUE  < 25, 0,
                                                ifelse(VALUE >= 10 & VALUE  < 12, 1, 
                                                       ifelse(VALUE >= 6 & VALUE  < 10, 2,
                                                              ifelse(VALUE  < 6 , 4, 
                          NA)  ))))))) %>% select (HADM_ID, ICUSTAY_ID , fr)  %>%
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(fr)) %>% distinct()->fr

# Temperature
temp5 %>%
  filter(VARIABLE == "Température (F)") %>%
  mutate(celsius = (VALUE - 32 )*5/9 ) %>%
  mutate(temp = ifelse(celsius >= 41 , 4,
                     ifelse(celsius >= 39 & celsius  < 41, 3,
                            ifelse(celsius >= 38.5 & celsius  < 39, 1,
                                   ifelse(celsius >= 36 & celsius  < 38.5, 0,
                                          ifelse(celsius >= 34 & celsius  < 36, 1, 
                                                 ifelse(celsius >= 32 & celsius  < 34, 2,
                                                        ifelse(celsius >= 30 & celsius  < 32, 3,
                                                               ifelse(celsius  < 30 , 4, 
                                                                      NA)  ))))))))   %>%
               select (HADM_ID, ICUSTAY_ID , temp)   %>%
                group_by(ICUSTAY_ID) %>% 
                slice(which.max(temp)) %>% distinct() ->temp


# Fréquence cardiaque  
temp5 %>%
  filter(VARIABLE == "Fréquence cardiaque") %>%
  mutate(fc = ifelse(VALUE >= 180 , 4,
                       ifelse(VALUE >= 140 & VALUE  < 180, 3,
                              ifelse(VALUE >= 110 & VALUE  < 140, 2,
                                     ifelse(VALUE >= 70 & VALUE  < 110, 0,
                                            ifelse(VALUE >= 55 & VALUE  < 70, 2, 
                                                   ifelse(VALUE >= 40 & VALUE  < 55, 3,
                                                                 ifelse(VALUE  < 40 , 4, 
                                                                        NA)  )))))))   %>%
  select (HADM_ID, ICUSTAY_ID , fc)   %>%
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(fc)) %>% distinct() ->fc



# Pression artérielle moyenne. 

temp5 %>%
  filter(VARIABLE == "NIBP mean" ) %>%
  mutate(nibp = ifelse(VALUE >= 159 , 4,
                     ifelse(VALUE >= 129 & VALUE  < 159, 3,
                            ifelse(VALUE >= 109 & VALUE  < 129, 2,
                                   ifelse(VALUE >= 69 & VALUE  < 109, 0,
                                          ifelse(VALUE >= 49 & VALUE  < 69, 2, 
                                                        ifelse(VALUE  < 49 , 4, 
                                                               NA)  )))))) %>% select (HADM_ID, ICUSTAY_ID , nibp) %>% 
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(nibp)) %>% distinct() ->nibp






merge(glasgow, nibp, by = c("ICUSTAY_ID" , "HADM_ID") , all = TRUE)->a
merge(a, temp,by = c("ICUSTAY_ID", "HADM_ID"), all = TRUE )->b
merge(b, fc,by = c("ICUSTAY_ID", "HADM_ID"), all = TRUE )->c
merge(c, fr,by = c("ICUSTAY_ID", "HADM_ID"), all = TRUE )->clinique


################################################################ Données de laboratoires ########################
# Oxygenation 


read.xlsx(file="./tables auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
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



temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


# Hospitalisation en USI. 
save0[c(  "HADM_ID"  , "ICUSTAY_ID" ,   "INTIME"  )]->temp1
paste(as.character(temp1$HADM_ID), as.character(temp1$ICUSTAY_ID), sep ="-")->temp1$ID 


merge(temp1, temp0, by = c( "HADM_ID" ), all = TRUE ) ->temp2


difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS

subset(temp2, temp2$TEMPS <= 36 & temp2$TEMPS > -24)->temp3


temp3 [c("HADM_ID","ICUSTAY_ID", "LABEL" , "FLUID","VALUE","VALUENUM","VALUEUOM","TEMPS" )]->temp4



# pH
temp4 %>%
  filter(LABEL == "pH"   ) %>%
  mutate(ph = ifelse(VALUE >= 7.7 , 4,
                       ifelse(VALUE >= 7.6 & VALUE  < 7.7, 3,
                              ifelse(VALUE >= 7.5 & VALUE  < 7.6, 1,
                                     ifelse(VALUE >= 7.33 & VALUE  < 7.5, 0,
                                            ifelse(VALUE >= 7.25 & VALUE  < 7.33, 2, 
                                                   ifelse(VALUE >= 7.15 & VALUE  < 7.25, 3, 
                                                   ifelse(VALUE  < 7.15 , 4, 
                                                          NA)  ))))))) %>% select (HADM_ID, ICUSTAY_ID , ph) %>% 
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(ph)) %>% distinct() ->ph


# Na
temp4 %>%
  filter(LABEL == "Sodium"   ) %>%
  mutate(Na = ifelse(VALUE >= 180 , 4,
                     ifelse(VALUE >= 160 & VALUE  < 180, 3,
                            ifelse(VALUE >= 155 & VALUE  < 160, 2,
                                   ifelse(VALUE >= 150 & VALUE  < 155, 1,
                                          ifelse(VALUE >= 130 & VALUE  < 150, 0 , 
                                                 ifelse(VALUE >= 130 & VALUE  < 150, 0 , 
                                                        ifelse(VALUE >= 120 & VALUE  < 130, 2,
                                                               ifelse(VALUE >= 111 & VALUE  < 120, 3,
                                                                      ifelse(VALUE  < 111 , 4, 
                                                                             NA)  ))))))))) %>% select (HADM_ID, ICUSTAY_ID , Na) %>% 
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(Na)) %>% distinct() ->Na



# K

temp4 %>%
  filter(LABEL == "Potassium"   ) %>%
  mutate(K = ifelse(VALUE >= 7 , 4,
                     ifelse(VALUE >= 6 & VALUE  < 7, 3,
                            ifelse(VALUE >= 5.5 & VALUE  < 6, 2,
                                   ifelse(VALUE >= 3.5 & VALUE  < 5.5, 0,
                                          ifelse(VALUE >= 3 & VALUE  < 3.5, 1 , 
                                                 ifelse(VALUE >= 2.5 & VALUE  < 3, 2 , 
                                                                      ifelse(VALUE  < 2.5 , 4, 
                                                                             NA)  ))))))) %>% select (HADM_ID, ICUSTAY_ID , K) %>% 
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(K)) %>% distinct() ->K



# Hematocrit

temp4 %>%
  filter(LABEL == "Hematocrit"  ) %>%
  mutate(Ht = ifelse(VALUE >= 60 , 4,
                    ifelse(VALUE >= 50 & VALUE  < 60, 2,
                           ifelse(VALUE >= 46 & VALUE  < 50, 1,
                                  ifelse(VALUE >= 30 & VALUE  < 46, 0,
                                         ifelse(VALUE >= 20 & VALUE  < 30, 2 , 
                                                       ifelse(VALUE  < 20 , 4, 
                                                              NA)  )))))) %>% select (HADM_ID, ICUSTAY_ID , Ht) %>% 
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(Ht)) %>% distinct() ->Ht


# Whole blood count

temp4 %>%
  filter(LABEL == "White Blood Cells"   ) %>%
  mutate(wbc = ifelse(VALUE >= 40 , 4,
                     ifelse(VALUE >= 20 & VALUE  < 40, 2,
                            ifelse(VALUE >= 15 & VALUE  < 20, 1,
                                   ifelse(VALUE >= 3 & VALUE  < 15, 0,
                                          ifelse(VALUE >= 1 & VALUE  < 3, 2 , 
                                                 ifelse(VALUE  < 1 , 4, 
                                                        NA)  )))))) %>% select (HADM_ID, ICUSTAY_ID , wbc) %>% 
  group_by(ICUSTAY_ID) %>% 
  slice(which.max(wbc)) %>% distinct() ->wbc


















PaO2

Sat oxygène
temp5 %>%
  filter(VARIABLE == "PaO2" )

temp5 %>%
  filter(VARIABLE == "Sat oxygène" )->fiO2


# On crée une petite figure pour visualiser dans un premier temps la FiO2 des malades. 
ggplot(data = fiO2, aes (x = VALUE)) + geom_density()   + xlim (c(0,100))










# Cette fonction prends le maximum par groupe.

[1] Systolique                  Pression artérelle moyenne  Glasgow- ouverture des yeux Fréquence cardiaque         Galsgow – réponse motrice  
[6] NIBP systolique             NIBP mean                   Pain Cause                  Pain Location               Pain Management            
[11] Pain Type                   Fréquence respiratoire      Température (F)             Glasgow – réponse verbale   Poids à l’admission (kg)   
[16] PaCO2                       PaO2                        Sat oxygène                 Taille (inch)               Pain Level                 
[21] Pain Present                Pain Assessment Method      Pain Level Acceptable       Diastolique                 NIBP diastolique           
[26] Riker-SAS Scale             Daily Wake Up Deferred      Untoward Effect             Gaz                         Pain Level Response        
[31] Pressure Ulcer Stage \\#1   Pressure Ulcer Stage \\#2   Daily Wake Up  


Température (F)
NIBP mean
Fréquence respiratoire
Fréquence cardiaque         
Galsgow – réponse motrice  Glasgow – réponse verbale   Glasgow- ouverture des yeux 
PaO2 
Sat oxygène


temp5 %>% 
  group_by(ICUSTAY_ID) %>%
  filter(VARIABLE == "Température (F)" ) %>%
  filter(VALUE == min(VALUE) & TEMPS == min(TEMPS))->res


temp6 %>% group_by(ICUSTAY_ID) %>% slice(which.min(VALUE))->bp  



       
Galsgow – réponse motrice  Glasgow – réponse verbale   Glasgow- ouverture des yeux 
PaO2 
Sat oxygène



 
df <- data.frame(
  A=c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  x=c(1, 1, 2, 2, 3, 4, 5, 5, 5),
  y=rnorm(9)
)
df %>% group_by(A) %>% slice(which.min(VALUE))



df <- data.frame(
  A=c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
  x=c(1, 1, 2, 2, 3, 4, 5, 5, 5),
  y=rnorm(9)
)


temp5 %>% group_by(HADM_ID, VARIABLE) %>% slice(which.min(VALUE))





 
 df <- data.frame(
   A=c("A", "A", "A", "B", "B", "B", "C", "C", "C"),
   x=c(1, 1, 2, 2, 3, 4, 5, 5, 5),
   y=rnorm(9)
 )
 
 library(dplyr)
 df.g <- group_by(df, A)
 filter(df.g, x == min(x))

 
 df %>% group_by(A) %>% slice(which.min(x))




# Ce script sert à calculer le score APACHE II à partir d'une liste d'identifiant fourni dans l'arrray ids. 








# Chargement des données socio-démographiques
read.table( file = "./tables_analyses/demographics_V2.csv", header = TRUE, sep = ",")->adms


# * Respiratory 
# * Renal 
# * Liver 
# * Cardiovascular 
# * Coagulation 
# * Systeme nerveux central


# Fichier LABEVENTS 
# * Creatinine 
# * Bilirubin 
# * Lactic acid
# * Platelets


read.xlsx(file="./tables auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$Inclure == 1)->temp1
temp1$ITEMID->listeids
paste(listeids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM LABEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
LABEVENTS<-dbFetch(res,n=-1)
sql <- paste("SELECT * FROM D_LABITEMS "  ,  sep=" " )
res <- dbSendQuery(con2, sql)
D_LABITEMS<-dbFetch(res,n=-1)
merge(LABEVENTS, D_LABITEMS, by = "ITEMID")->temp0



# Fichier CHARTEVENTS 

read.xlsx(file="./tables auxilliaires/Auxilliaire_CHARTEVENTS_for_SOFA.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$INCLURE.. == 1)->include
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
write.table(file="./tables_analyses/chartevents1_SOFA.csv", CHARTEVENTS, sep=",")

read.table(file ="./tables_analyses/chartevents1_SOFA.csv", sep ="," , header = TRUE)->temp0

# Tranformation de la donnée CHARTTIME en une donnée temporelle
temp0$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp0$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

ICUSTAYS[c(  "HADM_ID"  ,  "INTIME"   )]->temp1
merge(temp0,temp1, by = "HADM_ID")->temp2
difftime(temp2$CHARTTIME, temp2$INTIME , unit = "hours")->temp2$TEMPS

subset(temp2, temp2$TEMPS > -8 & temp2$TEMPS <72)->temp3

write.table(file="./tables_analyses/chartevents_3days.csv", temp3,  sep=",")

read.table(file ="./tables_analyses/chartevents_3days.csv", sep ="," , header = TRUE)->temp4
include[c("VARIABLE",  "ITEMID_MV" , "ITEMID_CV")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3

merge(temp4, inc3, by.x = "ITEMID" , by.y = "value" )->temp5


subset(temp5, temp5$TEMPS >-8 & temp5$TEMPS < 24 )->temp6

temp6$VALUE[temp6$VARIABLE == "NIBP mean" & temp6$VALUE == 0]<-NA





temp6$VALUE[temp6$VARIABLE == "Fi_O2"  &  temp6$VALUE == "3" ]<-30
temp6$VALUE[temp6$VARIABLE == "Fi_O2"  &  temp6$VALUE == "4" ]<-40
temp6$VALUE[temp6$VARIABLE == "Fi_O2"  &  temp6$VALUE == "8" ]<-80
temp6$VALUE[temp6$VARIABLE == "Fi_O2"  &  temp6$VALUE == "0" ]<-21
temp6$VALUE[temp6$VARIABLE == "Fi_O2" & temp6$VALUE <=1]*100->temp6$VALUE[temp6$VARIABLE == "Fi_O2" & temp6$VALUE <=1]

table(temp6$VALUE[temp6$VARIABLE == "Fi_O2"])
cast(temp6, HADM_ID ~ VARIABLE, value = "VALUE", min)->temp7
temp7->cli0


# Extraction des données biologiques. 



read.xlsx(file="./tables auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
subset(ty, ty$Inclure == 1)->temp1
temp1$ITEMID->listeids
paste(listeids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM LABEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
LABEVENTS<-dbFetch(res,n=-1)
sql <- paste("SELECT * FROM D_LABITEMS "  ,  sep=" " )
res <- dbSendQuery(con2, sql)
D_LABITEMS<-dbFetch(res,n=-1)
merge(LABEVENTS, D_LABITEMS, by = "ITEMID")->temp0



# Remarque : il existe 11 patients pour lesquels nous n'avons aucune donnée biologique. 
no_bio_data <-c(104941,105494,127122,134290,148977,149562,152285,159333,175983,189515,197245)



# Correction de la fonction temps en utilisant le fichier d'admission. 

merge(ICUSTAYS[c("HADM_ID",  "INTIME")], temp0, by = "HADM_ID" ) ->temp1
temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
temp1$INTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
difftime(temp1$CHARTTIME, temp1$INTIME,unit="hours")->temp1$TEMPS 
as.numeric(temp1$TEMPS)->temp1$TEMPS

temp1->bio0
cast(bio0, HADM_ID ~ LABEL, value = "VALUE", min)->biomin
cast(bio0, HADM_ID ~ LABEL, value = "VALUE", max)->biomax
biomin[c("HADM_ID" , "pO2" , "Platelet Count" )]->biomin1
biomax[c("HADM_ID" , "Bilirubin, Total" , "Creatinine"  )]->biomax1
merge(biomin1,biomax1, by = "HADM_ID" , all = TRUE )->bio2

#  
merge(adms[c("HADM_ID", "STATUS_D27" )], cli0, by = "HADM_ID", all.x = TRUE ) ->merge0 
merge(merge0, bio2, by = "HADM_ID", all.x = TRUE ) ->merge1




# Etape 3 : Posologie des amines vasopressives. 












# Calcul du score SOFA à partir du fichier merge1. 

merge1$Fi_O2[merge1$Fi_O2 == "Inf"] <-21
merge(ICUSTAYS[c("HADM_ID", "DBSOURCE")], merge1, by = "HADM_ID")->merge2

## Respiratory
merge2$PaO2 *100 /merge2$Fi_O2 ->merge2$rapport
merge2$Respiratory<-NA
merge2$Respiratory[merge2$rapport < 75 & merge2$rapport != "Inf" & !is.na(merge2$rapport)]<-4
merge2$Respiratory[merge2$rapport > 75 & merge2$rapport <=150 & merge2$rapport != "Inf" & !is.na(merge2$rapport)]<-3
merge2$Respiratory[merge2$rapport > 151 & merge2$rapport <=250 & merge2$rapport != "Inf" & !is.na(merge2$rapport)]<-2
merge2$Respiratory[merge2$rapport > 250 & merge2$rapport != "Inf" & !is.na(merge2$rapport)]<-1

# Renal 
merge2$REIN<-NA
as.numeric(as.character(merge2$Creatinine))->merge2$Creatinine
merge2$REIN[merge2$Creatinine <1.2 & !is.na(merge2$Creatinine)]<-0
merge2$REIN[merge2$Creatinine >=1.2 &  merge2$Creatinine < 2.2 & !is.na(merge2$Creatinine)]  <-1
merge2$REIN[merge2$Creatinine >=2.2 &  merge2$Creatinine < 4 & !is.na(merge2$Creatinine)]  <-2
merge2$REIN[merge2$Creatinine >=4 &  merge2$Creatinine < 5.5 & !is.na(merge2$Creatinine)]  <-3
merge2$REIN[merge2$Creatinine >= 5.5 & !is.na(merge2$Creatinine)]  <-4

#FOIE
merge2$FOIE<-NA
as.numeric(as.character(merge2$`Bilirubin, Total`))->merge2$`Bilirubin, Total`
merge2$FOIE[merge2$`Bilirubin, Total` <1.2 & !is.na(merge2$`Bilirubin, Total`)]<-0
merge2$FOIE[merge2$`Bilirubin, Total` >=1.2 &  merge2$`Bilirubin, Total` < 3.5 & !is.na(merge2$`Bilirubin, Total`)]  <-1
merge2$FOIE[merge2$`Bilirubin, Total` >=3.5 &  merge2$`Bilirubin, Total` < 7 & !is.na(merge2$`Bilirubin, Total`)]  <-2
merge2$FOIE[merge2$`Bilirubin, Total` >=7 &  merge2$`Bilirubin, Total` < 14 & !is.na(merge2$`Bilirubin, Total`)]  <-3
merge2$FOIE[merge2$`Bilirubin, Total` >= 14 & !is.na(merge2$`Bilirubin, Total`)]  <-4

# Platelets 
merge2$Platelets<-NA
as.numeric(as.character(merge2$`Platelet Count`))->merge2$`Platelet Count`
merge2$Platelets[merge2$`Platelet Count` >= 150 & !is.na(merge2$`Platelet Count`)]<-0
merge2$Platelets[merge2$`Platelet Count` >=100 &  merge2$`Platelet Count` < 150 & !is.na(merge2$`Platelet Count`)]  <-1
merge2$Platelets[merge2$`Platelet Count` >=50 &  merge2$`Platelet Count` < 100 & !is.na(merge2$`Platelet Count`)]  <-2
merge2$Platelets[merge2$`Platelet Count` >=20 &  merge2$`Platelet Count`< 50 & !is.na(merge2$`Platelet Count`)]  <-3
merge2$Platelets[merge2$`Platelet Count` <20 & !is.na(merge2$`Platelet Count`)]  <-4

# Pression artérielle. 



```






