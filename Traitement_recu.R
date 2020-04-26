
# Ce script sert à analyser l'utilisation des amines dans notre population. 

# Pour analyser les amines il faut analyser les fichiers INPUTEVENTS_CV et INPUTEVENTS_MV





paste(admid,collapse = ",")->ids
sql <- paste("SELECT HADM_ID, INTIME FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)



paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM INPUTEVENTS_CV WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
INPUTEVENTS_CV<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM D_ITEMS ",  sep=" " )
res <- dbSendQuery(con2, sql)
D_ITEMS<-dbFetch(res,n=-1)


   # CHARTTIME  represents the time at which the measurement was charted - that is - recorded on the 
   # clinical information system at the bedside. For amounts received (usually volumes), 
   # the CHARTTIME represents the time at which that volume was received. That is, it can be considered 
   # an “end time”, i.e. X millilitres of solution was administered to the patient by this CHARTTIME. 
   # For rates, the CHARTTIME represents the time at which that rate was set. 
   # That is, it can be considered a “start time”, i.e. the patient is now receiving X mcg/kg/min of a drug at this CHARTTIME.


merge(INPUTEVENTS_CV, D_ITEMS, by = "ITEMID")->temp0
merge(temp0, ICUSTAYS, by ="HADM_ID" )->temp1

temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(times,' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

temp1$INTIME->times
dtparts = t(as.data.frame(strsplit(times,' ')))
row.names(dtparts) = NULL
temp1$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

difftime(temp1$CHARTTIME,temp1$INTIME, unit="hours")->temp1$tmps_perfusion



# 1) On essaye de vidualiser combien de ligne et combien de ligne de rate et de Amount par patient. 


temp1->temp2
temp2$Data_type<-NA
temp2$Data_type [temp2$AMOUNT != "" & temp2$RATE == ""]<-"AMOUNT"
temp2$Data_type [temp2$AMOUNT == "" & temp2$RATE != ""]<-"RATE"
temp2$Data_type [temp2$AMOUNT == "" & temp2$RATE == ""]<-"None"
temp2$Data_type [temp2$AMOUNT != "" & temp2$RATE != ""]<-"Both"


# Analyse du volume d'hydratation recue 

"D5W"
"Lactated Ringers"
"Sodium Bicarbonate"
"OR Crystalloid" 
".9% Normal Saline" 









# Pour chaque patient on crée un pdf avec le traitement qu'il recoit. 

pdf(file ="./Resultats/input.pdf")

for (i in unique(temp2$HADM_ID)) {  
  print(i)
subset(temp2, temp2$HADM_ID == i )->y
as.character(y$ORDERID)->y$ORDERID
as.character(y$LINKORDERID)->y$LINKORDERID
ggplot(data = y, aes (x = tmps_perfusion, y= LABEL, fill = Data_type, color = LINKORDERID)) + geom_point() 

}

dev.off()  






























############################" Partie désuette #######################################












k<-c("HADM_ID" , "CHARTTIME"  , "ITEMID" , "AMOUNT" , "AMOUNTUOM" , "RATE", "RATEUOM" , "ORDERID",  "LINKORDERID")
INPUTEVENTS_CV[k]->tempCV
subset(tempCV, tempCV$AMOUNT != "")->tempCV1
colnames(tempCV1)[2]<-"STARTTIME"




paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM INPUTEVENTS_MV WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
INPUTEVENTS_MV<-dbFetch(res,n=-1)

k<-c("HADM_ID" , "STARTTIME"  , "ITEMID" , "AMOUNT" , "AMOUNTUOM" , "RATE", "RATEUOM" , "ORDERID",  "LINKORDERID")
INPUTEVENTS_MV[k]->tempMV


merge(tempCV1, tempMV, by = c("HADM_ID" , "STARTTIME"  , "ITEMID" , "AMOUNT" , 
                              "AMOUNTUOM" , "RATE", "RATEUOM" , "ORDERID",  "LINKORDERID"), all = TRUE)->tempCVMV


merge(tempCVMV, tempM, by.x = "ITEMID", by.y = "value")->tempCVMV1

merge(tempCVMV1, ICUSTAYS, by = "HADM_ID")->tempCVMV2

difftime(tempCVMV2$STARTTIME, tempCVMV2$INTIME, unit = "hours")->tempCVMV2$H
tempCVMV2[,!names(tempCVMV2) %in% c("CHARTTIME"  ,"INTIME" )]->tempCVMV3

tempCVMV3->temp3

as.numeric(as.character(temp3$H))->temp3$H

subset(temp3, temp3$H>=0 & temp3$H<168)->temp4
temp4$DAY<-NA

temp4$DAY[temp4$H >=0 & temp4$H <24  ]<-"Day 1"
temp4$DAY[temp4$H >=24 &  temp4$H <48]<-"Day 2"
temp4$DAY[temp4$H >=48 &  temp4$H <72]<-"Day 3"
temp4$DAY[temp4$H >=72 &  temp4$H <96]<-"Day 4"

temp4$DAY[temp4$H >=96 &  temp4$H <120]<-"Day 5"

temp4$DAY[temp4$H >=120 &  temp4$H <144]<-"Day 6"
temp4$DAY[temp4$H >=144 &  temp4$H <168]<-"Day 7"

temp4->temp5
paste(temp5$Médicament, temp5$AMOUNTUOM, sep=",")->temp5$LABEL
temp5$LABEL[temp5$LABEL == "Vasopressin,units"]<-"Vasopressin,U"

as.numeric(temp5$AMOUNT)->temp5$AMOUNT2

cast(temp5[c("HADM_ID", "LABEL", "DAY", "AMOUNT2")], HADM_ID + LABEL  ~ DAY, value = "AMOUNT2", sum )->temp6
temp6$`Day 1`+ temp6$`Day 2` + temp6$`Day 3` + temp6$`Day 4` +temp6$`Day 4` + temp6$`Day 5` + temp6$`Day 6` + temp6$`Day 7`->temp6$TotalSur7jours


subset(temp6, temp6$LABEL == "Epinephrine,mg" )->temp61
merge(admif,temp61,  by= "HADM_ID", all.x = TRUE)->temp0
temp0$LABEL[is.na(temp0$LABEL)]<-"No Epinephrine,mg"
temp0[is.na(temp0)]<-0
CreateTableOne(data = temp0, strata = "BL")










##########################
Toute la suite est désuette





# Chargement des patients BL 1 ou 0

read.table("/home/hedjoudje/Documents/Recherche/MIMICIII_generalites/tables-septic-shock/prescription-data.csv", 
            header= TRUE, sep=",")->admissions

admissions[c("HADM_ID",  "BL"  )]->admif

# Dose total d'amines. 


read.csv(file="./medocProj.csv", sep=",", header=TRUE)->temp0
subset(temp0, temp0$Classe == "Amines")->temp1

temp1[c(" Médicament", "ITEMID_CV", "ITEMID_MV")]
temp1[c("Médicament", "ITEMID_CV", "ITEMID_MV")]->temp2


melt(temp2, id = "Médicament" )->temp3
subset(temp3, temp3$value != "NA")->tempM



################################


paste(admid,collapse = ",")->ids
sql <- paste("SELECT HADM_ID, INTIME FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)



paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM INPUTEVENTS_CV WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
INPUTEVENTS_CV<-dbFetch(res,n=-1)

k<-c("HADM_ID" , "CHARTTIME"  , "ITEMID" , "AMOUNT" , "AMOUNTUOM" , "RATE", "RATEUOM" , "ORDERID",  "LINKORDERID")
INPUTEVENTS_CV[k]->tempCV
subset(tempCV, tempCV$AMOUNT != "")->tempCV1
colnames(tempCV1)[2]<-"STARTTIME"


paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM INPUTEVENTS_MV WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
INPUTEVENTS_MV<-dbFetch(res,n=-1)

k<-c("HADM_ID" , "STARTTIME"  , "ITEMID" , "AMOUNT" , "AMOUNTUOM" , "RATE", "RATEUOM" , "ORDERID",  "LINKORDERID")
INPUTEVENTS_MV[k]->tempMV


merge(tempCV1, tempMV, by = c("HADM_ID" , "STARTTIME"  , "ITEMID" , "AMOUNT" , 
                              "AMOUNTUOM" , "RATE", "RATEUOM" , "ORDERID",  "LINKORDERID"), all = TRUE)->tempCVMV


merge(tempCVMV, tempM, by.x = "ITEMID", by.y = "value")->tempCVMV1

merge(tempCVMV1, ICUSTAYS, by = "HADM_ID")->tempCVMV2

difftime(tempCVMV2$STARTTIME, tempCVMV2$INTIME, unit = "hours")->tempCVMV2$H
tempCVMV2[,!names(tempCVMV2) %in% c("CHARTTIME"  ,"INTIME" )]->tempCVMV3

tempCVMV3->temp3

as.numeric(as.character(temp3$H))->temp3$H

subset(temp3, temp3$H>=0 & temp3$H<168)->temp4
temp4$DAY<-NA

temp4$DAY[temp4$H >=0 & temp4$H <24  ]<-"Day 1"
temp4$DAY[temp4$H >=24 &  temp4$H <48]<-"Day 2"
temp4$DAY[temp4$H >=48 &  temp4$H <72]<-"Day 3"
temp4$DAY[temp4$H >=72 &  temp4$H <96]<-"Day 4"

temp4$DAY[temp4$H >=96 &  temp4$H <120]<-"Day 5"

temp4$DAY[temp4$H >=120 &  temp4$H <144]<-"Day 6"
temp4$DAY[temp4$H >=144 &  temp4$H <168]<-"Day 7"

temp4->temp5
paste(temp5$Médicament, temp5$AMOUNTUOM, sep=",")->temp5$LABEL
temp5$LABEL[temp5$LABEL == "Vasopressin,units"]<-"Vasopressin,U"

as.numeric(temp5$AMOUNT)->temp5$AMOUNT2

cast(temp5[c("HADM_ID", "LABEL", "DAY", "AMOUNT2")], HADM_ID + LABEL  ~ DAY, value = "AMOUNT2", sum )->temp6
temp6$`Day 1`+ temp6$`Day 2` + temp6$`Day 3` + temp6$`Day 4` +temp6$`Day 4` + temp6$`Day 5` + temp6$`Day 6` + temp6$`Day 7`->temp6$TotalSur7jours


subset(temp6, temp6$LABEL == "Epinephrine,mg" )->temp61
merge(admif,temp61,  by= "HADM_ID", all.x = TRUE)->temp0
temp0$LABEL[is.na(temp0$LABEL)]<-"No Epinephrine,mg"
temp0[is.na(temp0)]<-0
CreateTableOne(data = temp0, strata = "BL")







## Exploration de la taille: 

itemids<-c(226707 , 226730 , 920)
paste(itemids,collapse = ",")->items
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
res <- dbSendQuery(con2, sql)
ptm <- proc.time()
tailles<-dbFetch(res,n=-1)
proc.time() - ptm ->tmpsec

write.table(file="charteventsTAILLE.csv", tailles, sep=",")

tailles$VALUE[tailles$VALUE == 0]<-NA
mean(tailles$VALUE[tailles$ITEMID == 226730], na.rm = TRUE)

mean(tailles$VALUE[tailles$ITEMID == 226707], na.rm = TRUE)


tailles[c("HADM_ID", "ITEMID", "VALUE" )]->tailles2
tailles2$VALUE[tailles2$VALUE == 0]<-NA
tailles2$VALUE[tailles2$ITEMID == 920]*2.54->tailles2$VALUE[tailles2$ITEMID == 920]
tailles2$VALUE[tailles2$ITEMID == 226707]*2.54->tailles2$VALUE[tailles2$ITEMID == 226707]
tailles2$ITEMID<-"Taille(cm)"
cast(tailles2, HADM_ID~ ITEMID, mean)->tailles3

write.csv(file="/home/hedjoudje/Documents/Recherche/MIMICIII_generalites/tables-septic-shock/taille.csv", tailles3, row.names = FALSE)









