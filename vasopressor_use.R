
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

paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM INPUTEVENTS_MV WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
INPUTEVENTS_MV<-dbFetch(res,n=-1)



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
merge(INPUTEVENTS_MV, D_ITEMS, by = "ITEMID")->temp1


unique(temp0[c("HADM_ID", "LABEL")])
unique(rbind(unique(temp0[c("HADM_ID", "LABEL")]), unique(temp1[c("HADM_ID", "LABEL")])))->temp3

var<-c("Dobutamine","Dopamine","Epinephrine","Vasopressin", "Epinephrine-k","Norepinephrine","Phenylephrine","Fentanyl (Concentrate)")

temp3$VASOPRESSORS<-NA

temp3$VASOPRESSORS[temp3$LABEL %in% var]<-"yes"
temp3$VASOPRESSORS[!(temp3$LABEL %in% var)]<-"no"
temp3$VALUE<-1
cast(temp3, HADM_ID ~ VASOPRESSORS, fun.aggregate = length, value = "VALUE")->temp4

merge(ICUSTAYS["HADM_ID"], temp4, by = "HADM_ID", all = TRUE)->temp5
temp5$yes[is.na(temp5$yes)]<-"Missing"
temp5$yes[temp5$yes >= 1 & temp5$yes != "Missing"]<-"yes"
temp5$yes[temp5$yes == 0]<-"no"
colnames(temp5)[3]<-"Vasopressor"
temp5[c("HADM_ID", "Vasopressor")]->temp6


write.csv(x = temp6, file = "./resultats/vasopressor.csv", row.names = FALSE)

print("Un fichier appelé vasopressor.csv vient d'être créer dans l'environnement de travail")

