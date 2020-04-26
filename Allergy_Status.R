# Création du fichier qui contient  allergie aux beta-lactamines et absence d'allergie aux beta-lactamines. 


# Nécéssite une array 
## admid




# Début du script 
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)

ADMISSIONS[c("HADM_ID"     ,         "ADMITTIME"        ,    "DISCHTIME"  )]->presscri

# Extraction des données de prescriptions

paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM PRESCRIPTIONS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
PRESCRIPTIONS<-dbFetch(res,n=-1)



#write.table("./tables-septic-shock/prescription-data.csv", x= PRESCRIPTIONS , row.names = FALSE, sep=",")


# Patiets ayant recu des betalactamines


# Création et utilisation d'une base de données intermédiare pour savoir si le patients ont recu des betalactamines ou non
## Les pénicillines sont les pénicillines, les cephalosporines et  les carbapénèmes. 
as.data.frame(cbind(unique(PRESCRIPTIONS$DRUG), 0))->tableATB
colnames(tableATB) <-c("drug", "beta-lactamine")
as.character(tableATB$`beta-lactamine`)->tableATB$`beta-lactamine`

tableATB$`beta-lactamine`[grep("cillin", tableATB$drug, ignore.case = TRUE)]<-"1" # Pénams( Pénicllines)
tableATB$`beta-lactamine`[grep("cef", tableATB$drug, ignore.case = TRUE)]<-"1" # Céphem
tableATB$`beta-lactamine`[grep("penem", tableATB$drug, ignore.case = TRUE)]<-"1" # Pénèmes
tableATB$`beta-lactamine`[grep("bactam", tableATB$drug, ignore.case = TRUE)]<-"1" # Pénèmes

# Patient ayant recu un traitement antibiotiques
merge(PRESCRIPTIONS, tableATB, by.x = "DRUG", by.y = "drug")->temp
subset(temp, temp$`beta-lactamine` == 1)->temp1
unique(temp1$HADM_ID)->patientBL



presscri$BL<-0
presscri$BL [presscri$HADM_ID %in% patientBL]<-"1"


presscri[c("HADM_ID", "BL")]->temp0

write.csv(x = temp0, file = "./Resultats/Allergy_status.csv" , row.names = FALSE)

