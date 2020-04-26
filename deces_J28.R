
# Cette fonction sert à ajouter le décès à J28 comme covariable; 



# Chargement du fichier admissions avec nouvelle variables. 
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)

# Chargement du fichier ICUSTAYS
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)


# Fusion des fichiers
merge(ADMISSIONS, ICUSTAYS, by = "HADM_ID")->temp1

# Calcul des différences de temps
temp1$DEATHTIME[temp1$DEATHTIME != ""]->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
a<- chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$INTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime( a ,temp1$INTIME[temp1$DEATHTIME != ""], unit="days") ->temp1$TEMPS[temp1$DEATHTIME != ""]

temp1$DEATH_D7<-"no"
temp1$DEATH_D14<-"no"
temp1$DEATH_D21<-"no"
temp1$DEATH_D28<-"no"


temp1$DEATH_D7[temp1$TEMPS <= 7 & !is.na(temp1$TEMPS)  ]<-"yes"
temp1$DEATH_D14[temp1$TEMPS <= 14 & !is.na(temp1$TEMPS)  ]<-"yes"
temp1$DEATH_D21[temp1$TEMPS <= 21 & !is.na(temp1$TEMPS)  ]<-"yes"
temp1$DEATH_D28[temp1$TEMPS <= 28 & !is.na(temp1$TEMPS)  ]<-"yes"


write.csv(file = "./resultats/death_status.csv",  x = ADMISSIONS[c( "HADM_ID" , "DEAD" )], row.names = FALSE)

