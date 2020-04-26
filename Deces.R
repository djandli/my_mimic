
# Cette fonction sert à ajouter le décès à J28 comme covariable; 
# Elle décrit également le décès selon l'endroit (in-ICU, in-hospital, 1 year survical)

con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")


# Chargement du fichier admissions avec nouvelle variables. 
paste(admid,collapse = ",")->ids
# sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ")", sep=" " )
# res <- dbSendQuery(con2, sql)
# ADMISSIONS<-dbFetch(res,n=-1)
# 


paste(admid,collapse = ",")->ids
sql <- paste("select * from mimic3.admissions where  hadm_id in (", ids, ") ", sep=" " )
ADMISSIONS <- dbGetQuery(con, sql)




# Chargement du fichier ICUSTAYS
paste(admid,collapse = ",")->ids
sql <- paste("select * from mimic3.icustays where  hadm_id in (", ids, ") ", sep=" " )
ICUSTAYS <- dbGetQuery(con, sql)


# sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ")", sep=" " )
# res <- dbSendQuery(con2, sql)
# ICUSTAYS<-dbFetch(res,n=-1)


# Fusion des fichiers
merge(ADMISSIONS, ICUSTAYS, by = "hadm_id")->temp1

# Calcul des différences de temps
temp1$deathtime[temp1$deathtime != ""]->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
a<- chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$intime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$intime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

temp1$TEMPS<-NA
difftime( a[!is.na(temp1$deathtime)] ,temp1$intime[!is.na(temp1$deathtime)] , unit="days") ->temp1$TEMPS[!is.na(temp1$deathtime)] 




temp1$outtime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$outtime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))



temp1$dischtime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$dischtime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))




difftime(temp1$outtime ,temp1$intime, unit="days")->temp1$Sortie_USI

difftime(temp1$dischtime ,temp1$intime, unit="days")->temp1$Sortie_Hosto



# Décès de manière générale = donnée de sécurité sociale.

temp1$DEATH_D7<-"no"
temp1$DEATH_D14<-"no"
temp1$DEATH_D21<-"no"
temp1$DEATH_D28<-"no"
temp1$DEATH_D7[temp1$TEMPS <= 7 & !is.na(temp1$TEMPS)  ]<-"yes"
temp1$DEATH_D14[temp1$TEMPS <= 14 & !is.na(temp1$TEMPS)  ]<-"yes"
temp1$DEATH_D21[temp1$TEMPS <= 21 & !is.na(temp1$TEMPS)  ]<-"yes"
temp1$DEATH_D28[temp1$TEMPS <= 28 & !is.na(temp1$TEMPS)  ]<-"yes"


# Décès en USI 

temp1$DEATH_D7_USI<-"no"
temp1$DEATH_D14_USI<-"no"
temp1$DEATH_D21_USI<-"no"
temp1$DEATH_D28_USI<-"no"
temp1$DEATH_D7_USI[temp1$TEMPS <= 7 & !is.na(temp1$TEMPS) & temp1$TEMPS <= temp1$Sortie_USI   ]<-"yes"
temp1$DEATH_D14_USI[temp1$TEMPS <= 14 & !is.na(temp1$TEMPS)& temp1$TEMPS <= temp1$Sortie_USI  ]<-"yes"
temp1$DEATH_D21_USI[temp1$TEMPS <= 21 & !is.na(temp1$TEMPS)& temp1$TEMPS <= temp1$Sortie_USI  ]<-"yes"
temp1$DEATH_D28_USI[temp1$TEMPS <= 28 & !is.na(temp1$TEMPS) & temp1$TEMPS <= temp1$Sortie_USI  ]<-"yes"


# Décès à l'hôpital


temp1$Sortie_USI [ temp1$TEMPS < 0 & !is.na(temp1$TEMPS)]->temp1$TEMPS [ temp1$TEMPS < 0 & !is.na(temp1$TEMPS)]

temp1$DEATH_D7_HOSPI<-"no"
temp1$DEATH_D14_HOSPI<-"no"
temp1$DEATH_D21_HOSPI<-"no"
temp1$DEATH_D28_HOSPI<-"no"
temp1$DEATH_D7_HOSPI[temp1$TEMPS <= 7 & !is.na(temp1$TEMPS) 
                     & temp1$TEMPS > temp1$Sortie_USI & temp1$TEMPS <= temp1$Sortie_Hosto ]<-"yes"
temp1$DEATH_D14_HOSPI[temp1$TEMPS <= 14 & !is.na(temp1$TEMPS)
                      & temp1$TEMPS > temp1$Sortie_USI & temp1$TEMPS <= temp1$Sortie_Hosto ]<-"yes"
temp1$DEATH_D21_HOSPI[temp1$TEMPS <= 21 & !is.na(temp1$TEMPS) 
                      & temp1$TEMPS > temp1$Sortie_USI & temp1$TEMPS <= temp1$Sortie_Hosto ]<-"yes"
temp1$DEATH_D28_HOSPI[temp1$TEMPS <= 28 & !is.na(temp1$TEMPS) 
                      & temp1$TEMPS > temp1$Sortie_USI & temp1$TEMPS <= temp1$Sortie_Hosto ]<-"yes"



var<-c("hadm_id","DEATH_D7","DEATH_D14","DEATH_D21","DEATH_D28","DEATH_D7_USI",
       "DEATH_D14_USI","DEATH_D21_USI","DEATH_D28_USI","DEATH_D7_HOSPI",
       "DEATH_D14_HOSPI","DEATH_D21_HOSPI","DEATH_D28_HOSPI")

temp1[var]->temp2
melt(temp2, id = "hadm_id")->temp3
temp3$Death<-NA

temp3$Death[temp3$variable %in% c("DEATH_D7", "DEATH_D14","DEATH_D21","DEATH_D28")]<-"Mortality"
temp3$Death[temp3$variable %in% c("DEATH_D7_USI",
                                  "DEATH_D14_USI",
                                  "DEATH_D21_USI",
                                  "DEATH_D28_USI")]<-"In-ICU"
temp3$Death[temp3$variable %in% c("DEATH_D7_HOSPI",
                                  "DEATH_D14_HOSPI",
                                  "DEATH_D21_HOSPI",
                                  "DEATH_D28_HOSPI")]<-"In-hospital"

temp3$Day<-NA
temp3$Day[temp3$variable %in% var[grep( var, pattern =  "D7")]]<-"Day 7"
temp3$Day[temp3$variable %in% var[grep( var, pattern =  "D14")]]<-"Day 14"
temp3$Day[temp3$variable %in% var[grep( var, pattern =  "D21")]]<-"Day 21"
temp3$Day[temp3$variable %in% var[grep( var, pattern =  "D28")]]<-"Day 28"

#temp3$value[temp3$value == "yes"]<-1
#temp3$value[temp3$value == "no"]<-0
#as.numeric(temp3$value)->temp3$value
cast(data= temp3, hadm_id + Death ~ Day, value = "value")->temp4

cast(data= temp3, hadm_id + Day ~ Death, value = "value")->temp4b



#CreateTableOne(data = temp4, strata = "Death", vars =  c("Day 7","Day 14","Day 21","Day 28" ))
# Visualisation des différents temps 




write.csv(file = "./resultats/death_status.csv",  x = temp2, row.names = FALSE)

print("################################## \n ")
print(" Un fichier intitulé death_status.csv vient d'être créé dans l'environnement de travail")
print("################################## \n")


