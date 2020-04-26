# Script bactériologie 
# Ce sript extrait les informations de bactériologie de chacun des patients; 


# Merci d'indiquer un fichier de destination des résultas. 




# Création d'une fonction 
## Selection table et classifie la microbiologie

categorize<-function(data, column) {
  
  data[[column]]->categorie  


BGP<-c("CORYNEBACTERIUM SPECIE" , "CORYNEBACTERIUM SPECIE", "GEMELLA SPECIES", "ANAEROBIC GRAM POSITIV", "CORYNEBCATERIUM AMYCOL", 
       "PROBABLE GARDNERELLA V" , "PRESUMPTIVE GARDNERELL" , "GRAM POSITIVE BACTERIA" , "PROPIONIBACTERIUM ACNE" )
BGN<-c( "GRAM NEGATIVE ROD #3",  "GRAM NEGATIVE ROD #2" , "GRAM NEGATIVE ROD #1" ,  "GRAM NEGATIVE ROD(S)" , "STENOTROPHOMONAS (XANT", "CHRYSEOBACTERIUM MENIN", 
       "HAEMOPHILUS INFLUENZAE", "BACILLUS SPECIES; NOT ", "HAEMOPHILUS PARAINFLUE", "FUSOBACTERIUM SPECIES",  "AEROMONAS HYDROPHILIA ", "GRAM NEGATIVE ROD #4", 
       "HAEMOPHILUS SP","HAEMOPHILUS SPECIES NO")
CGP<-c("GRAM POSITIVE COCCUS(C" , "PRESUMPTIVE PEPTOSTREP", "GRAM POSITIVE RODS", "LACTOBACILLUS SPECIES", "PEDIOCOCCUS SPECIES", 
       "MICROCOCCUS/STOMATOCOC", "SLACKIA EXIGUA" , "DOLOSIGRANULUM PIGRUM" , "ABIOTROPHIA/GRANULICAT", "PRESUMPTIVE PROPIONIBA", 
         "RESEMBLING MICROCOCCUS", "CRYPTOCOCCUS SPECIES","CORYNEBACTERIUM UREALY" , "PROBABLE MICROCOCCUS S")
CGN<-c("NEISSERIA MENINGITIDIS", "PRESUMPTIVE VEILLONELL","AEROCOCCUS VIRIDANS" , "NEISSERIA SPECIES", "NEISSERIA GONORRHOEAE" , "MORAXELLA CATARRHALIS")





Other<-c("MYCOBACTERIUM GORDONAE" , "HERPES SIMPLEX VIRUS T" , "PENICILLIUM SPECIES" , "POSITIVE FOR PNEUMOCYS" , "BORDETELLA BRONCHISEPT", 
         "LEGIONELLA SPECIES","BACILLUS SPECIES","INFLUENZA A VIRUS","CYTOMEGALOVIRUS ","CRYPTOCOCCUS NEOFORMAN","POSITIVE FOR CYTOMEGAL","MOLD",
         "MYCELIA STERILIA","ASPERGILLUS NIGER", "YEAST, PRESUMPTIVELY N" , "SACCHAROMYCES CEREVISI", "CUNNINGHAMELLA SP." , "ASPERGILLUS FUMIGATUS" , 
         "VIRUS" ,  "NON-FERMENTER, NOT PSE" , "COCCIDIOIDES IMMITIS" , "LEGIONELLA PNEUMOPHILA", "YEAST" , "RHIZOPUS SPECIES" , "NUTRITIONALLY VARIANT ", "AEROMONAS SPECIES", 
         "PASTEURELLA MULTOCIDA" , "PAECILOMYCES SPECIES", "ASPERGILLUS SPECIES" , "ASPERGILLUS SP. NOT FU" , "BURKHOLDERIA CEPACIA G", "ALCALIGENES FAECALIS" , "RALSTONIA PICKETTII", 
         "FUSARIUM SPECIES" , "BURKHOLDERIA (PSEUDOMO" , "2ND ISOLATE" , "ORGANISM" , "NO GROWTH", "CANCELLED" )

Proteus<-c("PROTEUS MIRABILIS","PROTEUS VULGARIS" ,"PROTEUS SPECIES", "PROTEUS VULGARIS GROUP" )

Enterococcus<-c("ENTEROBACTERIACEAE" , "ENTEROCOCCUS FAECALIS" , "ENTEROCOCCUS FAECIUM" , "ENTEROCOCCUS SP." , "PROBABLE ENTEROCOCCUS" ,  "ENTEROCOCCUS GALLINARU" )

Enterobacter<-c(  "ENTEROBACTER CLOACAE", "ENTEROBACTERIACEAE", "ENTEROBACTER AEROGENES" , "ENTEROBACTER SPECIES" , "ENTEROBACTER CLOACAE C" ,
                  "ENTEROBACTER SAKAZAKII", "ENTEROBACTER ASBURIAE")

Citrobacter<-c("CITROBACTER FREUNDII C" , "CITROBACTER KOSERI", "CITROBACTER AMALONATIC", "CITROBACTER YOUNGAE")

Entero<-c("MORGANELLA MORGANII",  "PREVOTELLA SPECIES", "PROVIDENCIA STUARTII"  ,   'HAFNIA ALVEI' , 'SERRATIA MARCESCENS', 
          "PROVIDENCIA RETTGERI"   )

Staphyloccus<-c(  "STAPH AUREUS COAG +" , "STAPHYLOCOCCUS SAPROPH" , "STAPHYLOCOCCUS, COAGUL" , "STAPHYLOCOCCUS SPECIES" , "STAPHYLOCOCCUS EPIDERM" , "STAPHYLOCOCCUS HOMINIS"  )

Mycobacteries<-c("MYCOBACTERIUM AVIUM CO" , "AFB GROWN IN CULTURE; " , "MYCOBACTERIUM TUBERCUL" , "MYCOBACTERIUM ABSCESSU" , "ACIDFAST BACILLI" , "POSITIVE FOR M. TUBERC")

categorie[grep(x =  categorie,  "CANDIDA")] <-"Candida spp"
categorie[grep(x =  categorie,  "STREPTO")]<-"Streptococcus spp"
categorie[grep(x =  categorie,  "CLOSTRI")]<-"Clostridium spp"
categorie[grep(x =  categorie, "KLEBSIELL")]<-"Klebsiella spp"
categorie[grep(x =  categorie,  "ESCHERICHIA COLI" )]<-"E. Coli"
categorie[grep(x =  categorie, "PSEUDOMONAS" )]<-"Pseudomonas spp"
categorie[grep(x =  categorie,  "ACINETOBACTER" )]<-"Acinetobacter spp"
categorie[grep(x =  categorie, "BACTEROIDES" )]<-"Bacteroides spp"
categorie[categorie %in% Proteus ]<-"Proteus spp."
categorie[categorie %in% Enterococcus ]<-"Enterococcus spp."
categorie[categorie %in% Enterobacter ]<-"Enterobacter spp."
categorie[categorie %in% Citrobacter ]<-"Citrobacter spp."
categorie[categorie %in% Mycobacteries ]<-"Mycobacterium spp."
categorie[categorie %in% Staphyloccus ]<-"Staphylococcus spp."



# Catgories
categorie[categorie %in% BGP ]<-"Other Gram pos bacilli"
categorie[ categorie %in% BGN ]<-"Other Gram neg bacilli"
categorie[ categorie %in% CGP ]<-"Other Gram pos cocci"
categorie[ categorie %in% CGN ]<-"Other Gram neg cocci"
categorie[ categorie %in% Entero ]<-"Autres entérobactéries"
categorie[ categorie %in% Other ]<-"Other pathogens"




print("Une liste appelée liste1  vient d'être crée")

categorie->liste1

return(liste1)


}




# Les variables d'interêt sont dans la variable admid. 



paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM MICROBIOLOGYEVENTS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
MICROBIOLOGYEVENTS<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM D_LABITEMS ", " " ,  sep=" " )
res <- dbSendQuery(con2, sql)
D_LABITEMS<-dbFetch(res,n=-1)



# Il faut que l'on selectionne les prélèvements en USI uniquement. 

sql <- paste("SELECT * FROM ADMISSIONS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
ADMISSIONS<-dbFetch(res,n=-1)

sql <- paste("SELECT * FROM ICUSTAYS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
ICUSTAYS<-dbFetch(res,n=-1)


# Cette partie du script ne fonctionne que s'il y'a 1 séjour de réa et 1 séjour d'USI par hospitalisaiton; 


merge(ADMISSIONS[c("HADM_ID","ADMITTIME","DISCHTIME")] , ICUSTAYS[c( "HADM_ID",  "INTIME","OUTTIME")] , by = "HADM_ID")->TIME

TIME$INTIME->times
dtparts = t(as.data.frame(strsplit(times,' ')))
row.names(dtparts) = NULL
TIME$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

TIME$OUTTIME->times
dtparts = t(as.data.frame(strsplit(times,' ')))
row.names(dtparts) = NULL
TIME$OUTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

difftime(TIME$INTIME, TIME$ADMITTIME,unit="hours")->TIME$ENTRE_USI
difftime(TIME$OUTTIME, TIME$ADMITTIME,unit="hours")->TIME$SORTIE_USI

merge(MICROBIOLOGYEVENTS , TIME[c("HADM_ID"  , "ADMITTIME" ,   "ENTRE_USI" , "SORTIE_USI" )]  , by = "HADM_ID")->temp0

# On exclut les prélèvements ne disposant pas de temps
subset(temp0, temp0$CHARTTIME != "")->temp1


temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(times,' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME = chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
difftime(temp1$CHARTTIME, temp1$ADMITTIME,unit="hours")->temp1$heure_prelevement


temp1$setting<-NA
temp1$setting[temp1$heure_prelevement < temp1$ENTRE_USI  ]<-"avant"
temp1$setting[temp1$heure_prelevement > temp1$SORTIE_USI  ]<-"après"
temp1$setting[temp1$heure_prelevement >= temp1$ENTRE_USI  & temp1$heure_prelevement <= temp1$SORTIE_USI ]<-"USI"

#unique((temp1[c( "HADM_ID","CHARTTIME",  "SPEC_TYPE_DESC" , "setting")] ))->temp2

subset(temp1, temp1$setting == "USI")->temp3
temp3->MICROBIOLOGYEVENTS

subset(temp1, temp1$setting %in% c("avant" , "USI", "après"))->temp4
temp4->MICROBIOLOGYEVENTS2

##################################################################
# 1) La première table analyse le nombre de prélèvement par patient. 
##################################################################

unique((MICROBIOLOGYEVENTS[c( "HADM_ID","CHARTTIME",  "SPEC_TYPE_DESC")] ))->temp0
temp0$Number<-1
cast(temp0,  HADM_ID ~ SPEC_TYPE_DESC , value = "Number", length)->temp1
write.csv(file = "./tables_analyses/prelevement.csv",  x = temp1, row.names = FALSE)

## Nombre de patients préléver pour chaqye type d'échantillon

unique((MICROBIOLOGYEVENTS[c( "HADM_ID","CHARTTIME",  "SPEC_TYPE_DESC")] ))->temp0
temp0$Number<-1
cast(temp0,  HADM_ID ~ SPEC_TYPE_DESC , value = "Number", length)->temp1
temp1[, -1][temp1[, -1] == 0]<-"no"
temp1[, -1][temp1[, -1] !=  "no"]<-"yes"
write.csv(file = "./tables_analyses/prelevement_par_patient.csv",  x = temp1, row.names = FALSE)

# Nombre de patients avec prélèvement positif pour chaque type d'échantillon. 


unique((MICROBIOLOGYEVENTS[c( "HADM_ID","CHARTTIME",  "SPEC_TYPE_DESC" , "ORG_NAME")] ))->temp0
subset(temp0, temp0$ORG_NAME != "")->temp1
unique((temp1[c( "HADM_ID","CHARTTIME",  "SPEC_TYPE_DESC")] ))->temp2
temp2$Number<-1
cast(temp2,  HADM_ID ~ SPEC_TYPE_DESC , value = "Number", length)->temp3
temp3[, -1][temp3[, -1] == 0]<-"negative"
temp3[, -1][temp3[, -1] !=  "negative"]<-"positive"
write.csv(file = "./tables_analyses/prelevement_par_patient_positive.csv",  x = temp3, row.names = FALSE)


##################################################################
# 2) Nombre de prélèvements positifs selon la durée. 
##################################################################

# Quand sont positifs les prélèvements ? 
# Pourcentage de prélèvement positif par patient par période. 
unique(MICROBIOLOGYEVENTS2 [c( "HADM_ID","heure_prelevement","SPEC_TYPE_DESC","ORG_NAME" , "ENTRE_USI")])->temp0 
temp0$week<-NA
(temp0$heure_prelevement - temp0$ENTRE_USI )/24->temp0$jours
temp0$week[temp0$jours <0]<-"S-1"
temp0$week[temp0$jours >0 & temp0$jours <= 7]<-"S1"
temp0$week[temp0$jours > 7 & temp0$jours <= 14]<-"S2"
temp0$week[temp0$jours > 14 & temp0$jours <= 21]<-"S3"
temp0$week[temp0$jours > 21 & temp0$jours <= 28]<-"S4"
# On selectionne les germes présents les qutres premières semaines. 
subset(temp0, temp0$week %in% c("S-1","S1", "S2", "S3", "S4"))->temp1



# On selectionne les prélèvements suivants 
pvt2<-c("BLOOD CULTURE" , "URINE" , "SPUTUM" , "BRONCHOALVEOLAR LAVAGE" ,  "SWAB"   )

#####################################
pvt<-c("BLOOD CULTURE"   )
subset(temp1, temp1$SPEC_TYPE_DESC %in% pvt)->temp2
temp2$ORG_NAME [temp2$ORG_NAME == ""]<-0
temp2$ORG_NAME [temp2$ORG_NAME != 0]<-1
cast(temp2, HADM_ID  + week ~ ORG_NAME )->temp3
# On réutilise le fichier TIME crééer plus tot
melt(as.data.frame(cbind(unique(TIME), "S-1" , "S1", "S2", "S3","S4")), id = c( "HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI"))->i
colnames(i)<-c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI","value", "week")
unique(i[c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI", "week")])->i2
i2$SPEC_TYPE_DESC<-pvt
#Fusion des deux fichiers
merge(i2, temp3, by = c("HADM_ID", "week"), all = TRUE)->i3
# Créaton d'une variable status
i3$status<-NA
# On note lorsque le patient est sorti
i3$status[i3$SORTIE_USI/24 <7 & i3$week %in% c("S2", "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <14 & i3$week %in% c( "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <21 & i3$week %in% c(  "S4") ]<-"sorti"
# On note lorsqu'il n'y a pas eu de prélèvements. 
i3$status[ i3$`1` > 0 & !is.na(i3$`1`)]<-"Positive"
i3$status[ i3$`1` == 0 ]<-"Negative"
i3$status[ is.na(i3$status)]<-"No sample"
i3->bloodculture

#####################################
pvt<-c("URINE"   )
subset(temp1, temp1$SPEC_TYPE_DESC %in% pvt)->temp2
temp2$ORG_NAME [temp2$ORG_NAME == ""]<-0
temp2$ORG_NAME [temp2$ORG_NAME != 0]<-1
cast(temp2, HADM_ID  + week ~ ORG_NAME )->temp3
# On réutilise le fichier TIME crééer plus tot
melt(as.data.frame(cbind(unique(TIME),"S-1" , "S1", "S2", "S3","S4")), id = c( "HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI"))->i
colnames(i)<-c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI","value", "week")
unique(i[c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI", "week")])->i2
i2$SPEC_TYPE_DESC<-pvt
#Fusion des deux fichiers
merge(i2, temp3, by = c("HADM_ID", "week"), all = TRUE)->i3
# Créaton d'une variable status
i3$status<-NA
# On note lorsque le patient est sorti
i3$status[i3$SORTIE_USI/24 <7 & i3$week %in% c("S2", "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <14 & i3$week %in% c( "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <21 & i3$week %in% c(  "S4") ]<-"sorti"
# On note lorsqu'il n'y a pas eu de prélèvements. 
i3$status[ i3$`1` > 0 & !is.na(i3$`1`)]<-"Positive"
i3$status[ i3$`1` == 0 ]<-"Negative"
i3$status[ is.na(i3$status)]<-"No sample"
i3->urine
#####################################
pvt<-c("SPUTUM"  )
subset(temp1, temp1$SPEC_TYPE_DESC %in% pvt)->temp2
temp2$ORG_NAME [temp2$ORG_NAME == ""]<-0
temp2$ORG_NAME [temp2$ORG_NAME != 0]<-1
cast(temp2, HADM_ID  + week ~ ORG_NAME )->temp3
# On réutilise le fichier TIME crééer plus tot
melt(as.data.frame(cbind(unique(TIME),"S-1" , "S1", "S2", "S3","S4")), id = c( "HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI"))->i
colnames(i)<-c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI","value", "week")
unique(i[c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI", "week")])->i2
i2$SPEC_TYPE_DESC<-pvt
#Fusion des deux fichiers
merge(i2, temp3, by = c("HADM_ID", "week"), all = TRUE)->i3
# Créaton d'une variable status
i3$status<-NA
# On note lorsque le patient est sorti
i3$status[i3$SORTIE_USI/24 <7 & i3$week %in% c("S2", "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <14 & i3$week %in% c( "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <21 & i3$week %in% c(  "S4") ]<-"sorti"
# On note lorsqu'il n'y a pas eu de prélèvements. 
i3$status[ i3$`1` > 0 & !is.na(i3$`1`)]<-"Positive"
i3$status[ i3$`1` == 0 ]<-"Negative"
i3$status[ is.na(i3$status)]<-"No sample"
i3->sputum

#####################################
pvt<-c("BRONCHOALVEOLAR LAVAGE" )
subset(temp1, temp1$SPEC_TYPE_DESC %in% pvt)->temp2
temp2$ORG_NAME [temp2$ORG_NAME == ""]<-0
temp2$ORG_NAME [temp2$ORG_NAME != 0]<-1
cast(temp2, HADM_ID  + week ~ ORG_NAME )->temp3
# On réutilise le fichier TIME crééer plus tot
melt(as.data.frame(cbind(unique(TIME),"S-1" , "S1", "S2", "S3","S4")), id = c( "HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI"))->i
colnames(i)<-c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI","value", "week")
unique(i[c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI", "week")])->i2
i2$SPEC_TYPE_DESC<-pvt
#Fusion des deux fichiers
merge(i2, temp3, by = c("HADM_ID", "week"), all = TRUE)->i3
# Créaton d'une variable status
i3$status<-NA
# On note lorsque le patient est sorti
i3$status[i3$SORTIE_USI/24 <7 & i3$week %in% c("S2", "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <14 & i3$week %in% c( "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <21 & i3$week %in% c(  "S4") ]<-"sorti"
# On note lorsqu'il n'y a pas eu de prélèvements. 
i3$status[ i3$`1` > 0 & !is.na(i3$`1`)]<-"Positive"
i3$status[ i3$`1` == 0 ]<-"Negative"
i3$status[ is.na(i3$status)]<-"No sample"
i3->lba
#####################################
pvt<-c("SWAB" )
subset(temp1, temp1$SPEC_TYPE_DESC %in% pvt)->temp2
temp2$ORG_NAME [temp2$ORG_NAME == ""]<-0
temp2$ORG_NAME [temp2$ORG_NAME != 0]<-1
cast(temp2, HADM_ID  + week ~ ORG_NAME )->temp3
# On réutilise le fichier TIME crééer plus tot
melt(as.data.frame(cbind(unique(TIME),"S-1" , "S1", "S2", "S3","S4")), id = c( "HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI"))->i
colnames(i)<-c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI","value", "week")
unique(i[c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI", "week")])->i2
i2$SPEC_TYPE_DESC<-pvt
#Fusion des deux fichiers
merge(i2, temp3, by = c("HADM_ID", "week"), all = TRUE)->i3
# Créaton d'une variable status
i3$status<-NA
# On note lorsque le patient est sorti
i3$status[i3$SORTIE_USI/24 <7 & i3$week %in% c("S2", "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <14 & i3$week %in% c( "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <21 & i3$week %in% c(  "S4") ]<-"sorti"
# On note lorsqu'il n'y a pas eu de prélèvements. 
i3$status[ i3$`1` > 0 & !is.na(i3$`1`)]<-"Positive"
i3$status[ i3$`1` == 0 ]<-"Negative"
i3$status[ is.na(i3$status)]<-"No sample"
i3->swab
#####################################
pvt<-c("BLOOD CULTURE" , "URINE" , "SPUTUM" , "BRONCHOALVEOLAR LAVAGE" ,  "SWAB"   )
subset(temp1, temp1$SPEC_TYPE_DESC %in% pvt)->temp2
temp2$ORG_NAME [temp2$ORG_NAME == ""]<-0
temp2$ORG_NAME [temp2$ORG_NAME != 0]<-1
cast(temp2, HADM_ID  + week ~ ORG_NAME )->temp3
# On réutilise le fichier TIME crééer plus tot
melt(as.data.frame(cbind(unique(TIME),"S-1" , "S1", "S2", "S3","S4")), id = c( "HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI"))->i
colnames(i)<-c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI","value", "week")
unique(i[c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI", "week")])->i2
i2$SPEC_TYPE_DESC<-"At least 1 infection site"
#Fusion des deux fichiers
merge(i2, temp3, by = c("HADM_ID", "week"), all = TRUE)->i3
# Créaton d'une variable status
i3$status<-NA
# On note lorsque le patient est sorti
i3$status[i3$SORTIE_USI/24 <7 & i3$week %in% c("S2", "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <14 & i3$week %in% c( "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <21 & i3$week %in% c(  "S4") ]<-"sorti"
# On note lorsqu'il n'y a pas eu de prélèvements. 
i3$status[ i3$`1` > 0 & !is.na(i3$`1`)]<-"Positive"
i3$status[ i3$`1` == 0 ]<-"Negative"
i3$status[ is.na(i3$status)]<-"No sample"
i3->overall

#####################################
temp1->temp2
temp2$ORG_NAME [temp2$ORG_NAME == ""]<-0
temp2$ORG_NAME [temp2$ORG_NAME != 0]<-1
cast(temp2, HADM_ID  + week ~ ORG_NAME )->temp3
# On réutilise le fichier TIME crééer plus tot
melt(as.data.frame(cbind(unique(TIME),"S-1" , "S1", "S2", "S3","S4")), id = c( "HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI"))->i
colnames(i)<-c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI","value", "week")
unique(i[c("HADM_ID","ADMITTIME","DISCHTIME","INTIME","OUTTIME","ENTRE_USI","SORTIE_USI", "week")])->i2
i2$SPEC_TYPE_DESC<-"All possible"
#Fusion des deux fichiers
merge(i2, temp3, by = c("HADM_ID", "week"), all = TRUE)->i3
# Créaton d'une variable status
i3$status<-NA
# On note lorsque le patient est sorti
i3$status[i3$SORTIE_USI/24 <7 & i3$week %in% c("S2", "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <14 & i3$week %in% c( "S3", "S4") ]<-"sorti"
i3$status[i3$SORTIE_USI/24 <21 & i3$week %in% c(  "S4") ]<-"sorti"
# On note lorsqu'il n'y a pas eu de prélèvements. 
i3$status[ i3$`1` > 0 & !is.na(i3$`1`)]<-"Positive"
i3$status[ i3$`1` == 0 ]<-"Negative"
i3$status[ is.na(i3$status)]<-"No sample"
i3->all

rbind(bloodculture, urine, sputum, lba, swab, overall, all)->l

ggplot(data = l, aes (x = SPEC_TYPE_DESC , fill = status)) + geom_bar() + facet_grid( . ~  week) +  theme(axis.text.x = element_text(angle = 90))->g1

tiff(file = "./tables_analyses/figure_bacterio1.tiff", res = 300, width = 10, height = 10 , units = "in")
g1
dev.off() 




##################################################################################################
#################################################  Hémocultures #################################################
##################################################################################################

## Types de germes urinaires par catégories
subset(MICROBIOLOGYEVENTS, MICROBIOLOGYEVENTS$SPEC_TYPE_DESC  %in% c("BLOOD CULTURE ( MYCO/F", "BLOOD CULTURE" )  & MICROBIOLOGYEVENTS$ORG_NAME != "")->temp
unique((temp[c( "HADM_ID","CHARTTIME",  "ORG_NAME")] ))->temp0
categorize(data = temp0, column = "ORG_NAME")->liste1
temp0$CAT<- liste1 
temp0$Number<-1
cast(temp0,  HADM_ID ~ CAT , value = "Number", length)->temp1
temp1[, -1][temp1[, -1] == 0]<-"no"
temp1[, -1][temp1[, -1] !=  "no"]<-"yes"
write.csv(file = "./tables_analyses/hemocs_prelevement_categorie.csv",  x = temp1, row.names = FALSE)

# Précision sur les catégories "Others"

cats<-cats<-colnames(temp1)[-1]

c0<-c("Groupe", "Pathogenes")

for (i in cats) {   
  
  print(i)
  subset(temp0, temp0$CAT == i)->echantillon
  if (nrow(echantillon) == 0){ 
    
    b[1]<-i
    b[-1]<-"Nothing"
    
  } else {  
    cast(echantillon,  HADM_ID ~ ORG_NAME , value = "Number", length)->echantillon2
    echantillon2[, -1][echantillon2[, -1] == 0]<-"no"
    echantillon2[, -1][echantillon2[, -1] !=  "no"]<-"yes"
    
    CreateTableOne(data = echantillon2,vars =  colnames(echantillon2) [!(colnames(echantillon2) %in% c("HADM_ID"))])->T1
    invisible(capture.output(print(T1, nonnormal = TRUE)->T2))
    as.data.frame(T2)->T3
    gsub(x = rownames(T3), pattern = " = yes \\(%)", replacement = "")->a
    a[1]<-i
    paste(a, paste("n=", as.character(T3[,1]), sep=""))->b
  }
  c(  b[1],  paste(b[-1],  collapse = "\\\n"))->c1
  rbind(c0, c1)->c0
}
write.csv(file = "./tables_analyses/hemocs_others.csv",  x = c0, row.names = FALSE)



##################################################################################################
#################################################  LBA  #################################################
##################################################################################################


## Types de germes urinaires par catégories
subset(MICROBIOLOGYEVENTS, MICROBIOLOGYEVENTS$SPEC_TYPE_DESC == "BRONCHOALVEOLAR LAVAGE"  & MICROBIOLOGYEVENTS$ORG_NAME != "")->lba
unique((lba[c( "HADM_ID","CHARTTIME",  "ORG_NAME")] ))->temp0
categorize(data = temp0, column = "ORG_NAME")->liste1
temp0$CAT<- liste1 
temp0$Number<-1
cast(temp0,  HADM_ID ~ CAT , value = "Number", length)->temp1
temp1[, -1][temp1[, -1] == 0]<-"no"
temp1[, -1][temp1[, -1] !=  "no"]<-"yes"
write.csv(file = "./tables_analyses/lba_prelevement_categorie.csv",  x = temp1, row.names = FALSE)

# Précision sur les catégories "Others"

cats<-colnames(temp1)[-1]

c0<-c("Groupe", "Pathogenes")

for (i in cats) {   
  
print(i)
subset(temp0, temp0$CAT == i)->echantillon
if (nrow(echantillon) == 0){ 
  
  b[1]<-i
  b[-1]<-"Nothing"
  
} else {  
cast(echantillon,  HADM_ID ~ ORG_NAME , value = "Number", length)->echantillon2
echantillon2[, -1][echantillon2[, -1] == 0]<-"no"
echantillon2[, -1][echantillon2[, -1] !=  "no"]<-"yes"

CreateTableOne(data = echantillon2,vars =  colnames(echantillon2) [!(colnames(echantillon2) %in% c("HADM_ID"))])->T1
invisible(capture.output(print(T1, nonnormal = TRUE)->T2))
as.data.frame(T2)->T3
gsub(x = rownames(T3), pattern = " = yes \\(%)", replacement = "")->a
a[1]<-i
paste(a, paste("n=", as.character(T3[,1]), sep=""))->b
}
c(  b[1],  paste(b[-1],  collapse = "\\\n"))->c1
rbind(c0, c1)->c0
}
write.csv(file = "./tables_analyses/lba_others.csv",  x = c0, row.names = FALSE)



##################################################################################################
#################################################  SWAB #################################################
##################################################################################################

## Types de germes urinaires par catégories
subset(MICROBIOLOGYEVENTS, MICROBIOLOGYEVENTS$SPEC_TYPE_DESC == "SWAB"  & MICROBIOLOGYEVENTS$ORG_NAME != "")->temp
unique((temp[c( "HADM_ID","CHARTTIME",  "ORG_NAME")] ))->temp0
categorize(data = temp0, column = "ORG_NAME")->liste1
temp0$CAT<- liste1 
temp0$Number<-1
cast(temp0,  HADM_ID ~ CAT , value = "Number", length)->temp1
temp1[, -1][temp1[, -1] == 0]<-"no"
temp1[, -1][temp1[, -1] !=  "no"]<-"yes"
write.csv(file = "./tables_analyses/swab_prelevement_categorie.csv",  x = temp1, row.names = FALSE)

# Précision sur les catégories "Others"

cats<-colnames(temp1)[-1]

c0<-c("Groupe", "Pathogenes")

for (i in cats) {   
  
  print(i)
  subset(temp0, temp0$CAT == i)->echantillon
  if (nrow(echantillon) == 0){ 
    
    b[1]<-i
    b[-1]<-"Nothing"
    
  } else {  
    cast(echantillon,  HADM_ID ~ ORG_NAME , value = "Number", length)->echantillon2
    echantillon2[, -1][echantillon2[, -1] == 0]<-"no"
    echantillon2[, -1][echantillon2[, -1] !=  "no"]<-"yes"
    
    CreateTableOne(data = echantillon2,vars =  colnames(echantillon2) [!(colnames(echantillon2) %in% c("HADM_ID"))])->T1
    invisible(capture.output(print(T1, nonnormal = TRUE)->T2))
    as.data.frame(T2)->T3
    gsub(x = rownames(T3), pattern = " = yes \\(%)", replacement = "")->a
    a[1]<-i
    paste(a, paste("n=", as.character(T3[,1]), sep=""))->b
  }
  c(  b[1],  paste(b[-1],  collapse = "\\\n"))->c1
  rbind(c0, c1)->c0
}
write.csv(file = "./tables_analyses/swab_others.csv",  x = c0, row.names = FALSE)




##################################################################################################
#################################################  SPUTUM  #################################################
##################################################################################################

## Types de germes urinaires par catégories
subset(MICROBIOLOGYEVENTS, MICROBIOLOGYEVENTS$SPEC_TYPE_DESC == "SPUTUM"  & MICROBIOLOGYEVENTS$ORG_NAME != "")->temp
unique((temp[c( "HADM_ID","CHARTTIME",  "ORG_NAME")] ))->temp0
categorize(data = temp0, column = "ORG_NAME")->liste1
temp0$CAT<- liste1 
temp0$Number<-1
cast(temp0,  HADM_ID ~ CAT , value = "Number", length)->temp1
temp1[, -1][temp1[, -1] == 0]<-"no"
temp1[, -1][temp1[, -1] !=  "no"]<-"yes"
write.csv(file = "./tables_analyses/sputum_prelevement_categorie.csv",  x = temp1, row.names = FALSE)

# Précision sur les catégories "Others"

cats<-colnames(temp1)[-1]

c0<-c("Groupe", "Pathogenes")

for (i in cats) {   
  
  print(i)
  subset(temp0, temp0$CAT == i)->echantillon
  if (nrow(echantillon) == 0){ 
    
    b[1]<-i
    b[-1]<-"Nothing"
    
  } else {  
    cast(echantillon,  HADM_ID ~ ORG_NAME , value = "Number", length)->echantillon2
    echantillon2[, -1][echantillon2[, -1] == 0]<-"no"
    echantillon2[, -1][echantillon2[, -1] !=  "no"]<-"yes"
    
    CreateTableOne(data = echantillon2,vars =  colnames(echantillon2) [!(colnames(echantillon2) %in% c("HADM_ID"))])->T1
    invisible(capture.output(print(T1, nonnormal = TRUE)->T2))
    as.data.frame(T2)->T3
    gsub(x = rownames(T3), pattern = " = yes \\(%)", replacement = "")->a
    a[1]<-i
    paste(a, paste("n=", as.character(T3[,1]), sep=""))->b
  }
  c(  b[1],  paste(b[-1],  collapse = "\\\n"))->c1
  rbind(c0, c1)->c0
}
write.csv(file = "./tables_analyses/sputum_others.csv",  x = c0, row.names = FALSE)


##################################################################################################
#################################################  CATHETER TIP IV  #################################################
##################################################################################################

## Types de germes urinaires par catégories
subset(MICROBIOLOGYEVENTS, MICROBIOLOGYEVENTS$SPEC_TYPE_DESC == "CATHETER TIP-IV"  & MICROBIOLOGYEVENTS$ORG_NAME != "")->temp
unique((temp[c( "HADM_ID","CHARTTIME",  "ORG_NAME")] ))->temp0
categorize(data = temp0, column = "ORG_NAME")->liste1
temp0$CAT<- liste1 
temp0$Number<-1
cast(temp0,  HADM_ID ~ CAT , value = "Number", length)->temp1
temp1[, -1][temp1[, -1] == 0]<-"no"
temp1[, -1][temp1[, -1] !=  "no"]<-"yes"
write.csv(file = "./tables_analyses/catheter_prelevement_categorie.csv",  x = temp1, row.names = FALSE)

# Précision sur les catégories "Others"

cats<-colnames(temp1)[-1]

c0<-c("Groupe", "Pathogenes")

for (i in cats) {   
  
  print(i)
  subset(temp0, temp0$CAT == i)->echantillon
  if (nrow(echantillon) == 0){ 
    
    b[1]<-i
    b[-1]<-"Nothing"
    
  } else {  
    cast(echantillon,  HADM_ID ~ ORG_NAME , value = "Number", length)->echantillon2
    echantillon2[, -1][echantillon2[, -1] == 0]<-"no"
    echantillon2[, -1][echantillon2[, -1] !=  "no"]<-"yes"
    
    CreateTableOne(data = echantillon2,vars =  colnames(echantillon2) [!(colnames(echantillon2) %in% c("HADM_ID"))])->T1
    invisible(capture.output(print(T1, nonnormal = TRUE)->T2))
    as.data.frame(T2)->T3
    gsub(x = rownames(T3), pattern = " = yes \\(%)", replacement = "")->a
    a[1]<-i
    paste(a, paste("n=", as.character(T3[,1]), sep=""))->b
  }
  c(  b[1],  paste(b[-1],  collapse = "\\\n"))->c1
  rbind(c0, c1)->c0
}
write.csv(file = "./tables_analyses/catheter_others.csv",  x = c0, row.names = FALSE)




##################################################################################################
#################################################  URINE  #################################################
##################################################################################################

## Types de germes urinaires par catégories
subset(MICROBIOLOGYEVENTS, MICROBIOLOGYEVENTS$SPEC_TYPE_DESC == "URINE"  & MICROBIOLOGYEVENTS$ORG_NAME != "")->temp
unique((temp[c( "HADM_ID","CHARTTIME",  "ORG_NAME")] ))->temp0
categorize(data = temp0, column = "ORG_NAME")->liste1
temp0$CAT<- liste1 
temp0$Number<-1
cast(temp0,  HADM_ID ~ CAT , value = "Number", length)->temp1
temp1[, -1][temp1[, -1] == 0]<-"no"
temp1[, -1][temp1[, -1] !=  "no"]<-"yes"
write.csv(file = "./tables_analyses/urine_prelevement_categorie.csv",  x = temp1, row.names = FALSE)

# Précision sur les catégories "Others"

cats<-colnames(temp1)[-1]

c0<-c("Groupe", "Pathogenes")

for (i in cats) {   
  
  print(i)
  subset(temp0, temp0$CAT == i)->echantillon
  if (nrow(echantillon) == 0){ 
    
    b[1]<-i
    b[-1]<-"Nothing"
    
  } else {  
    cast(echantillon,  HADM_ID ~ ORG_NAME , value = "Number", length)->echantillon2
    echantillon2[, -1][echantillon2[, -1] == 0]<-"no"
    echantillon2[, -1][echantillon2[, -1] !=  "no"]<-"yes"
    
    CreateTableOne(data = echantillon2,vars =  colnames(echantillon2) [!(colnames(echantillon2) %in% c("HADM_ID"))])->T1
    invisible(capture.output(print(T1, nonnormal = TRUE)->T2))
    as.data.frame(T2)->T3
    gsub(x = rownames(T3), pattern = " = yes \\(%)", replacement = "")->a
    a[1]<-i
    paste(a, paste("n=", as.character(T3[,1]), sep=""))->b
  }
  c(  b[1],  paste(b[-1],  collapse = "\\\n"))->c1
  rbind(c0, c1)->c0
}
write.csv(file = "./tables_analyses/urine_others.csv",  x = c0, row.names = FALSE)




