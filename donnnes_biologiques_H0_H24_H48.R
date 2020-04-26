# Données biologiques à la baseline, H24 et H48. 


## Certaines données biologiques ne sont pas présentes dans la table LABEVENTS, il est donc pertinent de tenter d'extraire les informations 
# à partir de la table CHARTEVENTS également. Il est important de garder à l'esprit que les informations de données d'hosptialisation multiples
# en réanimation ne sont pas mentionnées. 



# Chargement des données de LABEVENTS. 

## Selection des données de laboratoires à partir de LABEVENTS

read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_LABEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 1)->ty
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

merge(temp0, ADMISSIONS[c("HADM_ID", "ADMITTIME")], by = "HADM_ID", all = TRUE)->temp1
temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$ADMITTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$ADMITTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp1$CHARTTIME ,temp1$ADMITTIME, unit="hours") ->temp1$TEMPS
temp1$TABLE<-"LABEVENTS"
temp1->save1

# Selection des données de laboratoires à partir de CHARTEVENTS

read.xlsx(file="../SCRIPT_MIMIC/Auxilliaires/Auxilliaire_CHARTEVENTS.xlsx" , sheetIndex = 1,   header= TRUE, startRow = 2 )->ty
subset(ty, ty$labo_standard == 1)->include
c(include$ITEMID_CV, include$ITEMID_MV, include$ITEMID_CV2, include$ITEMID_CV3)->ITEMID
ITEMID[!is.na(ITEMID)]->itemids
paste(itemids,collapse = ",")->items

# # Extraction à partir des données d'hospitalisation
# paste(admid,collapse = ",")->ids
# sql <- paste("SELECT * FROM CHARTEVENTS WHERE HADM_ID IN  (", ids, ") AND ITEMID IN (",items, ")",  sep=" " )
# res <- dbSendQuery(con2, sql)
# ptm <- proc.time()
# CHARTEVENTS<-dbFetch(res,n=-1)
# proc.time() - ptm ->tmpsec
# write.table(file="./tables_analyses/charteventsBIO.csv", CHARTEVENTS, sep=",")

read.table(file ="./tables_analyses/charteventsBIO.csv", sep ="," , header = TRUE)->temp0


merge(temp0, ADMISSIONS[c("HADM_ID", "ADMITTIME")], by = "HADM_ID", all = TRUE)->temp1
temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$ADMITTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$ADMITTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp1$CHARTTIME ,temp1$ADMITTIME, unit="hours") ->temp1$TEMPS
temp1$TABLE<-"CHARTEVENTS"

# Extraction du nom des variables
include[c("VARIABLE",  "ITEMID_MV" , "ITEMID_CV", "ITEMID_CV2", "ITEMID_CV3")]->inc1
melt(inc1, id = "VARIABLE") [c("VARIABLE", "value")]->inc2
subset(inc2, inc2$value != "NA")->inc3
merge(temp1, inc3, by.x = "ITEMID" , by.y = "value" )->temp2



temp2->save2


save1[c("HADM_ID","LABEL","VALUE","TEMPS","TABLE" )]->save1a
save2[c("HADM_ID","VARIABLE","VALUE","TEMPS","TABLE" )]->save2a
colnames(save2a)[2]<-"LABEL"
rbind(save1a, save2a)->save3



# Biologie à retenir pour chaque hospitalisation. 
## Baseline = entrée en USI (1ère entrée pour hospitalisation multiple)
ICUSTAYS [c("HADM_ID","ICUSTAY_ID","INTIME","OUTTIME")]->icu
ADMISSIONS[c("HADM_ID" , "ADMITTIME", "DISCHTIME")]   ->adms       
          
          
merge(icu, adms, by = c("HADM_ID"))->y

y$ADMITTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
y$ADMITTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


y$INTIME->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
y$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))
difftime(  y$INTIME, y$ADMITTIME , unit="hours") ->y$Entre_USI
as.numeric(y$Entre_USI)->y$Entre_USI
y %>% group_by(HADM_ID) %>% slice(which.min(Entre_USI))->t



# On funsionne avec le fichier des données biologiques; 

merge(save3, y [c( "HADM_ID","Entre_USI")],by = "HADM_ID", all = TRUE)->save4
as.numeric(save4$TEMPS - save4$Entre_USI)->save4$Delta_T
as.numeric(as.character(save4$VALUE))->save4$VALUE




# On regroupe les variables prenant le même nom. 








# On calcule maintenant les valeurs moyennes aux différents temps. 

# A la baseline
save4 %>%  filter(Delta_T >= -6,  Delta_T <  6  ) %>% group_by(HADM_ID , LABEL ) %>%  summarize(Mean = mean(VALUE, na.rm=TRUE))->save5
as.data.frame(save5)->f


library("UpSetR")
gg_miss_upset(save5)

unique(save5$LABEL)[1]->a
save5 %>%  filter(LABEL == "Alanine Aminotransferase (ALT)" ) ->i

cast(save4, HADM_ID ~ LABEL , value = "VALUE" , fun.aggregate = mean)->k


# Visualisation des données manquantes

cast(save4, HADM_ID ~ LABEL , value = "VALUE" , fun.aggregate = length)->o


head(o[,-1])->f
f[f>0]<-"yes"
f[f == 0]<-"no"
cbind(head(o[,1]) , f)->g
colnames(g)[1]<-"HADM_ID"

melt(g, id = "HADM_ID")->h



ggplot(data = h, aes (x= variable, fill = value) ) + geom_bar() +  coord_flip()

exit; 



