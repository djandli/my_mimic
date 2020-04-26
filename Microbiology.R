# tout d'abord, on commence par extraire le script R contenant les données de patients à extraire. 

## Le script permet de générer la table d'admission et les patients d'interêt (vecteur : admid)
source("./script_introductif.R")

# Chargement des données socio-démographiques
read.table( file = "./tables_analyses/demographics_V2.csv", header = TRUE, sep = ",")->adms




paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM MICROBIOLOGYEVENTS WHERE HADM_ID IN  (", ids, ") ",  sep=" " )
res <- dbSendQuery(con2, sql)
MICROBIOLOGYEVENTS<-dbFetch(res,n=-1)


sql <- paste("SELECT * FROM D_LABITEMS ", " " ,  sep=" " )
res <- dbSendQuery(con2, sql)
D_LABITEMS<-dbFetch(res,n=-1)


merge(MICROBIOLOGYEVENTS, adms[c("HADM_ID", "ID", "Entre_hospit","Sortie_usi","Sortie_hospit","STATUS_D27" )], by = "HADM_ID")->temp0


merge(ICUSTAYS[c("HADM_ID",  "INTIME")], temp0, by = "HADM_ID" ) ->temp1



as.character(temp1$INTIME)->temp1$INTIME
temp1$INTIME->times
dtparts = t(as.data.frame(strsplit(times,' ')))
row.names(dtparts) = NULL
temp1$INTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


as.character(temp1$CHARTTIME)->temp1$CHARTTIME
temp1$CHARTTIME[temp1$CHARTTIME == ""]<-NA

temp1$CHARTTIME->times
dtparts = t(as.data.frame(strsplit(times,' ')))
row.names(dtparts) = NULL
temp1$CHARTTIME= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))

difftime(temp1$CHARTTIME, temp1$INTIME,unit="hours")->temp1$TEMPS 
as.numeric(temp1$TEMPS)->temp1$TEMPS




write.csv(x = temp1, file = "./tables_analyses/microbiology.csv", row.names = FALSE)



# Hémocultures réalisées pendant le séjour. 

subset(temp1, temp1$SPEC_TYPE_DESC == "BLOOD CULTURE")->temp2

ggplot(data = temp2) + 
  geom_segment(aes(x = Entre_hospit, xend = Sortie_hospit , y = ID , yend = ID ), color = "black") + 
  geom_segment(aes(x = 0, xend = Sortie_usi , y = ID , yend = ID ), color = "orange" )+ 
  geom_title("Hémocultures réalisées pendant les hospitalisations") + 
  geom_point(aes(x = TEMPS , y = ID ), color = "red" , shape = 3) ->g01

pdf(file ="./microbiologyF1.pdf")
g01
dev.off() 

# Pourcentage d'hémocultures positives par temps. 

temp2$RESULTATS<-NA
temp2[c("HADM_ID","STATUS_D27","TEMPS","ORG_NAME","RESULTATS" )]->temp3 
temp3$RESULTATS[temp3$ORG_NAME == ""]<-"Sterile"
temp3$RESULTATS[temp3$ORG_NAME != ""]<-"Positive"

temp3$CAT_TEMPS<-NA
temp3$CAT_TEMPS[!is.na(temp3$TEMPS) & temp3$TEMPS >= 0 & temp3$TEMPS <48]<-"Within 48 hours"
temp3$CAT_TEMPS[!is.na(temp3$TEMPS) & temp3$TEMPS >= 48 & temp3$TEMPS <360]<-"Between 48 hours and D15"
temp3$CAT_TEMPS[!is.na(temp3$TEMPS) & temp3$TEMPS >= 360 ]<-"Greater than D15"
temp3$CAT_TEMPS[is.na(temp3$TEMPS)]<-"No time provided"


unique(subset(temp3, temp3$TEMPS >= 0))->temp4


temp4$CAT_TEMPS<-factor(temp4$CAT_TEMPS, levels = c( "Within 48 hours","Between 48 hours and D15","Greater than D15"))
g1<-ggplot(data = temp4) + geom_bar(aes(x = "BLOOD CULTURE" , fill = RESULTATS)) + facet_grid(  STATUS_D27 ~ CAT_TEMPS  )


g2<-ggplot(data = temp4) + geom_bar(aes(x = "BLOOD CULTURE" , fill = RESULTATS), position = "fill") + facet_grid(  STATUS_D27 ~ CAT_TEMPS  )

grid.arrange(g1,g2, ncol = 2)->g3


pdf(file ="./microbiologyF2.pdf", height= 10, width = 10)
grid.arrange(g1,g2, ncol = 2)->g3
dev.off() 




# Type de germes dans les hémocultures
subset(temp4, temp4$RESULTATS != "Sterile")->temp5


ggplot(data = temp5) + geom_bar(aes(x = "BLOOD CULTURE" , fill = ORG_NAME), position = "fill") + facet_grid(  STATUS_D27 ~ CAT_TEMPS  )->gA


# Si l'on choisit de regrouper les germes par famille

temp5->tempX

tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "CANDIDA ALBICANS" , "CANDIDA PARAPSILOSIS","CANDIDA KEFYR",
                                         "CANDIDA TROPICALIS", "CANDIDA (TORULOPSIS) G" ) ] <-"Candida"

tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "ENTEROCOCCUS FAECIUM" , "ENTEROCOCCUS FAECALIS" ,"ENTEROCOCCUS GALLINARU",
                                         "ENTEROCOCCUS SP."  ) ] <-"Enterocoque"


tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "KLEBSIELLA PNEUMONIAE", "KLEBSIELLA OXYTOCA" ) ] <-"Klebsielle"


tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "STREPTOCOCCUS SPECIES", "STREPTOCOCCUS BOVIS " , "BETA STREPTOCOCCUS GRO" , 
                                         "ALPHA STREPTOCOCCI", "VIRIDANS STREPTOCOCCI") ] <-"Streptocoques"


tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "STAPHYLOCOCCUS, COAGUL",  "STAPH AUREUS COAG +" , "STAPHYLOCOCCUS EPIDERM"  ) ] <-"Staphylocque"


tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "ENTEROBACTER CLOACAE"  ,  "ENTEROBACTER AEROGENES"   ) ] <-"Enterobacter"

tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "PSEUDOMONAS AERUGINOSA"   ) ] <-"Pseudomona A."

tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "ENTEROBACTER CLOACAE"  ,  "ENTEROBACTER AEROGENES"   ) ] <-"Enterobacter"

tempX$ORG_NAME [ tempX$ORG_NAME  %in% c( "ESCHERICHIA COLI"   ) ] <-"E. Coli"




var<-c ( "CORYNEBACTERIUM SPECIE" ,"STENOTROPHOMONAS (XANT", 
    "BACILLUS SPECIES; NOT ","BACTEROIDES FRAGILIS G" ,
    "CITROBACTER FREUNDII C","PRESUMPTIVE PROPIONIBA","NON-FERMENTER, NOT PSE",
    "ACINETOBACTER BAUMANNI","CLOSTRIDIUM SPECIES NO","PASTEURELLA MULTOCIDA",
    "PREVOTELLA SPECIES","LACTOBACILLUS SPECIES","GRAM POSITIVE COCCUS(C"   )

tempX$ORG_NAME [ tempX$ORG_NAME  %in% var ] <-"Other"


ggplot(data = tempX) + geom_bar(aes(x = "BLOOD CULTURE" , fill = ORG_NAME), position = "fill") + 
  facet_grid(  STATUS_D27 ~ CAT_TEMPS  ) + scale_fill_brewer(palette = "Set3") ->gA

pdf(file ="./microbiologyF5.pdf", height= 10, width = 10)
gA
dev.off()


# On reprends le reste du script; 
as.data.frame(table(temp5$ORG_NAME))->germs
germs[order(Freq),]

germs[order(-germs$Freq),]->germs1
as.character(germs1$Var1[germs1$Freq < 9])->germs2
temp5$ORG_NAME[temp5$ORG_NAME %in% germs2]<-"OTHER"

g1<-ggplot(data = temp5) + geom_bar(aes(x = "BLOOD CULTURE" , fill = ORG_NAME), position = "fill") + facet_grid(  STATUS_D27 ~ CAT_TEMPS  ) + scale_fill_brewer(palette="Paired")


pdf(file ="./microbiologyF3.pdf", height= 10, width = 10)
g1
dev.off() 



g2<-ggplot(data = temp5) + geom_bar(aes(x = "BLOOD CULTURE" , fill = ORG_NAME), position = "fill") + facet_grid(  . ~ CAT_TEMPS  ) + scale_fill_brewer(palette="Paired")


pdf(file ="./microbiologyF4.pdf", height= 10, width = 10)
g2
dev.off() 

grid.arrange(g1,g2, ncol = 2)


# Statistiques germes présents 

subset(temp4, temp4$RESULTATS != "Sterile")->temp5
as.data.frame(table(temp5$ORG_NAME))->germs
germs[order(-germs$Freq),]->germs1
as.character(germs1$Var1[germs1$Freq < 6])->germs2
temp5$ORG_NAME[temp5$ORG_NAME %in% germs2]<-"OTHER"


