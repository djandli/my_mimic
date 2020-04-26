read.table( file = "./tables_analyses/demographics_V2.csv", header = TRUE, sep = ",")->adms




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


# On ajoute les données numériques et de temps grâce au fichier de données socio-démographiques. 
read.table( file = "./tables_analyses/demographics_V2.csv", header = TRUE, sep = ",")->adms
merge(adms[c("ID", "HADM_ID","Sortie_usi","Sortie_hospit","Entre_hospit", "Delta_USI", "Delta_Hospit" , "STATUS_D27" , "DEAD_TIME")] , temp1, by = "HADM_ID")->temp2



# Quand est fait la lipase ? 
subset(temp2, temp2$LABEL == "Lipase")->temp3

temp3$ABNORMAL<-NA
temp3$ABNORMAL[temp3$FLAG == "abnormal"]<-"Abnormal"
temp3$ABNORMAL[temp3$FLAG != "abnormal"]<-"Normal"

ggplot(data = temp3) +  geom_segment(aes(x=Entre_hospit, xend=Sortie_hospit, y=ID, yend=ID )) + 
  geom_segment(aes(x=0, xend=Delta_USI, y=ID, yend=ID), color = "orange") + 
  geom_point(aes(x = TEMPS, y = ID , fill = ABNORMAL, shape = ABNORMAL, color = ABNORMAL ))->g0



pdf(file = "./labeventsF1.pdf")
g0
dev.off()  







# A partir du fichier temp2. On regarde le nombre variable biologique. 
## Si on regarde le nombre de nombre de patients présentant au moins une variable aux différents temps. 

#subset(temp2, temp2$LABEL %in% unique(temp2$LABEL)[1:10])->temp3
temp2->temp3

temp3$Within_M5_AND_3H<-0
temp3$Within_6H<-0
temp3$Within_12H<-0

temp3$Within_M5_AND_3H  [temp3$TEMPS >-5 & temp3$TEMPS <= 3]<-1
temp3$Within_6H  [temp3$TEMPS >-5 & temp3$TEMPS <= 6]<-1
temp3$Within_12H  [temp3$TEMPS >-5 & temp3$TEMPS <= 12]<-1

temp3[c("HADM_ID"  , "LABEL" ,"Within_6H","Within_12H","Within_M5_AND_3H")]->temp4
melt(temp4, id =  c("HADM_ID", "LABEL"))->temp5

cast(temp5, HADM_ID + LABEL ~ variable, value = "value", sum)->temp6
temp6$Binary_Within_M5_AND_3H<-"no"
temp6$Binary_Within_6H<-"no"
temp6$Binary_Within_12H<-"no"

temp6$Binary_Within_M5_AND_3H[temp6$Within_M5_AND_3H >= 1 ]<-"yes"
temp6$Binary_Within_6H[temp6$Within_6H >= 1 ]<-"yes"
temp6$Binary_Within_12H[temp6$Within_12H >= 1 ]<-"yes"

merge(adms[c("HADM_ID", "STATUS_D27")], temp6, by = "HADM_ID")->temp7

temp7$STATUS_D27 [temp7$STATUS_D27 == 0]<-"vivant"
temp7$STATUS_D27 [temp7$STATUS_D27 == 1]<-"deces"

melt(temp7, id = c("HADM_ID","STATUS_D27","LABEL","Within_6H","Within_12H","Within_M5_AND_3H" ))->temp8

# cast(temp8, HADM_ID + STATUS_D27 + LABEL + Within_6H Within_12H + Within_M5_AND_3H +  variable ~ value , su )
# 
# subset(temp8, temp8$value == "yes")->temp9
ggplot(data = temp8) + geom_bar(aes(x = STATUS_D27 , fill = value )  ,position = "fill" ) +
  facet_grid(  variable  ~ LABEL ) +
  theme(strip.text.x = element_text(size=12, angle=90, face="bold"),
        strip.text.y = element_text(size=10),
        strip.background = element_rect(colour="red", fill="grey")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) ->g01     #vertical x-axis


pdf(file = "./labeventsF2.pdf", height = 12, width = 15)
g01
dev.off()  





# Sous forme de tableau : pour chacun des intervalle de temps séparement. 

subset(temp8, temp8$variable == "Binary_Within_M5_AND_3H")->temp9
melt(temp9, id = c( "HADM_ID","STATUS_D27","Within_M5_AND_3H","Within_6H","Within_12H","variable", "value" ))->temp10
colnames(temp10)[7]<-"val"
temp10[c("HADM_ID","STATUS_D27", "value", "val")]->temp11
colnames(temp11)[3]<-"variable"
cast(temp11, HADM_ID + STATUS_D27  ~ variable , value = "val")->temp12
gsub(pattern = " ", replacement = "_", colnames(temp12))->colnames(temp12)
gsub(pattern = "[-,(\\)]", replacement = "", colnames(temp12))->colnames(temp12)
gsub(pattern = "/", replacement = "", colnames(temp12))->colnames(temp12)
CreateTableOne(data = temp12, strata = "STATUS_D27"  )->T1


subset(temp8, temp8$variable == "Binary_Within_6H")->temp9
melt(temp9, id = c( "HADM_ID","STATUS_D27","Within_M5_AND_3H","Within_6H","Within_12H","variable", "value" ))->temp10
colnames(temp10)[7]<-"val"
temp10[c("HADM_ID","STATUS_D27", "value", "val")]->temp11
colnames(temp11)[3]<-"variable"
cast(temp11, HADM_ID + STATUS_D27  ~ variable , value = "val")->temp12
gsub(pattern = " ", replacement = "_", colnames(temp12))->colnames(temp12)
gsub(pattern = "[-,(\\)]", replacement = "", colnames(temp12))->colnames(temp12)
gsub(pattern = "/", replacement = "", colnames(temp12))->colnames(temp12)
CreateTableOne(data = temp12, strata = "STATUS_D27"  )->T2


subset(temp8, temp8$variable == "Binary_Within_12H")->temp9
melt(temp9, id = c( "HADM_ID","STATUS_D27","Within_M5_AND_3H","Within_6H","Within_12H","variable", "value" ))->temp10
colnames(temp10)[7]<-"val"
temp10[c("HADM_ID","STATUS_D27", "value", "val")]->temp11
colnames(temp11)[3]<-"variable"
cast(temp11, HADM_ID + STATUS_D27  ~ variable , value = "val")->temp12
gsub(pattern = " ", replacement = "_", colnames(temp12))->colnames(temp12)
gsub(pattern = "[-,(\\)]", replacement = "", colnames(temp12))->colnames(temp12)
gsub(pattern = "/", replacement = "", colnames(temp12))->colnames(temp12)
CreateTableOne(data = temp12, strata = "STATUS_D27"  )->T3



invisible(capture.output(print(T1)->T1b))
invisible(capture.output(print(T2)->T2b))
invisible(capture.output(print(T3)->T3b))

write.csv(T1b, file = "./temp1.csv")
write.csv(T2b, file = "./temp2.csv")
write.csv(T3b, file = "./temp3.csv")

# Ces différents résultats sont mergés et mis dans le fichier appeler temp123c.csv




temp2[c("HADM_ID" , "STATUS_D27" , "TEMPS" )]->temp3
temp3$HEURE<-"HEURE"

# On considère les prises de sang réalisées après l'entrée en réanimation
subset(temp3, temp3$TEMPS >=0)->temp4
cast(temp4, HADM_ID + STATUS_D27 ~ HEURE, value = "TEMPS", fun.aggregate = min)->temp5
temp5$status<-1

fit <- survfit(Surv(HEURE, status) ~ STATUS_D27, data = temp5)


ggsurvplot(
  fit, 
  data = temp5, 
  size = 1,                 # change line size
  palette = 
    c("#E7B800", "#2E9FDF"),# custom color palettes
  #  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  legend.labs = 
    c("Alive", "Deceased"),    # Change legend labels
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  ggtheme = theme_bw()  ,     # Change ggplot2 theme
  title = "Intervalle de temps jusqu'à réalisation d'une prise de sang"
) ->g02






pdf(file = "./labeventsF3.pdf")
g02
dev.off()  




# Visualisation de la distribution de la lipase et de l'amylase. 
subset(temp2, temp2$LABEL %in% c( "Lipase",  "Amylase"))->temp3
temp3[c("HADM_ID" , "LABEL" , "VALUE" , "VALUENUM" , "FLAG" )]->temp4
temp4$status<-NA
temp4$status[temp4$FLAG == "abnormal"]<-"Abnormal"
temp4$status[temp4$FLAG != "abnormal"]<-"Normal"
temp4$value<-1
as.numeric(as.character(temp4$VALUE))->temp4$VALUE
g1<-ggplot(data = temp4) + aes (x = VALUE , fill = status) + geom_density(alpha=.5) +  xlim ( c(20,200)) + facet_grid( LABEL ~ . ) + 
  geom_tile("distribution des valeurs de la lipase et de l'amylase")


pdf(file = "./labeventsF4.pdf")
g1
dev.off()  

# Nombre de patients présentants des valeurs > 3N de lipase ou d'amylase. 


subset(temp2, temp2$LABEL %in% c( "Lipase",  "Amylase"))->temp3
temp3[c("HADM_ID" , "LABEL" , "VALUE" , "VALUENUM" , "FLAG" )]->temp4
temp4$status<-"N"
temp4$status[temp4$LABEL == "Lipase" & temp4$VALUE > 180]<-">3N"
temp4$status[temp4$LABEL == "Amylase" & temp4$VALUE > 300]<-">3N"
temp4$value<-1
cast(temp4, HADM_ID + LABEL ~ status, value = "value", fun.aggregate = length) ->temp5


temp5$Abnormal<-NA
temp5$Abnormal[temp5$`>3N` >=1]<-">3N"
temp5$Abnormal[temp5$`>3N` == 0]<-"N"
temp6$Amylase[is.na(temp6$Amylase)]<-"No measure"
cast(temp5, HADM_ID ~ LABEL ,value = "Abnormal")->temp6
as.character(temp6$Amylase)->temp6$Amylase
as.character(temp6$Lipase)->temp6$Lipase
temp6$Amylase[is.na(temp6$Amylase)]<-"No measure"
temp6$Lipase[is.na(temp6$Lipase)]<-"No measure"

temp6$Both<-"No"

temp6$Both[(temp6$Amylase == ">3N" | temp6$Lipase == ">3N")]<-"Yes"
melt(temp6, id = "HADM_ID")->temp7

g1<-ggplot(data = temp7, aes (x = LABEL , fill = value )) + geom_bar()
g2<-ggplot(data = temp6, aes (x = Both , fill = Both )) + geom_bar()
grid.arrange(g1, g2, ncol=2)->g04



pdf(file = "./labeventsF5.pdf")
g04
dev.off()  





# Date de la première lipase anormale comparativement au séjour d'entrée en réanimation. 

subset(temp2, temp2$LABEL %in% c( "Lipase",  "Amylase"))->temp3
temp3[c("HADM_ID" , "LABEL" , "VALUE" , "VALUENUM" , "TEMPS", "FLAG" )]->temp4
temp4$status<-"N"
temp4$status[temp4$LABEL == "Lipase" & temp4$VALUE > 180]<-">3N"
temp4$status[temp4$LABEL == "Amylase" & temp4$VALUE > 300]<-">3N"

subset(temp4, temp4$status == ">3N")->temp5
cast(temp5, HADM_ID ~ status , value = "TEMPS", min)->temp6
colnames(temp6)[2]<-"Temps"
ggplot(data = temp6, aes (x  = Temps)) + geom_density()->g06



pdf(file = "./labeventsF6.pdf")
g06
dev.off()  




# Une facon plus claire de visualiser est de comparer par rapport aux temps d'hospitalisations. 
merge(adms[c("HADM_ID"  , "ID"   , "Entre_hospit","Sortie_usi","Sortie_hospit" )] , temp6, by = "HADM_ID", all.x = TRUE)->temp7   

ggplot(data = temp7) + geom_segment(aes(x = Entre_hospit, xend = Sortie_hospit , y = ID , yend = ID)) +
  geom_segment(aes(x = 0, xend = Sortie_usi ,  y = ID , yend = ID ) , color = "yellow") + 
  geom_point(aes(x = Temps, y = ID ), color = "blue" )->g07

pdf(file = "./labeventsF7.pdf")
g07
dev.off() 


#ggplot(data = temp7, aes (x  = Temps)) + geom_density()



# Extraire : 1ère variable de chaque à la baseline , 24h et 48 heures. 

# Pour la baseline, on extrait la valeur la plus proche de l'entrée aux SI de -6 à +6 heures après. 
# On visualise quand sont réalisées les prises de sang. 

subset(temp2, temp2$TEMPS > -12 & temp2$TEMPS < 12  )->temp3
abs(temp3$TEMPS)->temp3$ABSOLUTE_TEMPS

colnames(temp3)
temp3[c("HADM_ID" , "LABEL" , "ABSOLUTE_TEMPS" )]->temp4
aggregate(ABSOLUTE_TEMPS ~ HADM_ID + LABEL, data = temp4, min)->temp5
temp5$Baseline<-"Yes"
merge(temp5, temp3, by = c("HADM_ID", "LABEL", "ABSOLUTE_TEMPS"), all = TRUE)->temp6
temp6$Baseline [temp6$Baseline != "Yes"]<-"No"

## Quelles sont les valeurs que nous possédons à la baseline. 
subset(temp6, temp6$Baseline == "Yes")->temp7

temp7[c("HADM_ID" ,"LABEL" )]->temp8
merge(temp8, adms["HADM_ID"], by = "HADM_ID", all.y = TRUE)->temp9

temp9$value<-1
cast(temp9, HADM_ID ~ LABEL, value= "value" , length) ->temp10
melt(temp10, id = c("HADM_ID", "LABEL"))->temp11

## Visualisation
merge(temp11, temp7, by = c("HADM_ID", "LABEL"), all.x = TRUE)->temp12
temp12$Missing<-"Missing"
temp12$Missing[!is.na(temp12$VALUE)]<-"Not missing"

ggplot(data = temp12, aes (x = LABEL , fill = Missing))  + geom_bar(position = "fill") + coord_polar()





# Extraire : 1ère variable de chaque à la baseline , 24h et 48 heures. 

# Pour la baseline, on extrait la valeur la plus proche de l'entrée aux SI de -6 à +6 heures après. 
# On visualise quand sont réalisées les prises de sang. 


temp2->temp3
temp3$DELTA2<-0
temp3$DELTA2[temp3$TEMPS> 22 & temp3$TEMPS <24]<-1
temp3$DELTA4<-0
temp3$DELTA4[temp3$TEMPS> 20 & temp3$TEMPS < 28]<-1
temp3$DELTA8<-0
temp3$DELTA8[temp3$TEMPS> 16 & temp3$TEMPS < 32]<-1
temp3$DELTA12<-0
temp3$DELTA12[temp3$TEMPS> 12 & temp3$TEMPS < 36]<-1
temp3$DELTA16<-0
temp3$DELTA16[temp3$TEMPS> 12 & temp3$TEMPS < 36]<-1


melt(temp3[c("HADM_ID" , "LABEL"  , "DELTA2" ,"DELTA4" , "DELTA8" ,  "DELTA12" , "DELTA16" )] , id = c("HADM_ID" , "LABEL") )-> temp4
cast(data = temp4, HADM_ID + LABEL ~ variable, value = "value", sum )->temp5 

temp5[c("HADM_ID", "LABEL")]->temp6
temp6$value<-1
cast(data = temp6[c("HADM_ID", "LABEL", "value")], HADM_ID ~ LABEL, value = "value")->temp7
melt(temp7, id = c("HADM_ID", "LABEL") )->temp8

merge(temp5, temp8[c("HADM_ID", "LABEL")], by = c("HADM_ID", "LABEL"), all.y = TRUE)->temp9


temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] [ temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] >0 ]<-"Not missing"
temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] [ temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] == 0 ]<-"Missing"
temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] [ is.na( temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] )]<-"Missing"

melt(temp9, id = c("HADM_ID", "LABEL"))->temp10

ggplot(data = temp10, aes (x = LABEL, fill = value)) + geom_bar() + coord_flip() + facet_grid( . ~ variable)





# A 48 heures. 



temp2->temp3
temp3$DELTA2<-0
temp3$DELTA2[temp3$TEMPS> 46 & temp3$TEMPS < 50]<-1
temp3$DELTA4<-0
temp3$DELTA4[temp3$TEMPS> 44 & temp3$TEMPS < 52]<-1
temp3$DELTA8<-0
temp3$DELTA8[temp3$TEMPS> 40 & temp3$TEMPS < 56]<-1
temp3$DELTA12<-0
temp3$DELTA12[temp3$TEMPS> 36 & temp3$TEMPS < 60]<-1
temp3$DELTA16<-0
temp3$DELTA16[temp3$TEMPS> 32 & temp3$TEMPS < 64]<-1


melt(temp3[c("HADM_ID" , "LABEL"  , "DELTA2" ,"DELTA4" , "DELTA8" ,  "DELTA12" , "DELTA16" )] , id = c("HADM_ID" , "LABEL") )-> temp4
cast(data = temp4, HADM_ID + LABEL ~ variable, value = "value", sum )->temp5 

temp5[c("HADM_ID", "LABEL")]->temp6
temp6$value<-1
cast(data = temp6[c("HADM_ID", "LABEL", "value")], HADM_ID ~ LABEL, value = "value")->temp7
melt(temp7, id = c("HADM_ID", "LABEL") )->temp8

merge(temp5, temp8[c("HADM_ID", "LABEL")], by = c("HADM_ID", "LABEL"), all.y = TRUE)->temp9


temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] [ temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] >0 ]<-"Not missing"
temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] [ temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] == 0 ]<-"Missing"
temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] [ is.na( temp9[!(colnames(temp9) %in% c("HADM_ID", "LABEL"))] )]<-"Missing"

melt(temp9, id = c("HADM_ID", "LABEL"))->temp10

ggplot(data = temp10, aes (x = LABEL, fill = value)) + geom_bar() + coord_flip() + facet_grid( . ~ variable)



# Baseline 
subset(temp2, temp2$TEMPS > -12 & temp2$TEMPS < 12  )->temp3
abs(temp3$TEMPS)->temp3$ABSOLUTE_TEMPS
colnames(temp3)
temp3[c("HADM_ID" , "LABEL" , "ABSOLUTE_TEMPS" )]->temp4
aggregate(ABSOLUTE_TEMPS ~ HADM_ID + LABEL, data = temp4, min)->temp5
temp5$Baseline<-"Yes"
merge(temp5, temp3, by = c("HADM_ID", "LABEL", "ABSOLUTE_TEMPS"), all = TRUE)->temp6
temp6$Baseline [temp6$Baseline != "Yes"]<-"No"
subset(temp6, temp6$Baseline == "Yes")->temp7
temp7[c("HADM_ID" ,"LABEL" , "VALUE" )]->temp8
merge(temp8, adms["HADM_ID"], by = "HADM_ID", all.y = TRUE)->temp9

meanNA <- function(x) {
  n <- mean(x, na.rm = TRUE)
  return(n)
}
as.numeric(as.character(temp9$VALUE))->temp9$VALUE
cast(temp9, HADM_ID ~ LABEL, value= "VALUE" , meanNA) ->temp10

temp10->baseline
melt(baseline, id = "HADM_ID")->baseline2
colnames(baseline2)[2]<-"Baseline"
baseline2->baseline
# 24 and 48H



temp2->temp3
temp3$H24<-0
temp3$H24[temp3$TEMPS> 12 & temp3$TEMPS <36]<-1
temp3$H48<-0
temp3$H48[temp3$TEMPS> 36 & temp3$TEMPS < 60]<-1

temp3[c("HADM_ID" , "LABEL" , "VALUE" , "H24" ,"H48" )]->temp4

# 24
subset(temp4, temp4$H24 > 0)->temp5
as.numeric(as.character(temp5$VALUE))->temp5$VALUE
cast(data = temp5, HADM_ID + LABEL ~ H24, value = "VALUE", meanNA )->temp6
colnames(temp6)[3]<-"H24"
temp6->hour24


# 48
subset(temp4, temp4$H48 > 0)->temp5
as.numeric(as.character(temp5$VALUE))->temp5$VALUE
cast(data = temp5, HADM_ID + LABEL ~ H48, value = "VALUE", meanNA )->temp6
colnames(temp6)[3]<-"H48"
temp6->hour48



merge(hour24, baseline, by = c("HADM_ID", "LABEL"), all = TRUE)->h1
merge(h1, hour48, by = c("HADM_ID", "LABEL"), all = TRUE)->h2

h2$Baseline[is.na(h2$Baseline)]<-NA
h2$H24[is.na(h2$H24)]<-NA
h2$H48[is.na(h2$H48)]<-NA


write.csv(file = "./tables_analyses/lab_3_times.csv", x = h2, row.names = FALSE)




# Création de catégorie de temps pour visualiser les paramètres de la baseline à chacune des heures. 

temp1$H0<-0
temp1$H1<-0
temp1$H2<-0
temp1$H3<-0
temp1$H4<-0
temp1$H6<-0
temp1$H8<-0
temp1$H10<-0
temp1$H12<-0
temp1$H16<-0
temp1$H20<-0
temp1$H24<-0

temp1$H0[temp1$TEMPS <0 ]<-1
temp1$H1[temp1$TEMPS >=0 & temp1$TEMPS <1 ]<-1
temp1$H2[temp1$TEMPS >=0 & temp1$TEMPS <2 ]<-1
temp1$H3[temp1$TEMPS >=0 & temp1$TEMPS <3 ]<-1
temp1$H4[temp1$TEMPS >=0 & temp1$TEMPS <4 ]<-1
temp1$H6[temp1$TEMPS >=0 & temp1$TEMPS <6 ]<-1
temp1$H8[temp1$TEMPS >=0 & temp1$TEMPS <8 ]<-1
temp1$H10[temp1$TEMPS >=0 & temp1$TEMPS <10 ]<-1
temp1$H12[temp1$TEMPS >=0 & temp1$TEMPS <12 ]<-1
temp1$H16[temp1$TEMPS >=0 & temp1$TEMPS <16 ]<-1
temp1$H20[temp1$TEMPS >=0 & temp1$TEMPS <20 ]<-1
temp1$H24[temp1$TEMPS >=0 & temp1$TEMPS <24 ]<-1



temp1[c("HADM_ID","LABEL","H1","H2","H3","H4","H6","H8","H10","H12","H16","H20","H24","H0")]->temp2

melt(temp2, id = c("HADM_ID","LABEL"))->temp3





# Création d'un fichier avec les paramètres d'interêt (valeurs à la baseline <3 heures. )
subset(temp1, temp1$TEMPS <3)->temp2
temp2[c("HADM_ID" , "LABEL" , "VALUE"  )]->temp3
as.numeric(as.character(temp3$VALUE))->temp3$VALUE
cast(temp3, HADM_ID ~ LABEL, mean)->temp4
write.csv(x = temp4 , file = "./tables_analyses/lab_baseline.csv", row.names = FALSE)




# Analyse des paramètres dont nous disposons à la baseline 

for (m in c(3, 5, 10))  { 
  print(m)
  m<-10
  subset(temp1, temp1$TEMPS <m)->temp2
  temp2[c("HADM_ID" , "LABEL" , "VALUE"  )]->temp3
  as.numeric(as.character(temp3$VALUE))->temp3$VALUE
  cast(temp3, HADM_ID ~ LABEL, length)->temp4
  temp4[,-1][temp4[,-1] != 0]<-"Oui"
  temp4[,-1][temp4[,-1] == 0]<-"Non"
  
  # temp4[,-1][temp4[,-1] >= 3]<-">=3"
  melt(temp4, id = c(HADM_ID))->temp5
  
  subset(as.data.frame(table(temp5[c("value", "LABEL")])), as.data.frame(table(temp5[c("value", "LABEL")]))$value == "Oui")->p
  p[order(-p$Freq),]->p1
  
  as.factor(temp5$LABEL)->temp5$LABEL
  factor(temp5$LABEL, levels = p1$LABEL)->temp5$LABEL
  
  
  
  png(file = paste("./figures/repartition_acqui/", "Avant " , m, " heures", ".png"), height = 10, width = 10, units = "in", res = 150)
  ggplot(data = temp5) + geom_bar(aes(x = LABEL  , fill = value, stat = "count")) + coord_polar()
  dev.off() 
}


# Visualisation des parametres


for (i in unique(temp1$LABEL)) {
  
  unique(temp1$LABEL)[1] ->i
  subset(temp1, temp1$LABEL == i)->po1
  
  
  po1[c("HADM_ID"  ,"LABEL",  "TEMPS","VALUE","VALUENUM" )]->po2
  po2->po1
  
  ggplot(data = po1, aes (x = TEMPS, y = HADM_ID)) + geom_point() ->g0
  
  
  po1->temp4
  temp4$CAT_TEMPS<-NA
  temp4$CAT_TEMPS[temp4$TEMPS < 3]<-"baseline"
  temp4$CAT_TEMPS[temp4$TEMPS < 28 & temp4$TEMPS >= 20]<-"24 hours"
  temp4$CAT_TEMPS[temp4$TEMPS < 52 & temp4$TEMPS >= 44]<-"48 hours"
  
  subset(temp4, temp4$CAT_TEMPS != "NA")->temp5
  
  
  
  # Valeurs moyennes par patient
  as.numeric(as.character(temp5$VALUE))->temp5$VALUE
  cast(temp5,  HADM_ID + CAT_TEMPS ~ LABEL , value = "VALUE", mean)->variableM
  melt(variableM, id = c("HADM_ID" ,"CAT_TEMPS"))->temp7  
  as.character(temp7$VARIABLE)->temp7$VARIABLE
  as.factor(temp7$CAT_TEMPS)->temp7$CAT_TEMPS
  temp7$CAT_TEMPS<- factor(temp7$CAT_TEMPS,levels(temp7$CAT_TEMPS)[c(3,1,2)])
  # compute lower and upper whiskers
  # ylim1 = boxplot.stats(temp7$value)$stats[c(1, 5)]
  # ggplot( data = temp7 , aes(y = value, x = CAT_TEMPS)) +  geom_boxplot() + coord_cartesian(ylim = ylim1*1.05) ->g6
  ggplot( data = temp7 , aes(y = value, x = CAT_TEMPS)) +  geom_boxplot() ->g6
  
  
  ggplot( data = temp7 , aes(x = value, fill = CAT_TEMPS)) +  geom_density() +  facet_grid( CAT_TEMPS  ~ .) + theme(legend.position = "none") ->g10
  
  
  #Nombre de valeurs par patients 
  
  cast(temp5,  HADM_ID + CAT_TEMPS ~ LABEL , value = "VALUE", length)->variableC
  cbind(unique(po1$HADM_ID), "Yes")->u 
  as.data.frame(u)->u1
  merge(variableC, u1, by.x = c("HADM_ID"), by.y = c("V1") , all.y = TRUE)->temp8
  
  temp8->variableC
  melt(variableC, id = c("HADM_ID" ,"CAT_TEMPS"))->temp7  
  temp7$value [temp7$value >= 3]<-">3"
  
  
  
  
  
  as.factor(temp7$value)->temp7$value
  temp7$value = factor(temp7$value,levels(temp7$value)[c(2:4,1)])
  as.factor(temp7$CAT_TEMPS)->temp7$CAT_TEMPS
  temp7$CAT_TEMPS = factor(temp7$CAT_TEMPS,levels(temp7$CAT_TEMPS)[c(3,1,2)])
  ggplot(temp7, aes (x = value)) +  geom_bar(position = "dodge") +  facet_grid( CAT_TEMPS  ~ .)->g5
  
  
  
  grid.arrange(g6, g5, g10 , ncol=3 )->g8
  grid.arrange(g0, g8, nrow=2 , top = i )->g9
  
  
  png(file = paste("./figures/repartition_acqui/", i, ".png"), height = 10, width = 10, units = "in", res = 150)
  plot(g9)
  dev.off() 
  
  
  
}



