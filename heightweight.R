
# ce fichier nécéssite une liste admid d'indentifiant d'interêt

# Exctraction des données à partir du fichier sofa créés par le script d'Alistair Johnson

con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")




paste(admid,collapse = ",")->ids

# Dans un premier temps, extraire les icustays_id
sql <- paste("select * from mimic3.icustays where  hadm_id in (", ids, ") ", sep=" " )
data1 <- dbGetQuery(con, sql)



paste(data1$icustay_id,collapse = ",")->ids2
sql <- paste("select * from mimic3.heightweight where  icustay_id in (", ids2, ") ", sep=" " )
data <- dbGetQuery(con, sql)


merge(data1 [c("hadm_id","icustay_id")], data, by = "icustay_id")->b

write.csv(x = b , file = "./resultats/heightweight.csv", row.names = FALSE)

print("##################################")
print(" Le fichier heightweight.csv vient d'être créé")
print("##################################")