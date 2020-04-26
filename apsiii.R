
# ce fichier nécéssite une liste admid d'indentifiant d'interêt

# Exctraction des données à partir du fichier sofa créés par le script d'Alistair Johnson

con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")


paste(admid,collapse = ",")->ids
sql <- paste("select * from mimic3.apsiii where  hadm_id in (", ids, ") ", sep=" " )
data <- dbGetQuery(con, sql)



write.csv(x = data , file = "./resultats/apsiii.csv", row.names = FALSE)

print("##################################")
print(" Le fichier apsiii.csv.csv vient d'être créé")
print("##################################")

dbDisconnect(con)