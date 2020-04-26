
# Cette fonction sert à ajouter le décès à J28 comme covariable; 

library(icd)

# Chargement du fichier DIAGNOSES_ICD
paste(admid,collapse = ",")->ids
sql <- paste("SELECT * FROM DIAGNOSES_ICD WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
DIAGNOSES_ICD<-dbFetch(res,n=-1)

patient_data<-DIAGNOSES_ICD[c("HADM_ID", "ICD9_CODE")]


comorbid_charlson(patient_data)->comorbidities
as.data.frame((comorbidities))->c2
rownames(c2)->c2$HADM_ID



write.csv(file = "./resultats/comorbidities.csv",  x = c2, row.names = FALSE)

print("##################################")
print(" Un fichier intitulé comorbidities.csv vient d'être créé dans l'environnement de travail")
print("##################################")


