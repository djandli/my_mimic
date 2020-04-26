# Extraction des données de NOTEEVENTS



#  Extraction des données concernant les patients une pancréatite aigue


sql <- paste("SELECT * FROM NOTEEVENTS WHERE HADM_ID IN  (", ids, ")", sep=" " )
res <- dbSendQuery(con2, sql)
NOTEEVENTS <-dbFetch(res,n=-1)


subset(NOTEEVENTS, NOTEEVENTS$HADM_ID == 190809)->t


