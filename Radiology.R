
# This script download imaging of interest and put it in an excel file
# Requires an "admid" vectir with hospitalisation of interest

dbDisconnect(con)
con <- dbConnect(drv, host= "localhost", dbname= "postgres", user= "postgres", password = "Arsenal001!")
sql <- paste("select * from mimic3.noteevents where  hadm_id in (", ids, ")", sep=" " )
start_time <- Sys.time()
NOTEEVENTS <- dbGetQuery(con, sql)
end_time <- Sys.time()
end_time - start_time


subset(NOTEEVENTS, NOTEEVENTS$category == "Radiology")->radiology


sql <- paste("select * from mimic3.icustays where  hadm_id in (", ids, ")",  sep=" " )
icustays <- dbGetQuery(con, sql)


merge(radiology, icustays[c("hadm_id", "intime")], by = "hadm_id", all = TRUE)->temp1
temp1$charttime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$charttime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


temp1$intime->times
dtparts = t(as.data.frame(strsplit(as.character(times),' ')))
row.names(dtparts) = NULL
temp1$intime= chron(dates=dtparts[,1],times=dtparts[,2], format=c('y-m-d','h:m:s'))


difftime(  temp1$charttime ,temp1$intime, unit="hours") ->temp1$TEMPS
#temp1$TABLE<-"LABEVENTS"
temp1->save0

write.xlsx(file = "./radiology.xlsx", x = save0)




