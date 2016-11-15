library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="postgres", host= "localhost", port=5432,  user="postgres")

q = paste("update assetdata set neteffect = 0.5 where asset_id ="," 1;",sep="")
  print(dbGetQuery(db, q))

dbDisconnect(db)
dbUnloadDriver(drv)
