library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="postgres", host= "localhost", port=5432,  user="postgres")

q = "update assetdata set neteffect = 0.4 where asset_id = 1;"
  print(dbGetQuery(db, q))

dbDisconnect(db)
dbUnloadDriver(drv)
