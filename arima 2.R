library(RPostgreSQL)
library(data.table)
library(reshape)
library(jsonlite)

## Loading data and taking relevant subset (of stocks having all data points without NAs)
# allData <- fread("nyseClosingPrice.csv")
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="postgres", host= "localhost", port=5432,  user="postgres")
#read data from db
q <- "select * from asset"
assets <- data.table(dbGetQuery(db, q))
q <- "select * from assetdata";
stockInfo <- data.table(dbGetQuery(db, q))

#disconnect from db
dbDisconnect(db)
dbUnloadDriver(drv)

#take reqd columns and pivot asset price data and change column names
allData <- unique(stockInfo[,list(timestamp,asset_id,price)])
allData <- cast(allData, timestamp ~ asset_id)
allData$timestamp <- as.Date(allData$timestamp)
colnames(allData)[1] <- 'Date'
colnames(allData)[2:ncol(allData)] <- lapply(colnames(allData)[2:ncol(allData)], function(x) {return(paste("stock",as.character(x),sep = ""))})
allData <- data.table(allData)

# allData$Date <- as.Date(allData$Date, "%m/%d/%y")
allData[is.na(allData)] <- 1
allData <- set(allData, j = which(colSums(is.na(allData)) > 0), value = NULL)
allData <- allData[with(allData, order(Date)), ]

## Finding normalized log return of all data points
stockPrices <- allData[, 2:ncol(allData), with = FALSE]
logStockPrices <- log10(stockPrices)
logReturn <- data.table(diff(as.matrix(logStockPrices)))
logReturn <- logReturn[, Date:= allData[2:nrow(allData), Date]]
logReturn <- logReturn[, c(ncol(logReturn),1:(ncol(logReturn)-1)), with = FALSE]
# save(logReturn,file="logReturn.rda")

## function to create arima model
getArimaModel <- function(data,order){
  #print(head(data))
  fit <- arima(data,order = c(order,0L,0L),method = "ML")
}

## create models for each stock
arimaModelDF <- apply(logReturn[,-1,with=FALSE],MARGIN = 2,FUN =  function(x){return(getArimaModel(x,5))})
arimaModelDF$Date <- NULL
# save(arimaModelDF,file = "arimaModels.rda")

arimaModelToDB <- data.frame(stock_id=names(arimaModelDF),coeff1=0,coeff2=0,coeff3=0,coeff4=0,coeff5=0,intercept=0)
for (stock in names(arimaModelDF)) {
  arimaModelToDB[arimaModelToDB$stock_id==stock,2:7] <- arimaModelDF[[stock]]$coef
}
arimaModelToDB$stock_id <- apply(arimaModelToDB, MARGIN =1, FUN = function(x) {
  substr(x["stock_id"],6,nchar(x["stock_id"]))
})

# ## save arima model as data frame
# drv <- dbDriver("PostgreSQL")
# db <- dbConnect(drv, dbname="roboadvisordb", host= "localhost", port=5432, 
#                 user="robouser", password="password")
# 
# dbWriteTable(db,"arima_model",arimaModelToDB,overwrite=TRUE,row.names=FALSE)
# 
# dbDisconnect(db)
# dbUnloadDriver(drv)

## save arima model as json
arimaJSONdf <- data.frame(apply(arimaModelToDB, MARGIN = 1, FUN = function(x) {
  toJSON(x[2:length(x)])
}))
arimaJSONdf$asset_id <- arimaModelToDB$stock_id
colnames(arimaJSONdf)[1] <- c("coefficients")

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="postgres", host= "localhost", port=5432,  user="postgres")
# arimaJSON <- toJSON(arimaModelToDB)

dbWriteTable(db,"time_series_model",arimaJSONdf,overwrite=TRUE,row.names=FALSE)

dbDisconnect(db)
dbUnloadDriver(drv)



# ## get prediction using model
# getTimeSeriesPrediction <- function(stockName,newdata) {
#   refit <- Arima(newdata, model = arimaModelDF[[stockName]])
#   prediction <- predict(refit,n.ahead=1)
#   return(prediction$pred[1])
# }

# arimaModelDF <- lapply(logReturn[1:701], function(x){return(getArimaModel(x,5))})
# arimaPredictions <- logReturn[1]
# arimaPredictions$Date <- NULL
# for (stockName in colnames(logReturn)[2:ncol(logReturn)]) {
#   refit <- Arima(logReturn[[stockName]][702:757],model = arimaModelDF[[stockName]])
#   prediction <- predict(refit,n.ahead=1)
#   print(prediction$pred[1])
# }

################################
## find right order
# MEList <- c()
# for (i in 10:12) {
#   print(i)
#   arimaModelDF <- lapply(logReturn[1:701], function(x){return(getArimaModel(x,i))})
#   arimaModelDF$Date <- NULL
#   totalME <- 0
#   for (stockName in colnames(logReturn)[2:ncol(logReturn)]) {
#     refit <- Arima(logReturn[[stockName]][702:757],model = arimaModelDF[[stockName]])
#     totalME <- totalME + accuracy(refit)[1]
#   }
#   MEList[i] <- totalME
# }
rm(list=ls())
