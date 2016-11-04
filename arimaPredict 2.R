library(RPostgreSQL)
library(data.table)
library(reshape)
library(jsonlite)

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="postgres", host= "localhost", port=5432,  user="postgres")

#read data from db
q <- "select * from assetdata";
stockInfo <- data.table(dbGetQuery(db, q))
q <- "select * from time_series_model";
arimaJSONdf <- data.table(dbGetQuery(db, q))
q <- "select * from news_group"
newsEffect <- data.table(dbGetQuery(db, q))
rm(q)

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
newsEffect$asset_id <- paste("stock",newsEffect$asset_id,sep = "")

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

## prediction by manual calculation
logReturnTrunk <- data.frame(tail(logReturn,5))
logReturnTrunk$Date <- as.character(logReturnTrunk$Date)
logReturnTrunk <- logReturnTrunk[with(logReturnTrunk, order(Date,decreasing = T)), ]
logReturnTrunk <- logReturnTrunk[,2:ncol(logReturn)]
logReturnTrunk <- rbind(logReturnTrunk,rep(1,ncol(logReturnTrunk)))

coefficients <- data.frame(apply(arimaJSONdf,MARGIN = 1,FUN = function(x){as.numeric(fromJSON(x[1]))}))
colnames(coefficients) <- paste("stock",arimaJSONdf$asset_id,sep = "")

## calculate timeseries prediction
predictionDF <- logReturnTrunk[1,] * 0
for (stock_id in colnames(predictionDF)) {
  predictionDF[1,stock_id] <- (sum(logReturnTrunk[stock_id] * coefficients[stock_id]))
}

## store timeseries prediction in seperate variable for waterfall visualization
arimaPredictionDF <- predictionDF

## add news effect
for (stock_id in colnames(predictionDF)) {
  stock_id_temp <- substr(stock_id,6,nchar(stock_id))
  predictionDF[1,stock_id] <- predictionDF[1,stock_id] + (0.01*(stockInfo[asset_id==stock_id_temp & timestamp==max(timestamp),]$neteffect))
}

## calculate price from log return
todayPrice <- allData[nrow(allData),]
tomorrowPrice <- todayPrice
tomorrowPrice$Date <- tomorrowPrice$Date + 1
for (asset_id in colnames(predictionDF)) {
  tomorrowPrice[1,asset_id] <- 10^(predictionDF[1,asset_id] + log10(todayPrice[1,asset_id,with=F]))
}
colnames(tomorrowPrice) <- substr(colnames(tomorrowPrice),6,nchar(colnames(tomorrowPrice)))
colnames(arimaPredictionDF) <- substr(colnames(arimaPredictionDF),6,nchar(colnames(arimaPredictionDF)))

## write prediction to db
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="postgres", host= "localhost", port=5432,  user="postgres")

for (stock_id in colnames(tomorrowPrice[,2:ncol(tomorrowPrice),with=F])) {
  q = paste("update assetdata set prediction =",tomorrowPrice[1,stock_id,with=F]," where asset_id = ",stock_id," AND timestamp = \'",max(stockInfo$timestamp)-19800,"\'",sep = "")
  print(dbGetQuery(db, q))
  
  q = paste("update assetdata set arimaeffect =",arimaPredictionDF[1,stock_id]," where asset_id = ",stock_id," AND timestamp = \'",max(stockInfo$timestamp)-19800,"\'",sep = "")
  print(dbGetQuery(db, q))
}

#disconnect from db
dbDisconnect(db)
dbUnloadDriver(drv)

rm(list=ls())
