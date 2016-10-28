# setwd("~/Google Drive/Data at TWI/Pursuits/Robo advice")
library(RPostgreSQL)
library(data.table)
library(reshape)

#list of edges in mst
drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="roboadvisordb", host= "localhost", port=5432, 
                user="robouser", password="password")

q <- "select * from news_group"
news <- data.table(dbGetQuery(db, q))

q <- "select * from minimum_spanning_tree_model"
mstEdges <- data.table(dbGetQuery(db, q))

q <- "select * from assetdata";
stockInfo <- data.table(dbGetQuery(db, q))

dbDisconnect(db)
dbUnloadDriver(drv)

stockPrice <- stockInfo[,list(timestamp,asset_id,price)]
stockPrice <- cast(stockPrice, timestamp ~ asset_id)
stockPrice$timestamp <- as.Date(stockPrice$timestamp)
colnames(stockPrice)[1] <- 'Date'
colnames(stockPrice)[2:ncol(stockPrice)] <- lapply(colnames(stockPrice)[2:ncol(stockPrice)], function(x) {return(paste("stock",as.character(x),sep = ""))})

news$assetId_id <- paste("stock",news$asset_id,sep = "")
mstEdges$assetIdOne_id <- paste("stock",mstEdges$assetIdOne_id,sep = "")
mstEdges$assetIdTwo <- paste("stock",mstEdges$assetIdTwo,sep = "")



#mstEdges <- read.csv("mstEdges.csv")
#mstEdges$X <- NULL
#closePriceLogDiff <- read.csv("closePriceLogDiff.csv")
#closePriceLogDiff$X<- NULL
numStocks <- ncol(stockPrice)-1
#latestLogReturns <- closePriceLogDiff[1,1:40]
#latestLogReturns <- c(latestLogReturns[1,])

newsEffect <- function(stockName,effect=0.005) {
  #create list of 40 zeroes to depict effect due to news for each of 40 stocks
  currentNewsEffect <- as.list(stockPrice[1,2:(numStocks+1)])
  currentNewsEffect[1:numStocks] <- 0
  sourceList <- c(stockName)
  currentNewsEffect[stockName] <- effect
  while (length(sourceList)!=0) {
    sourceNode <- sourceList[1]
    print(sourceNode)
    destList <- as.list(as.character(mstEdges[mstEdges$assetIdOne_id==sourceNode,]$assetIdTwo))
    while(length(destList)!=0) {
      dest <- destList[[1]]
      print(dest)
      sourceList <- c(sourceList, dest)
      #model1 <- lm(as.formula(paste(dest, "~",sourceNode)),data=closePriceLogDiff)
      #new <- data.frame(currentNewsEffect[sourceNode])
      #colnames(new) <- c(sourceNode)
      #result <- predict(model1,newdata = new)
      #currentNewsEffect[dest] <- result
      print(paste("source",sourceNode,sep = " "))
      print(paste("dest",dest,sep = " "))
      currentNewsEffect[dest] <- (currentNewsEffect[[sourceNode]] * 
                          mstEdges[assetIdOne_id==sourceNode & assetIdTwo==dest,]$slope) + 
                          mstEdges[assetIdOne_id==sourceNode & assetIdTwo==dest,]$intercept
      destList <- destList[-1]
    }
    sourceList <- sourceList[-1]
  }
  return(currentNewsEffect)
}


# loop over all assets to predict
netEffect <- as.list(stockPrice[1,2:(numStocks+1)])
netEffect[1:numStocks] <- 0
netEffect <- data.frame(netEffect)
for (stock in news$assetId_id) {
  temp <- newsEffect(stock,news[assetId_id==stock,]$effect)
  for (colname in colnames(netEffect)) {
    netEffect[1,colname] <- netEffect[1,colname] + temp[[colname]]
  }
}

# for (stock in colnames(netEffect)) {
#   altName <- substr(stock,6,nchar(stock))
#   temp <- newsEffect(stock,news[assetId_id==assetId_id,]$effect)
#   for (colname in colnames(netEffect)) {
#     netEffect[1,colname] <- netEffect[1,colname] + temp[[colname]]
#   }
# }

colnames(netEffect) <- substr(colnames(netEffect),6,nchar(colnames(netEffect)))

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="roboadvisordb", host= "localhost", port=5432, 
                user="robouser", password="password")

for (stock_id in colnames(netEffect)) {
  q = paste("update assetdata set neteffect =",netEffect[1,stock_id]," where asset_id = ",stock_id," AND timestamp = \'",max(stockInfo$timestamp-(19800)),"\'",sep = "")
  print(dbGetQuery(db, q))
}

dbDisconnect(db)
dbUnloadDriver(drv)

# c <- data.frame(newsEffect("ADX",-0.05))
# 
# ## Convert result to price prediction
# pricePrediction <- c
# for (stockName in colnames(c)) {
#   ## new closing price according to prediction
#   pricePrediction[1,stockName] <- 10^(log10(closePrice[1,stockName]) + c[1,stockName])
#   ## percentage change in price
#   pricePrediction[2,stockName] <- 100*(pricePrediction[1,stockName] - 
#                                      closePrice[1,stockName])/closePrice[1,stockName]
# }
rm(list=ls())

