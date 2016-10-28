library(igraph)
# library(rgl)
library(data.table)
library(RPostgreSQL)
library(reshape)
# setwd("~/Google Drive/Data at TWI/Pursuits/Robo advice")

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="roboadvisordb", host= "localhost", port=5432, 
                user="robouser", password="password")

#read data from db
q <- "select * from asset"
assets <- data.table(dbGetQuery(db, q))
q <- "select * from assetdata";
stockInfo <- data.table(dbGetQuery(db, q))

#disconnect from db
dbDisconnect(db)
dbUnloadDriver(drv)

#take reqd columns and pivot asset price data and change column names
stockPrice <- stockInfo[,list(timestamp,asset_id,price)]
stockPrice <- cast(stockPrice, timestamp ~ asset_id)
stockPrice$timestamp <- as.Date(stockPrice$timestamp)
colnames(stockPrice)[1] <- 'Date'
colnames(stockPrice)[2:ncol(stockPrice)] <- lapply(colnames(stockPrice)[2:ncol(stockPrice)], function(x) {return(paste("stock",as.character(x),sep = ""))})


takeDifferenceToggle <- TRUE
allData <- stockPrice #from integration.R file

##remove na columns
allData[is.na(allData)] <- 1
allData <- set(allData, j = which(colSums(is.na(allData)) > 0), value = NULL)
allData <- allData[with(allData, order(Date)), ]

# createMST <- function(allData,takeDifferenceToggle) {
  ## remove stocks with NA values
  rm(closePrice)
  closePrice <- set(allData, j = which(colSums(is.na(allData)) > 0), value = NULL)
  stockNum <- ncol(closePrice)-1
  rowNum <- nrow(closePrice)
   
  ## create log return
  rm(closePriceLog,closePriceLogDiff)
  closePriceLog <- closePrice[2:(stockNum+1)]
  closePriceLog <- log10(closePriceLog)
  closePriceLog$Date <- as.Date(closePrice$Date, "%m/%d/%y")
  if(takeDifferenceToggle) {
    closePriceLogDiff <- data.frame(closePriceLog[2:rowNum,]) - data.frame(closePriceLog[1:rowNum-1,])
    closePriceLogDiff$Date <- closePriceLog[1:rowNum-1,]$Date
  }else {
    closePriceLogDiff <- closePriceLog
  }
  colnames(closePriceLogDiff) <- colnames(closePriceLog)
  
  ## create correlation matrix, and directed graph using correlation and causality
  rm(corMatrix,edges,weights,negToPosLagRatioMatrix)
  corMatrix <- cor(data.frame(closePriceLogDiff[1:stockNum]))
  edges <- c()
  weights <- c()
  negToPosLagRatioMatrix <- matrix(0, nrow = stockNum, ncol = stockNum)
  colnames(negToPosLagRatioMatrix) <- colnames(data.frame(closePriceLogDiff[1:stockNum]))
  rownames(negToPosLagRatioMatrix) <- colnames(data.frame(closePriceLogDiff[1:stockNum]))
  for (i in 1:stockNum) {
    for (j in 0+i:stockNum) {
      if (i!=j) {
        l = 100
        lagCorrel = ccf(closePriceLogDiff[i],closePriceLogDiff[j],l)
        sumSquaresNegtv = 0
        sumSquaresPostv = 0
        for (k in 1:l) {
          temp <- lagCorrel$acf
          sumSquaresPostv <- sumSquaresPostv + (temp[k+l+1] * temp[k+l+1])
          sumSquaresNegtv <- sumSquaresNegtv + (temp[k] * temp[k])
        }
        sumSquaresPostv <- sqrt(sumSquaresPostv)
        sumSquaresNegtv <- sqrt(sumSquaresNegtv)
        negToPosLagRatio <- (sumSquaresPostv - sumSquaresNegtv)*100/sumSquaresPostv
        negToPosLagRatioMatrix[i,j] <- negToPosLagRatio
        if (negToPosLagRatio < -4) {
          edges <- c(edges,i,j)
          weights <- c(weights,1-(corMatrix[i,j] * corMatrix[i,j]))
        }
        else if(negToPosLagRatio > 4){
          edges <- c(edges,j,i)
          weights <- c(weights,1-(corMatrix[i,j] * corMatrix[i,j]))
        }
      }
    }
  }
  
  ## create graph and mst
  g<-graph(edges, n=max(edges), directed = TRUE)
  g<-set.vertex.attribute(g, "name", value=colnames(closePriceLog)[1:stockNum])
  gmst <- mst(g,weights = weights)
  # plot(gmst,vertex.size = 8,edge.arrow.size=0.5)
  
  ## prepare to export results to db
  mstEdges <- data.frame(get.edgelist(gmst))
  colnames(mstEdges) <- c("assetIdOne_id","assetIdTwo")
  mstEdges$assetIdOne_id <- as.character(mstEdges$assetIdOne_id)
  mstEdges$assetIdTwo <- as.character(mstEdges$assetIdTwo)
  mstEdges$intercept <- 0
  mstEdges$slope <- 0
  coefficients <- apply(mstEdges,1, function(x){
    model1 <- lm(as.formula(paste(x[[2]], "~",x[[1]])),data=closePriceLogDiff)
    return(model1$coefficients)
    })
  
  for (i in 1:nrow(mstEdges)) {
    mstEdges[i,1] <- substr(mstEdges[i,1],6,nchar(mstEdges[i,1]))
    mstEdges[i,2] <- substr(mstEdges[i,2],6,nchar(mstEdges[i,2]))
    mstEdges[i,3] <- coefficients[1,i]
    mstEdges[i,4] <- coefficients[2,i]
  }
  
  # write.csv(mstEdges,file = "mstEdges.csv")
  # write.csv(closePriceLogDiff,file = "closePriceLogDiff.csv")
  # write.csv(corMatrix,file = "correlationMatrix.csv")
  
  ## save results to db
  drv <- dbDriver("PostgreSQL")
  db <- dbConnect(drv, dbname="roboadvisordb", host= "localhost", port=5432, 
                  user="robouser", password="password")
  
  dbWriteTable(db,"minimum_spanning_tree_model",mstEdges,overwrite=TRUE,row.names=FALSE)
  
  dbDisconnect(db)
  dbUnloadDriver(drv)
  
# }
rm(list=ls())
  