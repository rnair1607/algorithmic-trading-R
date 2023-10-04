library(quantmod)
library(gramEvol)
library(GA)
library(TTR)
library(neuralnet)

e1 <- new.env(parent = baseenv())

#total window
fromDt <- '2019-01-01'
toDt <- '2021-03-02'

#number of days for which we fill forecast values
numOfPredictedDays <- 60


someStocks <- c("NFLX")

mystckdata <- get(getSymbols(someStocks, src="yahoo", from=fromDt, to=toDt, freq="daily"))
tail(mystckdata)
split_date <- format(as.Date(toDt)-as.difftime(numOfPredictedDays,unit='days'))
chartSeries(mystckdata)
#The data which we will use as training and testing dataset
stckdata <- mystckdata[ index(mystckdata) <= split_date ]

#Actual data for the period in which we will try to forecast
forecast_data_actual <- mystckdata[ index(mystckdata) > split_date ]
#There are 39 trading days in a period of 60 days
nrow(forecast_data_actual)

#Size of Window in our sliding window approach
windowSize <- 10
testTrainSplit <- .20

head(stckdata)
openPrice <- stckdata$NFLX.Open
closePrice <- stckdata$NFLX.Close


forecastingFunc <- function(data){
#Generate columns with lag for Closing price
#priceLag <- Lag(closePrice,seq(from=windowSize,to=1))
  priceLag <- Lag(data,seq(from=windowSize,to=1))

#Generate column names for each of those columns
cols <- paste0("x", 1:windowSize) #x1,x2
cols <- rev(cols) #x10,x9

#Rename columns
names(priceLag) <- cols

#Add another column which is data without lag
priceLag$x <- data #closePrice
priceLagDf <- data.frame(priceLag[(windowSize+1):nrow(priceLag),])
#data.frame(cpLag[(windowSize+1):nrow(cpLag),])

tail(priceLagDf)
trainData <- head(priceLagDf, floor(nrow(priceLagDf)*(1-testTrainSplit)))
testData <- tail(priceLagDf, ceiling(nrow(priceLagDf)*testTrainSplit))
trainData
#seq(-10,10,0.1)
#runif(20 ,0, 10)
rules <- list(expr = grule(op(expr, expr), func(expr), var,rd),
              func = grule(sin, cos, exp, log, sqrt,min,max,tan),
              op = grule('+', '-', '*', '/', '^'),
              var = grule(pricedata$x10, pricedata$x9, pricedata$x8, pricedata$x7, pricedata$x6, pricedata$x5, pricedata$x4, pricedata$x3, 
                          pricedata$x2, pricedata$x1),
              rd = gvrule(seq(-10,10,0.2))
)

ruleGrammar <- CreateGrammar(rules)

rmseFitnessFunc <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)) || any(is.na(result)) || (expr=="pricedata$x1") )
    return(Inf)
  return (sqrt(mean((pricedata$x - result)^2)))
}

pricedata <- trainData
pricedata
ge <- GrammaticalEvolution(ruleGrammar, rmseFitnessFunc,
                           terminationCost = 1, max.depth = 5,
                           optimizer = "ga",
                           iterations=500)
print(ge)
ge$best$expressions
predictions <- eval(ge$best$expressions)
predictions
pricedata <- testData
predictions <- eval(ge$best$expressions)
predictions

#Set last record of test data as input to predict next day's price
pricedata <- pricedata[nrow(pricedata),]
pricedata
future_date <- c()

##predict prices during trading days among next 60 days
for(i in 1:nrow(forecast_data_actual)){
  future_date[i] <- eval(ge$best$expressions)
  for (j in (windowSize:2)){
    pricedata[j]<-pricedata[j-1]
  }
  pricedata[1] <- future_date[i]
}
return (future_date)
}
future_Cp <- forecastingFunc(closePrice)
future_Op <- forecastingFunc(openPrice)
future_data <-  data.frame(future_Op,future_Cp)
future_data <-  xts(future_data, order.by =index(forecast_data_actual))
future_data
chartSeries(future_data)
forecast_data_actual[, c("NFLX.Open", "NFLX.Close")]

#Combine stockdata used for train-test with predicted values so that
#we have enough data to use for generating trading indicators
combinedData <-  rbind(stckdata[,c(1,4)],future_data) 
tail(combinedData)

#Use SMA to develop trading signals on forecasted data
low=20
high=50
SMALow <-SMA(combinedData$NFLX.Close,n=low)
SMAHigh <-SMA(combinedData$NFLX.Close,n=high)

SMAonForecasted <- tail(data.frame(SMALow, SMAHigh, 
                             combinedData$NFLX.Open, 
                             combinedData$NFLX.Close), 
                        nrow(forecast_data_actual)) #pick elements in prediction window
SMAonForecasted
colnames(SMAonForecasted) <- c("Low","High","Open","Close")
SMAonForecasted$signal <- ifelse(SMAonForecasted$Low>SMAonForecasted$High, 1,
                           ifelse(SMAonForecasted$Low<SMAonForecasted$High,-1, 0))
SMAonForecasted

#Calculate Profit
buy <- c()
sell <- c()
buyTrades = 1
sellTrades = 1

buyOK <- TRUE

for(i in 1:(nrow(SMAonForecasted)-1)){
  if ((SMAonForecasted$signal[i] == 1) & buyOK){
    buy[buyTrades] = SMAonForecasted$Open[i+1]
    buyTrades = buyTrades + 1
    buyOK <- FALSE}
  else if ((SMAonForecasted$signal[i] == -1) & !buyOK){
    sell[sellTrades] = SMAonForecasted$Open[i+1]
    sellTrades = sellTrades + 1
    buyOK <- TRUE}
}
buy
sell
profit = sum(sell - buy)
print(profit)
