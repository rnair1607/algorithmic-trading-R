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
forecast_data_actual<-forecast_data_actual[, c("NFLX.Open", "NFLX.Close")]
forecast_data_actual

#Combine stockdata used for train-test with predicted values so that
#we have enough data to use for generating trading indicators
combinedForecastData <-  rbind(stckdata[,c(1,4)],future_data)
combinedActualData <- rbind(stckdata[,c(1,4)],forecast_data_actual)
head(combinedForecastData)
head(combinedActualData)

#---------------------------------------------------------------------------------------------------------------------
#Use SMA to develop trading signals on forecasted data
generateSMA <- function(d){
  
  low=20
  high=50
  SMALow <-SMA(d$NFLX.Close,n=low)
  SMAHigh <-SMA(d$NFLX.Close,n=high)
  
  SMAonForecasted <- tail(data.frame(SMALow, SMAHigh, 
                                     d$NFLX.Open, 
                                     d$NFLX.Close), 
                          nrow(forecast_data_actual)) #pick elements in prediction window
  #SMAonForecasted
  colnames(SMAonForecasted) <- c("Low","High","Open","Close")
  SMAonForecasted$signal <- ifelse(SMAonForecasted$Low>SMAonForecasted$High, 1,
                                   ifelse(SMAonForecasted$Low<SMAonForecasted$High,-1, 0))
  #SMAonForecasted
  
  
  return (SMAonForecasted)
}


#---------------------------------------------------------------------------------------------------------------------
#Use Bollinger Band to develop trading signals on forecasted data
generateBBand <- function(d){
  
  BBand <- BBands(d$NFLX.Close,n=20,sd=2)
  BBandonForecasted <- tail(data.frame(BBand,d$NFLX.Open,d$NFLX.Close),nrow(forecast_data_actual))
  #BBandonForecasted
  colnames(BBandonForecasted) <- c("dn","mavg","up","pctB","Open","Close")
  BBandonForecasted$signal <- ifelse(BBandonForecasted$Close>BBandonForecasted$up,1,
                                     ifelse(BBandonForecasted$Close<BBandonForecasted$dn,-1,0))
  
  #BBandonForecasted
  
  return (BBandonForecasted)
}


#--------------------------------------------------------------------------------------------------------------------
#Use RSI to develop trading signals on forecasted data
generateRSI <- function(d,ma){
  Rsi <- RSI(d$NFLX.Close,ma,n=9)
  RsionForecasted <- tail(data.frame(Rsi,d$NFLX.Open,d$NFLX.Close),nrow(forecast_data_actual))
  #RsionForecasted
  colnames(RsionForecasted) <- c("rsi","Open","Close")
  RsionForecasted$signal <- ifelse(RsionForecasted$rsi>55,-1,
                                     ifelse(RsionForecasted$rsi<42,1,0))
  #RsionForecasted
  return (RsionForecasted)
}
#--------------------------------------------------------------------------------------------------------------------
#Use MACD to develop trading signals on forecasted data
generateMACD <- function(d){
  Macd <- MACD(d$NFLX.Close,nFast=43,nSlow=54,nSig=10)
  MacdonForecasted <- tail(data.frame(Macd,d$NFLX.Open,d$NFLX.Close),nrow(forecast_data_actual))
  #RsionForecasted
  colnames(MacdonForecasted) <- c("macd","signal_","Open","Close")
  MacdonForecasted$signal <- ifelse(MacdonForecasted$macd>MacdonForecasted$signal_,-1,
                                   ifelse(MacdonForecasted$macd<MacdonForecasted$signal_,1,0))
  #RsionForecasted
  return (MacdonForecasted)
}
#--------------------------------------------------------------------------------------------------------------------


#Calculate Profit
doTrading <- function(Forecasted){
  buy <- c()
  sell <- c()
  profit <- c()
  number_of_stocks <- c()
  buyTrades <- 1
  sellTrades <- 1
  tradingBudget <- 10000
  buyOK <- TRUE
for(i in 1:(nrow(Forecasted)-1)){
  if ((Forecasted$signal[i] == 1) & buyOK){
    
    buy[buyTrades] <- Forecasted$Open[i+1]
    number_of_stocks[buyTrades] <- tradingBudget %/% Forecasted$Open[i+1]
    buyTrades <- buyTrades + 1
    buyOK <- FALSE
     }
  else if ((Forecasted$signal[i] == -1) & !buyOK){
    sell [sellTrades] <- Forecasted$Open[i+1]
    profit[sellTrades] <- number_of_stocks[sellTrades] * (sell[sellTrades]-buy[sellTrades])
    sellTrades <- sellTrades + 1
    buyOK <- TRUE}
  if (i == (nrow(Forecasted)-1)){
    if(!buyOK){
      sell [sellTrades] <- Forecasted$Open[i]
      profit[sellTrades] <- number_of_stocks[sellTrades] * (sell[sellTrades]-buy[sellTrades])
      sellTrades <- sellTrades + 1
      buyOK <- TRUE
    }
    final_profit <- sum(profit)
  }
}
  return (final_profit)
}
#----------------------------------------------------------------------------------------------------------
# Generate signals for all data

RSI_SMA_predicted <- generateRSI(combinedForecastData,SMA)
RSI_SMA_actual <- generateRSI(combinedActualData,SMA)
RSI_EMA_predicted <- generateRSI(combinedForecastData,EMA)
RSI_EMA_actual <- generateRSI(combinedActualData,EMA)

MACD_predicted <- generateMACD(combinedForecastData)
MACD_actual <- generateMACD(combinedActualData)

BBand_predicted <- generateBBand(combinedForecastData)
BBand_actual <- generateBBand(combinedActualData)

SMA_predicted <- generateSMA(combinedForecastData)
SMA_actual <- generateSMA(combinedActualData)
#----------------------------------------------------------------------------------------------------------
# Calculate profits
#p <- doTrading(BBandonForecasted)

cat("Profit with forecasted RSI(SMA):",doTrading(RSI_SMA_predicted))
cat("Profit with actual RSI(SMA):",doTrading(RSI_SMA_actual))
cat("Profit with forecasted RSI(EMA):",doTrading(RSI_EMA_predicted))
cat("Profit with actual RSI(EMA):",doTrading(RSI_EMA_actual))

cat("Profit with MACD predicted",doTrading(MACD_predicted))
cat("Profit with MACD actual",doTrading(MACD_actual))

cat("Profit with BBAND predicted",doTrading(BBand_predicted))
cat("Profit with BBAND actual",doTrading(BBand_actual))

cat("Profit with SMA predicted",doTrading(SMA_predicted))
cat("Profit with SMA actual",doTrading(SMA_actual))




