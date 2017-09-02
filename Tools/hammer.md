
https://www.r-bloggers.com/a-hammer-trading-system-demonstrating-custom-indicator-based-limit-orders-in-quantstrat/
'''
hammer <- function(OHLC, profMargin=1.5) {
  dailyMax <- pmax(Op(OHLC), Cl(OHLC))
  dailyMin <- pmin(Op(OHLC), Cl(OHLC))
  upShadow <- Hi(OHLC) - dailyMax
  dnShadow <- dailyMin - Lo(OHLC)
  body <- dailyMax-dailyMin
  hammerDay <- dnShadow/body > 2 & dnShadow/upShadow > 5
  hammers <- OHLC[hammerDay==1,]
  hammers$stopLoss <- 4/3*Lo(hammers)-1/3*Hi(hammers)
  hammers$takeProfit <- Hi(hammers) + (Hi(hammers)-hammers$stopLoss)*profMargin
  hammers <- cbind(hammerDay, hammers$stopLoss, hammers$takeProfit)
  hammers$stopLoss <- na.locf(hammers$stopLoss)
  hammers$takeProfit <- na.locf(hammers$takeProfit)
  colnames(hammers) <- c("hammer", "SL", "TP")
  return(hammers)
}
 
require(IKTrading)
require(quantstrat)
require(PerformanceAnalytics)
 
#indicators
add.indicator(strategy.st, name="hammer",
              arguments=list(OHLC=quote(OHLC(mktdata)), 
                             profMargin=profMargin),
              label="hammer")
 '''
