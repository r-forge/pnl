library(xts)
library(pnl)

Sys.setenv(TZ="GMT")

prices = c(23.07, 23.05, 23.06, 23.09, 23.28, 23.36)
pseries = xts(prices, as.POSIXct(strptime(paste("2009-01-", seq(1:length(prices)), sep=""),"%Y-%m-%d")))
colnames(pseries) = "Close"

txn = addTxn(NULL, date='2009-01-02', size=36, price=23.05, fees=-1.0)
txn = addTxn(txn, date='2009-01-04', size=-36, price=23.09, fees=-1.0)

res = pnl(pseries, txn=txn)

checkEquals(matrix(res["2009-01-01",]), matrix(c(0,0,0,0,0,0,0,0,0,0)))
checkEquals(matrix(res["2009-01-02",]), matrix(c(36,23.05,-1,36,0,0,0,0,-1,0)))
checkEquals(matrix(res["2009-01-03",]), matrix(c(0,0,0,36,0,0,0,0.36,-0.64,0)))
checkEquals(matrix(res["2009-01-04",]), matrix(c(-36,23.09,-1,0,0,0,0,1.08,0,-0.56)))
checkEquals(matrix(res["2009-01-05",]), matrix(c(0,0,0,0,0,0,0,0,0,0)))
checkEquals(matrix(res["2009-01-06",]), matrix(c(0,0,0,0,0,0,0,0,0,0)))

