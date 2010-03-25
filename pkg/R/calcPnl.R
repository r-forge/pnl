##################################################################################
# Calculate trading-, unrealized- and realized pnl from transactions and price
# series.
#
##################################################################################
calcPnl <-
function(series, closepricecol, txn) {
	series$tpnl = (series$pos * series[,closepricecol]) - (lag(series$pos) * lag(series[,closepricecol])) + (-(series$size) * series$price)
	series[is.na(series$tpnl), "tpnl"] = 0	# remove NA's from top row caused by lag()
	
	# Calculate realized pnl from transactions
	if(!is.null(txn)) {
		if("rpnl" %in% colnames(series)) series$rpnl = NULL

		txn = rbind(txn, xts(matrix(c(0,0,0), ncol=3), as.Date(0)))

		txn$pos = cumsum(txn$size)
		txn$ec = (-(txn$size) * txn$price) + txn$fees
		txn$cumec = cumsum(txn$ec)
		
		txn$rpnl = 0
		t = unclass(txn)
		t[which(t[, "pos"]==0), "rpnl"] = c(0, diff(t[which(t[,"pos"]==0), "cumec"]))
		txn$rpnl = t[, "rpnl"]

		series = cbind(series, as.xts(txn[-1, "rpnl"]), fill=0)
		
	} else {
		series$rpnl = 0
	}

	# TODO: upnl should not depend on rpnl, but how to do this without falling back on a loop? 
	series$upnl = cumsum(series$tpnl + series$fees - series$rpnl)

	series[is.na(series$upnl), "upnl"] = 0	# remove NA from  top row caused by cumsum()

	return(series)
}
