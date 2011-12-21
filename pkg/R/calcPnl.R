##################################################################################
# Calculate trading-, unrealized- and realized pnl from transactions and price
# pseries.
#
##################################################################################
calcPnl <-
function(pseries, closepricecol, txn) {
	pseries$tpnl = (pseries$pos * pseries[,closepricecol]) - (lag(pseries$pos) * lag(pseries[,closepricecol])) + (-(pseries$size) * pseries$price)
	pseries[is.na(pseries$tpnl), "tpnl"] = 0	# remove NA's from top row caused by lag()
	
	# Calculate realized pnl from transactions
	if(!is.null(txn)) {
		if("rpnl" %in% colnames(pseries)) pseries$rpnl = NULL

		txn = rbind(txn, xts(matrix(c(0,0,0), ncol=3), as.Date("1970-01-01")))

		txn$pos = cumsum(txn$size)
		txn$ec = (-(txn$size) * txn$price) + txn$fees
		txn$cumec = cumsum(txn$ec)
		
		txn$rpnl = 0
		t = unclass(txn)
		t[which(t[, "pos"]==0), "rpnl"] = c(0, diff(t[which(t[,"pos"]==0), "cumec"]))
		txn$rpnl = t[, "rpnl"]

		pseries = cbind(pseries, as.xts(txn[-1, "rpnl"]), fill=0)
		
	} else {
		pseries$rpnl = 0
	}

	# TODO: upnl should not depend on rpnl, but how to do this without falling back on a loop? 
	pseries$upnl = cumsum(pseries$tpnl + pseries$fees - pseries$rpnl)

	pseries[is.na(pseries$upnl), "upnl"] = 0	# remove NA from  top row caused by cumsum()

	return(pseries)
}
