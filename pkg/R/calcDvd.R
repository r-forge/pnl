##################################################################################
# Calculate unrealized and realized dividends from corporate actions and transactions,
# and add to series
#	
# constraints:
# - dividend cumdate <= dividend paydate
# - dividendN cumdate > dividendN-1 paydate
#
##################################################################################
calcDvd <-
function(series, txn, corp) {
	divs=corp[which(corp$type %in% c(0,1)),]

	if(!is.null(txn) & !is.null(divs)) {
		# remove columns from possible previous runs
		if("rdiv" %in% colnames(series)) series$rdiv = NULL
		if("diva" %in% colnames(series)) series$diva = NULL
		if("udiv" %in% colnames(series)) series$udiv = NULL
		
		divs = rbind(divs, xts(matrix(c(0,0), ncol=2), as.Date(0)))
		txn = rbind(txn, xts(matrix(c(0,0,0), ncol=3), as.Date(0)))

		txn$pos = cumsum(txn$size)
		divs = cbind(divs, na.locf(cbind(divs, txn$pos)$pos), join="left")

		# add realized dividends
		divs$rdiv = round(lag(divs$pos) * divs$amount, 2)
		series = cbind(series, divs[-1, "rdiv"], fill=0)

		# add dividend amount
		diva = divs[which(divs$type==0), c("pos", "amount")]
		colnames(diva) = c("pos", "diva")
		series = cbind(series, diva[-1, "diva"], fill=0)

		# add unrealized dividend
		# TODO: udiv should not depend on rdiv, but how? 
		series$udiv = cumsum(round(series[, "diva"] * series[, "pos"] - series$rdiv, 2))

	
	} else {
	 	series$diva=0
		series$udiv = 0
		series$rdiv = 0
	}

	return(series)
}
