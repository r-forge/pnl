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
		if("rdvd" %in% colnames(series)) series$rdvd = NULL
		if("dvda" %in% colnames(series)) series$dvda = NULL
		if("udvd" %in% colnames(series)) series$udvd = NULL
		
		divs = rbind(divs, xts(matrix(c(0,0), ncol=2), as.Date(0)))
		txn = rbind(txn, xts(matrix(c(0,0,0), ncol=3), as.Date(0)))

		txn$pos = cumsum(txn$size)
		divs = cbind(divs, na.locf(cbind(divs, txn$pos)$pos), join="left")

		# add realized dividends
		divs$rdvd = round(lag(divs$pos) * divs$amount, 2)
		series = cbind(series, divs[-1, "rdvd"], fill=0)

		# add dividend amount
		dvda = divs[which(divs$type==0), c("pos", "amount")]
		colnames(dvda) = c("pos", "dvda")
		series = cbind(series, dvda[-1, "dvda"], fill=0)

		# add unrealized dividend
		# TODO: udvd should not depend on rdvd, but how? 
		series$udvd = cumsum(round(series[, "dvda"] * series[, "pos"] - series$rdvd, 2))

	
	} else {
	 	series$dvda=0
		series$udvd = 0
		series$rdvd = 0
	}

	return(series)
}
