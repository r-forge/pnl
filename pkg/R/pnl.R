###########################################################################################################
# pnl - Calculate trading-, unrealized- and realized profit and loss.
#
# @param pseries xts timeseries with prices
# @param closepricecol the column number in pseries that holds the close prices
# @param txn xts timeseries with transactions
# @param corp xts timeseries with corporate actions
#
# @author Mark Breman
# @export
pnl <-
function(pseries, closepricecol=1, txn=NULL, corp=NULL) {
	if(is.null(txn)) {
		pseries$size=0
		pseries$price=0
		pseries$fees=0
		pseries$pos=0
	} else {
		# for multiple transactions on one date
		pseries = na.locf(cbind(pseries, index(txn))) # ..also for corp?
		
		# remove columns from possible previous runs
		if("size" %in% colnames(pseries)) pseries$size = NULL
		if("price" %in% colnames(pseries)) pseries$price = NULL
		if("fees" %in% colnames(pseries)) pseries$fees = NULL
		if("pos" %in% colnames(pseries)) pseries$pos = NULL

		pseries = cbind(pseries, txn[, c("size", "price", "fees")], fill=0)
		pseries$pos = cumsum(pseries$size)

		if(is.null(corp)) {
			pseries$diva=0
			pseries$udiv=0
			pseries$rdiv=0
		} else {
			pseries = calcDiv(pseries, txn, corp)
		}

		pseries = calcPnl(pseries, closepricecol, txn)
	}

	return(pseries[, c("size", "price", "fees", "pos", "diva", "udiv", "rdiv", "tpnl", "upnl", "rpnl")])
}

