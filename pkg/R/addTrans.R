#################################################################################
# Add a transaction
# 
# @param txn xts timeseries with transactions
# @author Mark Breman
# @export
addTrans <-
function(txn, date=as.character(Sys.Date()), size=0, price=0.0, fees=0.0) {
	if(fees > 0) stop("fees must be <= 0")
 
	txn = rbind(txn, xts(matrix(c(size, price, fees), ncol=3), as.Date(date)))

	if(nrow(txn) == 1) colnames(txn) = c("size", "price", "fees") 

	return(txn)
}

