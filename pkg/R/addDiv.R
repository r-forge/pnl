#################################################################################
# Add a cash dividend to the corporate actions
# Corporate action types for dividends are:
# 0 = cumdate cash dividend
# 1 = paydate cash dividend
#################################################################################
addDiv <-
function(corp, cumdate, paydate, amount=0.0) {
	corp = rbind(corp, xts(matrix(c(0, 1, amount, amount), nrow=2), c(as.Date(cumdate), as.Date(paydate))))

	if(nrow(corp) == 2) colnames(corp) = c("type", "amount") 

	return(corp)
}
