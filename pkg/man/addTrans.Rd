\name{addTrans}
\alias{addTrans}
\title{ Add a transaction to a series of transactions}
\description{
  To add a transaction to the series of transactions for an instrument.
}
\usage{ txn = addTrans(txn, date, size=100, price, fees = 0) }
\arguments{
  \item{txn}{ the xts time series with transactions for this instrument. }
  \item{date}{ the transaction date as ISO 8106, e.g., '2008-09-01'. }
  \item{size}{ the size of the transaction. Positive values indicate a 'buy'; negative values indicate a 'sell'. }
  \item{price}{ market clearing price at which the transaction was done. }
  \item{fees}{ fees for the transaction, e.g. commissions. Fees are indicated as negative values. }
}
\details{
  The \code{addTrans} function will only add the transaction to the series of transactions.
  No PnL calculations are done.
}
\value{
  Returns the series of transactions with the new transaction.
}
\author{ Mark Breman }
\seealso{ \code{\link{addDvd}}, \code{\link{pnl}} }
\examples{
  ## first transaction for this instrument:  on 2009-01-02 buy 36@23.05 and pay a commission of 1.0
  txn = addTrans(NULL, date='2009-01-02', size=36, price=23.05, fees=-1.0)

  ## sell them on 2009-01-04 for 23.09, and pay a commission of 1.0 
  txn = addTrans(txn, date='2009-01-04', size=-36, price=23.09, fees=-1.0)
}

