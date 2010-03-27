\name{pnl}
\alias{pnl}
\title{ Calculate profit and loss }
\description{
  Calculate profit and loss based on price series, transactions and corporate actions for an instrument.
}
\usage{ pl = pnl(pseries, closepricecol, txn, corp) }
\arguments{
  \item{pseries}{ the xts time series with price data for this instrument. }
  \item{closepricecol}{ the column of pseries that holds the close prices for this instrument. }
  \item{txn}{ the xts time series with transactions for this instrument. }
  \item{corp}{ the xts time series with corporate actions for this instrument. }
}
\details{
  TODO
}
\value{
  Returns an xts series with period profit and loss.
}
\author{ Mark Breman }
\seealso{ \code{\link{addTrans}}, \code{\link{addDvd}} }
\examples{
  ## TODO
}

