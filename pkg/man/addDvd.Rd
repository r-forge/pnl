\name{addDvd}
\alias{addDvd}
\title{ Add a cash dividend to a series of corporate actions }
\description{
  To add a cash dividend to the series of corporate actions for an instrument.
}
\usage{ corp = addDvd(corp, cumdate, paydate, amount) }
\arguments{
  \item{corp}{ the xts time series with corporate actions for this instrument. }
  \item{cumdate}{ the in-dividend date ( cum dividend) as ISO 8106, e.g., '2008-09-01'. }
  \item{paydate}{ the paydate of the dividend as ISO 8106, e.g., '2008-09-01'. }
  \item{amount}{ the cash dividend amount per share. }
}
\details{
  The \code{addDvd} function will only add the cash dividend to the series of corporate actions.
  No PnL calculations are done.
}
\value{
  Returns the series of coprporate actions with the new cash dividend.
}
\author{ Mark Breman }
\seealso{ \code{\link{addTrans}}, \code{\link{pnl}} }
\examples{
  ## pay a cash dividend on 2009-01-06 for the amount of stock held on 2009-01-02 times 0.0936
  corp = addDvd(NULL, cumdate='2009-01-02', paydate='2009-01-06', amount=0.0936)
}

