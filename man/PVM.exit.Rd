\name{.PVM.exit}
\alias{.PVM.exit}
\title{Unregister process from local PVM deamon.}
\description{
  Tells the local pvmd that this process is leaving PVM.
}
\usage{
.PVM.exit()
}
\arguments{
  None.
}
\value{
  Return 0 when successfully unregistered.
}
\details{
  The function \code{.PVM.exit} tells local pvmd that this process is
  leaving PVM.  This process is not killed but can continue to perform
  other tasks.

  \code{.PVM.exit} should be called by all PVM process before they stop
  or exit for good.  It \emph{must} be called by processes what were not
  started with \code{.PVM.spawn}, such as the master \code{R} process.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.mytid}}
}
\examples{
## Finished with PVM
.PVM.exit()
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
