\name{.PVM.mstats}
\alias{.PVM.mstats}
\title{Status of host machines}
\description{
   Check the status of hosts computers in the virtual machine.
}
\usage{
.PVM.mstats (hosts)
}
\arguments{
  \item{hosts}{vector of host names (strings)}
}
\value{
  Return machine status. \code{"OK"} if host is running, \code{"Not in
  VM"} if host is not in virtual machine, \code{"Not reachable"} is host
  is unreachable (and possibly failed).
}
\details{
  \code{.PVM.mstats} returns the status the computers named by
  \code{hosts} with respect to running PVM processes.  This routine can
  be used to determine if a particular host has failed and if the
  virtual machine needs to be reconfigured. \code{\link{.PVM.notify}}
  can also be used to notify the caller that a host has failed.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.notify}}
}
\examples{
\dontrun{.PVM.mstats ("abacus")}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
