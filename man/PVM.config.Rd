\name{.PVM.config}
\alias{.PVM.config}
\title{PVMD configuration}
\description{
  Query the configuration of the parallel virtual machine.
}
\usage{
.PVM.config ()
}
\arguments{
  None.
}
\value{
  Returns the configuration information as a data frame, including each
  host's pvmd task id, name, architecture and relative speed.  One row
  per host.
}
\details{
  \code{.PVM.config} returns information about the present virtual
  machine, similar to that available from the PVM console command
  \code{conf}.
}
\references{
  PVM documentation
}
\examples{
try (.PVM.config ())
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
