\name{.PVM.kill}
\alias{.PVM.kill}
\title{Kill pvm process}
\description{
  Terminate a specified PVM process.
}
\usage{
.PVM.kill(tid)
}
\arguments{
  \item{tid}{Task id of the PVM process to be killded.}
}
\value{
  Return 0 when successful.  Values less than 0 indicate an error.
}
\details{
  \code{.PVM.kill} sends a terminate (\code{SIGTERM}) signal to the PVM
  process identified by \code{tid}.  It is not designed to kill the
  calling process.  To kill your self, call \code{\link{.PVM.exit}()}
  followed by \code{q()}.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.exit}}
}
\examples{
## kill one of spawned children
.PVM.kill (children[1])
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
