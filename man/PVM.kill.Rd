\name{.PVM.kill}
\alias{.PVM.kill}
\title{Kill pvm process}
\description{
  Terminate a specified PVM process.
}
\usage{
.PVM.kill(tids)
}
\arguments{
  \item{tid}{task ids of the PVM processes to be killded.}
}
\value{
  Return number of processes successfully killed.
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
\dontrun{.PVM.kill (children[1])}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
