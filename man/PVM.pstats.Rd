\name{.PVM.pstats}
\alias{.PVM.pstats}
\title{Status of PVM processes}
\description{
  Returns the status of the specified PVM process.
}
\usage{
.PVM.pstats (tids)
}
\arguments{
  \item{tid}{vector of integer task id of destination process}
}
\value{
  Returns the status of the PVM processes identified by tids. 1 if
  process is running, 0 if not, -1 if the tid is bad.
}
\details{
  The routine \code{.PVM.pstat} returns the status of the process
  identified by \code{tid}.  Also note that \code{\link{.PVM.notify}}
  can be used to notify the caller that a task has failed.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.notify}}
}
\examples{
tid <- .PVM.parent ()
status <- .PVM.pstats (tid)
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
