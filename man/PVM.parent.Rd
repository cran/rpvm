\name{.PVM.parent}
\alias{.PVM.parent}
\title{Parent task id}
\description{
  Returns the \code{tid} of the parent process.
}
\usage{
.PVM.parent ()
}
\arguments{
  None.
}
\value{
  Returns tid of the process that spawned the calling process.  
}
\details{
  If the calling process was not created with \code{\link{.PVM.spawn}},
  return 0 (no parent).
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.spawn}}, \code{\link{.PVM.mytid}}
}
\examples{
\dontrun{myparent <- .PVM.parent ()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
