\name{.PVM.mytid}
\alias{.PVM.mytid}
\title{Current task id}
\description{
  Returns the \code{tid} of the current process.
}
\usage{
.PVM.mytid()
}
\arguments{
  None.
}
\value{
  Returns integer task id of the calling process.  Return -1 if an error
  occured.
}
\details{
  \code{.PVM.mytid} returns the \code{tid} of the calling process and
  can be called multiple times.  The \code{tid} is a positive integer
  created by local pvmd.
  
  This function enrolls this process into PVM on its first call and
  generate a unique \code{tid} if this process was not created by
  \code{\link{.PVM.spawn}}.  In fact, any PVM system call will enroll a
  task in PVM if the task is not enrolled before the call.
}

\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.exit}}, \code{\link{.PVM.spawn}}
}
\examples{
## For name(tid) of process and to register with PVM
\dontrun{mytid <- .PVM.mytid()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
