\name{.PVM.mcast}
\alias{.PVM.mcast}
\title{Multicast data}
\description{
  Multicast the data in the active message buffer to a set of tasks.
}
\usage{
.PVM.mcast (tids, msgtag)
}
\arguments{
  \item{tids}{vector of the task ids of the tasks to be sent to}
  \item{msgtag}{integer message tag supplied by the user, must be >=0}
}
\value{
  Return a status code.  Values less than zero indicates an error.
}
\details{
  \code{.PVM.mcast} multicasts a message stored in the active send
  buffer to tasks specified in the vector \code{tids}.  The message is
  not send to the caller even if its tid is in \code{tids}.  The content
  of the message can be dustinguished by \code{msgtag}.

  The receiving processes can call either \code{\link{.PVM.recv}} or
  \code{\link{.PVM.nrecv}} to receive their copy of the multicast.
  \code{.PVM.mcast} is asynchronous.

  Multicasting is not supported by most multiprocessor vendors.
  Typically their native calls support only broadcasting to \emph{all}
  the user's processes on a multiprocessor.  Because of this omission,
  \code{.PVM.mcast} may not be an efficient communication method on some
  multiprocessors except in the special case of broadcasting to all PVM
  processses.    
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.send}}
}
\examples{
info <- .PVM.initsend ("Raw")
info <- .PVM.pkintvec (1:10)
info <- .PVM.mcast (tids, msgtag = 5)
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
