\name{.PVM.trecv}
\alias{.PVM.trecv}
\title{Timeout receive}
\description{
  Checks for nonblocking message.
}
\usage{
.PVM.trecv (tid = -1, msgtag = 1, sec = 0, usec = 0)
}
\arguments{
  \item{tid}{tid of sending process (-1 matches any tid)}
  \item{msgtag}{integer message tag (>=0) supplied by the user (-1
    matches any message tag)}
  \item{sec}{time to wait in seconds}
  \item{usec}{time to wait in microseconds}
}
\value{
  Returns the id of the new active receive buffer. -1 indicates an error.
}
\details{
  \code{.PVM.trecv} blocks the process until a message with label
  \code{msgtag} has arrived from \code{tid}.  \code{.PVM.trecv} then
  places the message in a new active receive buffer, also clearing the
  current receive buffer.  If no matching message arrives within the
  specified waiting time, \code{.PVM.trecv} returns without a message.

  \code{sec} and \code{usec} specify how long \code{.PVM.trecv} will
  wait without returning a matching message.  With both set to zero,
  \code{.PVM.trecv} behaves the same as \code{\link{.PVM.nrecv}}, which
  is to probe for messages and return immediately even if none are
  matched.  Setting \code{sec} to -1 makes \code{.PVM.trecv} behave like
  \code{\link{.PVM.trecv}}, that is, it will wait indefinitely.

  If \code{.PVM.trecv} is successful, it will return the new active
  receive buffer identifier.  If no message is received, it returns 0.
  If some error occurs then the return value will be < 0.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.nrecv}}, \code{\link{.PVM.recv}}
}
\examples{
\dontrun{myparent <- .PVM.parent ()}
\dontrun{bufid <- .PVM.trecv (myparent, 4, 2)}
\dontrun{if (bufid > 0) data <- .PVM.upkintvec ()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
