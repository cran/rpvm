\name{.PVM.recv}
\alias{.PVM.recv}
\title{Blocking receive}
\description{
  Receive a message.
}
\usage{
.PVM.recv (tid = -1, msgtag = 1)
}
\arguments{
  \item{tid}{tid of sending process (-1 matches any tid)}
  \item{msgtag}{integer message tag (>=0) supplied by the user (-1
  matches any message tag)}
}
\value{
  Returns the id of the new active receive buffer. Values less than zero
  indicate an error.
}
\details{
  \code{.PVM.recv} blocks the process until a message with lable
  \code{msgtag} has arrived from \code{tid}.  It then places the message
  in a new \emph{active} receive buffer, which also clears the current
  receive buffer.

  A -1 in \code{tid} or \code{msgtag} matches anything.  Later
  \code{\link{.PVM.bufinfo}} can be used to find out the tid of the
  sending process or msgtag.

  The PVM model guarantees the following about message order. If task 1
  sends message A to task 2, then task 1 sends message B to task 2,
  message A will arrive at task 2 before message B.  Moreover, if both
  tasks arrive before task 2 does a receive, then a wildcard receive
  will always return message A.

  \code{.PVM.recv} is blocking, which means the routing waits until a
  message matching the user specified tid and msgtag value arrives at
  the local pvmd.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.send}}, \code{\link{.PVM.nrecv}},
  \code{\link{.PVM.bufinfo}}
}
\examples{
myparent <- .PVM.parent ()
bufid <- .PVM.recv (myparent, 4)
data <- .PVM.upkintvec ()
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
