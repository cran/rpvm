\name{.PVM.nrecv}
\alias{.PVM.nrecv}
\title{Nonblocking receive}
\description{
  Checks for nonblocking message.
}
\usage{
.PVM.nrecv (tid = -1, msgtag = -1)
}
\arguments{
  \item{tid}{tid of sending process (-1 matches any tid)}
  \item{msgtag}{integer message tag (>=0) supplied by the user (-1
  matches any message tag)}
}
\value{
  Returns the id of the new active receive buffer. -1 indicates an error.
}
\details{
  \code{.PVM.nrecv} checks to see whether a message with lable
  \code{msgtag} has arrived from \code{tid}.  If a matching message has
  arrived, it then immediately places the message in a new \emph{active}
  receive buffer, which also clears the current receive buffer, if any,
  and returns the buffer id.

  If the requested message has not arrived, then \code{.PVM.nrecv}
  immediately returns 0.
  
  A -1 in \code{tid} or \code{msgtag} matches anything.  Later
  \code{\link{.PVM.bufinfo}} can be used to find out the tid of the
  sending process or msgtag.

  \code{.PVM.nrecv} is nonblocking in the sense that the routing always
  returns immediately either with the message or with the information
  that the message has not arrived at the local pvmd yet.
  \code{.PVM.nrecv} can be called multiple times to check whether a
  given message has arrived yet.

  If \code{.PVM.nrecv} returns with the message, the data in the message
  can be unpacked with the \code{\link{PVM.unpack}} functions.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.send}}, \code{\link{.PVM.recv}},
  \code{\link{.PVM.bufinfo}}, \code{\link{PVM.unpack}}
}
\examples{
\dontrun{myparent <- .PVM.parent ()}
\dontrun{while (1) {
    bufid <- .PVM.nrecv (myparent, 4)
    if (bufid > 0) {
        data <- .PVM.upkintvec ()
        # do something ...
        break
    } else {
        # do something else
    }
}}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
