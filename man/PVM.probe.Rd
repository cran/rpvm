\name{.PVM.probe}
\alias{.PVM.probe}
\title{Probe receive}
\description{
  Checks whether message has arrived.
}
\usage{
.PVM.probe(tid = -1, msgtag = -1)
}
\arguments{
  \item{tid}{the specific tid, -1 is every tid}
  \item{msgtag}{tag to use to specific, -1 is all}
}
\value{
  Returns the value of the new active receive buffer id. 0 if the
  message has not arrived.  -1 if there was an error.
}
\details{
  Checks to see if a message with label \code{msgtag} has arrived from
  \code{tid}.  If a matching message has arrived, returns a buffer id
  which can be used in a \code{\link{.PVM.bufinfo}} call to determine
  information about the message such as its source and length.

  A -1 in \code{msgtag} or \code{tid} matches anything (wildcard).

  After the message has arrived, \code{\link{.PVM.recv}} must be called
  before the messag can be unpacked into the user's memory using the
  unpack routines.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.recv}}, \code{\link{.PVM.bufinfo}}
}
\examples{
## To check a node (specified by tid) for a message
\dontrun{MsgReady <- .PVM.probe (tid, msgtag)}
## To see if any node is sending message with tag thisTag
\dontrun{AnyMessageWithThisTag <- .PVM.probe (-1, thisTag)}
## To see if node tid is sending any message.
\dontrun{AnyMessageFromThisTID <- .PVM.probe (tid, -1)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
