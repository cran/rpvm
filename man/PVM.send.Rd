\name{.PVM.send}
\alias{.PVM.send}
\title{Send data}
\description{
  Sends the data in the active message buffer.
}
\usage{
.PVM.send (tid, msgtag)
}
\arguments{
  \item{tid}{integer task id of destination process}
  \item{msgtag}{integer message tag (>=0) supplied by the user}
}
\value{
  Returns status code. 0 if sucessful. Values less than zero indicate an
  error.
}
\details{
  Sends a message stored in the active buffer to the PVM process
  identified by \code{tid}.  \code{msgtag} is used to label the content
  of the message.

  \code{.PVM.send} is asynchronous.  Computation on the sending processor
  resumes as soon as the message is safely on its way to the receiving
  processor.  This is in constrast to synchronous communication in which
  computation on the sending processor halts until the matching receive
  is executed by the receiving processor.

  \code{.PVM.send} first checks to see whether the destination is on the
  same machine.  If so and this host is a multiprocessor, then the
  vender's underlying message-passing routines are used to move the data
  between processes.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.mcast}}, \code{\link{.PVM.recv}}
}
\examples{
a <- 1:10
bufid <- .PVM.initsend ()
.PVM.pkintvec (a)
.PVM.send (tid, msgtag)
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
