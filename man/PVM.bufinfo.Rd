\name{.PVM.bufinfo}
\alias{.PVM.bufinfo}
\title{Message buffer infomation}
\description{
  Returns information about the requested message buffer.
}
\usage{
.PVM.bufinfo (bufid)
}
\arguments{
  \item{bufid}{id of a particular message buffer}
}
\value{
  Return a list (\code{bytes}, \code{msgtag}, \code{tid}) of information
  about the requested message buffer.
  \item{bytes}{the length in bytes of the entire message}
  \item{msgtag}{the message label}
  \item{tid}{the source of the message}
}
\details{
  Returns information about the requested message buffer.  Typically it
  is used to determine about the last received message such as it size
  or source.  It is especially useful when an application is able to
  receive any incoming message (with the use of wildcards).
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.send}}, \code{\link{.PVM.nrecv}},
  \code{\link{.PVM.recv}}
}
\examples{
\dontrun{bufid <- .PVM.recv (-1, -1)}
\dontrun{info <- .PVM.bufinfo (bufid)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
