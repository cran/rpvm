\name{.PVM.initsend}
\alias{.PVM.initsend}
\alias{.PVM.encoding}
\title{Initialize send buffer}
\description{
  Clears default send buffer and specifies message sending.
}
\usage{
.PVM.initsend (encoding = c("Default", "Raw", "InPlace"))
}
\arguments{
  \item{encoding}{a character string specifying the next message's
  encoding scheme.  Must be one of \code{"Default"} (default),
  \code{"Raw"} or \code{"InPlace"}.}
}
\value{
  Returns message buffer id. -1 if there was an error.
}
\details{
  This function clears the send buffer and prepares it for packing a new
  message.

  Possible encoding themes are \code{"Default"} (default), \code{"Raw"}
  or \code{"InPlace"}. If the user knows that the next message will be
  sent only to a machine that understands the native format, he can use
  \code{"Raw"} to save some encoding costs.

  \code{"InPlace"} encoding specifies that data be left in place during
  packing.  The message buffer contains only the size and pointers to
  the items be send.  When \code{\link{.PVM.send}} is called, the items
  are copied directly out of the user's memory.  This option decreases
  the number of times a message is copied, at the expense of requiring
  that the user not modify the items between the time they are packed
  and the time they are sent.

  \code{.PVM.encoding} is a mapping of the strings to integers (constants
  defined by PVM).
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.mkbuf}}
}
\examples{
a <- 1:10
\dontrun{bufid <- .PVM.initsend ("InPlace")}
\dontrun{.PVM.pkintvec (a)}
\dontrun{.PVM.send (tid, msgtag)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
