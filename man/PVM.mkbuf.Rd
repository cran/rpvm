\name{.PVM.mkbuf}
\alias{.PVM.mkbuf}
\title{Create message buffer}
\description{
  Creates a new message buffer
}
\usage{
.PVM.mkbuf (encoding = c("Default", "Raw", "InPlace"))
}
\arguments{
  \item{encoding}{a character string specifying the next message's
    encoding scheme.  Must be one of \code{"Default"} (default),
    \code{"Raw"} or \code{"InPlace"}.}
}
\value{
  Returns message buffer id or -1 if failed..
}
\details{
  This function creates a new message buffer and sets its encoding theme
  (see \code{\link{.PVM.initsend}}.

  It is useful if the user wishes to manage multiple message buffers and
  should be used in conjunction with \code{\link{.PVM.freebuf}}.
  \code{\link{.PVM.freebuf}} should be called for a send buffer after a
  message has been send and is no longer needed.

  Receive buffer are created automatically by \code{\link{.PVM.recv}}
  and \code{\link{.PVM.nrecv}} routines and do not have to be freed
  unless they have been explicitly saved with \code{\link{.PVM.setrbuf}}.

  Typically multiple send and receive buffer are not needed and the user
  can simply use the \code{\link{.PVM.initsend}} to reset the default
  send buffer.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.initsend}},\code{\link{.PVM.freebuf}},
  \code{\link{.PVM.setrbuf}}
}
\examples{
\dontrun{bufid <- .PVM.mkbuf ("Raw")}
## Send the message ...
\dontrun{.PVM.freebuf (bufid)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
