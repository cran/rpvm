\name{.PVM.freebuf}
\alias{.PVM.freebuf}
\title{Free message buffer}
\description{
  Disposes of a message buffer.
}
\usage{
.PVM.freebuf (bufid)
}
\arguments{
  \item{bufid}{integer message buffer id}
}
\value{
  Returns integer status code.  Values less than zero indicate an error.
}
\details{
  This function frees the memory associated with the message buffer
  identified by \code{bufid}.  Message buffers are created by
  \code{\link{.PVM.mkbuf}}, \code{\link{.PVM.initsend}} and
  \code{\link{.PVM.recv}}.

  \code{\link{.PVM.freebuf}} should be called for a send buffer created
  by \code{\link{.PVM.mkbuf}} after the message has been send and is no
  longer needed.

  Receive buffer typically do not have to be freed unless they have been
  saved in the cources of using multiple buffers.  But
  \code{\link{.PVM.freebuf}} can be used to destroy receive buffers as
  well.  Therefore, messages that arrive but are no longer needed as a
  result of some other event can be destroyed to save buffer space.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.initsend}}, \code{\link{.PVM.mkbuf}},
  \code{\link{.PVM.setrbuf}},  \code{\link{.PVM.recv}} 
}
\examples{
bufid <- .PVM.mkbuf ("Raw")
## Send the message ...
.PVM.freebuf (bufid)
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
