\name{PVM.buffers}
\alias{.PVM.getsbuf}
\alias{.PVM.getrbuf}
\alias{.PVM.setsbuf}
\alias{.PVM.setrbuf}
\title{Manipulating Message Buffers}
\description{
  Get or set the id of the active send/receive buffer.
}
\usage{
.PVM.getsbuf ()
.PVM.getrbuf ()
.PVM.setsbuf (bufid)
.PVM.setrbuf (bufid)
}
\arguments{
  \item{bufid}{integer message buffer id}
}
\value{
  The \emph{get} functions return the id of active send/receive buffer.
  The \emph{set} functions return the id the old send/receive buffer
  (-1 indicate an error).
}
\details{
  The function \code{.PVM.getsbuf} returns the id of the active send
  buffer or 0 if no current buffer.

  The function \code{.PVM.getrbuf} returns the id of the active receive
  buffer or 0 if no current buffer.

  The function \code{.PVM.setrbuf} switches the active receive buffer to
  \code{bufid} and saves the previous active receive buffer and returns
  its id.  If \code{bufid} is set to 0, the present active receive
  buffer is saved and no active receive buffer exists.

  The function \code{.PVM.setsbuf} switches the active send buffer to
  \code{bufid} and saves the previous active send buffer and returns its
  id.  If \code{bufid} is set to 0, the present active send buffer is
  saved and no active send buffer exists.

  The \code{set} functions are required to manage multiple message buffers. 
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.mkbuf}}, \code{\link{.PVM.freebuf}}
}
\examples{
\dontrun{sbufid <- .PVM.getsbuf ()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
