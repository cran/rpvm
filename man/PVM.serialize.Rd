\name{PVM.serialize}
\alias{.PVM.serialize}
\alias{.PVM.unserialize}
\title{Serialize R Objects}
\description{
  Serialize to/from the current active send/receive buffer.
}
\usage{
.PVM.serialize (object, refhook = NULL)
.PVM.unserialize(refhook = NULL)
}
\arguments{
  \item{object}{object to serialize.}
  \item{refhook}{hook function for handling reference objects}
}
\value{
  \code{.PVM.serialize} returns \code{NULL}; \code{.PVM.unserialize}
  returns the unserialized object.
}
\details{
  The function \code{.PVM.serialize} writes \code{object} to the
  current active send buffer.  Sharing of reference objects is
  preserved within the object but not across separate calls to
  \code{.PVM.serialize}.  \code{unserialize} reads an object from
  \code{connection}.  \code{connection} may also be a string.

  The \code{refhook} functions can be used to customize handling of
  non-system reference objects (all external pointers and weak
  references, and all environments other than name space and package
  environments and \code{.GlobalEnv}).  The hook function for
  \code{.PVM.serialize} should return a character vector for references it
  wants to handle; otherwise it should return NULL.  The hook for
  \code{.PVM.unserialize} will be called with character vectors supplied to
  \code{.PVM.serialize} and should return an appropriate object.
}
\examples{
## Pack and send a matrix
\dontrun{
BUFTAG<-22
tid <- .PVM.mytid()
a <- matrix (1:20, nrow=4)
.PVM.initsend()
.PVM.serialize(a)
.PVM.send(tid, BUFTAG)
## Receive the matrix
.PVM.recv(tid, BUFTAG)
.PVM.unserialize()
}
}
\author{
  Luke Tierney \email{luke@stat.umn.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
