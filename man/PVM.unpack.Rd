\name{PVM.unpack}
\alias{PVM.unpack}
\alias{.PVM.upkdouble}
\alias{.PVM.upkint}
\alias{.PVM.upkstr}
\alias{.PVM.upkintvec}
\alias{.PVM.upkdblvec}
\alias{.PVM.upkstrvec}
\alias{.PVM.upkintmat}
\alias{.PVM.upkdblmat}
\alias{.PVM.upkstrmat}
\alias{.PVM.upkfactor}
\title{Unpacking data}
\description{
  Unpack data from current active receive buffer.
}
\usage{
.PVM.upkdouble (nitem = 1, stride = 1)
.PVM.upkint (nitem = 1, stride = 1)
.PVM.upkstr (maxlen = 200)

.PVM.upkintvec ()
.PVM.upkdblvec ()
.PVM.upkstrvec ()

.PVM.upkintmat ()
.PVM.upkdblmat ()
.PVM.upkstrmat ()

.PVM.upkfactor ()
}
\arguments{
  \item{nitem}{number of items to unpack.}
  \item{stride}{the stride to be used when unpacking the items.  For
    example, if \code{stride = 2} with \code{.PVM.upkdouble} then every
    other element in the buffer will be unpacked.}
  \item{maxlen}{maximum length of the string buffer}
}
\value{
  Return unpacked data.
}
\details{
  These functions unpack data packed by corresponding \code{.PVM.pk*}
  functions.  The first two functions \code{.PVM.upkdoubl} and
  \code{.PVM.upkint} are lower-level correspondents of the PVM library
  and require the number of items to unpack as an argument.

  \code{.PVM.upkstr} is used to unpack a string send by C functions or
  \code{\link{.PVM.pkstr}}.  It requires an argument \code{maxlen} that
  should be larger than the length of the string passed.  For packing
  and unpacking strings between R processes, \code{\link{.PVM.pkstrvec}}
  and \code{.PVM.upkstrvec} can be used (in the case, the maximum length
  is part of the message).
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{PVM.pack}}
}
\examples{
## Unpack a matrix
\dontrun{a <- .PVM.upkdblmat ()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
