\name{PVM.pack}
\alias{.PVM.pkdouble}
\alias{.PVM.pkint}
\alias{.PVM.pkstr}
\alias{.PVM.pkintvec}
\alias{.PVM.pkdblvec}
\alias{.PVM.pkstrvec}
\alias{.PVM.pkintmat}
\alias{.PVM.pkdblmat}
\alias{.PVM.pkstrmat}
\alias{.PVM.pkfactor}

\title{Packing data}
\description{
  Pack data into current active send buffer.
}
\usage{
.PVM.pkdouble (data = 0.0, stride = 1)
.PVM.pkint (data = 0, stride = 1)
.PVM.pkstr (data = "")

.PVM.pkintvec (data)
.PVM.pkdblvec (data)
.PVM.pkstrvec (data)

.PVM.pkintmat (data)
.PVM.pkdblmat (data)
.PVM.pkstrmat (data)

.PVM.pkfactor (data)
}
\arguments{
  \item{data}{data to be packed.}
  \item{stride}{the stride to be used when packing the items.  For
    example, if \code{stride = 2} with \code{.PVM.pkdouble} then every
    other element in the vector \code{data} will be packed.}
}
\value{
  Return status code.  Values less than zero indicates an error.
}
\details{
  The first three functions are low-level correspondents of the PVM
  packing routines for packing double, integer arrays and a single
  character string.  In particular, the number of item packed
  is not passed and has to be specified when unpacking the data.

  The other functions also pack the dimension information therefore
  there is no need to specify length and the correct dimension will be
  automatically recovered when corresponding unpack functions are used.
}
\note{
  The PVM C library supports a variety of data types.  Since R doesn't
  have that many types, most are irrelevant.

  To passing data between R process, there is in general no need to
  distinguish between integer and double (except for efficiency
  consideration). \code{.PVM.dblvec} and \code{.PVM.dblmat} are
  recommended for all numerical data.

  Special values such as NAs, NaNs, Infs, etc., are not supported at
  this point.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{PVM.unpack}}
}
\examples{
## Pack and send a matrix
a <- matrix (1:20, nrow=4)
.PVM.pkdblmat (a)
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
