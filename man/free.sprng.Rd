\name{free.sprng}
\alias{free.sprng}
\title{Free memory used by SPRNG}
\description{
  \code{free.sprng} frees the memory allocated to save the RNG states by
  SPRNG and calls \code{\link{RNGkind}} to restore R's serial RNG.
}
\usage{
free.sprng (kind.old = c("Marsaglia-Multicarry",
                         "Kinderman-Ramage"))
}
\arguments{
  \item{kind.old}{a length 2 character vector, the old rng kinds
  (possiblly returned by \code{\link{init.sprng}})}
}
\value{
  Return current RNG state packed as a string and \code{NULL} if there
  no active RNG stream.
}
\details{
  SPRNG allocates some memory to save the state of the underlying RNG
  and the memory must be explicitly freed.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{init.sprng}}, \code{\link{pack.sprng}}
}
\examples{
# in master script
\dontrun{sprng.state.old <- free.sprng (oldrng)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{interface}
\keyword{distribution}