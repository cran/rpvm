\name{pack.sprng}
\alias{pack.sprng}
\title{Pack the RNG state into a string.}
\description{
  \code{pack.sprng} packs the SPRNG state into a string to print or to
  save for later use.
}
\usage{
pack.sprng ()
}
\value{
  Return a character string of the packed SPRNG state and \code{NULL} if
  there is none.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{unpack.sprng}}, \code{\link{spawn.sprng}}
}
\examples{
\dontrun{rngstate <- pack.sprng ()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{distribution}
\keyword{interface}