\name{unpack.sprng}
\alias{unpack.sprng}
\title{Unpack a string encoding the RNG state.}
\description{
  \code{pack.sprng} unpacks a SPRNG state string and replace the current
  SPRNG state with the new one.
}
\usage{
unpack.sprng (rngstate)
}
\arguments{
  \item{rngstate}{a character string representing SPRNG state (should be
  returned by either \code{\link{pack.sprng}} or \code{\link{spawn.sprng}})}
}
\value{
  Return a character string of the packed old SPRNG state if there is
  one and return \code{NULL} if not.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{unpack.sprng}}, \code{\link{spawn.sprng}}
}
\examples{
\dontrun{rng.old <- unpack.sprng (rng.new)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{distribution}
\keyword{interface}