\name{spawn.sprng}
\alias{spawn.sprng}
\title{Spawn new random number streams based on the current one.}
\description{
  Some times when a process spawns children processes, it is desirable
  to spawn new streams from an old one and pass them on to children.
  \code{spawn.sprng} creates new random number streams based on the
  current one, packs them into strings and returns a vector of strings.
  Then these new streams can be passed to children through usual PVM
  message passing theme.
}
\usage{
spawn.sprng (nspawn)
}
\arguments{
  \item{nspawn}{number of new streams to spawn}
}
\value{
  Return a vector of length \code{nspawn} of character strings that
  represent the spawned and packed SPRNG states.  
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{pack.sprng}}, \code{\link{unpack.sprng}}
}
\examples{
\dontrun{child.rngs <- spawn.sprng (3)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{interface}
\keyword{distribution}
