\name{init.sprng.master}
\alias{init.sprng.master}
\alias{init.sprng.slave}
\alias{init.sprng.group}
\alias{.SPRNG.INIT.TAG}
\title{Interface to rsprng for initializing SPRNG}
\description{
  \code{init.sprng.master} and \code{init.sprng.slave} are used together
  to initialize the parallel random number generator (PRNG) states in
  the RPVM master and slaves processes, respectively.

  \code{init.sprng.group} is used to initialize PRNG states for a RPVM
  group.

  \code{RNGkind ("user")} is called at the end of these functions so
  that the default RNG is substituted with the parallel one.

  \code{.SPRNG.INIT.TAG} is just a prefined message tag number.
}
\usage{
init.sprng.master (children, seed = 0, kindprng = "default",
                   para = 0)
init.sprng.slave  ()
init.sprng.group  (group, rootinst = 0, seed = 0,
                   kindprng = "default", para = 0)
.SPRNG.INIT.TAG <- 199
}
\arguments{
  \item{children}{vector of integer task ids of the slave processes}
  \item{seed}{an integer of random number seed.  It is not the starting
    state of the sequence; rather, it is an encoding of the starting
    state. The same seed for all the streams.  Distinct streams are
    returned.  Only the 31 least significant bits of seed are used in
    determining the initial starting state of the stream} 
  \item{kindprng}{a character string of the disired kind of parallel
    random number generator}
  \item{para}{additional parameters for the parallel random number
    generators.  If \code{para} is 0, default parameters for each PRNG
    are used.  When invalid parameter is given, a warning is issued and
    the default paramter is used.}
  \item{group}{a character string, the name of the group}
  \item{rootinst}{an integer, the root instance number of the group.
    The root instance is responsible for getting the \code{seed},
    \code{rngkind} and \code{para} parameters from the user}
}
\value{
  Return the a two-element character vector of the RNG and normal kinds
  in use before the call.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link[rsprng]{init.sprng}}
}
\examples{
# in master script
\dontrun{oldrng <- init.sprng.master (children, seed = 2321)}
# in slave script 
\dontrun{oldrng <- init.sprng.slave ()}
# in group processes
\dontrun{oldrng <- init.sprng.group ("mygroup", root = 0, seed = 1231)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{interface}
\keyword{distribution}
