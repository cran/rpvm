\name{init.sprng}
\alias{init.sprng.master}
\alias{init.sprng.slave}
\alias{init.sprng.group}
\alias{.SPRNG.INIT.TAG}
\alias{init.sprng}
\title{Initialize SPRNG (Scalable Parallel Random Number Generator)}
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
init.sprng.master (children, seed = .Random.seed[2], prngkind = "LFG",
                   para = 0)
init.sprng.slave  ()
init.sprng.group  (group, rootinst = 0, seed = .Random.seed[2],
                   prngkind = "LFG", para = 0)
.SPRNG.INIT.TAG <- 199
}
\arguments{
  \item{children}{vector of integer task ids of the slave processes}
  \item{seed}{an integer of random number seed.  It is not the starting
    state of the sequence; rather, it is an encoding of the starting
    state. The same seed for all the streams.  Distinct streams are
    returned.  Only the 31 least significant bits of seed are used in
    determining the initial starting state of the stream} 
  \item{prngkind}{a character string of the disired kind of parallel
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
\details{
  The currently available PRNG kinds are given below.  \code{prngkind} is
  partially matched to this list.  The default is \code{"LFG"}.
  \enumerate{
    \item{\code{"LFG"}}{\emph{Modified Lagged Fibonacci Generator}

     The recurrence relation for this sequence of random numbers is
     given by the following equation:
     
     \deqn{z_n = x_n \mathrm{XOR} y_n}
     
     where XOR is the exclusive-or operator, \eqn{x} and \eqn{y} are
     sequences obtained from Lagged Fibonacci sequences \eqn{X} and
     \eqn{Y} of the following form:
     
     \deqn{X_n = (X_{n-k} + X_{n-l}) \mathrm{ Mod } M}
     \deqn{Y_n = (Y_{n-k} + Y_{n-l}) \mathrm{ Mod } M}
     
     \eqn{l} and \eqn{k} are called the lags of the generator, and we
     use the convention that \eqn{l > k}.  \eqn{M} is chosen to be
     \eqn{2^32}. \eqn{X_n} and \eqn{Y_n} are 32 bit integers.  \eqn{x}
     is obtained from \eqn{X} by setting the Least Significant Bit of
     the latter to 0. \eqn{y} is obtained from \eqn{Y} by shifting the
     latter right by one bit. This modification of the Lagged Fibonacci
     Generator is performed in order to avoid certain correlations that
     are observed in the unmodified generator.
                                             
     The period of this generator is \eqn{2^31(2^l-1)} where \eqn{l} is
     the lag.  For the default generator with lag \eqn{l = 1279}, the
     period is approximately \eqn{2^1310}. The number of distinct
     streams available is \eqn{2^{31(l-1)-1}}.  For the default
     generator this gives \eqn{2^39648} distinct streams.
     
     The parameters to this generator are the values of the lags.
     Allowed \code{para} values are:
     \tabular{lrr}{
       \code{para}  \tab l     \tab k   \cr
       0  (default) \tab 1279  \tab 861 \cr
       1            \tab 17    \tab 5   \cr
       2            \tab 31    \tab 6   \cr
       3            \tab 55    \tab 24  \cr
       4            \tab 63    \tab 31  \cr
       5            \tab 127   \tab 97  \cr
       6            \tab 521   \tab 353 \cr
       7            \tab 521   \tab 168 \cr
       8            \tab 607   \tab 334 \cr
       9            \tab 607   \tab 273 \cr
       10           \tab 1279  \tab 418
     }
   }
    \item{\code{"LCG"}}{\emph{48 Bit Linear Congruential Generator with
        Prime Addend}
      
      The recurrence relation for the sequence of random numbers
      produced by this generator is given by the following recurrence:      
      \deqn{x_n = (a x_{n-1} + p ) \mathrm{ Mod } M}  
      where \eqn{x_n} is the \eqn{n} th term in the sequence, \eqn{p} is
      a prime number and \eqn{a} is the multiplier.  The value of
      \eqn{M} for this generator is \eqn{2^48}.  Different random number
      streams are obtained by choosing different prime numbers as the
      addend \eqn{p}.  The period of this generator is \eqn{2^48}. The
      number of distinct streams available is of the order of
      \eqn{2^19}.

      The multiplier \eqn{a} is a parameter to this generator.  Allowed
      \code{para} values are 0 to 6, corresponding to 7 prefined
      multipliers.
    }
    \item{\code{"LCG64"}}{\emph{64 Bit Linear Congruential Generator
        with Prime Addend}

      The features of this generator are similar to the \code{"LCG"},
      except that the arithmetic is modulo \eqn{2^64}.  The multipliers
      and prime addends \eqn{p} for this generator are different from
      those for the 48 bit generator.
     
      The period of this generator is \eqn{2^64}.  The number of
      distinct streams available is over \eqn{10^8}.  Allowed
      \code{para} values are 0 to 2, corresponding to 3 prefine
      multipliers.
    }
    \item{\code{"CMRG"}}{\emph{Combined Multiple Recursive Generator}

      This generator is defined by the following relation:      

      \deqn{z_n = (x_n + y_n * 2^32 ) \mathrm{ Mod } 2^64}
      
      where \eqn{x_n} is the sequence generated by the 64 bit Linear
      Congruential Generator and \eqn{y_n} is the sequence generated by
      the following prime modulus Multiple Recursive Generator:           

      \deqn{y_n = (107374182 * y_{n-1} + 104480 * y_{n-5}) \mathrm{ Mod } 2147483647}
    
      The same prime modulus generator is used for all the streams.
      Streams differ due to differences in the 64 bit LCG.  The period
      of this generator is around \eqn{2^219}. The number of distinct
      streams available is over \eqn{10^8}.

      The multiplier \eqn{a} for the 64 bit LCG is a parameter to this
      generator.  Allowed \code{para} values are 0 to 2, corresponding
      to 3 prefine multipliers (same as \code{"LCG64"}).
    }
    \item{\code{"MLFG"}}{\emph{Multiplicative Lagged Fibonacci Generator}

     The recurrence relation for this sequence of random numbers is
     given by the following equation:                              
     
     \deqn{x_n = (x_{n-k} * x_{n-l}) \mathrm{ Mod } M}
     
     \eqn{l} and \eqn{k} are called the lags of the generator, and we
     use the convention that \eqn{l > k}.  \eqn{M} is chosen to be
     \eqn{2^64}.
                                                  
     The period of this generator is \eqn{2^61(2^l-1)} where \eqn{l} is
     the lag.  For the default generator with lag \eqn{l = 17}, the
     period is approximately \eqn{2^81}.  The number of distinct streams
     available is \eqn{2^{63(l-1)-1}}.  For the default generator this
     gives around \eqn{2^1008} distinct streams.
     
     The parameters to this generator are the values of the lags.  Allowed
     \code{para} values are:
     \tabular{lrr}{
       \code{para}  \tab l     \tab k   \cr
       0  (default) \tab 17    \tab 5   \cr
       1            \tab 31    \tab 6   \cr
       2            \tab 55    \tab 24  \cr
       3            \tab 63    \tab 31  \cr
       4            \tab 127   \tab 97  \cr
       5            \tab 521   \tab 353 \cr
       6            \tab 521   \tab 168 \cr
       7            \tab 607   \tab 334 \cr
       8            \tab 607   \tab 273 \cr
       9            \tab 1279  \tab 418 \cr
       10           \tab 1279  \tab 861
     }
    }
    \item{\code{"PMLCG"}}{\emph{Prime Modulus Linear Congruential Generator}
      
      This generator is defined by the following relation:

      \eqn{x_n = ( a  x_{n-1} ) \mathrm{ Mod } 2^61-1}
      
      where the multiplier \eqn{a} differs for each stream.  The
      mulitpler is chosen to be certain powers of 37 that give maximal
      period cycles of acceptable quality.
      
      The period of this generator is \eqn{2^61-2}. The number of
      distinct streams available is roughly \eqn{2^58}.

      This generator only accept the default parameter thus 0 is the
      only allowed value for \code{para}.
    }
  }
}
\value{
  Return the a two-element character vector of the RNG and normal kinds
  in use before the call.
}
\note{
  Only one stream is allowed for each process at this point.  Multiple
  streams per process can be achieved by packing the streams into
  strings.
}
\references{
  SPRNG: Scalable Parallel Random Number Generator Library Web Page.
  \url{http://sprng.cs.fsu.edu/}
}
\seealso{
  \code{\link{free.sprng}}, \code{\link{pack.sprng}}
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
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{interface}
\keyword{distribution}
