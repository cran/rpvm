\name{.PVM.tidtohost}
\alias{.PVM.tidtohost}
\title{Host id of a task}
\description{
  Returns the host id which the specified task is running.
}
\usage{
.PVM.tidtohost (tid)
}
\arguments{
  \item{tid}{integer task id specified.}
}
\value{
  Positive integer represents the host id. -1 if failed.
}
\details{
}
\references{
  PVM documentation
}
\examples{
## Find out which host this process in running on
\dontrun{myhost <- .PVM.tidtohost (.PVM.mytid())}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
