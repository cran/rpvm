\name{PVM.rapply}
\alias{PVM.rapply}
\title{Parallel apply}
\description{
  Apply a function to the rows of a matrix in parallel, via PVM.
}
\usage{
PVM.rapply (X, FUN = mean, NTASK = 1)
}
\arguments{
  \item{X}{a numeric matrix}
  \item{FUN}{function to be applied on each row}
  \item{NTASK}{number of tasks to use}
}
\value{
  A vector with the same values as \code{apply (X, 1, FUN)}.
}
\details{
  This function requires the function \code{FUN} can be correctly
  evaluated in each spawned R process. 
}
\examples{
a <- matrix(rnorm(100), nrow = 10)
PVM.rapply (a, sum, 3)
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
