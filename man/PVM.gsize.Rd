\name{PVM.gsize}
\alias{.PVM.gsize}
\title{Get the size of the group}
\description{
  Get the number of processes currently in the group
}
\usage{
.PVM.gsize (group)
}
\arguments{
  \item{group}{a character string naming the group}
}
\value{
  Returns the number of processes currently in the group.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.joingroup}}, \code{\link{.PVM.lvgroup}}
}
\examples{
gname <- "pvmtest"
\dontrun{size <- .PVM.gsize(gname)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
