\name{PVM.gettid}
\alias{.PVM.gettid}
\title{Task id identified by group name and instance number}
\description{
  Get the task id identified by a group name and an instance number}
}
\usage{
.PVM.gettid (group, inum = 0:(.PVM.gsize(group) - 1))
}
\arguments{
  \item{group}{a character string naming the group}
  \item{inum}{a vector of integer instance numbers}
}
\value{
  a vector of task ids.
}
\details{
  \code{.PVM.gettid} returns the tid of the PVM process identified by
  the group name \code{group} and the instance number \code{inum}.
}
\references{
  PVM documentation
}
\note{
  \code{inum} is given a default value corresponding to all instances in
  the group.  It is not valid when the instances numbers are not
  continues (when, for example, some processes already left the group).
}
\seealso{
   \code{\link{.PVM.joingroup}}, \code{\link{.PVM.getinst}}
}
\examples{
gname <- "pvmtest"
leader <- 0
\dontrun{leadtid <- .PVM.gettid (gname, leader)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
