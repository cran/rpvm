\name{PVM.getinst}
\alias{.PVM.getinst}
\title{Instance number identified by group name and task id}
\description{
  Get the instance numbers identified by a group name and task ids}
}
\usage{
.PVM.getinst (group, tids = .PVM.mytid ())
}
\arguments{
  \item{group}{a character string naming the group}
  \item{tids}{a vector of integer task ids}
}
\value{
  a vector of instance numbers
}
\details{
  \code{.PVM.gettid} returns the corresponding instance number of the
  PVM process identified by the group name \code{group} and task ids
  \code{tids}. It can be called by any task whether in the group or not.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.joingroup}}, \code{\link{.PVM.gettid}}
}
\examples{
gname <- "pvmtest"
\dontrun{myinum <- .PVM.getinst (gname)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
