\name{PVM.barrier}
\alias{.PVM.barrier}
\title{Group synchronization}
\description{
 Blocks the calling process until all processes in a group have called
 it.
}
\usage{
.PVM.barrier (group, count = .PVM.gsize (group))
}
\arguments{
  \item{group}{a character string naming the group}
  \item{count}{the number of group members that must call
    \code{.PVM.barrier} before they are all released, usually the total
    number of members of the specified group.}
}
\value{
  None.
}
\details{
  \code{.PVM.barrier} blocks the calling process until count members of
  the group have called it.  The count argument is required because
  processes could be joining the given group after other processes have
  called \code{.PVM.barrier}.  Thus PVM doesn't know how many group
  members to wait for at any given instant. Although count can be set
  less, it is typically the total number of members of the group.  So
  the logical function of the \code{.PVM.barrier} call is to provide a
  group synchronization.  During any given barrier call all
  participating group members must call barrier with the \emph{same} count
  value.  Once a given barrier has been successfully passed,
  \code{.PVM.barrier} can be called again by the same group using the
  same group name.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.joingroup}}
}
\examples{
gname <- "pvmtest"
\dontrun{myinst <- .PVM.joingroup (gname)}
# do something here ...
\dontrun{.PVM.barrier (gname, 5)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
