\name{PVM.reduce}
\alias{.PVM.reduce}
\alias{.PVM.reduce.default}
\alias{.PVM.reduce.integer}
\alias{.PVM.reduce.double}
\title{Reduction}
\description{
  Performs a reduction operation over members of the specified group.  
}
\usage{
.PVM.reduce (x, ...)
.PVM.reduce.default (x, ...)
.PVM.reduce.integer (x, func = "Min", count = length (x), msgtag, group,
rootginst = 0)
.PVM.reduce.double (x, func = "Min", count = length (x), msgtag, group,
 rootginst = 0)
}
\arguments{
  \item{x}{an integer or double vector of length at least \code{count}}
  \item{func}{a character string specifying the name of the operation
    (function), must be one of \code{"Min"}, \code{"Max"}, \code{"Sum"},
    \code{"Product"}}
  \item{count}{the number of elements in \code{x}, must be the same
    among all members of the group}
  \item{msgtag}{an integer message tag supplied by the user.}
  \item{group}{a character string naming the group}
  \item{rootginst}{an integer instance number of group member who
    performs the reduce of its array to the members of the group.}
}
\value{
  On the root, a vector of length \code{count}.
}
\details{
  \code{.PVM.reduce} performs global operations such as max, min, sum,
  or a user provided operation on the data provided by the members of a
  group.  All group members call \code{.PVM.reduce} with the same size
  local data array which may contain one or more entries.  The root task
  is identified by its instance number in the

  When the \code{.PVM.reduce} completes, it returns a vector on the root
  which will be equal to the specified operation applied element-wise to
  the data vectors of all the group members.

  A broadcast by the root can be used if the other members of the group
  need the resultant value(s).

  \code{func} corresponds the four predefined functions in pvm,
  \code{PvmMin}, \code{PvmMax}, \code{PvmSum}, \code{PvmProduct}.  It is
  possible to definer user function in C with the following prototype,
  \code{void func(int *datatype, void *x, void *y, int *num, int
    *info)}.  However, it is not clear how to pass its name from C or how
  to use user function written in R which limits the usefulness of this
  function.
}
\note{
  \code{.PVM.reduce} does not block, a call to
  \code{\link{.PVM.barrier}} may be necessary.  For example, an error
  may occur if a task calls \code{.PVM.reduce} and then leaves the group
  before the root has completed its call to \code{.PVM.reduce}.
  Similarly, an error may occur if a task joins the group after the root
  has issued its call to \code{.PVM.reduce}.  Synchronization of the
  tasks (such as a call to \code{.PVM.barrier}) was not included within
  the \code{.PVM.reduce} implementation since this overhead is
  unnecessary in many user codes (which may already synchronize the
  tasks for other purposes).
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.joingroup}}
}
\examples{
gname <- "pvmtest"
\dontrun{if (myinum == 0) result <- .PVM.reduce (as.double (rnorm (10)),
"Sum", msgtag = 11, group = gname, root = 0)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
