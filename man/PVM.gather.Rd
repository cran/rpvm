\name{PVM.gather}
\alias{.PVM.gather}
\title{Gather the data into root}
\description{
  A specified member of the group (the root) receives mes sages from
  each member of the group and gathers these messages into a single
  array.
}
\usage{
.PVM.gather (x, count = length (x), msgtag, group, rootginst = 0)
}
\arguments{
  \item{x}{an integer or double vector of length at least \code{count}}
  \item{count}{the number of elements to be sent to the root}
  \item{msgtag}{an integer message tag supplied by the user.}
  \item{group}{a character string naming the group}
  \item{rootginst}{an integer instance number of group member who
    performs the gather of its array to the members of the group.}
}
\value{
  On the root, a vector combining all the \code{x}'s.
}
\details{
   \code{.PVM.gather} performs a send of messages from each member of
   the group to the specified root member of the group.  All group
   members must call \code{.PVM.gather}, each sends its vector data of
   to the root which accumulates these messages into a single vector.
   The root task is identified by its instance number in the group.

   \code{x} has to be a vector of storage mode integer or double.  The
   \code{.PVM.gather.default} function just calls \code{stop ()}.

   \code{.PVM.gather} does not block.  If a task calls
   \code{.PVM.gather} and then leaves the group before the root has
   called \code{.PVM.gather} an error may occur.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.joingroup}}, \code{\link{.PVM.reduce}},
  \code{\link{.PVM.scatter}}
}
\examples{
gname <- "pvmtest"
\dontrun{
if (myinum == 0) {
   result <- .PVM.gather(as.integer (1:100),
                         msgtag = 11, group = gname, root = 0)
}
}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
