\name{PVM.scatter}
\alias{.PVM.scatter}
\title{Scatter a vector across the group}
\description{
  Sends to each member of a group a section of a vector from a specified
  member of the group (the root).
}
\usage{
.PVM.scatter (x, count, msgtag, group, rootginst = 0)
}
\arguments{
  \item{x}{an integer or double vector on the root which are to be
    distributed to the members of the group.  If \code{n} is the number
    of members in the group, then this vector should be of length at
    least \eqn{n \times \mathrm{count}}{n * count}.  This argument is
    meaningful only on the root.}
  \item{count}{an integer specifying the number of elements to be sent
    to each member of the group from the root.}
  \item{msgtag}{an integer message tag supplied by the user.}
  \item{group}{a character string naming the group}
  \item{rootginst}{an integer instance number of group member who
    performs the scatter of its array to the members of the group.}
}
\value{
  a vector of length \code{count}
}
\details{
  \code{.PVM.scatter} performs a scatter of data from the specified
  root member of the group to each of the members of the group,
  including itself.  All group members must call \code{.PVM.scatter},
  each receives a portion of the data array from the root in their
  local result array.  It is as if the root node sends to the
  \emph{i}th member of the group count elements from its array data
  starting at offset \eqn{i \times \mathrm{count}}{i * count} from the
  beginning of the data array.  And, it is as if, each member of the
  group performs a corresponding receive of count values of datatype
  into its result array.  The root task is identified by its instance
  number in the group.
  
  \code{x} has to be a vector of storage mode integer or double.  The
  \code{.PVM.scatter} function just calls \code{stop ()}.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.joingroup}}, \code{\link{.PVM.gather}},
  \code{\link{.PVM.reduce}}, \code{\link{.PVM.bcast}}
}
\examples{
gname <- "pvmtest"
\dontrun{
myrow <- .PVM.scatter(as.integer (1:100), 10, msgtag = 11, group =
                      gname, root = 0)
}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
