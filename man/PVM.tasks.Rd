\name{.PVM.tasks}
\alias{.PVM.tasks}
\title{Tasks information}
\description{
  Returns information about the tasks running on the virtual machine
}
\usage{
.PVM.tasks (where = 0)
}
\arguments{
  \item{where}{integer specifying what tasks to return information
    about. The options are:
    \itemize{
      \item{0}{for all the tasks on the virtual machine.}
      \item{host id}{for all tasks on a given host.}
      \item{tid}{for a specific task.}
    }
  }
}
\value{
  A data frame.  One row for each task.  Names of the columns are,
  \itemize{
    \item{tid}{task id}
    \item{parent}{parent task id}
    \item{host}{host id}
    \item{status}{task running status (see \code{\link{.PVM.pstats}}).}
    \item{name}{string, name of the host}
  }
}
\details{
 \code{.PVM.tasks} returns information about tasks presently running on
 the virtual machine.  The information returned is the same as that
 available from the pvm console command \code{ps}.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.pstats}}, \code{\link{.PVM.mstats}},
  \code{\link{.PVM.config}}, \code{\link{.PVM.tidtohost}}
}
\examples{
\dontrun{.PVM.tasks ()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
