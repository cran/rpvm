\name{PVM.group}
\alias{.PVM.joingroup}
\alias{.PVM.lvgroup}
\title{Join or leave a names group}
\description{
  Enrolls or unenrolls the calling process to a named group.
}
\usage{
.PVM.joingroup (group)
.PVM.lvgroup (group)
}
\arguments{
  \item{group}{a character string naming the group}
}
\value{
  \code{.PVM.joingroup} returns the instance number (rank) of the the
  process in the group.

  \code{.PVM.lvgroup} returns no value.
}
\details{
  \code{.PVM.joingroup} enrolls the calling task in the group named
  \code{group} and returns the instance number (\code{inum}) this task
  in this group.

  Instance numbers start at 0 and count up.  When using groups a
  (\code{group}, \code{inum}) pair uniquely identifies a PVM process.
  If a task leaves a group by calling \code{.PVM.lvgroup} and later
  rejoins the same group, the task is not guaranteed to get the same
  instance number.  PVM attempts to reuse old instance numbers, so when
  a task joins a group it will get the lowest available instance number.
  A task can be a member of multiple groups simultaneously.

  \code{.PVM.lvgroup} unenrolls the calling task from the group named
  \code{group}.
}
\references{
  PVM documentation
}
\note{
  If the process fails to join a group with the following message,
  \code{
    libpvm [t40002]: gs_getgstid() failed to start group server: No such  file
  }
  It means pvmd cannot find the executable \code{pvmgs}, put its path to
  the \code{ep=} option of the host file.
}
\seealso{
}
\examples{
gname <- "pvmtest"
\dontrun{myinum <- .PVM.joingroup (gname)}
\dontrun{.PVM.lvgroup (gname)}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
