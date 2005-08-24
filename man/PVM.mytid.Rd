\name{.PVM.mytid}
\alias{.PVM.mytid}
\alias{.PVM.parent}
\alias{.PVM.siblings}
\title{Task IDs}
\description{
  Returns the task id(s) of the current, parent and sibling processes.
}
\usage{
.PVM.mytid()
.PVM.parent ()
.PVM.siblings ()
}
\arguments{
  None.
}
\value{
  Returns positive integer task id(s) of the calling process, the parent
  processor the sibling (started in a single \code{.PVM.spawn} call) processes.
  Return an negative integer if an error occured.
}
\details{
  \code{.PVM.mytid} returns the \code{tid} of the calling process and
  can be called multiple times.  The \code{tid} is a positive integer
  created by local pvmd.
  
  This function enrolls this process into PVM on its first call and
  generate a unique \code{tid} if this process was not created by
  \code{\link{.PVM.spawn}}.  In fact, any PVM system call will enroll a
  task in PVM if the task is not enrolled before the call.

  \code{.PVM.parent} returns tid of the process that spawned the calling
  process. If the calling process was not created with
  \code{\link{.PVM.spawn}}, return 0 (no parent).

  \code{.PVM.siblings} returns the task ids of processes that were
  spawned together in a single spawn call.  The internal list is not
  updated when sibling tasks exit the virtual machine and should be
  treated as a snapshot of the parallel program when it was first
  started.  If a task was not started by \code{.PVM.spawn}, then
  \code{.PVM.siblings} will behave identically to \code{.PVM.mytid}.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.exit}}, \code{\link{.PVM.spawn}}
}
\examples{
## For name(tid) of process and to register with PVM
\dontrun{
mytid      <- .PVM.mytid()
myparent   <- .PVM.parent ()
mysiblings <- .PVM.siblings ()
}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
