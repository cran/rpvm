\name{PVMD}
\alias{PVMD}
\alias{.PVM.start.pvmd}
\alias{.PVM.addhosts}
\alias{.PVM.delhosts}
\alias{.PVM.halt}
\title{Vitural Machine Control}
\description{
  Start new pvm daemon, add or delete hosts and stop pvmd.
}
\usage{
.PVM.start.pvmd (hosts = "", block = T)
.PVM.addhosts (hosts)
.PVM.delhosts (hosts)
.PVM.halt ()
}
\arguments{
  \item{hosts}{a character vector, arguments to pvmd}
  \item{block}{logical, if true, block until startup of all hosts complete}
}
\value{
  \code{.PVM.start.pvmd} and \code{.PVM.halt} returns 0 if
  successful or -1 if failed.

  \code{.PVM.addhosts} returns a vector of host ids that has been
  successfully added to the virtual machine.
  
  \code{.PVM.delhosts} returns a vector of status codes for each
  hosts successfulled removed from the virtual machine.
}
\details{
  \code{.PVM.start.pvmd} starts a \emph{pvmd3} process, the master of a
  new virtual machine.  It returns as soon as the pvmd is started and
  ready for work.  If the \code{block} parameter is nonzero and a
  hostfile is passed to the pvmd as a parameter, it returns when all
  hosts marked to start have been added.

  \code{.PVM.addhosts} takes a vector of host names and add them to the
  virtual machine.  The names should have the same syntax as lines of a
  pvmd hostfile (see man page for pvmd3): A hostname followed by options
  of the form xx=y.

  The status of hosts can be requested by the application using
  \code{\link{.PVM.mstats}} and \code{\link{.PVM.config}}.  If a host
  fails it will be automatically deleted from the configuration.  Using
  \code{\link{.PVM.addhosts}} a replacement host can be added by the
  application, however it is the responsibility of the application
  developer to make his application tolerant of host failure.  Another
  use of this feature would be to add more hosts as they become
  available, for example on a weekend, or if the application dynamically
  determines it could use more computational power.

  \code{.PVM.delhosts} deletes the computers pointed to in \code{hosts}
  from the existing configuration of computers making up the virtual
  machine.  All PVM processes and the pvmd running on these computers
  are killed as the computer is deleted.

  If a host fails, the PVM system will continue to function and will
  automatically delete this host from the virtual machine.  An
  application can be notified of a host failure by calling
  \code{\link{.PVM.notify}}.  It is still the responsibility of the
  application developer to make his application tolerant of host
  failure.

  \code{.PVM.halt} shuts down the the entire PVM system including remote
  tasks, remote pvmds, the local tasks (including the calling task) and
  the local pvmd.  Note, when I did this, it also kills the current R
  session.
}
\references{
  PVM documentation
}
\examples{
# start a new virtual machine on local machine 
\dontrun{.PVM.start.pvmd ()}
# add more hosts to it
\dontrun{.PVM.addhosts ("sparky",
               "thud.cs.utk.edu  ep=$R_LIBS/rpvm/:$PVM_ROOT/bin/$PVM_ARCH")}
# do some work ...
# finished with one machine
\dontrun{.PVM.delhosts ("thud.cs.utk.edu")}
# do some other work ...
# finished with pvm
\dontrun{.PVM.halt ()}
}
\author{
  Na (Michael) Li \email{lina@u.washington.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
