\name{.PVM.spawn}
\alias{.PVM.spawn}
\alias{.PVM.spawnR}
\alias{.PVM.spawnflags}
\title{Spawn child tasks}
\description{
  Starts up \code{ntask} copies of an executable file \code{task} or
  slave R processes on the virtual machine.
}
\usage{
.PVM.spawn (task, ntask = 1, flag  = "Default", where = "",
            arglist = NULL, verbose = FALSE)
.PVM.spawnR (slave, ntask = 1, flag  = "Default", where = "",
             slavedir = "demo", outdir = "/tmp", verbose = FALSE)
.PVM.spawnflags
}
\arguments{
  \item{task}{name of an executable file}
  \item{ntask}{number of tasks to spawn}
  \item{flag}{options flag}
  \item{where}{name of a host or architecture}
  \item{arglist}{a character vectpr to be passed to the spawned task.}
  \item{slave}{name of slave \code{R} script}
  \item{slavedir}{full path to the slave script}
  \item{outdir}{full path to the directory where the output from slave R
    process will be}
  \item{verbose}{if true, print out more information which might be
    helpful in debugging}
}
\value{
  A vector containing \code{tids} of successfully spawned processes (the
  length could be smaller than the number requested) or -1 if there is
  an system error.
}
\details{
  \emph{flag} is a list of strings used to specify options.  Allowed values 
  are
  \tabular{ll}{
    "Default" \tab PVM chooses where to spawn processes \cr
    "Host"    \tab spawn on a particular host (\code{where}) \cr
    "Arch"    \tab spawn on a particular \code{PVM_ARCH} (\code{where}) \cr
    "Debug"   \tab starts tasks under a debugger \cr
    "Trace"   \tab trace data is generated \cr
    "MppFront"    \tab starts tasks on MPP front-end \cr
    "HostCompl"   \tab complements host set in \emph{where} 
  }

  \code{task} is a character string containing the name of the
  executable file name of the PVM process to be started.  It must
  already reside on the host on which it is to be started and can be
  find by local pvmd (possibly by specifying \code{ep=} option when
  the host is added, see \code{\link{.PVM.addhosts}}).  The default
  location PVM looks at is \code{$PVM_ROOT/bin/$PVM_ARCH}.

  To spawn a R process, one can use the script \file{slaveR.sh} which
  has to be included in the search path of pvmd, for example by passing
  \code{ep="$R_LIBS/rpvm"} (the default place this script is installed)
  to \code{.PVM.addhosts}.  Note that here \code{"R_LIBS"} referes to
  the R library on the \emph{slave} node.

  \code{.PVM.spawnR} can be used to conveniently spawn slave \code{R}
  processes.  The paths for the slave script and output file should be
  given as arguments to\code{slavedir} and \code{outdir}.

  The name of slave script must be given as argument \code{slave}.  It
  is crucial that this script is present and can be found on each slave
  node. The input file path has to be relative to \code{system.file
  (package = "rpvm")} on each node, and it has to be the same on all
  nodes.  The default input path is the \code{"demo"} subdirectory
  of \code{rpvm} installed directory.

  The output path can be either relative to the home directory or an
  absolute pathname (the default is \code{"/tmp"}).  The output file
  from slave \code{R} process will have the same name as the slave
  script with process id and \code{.Rout} appended to it.

  \code{.PVM.spawnflags} is a mapping of strings to integer constants
  defined by PVM.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.mytid}}
}
\examples{
\dontrun{
children <- .PVM.spawnR (slave = "helloR.R")
}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
