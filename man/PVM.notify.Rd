\name{.PVM.notify}
\alias{.PVM.notify}
\title{Monitor pvmd}
\description{
  Request notification of PVM event such as host failure.
}
\usage{
.PVM.notify (msgtag, what = c("ExitTask", "DeleteHost", "AddHost"),
             par = 0)
}
\arguments{
  \item{msgtag}{message tag to be used in notification}
  \item{what}{a character string specifying the type of event to trigger
    the notification, must be one of \code{"ExitTask"} (default),
    \code{"DeleteHost"}, or \code{"AddHost"}.}
  \item{par}{additional parameter, if \code{what} = \code{"AddHost"},
    specify the number of times to notify, if \code{what} =
    \code{"DeleteHost"} or \code{"ExitTask"}, specify a vector of host
    id or task id.}
}
\value{
  Status code returned by the routine.  -1 if there was an error.
}
\details{
  The routine \code{.PVM.notify} requests PVM to notify the caller on
  detecting certain events.  One or more notify messages (see below) are
  sent by PVM back to the calling task.  The messages have tag
  \code{msgtag} supplied to notify.
  
  The notification messages have the following format:
  \itemize{
    \item{\code{"ExitTask"}}{One notify message for each task id requested.
      The message body contains a single integer task id of exited task.}
    \item{\code{"DeleteHost"}}{One notify message for each host id
      requested. The message  body  contains a single integer host
      (pvmd) id of exited pvmd.}
    \item{\code{"AddHost"}}{\code{par} notify messages are sent, one
      each time the local pvmd's host table is updated. The message is a
      vector of ids of new pvmds and can be unpacked by
      \code{\link{.PVM.upkintvec}}.  The  counter of \code{"AddHost"}
      messages yet to be sent is  replaced  by successive calls to
      \code{.PVM.notify}.  Specifying \code{par} = -1 turns on
      \code{"AddHost"} messages until a future notify; 0 disables them.}
    
    The calling task is responsible for receiving messages with the
    specified tag and taking appropriate action.  This feature makes PVM
    fault tolerant.
  }
}
\references{
  PVM documentation
}
\seealso{
  \link{PVMD}
}
\examples{
\dontrun{.PVM.notify (msgtag = 999, what = "Exit", tids)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
