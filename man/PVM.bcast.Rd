\name{PVM.bcast}
\alias{.PVM.bcast}
\title{Broadcasting  the  data}
\description{
  Broadcasts the data in the active message buffer to a group of
  processes.
}
\usage{
.PVM.bcast (group, msgtag)
}
\arguments{
  \item{group}{a character string naming the group}
  \item{msgtag}{an integer identifying the message}
}
\value{
  None.
}
\details{
   \code{.PVM.bcast} broadcasts a message stored in the active send
   buffer to all the members of \code{group}.  The broadcast message is
   not sent back to the sender.  Any PVM task can call
   \code{.PVM.bcast}, it need not be a member of the group.  The content
   of the message can be distinguished by \code{msgtag}.  At the other
   end, \code{\link{.PVM.recv}} or other receiving functions can be used
   to receive the message, just as the ones sent by
   \code{\link{.PVM.send}}.

   \code{.PVM.bcast} is asynchronous.  Computation on the sending
   processor resumes as soon as the message is safely on its way to the
   receiving processors.  This is in contrast to synchronous
   communication, during which computation on the sending processor
   halts until a matching receive is exe cuted by all the receiving
   processors.

   \code{.PVM.bcast} first determines the tids of the group members by
   checking a group data base.  A multicast is performed to these tids.
   If the group is changed during a broadcast the change will not be
   reflected in the broadcast.  Multicasting is not supported by most
   multiprocessor vendors.  Typically their native calls only support
   broadcasting to all the user's processes on a multiprocessor. Because
   of this omission, \code{.PVM.bcast} may not be an efficient
   communication method on some multiprocessors.
}
\references{
  PVM documentation
}
\seealso{
  \code{\link{.PVM.joingroup}}
}
\examples{
gname <- "pvmtest"
\dontrun{.PVM.initsend ()}
\dontrun{.PVM.pkintvec (1:10)}
\dontrun{.PVM.bcast(gname, msgtag = 11)}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
