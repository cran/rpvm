\name{PVM.options}
\alias{PVM.options}
\title{libpvm Options}
\description{
  Get or set the value of libpvm options.
}
\usage{
PVM.options (what, val)
}
\arguments{
  \item{what}{a character string specifying libpvm options, must be one
    of the following,
    \itemize{
      \item{\code{"Route"}}{Message routing policy}
      \item{\code{"DebugMask"}}{Libpvm debug mask}
      \item{\code{"AutoErr"}}{Auto error reporting}
      \item{\code{"OutputTid"}}{Stdout destination for children}
      \item{\code{"OutputCode"}}{Output message tag}
      \item{\code{"TraceTid"}}{Trace data destination for children}
      \item{\code{"TraceCode"}}{Trace message tag}
      \item{\code{"FragSize"}}{Message fragment size}
      \item{\code{"ResvTids"}}{Allow messages to reserved tags and TIDs}
      \item{\code{"SelfOutputTid"}}{Stdout destination}
      \item{\code{"SelfOutputCode"}}{Output message tag}
      \item{\code{"PvmSelfTraceTid"}}{Trace data destination}
      \item{\code{"SelfTraceCode"}}{Trace message tag}
      \item{\code{"ShowTids"}}{\code{pvm_catchout} prints task ids with output}
      \item{\code{"PollType"}}{Message wait policy (shared memory)}
      \item{\code{"PollTime"}}{Message spinwait duration}
    }
  }
  \item{val}{integer value of the option}
}
\value{
  Returns the current option value.  If \code{val} is present, set the
  corresonding option to new value \code{val}.
}
\details{
  \code{.PVM.options} sets or queries miscellaneous options in the PVM
  library.  The options are,
  \enumerate{    
    \item{\code{"Route"}}{
      Possible values are:
      \itemize{
        \item{1}{"RouteDirect", set up direct task-to-task links
          (using TCP) for all subsequent  communication.}
        \item{2}{"DontRouteDirect", communication through PVM daemon.}
        \item{3}{"AllowDirect" (the default), this setting on task A
        allows other tasks to set up direct links to A.}
      }
    }
    \item{\code{"DebugMask"}}{
      When debugging is turned on, PVM will log detailed information
      about its operations and progress on its stderr stream.  The value
      is the debugging level.  Default is 0, not to print any debug
      information.
    }
    \item{\code{"AutoErr"}}{
      When an error results from a libpvm function call and
      \code{"AutoErr"} is set to 1 (the default), an error message is
      automatically printed on stderr.  A set ting of 0 disables this.
      A setting of 2 causes the library to terminate the task by calling
      exit() after printing the error message.  A setting of 3 causes
      the library to abort after printing the error message.
    }
    \item{\code{"OutputTid"}}{
      The stdout destination for children tasks (spawned after the
      the option is set).  Everything printed on the standard output of
      tasks spawned by the calling task is packed into messages and sent
      to the destination.  \code{val} is the TID of a PVM task.  Setting
      PvmOutputTid to 0 redirects stdout to the master pvmd, which
      writes to the log file /tmp/pvml.<uid> The default setting is
      inherited from the parent task, else is 0.
    }
    \item{\code{"OutputCode"}}{
       The message tag for standard output messages.  Should only
       be set when a task has \code{"OutputTid"} set to itself.
    }
    \item{\code{"TraceTid"}}{
      The trace data message destination for chilren tasks (spawned
      after the option is set).  Libpvm trace data is sent as messages
      to the destination.  \code{val} is the TID of a PVM task.  Setting
      \code{"TraceTid"} to 0 discards trace data.  The default setting
      is inherited from the parent task, else is 0.
    }
    \item{\code{"TraceCode"}}{     
      The message tag for trace data messages.  Should only be set when
      a task has \code{"TraceTid"} set to itself.
    }
    \item{\code{"FragSize"}}{    
      Specifies the message fragment size in bytes.  Default value
      varies with host architecture.
    }
    \item{\code{"ResvTids"}}{
      A value of 1 enables the task to send messages with reserved tags
      and to non-task destinations.  The default (0) causes libpvm to
      generate a \code{"BadParam"} error when a reserved identifier is
      specified.
    }
    \item{\code{"SelfOutputTid"}}{
      Sets the \code{stdout} destination for the task.  Every thing
      printed on \code{stdout} is packed into messages and sent to the
      destination.  Note: this only works for spawned tasks, because the
      pvmd doesn't get the output from tasks started by other means.
      The value is the TID of a PVM task.  Setting
      \code{"SelfOutputTid"} to 0 redirects stdout to the master pvmd,
      which writes to the log file /tmp/pvml.<uid>.  The default setting
      is inherited from the parent task, else is 0.  Setting either
      \code{"SelfOutputTid"} or \code{"SelfOutputCode"} also causes both
      \code{"OutputTid"} and \code{"OutputCode"} to take on the values
      of \code{"SelfOutputTid"} and \code{"SelfOutputCode"},
      respectively.
    }
    \item{\code{"SelfOutputCode"}}{
      The message tag for standard output messages.
    }
    \item{\code{"PvmSelfTraceTid"}}{
     The trace data message destination for the task.  Libpvm trace data
     is sent as messages to the destination.  The value is the TID of a
     PVM task.  Set ting \code{"SelfTraceTid"} to 0 discards trace data.
     The default setting is inherited from the parent task, else is 0.
     Setting either \code{"SelfTraceTid"} or \code{"SelfTraceCode"} also
     causes both \code{"TraceTid"} and \code{"TraceCode"} to take on the
     values of \code{"SelfTraceTid"} and \code{"SelfTraceCode"},
     respectively.
    }
    \item{\code{"SelfTraceCode"}}{
      The message tag for trace data messages.
    }
    \item{\code{"ShowTids"}}{
      If true (nonzero), \code{pvm_catchout} (note: not supported by
      rpvm) tags each line of output printed by a child task with the
      task id.  Otherwise, output is exactly as printed.
    }
    \item{\code{"PollType"}}{
      The message wait policy when using shared-memory message
      transport.  Setting \code{"PollType"} to \code{"PollConstant"}
      causes the application to spin on its message queue waiting for a
      message.  Setting \code{"PollType"} to \code{"PollSleep"} causes
      the application to poll the message queue for messages
      \code{"PollTime"} times before pending on the semaphore.
    }
    \item{\code{"PollTime"}}{
      The poll count for applications checking their message queue
      before they pend on the semaphore.  This option is ignored if
      \code{"PollType"} is set to \code{"PollConstant"}.
    }
  }
} 
\references{
  PVM documentation
}
\examples{
\dontrun{PVM.options (what = "DebugMask")}
}
\author{
  Na (Michael) Li \email{nali@umn.edu} and
  A.J. Rossini \email{rossini@u.washington.edu}
}
\keyword{utilities}
\keyword{connection}
\keyword{interface}
