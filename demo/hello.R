
## $Id: hello.R,v 1.6 2004/03/12 23:09:44 nali Exp $

require (rpvm)

### Start pvmd if necessary

if (inherits (try (.PVM.config (), silent = TRUE), "try-error")) {
    # PVM is not started
    cat ("pvmd is not running. Try to start pvmd...\n")
    try (.PVM.start.pvmd ())
    if (inherits (.Last.value, "try-error")) {
        pvmrun <- FALSE
    } else {
        pvmrun <- TRUE
    }
} else {
    pvmrun <- TRUE
}

mytid <- .PVM.mytid()
cat ("I am the master", mytid, " from ", Sys.getenv("HOSTNAME"), "\n")
children <- .PVM.spawn ("hello_other", 1)
if (length (children[children > 0]) == 0) {
                                        # spawn failed
    cat ("Failed to spawn task.\n")
} else {
                                        # any message from any task
    bufid <- .PVM.recv ()
    bufinfo <- .PVM.bufinfo (bufid)
    msg <- .PVM.upkstr (100)
    cat ("Task ", bufinfo$tid, "said: ", msg, "\n")
}
.PVM.exit()
