###
### File: helloR.R
### Author: Na Li <nali@umn.edu>
### Created: 2004/03/12 19:24:20
### 
### $Id: helloR.R,v 1.3 2004/05/25 22:18:29 nali Exp $
###
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

myname <- paste (Sys.info ()["nodename"], "\n",
                 Sys.info ()["machine"], "running", Sys.info ()["sysname"],
                 Sys.info ()["release"])

if (pvmrun) {
    ## Register the master into PVM
    myparent <- .PVM.parent ()
    if (myparent == 0) {
        ## This is master
        cat ("I am the master", .PVM.mytid (), " from ", myname, "\n")
        slaveR <- "helloR.R"
        try (children <- .PVM.spawnR (slaveR, ntask = 1, verbose = TRUE,
                                      flag = "Trace"))
        if (inherits (.Last.value, "try-error")) {
            warning ("Falied to spawn slave R script")
        } else {
            bufid <- .PVM.recv ()
            bufinfo <- .PVM.bufinfo (bufid)
            msg <- .PVM.unserialize ()
            cat (msg, "\n")
        }
    } else {
        ## This is slave
        msg <- paste ("Task", .PVM.mytid (), "said 'Hello' from", myname)
        .PVM.initsend ()
        .PVM.serialize (msg)
        .PVM.send (myparent, 1)
    }
} else {
    warning ("PVMD is not running. Do nothing.")
}
