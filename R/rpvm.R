## pypvm uses exceptions rather than return values to indicate
## problems.  Should we do the same, or "directly map"?  (i.e. we
## would have to consider "try"...)

## $Id: rpvm.R,v 1.18 2001/09/03 23:03:34 snake Exp $

.PVM.spawnflags <- c (0, 2^(0:5))
names (.PVM.spawnflags) <- c("Default",
                             "Host",
                             "Arch",
                             "Debug",
                             "Trace",
                             "MppFront",
                             "Compl")

.PVM.encoding <- 0:2
names (.PVM.encoding) <- c("Default",
                           "Raw",
                           "InPlace")


.PVM.errors <- c(0, -2, -3, (-5):(-7), -10, -12, (-14):(-33))
names (.PVM.errors) <- c("OK",
                         "BadParam",
                         "Mismatch",
                         "NoData",
                         "NoHost",
                         "NoFile",
                         "NoMem",
                         "BadMsg",
                         "SysErr",
                         "NoBuf",
                         "NoSuchBuf",
                         "NullGroup",
                         "DupGroup",
                         "NoGroup",
                         "NotInGroup",
                         "NoInst",
                         "HostFail",
                         "NoParent",
                         "NotImpl",
                         "DSysErr",
                         "BadVersions",
                         "OutOfRes",
                         "DupHost",
                         "CantStart",
                         "Already",
                         "NoTask",
                         "NoEntry",
                         "DupEntry")

### Process control

.PVM.mytid <- function () {
    .Call ("rpvm_mytid")
}

.PVM.parent <- function () {
    .Call ("rpvm_parent")
}
  
.PVM.exit <- function () {
    info  <- .Call ("rpvm_exit")
    if (info == -14) {             #  PymSysErr
        stop ("pvmd not responding")
    }
    return (invisible (info))
}

.PVM.pstats <- function (tids) {
    pstats <- .Call ("rpvm_pstats", as.integer (tids))
    pstats[pstats == -31] <- 0
    pstats[pstats == 0]   <- 1
    pstats[pstats == -2]  <- -1
    pstats
}

.PVM.spawnR <- function (slave,                       
                         ntask = 1,
                         flag = "Default",
                         where = "",
                         slavedir = NULL,
                         outdir = NULL) {
    .Call ("rpvm_spawn",
           as.character ("slaveR.sh"),
           as.integer   (ntask),
           as.integer   (sum(.PVM.spawnflags[flag])),
           as.character (where),
           as.character (c(slave, slavedir, outdir)))
}

.PVM.spawn <- function (task = "slaveR.sh",                       
                        ntask = 1,
                        flag  = "Default",
                        where = "",
                        arglist = "") {
    ## Return a vector of length = ntask
    ## Had some of the tasks failed to spawn, the last few elements will
    ## be the error codes (< 0).
    .Call ("rpvm_spawn",
           as.character (task),
           as.integer   (ntask),
           as.integer   (sum(.PVM.spawnflags[flag])),
           as.character (where),
           as.character (arglist))
}

.PVM.kill <- function (tid) {
    if (tid <= 0)
        stop ("Invalid task id")        
    info <- .Call ("rpvm_kill", as.integer(tid))
    if (info == -14) {             #  PymSysErr
        stop ("pvmd not responding")
    } else if (info == -2) {
        stop ("invalid tid value.")
    }
    return (invisible (info))
}

.PVM.tasks <- function (where = 0) {
    tasks <- .Call ("rpvm_tasks", as.integer (where))
    names (tasks) <- c("tid", "parent", "host", "status", "name")
    tasks$status[task$status == -31] <- 0
    tasks$status[task$status == 0] <- 1
    tasks$status[task$status == -2] <- -1
    tasks
}

### Virtual machine control

.PVM.start.pvmd <- function (hosts = "", block = T) {
    .Call ("rpvm_start_pvmd", as.character (hosts), as.integer (block))
}

.PVM.halt <- function ()
    .Call ("rpvm_halt")

.PVM.addhosts <- function (hosts) {
    htids <- .Call ("rpvm_addhosts", as.character (hosts))
    htids[htids > 0]
}

.PVM.delhosts <- function (hosts) {
    htids <- .Call ("rpvm_delhosts", as.character (hosts))
    htids[htids > 0]
}

.PVM.mstats <- function (hosts) {
    status <- .Call ("rpvm_mstats", as.character (hosts))
    status[status == .PVM.errors["NoHost"]] <- "NoHost"
    status[status == .PVM.errors["HostFail"]] <- "HostFail"
    status[status == .PVM.errors["OK"]] <- "HostOK"
    status
}

.PVM.config <- function () {
    config <- data.frame (.Call ("rpvm_config"))
    names (config) <- c ("host.id", "name", "arch", "speed")
    config
}   

### Misc

.PVM.tidtohost <- function (tid) {
    if (tid <= 0)
        stop ("Invalid task id!")
    .Call ("rpvm_parent", as.integer (tid))
}

.PVM.notify <- function (msgtag,
                         what = c("ExitTask", "DeleteHost", "AddHost"),
                         par = 0) {
    what.parameters <- 1:3
    names (what.parameters) <- c ("ExitTask", "DeleteHost", "AddHost")
    what <- what.parameters [match.arg (what)]
    .Call ("rpvm_notify", as.integer (what), as.integer (msgtag),
           as.integer (par))
}

PVM.options <- function (what, val) {
    .PVM.Options  <- c("Route", "DebugMask", "AutoErr",
                       "OutputTid", "OutputCode", "TraceTid",
                       "TraceCode", "FragSize", "ResvTids", "SelfOutputTid",
                       "SelfOutputCode", "SelfTraceTid",  "SelfTraceCode",
                       "ShowTids", "PollType", "PollTime")
    what <- pmatch (what, .PVM.Options)
    if (is.na(what) || what < 1 || what > length (.PVM.Options))
        stop ("Invalid option")
    if (missing (val)) {
        .Call ("rpvm_getopt", as.integer (what))
    } else {
        .Call ("rpvm_setopt", as.integer (what), as.integer (val))
    }
}
##

### Message buffers
.PVM.initsend <- function (encode = c("Default", "Raw", "InPlace")) {
    encode <- .PVM.encoding[match.arg (encode)]
    .Call ("rpvm_initsend", as.integer(encode))
}

.PVM.mkbuf <- function (encode = c("Default", "Raw", "InPlace")) {
    encode <- .PVM.encoding[match.arg (encode)]
    .Call ("rpvm_mkbuf", as.integer (encode))
}

.PVM.freebuf <- function (bufid) {
    .Call ("rpvm_freebuf", as.integer (bufid))
}

.PVM.getsbuf <- function () {
    .Call ("rpvm_getsbuf")
}

.PVM.getrbuf <- function () {
    .Call ("rpvm_getsbuf")
}

.PVM.setsbuf <- function (bufid) {
    .Call ("rpvm_setsbuf", as.integer (bufid))
}

.PVM.setrbuf <- function (bufid) {
    .Call ("rpvm_setrbuf", as.integer (bufid))
}
###

### Packing data
.PVM.pkdouble <- function (data = 0.0, stride = 1)
    .Call ("rpvm_pkdouble", as.double(data), as.integer (stride))

.PVM.pkint <- function (data = 0, stride = 1)
    .Call ("rpvm_pkint", as.integer (data), as.integer (stride))

.PVM.pkstr <- function (data = "") {
    if (length (data) > 1) {
        warning ("Only the first element is packed.\n")
        data <- data[1]
    }
    .PVM.pkint (nchar (data[1]) + 10)
    .Call ("rpvm_pkstr", as.character (data))
}

.PVM.pkintvec <- function (data)
    .Call ("rpvm_pkintvec", as.integer (data))

.PVM.pkdblvec <- function (data)
    .Call ("rpvm_pkdblvec", as.double (data))

.PVM.pkstrvec <- function (data) {
    .PVM.pkint (max (nchar (data)) + 10)
    .Call ("rpvm_pkstrvec", as.character (data))
}
.PVM.pkintmat <- function (data) {
    storage.mode (data) <- "integer"
    .Call ("rpvm_pkintmat", as.matrix (data))
}

.PVM.pkdblmat <- function (data) {
    storage.mode (data) <- "double"
    .Call ("rpvm_pkdblmat", as.matrix (data))
}

.PVM.pkstrmat <- function (data) {
    .PVM.pkint (nrow (data))
    .PVM.pkint (ncol (data))
    .PVM.pkstrvec (as.character (data))
}

.PVM.pkfactor <- function (data) {
    if (!is.factor (data))
        stop ("Argument is not a factor")
    value <- as.numeric (data)
    levels <- attributes (data)$levels
    .PVM.pkintvec (as.numeric (data))
    .PVM.pkstrvec (attr (data, "levels"))
    .PVM.pkint (as.integer (is.ordered (data)))
}
###

### Unpacking data
.PVM.upkdouble <- function (nitem  = 1, stride = 1)
    .Call ("rpvm_upkdouble", as.integer (nitem), as.integer (stride))

.PVM.upkint <- function (nitem = 1, stride = 1)
    .Call ("rpvm_upkint", as.integer (nitem), as.integer (stride))

.PVM.upkstr <- function (maxlen) {
    if (missing (maxlen)) {
        maxlen <- .PVM.upkint ()
    }
    .Call ("rpvm_upkstr", as.integer (maxlen))
}

.PVM.upkintvec <- function ()
    .Call ("rpvm_upkintvec")

.PVM.upkdblvec <- function ()
    .Call ("rpvm_upkdblvec")

.PVM.upkstrvec <- function () {
    maxlen <- .PVM.upkint ()
    .Call ("rpvm_upkstrvec", as.integer (maxlen))
}
    
.PVM.upkintmat <- function ()
    .Call ("rpvm_upkintmat")

.PVM.upkdblmat <- function ()
    .Call ("rpvm_upkdblmat")

.PVM.upkstrmat <- function () {
    nr <- .PVM.upkint ()
    nc <- .PVM.upkint ()
    matrix (.PVM.upkstrvec (), nrow = nr, ncol = nc)
}

.PVM.upkfactor <- function () {
    val <- .PVM.upkintvec ()
    lbl <- .PVM.upkstrvec ()    
    ord <- .PVM.upkint ()
    factor (val, labels = lbl, ordered = as.logical (ord))
}
###


###  Sending and receving data
.PVM.send <- function (tid, msgtag)
    .Call ("rpvm_send", as.integer(tid), as.integer(msgtag))

.PVM.mcast <- function (tids, msgtag) {
    if (msgtag < 0) stop ("Message tag must be >= 0!")
    info <- .Call ("rpvm_mcast", as.integer(tids), as.integer(msgtag))
    if (info == .PVM.errors["PvmBadParam"]) {
        ## This error should have been caught
        stop ("Message tag must be >= 0!")
    } else if (info == .PVM.errors["PvmSysErr"]) {
        stop ("pvmd not responding")
    } else if (info == .PVM.errors["PvmNoBuf"]) {
        stop ("no send buffer")
    } else if (info < 0) {
        warning ("Unknown error!")
    }
    return (info)
}

.PVM.recv <- function (tid = -1, msgtag = -1)
    .Call ("rpvm_recv", as.integer (tid), as.integer (msgtag))

.PVM.trecv <- function (tid = -1, msgtag = -1, sec = 0, usec = 0) {
    .Call ("rpvm_trecv", as.integer (tid), as.integer (msgtag),
           as.double (c(sec, usec)))
}

.PVM.nrecv <- function (tid = -1, msgtag = -1)
    .Call ("rpvm_nrecv", as.integer(tid), as.integer(msgtag))

.PVM.probe <- function (tid = -1, msgtag = -1) {
    info <- .Call ("rpvm_probe", as.integer(tid), as.integer(msgtag))
    if (info < 0)
        stop ("Error probing message buffer")
    return (info)
}

.PVM.bufinfo <- function (bufid) {
    bufinfo <- .Call ("rpvm_bufinfo", as.integer(bufid))
    return (bytes = bufinfo[1], msgtag = bufinfo[2], tid = bufinfo[3])
}
    
###

## Parallel apply
## Divide a matrix up by rows and send each submatrix to a spawned process.
## It is assumed the slave script knows what to do with the data and return
## a scalar for each row of the original matrix.

PVM.rapply <- function (X,
                        FUN = mean,
                        NTASK = 1) {
    WORKTAG  <- 22
    RESULTAG <- 33
    
    ## Deal with matrix only first
    if (!is.matrix (X))
        stop ("X must be a matrix!")
    ## Split the matrix into NTASK submatrices, use apply if NTASK = 0
    if (NTASK == 0)
        return (apply (X, 1, FUN))
        
    end   <- nrow(X)
    chunk <- end %/% NTASK + 1
    start <- 1
    
    mytid <- .PVM.mytid()
    ## SLAVE should be the full path name of a R slave script
    children <- .PVM.spawnR (ntask = NTASK, slave = "slapply")
    if (all (children < 0)) {
        cat ("Failed to spawn any task: ", children, "\n")
        .PVM.exit ()
    } else if (any (children < 0)) {
        cat ("Failed to spawn some tasks.  Successfully spawned ",
             sum(children > 0), "tasks\n")
        children <- children[children > 0]
    }
   
    ## Deliver jobs
    for (id in 1:length(children)) {
        .PVM.initsend ()
        range <- c (start,
                    ifelse((start + chunk - 1) > end, end, start + chunk - 1))
        ## Take a submatrix
        work <- X[(range[1]):(range[2]),,drop=F]
        start <- start + chunk

        ## Function name       
        .PVM.pkstr (deparse (substitute (FUN)))
        
        ## Id identifies the order of the job
        .PVM.pkint (id)
        .PVM.pkdblmat (work)
        .PVM.send (children[id], WORKTAG)
        cat ("Work sent to ", children[id], "\n")
    }
    
    ## Receive any outstanding result (vector of doubles) from each child
    partial.results <- list ()
    for (child in children) {
        .PVM.recv (-1, RESULTAG)
        order  <- .PVM.upkint ()
        partial.results[[order]] <- .PVM.upkdblvec ()
    }

    .PVM.exit()
    return (unlist (partial.results))
}
