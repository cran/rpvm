## pypvm uses exceptions rather than return values to indicate
## problems.  Should we do the same, or "directly map"?  (i.e. we
## would have to consider "try"...)

## $Id: rpvm.R,v 1.29 2002/03/18 23:48:13 snake Exp $

.PVM.encoding <- 0:2
names (.PVM.encoding) <- c("Default",
                           "Raw",
                           "InPlace")


### Process control

.PVM.mytid <- function () {
    .Call ("rpvm_mytid")
}

.PVM.parent <- function () {
    .Call ("rpvm_parent")
}
  
.PVM.exit <- function () {
    info  <- .Call ("rpvm_exit")
    return (invisible (info))
}

.PVM.pstats <- function (tids) {
    .Call ("rpvm_pstats", as.integer (tids))
}

.PVM.spawnR <- function (slave,                       
                         ntask = 1,
                         flag =  "Default",
                         where = "",
                         slavedir = getwd (),
                         outdir = getwd ()) {
    cat ("\n")
    cat ("Try to spawn tasks...\n")
    .PVM.spawn (ntask = ntask,
                flag = flag,
                where = where,
                arglist = c(slave, slavedir, outdir))
}

.PVM.spawn <- function (task = system.file ("slaveR.sh", package = "rpvm"), 
                        ntask = 1,
                        flag  = "Default",
                        where = "",
                        arglist = NULL) {    
    .PVM.spawnflags <- c (0, 2^(0:5))
    names (.PVM.spawnflags) <- c("Default", "Host", "Arch",
                                 "Debug", "Trace", "MppFront", "Compl")
    tids <- .Call ("rpvm_spawn",
                   as.character (task),
                   as.integer   (ntask),
                   as.integer   (sum(.PVM.spawnflags[flag])),
                   as.character (where),
                   as.character (arglist))
    return (tids[tids > 0])
}

.PVM.kill <- function (tids) {
    .Call ("rpvm_kill", as.integer(tids))
}

.PVM.tasks <- function (where = 0) {
    tasks <- .Call ("rpvm_tasks", as.integer (where))
    names (tasks) <- c("tid", "parent", "host", "status", "name")
    return (tasks)
}

### Virtual machine control

.PVM.start.pvmd <- function (hosts = "", block = T) {
    info <- .Call ("rpvm_start_pvmd", as.character (hosts),
                   as.integer (block))
    return (invisible (info))
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
    .Call ("rpvm_mstats", as.character (hosts))    
}

.PVM.config <- function () {
    config <- data.frame (.Call ("rpvm_config"))
    if (!is.na (config)) {
        names (config) <- c ("host.id", "name", "arch", "speed")
    } else {
        names (config) <- NULL
    }
    config
}   

### Misc

.PVM.tidtohost <- function (tid) {
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
    if (is.na(what) || what < 1 || what > length (.PVM.Options)) {
        stop ("Invalid option")
    }
    if (missing (val)) {
        .Call ("rpvm_getopt", as.integer (what))
    } else {
        .Call ("rpvm_setopt", as.integer (what), as.integer (val))
    }
}
##

### Group library
.PVM.joingroup <- function (group) {
    .Call ("rpvm_joingroup", as.character (group))
}

.PVM.lvgroup <- function (group) {
    info <- .Call ("rpvm_lvgroup", as.character (group))
    return (invisible (info))  
}

.PVM.gettid <- function (group, inum = 0:(.PVM.gsize(group) - 1)) {
    .Call ("rpvm_gettid", as.character (group), as.integer (inum))
}

.PVM.getinst <- function (group, tids = .PVM.mytid ()) {
    .Call ("rpvm_getinst", as.character (group), as.integer (tids))
}

.PVM.gsize <- function (group) {
    .Call ("rpvm_gsize", as.character (group))
}

.PVM.barrier <- function (group, count = .PVM.gsize (group)) {
    info <- .Call ("rpvm_barrier", as.character (group), as.integer (count))
    return (invisible (info))
}

.PVM.bcast <- function (group, msgtag) {
    info <- .Call ("rpvm_bcast", as.character (group), as.integer (msgtag))
    return (invisible (info))
}

.PVM.scatter <- function (x, ...) {
    if (is.null (class (x))) {
        if (is.integer (x)) {
            class (x) <- "integer"
        } else if (is.double (data)) {
            class (x) <- "double"
        } 
    }
    UseMethod (".PVM.scatter", x, ...)
}

.PVM.scatter.default <- function (x, ...) {
    stop ("Unsupported class.  Must be integer or double vectors.")
}

.PVM.scatter.integer <- function (x, count, msgtag, group, rootginst = 0) {
    .Call ("rpvm_scatter_integer", as.integer (x), as.integer (count),
           as.integer (msgtag), as.character (group), as.integer (rootginst))
}

.PVM.scatter.double <- function (x, count, msgtag, group, rootginst = 0) {
    .Call ("rpvm_scatter_double", as.double (x), as.integer (count),
           as.integer (msgtag), as.character (group), as.integer (rootginst))
}

.PVM.gather <- function (x, ...) {
    if (is.null (class (x))) {
        if (is.integer (x)) {
            class (x) <- "integer"
        } else if (is.double (data)) {
            class (x) <- "double"
        } 
    }
    UseMethod (".PVM.gather", x, ...)
}

.PVM.gather.default <- function (x, ...) {
    stop ("Unsupported class.  Must be integer or double vectors.")
}

.PVM.gather.integer <- function (x, count = length (x),
                                 msgtag, group, rootginst = 0) {
    .Call ("rpvm_gather_integer", as.integer (x), as.integer (count),
           as.integer (msgtag), as.character (group), as.integer (rootginst))
}

.PVM.gather.double <- function (x, count = length (x),
                                msgtag, group, rootginst = 0) {
    .Call ("rpvm_gather_double", as.double (x), as.integer (count),
           as.integer (msgtag), as.character (group), as.integer (rootginst))
}

.PVM.reduce <- function (x, ...) {
    if (is.null (class (x))) {
        if (is.integer (x)) {
            class (x) <- "integer"
        } else if (is.double (data)) {
            class (x) <- "double"
        } 
    }
    UseMethod (".PVM.reduce", x, ...)
}

.PVM.reduce.default <- function (x, ...) {
    stop ("Unsupported class.  Must be integer or double vectors.")
}

.PVM.reduce.integer <- function (x, func = "Min", count = length (x),
                                 msgtag, group, rootginst = 0) {
    funcidx <- pmatch (func, c("Min", "Max", "Sum", "Product"))
    if (is.na (funcidx)) {
        stop ("Invalid arguments for func.")
    }
    .Call ("rpvm_reduce_integer", as.integer (x), as.integer (funcidx),
           as.integer (count), as.integer (msgtag),
           as.character (group), as.integer (rootginst))
}

.PVM.reduce.double <- function (x, func = "Min", count = length (x),
                                 msgtag, group, rootginst = 0) {
    funcidx <- pmatch (func, c("Min", "Max", "Sum", "Product"))
    if (is.na (funcidx)) {
        stop ("Invalid arguments for func.")
    }
    .Call ("rpvm_reduce_double", as.double (x), as.integer (funcidx),
           as.integer (count), as.integer (msgtag),
           as.character (group), as.integer (rootginst))
}

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
    info <- .Call ("rpvm_freebuf", as.integer (bufid))
    return (invisible (info))
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
.PVM.pkdouble <- function (data = 0.0, stride = 1) {
    info <- .Call ("rpvm_pkdouble", as.double (data), as.integer (stride))
    return (invisible (info))
}

.PVM.pkint <- function (data = 0, stride = 1) {
    info <- .Call ("rpvm_pkint", as.integer (data), as.integer (stride))
    return (invisible (info))
}

.PVM.pkstr <- function (data = "") {
    if (length (data) > 1) {
        warning ("Only the first element is packed.\n")
        data <- data[1]
    }
    info <- .Call ("rpvm_pkstr", as.character (data))
    return (invisible (info))
}

.PVM.pkintvec <- function (data) {
    info <- .Call ("rpvm_pkintvec", as.integer (data))
    return (invisible (info))
}

.PVM.pkdblvec <- function (data) {
    info <- .Call ("rpvm_pkdblvec", as.double (data))
    return (invisible (info))
}

.PVM.pkstrvec <- function (data) {
    .PVM.pkint (max (nchar (data)) + 10)
    .Call ("rpvm_pkstrvec", as.character (data))
}
.PVM.pkintmat <- function (data) {
    storage.mode (data) <- "integer"
    info <- .Call ("rpvm_pkintmat", as.matrix (data))
    return (invisible (info))
}

.PVM.pkdblmat <- function (data) {
    storage.mode (data) <- "double"
    info <- .Call ("rpvm_pkdblmat", as.matrix (data))
    return (invisible (info))
}

.PVM.pkstrmat <- function (data) {
    info <- .PVM.pkint (nrow (data))
    info <- .PVM.pkint (ncol (data))
    info <- .PVM.pkstrvec (as.character (data))
    return (invisible (info))
}

.PVM.pkfactor <- function (data) {
    if (!is.factor (data))
        stop ("Argument is not a factor")
    value <- as.numeric (data)
    levels <- attributes (data)$levels
    info <- .PVM.pkintvec (as.numeric (data))
    info <- .PVM.pkstrvec (attr (data, "levels"))
    info <- .PVM.pkint (as.integer (is.ordered (data)))
    return (invisible (info))
}
###

### Unpacking data
.PVM.upkdouble <- function (nitem  = 1, stride = 1)
    .Call ("rpvm_upkdouble", as.integer (nitem), as.integer (stride))

.PVM.upkint <- function (nitem = 1, stride = 1)
    .Call ("rpvm_upkint", as.integer (nitem), as.integer (stride))

.PVM.upkstr <- function (maxlen) {
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
    return (invisible (info))
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
    .Call ("rpvm_probe", as.integer(tid), as.integer(msgtag))    
}

.PVM.bufinfo <- function (bufid) {
    bufinfo <- .Call ("rpvm_bufinfo", as.integer(bufid))
    if (bufinfo == -1) {
        return (-1);
    } else {
        return (bytes = bufinfo[1], msgtag = bufinfo[2], tid = bufinfo[3])
    }
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
    if (!is.matrix (X)) {
        stop ("X must be a matrix!")
    }
    ## Split the matrix into NTASK submatrices, use apply if NTASK = 0
    if (NTASK == 0) {
        return (apply (X, 1, FUN))
    }
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
