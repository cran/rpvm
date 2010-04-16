# Emacs: -*- r -*- vim: ft=r 
## pypvm uses exceptions rather than return values to indicate
## problems.  Should we do the same, or "directly map"?  (i.e. we
## would have to consider "try"...)

## $Id: rpvm.R,v 1.42 2004/05/25 22:18:29 nali Exp $

.PVM.encoding <- 0:2
names (.PVM.encoding) <- c("Default",
                           "Raw",
                           "InPlace")


### Process control

.PVM.mytid <- function () {
    .Call ("rpvm_mytid", PACKAGE = "rpvm")
}

.PVM.parent <- function () {
    .Call ("rpvm_parent", PACKAGE = "rpvm")
}
  
.PVM.exit <- function () {
    info  <- .Call ("rpvm_exit", PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.pstats <- function (tids) {
    .Call ("rpvm_pstats", as.integer (tids), PACKAGE = "rpvm")
}

.PVM.spawnR <- function (slave,                       
                         ntask = 1,
                         flag =  "Default",
                         where = "",
                         slavedir = "demo",
                         outdir = "/tmp",
                         verbose = FALSE) {
    cat ("\n")
    cat ("Try to spawn tasks...\n")
    .PVM.spawn (task = file.path (system.file (package = "rpvm"), "slaveR.sh"),
                ntask = ntask,
                flag = flag,
                where = where,
                arglist = c(slave, slavedir, outdir),
                verbose = verbose)
}

.PVM.spawn <- function (task,
                        ntask = 1,
                        flag  = "Default",
                        where = "",
                        arglist = NULL,
                        verbose = FALSE) {    
    .PVM.spawnflags <- c (0, 2^(0:5))
    names (.PVM.spawnflags) <- c("Default", "Host", "Arch",
                                 "Debug", "Trace", "MppFront", "Compl")

    total.flag <- 0
    for (myflag in flag) {
        this.flag <- match.arg (myflag, names (.PVM.spawnflags))
        if (verbose) {
            cat (paste ("Flag ", this.flag, " matched\n"))
        }
        total.flag <- total.flag + .PVM.spawnflags[this.flag]
    }
    tids <- .Call ("rpvm_spawn",
                   as.character (task),
                   as.integer   (ntask),
                   as.integer   (total.flag),
                   as.character (where),
                   as.character (arglist),
                   as.integer (verbose),
                   PACKAGE = "rpvm")
    if (any (tids <= 0)) {
        warning (paste (sum (tids <= 0), "tasks failed to be start"))
    }
    return (tids[tids > 0])
}

.PVM.kill <- function (tids) {
    .Call ("rpvm_kill", as.integer(tids), PACKAGE = "rpvm")
}

.PVM.tasks <- function (where = 0) {
    tasks <- .Call ("rpvm_tasks", as.integer (where), PACKAGE = "rpvm")
    names (tasks) <- c("tid", "parent", "host", "status", "name")
    return (tasks)
}

### Virtual machine control

.PVM.start.pvmd <- function (hosts = "", block = TRUE) {
    info <- .Call ("rpvm_start_pvmd", as.character (hosts),
                   as.integer (block), PACKAGE = "rpvm")
    .PVM.config ()
}

.PVM.halt <- function () {
    .Call ("rpvm_halt", PACKAGE = "rpvm")
}
.PVM.addhosts <- function (hosts) {
    htids <- .Call ("rpvm_addhosts", as.character (hosts), PACKAGE = "rpvm")
    htids[htids > 0]
}

.PVM.delhosts <- function (hosts) {
    htids <- .Call ("rpvm_delhosts", as.character (hosts), PACKAGE = "rpvm")
    htids[htids > 0]
}

.PVM.mstats <- function (hosts) {
    .Call ("rpvm_mstats", as.character (hosts), PACKAGE = "rpvm")    
}

.PVM.config <- function () {
    config <- .Call ("rpvm_config", PACKAGE = "rpvm")
    config <- data.frame (config)
    names (config) <- c ("host.id", "name", "arch", "speed")
    config
}

### Misc

.PVM.tidtohost <- function (tid) {
    .Call ("rpvm_parent", as.integer (tid), PACKAGE = "rpvm")
}

.PVM.notify <- function (msgtag,
                         what = c("ExitTask", "DeleteHost", "AddHost"),
                         par = 0) {
    what.parameters <- 1:3
    names (what.parameters) <- c ("ExitTask", "DeleteHost", "AddHost")
    what <- what.parameters [match.arg (what)]
    .Call ("rpvm_notify", as.integer (what), as.integer (msgtag),
           as.integer (par), PACKAGE = "rpvm")
}

.PVM.unnotify <- function (msgtag,
                         what = c("ExitTask", "DeleteHost", "AddHost"),
                         par = 0) {
    what.parameters <- 1:3
    names (what.parameters) <- c ("ExitTask", "DeleteHost", "AddHost")
    what <- what.parameters [match.arg (what)]
    .Call ("rpvm_notify", as.integer (what), as.integer (msgtag),
           as.integer (par), PACKAGE = "rpvm")
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
        .Call ("rpvm_getopt", as.integer (what), PACKAGE = "rpvm")
    } else {
        .Call ("rpvm_setopt", as.integer (what), as.integer (val),
               PACKAGE = "rpvm")
    }
}
##

### Group library
.PVM.joingroup <- function (group) {
    .Call ("rpvm_joingroup", as.character (group), PACKAGE = "rpvm")
}

.PVM.lvgroup <- function (group) {
    info <- .Call ("rpvm_lvgroup", as.character (group), PACKAGE = "rpvm")
    return (invisible (info))  
}

.PVM.gettid <- function (group, inum = 0:(.PVM.gsize(group) - 1)) {
    .Call ("rpvm_gettid", as.character (group), as.integer (inum),
           PACKAGE = "rpvm")
}

.PVM.getinst <- function (group, tids = .PVM.mytid ()) {
    .Call ("rpvm_getinst", as.character (group), as.integer (tids),
           PACKAGE = "rpvm")
}

.PVM.gsize <- function (group) {
    .Call ("rpvm_gsize", as.character (group), PACKAGE = "rpvm")
}

.PVM.barrier <- function (group, count = .PVM.gsize (group)) {
    info <- .Call ("rpvm_barrier", as.character (group), as.integer (count),
                   PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.bcast <- function (group, msgtag) {
    info <- .Call ("rpvm_bcast", as.character (group), as.integer (msgtag),
                   PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.scatter <- function (x, count, msgtag, group, rootginst = 0) {
    if (is.integer (x)) {
        .Call ("rpvm_scatter_integer", x, as.integer (count),
               as.integer (msgtag), as.character (group),
               as.integer (rootginst), PACKAGE = "rpvm")
    } else {
        .Call ("rpvm_scatter_double", as.double (x), as.integer (count),
               as.integer (msgtag), as.character (group),
               as.integer (rootginst), PACKAGE = "rpvm")
    }
}

.PVM.gather <- function (x, count = length (x),
                         msgtag, group, rootginst = 0) {
    if (is.integer (x)) {
        .Call ("rpvm_gather_integer", x, as.integer (count),
               as.integer (msgtag), as.character (group),
               as.integer (rootginst), PACKAGE = "rpvm")
    } else {
        .Call ("rpvm_gather_double", as.double (x), as.integer (count),
               as.integer (msgtag), as.character (group),
               as.integer (rootginst), PACKAGE = "rpvm")
    }
}

.PVM.reduce <- function (x, func = "Min", count = length (x),
                         msgtag, group, rootginst = 0) {
    funcidx <- pmatch (func, c("Min", "Max", "Sum", "Product"))
    if (is.na (funcidx)) {
        stop ("Invalid arguments for func.")
    }
    if (is.integer (x)) {
        .Call ("rpvm_reduce_integer", x, as.integer (funcidx),
               as.integer (count), as.integer (msgtag),
               as.character (group), as.integer (rootginst), PACKAGE = "rpvm")
    } else {
        .Call ("rpvm_reduce_double", as.double (x), as.integer (funcidx),
               as.integer (count), as.integer (msgtag),
               as.character (group), as.integer (rootginst), PACKAGE = "rpvm")
    }
}

### Message buffers
.PVM.initsend <- function (encoding = c("Default", "Raw", "InPlace")) {
    encoding <- .PVM.encoding[match.arg (encoding)]
    .Call ("rpvm_initsend", as.integer(encoding), PACKAGE = "rpvm")
}

.PVM.mkbuf <- function (encoding = c("Default", "Raw", "InPlace")) {
    encoding <- .PVM.encoding[match.arg (encoding)]
    .Call ("rpvm_mkbuf", as.integer (encoding), PACKAGE = "rpvm")
}

.PVM.freebuf <- function (bufid) {
    info <- .Call ("rpvm_freebuf", as.integer (bufid), PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.getsbuf <- function () {
    .Call ("rpvm_getsbuf", PACKAGE = "rpvm")
}

.PVM.getrbuf <- function () {
    .Call ("rpvm_getsbuf", PACKAGE = "rpvm")
}

.PVM.setsbuf <- function (bufid) {
    .Call ("rpvm_setsbuf", as.integer (bufid), PACKAGE = "rpvm")
}

.PVM.setrbuf <- function (bufid) {
    .Call ("rpvm_setrbuf", as.integer (bufid), PACKAGE = "rpvm")
}
###

### Packing data
.PVM.pkdouble <- function (data = 0.0, stride = 1) {
    info <- .Call ("rpvm_pkdouble", as.double (data), as.integer (stride),
                   PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.pkint <- function (data = 0, stride = 1) {
    info <- .Call ("rpvm_pkint", as.integer (data), as.integer (stride),
                   PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.pkstr <- function (data = "") {
    if (length (data) > 1) {
        warning ("Only the first element is packed.\n")
        data <- data[1]
    }
    info <- .Call ("rpvm_pkstr", as.character (data), PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.pkintvec <- function (data) {
    info <- .Call ("rpvm_pkintvec", as.integer (data), PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.pkdblvec <- function (data) {
    info <- .Call ("rpvm_pkdblvec", as.double (data), PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.pkstrvec <- function (data) {
    .PVM.pkint (max (nchar (data)) + 10)
    .Call ("rpvm_pkstrvec", as.character (data), PACKAGE = "rpvm")
}
.PVM.pkintmat <- function (data) {
    storage.mode (data) <- "integer"
    info <- .Call ("rpvm_pkintmat", as.matrix (data), PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.pkdblmat <- function (data) {
    storage.mode (data) <- "double"
    info <- .Call ("rpvm_pkdblmat", as.matrix (data), PACKAGE = "rpvm")
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
    .Call ("rpvm_upkdouble", as.integer (nitem), as.integer (stride),
           PACKAGE = "rpvm")

.PVM.upkint <- function (nitem = 1, stride = 1)
    .Call ("rpvm_upkint", as.integer (nitem), as.integer (stride),
           PACKAGE = "rpvm")

.PVM.upkstr <- function (maxlen = 200) {
    .Call ("rpvm_upkstr", as.integer (maxlen), PACKAGE = "rpvm")
}

.PVM.upkintvec <- function ()
    .Call ("rpvm_upkintvec", PACKAGE = "rpvm")

.PVM.upkdblvec <- function ()
    .Call ("rpvm_upkdblvec", PACKAGE = "rpvm")

.PVM.upkstrvec <- function () {
    maxlen <- .PVM.upkint ()
    .Call ("rpvm_upkstrvec", as.integer (maxlen), PACKAGE = "rpvm")
}
    
.PVM.upkintmat <- function ()
    .Call ("rpvm_upkintmat", PACKAGE = "rpvm")

.PVM.upkdblmat <- function ()
    .Call ("rpvm_upkdblmat", PACKAGE = "rpvm")

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
    .Call ("rpvm_send", as.integer(tid), as.integer(msgtag),
           PACKAGE = "rpvm")

.PVM.mcast <- function (tids, msgtag) {
    if (msgtag < 0) stop ("Message tag must be >= 0!")
    info <- .Call ("rpvm_mcast", as.integer(tids), as.integer(msgtag),
                   PACKAGE = "rpvm")
    return (invisible (info))
}

.PVM.recv <- function (tid = -1, msgtag = -1)
    .Call ("rpvm_recv", as.integer (tid), as.integer (msgtag),
           PACKAGE = "rpvm")

.PVM.trecv <- function (tid = -1, msgtag = -1, sec = 0, usec = 0) {
    .Call ("rpvm_trecv", as.integer (tid), as.integer (msgtag),
           as.double (c(sec, usec)), PACKAGE = "rpvm")
}

.PVM.nrecv <- function (tid = -1, msgtag = -1)
    .Call ("rpvm_nrecv", as.integer(tid), as.integer(msgtag),
           PACKAGE = "rpvm")

.PVM.probe <- function (tid = -1, msgtag = -1) {
    .Call ("rpvm_probe", as.integer(tid), as.integer(msgtag),
           PACKAGE = "rpvm")    
}

.PVM.bufinfo <- function (bufid) {
    bufinfo <- .Call ("rpvm_bufinfo", as.integer(bufid), PACKAGE = "rpvm")
    if (length (bufinfo) == 1 && bufinfo == -1) {
        return (-1);
    } else {
        return (list (bytes = bufinfo[1], msgtag = bufinfo[2],
                      tid = bufinfo[3]))
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
    if (!NTASK) {
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
        work <- X[(range[1]):(range[2]),,drop=FALSE]
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

#
# wrapper functions for SPRNG (Scalable Parallel Random Number Generator)
# library

.SPRNG.INIT.TAG <- 199

init.sprng.master <- function (children,                         
                               seed = 0,
                               kindprng = "default",
                               para = 0) {
    if (require (rsprng)) {
        cat ("Sorry, SPRNG is not available")
        return (invisible (NULL))
    }
    if (!is.character(kindprng) || length (kindprng) > 1) {
        stop ("kindprng' must be a character string of length 1.")
    }
    if (!is.na (pmatch (kindprng, "default"))) {
        kindprng <- "LFG"
    }
    kind <- pmatch (kindprng, c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
    if (is.na (kind)) {
        stop(paste("'", kindprng, "' is not a valid choice", sep = ""))
    }
    for (i in 1:length (children)) {
        .PVM.initsend ()
        .PVM.pkint (length (children) + 1)
        .PVM.pkint (i)
        .PVM.pkint (seed)
        .PVM.pkstr (kindprng)
        .PVM.pkint (para)
        .PVM.send (children[i], .SPRNG.INIT.TAG)
    }
    init.sprng (length (children) + 1, 0, seed, kindprng, para)
}

init.sprng.slave <- function () {
    if (require (rsprng)) {
        cat ("Sorry, SPRNG is not available")
        return (invisible (NULL))
    }
    myparent <- .PVM.parent ()
    .PVM.recv (myparent, .SPRNG.INIT.TAG)
    nstream  <- .PVM.upkint (1)
    streamno <- .PVM.upkint (1)
    seed     <- .PVM.upkint (1)
    kindprng <- .PVM.upkstr (10)
    para     <- .PVM.upkint (1)
    init.sprng (nstream, streamno, seed, kindprng, para)
}

init.sprng.group <- function (group,
                              rootinst = 0,
                              seed = 0,
                              kindprng = "default",
                              para = 0) {
    if (require (rsprng)) {
        cat ("Sorry, SPRNG is not available")
        return (invisible (NULL))
    }
    if (!is.character(kindprng) || length (kindprng) > 1) {
        stop ("kindprng' must be a character string of length 1.")
    }
    if (!is.na (pmatch (kindprng, "default"))) {
        kindprng <- "LFG"
    } 
    kind <- pmatch (kindprng, c ("LFG", "LCG", "LCG64",
                                 "CMRG", "MLFG", "PMLCG")) - 1
    if (is.na (kind)) {
        stop(paste("'", kindprng, "' is not a valid choice", sep = ""))
    }
                                        # get my group instance number
    myinst <- .PVM.getinst ()
    if (myinst == rootinst) {
                                        # broadcast the init information if
                                        # I'm the root
        .PVM.initsend ()
        .PVM.pkint (seed)
        .PVM.pkstr (kindprng)
        .PVM.pkint (para)
        .PVM.bcast (group, .SPRNG.INIT.TAG)
    } else {
                                        # receive the init information from
                                        # the root
        .PVM.recv (.PVM.gettid (group, rootinst), .SPRNG.INIT.TAG)        
        seed <- .PVM.upkint (1)
        kindprng <- .PVM.upkstr (10)
        para <- .PVM.upkint (1)
    }
    init.sprng (.PVM.gsize (group), .PVM.getinst (), seed,
                kindprng, para)
}

# ## (Un)Serialization using the serialize package
# .PVM.serialize <- function (object, refhook = NULL) {
#     cat ("object = \n")
#     print (object)
#     cat ("\n")
#     str <- serialize (object, NULL, TRUE, refhook)
#     print (str)
#     .PVM.pkstr (str)
#     return (invisible (str))
# }
# .PVM.unserialize <- function (refhook = NULL) {
#     str <- .PVM.upkstr ()
#     unserialize (str, refhook)
# }


## (Un)Serialization of R objects (by  Luke Tierney <luke@stat.umn.edu>)
.PVM.serialize <- function(object, refhook = NULL)
    .Call("rpvm_pksexp", object, refhook, PACKAGE = "rpvm")

.PVM.unserialize <- function(refhook = NULL)
    .Call("rpvm_upksexp", refhook, PACKAGE = "rpvm")


.PVM.siblings <- function( )
    .Call ("rpvm_siblings", PACKAGE = "rpvm")
    
