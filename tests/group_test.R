library (rpvm)

## Example of group library functions.
## Adopted from trsg.c in pvm3/gexample.
## Arguments (meaningful only to the leader):
##     nprocs - no. of processes (workers)
##     count  - length of vectors (jobs) for each worker
##
gname <- "group_rsg"
                                        # start VM if necessary
if (is.na (.PVM.config ())) {
    cat ("pvmd is not running. Try to start pvmd...\n")
    options(show.error.messages = FALSE)
    try (.PVM.start.pvmd (file.path (.path.package (package = "rpvm"),
                                     "pvmhosts")))
    if (inherits (.Last.value, "try-error")) {
        print (.Last.value)
        pvmrun <- FALSE
    } else {
        pvmrun <- TRUE
    }
    options(show.error.messages = TRUE)
} else {
    pvmrun <- TRUE
}

if (pvmrun) {
                                        # Join the group
    myginst <- .PVM.joingroup (gname)
    ## I'm the first group member, start up copies of myself
    if (myginst == 0) {
        
        nprocs <- 4
        count <- 10
        
        if (nprocs > 1) {
            slavesh <- file.path (.path.package (package = "rpvm"),
                                  "slaveR.sh")
            tids <- .PVM.spawn (task = slavesh,
                                arglist = c("group_test", getwd (), getwd ()),
                                ntask = nprocs - 1)
        }
        ## Wait until they have all started
        while (.PVM.gsize (gname) < nprocs) {
            Sys.sleep (1)
        }
        gsize <- .PVM.gsize (gname)
        
        ## Add myself to the list of task ids.
        tids <- c(.PVM.mytid (), tids)
        print (tids)
        ## test gettid and getinst
        tids2 <- .PVM.gettid (gname)
        print (tids2)
        tids2 <- sample (tids2)
        inst2 <- .PVM.getinst (gname, tids2)
        tids3 <- .PVM.gettid  (gname, inst2)
        print (cbind(TIDS = tids2, INST = inst2, TIDS2 = tids3))
        
    ## Brief coworkers
        .PVM.initsend ()
        .PVM.pkint (nprocs)
        .PVM.pkint (count)
        .PVM.bcast (gname, msgtag = 17)
    } else {
        ## Receive brief info from root
        .PVM.recv (-1, msgtag = 17)
        nprocs <- .PVM.upkint ()
        count  <- .PVM.upkint ()
    }

    ## Determine the group leader
    leader <- 0
    
    if (myginst == leader) {
        ## Initialize the matrix (as a vector) values on the leader
        test.mat <- as.integer (1:(nprocs * count))
    } else {
        ## Others should also allocate memory for it (?)
        test.mat <- as.integer (rep (0, nprocs * count))
    }
    ## scatter rows of matrix to each processor
    myrow <- .PVM.scatter (test.mat, count, msgtag = 19, group = gname,
                           rootginst = leader)
    ## square each value on each proc
    myrow <- myrow ^ 2
    ## do partial sum on each proc
    P.sum <- sum (myrow)
    ## gather partial sums to the leader
    col.sum <- .PVM.gather (as.integer (P.sum), msgtag = 21, group = gname,
                            rootginst = leader)
    ## do a global sum over myrow, the result goes to leader
    row.sum <- .PVM.reduce (as.integer (myrow), func = "Sum", msgtag = 23,
                            group = gname, count = count,
                            rootginst = leader)
    
    if (myginst == leader) {
        ## Compute the sum of squares with different methods
        direct.sum <- sum (test.mat ^ 2)                    # direct method
        gather.sum <- sum (col.sum)                         # gather
        reduce.sum <- sum (row.sum)                         # reduce
        if (gather.sum == direct.sum && reduce.sum == direct.sum) {
            cat ("Test OK: Sum of squares is ", direct.sum, "\n")
        } else {
            cat ("ERROR! The sums of squares are:\n")
            cat ("       Direct :", direct.sum, "\n")
            cat ("       Reduce :", reduce.sum, "\n")
            cat ("       Gather :", gather.sum, "\n")
        }
    }
    
    
    ## Sync up again, leave group, exit pvm
    .PVM.barrier (gname)
    .PVM.lvgroup (gname)
    .PVM.exit ()
} else {
    cat ("PVMD is not running.  Do nothing.")
}
