
## Test script for PVM, both master and slave functions.

## Load in rpvm library
library (rpvm)

## Define functions

# Some arbitary positive integer as message tag

jointag <- 22

## Function to be executed by the master
master.f <- function (numChild = 1) {
    ## Spawn children
    cat ("## Spawning ", numChild, "children \n")
    curdir <- getwd ()
    shscript <- file.path (.lib.loc[1], "rpvm", "slaveR.sh")
    children <- .PVM.spawn (task = shscript,
                            ntask = numChild,
                            arglist = c("pvm_test", curdir, curdir))
    if (all (children < 0)) {
        .PVM.exit ()
        stop ("Failed to spawn any task: \n")
    } else if (any (children < 0)) {
        cat ("Failed to spawn some tasks.  Successfully spawned ",
             sum(children > 0), "tasks\n")
        children <- children[children > 0]
    }
    cat("### Spawned ", length (children), "Task, waiting for data \n")

    ## Receive data back from children.
    for (child in children) {
        ## receive a message from any child process with tag mytag
        buf <- .PVM.recv (-1, jointag)
        ## find out which task the message came from
        bufinfo <- .PVM.bufinfo (buf)
        cat ("Message received from ", bufinfo$tid, " \n")
        
        ## Unpacking messages
        hello <- .PVM.upkstrvec ()
        cat (hello, "\n")        
        msg1 <- .PVM.upkintvec ()
        cat ("Some integers ", msg1, "\n")
        msg2 <- .PVM.upkdblvec ()
        cat ("Some doubles ", msg2, "\n")
        msg3 <- .PVM.upkdblmat ()
        cat ("And a matrix\n")
        print (msg3)

        msg4 <- .PVM.upkfactor ()
        cat ("Even a factor!\n")
        print (msg4)        
        cat("That's all, folks \n")
    }
    .PVM.exit()
    return (invisible(NULL))
}

## Fuction to be executed by the slave
slave.f <- function (myparent) {
    
    ## Generate data
    data1 <- rpois (3, lambda = 10)
    print (data1)
    data2 <- rnorm (3, mean = 10)
    print (data2)
    data3 <- matrix (data = rnorm (9), nrow = 3)
    print (data3)
    ff <- factor(substring("statistics", 1:10, 1:10))
    print (ff)
    ## initialize buffer for sending
    .PVM.initsend()    
    ## Pack and send back
    .PVM.pkstrvec (c ("Hello", "World!", "from", Sys.getenv ("HOST")))
    .PVM.pkintvec (data1)
    .PVM.pkdblvec (data2)
    .PVM.pkdblmat (data3)
    .PVM.pkfactor (ff)
    
    myparent <- .PVM.parent ()
    .PVM.send (myparent, jointag)
    
    .PVM.exit()
}

## Actuall codes

if (is.na (.PVM.config ())) {
    .PVM.start.pvmd (file.path (.lib.loc[1], "rpvm", "pvmhosts"))
}

## Register the master into PVM
myparent <- .PVM.parent ()
if (myparent == 0) {
    conf <- .PVM.config ()
    print (conf)
    ## This is master
    master.f ()
} else {
    ## This is slave
    slave.f ()
}

