
## Test script for PVM, both master and slave functions.

## Load in rpvm library
library (rpvm)

## Define functions

# Some arbitary positive integer as message tag

jointag <- 22
BUFTAG <- 33
RESULTTAG <- 44

## Function to be executed by the master
master.f <- function () {
    ## Spawn one child
    child <- .PVM.spawnR (slave = "pvm_test", ntask = 1)
    if (length (child) != 1 || child < 0) {
        warning ("Failed to spawn tasks\n")
        .PVM.exit ()
    }
    cat("### Child process ", child, " spawned , waiting for data \n")
    
    ## Receive data back from children.
    ## receive a message from the child process with tag mytag
    buf <- .PVM.recv (child, jointag)
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

    cat ("\nLet's try some serialzation stuff \n")
    amatrx <- matrix (runif (12), nrow = 3)
    afunc <- function (x) {
        if (!is.matrix (x)) {
            stop ("x must be a matrix")
        }
        apply (x, 2, mean)
    }
    .PVM.initsend ()
    cat ("\nHere is a matrix: \n")
    print (amatrx)
    .PVM.serialize (amatrx)
    cat ("\nand a function \n")
    print (afunc)
    .PVM.serialize (afunc)
    .PVM.send (child, BUFTAG)

    cat ("\nBoth are sent to the child process and the child process\n")
    cat ("applies the function to the matrix and return the result\n\n")
    
    ## collecet result
    .PVM.recv (child, RESULTTAG)
    ar <- .PVM.unserialize ()
    print (ar)

    cat ("\nShould be the same as :\n")
    print (afunc (amatrx))
    
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

    ## Serialization
    .PVM.recv (myparent, BUFTAG)
    am <- .PVM.unserialize ()
    print (am)
    af <- .PVM.unserialize ()
    print (af)
    ares <- af (am)
    .PVM.initsend ()
    .PVM.serialize (ares)
    .PVM.send (myparent, RESULTTAG)
    
    .PVM.exit()
}

## Actuall codes
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
    ## Register the master into PVM
    myparent <- .PVM.parent ()
    if (myparent == 0) {
        ## This is master
        master.f ()
    } else {
        ## This is slave
        slave.f ()
    }
} else {
    cat ("PVMD is not running. Do nothing.")
}

