## Emacs: -*- r -*-
## Test script for PVM, both master and slave functions.

## Load in rpvm library
library (rpvm)

## Define functions

# Some arbitary positive integer as message tag

RANDOM.TAG <- 22

## Function to be executed by the master
master.f <- function (numChild = 2, seed = 1043323) {
    
    ## Spawn children
    children <- .PVM.spawnR (slave = "sprng_test", ntask = numChild)
    if (length (children) == 0) {
        warning ("Failed to spawn any task: \n")
        .PVM.exit ()
    } else if (length (children) < numChild) {
        cat ("Failed to spawn some tasks.\n ")
    }
    cat("### Spawned ", length (children), "\n")

    ## send random seeds
    init.sprng.master (children, 12312)

    ## receive random numbers from children
    alldata <- NULL
    for (child in children) {
        .PVM.recv (child, RANDOM.TAG)
        data <- .PVM.upkdblvec ()
        alldata <- cbind (alldata, data)
    }
    free.sprng ()
    .PVM.exit ()
    ## Receive data back from children.
    return (alldata)
}

## Fuction to be executed by the slave
slave.f <- function () {

    init.sprng.slave ()

    somedata <- runif (100)
    .PVM.initsend ()
    .PVM.pkdblvec (somedata)
    .PVM.send (.PVM.parent(), RANDOM.TAG)
    free.sprng ()
    .PVM.exit()
}

## Actuall codes

if (require (rsprng)) {
    if (is.na (.PVM.config ())) {
         cat ("pvmd is not running. Try to start pvmd...\n")
        .PVM.start.pvmd (file.path (.lib.loc[1], "rpvm", "pvmhosts"))
    }

    ## Register the master into PVM
    myparent <- .PVM.parent ()
    if (myparent == 0) {
        ## This is master
        data <- master.f ()
        ## Correlations of the two columns
        cor (data)
    } else {
        ## This is slave
        slave.f ()
    }
} else {
    warning ("Sorry, SPRNG is not avaiable! sprng_test is not run.")
}
