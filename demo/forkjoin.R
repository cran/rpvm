
## $Id: forkjoin.R,v 1.3 2004/05/17 04:47:16 nali Exp $

require (rpvm)

forkjoin.pvm <- function (nfork = 3) {

    JOINTAG <- 11
    
    mytid <- .PVM.mytid ()
    cat ("I am ", mytid, "\n")
    children <- .PVM.spawn ("forkjoin", nfork)
    if (length (children[children > 0]) == 0) {
                                        # spawn failed
        cat ("Failed to spawn task.\n")
    } else {
        for (child in children) {
            ## receive a message from any child process with tag JOINTAG
            buf <- .PVM.recv (-1, JOINTAG)
            ## find out which task the message came from
            bufinfo <- .PVM.bufinfo (buf)
            msg <- .PVM.upkint ()
            cat ("Message ", msg, "from ", bufinfo$tid, " \n")
        }
    }
    .PVM.exit()
    return (invisible(NULL))
}

forkjoin.pvm ()
