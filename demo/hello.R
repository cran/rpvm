library (rpvm)

hello.pvm <- function () {
    mytid <- .PVM.mytid()
    cat ("I am the master", mytid, " from ", Sys.getenv("HOST"), "\n")
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
    return (invisible(msg))
}

hello.pvm ()
