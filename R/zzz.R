# Emacs: -*- r -*- vim: ft=r 
.First.lib <- function (lib, pkg) {
    library.dynam("rpvm", pkg, lib)
}
