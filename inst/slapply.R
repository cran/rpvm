# $Id: slapply.R,v 1.2 2001/08/31 05:03:51 snake Exp $

### Slave R script for the PVM apply.
### Receive an 'order' and a matrix.  Process the matrix with apply()
### and return the resulted vector and order.

### To do: accept a string as the name of the function to be applied
### on each row.  This way, this script can be more generic. 

library (rpvm)
WORKTAG <- 22
RESULTAG <- 33

mytid  <- .PVM.mytid ()
myparent  <- .PVM.parent ()

## Receive work from parent (a matrix)
buf <- .PVM.recv (myparent, WORKTAG)

## Function to apply
func  <- .PVM.upkstr ()

cat ("Function to apply: ", func, "\n")

## Order
order <- .PVM.upkint ()
partial.work <- .PVM.upkdblmat ()

print (partial.work)

## actually work, take the mean of the rows
partial.result <- apply (partial.work, 1, func)

print (partial.result)

## Send result back
.PVM.initsend ()
.PVM.pkint (order)
.PVM.pkdblvec (partial.result)
.PVM.send (myparent, RESULTAG)

## Exit PVM
.PVM.exit ()
## Exit R
q (save="no")
