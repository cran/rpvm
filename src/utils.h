/*
 * $Id: utils.h,v 1.1 2001/11/20 21:46:38 snake Exp $
 */

#include <R.h>
#include <Rdefines.h>

/**
 * Make an SEXP out of an integer scalar
 */
SEXP mkInt (int a);

/**
 * Make an SEXP out of a real scalar
 */
SEXP mkReal (double x);

/**
 * Convert an SEXP string vector to a (char **).
 */
char **toPPChar (SEXP sexp_str);

