/*
 * $Id: utils.c,v 1.2 2002/03/18 23:47:38 snake Exp $
 */

#include "utils.h"

/**
 * Make an SEXP out of an integer scalar
 */
SEXP mkInt (int a)
{
    SEXP sexp_ans;
    PROTECT (sexp_ans = allocVector (INTSXP, 1));
    INTEGER (sexp_ans)[0] = a;
    UNPROTECT (1);
    return sexp_ans;
}

SEXP mkReal (double x)
{
    SEXP sexp_ans;
    PROTECT (sexp_ans = allocVector (REALSXP, 1));
    REAL (sexp_ans)[0] = x;
    UNPROTECT (1);
    return sexp_ans;
}

/**
 * Convert an SEXP string vector to a (char **).
 */
char **toPPChar (SEXP sexp_str)
{
    /* Note: it is the user's responsiblity to ensure sexp_str is
     * indeed a vector of strings */
    char **ppchar;
    int  i;
    int  len = LENGTH (sexp_str);
    /* Temporary memory will be released after exiting .Call */
    ppchar = (char **) R_alloc (len + 1, sizeof (char *));
    for (i = 0; i < len; ++i) {
        ppchar[i] = CHAR (STRING_ELT (sexp_str, i));
    }
    ppchar[len] = (char *) 0;
    return ppchar;
}
