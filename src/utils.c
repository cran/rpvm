/*
 * $Id: utils.c,v 1.3 2002/10/24 04:30:28 nali Exp $
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
 * Convert an SEXP string vector to a (char **), ending with a null string.
 */
char **toPPChar (SEXP sexp_str)
{
    char **ppchar;
    int  i;
    int  len = LENGTH (sexp_str);

    if (!isString (sexp_str)) {
        error ("argument is not a character vector");
    }

    /* Temporary memory will be released after exiting .Call */
    ppchar = (char **) R_alloc (len + 1, sizeof (char *));
    for (i = 0; i < len; ++i) {
        ppchar[i] = CHAR (STRING_ELT (sexp_str, i));
    }
    ppchar[len] = (char *) 0;
    return ppchar;
}
