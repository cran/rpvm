/* $Id: rpvm_ser.c,v 1.1 2002/04/22 19:59:46 snake Exp $ */

#include "utils.h"
#include "pvm3.h"

int rpvm_chkerror (int error_code, int exit_pvm);

static void OutChar(R_outpstream_t stream, int c)
{
    char buf[1];
    buf[0] = c;
    rpvm_chkerror(pvm_pkbyte(buf, 1, 1), 1);
}

static void OutBytes(R_outpstream_t stream, void *buf, int length)
{
    rpvm_chkerror(pvm_pkbyte(buf, length, 1), 1);
}

static int InChar(R_inpstream_t stream)
{
    char buf[1];
    rpvm_chkerror(pvm_upkbyte(buf, 1, 1), 1);
    return buf[0];
}

static void InBytes(R_inpstream_t stream, void *buf, int length)
{
    rpvm_chkerror(pvm_upkbyte(buf, length, 1), 1);
}


/* ought to quote the argument, but it should only be an ENVSXP or STRSXP */
static SEXP CallHook(SEXP x, SEXP fun)
{
    SEXP val, call;
    PROTECT(call = LCONS(fun, LCONS(x, R_NilValue)));
    val = eval(call, R_GlobalEnv);
    UNPROTECT(1);
    return val;
}

SEXP rpvm_pksexp (SEXP object, SEXP fun)
{
    struct R_outpstream_st out;
    R_pstream_format_t type = R_pstream_xdr_format;
    SEXP (*hook)(SEXP, SEXP);

    hook = fun != R_NilValue ? CallHook : NULL;
    R_InitOutPStream(&out, NULL, type, 0, OutChar, OutBytes, hook, fun);
    R_Serialize(object, &out);
    return R_NilValue;
}

SEXP rpvm_upksexp (SEXP fun)
{
    struct R_inpstream_st in;
    R_pstream_format_t type = R_pstream_xdr_format;
    SEXP (*hook)(SEXP, SEXP);

    hook = fun != R_NilValue ? CallHook : NULL;
    R_InitInPStream(&in, NULL, type, InChar, InBytes, hook, fun);
    return R_Unserialize(&in);
}
