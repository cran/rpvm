/*
 * $Id: rpvm_core.c,v 1.22 2004/05/25 22:18:29 nali Exp $
 */

#include "utils.h"
#include "pvm3.h"

int rpvm_chkerror (int error_code, int exit_pvm)
{
    if (error_code >= 0) {
        /* Not an error, return intact */
        return error_code;
    }

    switch (error_code) {
    case PvmBadParam :
        error ("PVM: invalid parameter");
        break;
    case PvmMismatch :
        error ("PVM: barrier count mismatch");
        break;
    case PvmNoData :
        error ("PVM: read past end of buffer");
        break;
    case PvmNoHost :
        error ("PVM: no such host");
        break;
    case PvmNoFile :
        error ("PVM: no such executable");
        break;
    case PvmNoMem :
        error ("PVM: can't get memory");
        break;
    case PvmBadMsg :
        error ("PVM: can't decode received message");
        break;
    case PvmSysErr :
        error ("PVM: pvmd not responding");
        break;
    case PvmNoBuf :
        error ("PVM: no current buffer");
        break;
    case PvmNoSuchBuf :
        error ("PVM: bad message id");
        break;
    case PvmNullGroup :
        error ("PVM: null group name");
        break;
    case PvmDupGroup :
        error ("PVM: already in group");
        break;
    case PvmNoGroup :
        error ("PVM: no such group");
        break;
    case PvmNotInGroup :
        error ("PVM: not in group");
        break;
    case PvmNoInst :
        error ("PVM: no such instance in group");
        break;
    case PvmHostFail :
        error ("PVM: host failed");
        break;
    case PvmNoParent :
        error ("PVM: no parent task");
        break;
    case PvmNotImpl :
        error ("PVM: function not implemented");
        break;
    case PvmDSysErr :
        error ("PVM: pvmd system error");
        break;
    case PvmBadVersion :
        error ("PVM: pvmd-pvmd protocol mismatch");
        break;
    case PvmOutOfRes :
        error ("PVM: out of resources");
        break;
    case PvmDupHost :
        error ("PVM: host already configured");
        break;
    case PvmCantStart :
        error ("PVM: failed to exec new slave pvmd");
        break;
    case PvmAlready :
        error ("PVM: already doing operation");
        break;
    case PvmNoTask :
        error ("PVM: no such task");
        break;
    case PvmDenied :
        error ("PVM: operation denied");
        break;
    case PvmExists :  /* replaced PvmDupEntry */
        error ("PVM: already an entry matching insert request");
        break;
    case PvmNotFound :  /* replaceed PvmNoEntry */
        error ("PVM: no entry matching the lookup request");
        break;
    case PvmOverflow :
        error ("PVM: value too large to be packed or unpacked");
        break;
    default:
        error ("PVM: unkown error");
        break;
    }
    /* Exit pvmd (necessary for all errors??) */
    if (exit_pvm) {
        pvm_perror ("Error encountered. Exiting PVM ...\n");
        pvm_exit ();
    }
    return error_code;
}

/* Process control */

SEXP rpvm_mytid ()
{
    return mkInt (rpvm_chkerror (pvm_mytid (), 1));
}

SEXP rpvm_parent ()
{
    int ptid = pvm_parent ();
    if (ptid == PvmNoParent) {
        return mkInt (0);
    } else {
        return mkInt (rpvm_chkerror (ptid, 1));
    }
}

SEXP rpvm_exit ()
{
    return mkInt (rpvm_chkerror (pvm_exit(), 1));
}

SEXP rpvm_pstats (SEXP sexp_tids)
{
    int i;
    int status;

    SEXP sexp_pstat;
    PROTECT (sexp_pstat  = allocVector (STRSXP, LENGTH (sexp_tids)));
    for (i = 0; i < LENGTH (sexp_tids); ++i) {
        status = pvm_pstat (INTEGER (sexp_tids)[i]);
        if (status == PvmOk) {
            SET_STRING_ELT (sexp_pstat,  i, mkChar ("OK"));
        } else if (status == PvmNoTask) {
            SET_STRING_ELT (sexp_pstat,  i, mkChar ("Not Running"));
        } else if (status == PvmBadParam) {
            SET_STRING_ELT (sexp_pstat,  i, mkChar ("Invalid tid"));
        } else {
            SET_STRING_ELT (sexp_pstat,  i, mkChar ("Unknown"));
        }
    }
    setAttrib (sexp_pstat, R_NamesSymbol, sexp_tids);
    UNPROTECT (1);
    return sexp_pstat;
}


SEXP rpvm_kill (SEXP sexp_tids)
{
    int n = LENGTH (sexp_tids);
    int i;
    int info;
    int killed = 0;
    for (i = 0; i < n; ++i) {
        info = pvm_kill (INTEGER (sexp_tids)[i]);
        if (info < 0) {
            rpvm_chkerror (info, 0);
        } else {
            ++killed;
        }
    }
    return mkInt (killed);
}

SEXP rpvm_tasks (SEXP sexp_where)
{
    int where = INTEGER (sexp_where) [0];
    int info, ntask, i;
    struct pvmtaskinfo *taskp;
    SEXP sexp_tasks, sexp_tid, sexp_ptid, sexp_host, sexp_status, sexp_aout;

    info = pvm_tasks (where, &ntask, &taskp );
    rpvm_chkerror (info, 1);

    PROTECT (sexp_tid   = allocVector (INTSXP, ntask));
    PROTECT (sexp_ptid  = allocVector (INTSXP, ntask));
    PROTECT (sexp_host  = allocVector (INTSXP, ntask));
    PROTECT (sexp_status = allocVector (STRSXP, ntask));
    PROTECT (sexp_aout  = allocVector (STRSXP, ntask));

    for (i = 0; i < ntask; ++i) {
        INTEGER (sexp_tid)[i]  = taskp[i].ti_tid;
        INTEGER (sexp_ptid)[i] = taskp[i].ti_ptid;
        INTEGER (sexp_host)[i] = taskp[i].ti_host;
        if (taskp[i].ti_flag == PvmOk) {
            SET_STRING_ELT (sexp_status,  i, mkChar ("OK"));
        } else if (taskp[i].ti_flag == PvmNoTask) {
            SET_STRING_ELT (sexp_status,  i, mkChar ("Not Running"));
        } else if (taskp[i].ti_flag == PvmBadParam) {
            SET_STRING_ELT (sexp_status,  i, mkChar ("Invalid tid"));
        } else {
            SET_STRING_ELT (sexp_status,  i, mkChar ("Unknown"));
        }
        SET_STRING_ELT (sexp_aout, i, mkChar (taskp[i].ti_a_out));
    }

    PROTECT (sexp_tasks = allocVector (VECSXP, 5));
    SET_VECTOR_ELT (sexp_tasks, 0, sexp_tid);
    SET_VECTOR_ELT (sexp_tasks, 1, sexp_ptid);
    SET_VECTOR_ELT (sexp_tasks, 2, sexp_host);
    SET_VECTOR_ELT (sexp_tasks, 3, sexp_status);
    SET_VECTOR_ELT (sexp_tasks, 4, sexp_aout);

    UNPROTECT (6);

    /* return a list */
    return sexp_tasks;
}


/*
 * The following needs to be fixed, with respect to the arglist to pass.
 */
SEXP rpvm_spawn (SEXP sexp_task,
                 SEXP sexp_ntask,
                 SEXP sexp_flag,
                 SEXP sexp_where,
                 SEXP sexp_arglist,
                 SEXP sexp_verbose)
{
    int numt;
    int ntask = INTEGER (sexp_ntask)[0];
    int flag  = INTEGER (sexp_flag)[0];
    int verbose = INTEGER (sexp_verbose)[0];
    char **arglist = toPPChar (sexp_arglist);
    int i = 0;
    SEXP sexp_tids;

    if (verbose) {
        /* printing arglist */
        Rprintf ("Arglist is: \n");
        for (i = 0; arglist[i]; ++i) {
            Rprintf ("     %d -> %s\n", i, arglist[i]);
        }
    }

    PROTECT (sexp_tids  = allocVector (INTSXP, ntask));
    /* spawn tasks */
    numt = pvm_spawn (CHAR (STRING_ELT (sexp_task, 0)),
                      arglist,
                      flag,
                      CHAR (STRING_ELT (sexp_where, 0)),
                      ntask,
                      INTEGER (sexp_tids));
    UNPROTECT (1);
    rpvm_chkerror (numt, 1);

    if (numt < ntask) {
        Rprintf ("Requested spawning %d tasks, %d successful.\n",
                 ntask, numt);
        for (i = 0; i < ntask; ++i) {
            Rprintf ("Tids[%d] = %d\n", i, INTEGER (sexp_tids)[i]);
            rpvm_chkerror (INTEGER (sexp_tids)[i], 1);
        }
    }
    return sexp_tids;
}

/* Message Passing */

/* Msssage Buffers */

/**
 * Clears the send buffer and creates a new one for packing a new package.
 * Return the buffer id.
 */

SEXP rpvm_initsend (SEXP sexp_encode)
{
    int encode = INTEGER (sexp_encode)[0];
    return mkInt (rpvm_chkerror (pvm_initsend (encode), 0));
}

/**
 * Create a new empty send buffer and specifies the encoding method.
 * Return the buffer id.
 */
SEXP rpvm_mkbuf (SEXP sexp_encode)
{
    int encode = INTEGER (sexp_encode)[0];
    return mkInt (rpvm_chkerror (pvm_mkbuf (encode), 1));
}

/**
 * Dispose a specified buffer
 */
SEXP rpvm_freebuf (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];
    return mkInt (rpvm_chkerror (pvm_freebuf (bufid), 1));
}

/**
 * Returns the active send buffer id.
 */
SEXP rpvm_getsbuf ()
{
    return mkInt (rpvm_chkerror (pvm_getsbuf (), 1));
}

/**
 * Returns the active receive buffer id.
 */
SEXP rpvm_getrbuf ()
{
    return mkInt (rpvm_chkerror (pvm_getrbuf (), 1));
}

/**
 * Set the active send buffer, save the state of the previous buffer.
 * Returns the previous buffer's id.
 */
SEXP rpvm_setsbuf (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];
    return mkInt (rpvm_chkerror (pvm_setsbuf (bufid), 1));
}

/**
 * Set the active receive buffer, save the state of the previous buffer.
 * Returns the previous buffer's id.
 */
SEXP rpvm_setrbuf (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];
    return mkInt (rpvm_chkerror (pvm_setrbuf (bufid), 1));
}

/* Packing data */

SEXP rpvm_pkint (SEXP sexp_np, SEXP sexp_stride)
{
    int nitem = length (sexp_np);
    int stride = INTEGER (sexp_stride)[0];
    int *np = INTEGER (sexp_np);

    return mkInt (rpvm_chkerror (pvm_pkint (np, nitem, stride), 1));
}

SEXP rpvm_pkdouble (SEXP sexp_np, SEXP sexp_stride)
{
    int nitem = length (sexp_np);
    int stride = INTEGER (sexp_stride)[0];
    double *np = REAL (sexp_np);

    return mkInt (rpvm_chkerror (pvm_pkdouble (np, nitem, stride), 1));
}

SEXP rpvm_pkstr (SEXP sexp_cp)
{
    char *cp = CHAR (STRING_ELT (sexp_cp, 0));
    return mkInt (rpvm_chkerror (pvm_pkstr (cp), 1));
}

SEXP rpvm_pkintvec (SEXP sexp_vec)
{
    int len = length (sexp_vec);
    int val;

    val = pvm_pkint (&len, 1, 1);
    rpvm_chkerror (val, 1);
    val = pvm_pkint (INTEGER (sexp_vec), len, 1);
    return mkInt (rpvm_chkerror (val, 1));
}


SEXP rpvm_pkdblvec (SEXP sexp_vec)
{
    int len = length (sexp_vec);
    int val;

    val = pvm_pkint (&len, 1, 1);
    rpvm_chkerror (val, 1);
    val = pvm_pkdouble (REAL (sexp_vec), len, 1);
    return mkInt (rpvm_chkerror (val, 1));
}

SEXP rpvm_pkstrvec (SEXP sexp_str)
{
    int len = LENGTH (sexp_str);
    int i, val;
    val = pvm_pkint (&len, 1, 1);
    rpvm_chkerror (val, 1);
    for (i = 0; i < len; ++i) {
        val = pvm_pkstr (CHAR (STRING_ELT (sexp_str, i)));
        if (val < 0) {
            return mkInt (rpvm_chkerror (val, 0));
        }
    }
    return mkInt (len);
}

SEXP rpvm_pkintmat (SEXP sexp_mat)
{
    int *dims;                 /* Dimension of the matrix */
    int *pmat;

    int val;

    if (!isMatrix(sexp_mat)) {
        error ("Argument must be a matrix.");
    }

    dims = INTEGER (coerceVector (getAttrib (sexp_mat, R_DimSymbol),
                                  INTSXP));
    PROTECT (sexp_mat = coerceVector(sexp_mat, INTSXP));
    pmat = INTEGER (sexp_mat);

    /* Pack dimensions */
    val = pvm_pkint (dims, 2, 1);
    rpvm_chkerror (val, 1);
    /* Pack data */
    val = pvm_pkint (pmat, dims[0]*dims[1], 1);
    rpvm_chkerror (val, 1);

    UNPROTECT(1);
    return mkInt (val);
}

SEXP rpvm_pkdblmat (SEXP sexp_mat)
{
    int *dims;                 /* Dimension of the matrix */
    double *pmat;

    int val;

    if (!isMatrix(sexp_mat)) {
        error ("Argument must be a matrix.");
    }

    dims = INTEGER (coerceVector (getAttrib (sexp_mat, R_DimSymbol),
                                  INTSXP));
    PROTECT (sexp_mat = coerceVector(sexp_mat, REALSXP));
    pmat = REAL (sexp_mat);

    /* Pack dimensions */
    val = pvm_pkint (dims, 2, 1);
    rpvm_chkerror (val, 1);
    /* Pack data */
    val = pvm_pkdouble (pmat, dims[0]*dims[1], 1);
    rpvm_chkerror (val, 1);
    UNPROTECT(1);
    return mkInt (val);
}

/* Sending and receving data */

SEXP rpvm_send (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (rpvm_chkerror (pvm_send (tid, msgtag), 1));
}

/**
 * Broadcase a message to all tasks in vector sexp_tids.
 */
SEXP rpvm_mcast (SEXP sexp_tids,  SEXP sexp_msgtag)
{
    int *tids  = INTEGER (sexp_tids);
    int ntask  = length (sexp_tids);
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (rpvm_chkerror (pvm_mcast (tids, ntask, msgtag), 1));
}

SEXP rpvm_recv (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (rpvm_chkerror (pvm_recv (tid, msgtag), 1));
}

/**
 * Nonblocking receive, return 0 if message not received.
 */
SEXP rpvm_nrecv (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (rpvm_chkerror (pvm_nrecv (tid, msgtag), 1));
}

/**
 * Check if message arrived.
 */
SEXP rpvm_probe (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (rpvm_chkerror (pvm_probe (tid, msgtag), 1));
}

/**
 * Timeout version of receive.
 */
SEXP rpvm_trecv (SEXP sexp_tid, SEXP sexp_msgtag, SEXP sexp_tmout)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    struct timeval tmout;
    if (REAL (sexp_tmout)[0] < 0) {
        return mkInt (rpvm_chkerror (pvm_trecv (tid, msgtag,
                                                (struct timeval*) 0), 1));
    }
    tmout.tv_sec  = REAL (sexp_tmout)[0];

    if (length (sexp_tmout) > 1) {
        tmout.tv_usec = REAL (sexp_tmout)[1];
    } else {
        tmout.tv_usec = 0.0;
    }

    return mkInt (rpvm_chkerror (pvm_trecv (tid, msgtag, &tmout), 1));
}

SEXP rpvm_bufinfo (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];

    int info, bytes, msgtag, tid;

    SEXP sexp_info;

    info = pvm_bufinfo (bufid, &bytes, &msgtag, &tid);
    if (info < 0) {
        return mkInt (rpvm_chkerror (info, 1));
    }

    PROTECT (sexp_info = allocVector (INTSXP, 3));
    INTEGER (sexp_info)[0] = bytes;
    INTEGER (sexp_info)[1] = msgtag;
    INTEGER (sexp_info)[2] = tid;
    UNPROTECT (1);
    return sexp_info;
}

/* Unpacking data */

SEXP rpvm_upkint (SEXP sexp_nitem, SEXP sexp_stride )
{
    int info;

    SEXP sexp_ans;

    int nitem  = INTEGER (sexp_nitem)[0];
    int stride = INTEGER (sexp_stride)[0];

    PROTECT (sexp_ans = allocVector (INTSXP, nitem));
    info = pvm_upkint (INTEGER (sexp_ans), nitem, stride);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);
    return sexp_ans;
}

SEXP rpvm_upkdouble (SEXP sexp_nitem, SEXP sexp_stride)
{
    SEXP sexp_ans;
    int info;
    int nitem  = INTEGER (sexp_nitem)[0];
    int stride = INTEGER (sexp_stride)[0];

    PROTECT (sexp_ans = allocVector (REALSXP, nitem));
    info = pvm_upkdouble (REAL (sexp_ans), nitem, stride);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);
    return sexp_ans;
}

SEXP rpvm_upkstr (SEXP sexp_maxlen)
{
    int maxlen = INTEGER (sexp_maxlen)[0];
    int i;
    int info;
    char *ans;

    ans = (char *) R_alloc (maxlen, sizeof(char));
    info = pvm_upkstr (ans);
    rpvm_chkerror (info, 1);
    return mkString (ans);
}

SEXP rpvm_upkstrvec (SEXP sexp_maxlen)
{
    int maxlen = INTEGER (sexp_maxlen)[0];
    int len;
    int i;
    int info;
    char *tmp;

    SEXP sexp_ans;

    tmp = (char *) R_alloc (maxlen, sizeof (char));
    /* No. of strings */
    info = pvm_upkint (&len, 1, 1);
    rpvm_chkerror (info, 1);
    PROTECT (sexp_ans = allocVector (STRSXP, len));
    for (i = 0; i < len; ++i) {
        info = pvm_upkstr (tmp);
        rpvm_chkerror (info, 1);
        SET_STRING_ELT(sexp_ans, i, COPY_TO_USER_STRING(tmp));
    }
    UNPROTECT (1);
    return sexp_ans;
}

SEXP rpvm_upkintvec ()
{
    int len;
    int info = 0;

    SEXP sexp_vec;

    /* Get length of the vector */
    info = pvm_upkint (&len, 1, 1);
    rpvm_chkerror (info, 1);

    PROTECT (sexp_vec = allocVector(INTSXP, len));
    info = pvm_upkint (INTEGER (sexp_vec), len, 1);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);

    return sexp_vec;
}

SEXP rpvm_upkdblvec ()
{
    int len;
    int info = 0;

    SEXP sexp_vec;

    /* Get length of the vector */
    info = pvm_upkint (&len, 1, 1);
    rpvm_chkerror (info, 1);

    PROTECT (sexp_vec = allocVector(REALSXP, len));
    info = pvm_upkdouble (REAL (sexp_vec), len, 1);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);
    return sexp_vec;
}

SEXP rpvm_upkintmat ()
{
    int info = 0;
    int dims[2];

    SEXP sexp_mat;

    info = pvm_upkint (dims, 2, 1);
    rpvm_chkerror (info, 1);

    PROTECT (sexp_mat = allocMatrix(INTSXP, dims[0], dims[1]));
    info = pvm_upkint (INTEGER (sexp_mat), dims[0] * dims[1], 1);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);
    return sexp_mat;
}


SEXP rpvm_upkdblmat ()
{
    int info = 0;
    int dims[2];

    SEXP sexp_mat;

    info = pvm_upkint (dims, 2, 1);
    rpvm_chkerror (info, 1);

    PROTECT (sexp_mat = allocMatrix(REALSXP, dims[0], dims[1]));
    info = pvm_upkdouble (REAL (sexp_mat), dims[0] * dims[1], 1);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);
    return sexp_mat;
}

/* Virtual machine control */

SEXP rpvm_config ()
{
    int nhost;
    int narch;
    struct pvmhostinfo *hostp;
    int info;
    SEXP sexp_info, sexp_tid, sexp_name, sexp_arch, sexp_speed;

    int i;

    info = pvm_config (&nhost, &narch, &hostp);
    rpvm_chkerror (info, 0);

    PROTECT (sexp_tid   = allocVector (INTSXP, nhost));
    PROTECT (sexp_name  = allocVector (STRSXP, nhost));
    PROTECT (sexp_arch  = allocVector (STRSXP, nhost));
    PROTECT (sexp_speed = allocVector (INTSXP, nhost));

    /* Info for one host */
    for (i = 0; i < nhost; ++i) {
        INTEGER (sexp_tid)[i] = hostp[i].hi_tid;
        SET_STRING_ELT (sexp_name,  i, mkChar (hostp[i].hi_name));
        SET_STRING_ELT (sexp_arch,  i, mkChar (hostp[i].hi_arch));
        INTEGER (sexp_speed)[i] = hostp[i].hi_speed;
    }
    PROTECT (sexp_info  = allocVector (VECSXP, 4));
    SET_VECTOR_ELT (sexp_info, 0, sexp_tid);
    SET_VECTOR_ELT (sexp_info, 1, sexp_name);
    SET_VECTOR_ELT (sexp_info, 2, sexp_arch);
    SET_VECTOR_ELT (sexp_info, 3, sexp_speed);

    UNPROTECT (5);
    return sexp_info;
}

SEXP rpvm_start_pvmd (SEXP sexp_argv, SEXP sexp_block)
{
    char **argv = toPPChar (sexp_argv);
    int argc = LENGTH (sexp_argv);
    int block = INTEGER (sexp_block)[0];
    return mkInt (rpvm_chkerror (pvm_start_pvmd (argc, argv, block), 1));
}

SEXP rpvm_addhosts (SEXP sexp_hosts)
{
    char **hosts = toPPChar (sexp_hosts);
    int nhost = LENGTH (sexp_hosts);
    SEXP sexp_infos;
    int info;

    PROTECT (sexp_infos  = allocVector (INTSXP, nhost));
    info = pvm_addhosts (hosts, nhost, INTEGER (sexp_infos));
    rpvm_chkerror (info, 1);
    setAttrib (sexp_infos, R_NamesSymbol, sexp_hosts);
    UNPROTECT (1);
    return sexp_infos;
}

SEXP rpvm_delhosts (SEXP sexp_hosts)
{
    char **hosts = toPPChar (sexp_hosts);
    int nhost = LENGTH (sexp_hosts);
    SEXP sexp_infos;
    int info;

    PROTECT (sexp_infos  = allocVector (INTSXP, nhost));
    info = pvm_delhosts (hosts, nhost, INTEGER (sexp_infos));
    rpvm_chkerror (info, 1);
    setAttrib (sexp_infos, R_NamesSymbol, sexp_hosts);
    UNPROTECT (1);
    return sexp_infos;
}

SEXP rpvm_halt ()
{
    return mkInt (rpvm_chkerror (pvm_halt (), 1));
}

SEXP rpvm_mstats (SEXP sexp_hosts)
{
    int i;
    int mstat;
    SEXP sexp_mstat;
    PROTECT (sexp_mstat  = allocVector (STRSXP, LENGTH (sexp_hosts)));
    for (i = 0; i < LENGTH (sexp_hosts); ++i) {
        mstat = pvm_mstat (CHAR (STRING_ELT (sexp_hosts, i)));
        switch (mstat) {
            case PvmOk :
                SET_STRING_ELT (sexp_mstat,  i, mkChar ("OK"));
                break;
            case PvmNoHost :
                SET_STRING_ELT (sexp_mstat,  i, mkChar ("Not in VM"));
                break;
            case PvmHostFail :
                SET_STRING_ELT (sexp_mstat,  i, mkChar ("Not Reachable"));
                break;
            default:
                SET_STRING_ELT (sexp_mstat,  i, mkChar ("Unknown"));
                break;
        }
    }
    setAttrib (sexp_mstat, R_NamesSymbol, sexp_hosts);
    UNPROTECT (1);
    return sexp_mstat;
}

/* Group Library */

SEXP rpvm_joingroup (SEXP sexp_group)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int inum = pvm_joingroup (group);
    /* instance number of the task in this group */
    return mkInt (rpvm_chkerror (inum, 1));
}

SEXP rpvm_lvgroup (SEXP sexp_group)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int info = pvm_lvgroup (group);
    return mkInt (rpvm_chkerror (info, 1));
}

SEXP rpvm_gettid (SEXP sexp_group, SEXP sexp_inums)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int i = 0;
    int tid;
    SEXP sexp_tids;
    PROTECT (sexp_tids = allocVector (INTSXP, LENGTH (sexp_inums)));
    for (i = 0; i < LENGTH (sexp_inums); ++i) {
        INTEGER (sexp_tids)[i] = rpvm_chkerror (
            pvm_gettid (group, INTEGER (sexp_inums)[i]), 0);
}
    UNPROTECT (1);
    return sexp_tids;
}

SEXP rpvm_getinst (SEXP sexp_group, SEXP sexp_tids)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int i = 0;
    SEXP sexp_inums;
    PROTECT (sexp_inums = allocVector (INTSXP, LENGTH (sexp_tids)));
    for (i = 0; i < LENGTH (sexp_tids); ++i) {
        INTEGER (sexp_inums)[i] = rpvm_chkerror (
            pvm_getinst (group, INTEGER (sexp_tids)[i]), 0);
    }
    UNPROTECT (1);
    return sexp_inums;
}

SEXP rpvm_gsize (SEXP sexp_group)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int size = pvm_gsize (group);
    return mkInt (rpvm_chkerror (size, 1));
}

SEXP rpvm_barrier (SEXP sexp_group, SEXP sexp_count)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int count = INTEGER (sexp_count)[0];
    int info = pvm_barrier (group, count);
    return mkInt (rpvm_chkerror (info, 1));
}

SEXP rpvm_bcast (SEXP sexp_group, SEXP sexp_msgtag)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int msgtag = INTEGER (sexp_msgtag)[0];
    int info = pvm_bcast (group, msgtag);
    return mkInt (rpvm_chkerror (info, 1));
}

SEXP rpvm_scatter_integer (SEXP sexp_data,
                           SEXP sexp_count,
                           SEXP sexp_msgtag,
                           SEXP sexp_group,
                           SEXP sexp_rootginst)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int count   = INTEGER (sexp_count)[0];
    int msgtag  = INTEGER (sexp_msgtag)[0];
    int rootginst = INTEGER (sexp_rootginst)[0];
    int info;

    SEXP sexp_result;

    PROTECT (sexp_result = allocVector (INTSXP, count));
    info = pvm_scatter ((void *) INTEGER (sexp_result),
                        (void *) INTEGER (sexp_data),
                        count, PVM_INT, msgtag, group, rootginst);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);
    return sexp_result;
}

SEXP rpvm_scatter_double (SEXP sexp_data,
                          SEXP sexp_count,
                          SEXP sexp_msgtag,
                          SEXP sexp_group,
                          SEXP sexp_rootginst)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int count   = INTEGER (sexp_count)[0];
    int msgtag  = INTEGER (sexp_msgtag)[0];
    int rootginst = INTEGER (sexp_rootginst)[0];
    int info;

    SEXP sexp_result;

    PROTECT (sexp_result = allocVector (REALSXP, count));
    info = pvm_scatter ((void *) REAL (sexp_result),
                        (void *) REAL (sexp_data),
                        count, PVM_DOUBLE, msgtag, group, rootginst);
    UNPROTECT (1);
    rpvm_chkerror (info, 1);
    return sexp_result;
}

SEXP rpvm_gather_integer (SEXP sexp_data,
                          SEXP sexp_count,
                          SEXP sexp_msgtag,
                          SEXP sexp_group,
                          SEXP sexp_rootginst)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int count   = INTEGER (sexp_count)[0];
    int msgtag  = INTEGER (sexp_msgtag)[0];
    int rootginst = INTEGER (sexp_rootginst)[0];
    int info;
    int gsize;
    int myinst = pvm_getinst (group, pvm_mytid ());
    int *result = 0;
    SEXP sexp_result;

    rpvm_chkerror (myinst, 1);

    /* allocate memory for root process */
    if (myinst == rootginst) {
        gsize = pvm_gsize (group);
        rpvm_chkerror (gsize, 1);
        PROTECT (sexp_result = allocVector (INTSXP, gsize * count));
        result = INTEGER (sexp_result);
    }
    info = pvm_gather ((void *) result,
                       (void *) INTEGER (sexp_data),
                       count, PVM_INT, msgtag, group, rootginst);
    rpvm_chkerror (info, 1);
    if (myinst == rootginst) {
        UNPROTECT (1);
        return sexp_result;
    } else {
        return mkInt (0);
    }
}

SEXP rpvm_gather_double (SEXP sexp_data,
                         SEXP sexp_count,
                         SEXP sexp_msgtag,
                         SEXP sexp_group,
                         SEXP sexp_rootginst)
{
    char *group = CHAR (STRING_ELT (sexp_group, 0));
    int count   = INTEGER (sexp_count)[0];
    int msgtag  = INTEGER (sexp_msgtag)[0];
    int rootginst = INTEGER (sexp_rootginst)[0];
    int info;
    int gsize;
    int myinst = pvm_getinst (group, pvm_mytid ());
    int *result = 0;
    SEXP sexp_result;

    rpvm_chkerror (myinst, 1);

    /* allocate memory for root process */
    if (myinst == rootginst) {
        gsize = pvm_gsize (group);
        rpvm_chkerror (gsize, 1);
        PROTECT (sexp_result = allocVector (REALSXP, gsize * count));
        result = INTEGER (sexp_result);
    }
    info = pvm_gather ((void *) result,
                       (void *) REAL (sexp_data),
                       count, PVM_DOUBLE, msgtag, group, rootginst);
    rpvm_chkerror (info, 1);
    if (myinst == rootginst) {
        UNPROTECT (1);
        return sexp_result;
    } else {
        return mkInt (0);
    }
}

SEXP rpvm_reduce_integer (SEXP sexp_data,
                          SEXP sexp_funcindex,
                          SEXP sexp_count,
                          SEXP sexp_msgtag,
                          SEXP sexp_group,
                          SEXP sexp_rootginst)
{
    char *group   = CHAR (STRING_ELT (sexp_group, 0));
    int count     = INTEGER (sexp_count)[0];
    int msgtag    = INTEGER (sexp_msgtag)[0];
    int rootginst = INTEGER (sexp_rootginst)[0];
    int info;
    int myinst;
    void (*func) ();

    SEXP sexp_result;

    myinst = pvm_getinst (group, pvm_mytid ());
    rpvm_chkerror (myinst, 1);
    switch (INTEGER (sexp_funcindex)[0]) {
        case 1 :
            func = PvmMin;
            break;
        case 2 :
            func = PvmMax;
            break;
        case 3 :
            func = PvmSum;
            break;
        case 4 :
            func = PvmProduct;
            break;
        default :
            error ("Unknown function specified.");
            return;
    }
    /* allocate memory for root process */
    info = pvm_reduce (func,
                       (void *) INTEGER (sexp_data),
                       count, PVM_INT, msgtag, group, rootginst);
    rpvm_chkerror (info, 1);
    if (myinst == rootginst) {
        return sexp_data;
    } else {
        return mkInt (0);
    }
}

SEXP rpvm_reduce_double (SEXP sexp_data,
                         SEXP sexp_funcindex,
                         SEXP sexp_count,
                         SEXP sexp_msgtag,
                         SEXP sexp_group,
                         SEXP sexp_rootginst)
{
    char *group   = CHAR (STRING_ELT (sexp_group, 0));
    int count     = INTEGER (sexp_count)[0];
    int msgtag    = INTEGER (sexp_msgtag)[0];
    int rootginst = INTEGER (sexp_rootginst)[0];
    int info;
    int myinst    = pvm_getinst (group, pvm_mytid ());
    int *result   = 0;
    void (*func) ();
    SEXP sexp_result;

    rpvm_chkerror (myinst, 1);
    switch (INTEGER (sexp_funcindex)[0]) {
        case 1 :
            func = PvmMin;
            break;
        case 2 :
            func = PvmMax;
            break;
        case 3 :
            func = PvmSum;
            break;
        case 4 :
            func = PvmProduct;
            break;
        default :
            error ("Unknown function specified.");
            return;
    }
    /* allocate memory for root process */
    info = pvm_reduce (func,
                       (void *) REAL (sexp_data),
                       count, PVM_DOUBLE, msgtag, group, rootginst);

    rpvm_chkerror (info, 1);
    if (myinst == rootginst) {
        return sexp_data;
    } else {
        return mkInt (0);
    }
}

/* Miscellaneous */

/**
 * Set options "what" to new value "val", return old value.
 */
SEXP rpvm_setopt (SEXP sexp_what, SEXP sexp_val)
{
    int what = INTEGER (sexp_what)[0];
    int val  = INTEGER (sexp_val)[0];
    return mkInt (rpvm_chkerror (pvm_setopt (what, val), 1));
}

/** Get current value of option "what" */
SEXP rpvm_getopt (SEXP sexp_what)
{
    int what = INTEGER (sexp_what)[0];
    return mkInt (rpvm_chkerror (pvm_getopt (what), 1));
}

/**
 * Returns the TID of the daemon running on the same host as tid.
 * Useful for determine on which host a given task is running
 */
SEXP rpvm_tidtohost (SEXP sexp_tid)
{
    int tid = INTEGER (sexp_tid)[0];
    return mkInt (rpvm_chkerror (pvm_tidtohost (tid), 1));
}

SEXP rpvm_notify (SEXP sexp_what, SEXP sexp_msgtag, SEXP sexp_tids)
{
    return mkInt (rpvm_chkerror (pvm_notify (INTEGER (sexp_what)[0],
                                             INTEGER (sexp_msgtag)[0],
                                             LENGTH (sexp_tids),
                                             INTEGER (sexp_tids)), 1));
}

SEXP rpvm_unnotify (SEXP sexp_what, SEXP sexp_msgtag, SEXP sexp_tids)
{
    return mkInt (rpvm_chkerror (pvm_notify (
                                     INTEGER (sexp_what)[0] | PvmNotifyCancel,
                                     INTEGER (sexp_msgtag)[0],
                                     LENGTH (sexp_tids),
                                     INTEGER (sexp_tids)), 1));
}

/* context management */

SEXP rpvm_newcontext ()
{
    return mkInt (pvm_newcontext ());
}

SEXP rpvm_setcontext (SEXP sexp_ctx)
{
    return mkInt (pvm_setcontext (INTEGER (sexp_ctx)[0]));
}

SEXP rpvm_freecontext (SEXP sexp_ctx)
{
    int info = pvm_freecontext (INTEGER (sexp_ctx)[0]);
    rpvm_chkerror (info, 0);
    return R_NilValue;
}

SEXP rpvm_getcontext ()
{
    int info = pvm_getcontext ();
    return mkInt (rpvm_chkerror (info, 1));
}

/* packing a message inside another message */

SEXP rpvm_pkmesg (SEXP sexp_bufid)
{
    int info = pvm_pkmesg (INTEGER (sexp_bufid)[0]);
    return mkInt (rpvm_chkerror (info, 1));
}

SEXP rpvm_upkmesg ()
{
    int newbufid = pvm_upkmesg ();
    return mkInt (rpvm_chkerror (newbufid, 1));
}

SEXP rpvm_pkmesgbody (SEXP sexp_bufid)
{
    int info = pvm_pkmesgbody (INTEGER (sexp_bufid)[0]);
    return mkInt (rpvm_chkerror (info, 1));
}

/* global mail box */

SEXP rpvm_putinfo (SEXP sexp_name, SEXP sexp_bufid, SEXP sexp_flags)
{
    int bufid = INTEGER (sexp_bufid)[0];
    int flags  = INTEGER (sexp_flags)[0];
    char *name = CHAR (STRING_ELT (sexp_name, 0));
    int index = pvm_putinfo (name, bufid, flags);
    return mkInt (rpvm_chkerror (index, 1));
}

SEXP rpvm_recvinfo (SEXP sexp_name, SEXP sexp_index, SEXP sexp_flags)
{
    int index = INTEGER (sexp_index)[0];
    int flags  = INTEGER (sexp_flags)[0];
    char *name = CHAR (STRING_ELT (sexp_name, 0));
    int bufid = pvm_recvinfo (name, index, flags);
    return mkInt (rpvm_chkerror (bufid, 1));
}

SEXP rpvm_delinfo (SEXP sexp_name, SEXP sexp_index, SEXP sexp_flags)
{
    int index = INTEGER (sexp_index)[0];
    int flags  = INTEGER (sexp_flags)[0];
    char *name = CHAR (STRING_ELT (sexp_name, 0));
    int info = pvm_delinfo (name, index, flags);
}

SEXP rpvm_getmboxinfo (SEXP sexp_pattern, SEXP sexp_classes)
{
    char *pattern = CHAR (STRING_ELT (sexp_pattern, 0));
    int nclasses = LENGTH (sexp_classes);
}

/* Message Handling */
/* A sample message hander, mid is the bufid of the received message */
int rpvm_hostadded( int mid )
{
    int n;
    pvm_unpackf ("%d", &n);
    Rprintf ( "*** %d new hosts just added ***\n", n );
    return 0;
}

SEXP rpvm_notify_hostadded( )
{
    int tag = 99;
    int mhid = pvm_addmhf (-1, tag, -1, rpvm_hostadded );
    int info = pvm_notify (PvmHostAdd, tag, -1, 0);
    /* return Message handler id */
    return mkInt (mhid);
}

SEXP rpvm_unnotify_hostadded( SEXP s_mhid )
{
    int tag = 99;
    int info = pvm_delmhf (INTEGER(s_mhid)[0]);
    info = pvm_notify (PvmHostAdd | PvmNotifyCancel, tag, -1, 0);
    return R_NilValue;
}


/* functions not yet implemented and may never be */
/*
 * pvm_addmhf, pvm_delmhf
 * pvm_freezegroup
 * pvm_gettmask,  pvm_settmask
 * pvm_reg_hoster, pvm_reg_rm, pvm_reg_tasker, pvm_reg_tracer
 * pvm_getfds
 * pvm_archcode
 * pvm_catchout
 * pvm_export,  pvm_unexport
 * pvm_hostsync
 * pvm_sendsig
 * pvm_siblings
 * pvm_getnoresets
 */
