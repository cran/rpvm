/*
 * $Id: rpvm_core.c,v 1.8 2001/08/29 20:53:02 snake Exp $
 */

#include <R.h>
#include <Rdefines.h>

#include <pvm3.h>

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
    ppchar = (char **) R_alloc (len, sizeof (char *));
    for (i = 0; i < len; ++i) {
        ppchar[i] = CHAR (STRING_ELT (sexp_str, i));
    }
    return ppchar;
}

/* {{{ Process control */

SEXP rpvm_mytid ()
{
    return mkInt (pvm_mytid ());
}

SEXP rpvm_parent ()
{
    return mkInt ( pvm_parent () );
}

SEXP rpvm_exit ()
{
    return mkInt (pvm_exit());
}

SEXP rpvm_pstats (SEXP sexp_tids)
{
    int i;
    SEXP sexp_pstat;
    PROTECT (sexp_pstat  = allocVector (INTSXP, LENGTH (sexp_tids)));
    for (i = 0; i < LENGTH (sexp_tids); ++i) {
        INTEGER (sexp_pstat)[i] = pvm_pstat (INTEGER (sexp_tids)[i]);
    }
    setAttrib (sexp_pstat, R_NamesSymbol, sexp_tids);
    UNPROTECT (1);
    return sexp_pstat;
}


SEXP rpvm_kill (SEXP sexp_tid)
{
    int n = LENGTH (sexp_tid);
    int i;
    int info;
    for (i = 0; i < n; ++i) {
        info = pvm_kill (INTEGER (sexp_tid)[i]);
    }
    return mkInt (info);
}

SEXP rpvm_tasks (SEXP sexp_where)
{
    int where = INTEGER (sexp_where) [0];
    int info, ntask, i;
    struct pvmtaskinfo *taskp;
    SEXP sexp_tasks, sexp_tid, sexp_ptid, sexp_host, sexp_flag, sexp_aout;

    info = pvm_tasks (where, &ntask, &taskp );
    if (info < 0) {
        return mkInt (info);
    }

    PROTECT (sexp_tid   = allocVector (INTSXP, ntask));
    PROTECT (sexp_ptid  = allocVector (INTSXP, ntask));
    PROTECT (sexp_host  = allocVector (INTSXP, ntask));
    PROTECT (sexp_flag  = allocVector (INTSXP, ntask));
    PROTECT (sexp_aout  = allocVector (STRSXP, ntask));

    for (i = 0; i < ntask; ++i) {
        INTEGER (sexp_tid)[i]  = taskp[i].ti_tid;
        INTEGER (sexp_ptid)[i] = taskp[i].ti_ptid;
        INTEGER (sexp_host)[i] = taskp[i].ti_host;
        INTEGER (sexp_flag)[i] = taskp[i].ti_flag;
        SET_STRING_ELT (sexp_aout, i, mkChar (taskp[i].ti_a_out));
    }

    PROTECT (sexp_tasks = allocVector (VECSXP, 5));
    SET_VECTOR_ELT (sexp_tasks, 0, sexp_tid);
    SET_VECTOR_ELT (sexp_tasks, 1, sexp_ptid);
    SET_VECTOR_ELT (sexp_tasks, 2, sexp_host);
    SET_VECTOR_ELT (sexp_tasks, 3, sexp_flag);
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
                 SEXP sexp_arglist)
{
    int numt;
    int ntask = INTEGER (sexp_ntask)[0];
    int flag  = INTEGER (sexp_flag)[0];

    char **arglist = toPPChar (sexp_arglist);

    SEXP sexp_tids;

    PROTECT (sexp_tids  = allocVector (INTSXP, ntask));

    /* spawn tasks */
    numt = pvm_spawn (CHAR (STRING_ELT (sexp_task, 0)),
                      arglist,
                      flag,
                      CHAR (STRING_ELT (sexp_where, 0)),
                      ntask,
                      INTEGER (sexp_tids));
    UNPROTECT (1);
    return sexp_tids;
}

/* }}} */

/* {{{ Message Passing */

/* {{{ Msssage Buffers */

/**
 * Clears the send buffer and creates a new one for packing a new package.
 * Return the buffer id.
 */

SEXP rpvm_initsend (SEXP sexp_encode)
{
    int encode = INTEGER (sexp_encode)[0];
    return mkInt (pvm_initsend (encode));
}

/**
 * Create a new empty send buffer and specifies the encoding method.
 * Return the buffer id.
 */
SEXP rpvm_mkbuf (SEXP sexp_encode)
{
    int encode = INTEGER (sexp_encode)[0];
    return mkInt (pvm_mkbuf (encode));
}

/**
 * Dispose a specified buffer
 */
SEXP rpvm_freebuf (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];
    return mkInt (pvm_freebuf (bufid));
}

/**
 * Returns the active send buffer id.
 */
SEXP rpvm_getsbuf ()
{
    return mkInt (pvm_getsbuf ());
}

/**
 * Returns the active receive buffer id.
 */
SEXP rpvm_getrbuf ()
{
    return mkInt (pvm_getrbuf ());
}

/**
 * Set the active send buffer, save the state of the previous buffer.
 * Returns the previous buffer's id.
 */
SEXP rpvm_setsbuf (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];
    return mkInt (pvm_setsbuf (bufid));
}

/**
 * Set the active receive buffer, save the state of the previous buffer.
 * Returns the previous buffer's id.
 */
SEXP rpvm_setrbuf (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];
    return mkInt (pvm_setrbuf (bufid));
}

/* }}} */

/* {{{ Packing data */

SEXP rpvm_pkint (SEXP sexp_np, SEXP sexp_stride)
{
    int nitem = length (sexp_np);
    int stride = INTEGER (sexp_stride)[0];
    int *np = INTEGER (sexp_np);

    return mkInt (pvm_pkint (np, nitem, stride));
}

SEXP rpvm_pkdouble (SEXP sexp_np, SEXP sexp_stride)
{
    int nitem = length (sexp_np);
    int stride = INTEGER (sexp_stride)[0];
    double *np = REAL (sexp_np);

    return mkInt (pvm_pkdouble (np, nitem, stride));
}

SEXP rpvm_pkstr (SEXP sexp_cp)
{
    char *cp = CHAR (STRING_ELT (sexp_cp, 0));
    return mkInt (pvm_pkstr (cp));
}

SEXP rpvm_pkintvec (SEXP sexp_vec)
{
    int len = length (sexp_vec);
    int val;

    val = pvm_pkint (&len, 1, 1);
    val = pvm_pkint (INTEGER (sexp_vec), len, 1);
    return mkInt (val);
}


SEXP rpvm_pkdblvec (SEXP sexp_vec)
{
    int len = length (sexp_vec);
    int val;

    val = pvm_pkint (&len, 1, 1);
    val = pvm_pkdouble (REAL (sexp_vec), len, 1);
    return mkInt (val);
}

SEXP rpvm_pkstrvec (SEXP sexp_str)
{
    int len = LENGTH (sexp_str);
    int i, val;
    val = pvm_pkint (&len, 1, 1);

    for (i = 0; i < len; ++i) {
        val = pvm_pkstr (CHAR (STRING_ELT (sexp_str, i)));
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
    /* Pack data */
    val = pvm_pkint (pmat, dims[0]*dims[1], 1);

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
    /* Pack data */
    val = pvm_pkdouble (pmat, dims[0]*dims[1], 1);

    UNPROTECT(1);
    return mkInt (val);
}

/* }}} */

/* {{{ Sending and receving data */

SEXP rpvm_send (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (pvm_send (tid, msgtag));
}

/**
 * Broadcase a message to all tasks in vector sexp_tids.
 */
SEXP rpvm_mcast (SEXP sexp_tids,  SEXP sexp_msgtag)
{
    int *tids  = INTEGER (sexp_tids);
    int ntask  = length (sexp_tids);
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (pvm_mcast (tids, ntask, msgtag));
}

SEXP rpvm_recv (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (pvm_recv (tid, msgtag));
}

/**
 * Nonblocking receive, return 0 if message not received.
 */
SEXP rpvm_nrecv (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (pvm_nrecv (tid, msgtag));
}

/**
 * Check if message arrived.
 */
SEXP rpvm_probe (SEXP sexp_tid, SEXP sexp_msgtag)
{
    int tid    = INTEGER (sexp_tid)[0];
    int msgtag = INTEGER (sexp_msgtag)[0];
    return mkInt (pvm_probe (tid, msgtag));
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
        return mkInt (pvm_trecv (tid, msgtag, (struct timeval*) 0));
    }
    tmout.tv_sec  = REAL (sexp_tmout)[0];

    if (length (sexp_tmout) > 1) {
        tmout.tv_usec = REAL (sexp_tmout)[1];
    } else {
        tmout.tv_usec = 0.0;
    }

    return mkInt (pvm_trecv (tid, msgtag, &tmout));
}

SEXP rpvm_bufinfo (SEXP sexp_bufid)
{
    int bufid = INTEGER (sexp_bufid)[0];

    int infoflag, bytes, msgtag, tid;

    SEXP sexp_info;

    infoflag = pvm_bufinfo (bufid, &bytes, &msgtag, &tid);

    if (infoflag < 0) {
        error ("Error receiving message");
        return mkInt ( infoflag );
    } else {
        PROTECT (sexp_info = allocVector (INTSXP, 3));
        INTEGER (sexp_info)[0] = bytes;
        INTEGER (sexp_info)[1] = msgtag;
        INTEGER (sexp_info)[2] = tid;
        UNPROTECT (1);
        return sexp_info;
    }
}

/* }}} */

/* {{{ Unpacking data */

SEXP rpvm_upkint (SEXP sexp_nitem, SEXP sexp_stride )
{
    int infoflag;

    SEXP sexp_ans;

    int nitem  = INTEGER (sexp_nitem)[0];
    int stride = INTEGER (sexp_stride)[0];

    PROTECT (sexp_ans = allocVector (INTSXP, nitem));
    infoflag = pvm_upkint (INTEGER (sexp_ans), nitem, stride);
    UNPROTECT (1);

    if (infoflag < 0) {
        error ("Error unpacking integers.");
        return mkInt (infoflag);
    } else {
        return sexp_ans;
    }
}

SEXP rpvm_upkdouble (SEXP sexp_nitem, SEXP sexp_stride)
{
    SEXP sexp_ans;
    int infoflag;
    int nitem  = INTEGER (sexp_nitem)[0];
    int stride = INTEGER (sexp_stride)[0];

    PROTECT (sexp_ans = allocVector (REALSXP, nitem));
    infoflag = pvm_upkdouble (REAL (sexp_ans), nitem, stride);
    UNPROTECT (1);

    if (infoflag < 0) {
        error ("Error unpacking real numbers.");
        return mkInt (infoflag);
    } else {
        return sexp_ans;
    }
}

SEXP rpvm_upkstr (SEXP sexp_maxlen)
{
    int maxlen = INTEGER (sexp_maxlen)[0];
    int i;
    int infoflag;
    char *ans;

    ans = (char *) R_alloc (maxlen, sizeof(char));
    infoflag = pvm_upkstr (ans);

    if (infoflag < 0) {
        error ("Error unpacking string message.");
        return mkInt (infoflag);
    } else {
        return mkString (ans);
    }
}

SEXP rpvm_upkstrvec (SEXP sexp_maxlen)
{
    int maxlen = INTEGER (sexp_maxlen)[0];
    int len;
    int i;
    int info_flag;
    char *tmp;

    SEXP sexp_ans;

    tmp = (char *) R_alloc (maxlen, sizeof (char));
    /* No. of strings */
    info_flag = pvm_upkint (&len, 1, 1);

    PROTECT (sexp_ans = allocVector (STRSXP, len));
    for (i = 0; i < len; ++i) {
        info_flag = pvm_upkstr (tmp);
        SET_STRING_ELT(sexp_ans, i, COPY_TO_USER_STRING(tmp));
    }
    UNPROTECT (1);
    return sexp_ans;
}

SEXP rpvm_upkintvec ()
{
    int len;
    int info_flag = 0;

    SEXP sexp_vec;

    /* Get length of the vector */
    info_flag = pvm_upkint (&len, 1, 1);
    if (info_flag < 0) {
        error ("Error retrieving vector length.");
        return mkInt (info_flag);
    }

    PROTECT (sexp_vec = allocVector(INTSXP, len));
    info_flag = pvm_upkint (INTEGER (sexp_vec), len, 1);
    UNPROTECT (1);
    if (info_flag < 0) {
        error ("Error retrieving vector.");
        return mkInt (info_flag);
    }
    return sexp_vec;
}

SEXP rpvm_upkdblvec ()
{
    int len;
    int info_flag = 0;

    SEXP sexp_vec;

    /* Get length of the vector */
    info_flag = pvm_upkint (&len, 1, 1);
    if (info_flag < 0) {
        error ("Error retrieving vector length.");
        return mkInt (info_flag);
    }

    PROTECT (sexp_vec = allocVector(REALSXP, len));
    info_flag = pvm_upkdouble (REAL (sexp_vec), len, 1);
    UNPROTECT (1);
    if (info_flag < 0) {
        error ("Error retrieving vector.");
        return mkInt (info_flag);
    }
    return sexp_vec;
}

SEXP rpvm_upkintmat ()
{
    int info_flag = 0;
    int dims[2];

    SEXP sexp_mat;

    info_flag = pvm_upkint (dims, 2, 1);
    if (info_flag < 0) {
        error ("Error retrieving matrix dimenstions.");
        return mkInt (info_flag);
    }

    PROTECT (sexp_mat = allocMatrix(INTSXP, dims[0], dims[1]));
    info_flag = pvm_upkint (INTEGER (sexp_mat), dims[0] * dims[1], 1);
    UNPROTECT (1);
    if (info_flag < 0) {
        error ("Error retrieving matrix.");
        return mkInt (info_flag);
    }
    return sexp_mat;
}


SEXP rpvm_upkdblmat ()
{
    int info_flag = 0;
    int dims[2];

    SEXP sexp_mat;

    info_flag = pvm_upkint (dims, 2, 1);
    if (info_flag < 0) {
        error ("Error retrieving matrix dimenstions.");
        return mkInt (info_flag);
    }

    PROTECT (sexp_mat = allocMatrix(REALSXP, dims[0], dims[1]));
    info_flag = pvm_upkdouble (REAL (sexp_mat), dims[0] * dims[1], 1);
    UNPROTECT (1);
    if (info_flag < 0) {
        error ("Error retrieving matrix.");
        return mkInt (info_flag);
    }
    return sexp_mat;
}

/* }}} */

/* }}} */

/* {{{ Virtual machine control */

SEXP rpvm_config ()
{
    int nhost;
    int narch;
    struct pvmhostinfo *hostp;
    int info;
    SEXP sexp_info, sexp_tid, sexp_name, sexp_arch, sexp_speed;

    int i;

    info = pvm_config (&nhost, &narch, &hostp);
    if (info < 0) {
        error ("Error getting PVM configuration");
    }

    Rprintf ("There are %d hosts and %d architectures.\n", nhost, narch);

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
    return mkInt (pvm_start_pvmd (argc, argv, block));
}

SEXP rpvm_addhosts (SEXP sexp_hosts)
{
    char **hosts = toPPChar (sexp_hosts);
    int nhost = LENGTH (sexp_hosts);
    SEXP sexp_infos;
    int info;

    PROTECT (sexp_infos  = allocVector (INTSXP, nhost));
    info = pvm_addhosts (hosts, nhost, INTEGER (sexp_infos));
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
    setAttrib (sexp_infos, R_NamesSymbol, sexp_hosts);
    UNPROTECT (1);
    return sexp_infos;
}

SEXP rpvm_halt ()
{
    return mkInt (pvm_halt ());
}

SEXP rpvm_mstats (SEXP sexp_hosts)
{
    int i;
    SEXP sexp_mstat;
    PROTECT (sexp_mstat  = allocVector (INTSXP, LENGTH (sexp_hosts)));
    for (i = 0; i < LENGTH (sexp_hosts); ++i) {
        INTEGER (sexp_mstat)[i] = pvm_mstat (CHAR
                                             (STRING_ELT (sexp_hosts, i)));
    }
    setAttrib (sexp_mstat, R_NamesSymbol, sexp_hosts);
    UNPROTECT (1);
    return sexp_mstat;
}

/* }}} */

/* {{{ Miscellaneous

/**
 * Set options "what" to new value "val", return old value.
 */
SEXP rpvm_setopt (SEXP sexp_what, SEXP sexp_val)
{
    int what = INTEGER (sexp_what)[0];
    int val  = INTEGER (sexp_val)[0];
    return mkInt (pvm_setopt (what, val));
}

/** Get current value of option "what" */
SEXP rpvm_getopt (SEXP sexp_what)
{
    int what = INTEGER (sexp_what)[0];
    return mkInt (pvm_getopt (what));
}

/**
 * Returns the TID of the daemon running on the same host as tid.
 * Useful for determine on which host a given task is running
 */
SEXP rpvm_tidtohost (SEXP sexp_tid)
{
    int tid = INTEGER (sexp_tid)[0];
    return mkInt (pvm_tidtohost (tid));
}

SEXP rpvm_notify (SEXP sexp_what, SEXP sexp_msgtag, SEXP sexp_tids)
{
    return mkInt (pvm_notify (INTEGER (sexp_what)[0],
                              INTEGER (sexp_msgtag)[0],
                              LENGTH (sexp_tids),
                              INTEGER (sexp_tids)));
}

/* }}} */
