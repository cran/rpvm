/*
 * Wrapper to core functions of sprng.
 * $Id: sprng_core.c,v 1.3 2001/12/04 00:07:09 snake Exp $
 *
 **/

#ifdef HAVE_SPRNG

#include "R_ext/Random.h"
#include "utils.h"
#define CHECK_POINTERS
#include "sprng.h"

static int *streamid = 0;
static double rn;

SEXP r_init_sprng (SEXP sexp_gtype,
                   SEXP sexp_streamno,
                   SEXP sexp_nstreams,
                   SEXP sexp_seed,
                   SEXP sexp_param)
{
    int gtype    = INTEGER (sexp_gtype)[0];
    int streamno = INTEGER (sexp_streamno)[0];
    int nstreams = INTEGER (sexp_nstreams)[0];
    int seed     = INTEGER (sexp_seed)[0];
    int param    = INTEGER (sexp_param)[0];

    streamid = init_sprng (gtype, streamno, nstreams, seed, param);
    return R_NilValue;
}

SEXP r_pack_sprng ()
{
    char *rng_buffer;
    int len = 0;
    if (streamid) {
        pack_sprng (streamid, &rng_buffer);
        return mkString (rng_buffer);
    } else {
        return R_NilValue;
    }
}

SEXP r_unpack_sprng (SEXP sexp_packed_stream)
{
    char *oldrng_buffer;
    if (streamid) {
        pack_sprng (streamid, &oldrng_buffer);
        free_sprng (streamid);
    }
    streamid = unpack_sprng (CHAR (STRING_ELT (sexp_packed_stream, 0)));
    if (streamid) {
        return mkString (oldrng_buffer);
    } else {
        return R_NilValue;
    }
}

SEXP r_free_sprng ()
{
    char *oldrng_buffer;
    int nstream;
    if (streamid) {
        pack_sprng (streamid, &oldrng_buffer);
        nstream = free_sprng (streamid);    
        streamid = 0;
        return mkString (oldrng_buffer);
    } else {
        return R_NilValue;
    }
}

SEXP r_spawn_sprng (SEXP sexp_nspawned)
{
    int  **newstreams;
    char **pkdstr;
    int i;
    int nspawned = spawn_sprng (streamid, INTEGER (sexp_nspawned)[0],
                                &newstreams);
    SEXP sexp_spawned_streams;
    PROTECT (sexp_spawned_streams = allocVector (STRSXP, nspawned));
    pkdstr = (char **) R_alloc (nspawned, sizeof (char *));
    for (i = 0; i < nspawned; ++i) {
        pack_sprng (newstreams[i], &pkdstr[i]);
        SET_STRING_ELT (sexp_spawned_streams, i,
                        COPY_TO_USER_STRING (pkdstr[i]));
        if (pkdstr[i]) {
            free (pkdstr[i]);
            pkdstr[i] = 0;
        }
        free_sprng (newstreams[i]);
    }
    UNPROTECT (1);
    return sexp_spawned_streams;
}

double *user_unif_rand ()
{
    rn = sprng (streamid);
    return &rn;
}

#endif
