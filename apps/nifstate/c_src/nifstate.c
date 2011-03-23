#include <assert.h>
#include "erl_nif.h"

typedef struct {
    ErlNifMutex*    lock;
    int             global;
} state;

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    state* st = enif_alloc(sizeof(state));
    
    if(st == NULL) return 1;
    
    st->lock = enif_mutex_create("lock");
    if(st->lock == NULL) {
        enif_free(st);
        return 1;
    }
    
    if(!enif_get_int(env, info, &(st->global)))
        st->global = 0;

    *priv = (void*) st;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    state* st = (state*) (*priv);
    
    if(!enif_get_int(env, info, &(st->global)))
        st->global = 0;
    
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    if(priv != NULL) enif_free(priv);
}

static ERL_NIF_TERM
curr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state* st = (state*) enif_priv_data(env);
    int ret;

    enif_mutex_lock(st->lock);
    ret = st->global;
    enif_mutex_unlock(st->lock);

    return enif_make_int(env, ret);
}

static ERL_NIF_TERM
incr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state* st = (state*) enif_priv_data(env);
    int ret;

    enif_mutex_lock(st->lock);
    ret = ++st->global;
    enif_mutex_unlock(st->lock);

    return enif_make_int(env, ret);
}

static ERL_NIF_TERM
decr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state* st = (state*) enif_priv_data(env);
    int ret;

    enif_mutex_lock(st->lock);
    ret = --st->global;
    enif_mutex_unlock(st->lock);

    return enif_make_int(env, ret);
}

static ERL_NIF_TERM
swap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state* st = (state*) enif_priv_data(env);
    int val;
    int ret;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[0], &val))
        return enif_make_badarg(env);

    enif_mutex_lock(st->lock);
    ret = st->global;
    st->global = val;
    enif_mutex_unlock(st->lock);

    return enif_make_int(env, ret);
}

static ErlNifFunc funcs[] =
{
    {"curr", 0, curr},
    {"incr", 0, incr},
    {"decr", 0, decr},
    {"swap", 1, swap}
};

ERL_NIF_INIT(nifstate, funcs, &load, &reload, NULL, &unload);
