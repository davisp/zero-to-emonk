#include "erl_nif.h"

static ERL_NIF_TERM
send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifEnv* cp_env = enif_alloc_env();
    ErlNifPid dst;
    ERL_NIF_TERM msg;
    ERL_NIF_TERM ret;
    
    if(argc == 1) {
        if(!enif_self(env, &dst)) {
            ret = enif_make_badarg(env);
            goto done;
        }
        msg = argv[0];
    } else if(argc == 2) {
        if(!enif_get_local_pid(env, argv[0], &dst)) {
            ret = enif_make_badarg(env);
            goto done;
        }
        msg = argv[1];
    } else {
        ret = enif_make_badarg(env);
        goto done;
    }

    msg = enif_make_copy(cp_env, msg);

    if(!enif_send(env, &dst, cp_env, msg)) {
        ret = enif_make_badarg(env);
        goto done;
    }
    
    ret = enif_make_atom(env, "ok");

done:
    enif_free_env(cp_env);
    return ret;
}

static ErlNifFunc funcs[] =
{
    {"send", 1, send},
    {"send", 2, send}
};

ERL_NIF_INIT(termsend, funcs, NULL, NULL, NULL, NULL);
