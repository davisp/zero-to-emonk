#include <unistd.h>
#include "erl_nif.h"

static ERL_NIF_TERM
nif_sleep(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    unsigned int num;
    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_uint(env, argv[0], &num))
        return enif_make_badarg(env);

    sleep(num);

    return enif_make_int(env, 0);
}

static ErlNifFunc funcs[] =
{
    {"sleep", 1, nif_sleep}
};

ERL_NIF_INIT(badnif, funcs, NULL, NULL, NULL, NULL);
