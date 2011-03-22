#include <stdio.h>
#include "erl_nif.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    fprintf(stderr, "loading\r\n");
    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    fprintf(stderr, "reloading\r\n");
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    fprintf(stderr, "upgrading\r\n");
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    fprintf(stderr, "unloading\r\n");
    return;
}

static ERL_NIF_TERM
myfun(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello, world!", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"myfun", 0, myfun}
};

ERL_NIF_INIT(reloading, nif_funcs, &load, &reload, &upgrade, &unload);
