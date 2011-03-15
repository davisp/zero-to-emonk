
#include "erl_nif.h"

static int
on_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
on_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
on_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}

static ERL_NIF_TERM
sayhi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_string(env, "Hello, world!", ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"sayhi", 0, sayhi}
};

ERL_NIF_INIT(first_nif, nif_funcs, &on_load, &on_reload, &on_upgrade, NULL);
