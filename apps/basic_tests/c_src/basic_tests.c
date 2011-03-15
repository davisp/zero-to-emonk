#include <assert.h>
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
mk_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
    {
        return ret;
    }

    return enif_make_atom(env, name);
}

static ERL_NIF_TERM
same_term(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 2 && "same_term is a 2-arity function.");
    if(enif_is_identical(argv[0], argv[1]))
    {
        return mk_atom(env, "true");
    }
    else
    {
        return  mk_atom(env, "false");
    }
}

static ERL_NIF_TERM
cmp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    assert(argc == 2 && "cmp is a 2-arity function.");
    int c = enif_compare(argv[0], argv[1]);
    if(c < 0)
    {
        return mk_atom(env, "lesser");
    }
    else if(c == 0)
    {
        return mk_atom(env, "equal");
    }
    else
    {
        return mk_atom(env, "greater");
    }
}

static ErlNifFunc nif_funcs[] =
{
    {"same_term", 2, same_term},
    {"cmp", 2, cmp}
};

ERL_NIF_INIT(basic_tests, nif_funcs, &on_load, &on_reload, &on_upgrade, NULL);
