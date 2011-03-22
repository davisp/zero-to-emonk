#include "erl_nif.h"

static ERL_NIF_TERM
say_hi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    const char* mesg = "Hello, World!";
    return enif_make_string(env, mesg, ERL_NIF_LATIN1);
}

static ErlNifFunc nif_funcs[] =
{
    {"say_hi", 0, say_hi}
};

ERL_NIF_INIT(first_nif, nif_funcs, NULL, NULL, NULL, NULL);
