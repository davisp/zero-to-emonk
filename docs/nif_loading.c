#include "erl_nif.h"

// Called in response to erlang:load_nif/2 when loading
// this NIF.
//
// Generally used as a place to create some sort of
// global scope that is then stored in priv.
int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info);


// When someone calls erlang:load_nif/2 and the NIF
// has already been loaded. Can be used to affect
// state in priv.
int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info);


// I have no idea how to trigger this or under
// which exact conditions it'll execute.
int upgrade(ErlNifEnv* env, void** priv, void** oldpriv, ERL_NIF_TERM info)

// Called when this NIF is being unloaded from the
// Erlang VM. It is not guranteed to be called.
void unload(ErlNifEnv* env, void* priv);


ERL_NIF_INIT(reloading, funcs, &load, &reload, &upgrade, &unload);
