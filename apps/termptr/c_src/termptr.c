#include <assert.h>
#include <stdio.h>

#include "erl_nif.h"

typedef struct {
    ErlNifEnv*      env;
    ERL_NIF_TERM    data;
} termptr;

void
termptr_dtor(ErlNifEnv* env, void* obj)
{
    termptr* ptr = (termptr*) obj;
    assert(ptr->env != NULL && "Invalid termptr.");
    enif_free_env(ptr->env);
}

ErlNifResourceType* ptr_type;

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ptr_type = enif_open_resource_type(
            env, NULL, "termptr", termptr_dtor, flags, NULL
        );
    if(ptr_type == NULL) return 1;
    return 0;
}

static ERL_NIF_TERM
wrap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    termptr* ptr = (termptr*) enif_alloc_resource(ptr_type, sizeof(termptr));
    ERL_NIF_TERM ret;

    ptr->env = enif_alloc_env();

    if(argc != 1) {
        ret = enif_make_badarg(env);
        goto done;
    }

    ptr->data = enif_make_copy(ptr->env, argv[0]);

    ret = enif_make_resource(env, ptr);

done:
    enif_release_resource(ptr);
    return ret;
}

static ERL_NIF_TERM
unwrap(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    termptr* ptr;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], ptr_type, (void**) &ptr))
        return enif_make_badarg(env);
   
    return enif_make_copy(env, ptr->data);
}

static ErlNifFunc funcs[] =
{
    {"wrap", 1, wrap},
    {"unwrap", 1, unwrap}
};

ERL_NIF_INIT(termptr, funcs, &load, NULL, NULL, NULL);
