#include <stdio.h>
#include <unistd.h>
#include "erl_nif.h"

typedef struct {
    ErlNifTid           tid;
    ErlNifThreadOpts*   opts;
    ErlNifPid           dst;
    ErlNifEnv*          env;
    ERL_NIF_TERM        msg;
    unsigned int        delay;
} state;

void*
state_run(void* obj)
{
    state* st = (state*) obj;
    enif_keep_resource(st);

    sleep(st->delay);

    enif_send(NULL, &(st->dst), st->env, st->msg);

    enif_release_resource(st);

    return NULL;
}

void
state_dtor(ErlNifEnv* env, void* obj)
{
    state* st = (state*) obj;
    void* ret;
    enif_thread_join(st->tid, &ret);
    enif_thread_opts_destroy(st->opts);
    enif_free_env(st->env);
}

ErlNifResourceType* st_type;

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    st_type = enif_open_resource_type(
            env, NULL, "state", state_dtor, flags, NULL
        );
    if(st_type == NULL) return 1;
    return 0;
}

static ERL_NIF_TERM
delayed_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    state* st;
    int status;
    ERL_NIF_TERM ret;

    st = enif_alloc_resource(st_type, sizeof(state));
    st->opts = enif_thread_opts_create("state");
    st->env = enif_alloc_env();

    if(argc != 3) {
        ret = enif_make_badarg(env);
        goto done;
    } else if(!enif_get_local_pid(env, argv[0], &(st->dst))) {
        ret = enif_make_badarg(env);
        goto done;
    } else if(!enif_get_uint(env, argv[2], &(st->delay))) {
        ret = enif_make_badarg(env);
        goto done;
    }

    st->msg = enif_make_copy(st->env, argv[1]);

    status = enif_thread_create("state", &(st->tid), state_run, st, st->opts);
    if(status != 0) {
        ret = enif_make_badarg(env);
        goto done;
    }

    ret = enif_make_resource(env, st);

done:
    enif_release_resource(st);
    return ret;
}

static ErlNifFunc funcs[] =
{
    {"delayed_send", 3, delayed_send}
};

ERL_NIF_INIT(goodnif, funcs, &load, NULL, NULL, NULL);
