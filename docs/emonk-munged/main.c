// This file is part of Emonk released under the MIT license. 
// See the LICENSE file for more information.

#include <string.h>

#include "erl_nif.h"

#include "alias.h"
#include "util.h"
#include "vm.h"

#define GC_THRESHOLD 10485760 // 10 MiB
#define MAX_BYTES 8388608
#define MAX_MALLOC_BYTES 8388608
#define MAX_WORKERS 64

struct state_t
{
    ErlNifResourceType*     res_type;
    JSRuntime*              runtime;
};

typedef struct state_t* state_ptr;

static int
load(ErlNifEnv* env, void** priv, ENTERM load_info)
{
    ErlNifResourceType* res;
    state_ptr state = (state_ptr) enif_alloc(sizeof(struct state_t));
    const char* name = "Context";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    JS_SetCStringsAreUTF8();

    if(state == NULL)
        return 1;

    state->res_type = NULL;
    state->runtime = NULL;

    res = enif_open_resource_type(env, NULL, name, vm_destroy, flags, NULL);
    if(res == NULL) {
        enif_free(state);
        return 1;
    }
    state->res_type = res;
    state->runtime = init_js_runtime();
    
    *priv = (void*) state;
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    state_ptr state = (state_ptr) priv;
    if(state->runtime != NULL) JS_DestroyRuntime(state->runtime);
    enif_free(state);
}

static ENTERM
create_ctx(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    unsigned int stack_size;
    vm_ptr vm;
    ENTERM ret;

    if(argc != 1 || !enif_get_uint(env, argv[0], &stack_size))
    {
        return enif_make_badarg(env);
    }

    vm = vm_init(state->res_type, state->runtime, (size_t) stack_size);
    if(vm == NULL) return util_mk_error(env, "vm_init_failed");
    
    ret = enif_make_resource(env, vm);
    enif_release_resource(vm);
    
    return util_mk_ok(env, ret);
}

static ENTERM
eval(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    vm_ptr vm;
    ENPID pid;
    ENBINARY bin;

    if(argc != 4) return enif_make_badarg(env);
    
    if(!enif_get_resource(env, argv[0], state->res_type, (void**) &vm))
        return enif_make_badarg(env);
    if(!enif_is_ref(env, argv[1]))
        return util_mk_error(env, "invalid_ref");
    if(!enif_get_local_pid(env, argv[2], &pid))
        return util_mk_error(env, "invalid_pid");
    if(!enif_inspect_binary(env, argv[3], &bin))
        return util_mk_error(env, "invalid_script");
    if(!vm_add_eval(vm, argv[1], pid, bin))
        return util_mk_error(env, "error_creating_job");
    
    return util_mk_atom(env, "ok");
}

static ENTERM
call(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    vm_ptr vm;
    ENPID pid;

    if(argc != 5) return enif_make_badarg(env);
    
    if(!enif_get_resource(env, argv[0], state->res_type, (void**) &vm))
    {
        return enif_make_badarg(env);
    }

    if(!enif_is_ref(env, argv[1]))
    {
        return util_mk_error(env, "invalid_ref");
    }

    if(!enif_get_local_pid(env, argv[2], &pid))
    {
        return util_mk_error(env, "invalid_pid");
    }
    
    if(!enif_is_binary(env, argv[3]))
    {
        return util_mk_error(env, "invalid_name");
    }
    
    if(!enif_is_list(env, argv[4]))
    {
        return util_mk_error(env, "invalid_args");
    }
    
    if(!vm_add_call(vm, argv[1], pid, argv[3], argv[4]))
    {
        return util_mk_error(env, "error_creating_job");
    }
    
    return util_mk_atom(env, "ok");
}

static ENTERM
send(ErlNifEnv* env, int argc, CENTERM argv[])
{
    state_ptr state = (state_ptr) enif_priv_data(env);
    vm_ptr vm;

    if(argc != 2) return enif_make_badarg(env);
    
    if(!enif_get_resource(env, argv[0], state->res_type, (void**) &vm))
    {
        return enif_make_badarg(env);
    }

    if(!vm_send(vm, argv[1]))
    {
        return util_mk_error(env, "error_sending_response");
    }
    
    return util_mk_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] = {
    {"create_ctx", 1, create_ctx},
    {"eval", 4, eval},
    {"call", 5, call},
    {"send", 2, send}
};

ERL_NIF_INIT(emonk, nif_funcs, &load, NULL, NULL, unload);

