#include <assert.h>
#include <string.h>

#include <openssl/md5.h>

#include "erl_nif.h"

ErlNifResourceType* md5_type;

typedef struct {
    MD5_CTX     md5;
    int         finalized;
} md5ctx;

void
md5_dtor(ErlNifEnv* env, void* obj)
{
    md5ctx* ctx = (md5ctx*) obj;
    unsigned char hash[16];
    if(!ctx->finalized)
        MD5_Final(hash, &(ctx->md5));
}

ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;
    return enif_make_atom(env, name);
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    md5_type = enif_open_resource_type(
            env, NULL, "md5", md5_dtor, flags, NULL
        );
    if(md5_type == NULL) return 1;
    return 0;
}

static ERL_NIF_TERM
init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    md5ctx* ctx = enif_alloc_resource(md5_type, sizeof(md5ctx));
    ERL_NIF_TERM ret;
    
    if(!MD5_Init(&(ctx->md5))) {
        ctx->finalized = 1;
        enif_release_resource(ctx);
        return make_atom(env, "init_error");
    }

    ctx->finalized = 0;
    
    ret = enif_make_resource(env, ctx);
    
    // Release our reference, term now has ownership.
    enif_release_resource(ctx);
   
    return ret;
}

static ERL_NIF_TERM
update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    md5ctx* ctx;
    ErlNifBinary bin;

    if(argc != 2)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], md5_type, (void**) &ctx))
        return enif_make_badarg(env);
    if(!enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    if(ctx->finalized)
        return make_atom(env, "already_finalized");

    if(!MD5_Update(&(ctx->md5), bin.data, bin.size))
        return make_atom(env, "update_error");

    return make_atom(env, "ok");
}

static ERL_NIF_TERM
hex(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    md5ctx* ctx;
    ERL_NIF_TERM ret;
    unsigned char* hash = enif_make_new_binary(env, 16, &ret);

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], md5_type, (void**) &ctx))
        return enif_make_badarg(env);

    if(!MD5_Final(hash, &(ctx->md5)))
        return make_atom(env, "finalization_error");
    ctx->finalized = 1;

    return ret;
}

static ErlNifFunc funcs[] =
{
    {"init", 0, init},
    {"update", 2, update},
    {"hex", 1, hex}
};

ERL_NIF_INIT(resources, funcs, &load, NULL, NULL, NULL);
