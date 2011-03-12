

A minimal example of a NIF library can look like this:

    /* niftest.c */
    #include "erl_nif.h"

    static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
    {
        return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);
    }

    static ErlNifFunc nif_funcs[] =
    {
        {"hello", 0, hello}
    };

    ERL_NIF_INIT(niftest,nif_funcs,NULL,NULL,NULL,NULL)

And the Erlang module would have to look something like this:

    -module(niftest).
    -export([init/0, hello/0]).

    init() ->
        erlang:load_nif("./niftest", 0).

    hello() ->
        "NIF library not loaded".

And compile and test something like this (on Linux):

    $ gcc -fPIC -shared -o niftest.so niftest.c -I $ERL_ROOT/usr/include/
    $ erl
    1> c(niftest).
    {ok,niftest}
    2> niftest:hello().
    "NIF library not loaded"
    3> niftest:init().
    ok
    4> niftest:hello().
    "Hello world!"

A better solution for a real module is to take advantage of the new directive
`on_load` to automatically load the NIF library when the module is loaded.

Here is a template example of how to create and return a resource object.

    ERL_NIF_TERM term;
    MyStruct* ptr = enif_alloc_resource(my_resource_type, sizeof(MyStruct));

    /* initialize struct ... */

    term = enif_make_resource(env, ptr);

    if (keep_a_reference_of_our_own) {
        /* store 'ptr' in static variable, private data or other resource object */
    }
    else {
        enif_release_resource(obj);
        /* resource now only owned by "Erlang" */
    }
    return term;

NIF Initialization
------------------

    ERL_NIF_INIT(MODULE, ErlNifFunc funcs[], load, reload, upgrade, unload)

This is the magic macro to initialize a NIF library. It should be evaluated in
global file scope.

MODULE is the name of the Erlang module as an identifier without string
quotations. It will be stringified by the macro.

funcs is a static array of function descriptors for all the implemented NIFs
in this library.

load, reload, upgrade and unload are pointers to functions. One of load, reload
or upgrade will be called to initialize the library. unload is called to
release the library. They are all described individually below.

Load
----

    int (*load)(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)

load is called when the NIF library is loaded and there is no previously loaded
library for this module.

`*priv_data` can be set to point to some private data that the library needs in
order to keep a state between NIF calls. `enif_priv_data` will return this
pointer. `*priv_data` will be initialized to NULL when load is called.

`load_info` is the second argument to `erlang:load_nif/2`.

The library will fail to load if load returns anything other than 0. `load`
can be `NULL` in case no initialization is needed.

Reload
------

    int (*reload)(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)

`reload` is called when the NIF library is loaded and there is already a
previously loaded library for this module code.

Works the same as load. The only difference is that `*priv_data` already
contains the value set by the previous call to `load` or `reload`.

The library will fail to load if `reload` returns anything other than 0 or if
`reload` is NULL.

Upgrade
-------

    int (*upgrade)(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)

`upgrade` is called when the NIF library is loaded and there is no previously
loaded library for this module code, BUT there is old code of this module with
a loaded NIF library.

Works the same as `load`. The only difference is that `*old_priv_data` already
contains the value set by the last call to `load` or `reload` for the old
module code. `*priv_data` will be initialized to `NULL` when `upgrade`
is called. It is allowed to write to both `*priv_data` and `*old_priv_data`.

The library will fail to load if `upgrade` returns anything other than 0 or if
`upgrade` is `NULL`.

Unload
-------

    void (*unload)(ErlNifEnv* env, void* priv_data)

`unload` is called when the module code that the NIF library belongs to is
purged as old. New code of the same module may or may not exist. Note that
`unload` is not called for a replaced library as a consequence of reload.


