DATA TYPES
==========


ERL_NIF_TERM
------------

Variables of type `ERL_NIF_TERM` can refer to any Erlang term. This is an
opaque type and values of it can only by used either as arguments to API
functions or as return values from NIFs. All `ERL_NIF_TERM`'s belong to an
environment (ErlNifEn. A term can not be destructed individually, it is valid until its environment is destructed.

ErlNifEnv
---------

ErlNifEnv represents an environment that can host Erlang terms. All terms in
an environment are valid as long as the environment is valid. ErlNifEnv is an
opaque type and pointers to it can only be passed on to API functions. There
are two types of environments; process bound and process independent.

A process bound environment is passed as the first argument to all NIFs. All
function arguments passed to a NIF will belong to that environment. The return
value from a NIF must also be a term belonging to the same environment. In
addition a process bound environment contains transient information about the
calling Erlang process. The environment is only valid in the thread where it
was supplied as argument until the NIF returns. It is thus useless and
dangerous to store pointers to process bound environments between NIF calls.

A process independent environment is created by calling `enif_alloc_env`. It
can be used to store terms beteen NIF calls and to send terms with `enif_send`.
A process independent environment with all its terms is valid until you
explicitly invalidates it with `enif_free_env` or `enif_send`.

All elements of a list/tuple must belong to the same environment as the list/
tuple itself. Terms can be copied between environments with `enif_make_copy`.

ErlNifFunc
----------

    typedef struct {
        const char* name;
        unsigned arity;
        ERL_NIF_TERM (*fptr)(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    } ErlNifFunc;

Describes a NIF by its name, arity and implementation. fptr is a pointer to
the function that implements the NIF. The argument argv of a NIF will contain
the function arguments passed to the NIF and argc is the length of the array,
i.e. the function arity. `argv[N-1]` will thus denote the Nth argument to the
NIF. Note that the argc argument allows for the same C function to implement
several Erlang functions with different arity (but same name probably).

ErlNifBinary
------------

    typedef struct {
        unsigned size;
        unsigned char* data;
    } ErlNifBinary;

ErlNifBinary contains transient information about an inspected binary term.
`data` is a pointer to a buffer of size bytes with the raw content of the
binary.

Note that ErlNifBinary is a semi-opaque type and you are only allowed to read
fields size and data.

ErlNifPid
---------

ErlNifPid is a process identifier (`pid`). In contrast to pid terms (instances
of `ERL_NIF_TERM`), `ErlNifPid`'s are self contained and not bound to any
environment. ErlNifPid is an opaque type.

ErlNifResourceType
-------------------

Each instance of ErlNifResourceType represent a class of memory managed
resource objects that can be garbage collected. Each resource type has a unique
name and a destructor function that is called when objects of its type are
released.

ErlNifResourceDtor
------------------

    typedef void ErlNifResourceDtor(ErlNifEnv* env, void* obj);

The function prototype of a resource destructor function. A destructor function
is not allowed to call any term-making functions.

ErlNifCharEncoding
------------------

    typedef enum {
        ERL_NIF_LATIN1
    } ErlNifCharEncoding;

The character encoding used in strings and atoms. The only supported encoding
is currently `ERL_NIF_LATIN1` for iso-latin-1 (8-bit ascii).

ErlNifSysInfo
-------------

    typedef struct ErlNifSysInfo {
        int driver_major_version;
        int driver_minor_version;
        char *erts_version;
        char *otp_release;
        int thread_support;
        int smp_support;
        int async_threads;
        int scheduler_threads;
        int nif_major_version;
        int nif_minor_version;
    } ErlNifSysInfo;
      
The ErlNifSysInfo structure is used for storage of information about the Erlang
runtime system. `driver_system_info()` will write the system information when
passed a reference to a `ErlNifSysInfo` structure. A description of the fields
in the structure follow:

* `driver_major_version`
    The value of `ERL_DRV_EXTENDED_MAJOR_VERSION` when the runtime system was
    compiled. This value is the same as the value of
    `ERL_DRV_EXTENDED_MAJOR_VERSION` used when compiling the driver;
    otherwise, the runtime system would have refused to load the driver.
* `driver_minor_version`
    The value of `ERL_DRV_EXTENDED_MINOR_VERSION` when the runtime system was
    compiled. This value might differ from the value of
    `ERL_DRV_EXTENDED_MINOR_VERSION` used when compiling the driver.
* `erts_version`
    A string containing the version number of the runtime system (the same as
    returned by `erlang:system_info(version)`).
* `otp_release`
    A string containing the OTP release number (the same as returned by
    `erlang: system_info(otp_release)`).
* `thread_support`
    A value != 0 if the runtime system has thread support; otherwise, 0.
* `smp_support`
    A value != 0 if the runtime system has SMP support; otherwise, 0.
* `async_threads`
    The number of async threads in the async thread pool used by
    `driver_async()` (the same as returned by
    `erlang:system_info(thread_pool_size)`).
* `scheduler_threads`
    The number of scheduler threads used by the runtime system (the same as
    returned by `erlang:system_info(schedulers)`).
* `nif_major_version`
    The value of `ERL_NIF_MAJOR_VERSION` when the runtime system was compiled.
* `nif_minor_version`
    The value of `ERL_NIF_MINOR_VERSION` when the runtime system was compiled.


ErlNifSInt64
------------

A native signed 64-bit integer type.

ErlNifUInt64
------------

A native unsigned 64-bit integer type.

