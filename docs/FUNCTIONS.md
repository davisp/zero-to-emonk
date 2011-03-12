
NIF API Functions
=================

Misc
-----

	void* enif_priv_data(ErlNifEnv* env)
	void enif_system_info(ErlNifSysInfo *sys_info_ptr, size_t size)


Testing Terms
-------------

    int enif_is_atom(ErlNifEnv* env, ERL_NIF_TERM term)
    int enif_is_binary(ErlNifEnv* env, ERL_NIF_TERM term)
    int enif_is_empty_list(ErlNifEnv* env, ERL_NIF_TERM term)
	int enif_is_fun(ErlNifEnv* env, ERL_NIF_TERM term)
	int enif_is_identical(ERL_NIF_TERM lhs, ERL_NIF_TERM rhs)
	int enif_is_pid(ErlNifEnv* env, ERL_NIF_TERM term)
	int enif_is_port(ErlNifEnv* env, ERL_NIF_TERM term)
	int enif_is_ref(ErlNifEnv* env, ERL_NIF_TERM term)
	int enif_is_tuple(ErlNifEnv* env, ERL_NIF_TERM term)
	int enif_is_list(ErlNifEnv* env, ERL_NIF_TERM term)

    int enif_compare(ERL_NIF_TERM lhs, ERL_NIF_TERM rhs)


Getting Term Values
-------------------

    int enif_get_atom(ErlNifEnv* env, ERL_NIF_TERM term, char* buf, unsigned size, ErlNifCharEncoding encode)
    int enif_get_atom_length(ErlNifEnv* env, ERL_NIF_TERM term, unsigned* len, ErlNifCharEncoding encode)

    int enif_get_int(ErlNifEnv* env, ERL_NIF_TERM term, int* ip)
    int enif_get_uint(ErlNifEnv* env, ERL_NIF_TERM term, unsigned int* ip)

    int enif_get_int64(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifSInt64* ip)
    int enif_get_uint64(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifUInt64* ip)

    int enif_get_long(ErlNifEnv* env, ERL_NIF_TERM term, long int* ip)
    int enif_get_ulong(ErlNifEnv* env, ERL_NIF_TERM term, unsigned long* ip)

    int enif_get_double(ErlNifEnv* env, ERL_NIF_TERM term, double* dp)

    int enif_get_string(ErlNifEnv* env, ERL_NIF_TERM list, char* buf, unsigned size, ErlNifCharEncoding encode)

    int enif_get_list_cell(ErlNifEnv* env, ERL_NIF_TERM list, ERL_NIF_TERM* head, ERL_NIF_TERM* tail)
    int enif_get_list_length(ErlNifEnv* env, ERL_NIF_TERM term, unsigned* len)

    int enif_get_tuple(ErlNifEnv* env, ERL_NIF_TERM term, int* arity, const ERL_NIF_TERM** array)

    int enif_inspect_binary(ErlNifEnv* env, ERL_NIF_TERM bin_term, ErlNifBinary* bin)
    int enif_inspect_iolist_as_binary(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifBinary* bin)

    int enif_get_local_pid(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifPid* pid)

    int enif_get_resource(ErlNifEnv* env, ERL_NIF_TERM term, ErlNifResourceType* type, void** objp)


Creating Terms
--------------

    ERL_NIF_TERM enif_make_badarg(ErlNifEnv* env)

	ERL_NIF_TERM enif_make_atom(ErlNifEnv* env, const char* name)
	ERL_NIF_TERM enif_make_atom_len(ErlNifEnv* env, const char* name, size_t len)
	int enif_make_existing_atom(ErlNifEnv* env, const char* name, ERL_NIF_TERM* atom, ErlNifCharEncoding encode)
	int enif_make_existing_atom_len(ErlNifEnv* env, const char* name, size_t len, ERL_NIF_TERM* atom, ErlNifCharEncoding encoding)
	
	ERL_NIF_TERM enif_make_int(ErlNifEnv* env, int i)
    ERL_NIF_TERM enif_make_uint(ErlNifEnv* env, unsigned int i)
	
	ERL_NIF_TERM enif_make_int64(ErlNifEnv* env, ErlNifSInt64 i)
    ERL_NIF_TERM enif_make_uint64(ErlNifEnv* env, ErlNifUInt64 i)

	ERL_NIF_TERM enif_make_long(ErlNifEnv* env, long int i)
	ERL_NIF_TERM enif_make_ulong(ErlNifEnv* env, unsigned long i)
	
    ERL_NIF_TERM enif_make_double(ErlNifEnv* env, double d)

    ERL_NIF_TERM enif_make_string(ErlNifEnv* env, const char* string, ErlNifCharEncoding encoding)
	ERL_NIF_TERM enif_make_string_len(ErlNifEnv* env, const char* string, size_t len, ErlNifCharEncoding encoding)
	
    ERL_NIF_TERM enif_make_list(ErlNifEnv* env, unsigned cnt, ...)
	ERL_NIF_TERM enif_make_list1(ErlNifEnv* env, ERL_NIF_TERM e1)
	ERL_NIF_TERM enif_make_list2(ErlNifEnv* env, ERL_NIF_TERM e1, ERL_NIF_TERM e2)
	ERL_NIF_TERM enif_make_list3(ErlNifEnv* env, ERL_NIF_TERM e1, ERL_NIF_TERM e2, ERL_NIF_TERM e3)
	ERL_NIF_TERM enif_make_list4(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e4)
	ERL_NIF_TERM enif_make_list5(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e5)
	ERL_NIF_TERM enif_make_list6(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e6)
	ERL_NIF_TERM enif_make_list7(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e7)
	ERL_NIF_TERM enif_make_list8(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e8)
	ERL_NIF_TERM enif_make_list9(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e9)
	ERL_NIF_TERM enif_make_list_cell(ErlNifEnv* env, ERL_NIF_TERM head, ERL_NIF_TERM tail)
	ERL_NIF_TERM enif_make_list_from_array(ErlNifEnv* env, const ERL_NIF_TERM arr[], unsigned cnt)
	
    ERL_NIF_TERM enif_make_tuple(ErlNifEnv* env, unsigned cnt, ...)
	ERL_NIF_TERM enif_make_tuple1(ErlNifEnv* env, ERL_NIF_TERM e1)
	ERL_NIF_TERM enif_make_tuple2(ErlNifEnv* env, ERL_NIF_TERM e1, ERL_NIF_TERM e2)
	ERL_NIF_TERM enif_make_tuple3(ErlNifEnv* env, ERL_NIF_TERM e1, ERL_NIF_TERM e2, ERL_NIF_TERM e3)
	ERL_NIF_TERM enif_make_tuple4(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e4)
	ERL_NIF_TERM enif_make_tuple5(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e5)
	ERL_NIF_TERM enif_make_tuple6(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e6)
	ERL_NIF_TERM enif_make_tuple7(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e7)
	ERL_NIF_TERM enif_make_tuple8(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e8)
	ERL_NIF_TERM enif_make_tuple9(ErlNifEnv* env, ERL_NIF_TERM e1, ..., ERL_NIF_TERM e9)
	ERL_NIF_TERM enif_make_tuple_from_array(ErlNifEnv* env, const ERL_NIF_TERM arr[], unsigned cnt)

    ERL_NIF_TERM enif_make_binary(ErlNifEnv* env, ErlNifBinary* bin)
    unsigned char* enif_make_new_binary(ErlNifEnv* env, size_t size, ERL_NIF_TERM* termp)
    ERL_NIF_TERM enif_make_sub_binary(ErlNifEnv* env, ERL_NIF_TERM bin_term, size_t pos, size_t size)

    ERL_NIF_TERM enif_make_pid(ErlNifEnv* env, const ErlNifPid* pid)
	
    ERL_NIF_TERM enif_make_ref(ErlNifEnv* env)

    ERL_NIF_TERM enif_make_copy(ErlNifEnv* dst_env, ERL_NIF_TERM src_term)


Memory Management
-----------------

    void* enif_alloc(ErlNifEnv* env, size_t size)
    void enif_free(ErlNifEnv* env, void* ptr)

    int enif_alloc_binary(size_t size, ErlNifBinary* bin)
	void enif_realloc_binary(ErlNifBinary* bin, size_t size)
	void enif_release_binary(ErlNifBinary* bin)


Resource Objects
----------------

	ErlNifResourceType* enif_open_resource_type(ErlNifEnv* env, const char* module_str, const char* name,
	                                                ErlNifResourceDtor* dtor, ErlNifResourceFlags flags, ErlNifResourceFlags* tried)

    void* enif_alloc_resource(ErlNifResourceType* type, unsigned size)
	int enif_keep_resource(void* obj)
	void enif_release_resource(void* obj)

    ERL_NIF_TERM enif_make_resource(ErlNifEnv* env, void* obj)
    ERL_NIF_TERM enif_make_resource_binary(ErlNifEnv* env, void* obj, const void* data, size_t size)
	
    unsigned enif_sizeof_resource(void* obj)
	

Environments
------------

    ErlNifEnv* enif_alloc_env()
    void enif_clear_env(ErlNifEnv* env)
    void enif_free_env(ErlNifEnv* env)


Message Passing
---------------

	ErlNifPid* enif_self(ErlNifEnv* caller_env, ErlNifPid* pid)
	unsigned enif_send(ErlNifEnv* env, ErlNifPid* to_pid, ErlNifEnv* msg_env, ERL_NIF_TERM msg)


Threading
---------
	
    int enif_thread_create(char *name,ErlNifTid *tid,void * (*func)(void *),void *args,ErlNifThreadOpts *opts)
	void enif_thread_exit(void *resp)
	int enif_thread_join(ErlNifTid, void **respp)

	ErlNifThreadOpts* enif_thread_opts_create(char *name)
	void enif_thread_opts_destroy(ErlNifThreadOpts *opts)

    ErlNifTid enif_thread_self(void)

    int enif_equal_tids(ErlNifTid tid1, ErlNifTid tid2)

	ErlNifMutex* enif_mutex_create(char *name)
	void enif_mutex_destroy(ErlNifMutex *mtx)
	void enif_mutex_lock(ErlNifMutex *mtx)
	int enif_mutex_trylock(ErlNifMutex *mtx)
	void enif_mutex_unlock(ErlNifMutex *mtx)

    void enif_cond_broadcast(ErlNifCond *cnd)
    ErlNifCond* enif_cond_create(char *name)
    void enif_cond_destroy(ErlNifCond *cnd)
    void enif_cond_signal(ErlNifCond *cnd)
    void enif_cond_wait(ErlNifCond *cnd, ErlNifMutex *mtx)

	ErlNifRWLock* enif_rwlock_create(char *name)
	void enif_rwlock_destroy(ErlNifRWLock *rwlck)
	void enif_rwlock_rlock(ErlNifRWLock *rwlck)
	void enif_rwlock_runlock(ErlNifRWLock *rwlck)
	void enif_rwlock_rwlock(ErlNifRWLock *rwlck)
	void enif_rwlock_rwunlock(ErlNifRWLock *rwlck)
	int enif_rwlock_tryrlock(ErlNifRWLock *rwlck)
	int enif_rwlock_tryrwlock(ErlNifRWLock *rwlck)

	int enif_tsd_key_create(char *name, ErlNifTSDKey *key)
	void enif_tsd_key_destroy(ErlNifTSDKey key)
	void* enif_tsd_get(ErlNifTSDKey key)
	void enif_tsd_set(ErlNifTSDKey key, void *data)














