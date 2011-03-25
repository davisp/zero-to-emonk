#include <assert.h>
#include <string.h>

#include "queue.h"
#include "util.h"
#include "vm.h"

struct vm_t
{
    ErlNifTid           tid;
    ErlNifThreadOpts*   opts;
    JSRuntime*          runtime;
    queue_ptr           jobs;
    job_ptr             curr_job;
    size_t              stack_size;
    int                 alive;
};

void
vm_destroy(ErlNifEnv* env, void* obj)
{
    vm_ptr vm = (vm_ptr) obj;
    job_ptr job = job_create();
    void* resp;
    
    assert(job != NULL && "Failed to create job.");
    job->type = job_close;
    queue_push(vm->jobs, job);
    queue_send(vm->jobs, job);

    enif_thread_join(vm->tid, &resp);
    
    queue_destroy(vm->jobs);
    enif_thread_opts_destroy(vm->opts);
}

vm_ptr
vm_init(ErlNifResourceType* res_type, JSRuntime* runtime, size_t stack_size)
{
    int status;
    vm_ptr vm = (vm_ptr) enif_alloc_resource(res_type, sizeof(struct vm_t));
    if(vm == NULL) return NULL;

    vm->runtime = runtime;
    vm->curr_job = NULL;
    vm->stack_size = stack_size;

    vm->jobs = queue_create();
    if(vm->jobs == NULL) goto error;
    
    vm->opts = enif_thread_opts_create("vm_thread_opts");
    status = enif_thread_create("", &vm->tid, vm_run, vm, vm->opts)
    if(status != 0) goto error;
    
    return vm;

error:
    enif_release_resource(vm);
    return NULL;
}

void*
vm_run(void* arg)
{
    vm_ptr vm = (vm_ptr) arg;
    JSContext* cx;
    job_ptr job;
    ENTERM resp;
   
    cx = init_js_context();
    if(cx == NULL) goto done;

    while(1)
    {
        job = queue_pop(vm->jobs);
        if(job->type == job_close) {job_destroy(job); break;}

        if(job->type == job_eval) {
            resp = vm_eval(cx, gl, job);
        } else if(job->type == job_call) {
            resp = vm_call(cx, gl, job);
        }

        enif_send(NULL, &(job->pid), job->env, resp);
        job_destroy(job);
    }

done:
    if(cx != NULL) JS_DestroyContext(cx);
    return NULL;
}

int
vm_add_eval(vm_ptr vm, ENTERM ref, ENPID pid, ENBINARY bin)
{
    job_ptr job = job_create();

    job->type = job_eval;
    job->ref = enif_make_copy(job->env, ref);
    job->pid = pid;
    
    if(!enif_alloc_binary(bin.size, &(job->script))) goto error;
    memcpy(job->script.data, bin.data, bin.size);

    if(!queue_push(vm->jobs, job)) goto error;

    return 1;

error:
    if(job != NULL) job_destroy(job);
    return 0;
}

ENTERM
vm_eval(JSContext* cx, JSObject* gl, job_ptr job)
{
    ENTERM resp;
    const char* script;
    size_t length;
    jsval rval;
    int cnt;
    int i;

    script = (const char*) job->script.data;
    length = job->script.size;

    for(i = 0, cnt = 0; i < length; i++) {
        if(script[i] == '\n') cnt += 1;
    }

    if(!JS_EvaluateScript(cx, gl, script, length, "", cnt, &rval)) {
        if(job->error != 0)
            resp = vm_mk_error(job->env, job->error);
        else
            resp = vm_mk_error(job->env, util_mk_atom(job->env, "unknown"));
    } else {
        resp = vm_mk_ok(job->env, to_erl(job->env, cx, rval));
    }

    return enif_make_tuple2(job->env, job->ref, resp);
}

