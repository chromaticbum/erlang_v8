#include "erlang_v8_drv.h"

static ErlNifResourceType *VmResource;
static ErlNifResourceType *VmContextResource;
static ErlNifResourceType *JsWrapperResource;

static ERL_NIF_TERM NewVm(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlNifPid server;

  if(enif_get_local_pid(env, argv[0], &server)) {
    Vm *vm = new Vm(server);
    ErlVm *erlVm = (ErlVm *)enif_alloc_resource(VmResource, sizeof(ErlVm));
    erlVm->vm = vm;
    ERL_NIF_TERM term = enif_make_resource(env, erlVm);
    enif_release_resource(erlVm);

    return term;
  } else {
    return enif_make_badarg(env);
  }
}

static void VmDestroy(ErlNifEnv *env, void *obj) {
  Vm *vm = (Vm *)obj;

  delete vm;
}

static ERL_NIF_TERM NewContext(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  TRACE("NewContext\n");
  ErlVm *erlVm;

  if(enif_get_resource(env, argv[0], VmResource, (void **)(&erlVm))) {
    Vm *vm = erlVm->vm;
    TRACE("NewContext: Creating\n");
    VmContext *vmContext = new VmContext(vm);
    TRACE("NewContext: Done creating\n");
    ErlVmContext *erlVmContext = (ErlVmContext *)enif_alloc_resource(VmContextResource, sizeof(ErlVmContext));
    erlVmContext->vmContext = vmContext;
    ERL_NIF_TERM term = enif_make_resource(env, erlVmContext);
    enif_release_resource(erlVmContext);

    return term;
  } else {
    return enif_make_badarg(env);
  }
}

static void VmContextDestroy(ErlNifEnv *env, void *obj) {
  Vm *vm = (Vm *)obj;

  delete vm;
}

static void JsWrapperDestroy(ErlNifEnv *env, void *obj) {
  JsWrapper *jsWrapper = (JsWrapper *)obj;

  delete jsWrapper;
}

static int Load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info) {
  VmResource = enif_open_resource_type(env, NULL, "erlang_v8_VmResource", VmDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  VmContextResource = enif_open_resource_type(env, NULL, "erlang_v8_VmContextResource", VmContextDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  JsWrapperResource = enif_open_resource_type(env, NULL, "erlang_v8_JsWrapperResource", JsWrapperDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);

  return 0;
};

static ErlNifFunc nif_funcs[] = {
  {"new_vm", 1, NewVm},
  {"new_context", 1, NewContext}
};

ERL_NIF_INIT(v8nif, nif_funcs, Load, NULL, NULL, NULL)
