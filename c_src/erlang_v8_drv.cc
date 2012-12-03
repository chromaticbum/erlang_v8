#include "erlang_v8_drv.h"
#include <string.h>

ErlNifResourceType *JsWrapperResource;
ErlNifResourceType *VmResource;
ErlNifResourceType *VmContextResource;

static ERL_NIF_TERM NewVm(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlNifPid server;

  if(enif_get_local_pid(env, argv[0], &server)) {
    Vm *vm = new Vm(server);

    return vm->MakeTerm(env);
  } else {
    return enif_make_badarg(env);
  }
}

static void VmDestroy(ErlNifEnv *env, void *obj) {
  TRACE("VmDestroy\n");
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
    VmContext *vmContext = vm->CreateVmContext();

    return vmContext->MakeTerm(env);
  } else {
    return enif_make_badarg(env);
  }
}

static void VmContextDestroy(ErlNifEnv *env, void *obj) {
  TRACE("VmContextDestroy\n");
  Vm *vm = (Vm *)obj;

  delete vm;
}

static void JsWrapperDestroy(ErlNifEnv *env, void *obj) {
  TRACE("JsWrapperDestroy\n");
  JsWrapper *jsWrapper = (JsWrapper *)obj;

  delete jsWrapper;
}

static ERL_NIF_TERM Execute(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlVmContext *erlVmContext;

  if(enif_get_resource(env, argv[0], VmContextResource, (void **)(&erlVmContext))) {
    VmContext *vmContext = erlVmContext->vmContext;
    ErlNifBinary binary;

    if(enif_inspect_iolist_as_binary(env, argv[0], &binary)) {
      char *script = (char *)malloc((binary.size + 1) * sizeof(char));
      memcpy(script, binary.data, binary.size);
      script[binary.size] = NULL;

      Persistent<Value> jsValue = vmContext->Execute(script);
      JsWrapper *jsWrapper = new JsWrapper(vmContext, jsValue);

      return jsWrapper->MakeTerm(env);
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

static int Load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info) {
  VmResource = enif_open_resource_type(env, NULL, "erlang_v8_VmResource", VmDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  VmContextResource = enif_open_resource_type(env, NULL, "erlang_v8_VmContextResource", VmContextDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  JsWrapperResource = enif_open_resource_type(env, NULL, "erlang_v8_JsWrapperResource", JsWrapperDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);

  return 0;
};

static ErlNifFunc nif_funcs[] = {
  {"new_vm", 1, NewVm},
  {"new_context", 1, NewContext},
  {"execute", 2, Execute}
};

ERL_NIF_INIT(v8nif, nif_funcs, Load, NULL, NULL, NULL)
