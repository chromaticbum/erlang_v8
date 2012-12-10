#include "erlang_v8_drv.h"
#include <string.h>

ErlNifResourceType *JsWrapperResource;
ErlNifResourceType *VmResource;
ErlNifResourceType *VmContextResource;

static ERL_NIF_TERM NewVm(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  Vm *vm = new Vm(env);

  return vm->term;
}

static void VmDestroy(ErlNifEnv *env, void *obj) {
  TRACE("VmDestroy\n");
  ErlVm *erlVm = (ErlVm *)obj;

  delete erlVm->vm;
}

static ERL_NIF_TERM NewContext(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlVm *erlVm;

  if(enif_get_resource(env, argv[0], VmResource, (void **)(&erlVm))) {
    ErlNifPid server;
    if(enif_get_local_pid(env, argv[1], &server)) {
      Vm *vm = erlVm->vm;
      VmContext *vmContext = vm->CreateVmContext(env, server);

      return vmContext->term;
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

static void VmContextDestroy(ErlNifEnv *env, void *obj) {
  TRACE("VmContextDestroy\n");
  ErlVmContext *erlVmContext = (ErlVmContext *)obj;

  delete erlVmContext->vmContext;
}

static void JsWrapperDestroy(ErlNifEnv *env, void *obj) {
  TRACE("JsWrapperDestroy\n");
  ErlJsWrapper *erlJsWrapper = (ErlJsWrapper *)obj;

  delete erlJsWrapper->jsWrapper;
}

static ERL_NIF_TERM Execute(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlVmContext *erlVmContext;

  if(enif_get_resource(env, argv[0], VmContextResource, (void **)(&erlVmContext))) {
    VmContext *vmContext = erlVmContext->vmContext;
    ErlNifPid pid;
    if(enif_get_local_pid(env, argv[1], &pid)) {
      if(vmContext->Send(env, pid, argv[2])) {
        return enif_make_atom(env, "ok");
      } else {
        return enif_make_badarg(env);
      }
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

static ERL_NIF_TERM SetField(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlJsWrapper *erlJsWrapper;

  if(enif_get_resource(env, argv[0], JsWrapperResource, (void **)(&erlJsWrapper))) {
    ErlNifBinary binary;

    if(enif_inspect_binary(env, argv[1], &binary)) {
      char *field = (char *)malloc((binary.size + 1) * sizeof(char));
      memcpy(field, binary.data, binary.size);
      field[binary.size] = NULL;

      if(erlJsWrapper->jsWrapper->Set(field, argv[2])) {
        return enif_make_atom(env, "ok");
      } else {
        return enif_make_badarg(env);
      }
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
  {"new_vm", 0, NewVm},
  {"new_context", 2, NewContext},
  {"execute", 3, Execute},
  {"set_field", 3, SetField}
};

ERL_NIF_INIT(v8nif, nif_funcs, Load, NULL, NULL, NULL)
