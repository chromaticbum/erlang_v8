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
  printf("VmDestroy\n");
  ErlVm *erlVm = (ErlVm *)obj;

  delete erlVm->vm;
}

static ERL_NIF_TERM SetVmServer(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlVm *erlVm;

  if(enif_get_resource(env, argv[0], VmResource, (void **)(&erlVm))) {
    Vm *vm = erlVm->vm;
    ErlNifPid server;

    if(enif_get_local_pid(env, argv[1], &server)) {
      vm->SetServer(server);

      return enif_make_atom(env, "ok");
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

static void VmContextDestroy(ErlNifEnv *env, void *obj) {
  printf("VmContextDestroy\n");
  ErlVmContext *erlVmContext = (ErlVmContext *)obj;

  delete erlVmContext->vmContext;
}

static void JsWrapperDestroy(ErlNifEnv *env, void *obj) {
  printf("JsWrapperDestroy\n");
  ErlJsWrapper *erlJsWrapper = (ErlJsWrapper *)obj;

  erlJsWrapper->jsWrapper->Destroy();
}

static ERL_NIF_TERM VmExecute(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlVm *erlVm;

  if(enif_get_resource(env, argv[0], VmResource, (void **)(&erlVm))) {
    Vm *vm = erlVm->vm;
    ErlNifPid pid;

    if(enif_get_local_pid(env, argv[1], &pid)) {
      return vm->Send(NULL, env, pid, argv[2]);
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

static ERL_NIF_TERM Execute(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  TRACE("Execute\n");
  ErlVmContext *erlVmContext;

  if(enif_get_resource(env, argv[0], VmContextResource, (void **)(&erlVmContext))) {
    VmContext *vmContext = erlVmContext->vmContext;
    enif_mutex_lock(vmContext->vm->mutex);
    enif_mutex_lock(vmContext->vm->mutex2);
    ErlNifPid pid;

    if(enif_get_local_pid(env, argv[1], &pid)) {
      TRACE("Execute - 1\n");
      return vmContext->vm->Send(vmContext, env, pid, argv[2]);
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
  {"set_vm_server", 2, SetVmServer},
  {"execute", 3, Execute},
  {"vm_execute", 3, VmExecute}
};

ERL_NIF_INIT(v8nif, nif_funcs, Load, NULL, NULL, NULL)
