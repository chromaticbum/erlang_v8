#include "erlang_v8_drv.h"

Vm::Vm(ErlNifEnv *_env) {
  env = _env;

  isolate = Isolate::New();

  erlVm = (ErlVm *) enif_alloc_resource(VmResource, sizeof(ErlVm));
  erlVm->vm = this;
  term = enif_make_resource(env, erlVm);
  enif_release_resource(erlVm);
}

Vm::~Vm() {
}

VmContext *Vm::CreateVmContext(ErlNifEnv *env, ErlNifPid server) {
  return new VmContext(this, env, server);
}
