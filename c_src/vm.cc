#include "erlang_v8_drv.h"

Vm::Vm() {
  isolate = Isolate::New();

  erlVm = (ErlVm *) enif_alloc_resource(VmResource, sizeof(ErlVm));
  erlVm->vm = this;
}

Vm::~Vm() {
  enif_release_resource(erlVm);
}

VmContext *Vm::CreateVmContext() {
  return new VmContext(this);
}

ERL_NIF_TERM Vm::MakeTerm(ErlNifEnv *env) {
  return enif_make_resource(env, erlVm);
}
