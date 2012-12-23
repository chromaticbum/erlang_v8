#include "erlang_v8_drv.h"

static unsigned _id = 0;

VmContext::VmContext(Vm *_vm, ErlNifEnv *env) {
  TRACE("VmContext::VmContext\n");
  vm = _vm;

  sprintf(id, "VmContext:%d", _id++);

  erlVmContext = (ErlVmContext *)enif_alloc_resource(VmContextResource, sizeof(ErlVmContext));
  erlVmContext->vmContext = this;
  term = MakeTerm(env);
  enif_release_resource(erlVmContext);
  enif_keep_resource(vm->erlVm);

  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  HandleScope handle_scope;
  context = Persistent<Context>::New(Context::New(NULL, vm->global));
}

VmContext::~VmContext() {
  TRACE("VmContext::~VmContext\n");
  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  context.Dispose();
  context.Clear();

  enif_release_resource(vm->erlVm);
}

ERL_NIF_TERM VmContext::MakeTerm(ErlNifEnv *env) {
  TRACE("Vm::MakeTerm\n");
  return enif_make_resource_binary(env, erlVmContext, id, strlen(id));
}
