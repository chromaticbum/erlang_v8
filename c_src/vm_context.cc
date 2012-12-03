#include "erlang_v8_drv.h"

VmContext::VmContext(Vm *_vm) {
  TRACE("VmContext::VmContext\n");
  vm = _vm;

  enif_keep_resource(vm->erlVm);
  erlVmContext = (ErlVmContext *)enif_alloc_resource(VmContextResource, sizeof(ErlVmContext));
  erlVmContext->vmContext = this;

  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  HandleScope handle_scope;
  context = Persistent<Context>::New(Context::New());
}

VmContext::~VmContext() {
  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  context.Dispose();
  context.Clear();

  enif_release_resource(vm->erlVm);
  enif_release_resource(erlVmContext);
}

ERL_NIF_TERM VmContext::MakeTerm(ErlNifEnv *env) {
  TRACE("VmContext::MakeTerm\n");
  return enif_make_resource(env, erlVmContext);
}

Persistent<Value> VmContext::Execute(char *script) {
  LHCS(this);

  Handle<Script> scriptHandle = Script::Compile(String::New(script));

  return Persistent<Value>::New(scriptHandle->Run());
}
