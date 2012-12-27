#include "erlang_v8_drv.h"

static unsigned _id = 0;

static void VmContextDestroy(Persistent<Value> value, void *ptr) {
  TRACE("VmContextDestroy\n");
  Handle<External> external = Handle<External>::Cast(value);
  ErlExternal *erlExternal = (ErlExternal *)external->Value();
  value.Dispose();
  free(erlExternal);
}

VmContext::VmContext(Vm *_vm, ErlNifEnv *env) {
  TRACE("VmContext::VmContext\n");
  vm = _vm;

  sprintf(id, "VmContext:%d", _id++);

  erlVmContext = (ErlVmContext *)enif_alloc_resource(VmContextResource, sizeof(ErlVmContext));
  erlVmContext->vmContext = this;
  term = MakeTerm(env);
  enif_release_resource(erlVmContext);
  enif_keep_resource(vm->erlVm);

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

Handle<Value> VmContext::MakeHandle() {
  ErlExternal *erlExternal = (ErlExternal *)malloc(sizeof(ErlExternal));
  erlExternal->type = VM_CONTEXT;
  erlExternal->ptr = this;
  Persistent<External> external = Persistent<External>::New(
      External::New(erlExternal));
  external.MakeWeak(NULL, VmContextDestroy);

  return external;
}

ERL_NIF_TERM VmContext::MakeTerm(ErlNifEnv *env) {
  TRACE("VmContext::MakeTerm: %u-%s\n", strlen(id), id);
  return enif_make_resource_binary(env, erlVmContext, id, strlen(id));
}
