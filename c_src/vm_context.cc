#include "erlang_v8_drv.h"

VmContext::VmContext(Vm *_vm) {
  vm = _vm;

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
}
