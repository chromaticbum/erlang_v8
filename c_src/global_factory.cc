#include "erlang_v8_drv.h"

Persistent<ObjectTemplate> GlobalFactory::Generate(Vm *vm,
    ErlNifEnv *env) {
  TRACE("GlobalFactory::Generate\n");
  Locker locker(vm->isolate);
  TRACE("GlobalFactory::Generate - 1\n");
  Isolate::Scope iscope(vm->isolate);
  TRACE("GlobalFactory::Generate - 2\n");
  HandleScope handle_scope;
  TRACE("GlobalFactory::Generate - 3\n");
  Context::Scope scope(Context::New());

  Local<ObjectTemplate> global = ObjectTemplate::New();
  Local<Object> erlangV8 = Object::New();

  global->Set(String::New("erlang_v8"), erlangV8);
  erlangV8->Set(String::New("vm"), ErlWrapper::MakeHandle(vm, env, vm->term));

  return Persistent<ObjectTemplate>::New(global);
}
