#include "erlang_v8_drv.h"

static Handle<Value> GetContext(Local<String> property,
    const AccessorInfo &info) {
  Local<Value> value = info.Data();
  Local<External> external = Local<External>::Cast(value);
  Vm *vm = (Vm *)external->Value();
  VmContext *vmContext = vm->CurrentContext();

  ErlNifEnv *env = enif_alloc_env();
  Handle<Value> result = ErlWrapper::MakeHandle(vm,
      env, vmContext->MakeTerm(env));
  enif_clear_env(env);
  enif_free_env(env);

  return result;
}

static Handle<Value> GetScriptName(Local<String> property,
    const AccessorInfo &info) {
  return StackTrace::CurrentStackTrace(1, StackTrace::kScriptName)
    ->GetFrame(0)->GetScriptName();
}

static Handle<Value> GetCurrentScriptName(Local<String> property,
    const AccessorInfo &info) {
  Handle<External> external = Handle<External>::Cast(info.Data());
  Vm *vm = (Vm *)external->Value();

  return vm->CurrentScriptOrigin().ResourceName();
}

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

  global->Set(String::New("__ev8__"), erlangV8);
  erlangV8->Set(String::New("vm"), vm->MakeHandle());
  Handle<External> external = External::New(vm);
  erlangV8->SetAccessor(String::New("context"), GetContext,
      NULL, external);
  erlangV8->SetAccessor(String::New("script_name"), GetScriptName);
  erlangV8->SetAccessor(String::New("current_script_name"), GetCurrentScriptName,
      NULL, External::New(vm));

  return Persistent<ObjectTemplate>::New(global);
}
