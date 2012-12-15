#include "erlang_v8_drv.h"

static void ErlWrapperDestroy(Persistent<Value> value, void *ptr) {
  Handle<External> external = Persistent<External>::Cast(value);
  ErlWrapper *erlWrapper = (ErlWrapper *)external->Value();
  value.Dispose();
  V8::AdjustAmountOfExternalAllocatedMemory((intptr_t)(-sizeof(ErlWrapper)));

  delete erlWrapper;
}

static Handle<Value> WrapFun(const Arguments &args) {
  Handle<External> external = Local<External>::Cast(args.Data());
  ErlWrapper *erlWrapper = (ErlWrapper *)external->Value();
  ErlNifEnv *env = enif_alloc_env();

  ERL_NIF_TERM term = enif_make_tuple3(env,
      enif_make_atom(env, "call"),
      enif_make_copy(env, erlWrapper->term),
      enif_make_list(env, 0)
      );
  enif_send(NULL, &(erlWrapper->vmContext->server), env, term);
  enif_clear_env(env);
  enif_free_env(env);
  // TODO error handling

  return erlWrapper->vmContext->Poll();
}

ErlWrapper::ErlWrapper(VmContext *_vmContext, ERL_NIF_TERM _term) {
  vmContext = _vmContext;
  env = enif_alloc_env();
  term = enif_make_copy(env, _term);
}

ErlWrapper::~ErlWrapper() {
  enif_clear_env(env);
  enif_free_env(env);
}

Handle<External> ErlWrapper::MakeExternal() {
  V8::AdjustAmountOfExternalAllocatedMemory(sizeof(ErlWrapper));

  return External::New(this);
}

Persistent<Value> ErlWrapper::MakeHandle() {
  LHCS(vmContext);
  int _int;
  unsigned int _uint;
  long _long;
  unsigned long _ulong;
  ErlNifSInt64 _int64;
  ErlNifUInt64 _uint64;
  double _double;
  ErlNifBinary binary;

  Handle<Value> value;
  if(enif_get_atom_length(env, term, &_uint, ERL_NIF_LATIN1)) {
    char *buffer = (char *)malloc((_uint + 1) * sizeof(char));
    enif_get_atom(env, term, buffer, _uint + 1, ERL_NIF_LATIN1);
    value = String::New(buffer);
  } else if(enif_get_double(env, term, &_double)) {
    value = Number::New(_double);
  } else if(enif_get_int(env, term, &_int)) {
    value = Integer::New(_int);
  } else if(enif_get_int64(env, term, &_int64)) {
    value = Integer::New(_int64);
  } else if(enif_get_long(env, term, &_long)) {
    value = Integer::New(_long);
  } else if(enif_get_uint(env, term, &_uint)) {
    value = Integer::NewFromUnsigned(_uint);
  } else if(enif_get_uint64(env, term, &_uint64)) {
    value = Integer::NewFromUnsigned(_uint64);
  } else if(enif_get_ulong(env, term, &_ulong)) {
    value = Integer::NewFromUnsigned(_ulong);
  } else if(enif_inspect_iolist_as_binary(env, term, &binary)) {
    char *buffer = (char *)malloc((binary.size + 1) * sizeof(char));
    memcpy(buffer, binary.data, binary.size);
    buffer[binary.size] = NULL;
    value = String::New(buffer);
  } else if(enif_is_fun(env, term)) {
    Local<FunctionTemplate> fn = FunctionTemplate::New(WrapFun, MakeExternal());
    value = fn->GetFunction();
  } else {
    value = MakeExternal();
  }

  persistent = Persistent<Value>::New(value);
  persistent.MakeWeak(NULL, ErlWrapperDestroy);

  return persistent;
}
