#include "erlang_v8_drv.h"

static void ErlWrapperDestroy(Persistent<Value> value, void *ptr) {
  TRACE("ErlWrapperDestroy\n");
  Handle<External> external = Handle<External>::Cast(value);
  ErlWrapper *erlWrapper = (ErlWrapper *)external->Value();
  value.Dispose();
  V8::AdjustAmountOfExternalAllocatedMemory((intptr_t)(-sizeof(ErlWrapper)));

  delete erlWrapper;
}

static Handle<Value> WrapFun(const Arguments &args) {
  TRACE("WrapFun\n");
  Handle<External> external = Local<External>::Cast(args.Data());
  TRACE("WrapFun - 1\n");
  ErlWrapper *erlWrapper = (ErlWrapper *)external->Value();
  TRACE("WrapFun - 2\n");
  ErlNifEnv *env = enif_alloc_env();
  TRACE("WrapFun - 3\n");
  unsigned length = args.Length();
  TRACE("WrapFun - 4\n");
  ERL_NIF_TERM *terms = (ERL_NIF_TERM *)malloc(sizeof(ERL_NIF_TERM) * length);
  TRACE("WrapFun - 5\n");

  for(int i = 0; i < length; i++) {
  TRACE("WrapFun - 6\n");
    terms[i] = JsWrapper::MakeTerm(erlWrapper->vmContext, env, args[i]);
  }

  ERL_NIF_TERM term = enif_make_tuple3(env,
      enif_make_atom(env, "call"),
      enif_make_copy(env, erlWrapper->term),
      enif_make_list_from_array(env, terms, length)
      );
  TRACE("WrapFun - 7\n");
  enif_send(NULL, &(erlWrapper->vmContext->server), env, term);
  TRACE("WrapFun - 8\n");
  free(terms);
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

Persistent<External> ErlWrapper::MakeExternal() {
  V8::AdjustAmountOfExternalAllocatedMemory(sizeof(ErlWrapper));
  Persistent<External> external = Persistent<External>::New(External::New(this));

  external.MakeWeak(NULL, ErlWrapperDestroy);

  return external;
}

Local<Value> ErlWrapper::MakeHandle(VmContext *vmContext,
    ErlNifEnv *env,
    ERL_NIF_TERM term) {
  int _int;
  unsigned int _uint;
  long _long;
  unsigned long _ulong;
  ErlNifSInt64 _int64;
  ErlNifUInt64 _uint64;
  double _double;
  ErlNifBinary binary;
  ErlJsWrapper *erlJsWrapper;

  Local<Value> value;
  if(enif_get_atom_length(env, term, &_uint, ERL_NIF_LATIN1)) {
    char *buffer = (char *)malloc((_uint + 1) * sizeof(char));
    enif_get_atom(env, term, buffer, _uint + 1, ERL_NIF_LATIN1);

    if(strncmp(buffer, "undefined", _uint) == 0) {
      value = Local<Value>::New(Undefined());
    } else if(strncmp(buffer, "null", _uint) == 0) {
      value = Local<Value>::New(Null());
    } else if(strncmp(buffer, "true", _uint) == 0) {
      value = Local<Value>::New(True());
    } else if(strncmp(buffer, "false", _uint) == 0) {
      value = Local<Value>::New(False());
    } else if(strncmp(buffer, "global", _uint) == 0) {
      value = Context::GetCurrent()->Global();
    } else {
      value = Local<Value>::New(Undefined());
    }

    free(buffer);
  } else if(enif_get_double(env, term, &_double)) {
    value = Number::New(_double);
  } else if(enif_get_int(env, term, &_int)) {
    TRACE("ErlWrapper::MakeHandle - INT\n");
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
  } else if(enif_get_resource(env, term, JsWrapperResource, (void **)(&erlJsWrapper))) {
    TRACE("ErlWrapper::MakeHandle - RESOURCE\n");
    value = Local<Value>::New(erlJsWrapper->jsWrapper->value);
  } else if(enif_inspect_binary(env, term, &binary)) {
    char *buffer = (char *)malloc((binary.size + 1) * sizeof(char));
    memcpy(buffer, binary.data, binary.size);
    buffer[binary.size] = NULL;
    value = String::New(buffer);
    free(buffer);
  } else if(enif_is_fun(env, term)) {
    TRACE("ErlWrapper::MakeHandle - FUN\n");
    ErlWrapper *erlWrapper = new ErlWrapper(vmContext, term);
    Handle<FunctionTemplate> fn = FunctionTemplate::New(WrapFun, erlWrapper->MakeExternal());
    value = fn->GetFunction();
  } else {
    ErlWrapper *erlWrapper = new ErlWrapper(vmContext, term);
    value = Local<External>::New(erlWrapper->MakeExternal());
  }

  return value;
}
