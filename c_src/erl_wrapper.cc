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
  Handle<External> external = Local<External>::Cast(args.Data());
  ErlWrapper *erlWrapper = (ErlWrapper *)external->Value();
  ErlNifEnv *env = enif_alloc_env();
  unsigned length = args.Length();
  ERL_NIF_TERM *terms = (ERL_NIF_TERM *)malloc(sizeof(ERL_NIF_TERM) * length);

  for(int i = 0; i < length; i++) {
    terms[i] = JsWrapper::MakeTerm(erlWrapper->vmContext, env, args[i]);
  }

  ERL_NIF_TERM term = enif_make_tuple3(env,
      enif_make_atom(env, "call"),
      enif_make_copy(env, erlWrapper->term),
      enif_make_list_from_array(env, terms, length)
      );
  enif_send(NULL, &(erlWrapper->vmContext->server), env, term);
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

    if(strncmp(buffer, (char *)"undefined", _uint) == 0) {
      value = Local<Value>::New(Undefined());
    } else if(strncmp(buffer, (char *)"null", _uint) == 0) {
      value = Local<Value>::New(Null());
    } else if(strncmp(buffer, (char *)"true", _uint) == 0) {
      value = Local<Value>::New(True());
    } else if(strncmp(buffer, (char *)"false", _uint) == 0) {
      value = Local<Value>::New(False());
    } else {
      value = Local<Value>::New(Undefined());
    }

    free(buffer);
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
    ErlWrapper *erlWrapper = new ErlWrapper(vmContext, term);
    Local<FunctionTemplate> fn = FunctionTemplate::New(WrapFun, erlWrapper->MakeExternal());
    value = fn->GetFunction();
  } else {
    ErlWrapper *erlWrapper = new ErlWrapper(vmContext, term);
    value = Local<External>::New(erlWrapper->MakeExternal());
  }

  return value;
}
