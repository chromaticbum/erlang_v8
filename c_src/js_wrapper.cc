#include "erlang_v8_drv.h"

using namespace std;

JsWrapper::JsWrapper(VmContext *_vmContext, ErlNifEnv *env, Persistent<Value> _value) {
  vmContext = _vmContext;
  value = _value;

  erlJsWrapper = (ErlJsWrapper *)enif_alloc_resource(JsWrapperResource, sizeof(ErlJsWrapper));
  erlJsWrapper->jsWrapper = this;
  resourceTerm = enif_make_resource(env, erlJsWrapper);
  enif_release_resource(erlJsWrapper);
  enif_keep_resource(vmContext->erlVmContext);
}

JsWrapper::~JsWrapper() {
  LHCST(vmContext);
  value.Dispose();
  enif_release_resource(vmContext->erlVmContext);
}

ERL_NIF_TERM JsWrapper::MakeBinary(ErlNifEnv *env,
    Handle<Value> value) {
  String::AsciiValue ascii(value);
  unsigned length = ascii.length();
  ERL_NIF_TERM binary;
  char *buffer = (char *)enif_make_new_binary(env, length, &binary);
  memcpy(buffer, *ascii, length);

  return binary;
}

ERL_NIF_TERM JsWrapper::MakeTerm(ErlNifEnv *env,
    TryCatch trycatch) {
  Handle<Value> exception = trycatch.Exception();
  Handle<Value> stackTrace = trycatch.StackTrace();

  ERL_NIF_TERM exceptionTerm = MakeBinary(env, exception);
  ERL_NIF_TERM stackTraceTerm = MakeBinary(env, stackTrace);

  return enif_make_tuple3(env,
      enif_make_atom(env, "js_error"),
      exceptionTerm, stackTraceTerm);
}

ERL_NIF_TERM JsWrapper::MakeTerm(VmContext *vmContext,
    ErlNifEnv *env,
    Local<Value> value) {
  if(value->IsObject()) {
    if(value->IsExternal()) {
      Handle<External> external = Handle<External>::Cast(value);
      ErlWrapper *erlWrapper = (ErlWrapper *)external->Value();

      return enif_make_copy(env, erlWrapper->term);
    } else {
      JsWrapper *jsWrapper = new JsWrapper(vmContext,
          env,
          Persistent<Value>::New(value));
      return jsWrapper->resourceTerm;
    }
  } else if(value->IsBoolean()) {
    if(value->IsTrue()) {
      return enif_make_atom(env, "true");
    } else {
      return enif_make_atom(env, "false");
    }
  } else if(value->IsInt32()) {
    TRACE("MAKE INT\n");
    return enif_make_int(env, value->Int32Value());
  } else if(value->IsUint32()) {
    return enif_make_uint(env, value->Uint32Value());
  } else if(value->IsNumber()) {
    // Must be a double
    return enif_make_double(env, value->NumberValue());
  } else if(value->IsString()) {
    String::AsciiValue ascii(value);
    ERL_NIF_TERM binary;
    char *buffer = (char *)enif_make_new_binary(env, strlen(*ascii), &binary);
    memcpy(buffer, *ascii, strlen(*ascii));

    return binary;
  } else if(value->IsNull()) {
    return enif_make_atom(env, "null");
  } else {
    // Must be undefined

    return enif_make_atom(env, "undefined");
  }
}
