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
  LHCS(vmContext);
  value.Dispose();
  enif_release_resource(vmContext->erlVmContext);
}

bool JsWrapper::Set(char *field, ERL_NIF_TERM term) {
  if(value->IsObject()) {
    Handle<Object> object = value->ToObject();
    Handle<String> fieldStr = String::New(field);
    ErlWrapper *erlWrapper = new ErlWrapper(vmContext, term);
    object->Set(fieldStr, erlWrapper->MakeHandle());

    return true;
  } else {
    return false;
  }
}

Local<Value> JsWrapper::Get(char *field) {
  return value->ToObject()->Get(String::New(field));
}

ERL_NIF_TERM JsWrapper::MakeTerm(VmContext *vmContext,
    ErlNifEnv *env,
    Local<Value> value) {
  if(value->IsObject()) {
    JsWrapper *jsWrapper = new JsWrapper(vmContext,
        env,
        Persistent<Value>::New(value));
    return jsWrapper->resourceTerm;
  } else if(value->IsBoolean()) {
    if(value->IsTrue()) {
      return enif_make_atom(env, "true");
    } else {
      return enif_make_atom(env, "false");
    }
  } else if(value->IsNumber()) {
    if(value->IsInt32()) {
      return enif_make_int(env, value->Int32Value());
    } else if(value->IsUint32()) {
      return enif_make_uint(env, value->Uint32Value());
    } else {
      // Must be a double
      return enif_make_double(env, value->NumberValue());
    }
  } else if(value->IsString()) {
    String::AsciiValue ascii(value);
    ERL_NIF_TERM binary;
    char *buffer = (char *)enif_make_new_binary(env, strlen(*ascii), &binary);
    memcpy(buffer, *ascii, strlen(*ascii));

    return binary;
  } else if(value->IsExternal()) {
    return enif_make_atom(env, "external");
  } else if(value->IsNull()) {
    return enif_make_atom(env, "null");
  } else {
    // Must be undefined

    return enif_make_atom(env, "undefined");
  }
}
