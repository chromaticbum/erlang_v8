#include "erlang_v8_drv.h"

using namespace std;

static void JsWrapperDelete(void *ptr) {
  JsWrapper *jsWrapper = (JsWrapper *)ptr;

  delete jsWrapper;
}

JsWrapper::JsWrapper(Vm *_vm,
      ErlNifEnv *env, Persistent<Value> _value) {
  vm = _vm;
  value = _value;

  erlJsWrapper = (ErlJsWrapper *)enif_alloc_resource(JsWrapperResource, sizeof(ErlJsWrapper));
  erlJsWrapper->jsWrapper = this;
  resourceTerm = enif_make_resource(env, erlJsWrapper);
  enif_release_resource(erlJsWrapper);
  enif_keep_resource(vm->erlVm);
}

JsWrapper::~JsWrapper() {
  value.Dispose();

  enif_release_resource(vm->erlVm);
}

void JsWrapper::Destroy() {
  vm->Send(vm, JsWrapperDelete, this);
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

ERL_NIF_TERM JsWrapper::MakeList(Vm *vm,
    ErlNifEnv *env,
    Local<Array> arr) {
  unsigned length = arr->Length();
  ERL_NIF_TERM *terms = (ERL_NIF_TERM *)malloc(length * sizeof(ERL_NIF_TERM));
  ERL_NIF_TERM term;

  for(int i = 0; i < length; i++) {
    terms[i] = MakeTerm(vm, env, arr->Get(Integer::New(i)));
  }
  term = enif_make_list_from_array(env, terms, length);
  free(terms);

  return term;
}

ERL_NIF_TERM JsWrapper::MakeStruct(Vm *vm,
    ErlNifEnv *env,
    Local<Object> object) {
  Local<Array> properties = object->GetOwnPropertyNames();
  unsigned length = properties->Length();
  ERL_NIF_TERM *terms = (ERL_NIF_TERM *)malloc(length * sizeof(ERL_NIF_TERM));
  ERL_NIF_TERM term;

  for(int i = 0; i < length; i++) {
    Local<Value> field = properties->Get(Integer::New(i));
    Local<Value> value = object->Get(field);
    terms[i] = enif_make_tuple2(env,
        MakeTerm(vm, env, field),
        MakeTerm(vm, env, value));
  }

  term = enif_make_list_from_array(env, terms, length);
  free(terms);

  return enif_make_tuple2(env,
      enif_make_atom(env, "struct"),
      term);
}

ERL_NIF_TERM JsWrapper::MakeTerm(Vm *vm,
    ErlNifEnv *env,
    Local<Value> value) {
  if(value->IsObject()) {
    Handle<Object> obj = value->ToObject();

    if(value->IsExternal()) {
      Handle<External> external = Handle<External>::Cast(value);
      ErlWrapper *erlWrapper = (ErlWrapper *)external->Value();

      return enif_make_copy(env, erlWrapper->term);
    } else if(value->IsArray()) {
      return MakeList(vm, env, Local<Array>::Cast(value));
    } else if(obj->IsCallable()) {
      JsWrapper *jsWrapper = new JsWrapper(vm, env, Persistent<Value>::New(value));

      return jsWrapper->resourceTerm;
    } else {
      return MakeStruct(vm, env, value->ToObject());
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
