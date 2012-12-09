#include "erlang_v8_drv.h"

using namespace std;

JsWrapper::JsWrapper(VmContext *_vmContext, Persistent<Value> _value) {
  vmContext = _vmContext;
  value = _value;

  erlJsWrapper = (ErlJsWrapper *)enif_alloc_resource(JsWrapperResource, sizeof(ErlJsWrapper));
  erlJsWrapper->jsWrapper = this;
}

JsWrapper::~JsWrapper() {
  enif_release_resource(erlJsWrapper);
}

bool JsWrapper::Define(char *field, ERL_NIF_TERM term) {
  LHCS(vmContext);

  if(value->IsObject()) {
    Handle<Object> object = value->ToObject();
    Handle<String> fieldStr = String::New(field);
    object->Set(fieldStr, fieldStr);
    return true;
  } else {
    return false;
  }
}

ERL_NIF_TERM JsWrapper::MakeResourceTerm(ErlNifEnv *env) {
  enif_keep_resource(vmContext->erlVmContext);

  return enif_make_resource(env, erlJsWrapper);
}

ERL_NIF_TERM JsWrapper::MakeTerm(ErlNifEnv *env, string type, ERL_NIF_TERM term) {
  return enif_make_tuple4(env,
      enif_make_atom(env, type.c_str()),
      vmContext->MakeTerm(env),
      MakeResourceTerm(env),
      term
      );
}

ERL_NIF_TERM JsWrapper::MakeTerm(ErlNifEnv *env, string type) {
  return enif_make_tuple3(env,
      enif_make_atom(env, type.c_str()),
      vmContext->MakeTerm(env),
      MakeResourceTerm(env)
      );
}

ERL_NIF_TERM JsWrapper::MakeTerm(ErlNifEnv *env) {
  if(value->IsObject()) {
    if(value->IsArray()) {
      return MakeTerm(env, "js_array");
    } else if(value->IsBooleanObject()) {
      return MakeTerm(env, "js_boolean_object");
    } else if(value->IsDate()) {
      return MakeTerm(env, "js_date");
    } else if(value->IsFunction()) {
      return MakeTerm(env, "js_function");
    } else if(value->IsNumberObject()) {
      return MakeTerm(env, "js_number_object");
    } else if(value->IsRegExp()) {
      return MakeTerm(env, "js_reg_exp");
    } else if(value->IsStringObject()) {
      return MakeTerm(env, "js_string_object");
    } else {
      return MakeTerm(env, "js_object");
    }
  } else if(value->IsBoolean()) {
    if(value->BooleanValue()) {
      return MakeTerm(env, "js_boolean",
          enif_make_atom(env, "true"));
    } else {
      return MakeTerm(env, "js_boolean",
          enif_make_atom(env, "false"));
    }
  } else if(value->IsNumber()) {
    if(value->IsInt32()) {
      return MakeTerm(env, "js_number",
          enif_make_int(env, value->Int32Value()));
    } else if(value->IsUint32()) {
      return MakeTerm(env, "js_number",
          enif_make_uint(env, value->Uint32Value()));
    } else {
      // Must be a double
      return MakeTerm(env, "js_number",
          enif_make_double(env, value->NumberValue()));
    }
  } else if(value->IsString()) {
    String::AsciiValue ascii(value);
    ERL_NIF_TERM binary;
    char *buffer = (char *)enif_make_new_binary(env, strlen(*ascii), &binary);
    memcpy(buffer, *ascii, strlen(*ascii));

    return MakeTerm(env, "js_string", binary);
  } else {
    // Must be an external

    return MakeTerm(env, "js_external");
  }
}
