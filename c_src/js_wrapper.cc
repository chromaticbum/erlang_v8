#include "erlang_v8_drv.h"

using namespace std;

JsWrapper::JsWrapper(VmContext *_vmContext, Persistent<Value> _value) {
  vmContext = _vmContext;
  value = _value;

  erlJsWrapper = (ErlJsWrapper *)enif_alloc_resource(JsWrapperResource, sizeof(ErlJsWrapper));
  erlJsWrapper->jsWrapper = this;
  resourceTerm = enif_make_resource(vmContext->env, erlJsWrapper);
  enif_release_resource(erlJsWrapper);
  enif_keep_resource(vmContext->erlVmContext);
}

JsWrapper::~JsWrapper() {
  enif_release_resource(vmContext->erlVmContext);
}

bool JsWrapper::Set(char *field, ERL_NIF_TERM term) {
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

ERL_NIF_TERM JsWrapper::MakeTerm(string type, ERL_NIF_TERM term) {
  return enif_make_tuple4(vmContext->env,
      enif_make_atom(vmContext->env, type.c_str()),
      vmContext->MakeTerm(),
      resourceTerm,
      term
      );
}

ERL_NIF_TERM JsWrapper::MakeTerm(string type) {
  return enif_make_tuple3(vmContext->env,
      enif_make_atom(vmContext->env, type.c_str()),
      vmContext->MakeTerm(),
      resourceTerm
      );
}

ERL_NIF_TERM JsWrapper::MakeTerm() {
  if(value->IsObject()) {
    if(value->IsArray()) {
      return MakeTerm("js_array");
    } else if(value->IsBooleanObject()) {
      return MakeTerm("js_boolean_object");
    } else if(value->IsDate()) {
      return MakeTerm("js_date");
    } else if(value->IsFunction()) {
      return MakeTerm("js_function");
    } else if(value->IsNumberObject()) {
      return MakeTerm("js_number_object");
    } else if(value->IsRegExp()) {
      return MakeTerm("js_reg_exp");
    } else if(value->IsStringObject()) {
      return MakeTerm("js_string_object");
    } else {
      return MakeTerm("js_object");
    }
  } else if(value->IsBoolean()) {
    if(value->BooleanValue()) {
      return MakeTerm("js_boolean",
          enif_make_atom(vmContext->env, "true"));
    } else {
      return MakeTerm("js_boolean",
          enif_make_atom(vmContext->env, "false"));
    }
  } else if(value->IsNumber()) {
    if(value->IsInt32()) {
      return MakeTerm("js_number",
          enif_make_int(vmContext->env, value->Int32Value()));
    } else if(value->IsUint32()) {
      return MakeTerm("js_number",
          enif_make_uint(vmContext->env, value->Uint32Value()));
    } else {
      // Must be a double
      return MakeTerm("js_number",
          enif_make_double(vmContext->env, value->NumberValue()));
    }
  } else if(value->IsString()) {
    String::AsciiValue ascii(value);
    ERL_NIF_TERM binary;
    char *buffer = (char *)enif_make_new_binary(vmContext->env, strlen(*ascii), &binary);
    memcpy(buffer, *ascii, strlen(*ascii));

    return MakeTerm("js_string", binary);
  } else if(value->IsExternal()) {
    return MakeTerm("js_external");
  } else if(value->IsNull()) {
    return MakeTerm("js_null");
  } else {
    // Must be undefined

    return MakeTerm("js_undefined");
  }
}
