#include "erlang_v8_drv.h"

JsWrapper::JsWrapper(VmContext *_vmContext, Handle<Value> _value) {
  vmContext = _vmContext;
  value = _value;

  erlJsWrapper = (ErlJsWrapper *)enif_alloc_resource(JsWrapperResource, sizeof(ErlJsWrapper));
  erlJsWrapper->jsWrapper = this;
}

JsWrapper::~JsWrapper() {
  enif_release_resource(erlJsWrapper);
}

ERL_NIF_TERM JsWrapper::MakeTerm(ErlNifEnv *env) {
  enif_keep_resource(vmContext->erlVmContext);

  return enif_make_resource(env, erlJsWrapper);
}
