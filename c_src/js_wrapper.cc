#include "erlang_v8_drv.h"

JsWrapper::JsWrapper(VmContext *_vmContext, Handle<Value> _value) {
  vmContext = _vmContext;
  value = _value;
}

JsWrapper::~JsWrapper() {
}
