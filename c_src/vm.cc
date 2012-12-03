#include "erlang_v8_drv.h"

Vm::Vm(ErlNifPid _server) {
  server = _server;
  isolate = Isolate::New();
}

Vm::~Vm() {
}
