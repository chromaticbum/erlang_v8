#include <v8.h>
#include "erlang/erl_nif.h"

using namespace v8;

#define LHCS(obj) \
  Locker locker(obj->vm->isolate) \
  Isolate::Scope iscop(obj->vm->isolate) \
  HandleScope handle_scope \
  Context::Scope context_scope(obj->context)

#define TRACE printf

class Vm {
  public:
    ErlNifPid server;
    Isolate *isolate;
    Persistent<Context> context;

    Vm(ErlNifPid _server);
    ~Vm();
};

class VmContext {
  public:
    Vm *vm;
    Persistent<Context> context;

    VmContext(Vm *_vm);
    ~VmContext();
};

class JsWrapper {
  public:
    VmContext *vmContext;
    Handle<Value> value;

    JsWrapper(VmContext *_vmContext, Handle<Value> _value);
    ~JsWrapper();
};

typedef struct {
  Vm *vm;
} ErlVm;

typedef struct {
  VmContext *vmContext;
} ErlVmContext;

typedef struct {
  JsWrapper *jsWrapper;
} ErlJsWrapper;
