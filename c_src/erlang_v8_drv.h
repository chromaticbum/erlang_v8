#include <v8.h>
#include "erlang/erl_nif.h"

using namespace v8;

#define LHCS(obj) \
  Locker locker(obj->vm->isolate); \
  Isolate::Scope iscop(obj->vm->isolate); \
  HandleScope handle_scope; \
  Context::Scope context_scope(obj->context);

#define TRACE printf

extern ErlNifResourceType *JsWrapperResource;
extern ErlNifResourceType *VmResource;
extern ErlNifResourceType *VmContextResource;

class Vm;
class VmContext;
class JsWrapper;

typedef struct {
  Vm *vm;
} ErlVm;

typedef struct {
  VmContext *vmContext;
} ErlVmContext;

typedef struct {
  JsWrapper *jsWrapper;
} ErlJsWrapper;

class Vm {
  public:
    ErlVm *erlVm;
    ErlNifPid server;
    Isolate *isolate;
    Persistent<Context> context;

    Vm(ErlNifPid _server);
    ~Vm();

    VmContext *CreateVmContext();
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
};

class VmContext {
  public:
    Vm *vm;
    Persistent<Context> context;
    ErlVmContext *erlVmContext;

    VmContext(Vm *_vm);
    ~VmContext();

    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    Persistent<Value> Execute(char *script);
};

class JsWrapper {
  public:
    VmContext *vmContext;
    Handle<Value> value;
    ErlJsWrapper *erlJsWrapper;

    JsWrapper(VmContext *_vmContext, Handle<Value> _value);
    ~JsWrapper();

    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
};
