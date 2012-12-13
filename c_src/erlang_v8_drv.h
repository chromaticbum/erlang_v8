#include <v8.h>
#include "erlang/erl_nif.h"
#include <string.h>
#include <string>

using namespace v8;
using namespace std;

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

typedef enum {
  EXIT,
  SCRIPT
} JsCallType;

typedef struct {
  ErlNifPid pid;
  JsCallType type;
  void *data;
} JsCall;

class Vm {
  public:
    ErlNifEnv *env;
    ErlVm *erlVm;
    ERL_NIF_TERM term;
    Isolate *isolate;
    Persistent<Context> context;

    Vm(ErlNifEnv *_env);
    ~Vm();

    VmContext *CreateVmContext(ErlNifEnv *env, ErlNifPid server);
};

class VmContext {
  public:
    Vm *vm;
    Persistent<Context> context;
    ErlNifPid server;
    ErlVmContext *erlVmContext;
    ErlNifTid tid;
    ErlNifCond *cond, *cond2;
    ErlNifMutex *mutex, *mutex2;
    ERL_NIF_TERM term;
    JsCall *jsCall;

    VmContext(Vm *_vm, ErlNifEnv *env, ErlNifPid _server);
    ~VmContext();

    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    bool Run();
    void Stop();
    void RunLoop();
    Persistent<Value> Poll();
    ERL_NIF_TERM Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term);
    ERL_NIF_TERM SendScript(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term);
    void PostResult(ErlNifPid pid, Persistent<Value> result);
    JsCall *ResetJsCall();
    void FreeJsCall(JsCall *jsCall);

    void ExecuteScript(JsCall *jsCall);
    void Exit(JsCall *jsCall);
};

class JsWrapper {
  public:
    VmContext *vmContext;
    Persistent<Value> value;
    ErlJsWrapper *erlJsWrapper;
    ERL_NIF_TERM resourceTerm;

    JsWrapper(VmContext *_vmContext, ErlNifEnv *env, Persistent<Value> _value);
    ~JsWrapper();

    bool Set(char *field, ERL_NIF_TERM term);
    static ERL_NIF_TERM MakeTerm(VmContext *vmContext,
        ErlNifEnv *env,
        Handle<Value> value);
};

class ErlWrapper {
  public:
    ErlNifEnv *env;
    VmContext *vmContext;
    ERL_NIF_TERM term;
    Persistent<Value> persistent;

    ErlWrapper(VmContext *_vmContext, ERL_NIF_TERM _term);
    ~ErlWrapper();

    Handle<External> MakeExternal();
    Handle<Value> MakeHandle();
};
