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
  SCRIPT
} JsCallType;

typedef struct {
  ErlNifPid pid;
  JsCallType type;
  void *data;
} JsCall;

class Vm {
  public:
    ErlVm *erlVm;
    Isolate *isolate;
    Persistent<Context> context;

    Vm();
    ~Vm();

    VmContext *CreateVmContext(ErlNifEnv *env);
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
};

class VmContext {
  public:
    Vm *vm;
    Persistent<Context> context;
    ErlNifEnv *env;
    ErlVmContext *erlVmContext;
    ErlNifTid tid;
    ErlNifCond *cond;
    ErlNifMutex *mutex;
    JsCall *jsCall;

    VmContext(Vm *_vm, ErlNifEnv *_env);
    ~VmContext();

    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    int Run();
    void RunLoop();
    Persistent<Value> Poll();
    bool Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term);
    void PostResult(JsCall *call, Persistent<Value> result);
    void ResetJsCall();

    void ExecuteScript(JsCall *jsCall);
};

class JsWrapper {
  public:
    VmContext *vmContext;
    Persistent<Value> value;
    ErlJsWrapper *erlJsWrapper;
    ERL_NIF_TERM resourceTerm;

    JsWrapper(VmContext *_vmContext, Persistent<Value> _value);
    ~JsWrapper();

    bool Define(char *field, ERL_NIF_TERM term);

    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env, string type);
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env, string type, ERL_NIF_TERM term);
};

class ErlWrapper {
  public:
    VmContext *vmContext;
    ERL_NIF_TERM term;

    ErlWrapper(VmContext *_vmContext, ERL_NIF_TERM term);
    ~ErlWrapper();
};
