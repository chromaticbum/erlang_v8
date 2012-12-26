#include <v8.h>
#include "erlang/erl_nif.h"
#include <string.h>
#include <string>
#include <stack>
#include <list>

using namespace v8;
using namespace std;

#define LHCST(vm, vmContext) \
  Locker locker(vm->isolate); \
  Isolate::Scope iscop(vm->isolate); \
  HandleScope handle_scope; \
  Context::Scope context_scope(vmContext->context); \
  TryCatch trycatch;

#define TRACE printf

extern ErlNifResourceType *JsWrapperResource;
extern ErlNifResourceType *VmResource;
extern ErlNifResourceType *VmContextResource;

class Vm;
class VmContext;
class JsWrapper;

typedef void (*VmCallback)(void *ptr);

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
  WRAPPER,
  VM,
  VM_CONTEXT
} ErlExternalType;

typedef struct {
  ErlExternalType type;
  void *ptr;
} ErlExternal;

typedef enum {
  EXIT,
  EVAL,
  CALL,
  CALL_RESPOND,
  SET,
  GET,
  HEAP_STATISTICS,
  CALLBACK,
  NEW_CONTEXT
} JsExecType;

typedef enum {
  NORMAL,
  CONSTRUCTOR
} JsCallType;

typedef struct {
  VmContext *vmContext;
  ErlNifPid pid;
  JsExecType type;
  ErlNifEnv *env;
  int arity;
  ERL_NIF_TERM *terms;
  VmCallback callback;
  void *data;
} JsExec;

class GlobalFactory {
  public:
    static Persistent<ObjectTemplate> Generate(Vm *vm,
        ErlNifEnv *env);
};

class Vm {
  public:
    char id[128];
    ErlNifEnv *env;
    ErlVm *erlVm;
    ERL_NIF_TERM term;
    Isolate *isolate;
    Persistent<ObjectTemplate> global;
    ErlNifTid tid;
    ErlNifCond *cond, *cond2;
    ErlNifMutex *mutex, *mutex2;
    JsExec *jsExec;
    ErlNifPid server;
    stack<VmContext *, list<VmContext *> > contextStack;
    stack<ScriptOrigin, list<ScriptOrigin> > scriptOriginStack;

    Vm(ErlNifEnv *_env);
    ~Vm();

    Handle<Value> MakeHandle();
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    void SetServer(ErlNifPid pid);
    VmContext *CurrentContext();
    ScriptOrigin CurrentScriptOrigin();
    Local<Value> MakeExternal();

    VmContext *CreateVmContext(ErlNifEnv *env);
    void Run();
    void RunLoop();
    Handle<Value> Poll();
    JsExec *ResetJsExec();

    void ExecuteCallback(JsExec *jsExec);
    void ExecuteEval(JsExec *jsExec);
    void ExecuteSet(JsExec *jsExec);
    void ExecuteGet(JsExec *jsExec);
    ERL_NIF_TERM ExecuteCall(JsCallType type,
        ErlNifEnv *env,
        ERL_NIF_TERM recvTerm,
        ERL_NIF_TERM funTerm,
        ERL_NIF_TERM argsTerm,
        int wrap);
    void ExecuteCall(JsExec *jsExec);
    void ExecuteHeapStatistics(JsExec *jsExec);
    void ExecuteNewContext(JsExec *jsExec);
    Handle<Value> ExecuteCallRespond(JsExec *jsExec);
    void ExecuteExit(JsExec *jsExec);

    void Stop();

    ERL_NIF_TERM Send(VmContext *vmContext,
        ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM term);
    ERL_NIF_TERM Send(VmContext *vmContext,
        ErlNifEnv *returnEnv,
        JsExecType type,
        ErlNifPid pid,
        int arity,
        const ERL_NIF_TERM *terms);
    void Send(Vm *vm, VmCallback callback, void *ptr);

    void PostResult(ErlNifPid pid, ErlNifEnv *env, ERL_NIF_TERM term);

    ERL_NIF_TERM MakeError(ErlNifEnv *env, const char *reason);
    ERL_NIF_TERM MakeError(ErlNifEnv *env, ERL_NIF_TERM reason);
};

class VmContext {
  public:
    char id[128];
    Vm *vm;
    Persistent<Context> context;
    ErlVmContext *erlVmContext;
    ERL_NIF_TERM term;

    VmContext(Vm *_vm, ErlNifEnv *env);
    ~VmContext();

    Handle<Value> MakeHandle();
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
};

class JsWrapper {
  public:
    Vm *vm;
    Persistent<Value> value;
    ErlJsWrapper *erlJsWrapper;
    ERL_NIF_TERM resourceTerm;

    JsWrapper(Vm *_vm,
        ErlNifEnv *env, Local<Value> _value);
    ~JsWrapper();

    void Destroy();

    static ERL_NIF_TERM MakeBinary(ErlNifEnv *env,
        Handle<Value> value);
    static ERL_NIF_TERM MakeStruct(Vm *vm,
        ErlNifEnv *env,
        Local<Object> obj);
    static ERL_NIF_TERM MakeList(Vm *vm,
        ErlNifEnv *env,
        Local<Array> arr);
    static ERL_NIF_TERM MakeTerm(Vm *vm,
        ErlNifEnv *env,
        Local<Value> value);
    static ERL_NIF_TERM MakeTerm(ErlNifEnv *env,
        TryCatch trycatch);
    static ERL_NIF_TERM MakeWrapper(Vm *vm,
        ErlNifEnv *env,
        Local<Value> value);
};

class ErlWrapper {
  public:
    Vm *vm;
    ErlNifEnv *env;
    ERL_NIF_TERM term;
    Persistent<Value> persistent;

    ErlWrapper(Vm *_vm, ERL_NIF_TERM _term);
    ~ErlWrapper();

    Persistent<External> MakeExternal();
    static Local<Value> MakeHandle(Vm *vm,
        ErlNifEnv *env,
        ERL_NIF_TERM term);
    static Local<Value> MakeArray(Vm *vm,
        ErlNifEnv *env,
        unsigned length,
        ERL_NIF_TERM arrTerm);
};
