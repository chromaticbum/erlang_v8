#include <v8.h>
#include "erlang/erl_nif.h"
#include <string.h>
#include <string>
#include <stack>

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
  EVAL,
  CALL,
  CALL_RESPOND,
  SET,
  GET,
  HEAP_STATISTICS
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
} JsExec;

class Vm {
  public:
    char id[128];
    ErlNifEnv *env;
    ErlVm *erlVm;
    ERL_NIF_TERM term;
    Isolate *isolate;
    Persistent<Context> context;
    ErlNifTid tid;
    ErlNifCond *cond, *cond2;
    ErlNifMutex *mutex, *mutex2;
    JsExec *jsExec;
    ErlNifPid server;
    stack<VmContext *> contextStack;

    Vm(ErlNifEnv *_env);
    ~Vm();

    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    void SetServer(ErlNifPid pid);
    VmContext *CurrentContext();

    VmContext *CreateVmContext(ErlNifEnv *env);
    void Run();
    void RunLoop();
    Handle<Value> Poll();
    JsExec *ResetJsExec();

    void ExecuteEval(JsExec *jsExec);
    void ExecuteSet(JsExec *jsExec);
    void ExecuteGet(JsExec *jsExec);
    ERL_NIF_TERM ExecuteCall(JsCallType type,
        ErlNifEnv *env,
        ERL_NIF_TERM recvTerm,
        ERL_NIF_TERM funTerm,
        ERL_NIF_TERM argsTerm);
    void ExecuteCall(JsExec *jsExec);
    void ExecuteHeapStatistics(JsExec *jsExec);
    Handle<Value> ExecuteCallRespond(JsExec *jsExec);
    void Exit(JsExec *jsExec);

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

    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
};

class JsWrapper {
  public:
    Vm *vm;
    Persistent<Value> value;
    ErlJsWrapper *erlJsWrapper;
    ERL_NIF_TERM resourceTerm;

    JsWrapper(Vm *_vm,
        ErlNifEnv *env, Persistent<Value> _value);
    ~JsWrapper();

    static ERL_NIF_TERM MakeBinary(ErlNifEnv *env,
        Handle<Value> value);
    static ERL_NIF_TERM MakeList(Vm *vm,
        ErlNifEnv *env,
        Local<Array> arr);
    static ERL_NIF_TERM MakeTerm(Vm *vm,
        ErlNifEnv *env,
        Local<Value> value);
    static ERL_NIF_TERM MakeTerm(ErlNifEnv *env,
        TryCatch trycatch);
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
