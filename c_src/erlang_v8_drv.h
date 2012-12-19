#include <v8.h>
#include "erlang/erl_nif.h"
#include <string.h>
#include <string>

using namespace v8;
using namespace std;

#define LHCST(obj) \
  Locker locker(obj->vm->isolate); \
  Isolate::Scope iscop(obj->vm->isolate); \
  HandleScope handle_scope; \
  Context::Scope context_scope(obj->context); \
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
  RUN_SCRIPT,
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
  ErlNifPid pid;
  JsExecType type;
  void *data;
} JsExec;

typedef struct {
  ErlNifEnv *env;
  ERL_NIF_TERM originTerm;
  ERL_NIF_TERM scriptTerm;
} JsRunScript;

typedef struct {
  ErlNifEnv *env;
  ERL_NIF_TERM objectTerm;
  ERL_NIF_TERM fieldsTerm;
} JsSet;

typedef struct {
  ErlNifEnv *env;
  ERL_NIF_TERM objectTerm;
  ERL_NIF_TERM fieldTerm;
} JsGet;

typedef struct {
  ErlNifEnv *env;
  ERL_NIF_TERM term;
} JsCallRespond;

typedef struct {
  ErlNifEnv *env;
  JsCallType type;
  ERL_NIF_TERM recv;
  ERL_NIF_TERM fun;
  ERL_NIF_TERM args;
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

    VmContext *CreateVmContext(ErlNifEnv *env);
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
    JsExec *jsExec;

    VmContext(Vm *_vm, ErlNifEnv *env);
    ~VmContext();

    void SetServer(ErlNifPid pid);
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    bool Run();
    void Stop();
    void RunLoop();
    Handle<Value> Poll();

    ERL_NIF_TERM Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term);
    ERL_NIF_TERM SendRunScript(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM originTerm,
        ERL_NIF_TERM scriptTerm);
    ERL_NIF_TERM SendCallRespond(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM term);
    ERL_NIF_TERM SendSet(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM wrapperTerm,
        ERL_NIF_TERM fields);
    ERL_NIF_TERM SendGet(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM wrapperTerm,
        ERL_NIF_TERM fieldsTerm);
    ERL_NIF_TERM SendCall(ErlNifEnv *env,
        ErlNifPid pid,
        JsCallType type,
        ERL_NIF_TERM recv,
        ERL_NIF_TERM fun,
        ERL_NIF_TERM args);
    ERL_NIF_TERM SendCall(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM type,
        ERL_NIF_TERM call);
    ERL_NIF_TERM SendHeapStatistics(ErlNifEnv *env,
        ErlNifPid pid);

    void PostResult(ErlNifPid pid, ErlNifEnv *env, ERL_NIF_TERM term);
    JsExec *ResetJsExec();

    ERL_NIF_TERM MakeError(ErlNifEnv *env, const char *reason);
    ERL_NIF_TERM MakeError(ErlNifEnv *env, ERL_NIF_TERM reason);

    void ExecuteRunScript(JsExec *jsExec);
    void ExecuteSet(JsExec *jsExec);
    void ExecuteGet(JsExec *jsExec);
    void ExecuteCall(JsExec *jsExec);
    void ExecuteHeapStatistics(JsExec *jsExec);
    Handle<Value> ExecuteCallRespond(JsExec *jsExec);
    void Exit(JsExec *jsExec);
};

class JsWrapper {
  public:
    VmContext *vmContext;
    Persistent<Value> value;
    ErlJsWrapper *erlJsWrapper;
    ERL_NIF_TERM resourceTerm;

    JsWrapper(VmContext *_vmContext, ErlNifEnv *env, Persistent<Value> _value);
    ~JsWrapper();

    static ERL_NIF_TERM MakeBinary(ErlNifEnv *env,
        Handle<Value> value);
    static ERL_NIF_TERM MakeTerm(VmContext *vmContext,
        ErlNifEnv *env,
        Local<Value> value);
    static ERL_NIF_TERM MakeTerm(ErlNifEnv *env,
        TryCatch trycatch);
};

class ErlWrapper {
  public:
    ErlNifEnv *env;
    VmContext *vmContext;
    ERL_NIF_TERM term;
    Persistent<Value> persistent;

    ErlWrapper(VmContext *_vmContext, ERL_NIF_TERM _term);
    ~ErlWrapper();

    Persistent<External> MakeExternal();
    static Local<Value> MakeHandle(VmContext *vmContext,
        ErlNifEnv *env,
        ERL_NIF_TERM term);
};
