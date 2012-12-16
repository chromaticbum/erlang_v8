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
  SCRIPT,
  CALL,
  CALL_RESPOND,
  SET_FIELD,
  GET_FIELD
} JsCallType;

typedef struct {
  ErlNifPid pid;
  JsCallType type;
  void *data;
} JsCall;

typedef struct {
  Persistent<Value> value;
  char *field;
} JsCallObject;

typedef struct {
  JsWrapper *jsWrapper;
  char *field;
  ERL_NIF_TERM term;
} JsSetField;

typedef struct {
  JsWrapper *jsWrapper;
  char *field;
} JsGetField;

typedef struct {
  ERL_NIF_TERM term;
} JsCallRespond;

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
    JsCall *jsCall;

    VmContext(Vm *_vm, ErlNifEnv *env);
    ~VmContext();

    void SetServer(ErlNifPid pid);
    ERL_NIF_TERM MakeTerm(ErlNifEnv *env);
    bool Run();
    void Stop();
    void RunLoop();
    Handle<Value> Poll();
    ERL_NIF_TERM Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term);
    ERL_NIF_TERM SendScript(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term);
    ERL_NIF_TERM SendCall(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM obj,
        ERL_NIF_TERM field,
        ERL_NIF_TERM args);
    ERL_NIF_TERM SendCallRespond(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM term);
    ERL_NIF_TERM SendSetField(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM wrapperTerm,
        ERL_NIF_TERM fieldTerm,
        ERL_NIF_TERM term);
    ERL_NIF_TERM SendGetField(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM wrapperTerm,
        ERL_NIF_TERM fieldTerm);
    void PostResult(ErlNifPid pid, Local<Value> result);
    void PostResult(ErlNifPid pid, ERL_NIF_TERM term);
    JsCall *ResetJsCall();

    void ExecuteScript(JsCall *jsCall);
    void ExecuteCall(JsCall *jsCall);
    void ExecuteSetField(JsCall *jsCall);
    void ExecuteGetField(JsCall *jsCall);
    Handle<Value> ExecuteCallRespond(JsCall *jsCall);
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
    Local<Value> Get(char *field);
    static ERL_NIF_TERM MakeTerm(VmContext *vmContext,
        ErlNifEnv *env,
        Local<Value> value);
};

class ErlWrapper {
  public:
    ErlNifEnv *env;
    VmContext *vmContext;
    ERL_NIF_TERM term;
    Persistent<Value> persistent;

    ErlWrapper(VmContext *_vmContext, ERL_NIF_TERM _term);
    ~ErlWrapper();

    Local<External> MakeExternal();
    static Local<Value> MakeHandle(VmContext *vmContext,
        ERL_NIF_TERM term);
};
