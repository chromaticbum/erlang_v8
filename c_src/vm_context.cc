#include "erlang_v8_drv.h"

static void *StartRunLoop(void *ptr) {
  VmContext *vmContext = (VmContext *)ptr;
  vmContext->RunLoop();

  return NULL;
}

VmContext::VmContext(Vm *_vm) {
  TRACE("VmContext::VmContext\n");
  vm = _vm;

  env = enif_alloc_env();
  enif_keep_resource(vm->erlVm);
  erlVmContext = (ErlVmContext *)enif_alloc_resource(VmContextResource, sizeof(ErlVmContext));
  erlVmContext->vmContext = this;

  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  HandleScope handle_scope;
  context = Persistent<Context>::New(Context::New());

  cond = enif_cond_create((char *)"VmContext.cond");
  mutex = enif_mutex_create((char *)"VmContext.mutex");
  Run();
}

VmContext::~VmContext() {
  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  context.Dispose();
  context.Clear();

  enif_release_resource(vm->erlVm);
  enif_release_resource(erlVmContext);
  enif_cond_destroy(cond);
  enif_mutex_destroy(mutex);
  enif_clear_env(env);
  enif_free_env(env);
}

ERL_NIF_TERM VmContext::MakeTerm(ErlNifEnv *env) {
  TRACE("VmContext::MakeTerm\n");
  return enif_make_resource(env, erlVmContext);
}

int VmContext::Run() {
  int thread = enif_thread_create(
      (char *)"VmContext::Run",
      &tid,
      StartRunLoop,
      (void *)this,
      NULL);

  if(thread == 0) {
    return 1;
  } else {
    return 0;
  }
}

void VmContext::PostResult(JsCall *jsCall, Persistent<Value> result) {
  JsWrapper *jsWrapper = new JsWrapper(this, result);
  ERL_NIF_TERM term = enif_make_tuple2(env,
      enif_make_atom(env, "result"),
      jsWrapper->MakeTerm(env)
      );

  // TODO: error handling
  enif_send(NULL, &(jsCall->pid), env, term);
}

void VmContext::ExecuteScript(JsCall *jsCall) {
  LHCS(this);
  char *sourceBuffer = (char *)jsCall->data;
  Handle<String> source = String::New(sourceBuffer);
  Handle<Script> script = Script::Compile(source);
  Persistent<Value> result = Persistent<Value>::New(script->Run());
  PostResult(jsCall, result);
}

void VmContext::ResetJsCall() {
  jsCall = NULL;
}

Persistent<Value> VmContext::Poll() {
  enif_mutex_lock(mutex);
  while(jsCall == NULL) {
    enif_cond_wait(cond, mutex);
  }
  enif_mutex_unlock(mutex);

  JsCall *jsCall2 = jsCall;
  ResetJsCall();
  switch(jsCall2->type) {
    case SCRIPT:
      ExecuteScript(jsCall2);
  }

  return Poll();
}

int VmContext::Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term) {
  ErlNifBinary binary;

  if(enif_inspect_iolist_as_binary(env, term, &binary)) {
    char *script = (char *)malloc((binary.size + 1) * sizeof(char));
    memcpy(script, binary.data, binary.size);
    script[binary.size] = NULL;

    jsCall = (JsCall *)malloc(sizeof(JsCall));
    jsCall->pid = pid;
    jsCall->type = SCRIPT;
    jsCall->data = (void *)script;
    enif_cond_broadcast(cond);

    return 1;
  } else {
    return 0;
  }
}

void VmContext::RunLoop() {
  Poll();
}
