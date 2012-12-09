#include "erlang_v8_drv.h"

static void *StartRunLoop(void *ptr) {
  VmContext *vmContext = (VmContext *)ptr;
  vmContext->RunLoop();

  return NULL;
}

VmContext::VmContext(Vm *_vm, ErlNifEnv *_env) {
  TRACE("VmContext::VmContext\n");
  vm = _vm;
  env = _env;

  erlVmContext = (ErlVmContext *)enif_alloc_resource(VmContextResource, sizeof(ErlVmContext));
  erlVmContext->vmContext = this;
  term = enif_make_resource(env, erlVmContext);
  enif_release_resource(erlVmContext);
  enif_keep_resource(vm->erlVm);

  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  HandleScope handle_scope;
  context = Persistent<Context>::New(Context::New());

  cond = enif_cond_create((char *)"VmContext.cond");
  mutex = enif_mutex_create((char *)"VmContext.mutex");
  Run();
}

VmContext::~VmContext() {
  Stop();
  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  context.Dispose();
  context.Clear();

  enif_release_resource(vm->erlVm);
  enif_cond_destroy(cond);
  enif_mutex_destroy(mutex);
}

ERL_NIF_TERM VmContext::MakeTerm(ErlNifEnv *env) {
  TRACE("VmContext::MakeTerm\n");
  return enif_make_resource(env, erlVmContext);
}

bool VmContext::Run() {
  int thread = enif_thread_create(
      (char *)"VmContext::Run",
      &tid,
      StartRunLoop,
      (void *)this,
      NULL);

  if(thread == 0) {
    return true;
  } else {
    return false;
  }
}

void VmContext::Stop() {
  TRACE("VmContext::Stop\n");
  jsCall = (JsCall *)malloc(sizeof(JsCall));
  jsCall->type = EXIT;
  enif_cond_broadcast(cond);

  void *result;
  enif_thread_join(tid, &result);
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

void VmContext::ExecuteScript() {
  LHCS(this);
  char *sourceBuffer = (char *)jsCall->data;
  Handle<String> source = String::New(sourceBuffer);
  Handle<Script> script = Script::Compile(source);
  Persistent<Value> result = Persistent<Value>::New(script->Run());
  PostResult(jsCall, result);
  ResetJsCall();
}

void VmContext::ResetJsCall() {
  if(jsCall) {
    free(jsCall);
    jsCall = NULL;
  }
}

void VmContext::Exit() {
  ResetJsCall();
  enif_thread_exit(NULL);
}

Persistent<Value> VmContext::Poll() {
  enif_mutex_lock(mutex);
  while(jsCall == NULL) {
    enif_cond_wait(cond, mutex);
  }
  enif_mutex_unlock(mutex);

  switch(jsCall->type) {
    case SCRIPT:
      ExecuteScript();
      break;
    case EXIT:
      Exit();
      break;
  }

  return Poll();
}

bool VmContext::Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term) {
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

    return true;
  } else {
    return false;
  }
}

void VmContext::RunLoop() {
  Poll();
}
