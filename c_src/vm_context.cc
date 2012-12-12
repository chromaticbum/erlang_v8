#include "erlang_v8_drv.h"

static int id = 0;

static void *StartRunLoop(void *ptr) {
  VmContext *vmContext = (VmContext *)ptr;
  vmContext->RunLoop();

  return NULL;
}

VmContext::VmContext(Vm *_vm, ErlNifEnv *env, ErlNifPid _server) {
  TRACE("VmContext::VmContext\n");
  vm = _vm;
  server = _server;

  erlVmContext = (ErlVmContext *)enif_alloc_resource(VmContextResource, sizeof(ErlVmContext));
  erlVmContext->vmContext = this;
  term = enif_make_resource(env, erlVmContext);
  enif_release_resource(erlVmContext);
  enif_keep_resource(vm->erlVm);

  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  HandleScope handle_scope;
  context = Persistent<Context>::New(Context::New());

  string condName = "VmContext.cond" + id++;
  string mutexName = "VmContext.mutex" + id++;
  string condName2 = "VmContext.cond" + id++;
  string mutexName2 = "VmContext.mutex" + id++;
  cond = enif_cond_create((char *)condName.c_str());
  mutex = enif_mutex_create((char *)mutexName.c_str());
  cond2 = enif_cond_create((char *)condName2.c_str());
  mutex2 = enif_mutex_create((char *)mutexName2.c_str());
  Run();
}

VmContext::~VmContext() {
  TRACE("VmContext::~VmContext\n");
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

void VmContext::PostResult(ErlNifPid pid, Persistent<Value> result) {
  TRACE("VmContext::PostResult\n");
  ErlNifEnv *env = enif_alloc_env();
  JsWrapper *jsWrapper = new JsWrapper(this, env, result);
  ERL_NIF_TERM term = enif_make_tuple2(env,
      enif_make_atom(env, "result"),
      jsWrapper->MakeTerm(env)
      );

  // TODO: error handling
  enif_send(NULL, &pid, env, term);
  enif_clear_env(env);
  enif_free_env(env);
}

void VmContext::ExecuteScript(JsCall *jsCall) {
  TRACE("VmContext::ExecuteScript\n");
  LHCS(this);
  char *sourceBuffer = (char *)jsCall->data;
  Handle<String> source = String::New(sourceBuffer);
  Handle<Script> script = Script::Compile(source);
  Persistent<Value> result = Persistent<Value>::New(script->Run());
  ErlNifPid pid = jsCall->pid;
  TRACE("VmContext::ExecuteScript - 1\n");
  PostResult(jsCall->pid, result);
  TRACE("VmContext::ExecuteScript - 2\n");
  FreeJsCall(jsCall);
}

void VmContext::FreeJsCall(JsCall *jsCall) {
  free(jsCall->data);
  free(jsCall);
}

JsCall *VmContext::ResetJsCall() {
  JsCall *jsCall2 = NULL;
  TRACE("VmContext::ResetJsCall\n");
  if(jsCall) {
    TRACE("VmContext::ResetJsCall - 1\n");
    jsCall2 = jsCall;
    jsCall = NULL;
  }

  return jsCall2;
}

void VmContext::Exit(JsCall *jsCall) {
  free(jsCall);
  enif_mutex_unlock(mutex);
  enif_thread_exit(NULL);
}

Persistent<Value> VmContext::Poll() {
  TRACE("VmContext::Poll\n");

  while(jsCall == NULL) {
    TRACE("VmContext::Poll - 3\n");
    enif_cond_wait(cond, mutex);
  }
  TRACE("VmContext::Poll - 4\n");
  JsCall *jsCall2 = ResetJsCall();
  enif_mutex_lock(mutex2);
  enif_cond_broadcast(cond2);
  enif_mutex_unlock(mutex2);

  TRACE("VmContext::Poll - 5\n");
  switch(jsCall2->type) {
    case SCRIPT:
      ExecuteScript(jsCall2);
      break;
    case EXIT:
      Exit(jsCall2);
      break;
    default:
      TRACE("VmContext::Poll - Default\n");
  }
  TRACE("VmContext::Poll - 6\n");
  return Poll();
}

bool VmContext::Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term) {
  TRACE("VmContext::Send\n");
  ErlNifBinary binary;

  if(enif_inspect_iolist_as_binary(env, term, &binary)) {
    char *script = (char *)malloc((binary.size + 1) * sizeof(char));
    memcpy(script, binary.data, binary.size);
    script[binary.size] = NULL;

    jsCall = (JsCall *)malloc(sizeof(JsCall));
    jsCall->pid = pid;
    jsCall->type = SCRIPT;
    jsCall->data = script;

    enif_cond_broadcast(cond);
    enif_mutex_unlock(mutex);
    while(jsCall != NULL) {
      enif_cond_wait(cond2, mutex2);
    }
    enif_mutex_unlock(mutex2);

    return true;
  } else {
    return false;
  }
}

void VmContext::RunLoop() {
  enif_mutex_lock(mutex);
  Poll();
}
