#include "erlang_v8_drv.h"

static int id = 0;

static void *StartRunLoop(void *ptr) {
  VmContext *vmContext = (VmContext *)ptr;
  vmContext->RunLoop();

  return NULL;
}

VmContext::VmContext(Vm *_vm, ErlNifEnv *env) {
  TRACE("VmContext::VmContext\n");
  vm = _vm;

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

void VmContext::SetServer(ErlNifPid pid) {
  server = pid; 
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
  ERL_NIF_TERM term = enif_make_tuple2(env,
      enif_make_atom(env, "result"),
      JsWrapper::MakeTerm(this, env, result)
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
  TRACE("VmContext::ExecuteScript - 1\n");
  PostResult(jsCall->pid, result);
  TRACE("VmContext::ExecuteScript - 2\n");
  FreeJsCall(jsCall);
}

void VmContext::FreeJsCall(JsCall *jsCall) {
  if(jsCall->type == CALL) {
      JsCallObject *jsCallObject = (JsCallObject *)jsCall->data;
      free(jsCallObject->field);
  }
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

void VmContext::ExecuteCall(JsCall *jsCall) {
  LHCS(this);
  JsCallObject *jsCallObject = (JsCallObject *)jsCall->data;
  Handle<String> field = String::New(jsCallObject->field);
  Handle<Function> fun = Local<Function>::Cast(jsCallObject->value->ToObject()->Get(field));
  Persistent<Value> result = Persistent<Value>::New(
      fun->Call(jsCallObject->value->ToObject(), 0, 0)
    );
  PostResult(jsCall->pid, result);
  FreeJsCall(jsCall);
}

Persistent<Value> VmContext::Poll() {
  TRACE("VmContext::Poll\n");
  ErlWrapper *erlWrapper;

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
    case CALL:
      ExecuteCall(jsCall2);
      break;
    case EXIT:
      Exit(jsCall2);
      break;
    case CALL_RESPOND:
      erlWrapper = (ErlWrapper *)jsCall2->data;
      return erlWrapper->MakeHandle();
    default:
      TRACE("VmContext::Poll - Default\n");
  }
  TRACE("VmContext::Poll - 6\n");
  return Poll();
}

ERL_NIF_TERM VmContext::SendScript(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term) {
  ErlNifBinary binary;

  if(enif_inspect_iolist_as_binary(env, term, &binary)) {
    char *script = (char *)malloc((binary.size + 1) * sizeof(char));
    memcpy(script, binary.data, binary.size);
    script[binary.size] = NULL;

    jsCall = (JsCall *)malloc(sizeof(JsCall));
    jsCall->pid = pid;
    jsCall->type = SCRIPT;
    jsCall->data = script;

    return enif_make_atom(env, "ok");
  } else {
    return enif_make_badarg(env);
  }
}

ERL_NIF_TERM VmContext::SendCall(ErlNifEnv *env,
    ErlNifPid pid,
    ERL_NIF_TERM obj,
    ERL_NIF_TERM field,
    ERL_NIF_TERM args) {
  ErlJsWrapper *erlJsWrapper;

  if(enif_get_resource(env, obj, JsWrapperResource, (void **)(&erlJsWrapper))) {
    ErlNifBinary binary;
    if(enif_inspect_iolist_as_binary(env, field, &binary)) {
      char *buffer = (char *)malloc((binary.size + 1) * sizeof(char));
      memcpy(buffer, binary.data, binary.size);
      buffer[binary.size] = NULL;

      JsCallObject *jsCallObject = (JsCallObject *)malloc(sizeof(JsCallObject));
      jsCallObject->value = erlJsWrapper->jsWrapper->value;
      jsCallObject->field = buffer;

      jsCall = (JsCall *)malloc(sizeof(JsCall));
      jsCall->pid = pid;
      jsCall->type = CALL;
      jsCall->data = jsCallObject;

      return enif_make_atom(env, "ok");
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

ERL_NIF_TERM VmContext::SendCallRespond(ErlNifEnv *env,
    ErlNifPid pid, ERL_NIF_TERM term) {
  ErlWrapper *erlWrapper = new ErlWrapper(this, term);

  jsCall = (JsCall *)malloc(sizeof(JsCall));
  jsCall->pid = pid;
  jsCall->type = CALL_RESPOND;
  jsCall->data = erlWrapper;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::Send(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term) {
  TRACE("VmContext::Send\n");
  const ERL_NIF_TERM *command;
  int arity;
  ERL_NIF_TERM result;

  if(enif_get_tuple(env, term, &arity, &command)) {
    unsigned length;
    if(enif_get_atom_length(env, command[0], &length, ERL_NIF_LATIN1)) {
      char *buffer = (char *)malloc((length + 1) * sizeof(char));
      if(enif_get_atom(env, command[0], buffer, length + 1, ERL_NIF_LATIN1)) {
        if(strncmp(buffer, (char *)"script", length) == 0) {
          result = SendScript(env, pid, command[1]);
        } else if(strncmp(buffer, (char *)"call", length) == 0) {
          result = SendCall(env, pid, command[1], command[2], command[3]);
        } else if(strncmp(buffer, (char *)"call_respond", length) == 0) {
          result = SendCallRespond(env, pid, command[1]);
        }else {
          result = enif_make_badarg(env);
        }
      } else {
        result = enif_make_badarg(env);
      }
    } else {
      result = enif_make_badarg(env);
    }
  } else {
    result = enif_make_badarg(env);
  }

  enif_cond_broadcast(cond);
  enif_mutex_unlock(mutex);
  while(jsCall != NULL) {
    enif_cond_wait(cond2, mutex2);
  }
  enif_mutex_unlock(mutex2);

  return result;
}

void VmContext::RunLoop() {
  enif_mutex_lock(mutex);
  Poll();
}
