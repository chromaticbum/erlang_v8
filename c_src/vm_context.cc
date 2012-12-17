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

void VmContext::PostResult(ErlNifPid pid, Local<Value> result) {
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

void VmContext::PostResult(ErlNifPid pid, ERL_NIF_TERM term) {
  TRACE("VmContext::PostResult(term)\n");
  ErlNifEnv *env = enif_alloc_env();
  ERL_NIF_TERM result = enif_make_tuple2(env,
      enif_make_atom(env, "result"),
      enif_make_copy(env, term)
      );

  // TODO: error handling
  enif_send(NULL, &pid, env, result);
  enif_clear_env(env);
  enif_free_env(env);
}

void VmContext::ExecuteScript(JsCall *jsCall) {
  TRACE("VmContext::ExecuteScript\n");
  LHCS(this);
  char *sourceBuffer = (char *)jsCall->data;
  Handle<String> source = String::New(sourceBuffer);
  Handle<Script> script = Script::Compile(source);
  Local<Value> result = script->Run();
  TRACE("VmContext::ExecuteScript - 1\n");
  PostResult(jsCall->pid, result);
  TRACE("VmContext::ExecuteScript - 2\n");

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
  TRACE("VmContext::ExecuteCall\n");
  LHCS(this);
  JsCallObject *jsCallObject = (JsCallObject *)jsCall->data;
  ErlNifEnv *env = jsCallObject->env;
  ERL_NIF_TERM erlArgs, head, tail;
  ERL_NIF_TERM term;
  unsigned length;
  erlArgs = jsCallObject->args;
  Handle<String> field = String::New(jsCallObject->field);
  Handle<Function> fun = Local<Function>::Cast(jsCallObject->value->ToObject()->Get(field));

  if(enif_get_list_length(env, erlArgs, &length)) {
    Handle<Value> *args = (Handle<Value> *)malloc(sizeof(Handle<Value>) * length);

    int i = 0;
    tail = erlArgs;
    while(enif_get_list_cell(env, erlArgs, &head, &tail)) {
      args[i] = ErlWrapper::MakeHandle(this,
          jsCallObject->env, head);
      erlArgs = tail;
      i++;
    }

    Local<Value> result = fun->Call(jsCallObject->value->ToObject(), length, args);
    free(args);
    term = JsWrapper::MakeTerm(this, env, result);
  } else {
    term = enif_make_atom(env, "bad_args");
  }

  PostResult(jsCall->pid, term);

  enif_clear_env(jsCallObject->env);
  enif_free_env(jsCallObject->env);
  free(jsCallObject->field);
  free(jsCallObject);
  free(jsCall);
}

void VmContext::ExecuteSetField(JsCall *jsCall) {
  LHCS(this);
  JsSetField *jsSetField = (JsSetField *)jsCall->data;
  ERL_NIF_TERM term = jsSetField->jsWrapper->Set(jsSetField->env, jsSetField->field, jsSetField->term);
  PostResult(jsCall->pid, term);

  enif_clear_env(jsSetField->env);
  enif_free_env(jsSetField->env);
  free(jsSetField->field);
  free(jsSetField);
  free(jsCall);
}

void VmContext::ExecuteGetField(JsCall *jsCall) {
  TRACE("VmContext::ExecuteGetField\n");
  LHCS(this);

  JsGetField *jsGetField = (JsGetField *)jsCall->data;
  Local<Value> value = jsGetField->jsWrapper->Get(jsGetField->field);
  PostResult(jsCall->pid, value);

  free(jsGetField->field);
  free(jsGetField);
  free(jsCall);
}

Handle<Value> VmContext::ExecuteCallRespond(JsCall *jsCall) {
  TRACE("VmContext::ExecuteCallRespond\n");
  JsCallRespond *jsCallRespond = (JsCallRespond *)jsCall->data;
  ERL_NIF_TERM term = jsCallRespond->term;

  Handle<Value> value = ErlWrapper::MakeHandle(this, jsCallRespond->env, term);

  enif_clear_env(jsCallRespond->env);
  enif_free_env(jsCallRespond->env);
  free(jsCallRespond);
  free(jsCall);

  return value;
}

void VmContext::ExecuteErlNative(JsCall *jsCall) {
  LHCS(this);
  TRACE("VmContext::ExecuteErlNative\n");
  JsWrapper *jsWrapper = (JsWrapper *)jsCall->data;
  Local<Value> value = Local<Value>::New(jsWrapper->value);
  ErlNifEnv *env = enif_alloc_env();
  ERL_NIF_TERM term = JsWrapper::MakeNativeTerm(this, env, value);

  PostResult(jsCall->pid, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsCall);
}

void VmContext::ExecuteHeapStatistics(JsCall *jsCall) {
  LHCS(this);

  HeapStatistics hs;
  V8::GetHeapStatistics(&hs);

  ErlNifEnv *env = enif_alloc_env();
  ERL_NIF_TERM term = enif_make_list4(env,
      enif_make_tuple2(env,
        enif_make_atom(env, "total_heap_size"),
        enif_make_uint(env, hs.total_heap_size())
        ),
      enif_make_tuple2(env,
        enif_make_atom(env, "total_heap_size_executable"),
        enif_make_uint(env, hs.total_heap_size_executable())
        ),
      enif_make_tuple2(env,
        enif_make_atom(env, "used_heap_size"),
        enif_make_uint(env, hs.used_heap_size())
        ),
      enif_make_tuple2(env,
        enif_make_atom(env, "heap_size_limit"),
        enif_make_uint(env, hs.heap_size_limit())
        )
      );
  PostResult(jsCall->pid, term);
  enif_clear_env(env);
  enif_free_env(env);

  free(jsCall);
}

Handle<Value> VmContext::Poll() {
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
    case CALL:
      ExecuteCall(jsCall2);
      break;
    case SET_FIELD:
      ExecuteSetField(jsCall2);
      break;
    case GET_FIELD:
      ExecuteGetField(jsCall2);
      break;
    case ERL_NATIVE:
      ExecuteErlNative(jsCall2);
      break;
    case HEAP_STATISTICS:
      ExecuteHeapStatistics(jsCall2);
      break;
    case EXIT:
      Exit(jsCall2);
      break;
    case CALL_RESPOND:
      return ExecuteCallRespond(jsCall2);
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
      jsCallObject->env = enif_alloc_env();
      jsCallObject->args = enif_make_copy(jsCallObject->env, args);

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
  JsCallRespond *jsCallRespond = (JsCallRespond *)malloc(sizeof(JsCallRespond));
  jsCallRespond->env = enif_alloc_env();
  jsCallRespond->term = enif_make_copy(env, term);

  jsCall = (JsCall *)malloc(sizeof(JsCall));
  jsCall->pid = pid;
  jsCall->type = CALL_RESPOND;
  jsCall->data = jsCallRespond;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::SendSetField(ErlNifEnv *env,
    ErlNifPid pid,
    ERL_NIF_TERM wrapperTerm,
    ERL_NIF_TERM fieldTerm,
    ERL_NIF_TERM term) {
  ErlJsWrapper *erlJsWrapper;

  if(enif_get_resource(env, wrapperTerm, JsWrapperResource, (void **)(&erlJsWrapper))) {
    ErlNifBinary binary;

    if(enif_inspect_binary(env, fieldTerm, &binary)) {
      char *field = (char *)malloc((binary.size + 1) * sizeof(char));
      memcpy(field, binary.data, binary.size);
      field[binary.size] = NULL;

      JsSetField *jsSetField = (JsSetField *)malloc(sizeof(JsSetField));
      jsSetField->jsWrapper = erlJsWrapper->jsWrapper;
      jsSetField->field = field;
      jsSetField->env = enif_alloc_env();
      jsSetField->term = enif_make_copy(jsSetField->env, term);

      jsCall = (JsCall *)malloc(sizeof(JsCall));
      jsCall->pid = pid;
      jsCall->type = SET_FIELD;
      jsCall->data = jsSetField;

      return enif_make_atom(env, "ok");
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

ERL_NIF_TERM VmContext::SendGetField(ErlNifEnv *env,
        ErlNifPid pid,
        ERL_NIF_TERM wrapperTerm,
        ERL_NIF_TERM fieldTerm) {
  ErlJsWrapper *erlJsWrapper;

  if(enif_get_resource(env, wrapperTerm, JsWrapperResource, (void **)(&erlJsWrapper))) {
    ErlNifBinary binary;

    if(enif_inspect_binary(env, fieldTerm, &binary)) {
      char *field = (char *)malloc((binary.size + 1) * sizeof(char));
      memcpy(field, binary.data, binary.size);
      field[binary.size] = NULL;

      JsGetField *jsGetField = (JsGetField *)malloc(sizeof(JsGetField));
      jsGetField->jsWrapper = erlJsWrapper->jsWrapper;
      jsGetField->field = field;

      jsCall = (JsCall *)malloc(sizeof(JsCall));
      jsCall->pid = pid;
      jsCall->type = GET_FIELD;
      jsCall->data = jsGetField;

      return enif_make_atom(env, "ok");
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

ERL_NIF_TERM VmContext::SendHeapStatistics(ErlNifEnv *env,
    ErlNifPid pid) {
  jsCall = (JsCall *)malloc(sizeof(JsCall));
  jsCall->pid = pid;
  jsCall->type = HEAP_STATISTICS;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::SendErlNative(ErlNifEnv *env,
    ErlNifPid pid,
    ERL_NIF_TERM term) {
  TRACE("VmContext::SendErlNative\n");
  ErlJsWrapper *erlJsWrapper;

  if(enif_get_resource(env, term, JsWrapperResource, (void **)(&erlJsWrapper))) {
    jsCall = (JsCall *)malloc(sizeof(JsCall));
    jsCall->pid = pid;
    jsCall->type = ERL_NATIVE;
    jsCall->data = erlJsWrapper->jsWrapper;

    return enif_make_atom(env, "ok");
  } else {
    return enif_make_badarg(env);
  }
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
        } else if(strncmp(buffer, (char *)"set_field", length) == 0) {
          result = SendSetField(env, pid, command[1], command[2], command[3]);
        } else if(strncmp(buffer, (char *)"get_field", length) == 0) {
          result = SendGetField(env, pid, command[1], command[2]);
        } else if(strncmp(buffer, (char *)"heap_statistics", length) == 0) {
          result = SendHeapStatistics(env, pid);
        } else if(strncmp(buffer, (char *)"erl_native", length) == 0) {
          result = SendErlNative(env, pid, command[1]);
        } else {
          result = enif_make_badarg(env);
        }
      } else {
        result = enif_make_badarg(env);
      }
      free(buffer);
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
