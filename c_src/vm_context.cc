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

void VmContext::PostResult(ErlNifPid pid,
    ErlNifEnv *env,
    ERL_NIF_TERM term) {
  TRACE("VmContext::PostResult(term)\n");
  ERL_NIF_TERM result = enif_make_tuple2(env,
      enif_make_atom(env, "result"),
      enif_make_copy(env, term)
      );

  // TODO: error handling
  enif_send(NULL, &pid, env, result);
}

void VmContext::ExecuteRunScript(JsCall *jsCall) {
  TRACE("VmContext::ExecuteRunScript\n");
  LHCS(this);
  TryCatch trycatch;

  char *sourceBuffer = (char *)jsCall->data;
  Handle<String> source = String::New(sourceBuffer);
  Handle<Script> script = Script::Compile(source);
  Local<Value> result = script->Run();

  ErlNifEnv *env = enif_alloc_env();
  if(!result.IsEmpty()) {
    ERL_NIF_TERM term = JsWrapper::MakeTerm(this,
        env, result);
    PostResult(jsCall->pid, env, term);
  } else {
    Handle<Value> exception = trycatch.Exception();
    Handle<Value> stackTrace = trycatch.StackTrace();
    String::AsciiValue exceptionStr(exception);
    String::AsciiValue stackTraceStr(stackTrace);
    ERL_NIF_TERM term;
    ERL_NIF_TERM stTerm;
    char *buffer = (char *)enif_make_new_binary(env, strlen(*exceptionStr), &term);
    char *stBuffer = (char *)enif_make_new_binary(env, strlen(*stackTraceStr), &stTerm);
    memcpy(buffer, *exceptionStr, strlen(*exceptionStr));
    memcpy(stBuffer, *stackTraceStr, strlen(*stackTraceStr));

    PostResult(jsCall->pid, env, enif_make_tuple2(env,
          enif_make_atom(env, "error"),
          enif_make_tuple3(env,
            enif_make_atom(env, "js_compile_error"),
            term,
            stTerm)
          ));
  }

  enif_clear_env(env);
  enif_free_env(env);

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

void VmContext::ExecuteSet(JsCall *jsCall) {
  LHCS(this);
  JsSet*jsSet= (JsSet*)jsCall->data;
  ERL_NIF_TERM term = jsSet->jsWrapper->Set(jsSet->env,
      jsSet->field,
      jsSet->term);
  PostResult(jsCall->pid, jsSet->env, term);

  enif_clear_env(jsSet->env);
  enif_free_env(jsSet->env);
  free(jsSet->field);
  free(jsSet);
  free(jsCall);
}

void VmContext::ExecuteGet(JsCall *jsCall) {
  TRACE("VmContext::ExecuteGet\n");
  LHCS(this);

  ErlNifEnv *env = enif_alloc_env();
  JsGet *jsGet= (JsGet*)jsCall->data;
  Local<Value> value = jsGet->jsWrapper->Get(jsGet->field);
  ERL_NIF_TERM term = JsWrapper::MakeTerm(this,
      env, value);
  PostResult(jsCall->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsGet->field);
  free(jsGet);
  free(jsCall);
}

Handle<Value> VmContext::ExecuteCallRespond(JsCall *jsCall) {
  TRACE("VmContext::ExecuteCallRespond\n");
  JsCallRespond *jsCallRespond = (JsCallRespond *)jsCall->data;
  ErlNifEnv *env = jsCallRespond->env;
  Handle<Value> value;
  ERL_NIF_TERM term = jsCallRespond->term;
  const ERL_NIF_TERM *terms;
  int arity;
  unsigned length;

  if(enif_get_tuple(env, term, &arity, &terms)) {
    TRACE("WE HAVE TUPLE\n");

    if(arity == 2 && enif_get_atom_length(env, terms[0], &length, ERL_NIF_LATIN1)) {
      char *buffer = (char *)malloc((length + 1) * sizeof(char));
      enif_get_atom(env, terms[0], buffer, length + 1, ERL_NIF_LATIN1);
      if(strncmp(buffer, (char *)"ok", length) == 0) {
        value = ErlWrapper::MakeHandle(this, env, terms[1]);
      } else if(strncmp(buffer, (char *)"error", length) == 0) {
        value = Exception::Error(String::New("erlang function returned error"));
      } else {
        value = Exception::Error(String::New("erlang function returned error"));
      }
    } else {
      value = Exception::Error(String::New("erlang function returned error"));
    }
  } else {
    value = Exception::Error(String::New("erlang function returned error"));
    // TODO handle error condition
  }

  enif_clear_env(jsCallRespond->env);
  enif_free_env(jsCallRespond->env);
  free(jsCallRespond);
  free(jsCall);

  return value;
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
  PostResult(jsCall->pid, env, term);

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
    case RUN_SCRIPT:
      ExecuteRunScript(jsCall2);
      break;
    case SET:
      ExecuteSet(jsCall2);
      break;
    case GET:
      ExecuteGet(jsCall2);
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

ERL_NIF_TERM VmContext::SendRunScript(ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term) {
  ErlNifBinary binary;

  if(enif_inspect_iolist_as_binary(env, term, &binary)) {
    char *script = (char *)malloc((binary.size + 1) * sizeof(char));
    memcpy(script, binary.data, binary.size);
    script[binary.size] = NULL;

    jsCall = (JsCall *)malloc(sizeof(JsCall));
    jsCall->pid = pid;
    jsCall->type = RUN_SCRIPT;
    jsCall->data = script;

    return enif_make_atom(env, "ok");
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

ERL_NIF_TERM VmContext::SendSet(ErlNifEnv *env,
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

      JsSet *jsSet = (JsSet *)malloc(sizeof(JsSet));
      jsSet->jsWrapper = erlJsWrapper->jsWrapper;
      jsSet->field = field;
      jsSet->env = enif_alloc_env();
      jsSet->term = enif_make_copy(jsSet->env, term);

      jsCall = (JsCall *)malloc(sizeof(JsCall));
      jsCall->pid = pid;
      jsCall->type = SET;
      jsCall->data = jsSet;

      return enif_make_atom(env, "ok");
    } else {
      return enif_make_badarg(env);
    }
  } else {
    return enif_make_badarg(env);
  }
}

ERL_NIF_TERM VmContext::SendGet(ErlNifEnv *env,
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

      JsGet *jsGet = (JsGet*)malloc(sizeof(JsGet));
      jsGet->jsWrapper = erlJsWrapper->jsWrapper;
      jsGet->field = field;

      jsCall = (JsCall *)malloc(sizeof(JsCall));
      jsCall->pid = pid;
      jsCall->type = GET;
      jsCall->data = jsGet;

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
        if(strncmp(buffer, (char *)"run_script", length) == 0) {
          result = SendRunScript(env, pid, command[1]);
        } else if(strncmp(buffer, (char *)"call_respond", length) == 0) {
          result = SendCallRespond(env, pid, command[1]);
        } else if(strncmp(buffer, (char *)"set", length) == 0) {
          result = SendSet(env, pid, command[1], command[2], command[3]);
        } else if(strncmp(buffer, (char *)"get", length) == 0) {
          result = SendGet(env, pid, command[1], command[2]);
        } else if(strncmp(buffer, (char *)"heap_statistics", length) == 0) {
          result = SendHeapStatistics(env, pid);
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
