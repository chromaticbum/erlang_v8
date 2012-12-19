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

  string condName = "VmContext.cond" + id++;
  string mutexName = "VmContext.mutex" + id++;
  string condName2 = "VmContext.cond" + id++;
  string mutexName2 = "VmContext.mutex" + id++;
  cond = enif_cond_create((char *)condName.c_str());
  mutex = enif_mutex_create((char *)mutexName.c_str());
  cond2 = enif_cond_create((char *)condName2.c_str());
  mutex2 = enif_mutex_create((char *)mutexName2.c_str());

  Locker locker(vm->isolate);
  Isolate::Scope iscope(vm->isolate);
  HandleScope handle_scope;
  context = Persistent<Context>::New(Context::New());

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
  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->type = EXIT;
  enif_cond_broadcast(cond);

  void *result;
  enif_thread_join(tid, &result);
}

void VmContext::PostResult(ErlNifPid pid,
    ErlNifEnv *env,
    ERL_NIF_TERM term) {
  TRACE("VmContext::PostResult(term)\n");
  ERL_NIF_TERM result = enif_make_tuple2(env,
      enif_make_atom(env, "ok"),
      term
      );

  // TODO: error handling
  enif_send(NULL, &pid, env, result);
}

char *MakeBuffer(ErlNifBinary binary) {
  char *buffer = (char *)malloc((binary.size + 1) * sizeof(char));
  memcpy(buffer, binary.data, binary.size);
  buffer[binary.size] = NULL;

  return buffer;
}

void VmContext::ExecuteRunScript(JsExec *jsExec) {
  TRACE("VmContext::ExecuteRunScript\n");
  LHCST(this);

  JsRunScript *jsRunScript = (JsRunScript *)jsExec->data;
  ErlNifEnv *env = jsRunScript->env;
  ERL_NIF_TERM originTerm = jsRunScript->originTerm;
  ERL_NIF_TERM scriptTerm = jsRunScript->scriptTerm;
  ErlNifBinary binary;
  int arity, line;
  const ERL_NIF_TERM *terms;
  ERL_NIF_TERM term;

  if(enif_get_tuple(env, originTerm, &arity, &terms) &&
      arity == 2 &&
      enif_inspect_iolist_as_binary(env, terms[0], &binary) &&
      enif_get_int(env, terms[1], &line)) {
    char *buffer = MakeBuffer(binary);
    Handle<String> resourceName = String::New(buffer);
    Handle<Integer> resourceLine = Integer::New(line - 1);
    ScriptOrigin origin(resourceName, resourceLine);
    free(buffer);

    if(enif_inspect_iolist_as_binary(env, scriptTerm, &binary)) {
      buffer = MakeBuffer(binary);
      Handle<String> source = String::New(buffer);
      Handle<Script> script = Script::Compile(source, &origin);
      Local<Value> result = script->Run();

      if(!result.IsEmpty()) {
        term = JsWrapper::MakeTerm(this,
            env, result);
      } else {
        term = MakeError(env,
            JsWrapper::MakeTerm(env, COMPILER, trycatch));
      }

      free(buffer);
    } else {
      term = MakeError(env, "invalid_script");
    } 

  } else {
    term = MakeError(env, "invalid_origin");
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsRunScript);
  free(jsExec);
}

JsExec *VmContext::ResetJsExec() {
  JsExec *jsExec2 = NULL;
  TRACE("VmContext::ResetJsExec\n");
  if(jsExec) {
    TRACE("VmContext::ResetJsExec - 1\n");
    jsExec2 = jsExec;
    jsExec = NULL;
  }

  return jsExec2;
}

ERL_NIF_TERM VmContext::MakeError(ErlNifEnv *env, const char *reason) {
  return MakeError(env, enif_make_atom(env, reason));
}

ERL_NIF_TERM VmContext::MakeError(ErlNifEnv *env, ERL_NIF_TERM reason) {
  return enif_make_tuple2(env,
      enif_make_atom(env, "error"),
      reason);
}

void VmContext::Exit(JsExec *jsExec) {
  free(jsExec);
  enif_mutex_unlock(mutex);
  enif_thread_exit(NULL);
}

void VmContext::ExecuteSet(JsExec *jsExec) {
  LHCST(this);

  JsSet *jsSet = (JsSet*)jsExec->data;
  ErlNifEnv *env = jsSet->env;
  ERL_NIF_TERM fieldsTerm = jsSet->fieldsTerm;
  ERL_NIF_TERM head;
  Handle<Value> value = ErlWrapper::MakeHandle(this,
      env, jsSet->objectTerm);
  ERL_NIF_TERM term;
  int arity;
  const ERL_NIF_TERM *terms;

  if(value->IsObject()) {
    Handle<Object> obj = value->ToObject();

    while(enif_get_list_cell(env, fieldsTerm, &head, &fieldsTerm)) {
      if(enif_get_tuple(env, head, &arity, &terms) && arity == 2) {
        Local<Value> field = ErlWrapper::MakeHandle(this,
            env, terms[0]);
        Local<Value> fieldValue = ErlWrapper::MakeHandle(this,
            env, terms[1]);

        obj->Set(field, fieldValue);
      }
    }

    term = enif_make_atom(env, "ok");
  } else {
    term = MakeError(env, "invalid_object");
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(jsSet->env);
  enif_free_env(jsSet->env);
  free(jsSet);
  free(jsExec);
}

void VmContext::ExecuteGet(JsExec *jsExec) {
  TRACE("VmContext::ExecuteGet\n");
  LHCST(this);

  JsGet *jsGet = (JsGet*)jsExec->data;
  ErlNifEnv *env = jsGet->env;
  ERL_NIF_TERM term;
  Handle<Value> value = ErlWrapper::MakeHandle(this,
      env, jsGet->objectTerm);

  if(value->IsObject()) {
    Handle<Object> obj = value->ToObject();
    Local<Value> fieldHandle = ErlWrapper::MakeHandle(this,
        env, jsGet->fieldTerm);
    Local<Value> fieldValue = obj->Get(fieldHandle);

    term = JsWrapper::MakeTerm(this,
        env, fieldValue);
  } else {
    term = MakeError(env, "invalid_object");
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsGet);
  free(jsExec);
}

void VmContext::ExecuteCall(JsExec *jsExec) {
  TRACE("VmContext::ExecuteCall\n");
  LHCST(this);

  JsCall *jsCall = (JsCall *)jsExec->data;
  ErlNifEnv *env = jsCall->env;
  ERL_NIF_TERM term;
  ERL_NIF_TERM erlArgs, head;

  erlArgs = jsCall->args;
  Handle<Value> fun = ErlWrapper::MakeHandle(this,
      env, jsCall->fun);
  if(fun->IsFunction()) {
  TRACE("VmContext::ExecuteCall - 1\n");
    unsigned length;

    if(enif_get_list_length(env, erlArgs, &length)) {
  TRACE("VmContext::ExecuteCall - 2\n");
      Handle<Value> *args = (Handle<Value> *)malloc(length * sizeof(Handle<Value>));
      int i = 0;

      while(enif_get_list_cell(env, erlArgs, &head, &erlArgs)) {
        args[i] = ErlWrapper::MakeHandle(this,
            env, head);
        i++;
      }

      if(jsCall->type == NORMAL) {
  TRACE("VmContext::ExecuteCall - 3\n");
        Handle<Object> recv;
        Handle<Value> recvValue = ErlWrapper::MakeHandle(this,
            env, jsCall->recv);

        if(recvValue->IsObject()) {
  TRACE("VmContext::ExecuteCall - 4\n");
          recv = recvValue->ToObject();
        } else {
          recv = context->Global();
        }

  TRACE("VmContext::ExecuteCall - 5\n");
        Local<Value> result = fun->ToObject()->CallAsFunction(recv, length, args);
  TRACE("VmContext::ExecuteCall - 6\n");
        term = JsWrapper::MakeTerm(this, env, result);
  TRACE("VmContext::ExecuteCall - 7\n");
      } else {
        // Must be CONSTRUCTOR

        Local<Value> result = fun->ToObject()->CallAsConstructor(length, args);
        term = JsWrapper::MakeTerm(this, env, result);
      }

      free(args);
    } else {
      term = MakeError(env, "args_not_list");
    }
  } else {
    term = MakeError(env, "not_fun");
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsCall);
  free(jsExec);
}

Handle<Value> VmContext::ExecuteCallRespond(JsExec *jsExec) {
  TRACE("VmContext::ExecuteCallRespond\n");
  // Don't need LHCST here because
  // To get here there must already be one on the stack

  JsCallRespond *jsCallRespond = (JsCallRespond *)jsExec->data;
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
      if(strncmp(buffer, "ok", length) == 0) {
        value = ErlWrapper::MakeHandle(this, env, terms[1]);
      } else if(strncmp(buffer, "error", length) == 0) {
        value = ThrowException(Exception::Error(String::New("erlang function returned error")));
      } else {
        value = ThrowException(Exception::Error(String::New("erlang function returned error")));
      }
    } else {
      value = ThrowException(Exception::Error(String::New("erlang function returned error")));
    }
  } else {
    value = ThrowException(Exception::Error(String::New("erlang function returned error")));
    // TODO handle error condition
  }

  enif_clear_env(jsCallRespond->env);
  enif_free_env(jsCallRespond->env);
  free(jsCallRespond);
  free(jsExec);

  return value;
}

void VmContext::ExecuteHeapStatistics(JsExec *jsExec) {
  LHCST(this);

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
  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);

  free(jsExec);
}

Handle<Value> VmContext::Poll() {
  TRACE("VmContext::Poll\n");

  while(jsExec == NULL) {
    TRACE("VmContext::Poll - 3\n");
    enif_cond_wait(cond, mutex);
  }
  TRACE("VmContext::Poll - 4\n");
  JsExec *jsExec2 = ResetJsExec();
  enif_mutex_lock(mutex2);
  enif_cond_broadcast(cond2);
  enif_mutex_unlock(mutex2);

  TRACE("VmContext::Poll - 5\n");
  switch(jsExec2->type) {
    case RUN_SCRIPT:
      ExecuteRunScript(jsExec2);
      break;
    case SET:
      ExecuteSet(jsExec2);
      break;
    case GET:
      ExecuteGet(jsExec2);
      break;
    case CALL:
      ExecuteCall(jsExec2);
      break;
    case HEAP_STATISTICS:
      ExecuteHeapStatistics(jsExec2);
      break;
    case EXIT:
      Exit(jsExec2);
      break;
    case CALL_RESPOND:
      return ExecuteCallRespond(jsExec2);
    default:
      TRACE("VmContext::Poll - Default\n");
  }
  TRACE("VmContext::Poll - 6\n");
  return Poll();
}

ERL_NIF_TERM VmContext::SendRunScript(ErlNifEnv *env,
    ErlNifPid pid,
    ERL_NIF_TERM originTerm,
    ERL_NIF_TERM scriptTerm) {
  JsRunScript *jsRunScript = (JsRunScript *)malloc(sizeof(JsRunScript));
  jsRunScript->env = enif_alloc_env();
  jsRunScript->originTerm = enif_make_copy(jsRunScript->env, originTerm);
  jsRunScript->scriptTerm = enif_make_copy(jsRunScript->env, scriptTerm);

  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->pid = pid;
  jsExec->type = RUN_SCRIPT;
  jsExec->data = jsRunScript;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::SendCallRespond(ErlNifEnv *env,
    ErlNifPid pid, ERL_NIF_TERM term) {
  JsCallRespond *jsCallRespond = (JsCallRespond *)malloc(sizeof(JsCallRespond));
  jsCallRespond->env = enif_alloc_env();
  jsCallRespond->term = enif_make_copy(env, term);

  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->pid = pid;
  jsExec->type = CALL_RESPOND;
  jsExec->data = jsCallRespond;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::SendSet(ErlNifEnv *env,
    ErlNifPid pid,
    ERL_NIF_TERM objectTerm,
    ERL_NIF_TERM fieldsTerm) {
  JsSet *jsSet = (JsSet *)malloc(sizeof(JsSet));
  jsSet->env = enif_alloc_env();
  jsSet->objectTerm = enif_make_copy(jsSet->env, objectTerm);
  jsSet->fieldsTerm = enif_make_copy(jsSet->env, fieldsTerm);

  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->pid = pid;
  jsExec->type = SET;
  jsExec->data = jsSet;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::SendGet(ErlNifEnv *env,
    ErlNifPid pid,
    ERL_NIF_TERM objectTerm,
    ERL_NIF_TERM fieldTerm) {
  JsGet *jsGet = (JsGet*)malloc(sizeof(JsGet));
  jsGet->env = enif_alloc_env();
  jsGet->objectTerm = enif_make_copy(jsGet->env, objectTerm);
  jsGet->fieldTerm = enif_make_copy(jsGet->env, fieldTerm);

  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->pid = pid;
  jsExec->type = GET;
  jsExec->data = jsGet;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::SendCall(ErlNifEnv *env,
    ErlNifPid pid,
    JsCallType type,
    ERL_NIF_TERM recv,
    ERL_NIF_TERM fun,
    ERL_NIF_TERM args) {
  JsCall *jsCall = (JsCall *)malloc(sizeof(JsCall));
  jsCall->type = type;
  jsCall->env = enif_alloc_env();
  if(jsCall->type == NORMAL) {
    jsCall->recv = enif_make_copy(jsCall->env, recv);
  }
  jsCall->fun = enif_make_copy(jsCall->env, fun);
  jsCall->args = enif_make_copy(jsCall->env, args);

  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->pid = pid;
  jsExec->type = CALL;
  jsExec->data = jsCall;

  return enif_make_atom(env, "ok");
}

ERL_NIF_TERM VmContext::SendCall(ErlNifEnv *env,
    ErlNifPid pid,
    ERL_NIF_TERM type,
    ERL_NIF_TERM call) {
  unsigned length;
  int arity;
  const ERL_NIF_TERM *terms;
  ERL_NIF_TERM term;

  if(enif_get_tuple(env, call, &arity, &terms)) {
    if(enif_get_atom_length(env, type, &length, ERL_NIF_LATIN1)) {
      char *buffer = (char *)malloc((length + 1) * sizeof(char));

      if(enif_get_atom(env, type, buffer, length + 1, ERL_NIF_LATIN1)) {
        if(strncmp(buffer, "normal", length) == 0) {
          if(arity == 3) {
            term = SendCall(env, pid, NORMAL, terms[0], terms[1], terms[2]);
          } else {
            term = enif_make_badarg(env);
          }
        } else if(strncmp(buffer, "constructor", length) == 0) {
          if(arity == 2) {
            term = SendCall(env, pid, CONSTRUCTOR, 0, terms[0], terms[1]);
          }
        } else {
          term = enif_make_badarg(env);
        }
      } else {
        term = enif_make_badarg(env);
      }

      free(buffer);
    } else {
      term = enif_make_badarg(env);
    }
  } else {
    term = enif_make_badarg(env);
  }

  return term;
}

ERL_NIF_TERM VmContext::SendHeapStatistics(ErlNifEnv *env,
    ErlNifPid pid) {
  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->pid = pid;
  jsExec->type = HEAP_STATISTICS;

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
          result = SendRunScript(env, pid, command[1], command[2]);
        } else if(strncmp(buffer, (char *)"call", length) == 0) {
          result = SendCall(env, pid, command[1], command[2]);
        } else if(strncmp(buffer, (char *)"call_respond", length) == 0) {
          result = SendCallRespond(env, pid, command[1]);
        } else if(strncmp(buffer, (char *)"set", length) == 0) {
          result = SendSet(env, pid, command[1], command[2]);
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
  while(jsExec != NULL) {
    enif_cond_wait(cond2, mutex2);
  }
  enif_mutex_unlock(mutex2);

  return result;
}

void VmContext::RunLoop() {
  enif_mutex_lock(mutex);
  Poll();
}
