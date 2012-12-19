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

  ErlNifBinary binary;
  int arity, line;
  const ERL_NIF_TERM *terms;
  ERL_NIF_TERM term;
  ErlNifEnv *env = jsExec->env;

  if(jsExec->arity == 2) {
    ERL_NIF_TERM originTerm = jsExec->terms[0];
    ERL_NIF_TERM scriptTerm = jsExec->terms[1];

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
              JsWrapper::MakeTerm(env, trycatch));
        }

        free(buffer);
      } else {
        term = MakeError(env, "invalid_script");
      } 

    } else {
      term = MakeError(env, "invalid_origin");
    }
  } else {
    term = MakeError(env, "badarity");
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsExec->terms);
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

  ERL_NIF_TERM head;
  ERL_NIF_TERM term;
  int arity;
  const ERL_NIF_TERM *terms;
  ErlNifEnv *env = jsExec->env;

  if(jsExec->arity == 2) {
    ERL_NIF_TERM objectTerm = jsExec->terms[0];
    ERL_NIF_TERM fieldsTerm = jsExec->terms[1];

    Handle<Value> value = ErlWrapper::MakeHandle(this,
      env, objectTerm);

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
  } else {
    term = MakeError(env, "badarity");
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsExec->terms);
  free(jsExec);
}

void VmContext::ExecuteGet(JsExec *jsExec) {
  TRACE("VmContext::ExecuteGet\n");
  LHCST(this);

  ErlNifEnv *env = jsExec->env;
  ERL_NIF_TERM term;

  if(jsExec->arity == 2) {
    ERL_NIF_TERM objectTerm = jsExec->terms[0];
    ERL_NIF_TERM fieldTerm = jsExec->terms[1];

    Handle<Value> value = ErlWrapper::MakeHandle(this,
      env, objectTerm);

    if(value->IsObject()) {
      Handle<Object> obj = value->ToObject();
      Local<Value> fieldHandle = ErlWrapper::MakeHandle(this,
          env, fieldTerm);
      Local<Value> fieldValue = obj->Get(fieldHandle);

      term = JsWrapper::MakeTerm(this,
          env, fieldValue);
    } else {
      term = MakeError(env, "invalid_object");
    }
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsExec->terms);
  free(jsExec);
}

ERL_NIF_TERM VmContext::ExecuteCall(JsCallType type,
    ErlNifEnv *env,
    ERL_NIF_TERM recvTerm,
    ERL_NIF_TERM funTerm,
    ERL_NIF_TERM argsTerm) {
  Handle<Value> fun = ErlWrapper::MakeHandle(this,
          env, funTerm);
  ERL_NIF_TERM head;

  if(fun->IsFunction()) {
    unsigned length;

    if(enif_get_list_length(env, argsTerm, &length)) {
      Handle<Value> *args = (Handle<Value> *)malloc(length * sizeof(Handle<Value>));
      int i = 0;

      while(enif_get_list_cell(env, argsTerm, &head, &argsTerm)) {
        args[i] = ErlWrapper::MakeHandle(this,
            env, head);
        i++;
      }

      if(type == NORMAL) {
        Handle<Object> recv;
        Handle<Value> recvValue = ErlWrapper::MakeHandle(this,
            env, recvTerm);

        if(recvValue->IsObject()) {
          recv = recvValue->ToObject();
        } else {
          recv = context->Global();
        }

        Local<Value> result = fun->ToObject()->CallAsFunction(recv, length, args);
        term = JsWrapper::MakeTerm(this, env, result);
      } else {
        // Must be CONSTRUCTOR

        Local<Value> result = fun->ToObject()->CallAsConstructor(length, args);
        term = JsWrapper::MakeTerm(this, env, result);
      }

      free(args);
    } else {
      term = MakeError(env, "badargs");
    }
  } else {
    term = MakeError(env, "badfun");
  }

  return term;
}

void VmContext::ExecuteCall(JsExec *jsExec) {
  LHCST(this);

  unsigned length;
  int arity;
  const ERL_NIF_TERM *terms;
  ERL_NIF_TERM term;
  ErlNifEnv *env = jsExec->env;

  if(jsExec->arity == 2) {
    ERL_NIF_TERM typeTerm = jsExec->terms[0];
    ERL_NIF_TERM callTerm = jsExec->terms[1];

    if(enif_get_tuple(env, callTerm, &arity, &terms) &&
        enif_get_atom_length(env, typeTerm, &length, ERL_NIF_LATIN1)) {
      char *buffer = (char *)malloc((length + 1) * sizeof(char));
      enif_get_atom(env, typeTerm, buffer, length + 1, ERL_NIF_LATIN1);

      if(strncmp(buffer, "normal", length) == 0) {
        if(arity == 3) {
          term = ExecuteCall(NORMAL,
              env, terms[0], terms[1], terms[2]);
        } else {
          term = MakeError(env, "badcallarity");
        }
      } else if(strncmp(buffer, "constructor", length) == 0) {
        if(arity == 2) {
          term = ExecuteCall(CONSTRUCTOR,
              env, 0, terms[1], terms[2]);
        } else {
          term = MakeError(env, "badcallarity");
        }
      } else {
        term = MakeError(env, "badcalltype");
      }

      free(buffer);
    } else {
      term = MakeError(env, "badcall");
    }
  } else {
    term = MakeError(env, "badarity");
  }

  PostResult(jsExec->pid, env, term);

  enif_clear_env(env);
  enif_free_env(env);
  free(jsExec->terms);
  free(jsExec);
}

Handle<Value> VmContext::ExecuteCallRespond(JsExec *jsExec) {
  TRACE("VmContext::ExecuteCallRespond\n");
  // Don't need LHCST here because
  // To get here there must already be one on the stack

  Handle<Value> value;
  const ERL_NIF_TERM *terms;
  int arity;
  unsigned length;
  ErlNifEnv *env = jsExec->env;

  if(jsExec->arity == 1) {
    ERL_NIF_TERM responseTerm = jsExec->terms[0];

    if(enif_get_tuple(env, responseTerm, &arity, &terms)) {
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
  } else {
    value = ThrowException(Exception::Error(String::New("badarity")));
  }

  enif_clear_env(env);
  enif_free_env(env);
  free(jsExec->terms);
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

ERL_NIF_TERM VmContext::Send(ErlNifEnv *returnEnv,
    JsExecType type,
    ErlNifPid pid,
    int arity, const ERL_NIF_TERM *terms) {
  int nArity = arity - 1;
  ERL_NIF_TERM *nTerms;
  ErlNifEnv *env = enif_alloc_env();

  jsExec = (JsExec *)malloc(sizeof(JsExec));

  nTerms = (ERL_NIF_TERM *)malloc(nArity * sizeof(ERL_NIF_TERM));
  for(int i = 0; i < nArity; i++) {
    nTerms[i] = enif_make_copy(env, terms[i + 1]);
  }

  jsExec->pid = pid;
  jsExec->type = type;
  jsExec->env = env;
  jsExec->arity = nArity;
  jsExec->terms = nTerms;

  return enif_make_atom(returnEnv, "ok");
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
        if(strncmp(buffer, "run_script", length) == 0) {
          result = Send(env, RUN_SCRIPT, pid, arity, command);
        } else if(strncmp(buffer, "call", length) == 0) {
          result = Send(env, CALL, pid, arity, command);
        } else if(strncmp(buffer, "call_respond", length) == 0) {
          result = Send(env, CALL_RESPOND, pid, arity, command);
        } else if(strncmp(buffer, "set", length) == 0) {
          result = Send(env, SET, pid, arity, command);
        } else if(strncmp(buffer, "get", length) == 0) {
          result = Send(env, GET, pid, arity, command);
        } else if(strncmp(buffer, "heap_statistics", length) == 0) {
          result = Send(env, HEAP_STATISTICS, pid, arity, command);
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
