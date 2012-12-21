#include "erlang_v8_drv.h"

static int _id = 0;

static void *StartRunLoop(void *ptr) {
  Vm *vm = (Vm *)ptr;
  vm->RunLoop();

  return NULL;
}

Vm::Vm(ErlNifEnv *_env) {
  env = _env;
  
  sprintf(id, "Vm:%d", _id++);

  isolate = Isolate::New();

  erlVm = (ErlVm *) enif_alloc_resource(VmResource, sizeof(ErlVm));
  erlVm->vm = this;
  term = MakeTerm(env);
  enif_release_resource(erlVm);

  cond = enif_cond_create((char *)"VmCond1");
  mutex = enif_mutex_create((char *)"VmMutex1");
  cond2 = enif_cond_create((char *)"VmCond2");
  mutex2 = enif_mutex_create((char *)"VmMutex2");

  Run();
}

Vm::~Vm() {
  Stop();
  enif_cond_destroy(cond);
  enif_mutex_destroy(mutex);
}

ERL_NIF_TERM Vm::MakeTerm(ErlNifEnv *env) {
  return enif_make_resource_binary(env, erlVm, id, strlen(id));
}

void Vm::SetServer(ErlNifPid pid) {
  server = pid; 
}

VmContext *Vm::CurrentContext() {
  return contextStack.top();
}

VmContext *Vm::CreateVmContext(ErlNifEnv *env) {
  return new VmContext(this, env);
}

void Vm::Run() {
  enif_thread_create(
      (char *)"Vm::Run",
      &tid,
      StartRunLoop,
      (void *)this,
      NULL);
}

void Vm::RunLoop() {
  enif_mutex_lock(mutex);
  Poll();
}

Handle<Value> Vm::Poll() {
  TRACE("Vm::Poll\n");

  while(jsExec == NULL) {
    TRACE("Vm::Poll - 3\n");
    enif_cond_wait(cond, mutex);
  }
  TRACE("Vm::Poll - 4\n");
  JsExec *jsExec2 = ResetJsExec();
  Handle<Value> v;
  VmContext *vmContext = jsExec2->vmContext;
  enif_mutex_lock(mutex2);
  enif_cond_broadcast(cond2);
  enif_mutex_unlock(mutex2);

  if(vmContext) {
    contextStack.push(vmContext);
  }

  TRACE("Vm::Poll - 5\n");
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
      v = ExecuteCallRespond(jsExec2);
      if(vmContext) {
        contextStack.pop();
      }
      return v;
    default:
      TRACE("Vm::Poll - Default\n");
  }

  if(vmContext) {
    contextStack.pop();
  }

  TRACE("Vm::Poll - 6\n");
  return Poll();
}

JsExec *Vm::ResetJsExec() {
  JsExec *jsExec2 = NULL;
  TRACE("Vm::ResetJsExec\n");
  if(jsExec) {
    TRACE("Vm::ResetJsExec - 1\n");
    jsExec2 = jsExec;
    jsExec = NULL;
  }

  return jsExec2;
}

void Vm::Stop() {
  TRACE("Vm::Stop\n");
  jsExec = (JsExec *)malloc(sizeof(JsExec));
  jsExec->type = EXIT;
  jsExec->vmContext = NULL;
  enif_cond_broadcast(cond);

  void *result;
  enif_thread_join(tid, &result);
}

void Vm::PostResult(ErlNifPid pid,
    ErlNifEnv *env,
    ERL_NIF_TERM term) {
  TRACE("Vm::PostResult(term)\n");
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

void Vm::ExecuteRunScript(JsExec *jsExec) {
  TRACE("Vm::ExecuteRunScript\n");
  LHCST(this, jsExec->vmContext);

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

      if(enif_inspect_binary(env, scriptTerm, &binary)) {
        buffer = MakeBuffer(binary);
        Handle<String> source = String::New(buffer);
        free(buffer);
        Handle<Script> script = Script::Compile(source, &origin);
        Local<Value> result;

        if(!script.IsEmpty() &&
            !(result = script->Run()).IsEmpty()) {
          term = JsWrapper::MakeTerm(this,
              env, result);
        } else {
          term = MakeError(env,
              JsWrapper::MakeTerm(env, trycatch));
        }

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

ERL_NIF_TERM Vm::MakeError(ErlNifEnv *env, const char *reason) {
  return MakeError(env, enif_make_atom(env, reason));
}

ERL_NIF_TERM Vm::MakeError(ErlNifEnv *env, ERL_NIF_TERM reason) {
  return enif_make_tuple2(env,
      enif_make_atom(env, "error"),
      reason);
}

void Vm::Exit(JsExec *jsExec) {
  TRACE("Vm::Exit\n");

  // Dead lock for some reason
  //Locker locker(isolate);
  //TRACE("Vm::Exit - 1\n");
  //isolate->Dispose();
  //TRACE("Vm::Exit - 2\n");

  free(jsExec);
  enif_mutex_unlock(mutex);
  enif_thread_exit(NULL);
}

void Vm::ExecuteSet(JsExec *jsExec) {
  LHCST(this, jsExec->vmContext);

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

void Vm::ExecuteGet(JsExec *jsExec) {
  TRACE("Vm::ExecuteGet\n");
  LHCST(this, jsExec->vmContext);

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

ERL_NIF_TERM Vm::ExecuteCall(JsCallType type,
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

void Vm::ExecuteCall(JsExec *jsExec) {
  LHCST(this, jsExec->vmContext);

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

Handle<Value> Vm::ExecuteCallRespond(JsExec *jsExec) {
  TRACE("Vm::ExecuteCallRespond\n");
  LHCST(this, jsExec->vmContext);

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

void Vm::ExecuteHeapStatistics(JsExec *jsExec) {
  LHCST(this, jsExec->vmContext);

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

ERL_NIF_TERM Vm::Send(VmContext *vmContext,
    ErlNifEnv *returnEnv,
    JsExecType type,
    ErlNifPid pid,
    int arity, const ERL_NIF_TERM *terms) {
  TRACE("Vm::Send(arg)\n");
  int nArity = arity - 1;
  ERL_NIF_TERM *nTerms;
  ErlNifEnv *env = enif_alloc_env();

  jsExec = (JsExec *)malloc(sizeof(JsExec));

  nTerms = (ERL_NIF_TERM *)malloc(nArity * sizeof(ERL_NIF_TERM));
  for(int i = 0; i < nArity; i++) {
    nTerms[i] = enif_make_copy(env, terms[i + 1]);
  }

  jsExec->vmContext = vmContext;
  jsExec->pid = pid;
  jsExec->type = type;
  jsExec->env = env;
  jsExec->arity = nArity;
  jsExec->terms = nTerms;

  TRACE("Vm::Send(arg) - 1\n");
  return enif_make_atom(returnEnv, "ok");
}

ERL_NIF_TERM Vm::Send(VmContext *vmContext,
    ErlNifEnv *env, ErlNifPid pid, ERL_NIF_TERM term) {
  TRACE("Vm::Send\n");
  const ERL_NIF_TERM *command;
  int arity;
  ERL_NIF_TERM result;

  if(enif_get_tuple(env, term, &arity, &command)) {
    unsigned length;
    if(enif_get_atom_length(env, command[0], &length, ERL_NIF_LATIN1)) {
      char *buffer = (char *)malloc((length + 1) * sizeof(char));
      if(enif_get_atom(env, command[0], buffer, length + 1, ERL_NIF_LATIN1)) {
        if(strncmp(buffer, "run_script", length) == 0) {
          TRACE("Vm::Send - 1\n");
          result = Send(vmContext, env, RUN_SCRIPT, pid, arity, command);
        } else if(strncmp(buffer, "call", length) == 0) {
          result = Send(vmContext, env, CALL, pid, arity, command);
        } else if(strncmp(buffer, "call_respond", length) == 0) {
          TRACE("CALL - 1\n");
          result = Send(vmContext, env, CALL_RESPOND, pid, arity, command);
        } else if(strncmp(buffer, "set", length) == 0) {
          result = Send(vmContext, env, SET, pid, arity, command);
        } else if(strncmp(buffer, "get", length) == 0) {
          result = Send(vmContext, env, GET, pid, arity, command);
        } else if(strncmp(buffer, "heap_statistics", length) == 0) {
          result = Send(vmContext, env, HEAP_STATISTICS, pid, arity, command);
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
  TRACE("Vm::Send - 1\n");
  while(jsExec != NULL) {
  TRACE("Vm::Send - 2\n");
    enif_cond_wait(cond2, mutex2);
  }
  TRACE("Vm::Send - 3\n");
  enif_mutex_unlock(mutex2);

  return result;
}
