#include "erlang_v8_drv.h"

static void ErlWrapperDestroy(Persistent<Value> value, void *ptr) {
  TRACE("ErlWrapperDestroy\n");
  Handle<External> external = Handle<External>::Cast(value);
  ErlExternal *erlExternal = (ErlExternal *)external->Value();
  ErlWrapper *erlWrapper = (ErlWrapper *)erlExternal->ptr;
  value.Dispose();

  free(erlExternal);
  delete erlWrapper;
}

static Handle<Value> WrapFun(const Arguments &args) {
  TRACE("WrapFun\n");
  Handle<External> external = Local<External>::Cast(args.Data());
  ErlExternal *erlExternal = (ErlExternal *)external->Value();
  ErlWrapper *erlWrapper = (ErlWrapper *)erlExternal->ptr;
  ErlNifEnv *env = enif_alloc_env();
  unsigned length = args.Length();
  ERL_NIF_TERM *terms = (ERL_NIF_TERM *)malloc(length * sizeof(ERL_NIF_TERM));

  for(int i = 0; i < length; i++) {
    TRACE("WrapFun - 1\n");
    terms[i] = JsWrapper::MakeTerm(erlWrapper->vm, env, args[i]);
  }

  // TODO this for constructor
  ERL_NIF_TERM term = enif_make_tuple5(env,
      enif_make_atom(env, "call"),
      erlWrapper->vm->CurrentContext()->MakeTerm(env),
      JsWrapper::MakeWrapper(erlWrapper->vm, env, args.This()),
      enif_make_copy(env, erlWrapper->term),
      enif_make_list_from_array(env, terms, length)
      );
  enif_send(NULL, &(erlWrapper->vm->server), env, term);
  free(terms);
  enif_clear_env(env);
  enif_free_env(env);

  return erlWrapper->vm->Poll();
}

ErlWrapper::ErlWrapper(Vm *_vm, ERL_NIF_TERM _term) {
  vm = _vm;
  env = enif_alloc_env();
  term = enif_make_copy(env, _term);
}

ErlWrapper::~ErlWrapper() {
  enif_clear_env(env);
  enif_free_env(env);
}

Persistent<External> ErlWrapper::MakeExternal() {
  ErlExternal *erlExternal = (ErlExternal *)malloc(sizeof(ErlExternal));
  erlExternal->type = WRAPPER;
  erlExternal->ptr = this;
  Persistent<External> external = Persistent<External>::New(External::New(erlExternal));
  external.MakeWeak(NULL, ErlWrapperDestroy);

  return external;
}

Local<Value> ErlWrapper::MakeArray(Vm *vm,
    ErlNifEnv *env,
    unsigned length,
    ERL_NIF_TERM arrTerm) {
  ERL_NIF_TERM head;
  Local<Array> arr = Array::New(length);

  int i = 0;
  while(enif_get_list_cell(env, arrTerm, &head, &arrTerm)) {
    arr->Set(Integer::New(i), MakeHandle(vm, env, head));

    i++;
  }

  return arr;
}

Local<Value> ErlWrapper::MakeHandle(Vm *vm,
    ErlNifEnv *env,
    ERL_NIF_TERM term) {
  int _int;
  unsigned int _uint;
  long _long;
  unsigned long _ulong;
  ErlNifSInt64 _int64;
  ErlNifUInt64 _uint64;
  double _double;
  ErlNifBinary binary;
  ErlJsWrapper *erlJsWrapper;
  const ERL_NIF_TERM *terms;
  ErlVm *erlVm;
  ErlVmContext *erlVmContext;

  Local<Value> value;
  if(enif_get_atom_length(env, term, &_uint, ERL_NIF_LATIN1)) {
    char *buffer = (char *)malloc((_uint + 1) * sizeof(char));
    enif_get_atom(env, term, buffer, _uint + 1, ERL_NIF_LATIN1);

    if(strncmp(buffer, "undefined", _uint) == 0) {
      value = Local<Value>::New(Undefined());
    } else if(strncmp(buffer, "null", _uint) == 0) {
      value = Local<Value>::New(Null());
    } else if(strncmp(buffer, "true", _uint) == 0) {
      value = Local<Value>::New(True());
    } else if(strncmp(buffer, "false", _uint) == 0) {
      value = Local<Value>::New(False());
    } else if(strncmp(buffer, "global", _uint) == 0) {
      value = Context::GetCurrent()->Global();
    } else {
      value = Local<Value>::New(Undefined());
    }

    free(buffer);
  } else if(enif_get_double(env, term, &_double)) {
    value = Number::New(_double);
  } else if(enif_get_int(env, term, &_int)) {
    TRACE("ErlWrapper::MakeHandle - INT\n");
    value = Integer::New(_int);
  } else if(enif_get_int64(env, term, &_int64)) {
    value = Integer::New(_int64);
  } else if(enif_get_long(env, term, &_long)) {
    value = Integer::New(_long);
  } else if(enif_get_uint(env, term, &_uint)) {
    value = Integer::NewFromUnsigned(_uint);
  } else if(enif_get_uint64(env, term, &_uint64)) {
    value = Integer::NewFromUnsigned(_uint64);
  } else if(enif_get_ulong(env, term, &_ulong)) {
    value = Integer::NewFromUnsigned(_ulong);
  } else if(enif_get_resource(env, term, VmResource, (void **)(&erlVm))) {
    value = Local<Value>::New(erlVm->vm->MakeHandle());
  } else if(enif_get_resource(env, term, VmContextResource, (void **)(&erlVmContext))) {
    value = Local<Value>::New(erlVmContext->vmContext->MakeHandle());
  } else if(enif_get_resource(env, term, JsWrapperResource, (void **)(&erlJsWrapper))) {
    TRACE("ErlWrapper::MakeHandle - RESOURCE\n");
    value = Local<Value>::New(erlJsWrapper->jsWrapper->value);
  } else if(enif_inspect_binary(env, term, &binary)) {
    char *buffer = (char *)malloc((binary.size + 1) * sizeof(char));
    memcpy(buffer, binary.data, binary.size);
    buffer[binary.size] = NULL;
    value = String::New(buffer);
    free(buffer);
  } else if(enif_is_fun(env, term)) {
    TRACE("ErlWrapper::MakeHandle - FUN\n");
    ErlWrapper *erlWrapper = new ErlWrapper(vm, term);
    Handle<FunctionTemplate> fn = FunctionTemplate::New(WrapFun, erlWrapper->MakeExternal());
    value = fn->GetFunction();
  } else if(enif_get_list_length(env, term, &_uint)) {
    value = MakeArray(vm, env, _uint, term);
  } else if(enif_get_tuple(env, term, &_int, &terms)) {
    TRACE("ErlWrapper::MakeHandle - TUPLE\n");
    value = MakeTupleHandle(vm, env, _int, terms);
  } else {
    ErlWrapper *erlWrapper = new ErlWrapper(vm, term);
    value = Local<External>::New(erlWrapper->MakeExternal());
  }

  return value;
}

Local<Value> ErlWrapper::MakeWrapper(Vm *vm,
    ERL_NIF_TERM term) {
  ErlWrapper *erlWrapper = new ErlWrapper(vm, term);

  return Local<Value>::New(erlWrapper->MakeExternal());
}

Local<Value> ErlWrapper::MakeObject(Vm *vm,
    ErlNifEnv *env,
    ERL_NIF_TERM term) {
  int arity;
  const ERL_NIF_TERM *terms;
  ERL_NIF_TERM head;
  Local<Object> object = Object::New();

  while(enif_get_list_cell(env, term, &head, &term)) {
    if(enif_get_tuple(env, head, &arity, &terms) &&
        arity == 2) {
      Handle<Value> fieldValue = MakeHandle(vm, env, terms[0]);
      Handle<Value> value = MakeHandle(vm, env, terms[1]);

      object->Set(fieldValue, value);
    }
  }

  return object;
}

Local<Value> ErlWrapper::MakeDate(Vm *vm,
    ErlNifEnv *env,
    ERL_NIF_TERM term) {
  unsigned long time;
  Handle<Value> value;

  if(enif_get_ulong(env, term, &time)) {
    value = Date::New(time * 1000);
  } else {
    value = ThrowException(
      Exception::Error(String::New("bad time")));
  }

  return Local<Value>::New(value);
}

Local<Value> ErlWrapper::MakeTupleHandle(Vm *vm,
    ErlNifEnv *env, int arity,
    const ERL_NIF_TERM *terms) {
  Handle<Value> value;

  if(arity == 1) {
    value = MakeWrapper(vm, terms[0]);
  } else if(arity == 2) {
    unsigned length;

    if(enif_get_atom_length(env, terms[0], &length, ERL_NIF_LATIN1)) {
      char *buffer = (char *)malloc((length + 1) * sizeof(char));

      if(enif_get_atom(env, terms[0], buffer, length + 1, ERL_NIF_LATIN1)) {
        if(strncmp(buffer, "struct", length) == 0) {
          value = MakeObject(vm, env, terms[1]);
        } else if(strncmp(buffer, "mf", length) == 0) {
          ErlWrapper *erlWrapper = new ErlWrapper(vm, terms[1]);
          Handle<FunctionTemplate> fn = FunctionTemplate::New(WrapFun, erlWrapper->MakeExternal());
          value = fn->GetFunction();
        } else if(strncmp(buffer, "date", length) == 0) {
          value = MakeDate(vm, env, terms[1]);
        } else if(strncmp(buffer, "eval", length) == 0) {
          value = MakeEval(vm, env, arity, terms);
        } else if(strncmp(buffer, "set", length) == 0) {
          value = MakeSet(vm, env, arity, terms);
        } else if(strncmp(buffer, "get", length) == 0) {
          value = MakeGet(vm, env, arity, terms);
        } else if(strncmp(buffer, "call", length) == 0) {
          value = MakeCall(vm, env, arity, terms);
        } else {
          value = ThrowException(
            Exception::Error(String::New("bad tuple: unrecognized atom")));
        }
      } else {
        value = ThrowException(
          Exception::Error(String::New("bad tuple: first argument not an atom")));
      }

      free(buffer);
    } else {
      value = ThrowException(
          Exception::Error(String::New("bad tuple")));
    }
  }

  return Local<Value>::New(value);
}

Local<Value> ErlWrapper::MakeEval(Vm *vm,
    ErlNifEnv *env,
    int arity,
    const ERL_NIF_TERM *terms) {
  return Local<Value>::New(Undefined());
}

Local<Value> ErlWrapper::MakeSet(Vm *vm,
    ErlNifEnv *env,
    int arity,
    const ERL_NIF_TERM *terms) {
  return Local<Value>::New(Undefined());
}

Local<Value> ErlWrapper::MakeGet(Vm *vm,
    ErlNifEnv *env,
    int arity,
    const ERL_NIF_TERM *terms) {
  return Local<Value>::New(Undefined());
}

Local<Value> ErlWrapper::MakeCall(Vm *vm,
    ErlNifEnv *env,
    int arity,
    const ERL_NIF_TERM *terms) {
  return Local<Value>::New(Undefined());
}
