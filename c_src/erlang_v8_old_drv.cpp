#include "v8.h"
#include "erlang/erl_nif.h"
#include <string>

using namespace std;
using namespace v8;

typedef struct {
  Isolate *isolate;
  Persistent<Context> bootstrapContext;

  // Prototypes
  Persistent<ObjectTemplate> AtomPrototype;
} ErlIsolate;

typedef struct {
  ErlIsolate *erlIsolate;
  Persistent<Context> context;
} ErlContext;

typedef struct {
  ErlContext *erlContext;
  Persistent<Value> value;
} ErlPersistentValue;

static ErlNifResourceType *IsolateResource;
static ErlNifResourceType *ContextResource;
static ErlNifResourceType *PersistentValueResource;

static ERL_NIF_TERM Error(ErlNifEnv *env, string reason) {
  return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, reason.c_str()));
}

static void IsolateResourceDestroy(ErlNifEnv* env, void* obj) {
  printf("Destroy isolate\n");
  ErlIsolate *erlIsolate = (ErlIsolate *)obj;
  Isolate *isolate = erlIsolate->isolate;

  Locker locker(isolate);
  Isolate::Scope iscope(isolate);
  erlIsolate->bootstrapContext.Dispose();
  erlIsolate->bootstrapContext.Clear();

  // ===============================================
  // Commented out because of seg faults when trying
  // to dispose the isolate
  // ===============================================

  //V8::TerminateExecution(isolate);
  //Locker locker(isolate);
  //isolate->Dispose();
}

static void ContextResourceDestroy(ErlNifEnv* env, void* obj) {
  printf("Destroy context\n");
  ErlContext *context = (ErlContext *)obj;
  ErlIsolate *erlIsolate = context->erlIsolate;

  Locker locker(erlIsolate->isolate);
  Isolate::Scope iscope(erlIsolate->isolate);
  context->context.Dispose();
  context->context.Clear();

  enif_release_resource(erlIsolate);
}

static void PersistenValueResourceDestroy(ErlNifEnv* env, void* obj) {
  printf("Destroy resource\n");
  ErlPersistentValue *value = (ErlPersistentValue *)obj;
  ErlContext *erlContext = value->erlContext;
  ErlIsolate *erlIsolate = erlContext->erlIsolate;

  Locker locker(erlIsolate->isolate);
  Isolate::Scope iscope(erlIsolate->isolate);
  HandleScope handle_scope;
  Context::Scope context_scope(erlContext->context);

  value->value.Dispose();
  enif_release_resource(erlContext);
}

class Term {
  public:
    ErlNifEnv *env;
    ERL_NIF_TERM term;

    Term(ERL_NIF_TERM term);
    ~Term();
};

Term::Term(ERL_NIF_TERM _term) {
  env = enif_alloc_env();
  term = enif_make_copy(env, _term);
}

Term::~Term() {
  enif_clear_env(env);
  enif_free_env(env);
}

static void DeleteTerm(Persistent<Value> value, void *data) {
  printf("Deleting term\n");
  Term *term = (Term *)data;
  delete term;
}

static Handle<External> TermToExternal(ERL_NIF_TERM term) {
  Term *erlTerm = new Term(term);
  Persistent<External> external = Persistent<External>::New(External::New(erlTerm));
  external.MakeWeak(NULL, DeleteTerm);

  return external;
}

static Handle<Value> AtomToString(const Arguments &args) {
  Local<Object> atomJs = args.This();
  Local<External> external = Local<External>::Cast(atomJs->GetInternalField(0));
  Term *term = (Term *)external->Value();
  unsigned length;

  if(enif_get_atom_length(term->env, term->term, &length, ERL_NIF_LATIN1)) {
    char *buffer = (char *)malloc(length * sizeof(char) + 1);

    enif_get_atom(term->env, term->term, buffer, length + 1, ERL_NIF_LATIN1);
    printf("atom_to_string(%d): %s\n", length, buffer);

    Local<String> str = Local<String>::New(String::New(buffer));
    free(buffer);

    return str;
  } else {
    return String::New("not an atom");
  }
}

static Handle<Value> FunCall(const Arguments &args) {
  ERL_NIF_TERM fun;

  Local<External> external = Local<External>::Cast(args.Data());
  Term *term = (Term *)external->Value();

  return term->term;
}

static ERL_NIF_TERM NewIsolate(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  Isolate *isolate = Isolate::New();
  ErlIsolate *erlIsolate = (ErlIsolate *)enif_alloc_resource(IsolateResource, sizeof(ErlIsolate));
  erlIsolate->isolate = isolate;
  ERL_NIF_TERM term = enif_make_resource(env, erlIsolate);
  enif_release_resource(erlIsolate);

  Locker locker(isolate);
  Isolate::Scope iscope(isolate);
  HandleScope handle_scope;
  erlIsolate->bootstrapContext = Persistent<Context>::New(Context::New());
  Context::Scope context_scope(erlIsolate->bootstrapContext);

  // Create prototypes
  Persistent<ObjectTemplate> AtomPrototype = Persistent<ObjectTemplate>::New(ObjectTemplate::New());
  AtomPrototype->SetInternalFieldCount(1);
  AtomPrototype->Set(String::New("toString"), FunctionTemplate::New(&AtomToString));
  erlIsolate->AtomPrototype = AtomPrototype;

  return term;
};

static Persistent<Context> CreateContext(Isolate *isolate) {
    Locker locker(isolate);
    Isolate::Scope iscope(isolate);
    HandleScope handle_scope;

    return Persistent<Context>::New(Context::New());
};

static ERL_NIF_TERM NewContext(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlIsolate *erlIsolate;

  if(enif_get_resource(env, argv[0], IsolateResource, (void **)(&erlIsolate))) {
    ErlContext *erlContext = (ErlContext *)enif_alloc_resource(ContextResource, sizeof(ErlContext));

    Persistent<Context> context = CreateContext(erlIsolate->isolate);

    erlContext->erlIsolate = erlIsolate;
    erlContext->context = context;

    enif_keep_resource(erlIsolate);

    ERL_NIF_TERM term = enif_make_resource(env, erlContext);
    enif_release_resource(erlContext);

    return term;
  } else {
    return Error(env, "invalid_isolate");
  }
};

static char *BinaryToString(ErlNifBinary binary) {
  char *str = (char *)malloc(binary.size * sizeof(char) + 1);
  memcpy(str, binary.data, binary.size);
  str[binary.size] = NULL;

  return str;
}

static ERL_NIF_TERM WrapJs(ErlNifEnv *env,
    string type,
    ErlPersistentValue *erlValue) {
  ERL_NIF_TERM typeTerm = enif_make_atom(env, type.c_str());
  ERL_NIF_TERM contextTerm = enif_make_resource(env, erlValue->erlContext);
  ERL_NIF_TERM valueTerm = enif_make_resource(env, erlValue);

  return enif_make_tuple3(env, typeTerm, contextTerm, valueTerm);
}

static ERL_NIF_TERM WrapJs(ErlNifEnv *env,
    string type,
    ErlPersistentValue *erlValue,
    ERL_NIF_TERM primitiveTerm) {
  ERL_NIF_TERM typeTerm = enif_make_atom(env, type.c_str());
  ERL_NIF_TERM contextTerm = enif_make_resource(env, erlValue->erlContext);
  ERL_NIF_TERM valueTerm = enif_make_resource(env, erlValue);

  return enif_make_tuple4(env, typeTerm, contextTerm, valueTerm, primitiveTerm);
}

static ERL_NIF_TERM JsObjectToTerm(ErlNifEnv *env,
    ErlPersistentValue *erlValue) {
  Persistent<Value> value = erlValue->value;

  if(value->IsArray()) {
    return WrapJs(env, "js_array", erlValue);
  } else if(value->IsBooleanObject()) {
    return WrapJs(env, "js_boolean_object", erlValue);
  } else if(value->IsDate()) {
    return WrapJs(env, "js_date", erlValue);
  } else if(value->IsFunction()) {
    return WrapJs(env, "js_function", erlValue);
  } else if(value->IsNumberObject()) {
    return WrapJs(env, "js_number_object", erlValue);
  } else if(value->IsRegExp()) {
    return WrapJs(env, "js_reg_exp", erlValue);
  } else if(value->IsStringObject()) {
    return WrapJs(env, "js_string_object", erlValue);
  } else if(value->IsUndefined()) {
    return WrapJs(env, "js_undefined", erlValue);
  } else if(value->IsNull()) {
    return WrapJs(env, "js_null", erlValue);
  } else if(value->IsNumber()) {
    ERL_NIF_TERM primitiveTerm;
    if(value->IsInt32()) {
      primitiveTerm = enif_make_int(env, value->ToInt32()->Value());
    } else if(value->IsUint32()) {
      primitiveTerm = enif_make_uint(env, value->ToUint32()->Value());
    } else {
      primitiveTerm = enif_make_double(env, value->ToNumber()->Value());
    }
    return WrapJs(env, "js_number", erlValue, primitiveTerm);
  } else if(value->IsBoolean()) {
    ERL_NIF_TERM primitiveTerm;
    if(value->IsTrue()) {
      primitiveTerm = enif_make_atom(env, (char *)"true");
    } else {
      primitiveTerm = enif_make_atom(env, (char *)"false");
    }
    return WrapJs(env, "js_boolean", erlValue, primitiveTerm);
  } else if(value->IsString()) {
    Local<String> str = value->ToString();
    ERL_NIF_TERM primitiveTerm;
    char *binary = (char *)enif_make_new_binary(env, str->Length(), &primitiveTerm);
    str->WriteAscii(binary, 0, -1, String::NO_NULL_TERMINATION);
    return WrapJs(env, "js_string", erlValue, primitiveTerm);
  } else if(value->IsExternal()) {
    return WrapJs(env, "js_external", erlValue);
  } else if(value->IsObject()) {
    return WrapJs(env, "js_object", erlValue);
  } else {
    return WrapJs(env, "js_unknown", erlValue);
  }
}

static ERL_NIF_TERM ExecuteJs(ErlNifEnv *env,
    ErlIsolate *erlIsolate,
    ErlContext *erlContext,
    char *script) {
  Locker locker(erlIsolate->isolate);
  Isolate::Scope iscope(erlIsolate->isolate);
  HandleScope handle_scope;
  Context::Scope context_scope(erlContext->context);

  Handle<String> source = String::New(script);
  Handle<Script> scriptHandle = Script::Compile(source);
  Handle<Value> result = scriptHandle->Run();

  ErlPersistentValue *erlValue = (ErlPersistentValue *)enif_alloc_resource(PersistentValueResource, sizeof(ErlPersistentValue));
  erlValue->erlContext = erlContext;
  enif_keep_resource(erlContext);
  erlValue->value = Persistent<Value>::New(result);

  ERL_NIF_TERM term = JsObjectToTerm(env, erlValue);
  enif_release_resource(erlValue);

  return term;
}

static ERL_NIF_TERM Execute(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlContext *erlContext;

  if(enif_get_resource(env, argv[0], ContextResource, (void **)(&erlContext))) {
    ErlIsolate *erlIsolate = erlContext->erlIsolate;
    ErlNifBinary binary;

    if(enif_inspect_binary(env, argv[1], &binary)) {
      char *source = BinaryToString(binary);
      ERL_NIF_TERM term = ExecuteJs(env, erlIsolate, erlContext, source);
      free(source);

      return term;
    } else {
      return Error(env, "invalid_binary");
    }
  } else {
    return Error(env, "invalid_context");
  }
};

static Local<Value> WrapErl(ErlContext *erlContext, ErlNifEnv *env, ERL_NIF_TERM term) {
  Local<Value> wrapped;
  int wrapTerm = 0;
  int _int;
  unsigned int _uint;
  long _long;
  unsigned long _ulong;
  ErlNifSInt64 _int64;
  ErlNifUInt64 _uint64;
  double _double;

  if(enif_is_atom(env, term)) {
    wrapped = erlContext->erlIsolate->AtomPrototype->NewInstance();
    wrapTerm = 1;
  } else if(enif_get_int(env, term, &_int)) {
    wrapped = Local<Integer>::New(Integer::New(_int));
  } else if(enif_get_uint(env, term, &_uint)) {
    wrapped = Local<Integer>::New(Integer::NewFromUnsigned(_uint));
  } else if(enif_get_long(env, term, &_long)) {
    wrapped = Local<Integer>::New(Integer::New(_long));
  } else if(enif_get_ulong(env, term, &_ulong)) {
    wrapped = Local<Integer>::New(Integer::NewFromUnsigned(_ulong));
  } else if(enif_get_int64(env, term, &_int64)) {
    wrapped = Local<Integer>::New(Integer::New(_int64));
  } else if(enif_get_uint64(env, term, &_uint64)) {
    wrapped = Local<Integer>::New(Integer::NewFromUnsigned(_uint64));
  } else if(enif_get_double(env, term, &_double)) {
    wrapped = Local<Number>::New(Number::New(_double));
  } else if(enif_is_fun(env, term)) {
    Handle<External> external = TermToExternal(term);
    Persistent<FunctionTemplate> functionTemplate = Persistent<FunctionTemplate>::New(FunctionTemplate::New(FunCall, external));
    wrapped = functionTemplate->GetFunction();
  }

  if(wrapTerm) {
    Handle<External> external = TermToExternal(term);
    Local<Object>::Cast(wrapped)->SetInternalField(0, external);

    v8::V8::AdjustAmountOfExternalAllocatedMemory((long)sizeof(Term));
  }

  return wrapped;
}

static ERL_NIF_TERM DoExecuteFunction(ErlNifEnv *env,
    ErlPersistentValue *erlThis,
    ErlPersistentValue *erlValue,
    unsigned jsArgc,
    ERL_NIF_TERM argList) {
  ErlContext *erlContext = erlValue->erlContext;
  ErlIsolate *erlIsolate = erlContext->erlIsolate;

  Locker locker(erlIsolate->isolate);
  Isolate::Scope iscope(erlIsolate->isolate);
  Context::Scope context_scope(erlContext->context);
  HandleScope handle_scope;

  Handle<Value> *argv = (Handle<Value> *)malloc(jsArgc * sizeof(Handle<Value>));

  ERL_NIF_TERM head, tail, newTail;
  tail = argList;
  for(int i = 0; i < jsArgc; i++) {
    enif_get_list_cell(env, tail, &head, &newTail);
    tail = newTail;

    Handle<Value> arg = WrapErl(erlContext, env, head);
    argv[i] = arg;
  }

  Persistent<Function> function = Persistent<Function>::Cast(erlValue->value);
  Persistent<Value> result = Persistent<Value>::New(function->Call(erlThis->value->ToObject(), jsArgc, argv));

  ErlPersistentValue *retValue = (ErlPersistentValue *)enif_alloc_resource(PersistentValueResource, sizeof(ErlPersistentValue));
  retValue->erlContext = erlContext;
  enif_keep_resource(erlContext);
  retValue->value = result;

  ERL_NIF_TERM term = JsObjectToTerm(env, retValue);
  enif_release_resource(retValue);

  return term;
}

static ERL_NIF_TERM ExecuteFunction(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlPersistentValue *erlThis;

  if(enif_get_resource(env, argv[0], PersistentValueResource, (void **)&erlThis)) {
    ErlPersistentValue *erlValue;

    if(enif_get_resource(env, argv[1], PersistentValueResource, (void **)&erlValue)) {
      ERL_NIF_TERM argList = argv[2];
      unsigned jsArgc;
      if(enif_get_list_length(env, argList, &jsArgc)) {
        return DoExecuteFunction(env, erlThis, erlValue, jsArgc, argList);
      } else {
        return Error(env, "args_not_list");
      }
    } else {
      return Error(env, "invalid_resource");
    }
  } else {
    return Error(env, "invalid_this");
  }
}

static ERL_NIF_TERM DoSetField(ErlNifEnv *env,
    ErlPersistentValue *erlObject,
    char *field,
    ERL_NIF_TERM term) {
  ErlContext *erlContext = erlObject->erlContext;
  Isolate *isolate = erlContext->erlIsolate->isolate;

  Locker locker(isolate);
  Isolate::Scope iscope(isolate);
  Context::Scope context_scope(erlContext->context);
  HandleScope handle_scope;

  Local<Value> value = WrapErl(erlContext, env, term);
  Persistent<Object>::Cast(erlObject->value)->Set(String::New(field), value);

  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM SetField(ErlNifEnv *env,
    int argc,
    const ERL_NIF_TERM argv[]) {
  ErlPersistentValue *erlObject;

  if(enif_get_resource(env, argv[0], PersistentValueResource, (void **)&erlObject)) {
    ErlNifBinary binary;
    if(enif_inspect_binary(env, argv[1], &binary)) {
      char *field = BinaryToString(binary);
      ERL_NIF_TERM term = DoSetField(env, erlObject, field, argv[2]);
      free(field);

      return term;
    } else {
      return Error(env, "not_binary");
    }
  } else {
    return Error(env, "invalid_object");
  }
}

static int Load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info) {
  IsolateResource = enif_open_resource_type(env, NULL, "erlang_v8_IsolateResource", IsolateResourceDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  ContextResource = enif_open_resource_type(env, NULL, "erlang_v8_ContextResource", ContextResourceDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  PersistentValueResource = enif_open_resource_type(env, NULL, "erlang_v8_PersistentValueResource", PersistenValueResourceDestroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);

  V8::Initialize();

  return 0;
};

static ErlNifFunc nif_funcs[] = {
  {"new_isolate", 0, NewIsolate},
  {"new_context", 1, NewContext},
  {"execute", 2, Execute},
  {"execute_function", 3, ExecuteFunction},
  {"set_field", 3, SetField}
};

ERL_NIF_INIT(v8nif, nif_funcs, Load, NULL, NULL, NULL)
