
#include "schpriv.h"

#ifdef MZ_USE_PLACES

#include "mzrt.h"

Scheme_Object *scheme_place(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_sleep(int argc, Scheme_Object *args[]);
static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[]);
static void load_namespace(char *namespace_name);
static void load_namespace_utf8(Scheme_Object *namespace_name);

# ifdef MZ_PRECISE_GC
static void register_traversers(void);
# endif

static void *place_start_proc(void *arg);

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, func, a1, a2, env)

#else

# define PLACE_PRIM_W_ARITY(name, func, a1, a2, env) GLOBAL_PRIM_W_ARITY(name, not_implemented, a1, a2, env)

static Scheme_Object *not_implemented(int argc, Scheme_Object **argv)
{
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED, "not supported");
  return NULL;
}

# ifdef MZ_PRECISE_GC
static void register_traversers(void) { }
# endif

#endif

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/
void scheme_init_place(Scheme_Env *env)
{
  Scheme_Env *plenv;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
  
  plenv = scheme_primitive_module(scheme_intern_symbol("#%place"), env);

  PLACE_PRIM_W_ARITY("place",       scheme_place,       1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-sleep", scheme_place_sleep, 1, 1, plenv);
  PLACE_PRIM_W_ARITY("place-wait",  scheme_place_wait,  1, 1, plenv);
  PLACE_PRIM_W_ARITY("place?",      scheme_place_p,     1, 1, plenv);

  scheme_finish_primitive_module(plenv);
}

#ifdef MZ_USE_PLACES

/************************************************************************/
/************************************************************************/
/************************************************************************/

/* FIXME this struct probably will need to be garbage collected as stuff
 * is added to it */
typedef struct Place_Start_Data {
  Scheme_Object *thunk;
  Scheme_Object *current_library_collection_paths;
} Place_Start_Data;

static void null_out_runtime_globals() {
  scheme_current_thread           = NULL;
  scheme_first_thread             = NULL;
  scheme_main_thread              = NULL;
                                                                   
  scheme_current_runstack_start   = NULL;
  scheme_current_runstack         = NULL;
  scheme_current_cont_mark_stack  = 0;
  scheme_current_cont_mark_pos    = 0;
}

Scheme_Object *scheme_place_sleep(int argc, Scheme_Object *args[]) {
  mzrt_sleep(SCHEME_INT_VAL(args[0]));
  return scheme_void;
}

Scheme_Object *scheme_place(int argc, Scheme_Object *args[]) {
  Scheme_Place          *place;
  Place_Start_Data      *place_data;
  mz_proc_thread        *proc_thread;
  Scheme_Object         *collection_paths;

  /* create place object */
  place = MALLOC_ONE_TAGGED(Scheme_Place);
  place->so.type = scheme_place_type;

  /* pass critical info to new place */
  place_data = MALLOC_ONE(Place_Start_Data);
  place_data->thunk  = args[0];
  collection_paths = scheme_current_library_collection_paths(0, NULL);
  place_data->current_library_collection_paths = collection_paths;

  /* create new place */
  proc_thread = mz_proc_thread_create(place_start_proc, place_data);
  place->proc_thread = proc_thread;

  return (Scheme_Object*) place;
}

static Scheme_Object *scheme_place_wait(int argc, Scheme_Object *args[]) {
  void                  *rc;
  Scheme_Place          *place;
  place = (Scheme_Place *) args[0];

  rc = mz_proc_thread_wait((mz_proc_thread *)place->proc_thread);
  
  return scheme_void;
}

static Scheme_Object *scheme_place_p(int argc, Scheme_Object *args[])
{
  return SAME_TYPE(SCHEME_TYPE(args[0]), scheme_place_type) ? scheme_true : scheme_false;
}

static void load_namespace(char *namespace_name) {
  load_namespace_utf8( scheme_make_utf8_string(namespace_name));
}

static void load_namespace_utf8(Scheme_Object *namespace_name) {
  Scheme_Object *nsreq;
  Scheme_Object *a[1];
  Scheme_Thread * volatile p;
  mz_jmp_buf * volatile saved_error_buf;
  mz_jmp_buf volatile new_error_buf;

  nsreq = scheme_builtin_value("namespace-require");
  a[0] = scheme_make_pair(scheme_intern_symbol("lib"),
      scheme_make_pair(namespace_name, scheme_make_null()));

  p = scheme_get_current_thread();
  saved_error_buf = p->error_buf;
  p->error_buf = &new_error_buf;
  if (!scheme_setjmp(new_error_buf))
    scheme_apply(nsreq, 1, a);
  p->error_buf = saved_error_buf;
}

static void *place_start_proc(void *data_arg) {
  void *stack_base;
  Scheme_Object *thunk;
  Place_Start_Data *place_data;
  Scheme_Object *a[1];
  int ptid;
  ptid = mz_proc_thread_self();


  stack_base = PROMPT_STACK(stack_base);
  place_data = (Place_Start_Data *) data_arg;
 
  printf("Startin place: proc thread id%u\n", ptid);

  /* create pristine THREAD_LOCAL variables*/
  null_out_runtime_globals();

  /* scheme_make_thread behaves differently if the above global vars are not null */
  scheme_place_instance_init(stack_base);
  a[0] = place_data->current_library_collection_paths;
  scheme_current_library_collection_paths(1, a);

  load_namespace("scheme/init");

  thunk = place_data->thunk;

  scheme_apply(thunk, 0, NULL);

  stack_base = NULL;

  return scheme_true;
}


/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_PLACES_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_place_type, place_val);
}

END_XFORM_SKIP;

#endif

/************************************************************************/
/************************************************************************/
/************************************************************************/

#endif
