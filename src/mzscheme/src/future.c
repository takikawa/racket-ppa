
#ifndef UNIT_TEST
# include "schpriv.h"
#endif

#ifdef FUTURES_ENABLED

#include "future.h"
#include <stdlib.h>
#include <string.h>
#ifdef UNIT_TEST
# include "./tests/unit_test.h"
#endif 

#define THREAD_POOL_SIZE 1
static pthread_t g_pool_threads[THREAD_POOL_SIZE];

future_t *g_future_queue = NULL;
int g_next_futureid = 0;
pthread_t g_rt_threadid = 0;

static pthread_mutex_t g_future_queue_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t g_future_pending_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t g_future_pending_cv = PTHREAD_COND_INITIALIZER;

//Stuff for scheme runstack 
//Some of these may mimic defines in thread.c, but are redefined here 
//to avoid making any changes to that file for now (moving anything out into common 
//headers, etc.)
#ifndef DEFAULT_INIT_STACK_SIZE 
#define DEFAULT_INIT_STACK_SIZE 1000 
#endif

//Functions
#ifndef UNIT_TEST
static Scheme_Object *future(int argc, Scheme_Object *argv[]);
static Scheme_Object *touch(int argc, Scheme_Object **argv);
static void *worker_thread_future_loop(void *arg);
static void *invoke_rtcall(future_t *future);
static future_t *enqueue_future(void);
static future_t *get_pending_future(void);
static future_t *get_my_future(void);
static future_t *get_future_by_threadid(pthread_t threadid);
static future_t *get_future(int futureid);
static future_t *get_last_future(void);
#else
//Garbage stubs for unit testing 
#define START_XFORM_SKIP 
#define END_XFORM_SKIP 
void scheme_add_global(char *name, int arity, Scheme_Env *env) { }
int scheme_make_prim_w_arity(prim_t func, char *name, int arg1, int arg2) { return 1; }
Scheme_Object *future_touch(int futureid)
{
    Scheme_Object *args[1] = { &futureid };
    return touch(1, args);
}
#endif

void *g_funcargs[5];
void *func_retval = NULL;


/**********************************************************************/
/* Helpers for debugging                    						  */
/**********************************************************************/
#ifdef DEBUG_FUTURES 
int g_rtcall_count = 0;

void debug_save_context(void)
{
	future_t *future;
	rtcall_context_t *context;
	future = get_my_future();
	context = (rtcall_context_t*)malloc(sizeof(rtcall_context_t));

	future->context = context;
	future->context->mz_runstack_start = MZ_RUNSTACK_START;
	future->context->mz_runstack = MZ_RUNSTACK;
}

void debug_assert_context(future_t *future)
{
	rtcall_context_t *context = future->context;
	if (MZ_RUNSTACK_START != future->context->mz_runstack_start)
	{
		printf("MZ_RUNSTACK_START was %p, but future runstack start should be %p.\n", 
			MZ_RUNSTACK_START, 
			context->mz_runstack_start);
	}

	if (MZ_RUNSTACK != context->mz_runstack)
	{
		printf("MZ_RUNSTACK was %p, but future runstack should be %p.\n", 
			MZ_RUNSTACK, 
			context->mz_runstack);
	}
}

void debug_kill_context(void)
{
	future_t *future;
	future = get_my_future();
	free(future->context);
	future->context = NULL;
}
#endif

static Scheme_Object **get_thread_runstack(void)
{
	return MZ_RUNSTACK;
}


static Scheme_Object **get_thread_runstack_start(void)
{
	return MZ_RUNSTACK_START;
}


/**********************************************************************/
/* Plumbing for MzScheme initialization                               */
/**********************************************************************/

//Invoked by the runtime on startup to make 
//primitives known
void scheme_init_futures(Scheme_Env *env)
{
	START_XFORM_SKIP;
	Scheme_Object *v;
	Scheme_Env *newenv;

	futures_init();

	v = scheme_intern_symbol("#%futures");
	newenv = scheme_primitive_module(v, env);

	scheme_add_global_constant(
		"future", 
		scheme_make_prim_w_arity(
			future, 
			"future", 
			1, 
			1), 
		newenv);

	scheme_add_global_constant(
		"touch", 
		scheme_make_prim_w_arity(
			touch, 
			"touch", 
			1, 
			1), 
		newenv);

	scheme_finish_primitive_module(newenv);
	scheme_protect_primitive_provide(newenv, NULL);
	END_XFORM_SKIP;
}


//Setup code here that should be invoked on
//the runtime thread.
void futures_init(void)
{
	int i;
	pthread_t threadid;
    g_rt_threadid = pthread_self();

    //Create the worker thread pool.  These threads will
    //'queue up' and wait for futures to become available    
    for (i = 0; i < THREAD_POOL_SIZE; i++)
    {
        pthread_create(&threadid, NULL, worker_thread_future_loop, NULL);
        g_pool_threads[i] = threadid;
    }
}


/**********************************************************************/
/* Primitive implementations                    					  */
/**********************************************************************/

Scheme_Object *future(int argc, Scheme_Object *argv[])
{
	START_XFORM_SKIP;
    int init_runstack_size, main_runstack_size;
    int futureid = ++g_next_futureid;
    future_t *ft;
    Scheme_Object **old_rs, **old_rs_start;
    Scheme_Native_Closure *nc;
    Scheme_Native_Closure_Data *ncd;
    Scheme_Object *lambda = argv[0];
    Scheme_Type type = SCHEME_TYPE(lambda);
    nc = (Scheme_Native_Closure*)lambda;
    ncd = nc->code;

    //Create the future descriptor and add to the queue as 'pending'    
    pthread_mutex_lock(&g_future_queue_mutex);
    ft = enqueue_future();
    pthread_cond_init(&ft->can_continue_cv, NULL);
    ft->id = futureid;
    ft->orig_lambda = lambda;
    ft->pending = 1;
   
    //Allocate the runstack and copy the runtime thread's
    //runstack
		init_runstack_size = MZ_RUNSTACK - MZ_RUNSTACK_START;

    ft->runstack_start = scheme_alloc_runstack(init_runstack_size);
    ft->runstack = ft->runstack_start + init_runstack_size;
    //memcpy(ft->runstack_start, MZ_RUNSTACK_START, main_runstack_size);

    pthread_mutex_unlock(&g_future_queue_mutex);

    //JIT compile the code
    //Temporarily repoint MZ_RUNSTACK
    //to the worker thread's runstack -
    //in case the JIT compiler uses the stack address
    //when generating code
    //old_rs = MZ_RUNSTACK;
    //old_rs_start = MZ_RUNSTACK_START;
    //MZ_RUNSTACK = ft->runstack;
    //MZ_RUNSTACK_START = ft->runstack_start;
    scheme_on_demand_generate_lambda(nc, 0, NULL);
    //MZ_RUNSTACK = old_rs;
    //MZ_RUNSTACK_START = old_rs_start;

		pthread_mutex_lock(&g_future_queue_mutex);
		ft->code = (void*)ncd->code;
		pthread_mutex_unlock(&g_future_queue_mutex);

    //Signal that a future is pending
    pthread_mutex_lock(&g_future_pending_mutex);
    pthread_cond_signal(&g_future_pending_cv);
    pthread_mutex_unlock(&g_future_pending_mutex);

    return scheme_make_integer(futureid);
    END_XFORM_SKIP;
}


Scheme_Object *touch(int argc, Scheme_Object *argv[])
{
	START_XFORM_SKIP;
    Scheme_Object *retval = NULL;
    void *rtcall_retval = NULL;
	future_t *ft;
	int futureid;

	futureid = SCHEME_INT_VAL(argv[0]);

    pthread_mutex_lock(&g_future_queue_mutex);
    ft = get_future(futureid);
    pthread_mutex_unlock(&g_future_queue_mutex);

    //Spin waiting for primitive calls or a return value from
    //the worker thread
    wait_for_rtcall_or_completion:
        pthread_mutex_lock(&g_future_queue_mutex);
        if (ft->work_completed)
        {
            retval = ft->retval;

            //Destroy the future descriptor
            if (ft->prev == NULL)
            {
                //Set next to be the head of the queue
                g_future_queue = ft->next;
                if (g_future_queue != NULL)
                    g_future_queue->prev = NULL;
                
                free(ft);
            }
            else
            {
                ft->prev->next = ft->next;
                if (NULL != ft->next)
                    ft->next->prev = ft->prev;

                free(ft);
            }

            pthread_mutex_unlock(&g_future_queue_mutex);
        }
        else if (ft->rt_prim != NULL)
        {
            //Invoke the primitive and stash the result
            //Release the lock so other threads can manipulate the queue
            //while the runtime call executes
            pthread_mutex_unlock(&g_future_queue_mutex);
            rtcall_retval = invoke_rtcall(ft);
            pthread_mutex_lock(&g_future_queue_mutex);

            ft->rt_prim_retval = rtcall_retval;
            ft->rt_prim = NULL;
            ft->rt_prim_sigtype = 0;
            ft->rt_prim_args = NULL;

            //Signal the waiting worker thread that it
            //can continue running machine code
            pthread_cond_signal(&ft->can_continue_cv);
            pthread_mutex_unlock(&g_future_queue_mutex);

            goto wait_for_rtcall_or_completion;
        }
        else
        {
            pthread_mutex_unlock(&g_future_queue_mutex);
            goto wait_for_rtcall_or_completion;
        }

    return retval;
	END_XFORM_SKIP;
}


//Entry point for a worker thread allocated for
//executing futures.  This function will never terminate
//(until the process dies).
void *worker_thread_future_loop(void *arg)
{
	START_XFORM_SKIP;
	Scheme_Object *v;
	Scheme_Object* (*jitcode)(Scheme_Object*, int, Scheme_Object**);

    wait_for_work:
        LOG("Waiting for new future work...");
        pthread_mutex_lock(&g_future_pending_mutex);
        pthread_cond_wait(&g_future_pending_cv, &g_future_pending_mutex);

        LOG("Got a signal that a future is pending...");
        
        //Work is available for this thread
        pthread_mutex_lock(&g_future_queue_mutex);
        future_t *ft = get_pending_future();
        ft->pending = 0;
        ft->threadid = pthread_self();

		//Initialize the runstack for this thread 
		//MZ_RUNSTACK AND MZ_RUNSTACK_START should be thread-local
		MZ_RUNSTACK = ft->runstack;
		MZ_RUNSTACK_START = ft->runstack_start;		

		//Set up the JIT compiler for this thread 
		scheme_jit_fill_threadlocal_table();
        
        jitcode = (Scheme_Object* (*)(Scheme_Object*, int, Scheme_Object**))(ft->code);
        pthread_mutex_unlock(&g_future_queue_mutex);
        pthread_mutex_unlock(&g_future_pending_mutex);

        //Run the code
        //Passing no arguments for now.
        //The lambda passed to a future will always be a parameterless
        //function.
        //From this thread's perspective, this call will never return
        //until all the work to be done in the future has been completed,
        //including runtime calls.
        v = jitcode(ft->orig_lambda, 0, NULL);

        //Set the return val in the descriptor
        pthread_mutex_lock(&g_future_queue_mutex);
        ft->work_completed = 1;
        ft->retval = v;
        pthread_mutex_unlock(&g_future_queue_mutex);

        goto wait_for_work;

    return NULL;
	END_XFORM_SKIP;
}


//Returns 0 if the call isn't actually executed by this function,
//i.e. if we are already running on the runtime thread.  Otherwise returns
//1, and 'retval' is set to point to the return value of the runtime
//call invocation.
int future_do_runtimecall(
    void *func,
    int sigtype,
    void *args,
    void *retval)
{
	START_XFORM_SKIP;
	future_t *future;
    //If already running on the main thread
    //or no future is involved, do nothing
    //and return FALSE
    if (pthread_self() == g_rt_threadid)
    {
		//Should never get here!  This check should be done 
		//by the caller using the macros defined in scheme-futures.h!
        return 0;
    }

    //Fetch the future descriptor for this thread
    future = get_my_future();

    //set up the arguments for the runtime call
    //to be picked up by the main rt thread
    //pthread_mutex_lock(&future->mutex);
    pthread_mutex_lock(&g_future_queue_mutex);

	//Update the stack pointer for this future 
	//to be in sync with MZ_RUNSTACK - the runtime thread 
	//will use this value to temporarily swap its stack 
	//for the worker thread's
	future->runstack = MZ_RUNSTACK;
    future->rt_prim = func;
    future->rt_prim_sigtype = sigtype;
    future->rt_prim_args = args;

    //Wait for the signal that the RT call is finished
    pthread_cond_wait(&future->can_continue_cv, &g_future_queue_mutex);

    //Clear rt call fields before releasing the lock on the descriptor
    future->rt_prim = NULL;
    future->rt_prim_sigtype = 0;
    future->rt_prim_args = NULL;

    retval = future->rt_prim_retval;
    pthread_mutex_unlock(&g_future_queue_mutex);
    return 1;
	END_XFORM_SKIP;
}


/**********************************************************************/
/* Functions for primitive invocation                   			  */
/**********************************************************************/
int rtcall_void_void(void (*f)())
{
	START_XFORM_SKIP;
	future_t *future;
	sig_void_void_t data;
	memset(&data, 0, sizeof(sig_void_void_t));
	if (!IS_WORKER_THREAD)
	{
		return 0;
	}

	LOG_RTCALL_VOID_VOID(f);

	#ifdef DEBUG_FUTURES
	debug_save_context();
	#endif

	data.prim = f;

	future = get_my_future();
	future->rt_prim_sigtype = SIG_VOID_VOID;
	future->calldata.void_void = data;

	future_do_runtimecall((void*)f, SIG_VOID_VOID, NULL, NULL);

	#ifdef DEBUG_FUTURES
	debug_kill_context();
	#endif

	return 1;
	END_XFORM_SKIP;
}


int rtcall_obj_int_pobj_obj(
	Scheme_Object* (*f)(Scheme_Object*, int, Scheme_Object**), 
	Scheme_Object *a, 
	int b, 
	Scheme_Object **c, 
	Scheme_Object *retval)
{
	START_XFORM_SKIP;
	future_t *future;
	sig_obj_int_pobj_obj_t data;
	memset(&data, 0, sizeof(sig_obj_int_pobj_obj_t));
	if (!IS_WORKER_THREAD)	
	{
		return 0;
	}

	LOG_RTCALL_OBJ_INT_POBJ_OBJ(f, a, b, c);

	#ifdef DEBUG_FUTURES
	debug_save_context();
	#endif

	data.prim = f;
	data.a = a;
	data.b = b;
	data.c = c;
	
	future = get_my_future();
	future->rt_prim_sigtype = SIG_OBJ_INT_POBJ_OBJ;
	future->calldata.obj_int_pobj_obj = data;

	future_do_runtimecall((void*)f, SIG_OBJ_INT_POBJ_OBJ, NULL, NULL);
	*retval = *(future->calldata.obj_int_pobj_obj.retval);

	#ifdef DEBUG_FUTURES
	debug_kill_context();
	#endif

	return 1;
	END_XFORM_SKIP;
}


//Does the work of actually invoking a primitive on behalf of a 
//future.  This function is always invoked on the main (runtime) 
//thread.
void *invoke_rtcall(future_t *future)
{
  START_XFORM_SKIP;
  void *ret = NULL, *dummy_ret, *args = future->rt_prim_args;
  void **arr = NULL;
  MZ_MARK_STACK_TYPE lret = 0;

	//Temporarily use the worker thread's runstack 
	Scheme_Object **old_rs = MZ_RUNSTACK, **old_rs_start = MZ_RUNSTACK_START;
	MZ_RUNSTACK = future->runstack;
	MZ_RUNSTACK_START = future->runstack_start;
	#ifdef DEBUG_FUTURES
	debug_assert_context(future);
	g_rtcall_count++;
	#endif

  switch (future->rt_prim_sigtype)
  {
    case SIG_VOID_VOID:
		{
			sig_void_void_t *data = &future->calldata.void_void;
			data->prim();

      //((void (*)(void))future->rt_prim)();
      ret = &dummy_ret;
      break;
		}
    case SIG_OBJ_INT_POBJ_OBJ:
		{
			sig_obj_int_pobj_obj_t *data = &future->calldata.obj_int_pobj_obj;
			data->retval = data->prim(
				data->a, 
				data->b, 
				data->c);	

        //arr = (void**)args;
        //ret = (void*)((Scheme_Object* (*)(Scheme_Object*, int, Scheme_Object**))future->rt_prim)(
        //    (Scheme_Object*)arr[0],
        //    GET_INT(arr[1]),
        //    (Scheme_Object**)arr[2]);
                    
			break;
		}
            case SIG_OBJ_INT_POBJ_VOID:
                    arr = (void**)args;
                    ((Scheme_Object* (*)(Scheme_Object*, int, Scheme_Object**))future->rt_prim)(
                        (Scheme_Object*)arr[0],
                        GET_INT(arr[1]),
                        (Scheme_Object**)arr[2]);

                    ret = (void*)0x1;
            case SIG_INT_OBJARR_OBJ:
                    arr = (void**)args;
                    ret = (void*)((Scheme_Object* (*)(int, Scheme_Object*[]))future->rt_prim)(
                        GET_INT(arr[0]),
                        (Scheme_Object**)arr[1]);
                    break;
            case SIG_LONG_OBJ_OBJ:
                    arr = (void**)args;
                    ret = (void*)((Scheme_Object* (*)(long, Scheme_Object*))future->rt_prim)(
                        GET_LONG(arr[0]),
                        (Scheme_Object*)arr[1]);
                    break;
            case SIG_OBJ_OBJ:
                    ret = (void*)((Scheme_Object* (*)(Scheme_Object*))future->rt_prim)((Scheme_Object*)args);
                    break;
            case SIG_OBJ_OBJ_OBJ:
                    arr = (void**)args;
                    ret = (void*)((Scheme_Object * (*)(Scheme_Object*, Scheme_Object*))future->rt_prim)(
                        (Scheme_Object*)arr[0],
                        (Scheme_Object*)arr[1]);
                    break;
            case SIG_VOID_PVOID:
                    ret = ((void* (*)(void))future->rt_prim)();
                    break;
            case SIG_SNCD_OBJ:
                    ret = (void*)((Scheme_Object* (*)(Scheme_Native_Closure_Data*))future->rt_prim)(
                        (Scheme_Native_Closure_Data*)args);
                    break;
            case SIG_OBJ_VOID:
                    ((void (*)(Scheme_Object*))future->rt_prim)((Scheme_Object*)args);
                    ret = &dummy_ret;
                    break;
            case SIG_LONG_OBJ:
                    ret = ((Scheme_Object* (*)(long))future->rt_prim)(GET_LONG(args));
                    break;
            case SIG_BUCKET_OBJ_INT_VOID:
                    arr = (void**)args;
                    ((void (*)(Scheme_Bucket*, Scheme_Object*, int))future->rt_prim)(
                        (Scheme_Bucket*)arr[0],
                        (Scheme_Object*)arr[1],
                        GET_INT(arr[2]));

                    ret = &dummy_ret;
                    break;
            case SIG_INT_INT_POBJ_VOID:
                    arr = (void**)args;
                    ((void (*)(int, int, Scheme_Object**))future->rt_prim)(
                        GET_INT(arr[0]),
                        GET_INT(arr[1]),
                        (Scheme_Object**)arr[2]);
                    break;
            case SIG_OBJ_OBJ_MZST:
                    arr = (void**)args;
                    lret = ((MZ_MARK_STACK_TYPE (*)(Scheme_Object*, Scheme_Object*))future->rt_prim)(
                        (Scheme_Object*)arr[0],
                        (Scheme_Object*)arr[1]);

                    ret = malloc(sizeof(MZ_MARK_STACK_TYPE));
                    *((MZ_MARK_STACK_TYPE*)ret) = lret;
                    break;
            case SIG_BUCKET_VOID:
                    ((void (*)(Scheme_Bucket*))future->rt_prim)((Scheme_Bucket*)args);
                    ret = &dummy_ret;
                    break;
            case SIG_POBJ_LONG_OBJ:
                    arr = (void**)args;
                    ret = ((Scheme_Object* (*)(Scheme_Object**, long))future->rt_prim)(
                        (Scheme_Object**)arr[0],
                        GET_LONG(arr[1]));
                    break;
            case SIG_INT_POBJ_INT_OBJ:
                    arr = (void**)args;
                    ret = ((Scheme_Object* (*)(int, Scheme_Object**, int))future->rt_prim)(
                        GET_INT(arr[0]),
                        (Scheme_Object**)arr[1],
                        GET_INT(arr[2]));
                    break;
            case SIG_INT_POBJ_OBJ_OBJ:
                    arr = (void**)args;
                    ret = ((Scheme_Object* (*)(int, Scheme_Object**, Scheme_Object*))future->rt_prim)(
                        GET_INT(arr[0]),
                        (Scheme_Object**)arr[1],
                        (Scheme_Object*)arr[2]);
                    break;
						case SIG_ENV_ENV_VOID: 
										arr = (void**)args;
										((void (*)(Scheme_Env*, Scheme_Env*))future->rt_prim)(
											GET_SCHEMEENV(arr[0]), 
											GET_SCHEMEENV(arr[1]));
										break;
    }

		//Restore main thread's runstack 
		MZ_RUNSTACK = old_rs;
		MZ_RUNSTACK_START = old_rs_start;

    return ret;
    END_XFORM_SKIP;
}


/**********************************************************************/
/* Helpers for manipulating the futures queue                         */
/**********************************************************************/

future_t *enqueue_future(void)
{
	START_XFORM_SKIP;
    future_t *last = get_last_future();
    future_t *ft = (future_t*)malloc(sizeof(future_t));
    memset(ft, 0, sizeof(future_t));
    if (NULL == last)
    {
        g_future_queue = ft;
        return ft;
    }

    ft->prev = last;
    last->next = ft;
    ft->next = NULL;
    
    return ft;
	END_XFORM_SKIP;
}


future_t *get_pending_future(void)
{
	START_XFORM_SKIP;
    future_t *f;
    for (f = g_future_queue; f != NULL; f = f->next)
    {
        if (f->pending)
            return f;
    }

    return NULL;
	END_XFORM_SKIP;
}


future_t *get_my_future(void)
{
    return get_future_by_threadid(pthread_self());
}


future_t *get_future_by_threadid(pthread_t threadid)
{
	START_XFORM_SKIP;
    future_t *ft = g_future_queue;
    if (NULL == ft)
		{
        return ft;
		}
    
    while (ft->threadid != threadid)
		{
        ft = ft->next;
		}

    //Sanity check
    if (ft->threadid != threadid)
		{
        return NULL;
		}

    return ft;
		END_XFORM_SKIP;
}


future_t *get_future(int futureid)
{
	START_XFORM_SKIP;
    future_t *ft = g_future_queue;
    if (NULL == ft)
	{
        return ft;
	}
    
    while (ft->id != futureid)
	{
        ft = ft->next;
	}

    //Sanity check
    if (ft->id != futureid)
	{
        return NULL;
	}

    return ft;
	END_XFORM_SKIP;
}


future_t *get_last_future(void)
{
	START_XFORM_SKIP;
  future_t *ft = g_future_queue;
  if (NULL == ft)
	{
        return ft;
	}
    
  while (ft->next != NULL)
	{
    ft = ft->next;
	}

    return ft;
	END_XFORM_SKIP;
}


void clear_futures(void)
{
    int i;
	future_t *f, *tmp;
  pthread_mutex_lock(&g_future_queue_mutex);
  for (i = 0; i < THREAD_POOL_SIZE; i++)
  {
    pthread_cancel(g_pool_threads[i]);
  }

  pthread_mutex_unlock(&g_future_queue_mutex);
  f = get_last_future();
  if (NULL == f)
      return;

  while (1)
  {
      tmp = f->prev;
      free(f);
      if (tmp == NULL)
			{
          break;
			}

      tmp->next = NULL;
      f = tmp;
  }

  g_future_queue = NULL;
}

#endif
