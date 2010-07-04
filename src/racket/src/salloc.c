/*
  Racket
  Copyright (c) 2004-2010 PLT Scheme Inc.
  Copyright (c) 1995-2001 Matthew Flatt
 
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
    Boston, MA 02110-1301 USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#ifndef MZ_PRECISE_GC
# define SCHEME_NO_GC_PROTO
#endif

#include "schpriv.h"
#include <string.h>
#include "schgc.h"

#ifdef DOS_FAR_POINTERS
# include <alloc.h>
# define MALLOC farmalloc
#else
# define MALLOC malloc
#endif

#ifdef MZ_JIT_USE_MPROTECT
# include <unistd.h>
# include <sys/mman.h>
# ifndef MAP_ANON
#  include <fcntl.h>
# endif
#endif
#ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
# include <windows.h>
#endif

THREAD_LOCAL_DECL(static void **dgc_array);
THREAD_LOCAL_DECL(static int *dgc_count);
THREAD_LOCAL_DECL(static int dgc_size);

#ifdef USE_THREAD_LOCAL
# if defined(IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS)
pthread_key_t scheme_thread_local_key;
# elif defined(IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS)
unsigned long scheme_tls_delta;
int scheme_tls_index;
# elif defined(IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS_FUNC)
DWORD scheme_thread_local_key;
# else
SHARED_OK THREAD_LOCAL Thread_Local_Variables scheme_thread_locals;
# endif
#endif

extern int scheme_num_copied_stacks;
SHARED_OK static unsigned long scheme_primordial_os_thread_stack_base;
THREAD_LOCAL_DECL(static unsigned long scheme_os_thread_stack_base);
#ifdef USE_THREAD_LOCAL
SHARED_OK Thread_Local_Variables *scheme_vars; /* for debugging */
#endif

HOOK_SHARED_OK static Scheme_Report_Out_Of_Memory_Proc more_report_out_of_memory;

#if defined(MZ_XFORM) && !defined(MZ_PRECISE_GC)
void **GC_variable_stack;
#endif

#ifndef MZ_PRECISE_GC
extern MZ_DLLIMPORT void GC_register_late_disappearing_link(void **link, void *obj);
#endif

SHARED_OK static int use_registered_statics;

/************************************************************************/
/*                           stack setup                                */
/************************************************************************/

#if !defined(MZ_PRECISE_GC) && !defined(USE_SENORA_GC)
extern MZ_DLLIMPORT void GC_init();
#endif

struct free_list_entry {
  long size; /* size of elements in this bucket */
  void *elems; /* doubly linked list for free blocks */
  int count; /* number of items in `elems' */
};

SHARED_OK static struct free_list_entry *free_list;
SHARED_OK static int free_list_bucket_count;
#ifdef MZ_USE_PLACES
SHARED_OK static mzrt_mutex *free_list_mutex;
#endif


void scheme_init_salloc() {
#ifdef MZ_USE_PLACES
  mzrt_mutex_create(&free_list_mutex);
#endif
}

void scheme_set_stack_base(void *base, int no_auto_statics) XFORM_SKIP_PROC
{
#ifdef MZ_PRECISE_GC
  GC_init_type_tags(_scheme_last_type_, 
                    scheme_pair_type, scheme_mutable_pair_type, scheme_weak_box_type, 
                    scheme_ephemeron_type, scheme_rt_weak_array,
                    scheme_cust_box_type);
  /* We want to be able to allocate symbols early. */
  scheme_register_traversers();
#endif

  scheme_primordial_os_thread_stack_base  = (unsigned long) base;
  scheme_os_thread_stack_base             = (unsigned long) base;

#if defined(MZ_PRECISE_GC) || defined(USE_SENORA_GC)
  GC_set_stack_base(base);
  /* no_auto_statics must always be true! */
#else
  GC_stackbottom = base;
  if (no_auto_statics) {
    GC_no_dls = 1;
    GC_init();
    GC_clear_roots();
  } else {
# if (defined(__APPLE__) && defined(__MACH__)) || defined(MZ_USE_IRIX_SPROCS)
    GC_init(); /* For Darwin, CGC requires GC_init() always */
# endif
  }
#endif
  use_registered_statics = no_auto_statics;
#if defined(MZ_PRECISE_GC)
  GC_report_out_of_memory = scheme_out_of_memory_abort;
#endif
}

void scheme_set_current_os_thread_stack_base(void *base)
{
  scheme_os_thread_stack_base = (unsigned long) base;
}

unsigned long scheme_get_current_os_thread_stack_base()
{
  return scheme_os_thread_stack_base;
}

typedef struct {
  Scheme_Env_Main _main;
  int argc;
  char **argv;
} Scheme_Main_Data;

static int call_with_basic(void *data)
{
  Scheme_Main_Data *ma = (Scheme_Main_Data *)data;
  Scheme_Env_Main _main = ma->_main;
  
  return _main(scheme_basic_env(), ma->argc, ma->argv);
}

int scheme_main_setup(int no_auto_statics, Scheme_Env_Main _main, int argc, char **argv) XFORM_SKIP_PROC
{
  Scheme_Main_Data d;
  d._main = _main;
  d.argc = argc;
  d.argv = argv;
  return scheme_main_stack_setup(no_auto_statics, call_with_basic, &d);
}

static int do_main_stack_setup(int no_auto_statics, Scheme_Nested_Main _main, void *data) 
{
  void *stack_start;
  int volatile return_code;

#ifdef USE_THREAD_LOCAL
  scheme_vars = scheme_get_thread_local_variables();
#endif

  scheme_set_stack_base(PROMPT_STACK(stack_start), no_auto_statics);

  return_code = _main(data);

#ifdef MZ_PRECISE_GC
  /* Trick xform conversion to keep start_addr: */
  stack_start = NULL;
#endif

  return return_code;
}

#if defined(IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS)
/* This allows for places gc unit tests to switch the Thread_Local_Variables and simulate places */
void scheme_set_thread_local_variables(Thread_Local_Variables *tlvs) XFORM_SKIP_PROC
{
  pthread_setspecific(scheme_thread_local_key, tlvs);
}
#endif

#if 0 && defined(IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS) && defined(INLINE_GETSPECIFIC_ASSEMBLY_CODE)
/* This code is dsiabled */
static void macosx_get_thread_local_key_for_assembly_code() XFORM_SKIP_PROC
{
  /* Our [highly questionable] strategy for inlining pthread_getspecific() is taken from 
     the Go implementation (see "http://golang.org/src/libcgo/darwin_386.c").
     In brief, we assume that thread-local variables are going to be
     accessed via the gs segment register at offset 0x48 (i386) or 0x60 (x86_64),
     and we also hardwire the thread-local key 0x110. Here we have to try to get
     that particular key and double-check that it worked. */
  pthread_key_t unwanted[16];
  int num_unwanted = 0;

  while (1) {
    if (pthread_key_create(&scheme_thread_local_key, NULL)) {
      fprintf(stderr, "pthread key create failed\n");
      abort();
    }
    if (scheme_thread_local_key == 0x110)
      break;
    else {
      if (num_unwanted == 24) {
        fprintf(stderr, "pthread key create never produced 0x110 for inline hack\n");
        abort();
      }
      unwanted[num_unwanted++] = scheme_thread_local_key;
    }
  }

  pthread_setspecific(scheme_thread_local_key, (void *)0xaced);
  if (scheme_get_thread_local_variables() != (Thread_Local_Variables *)0xaced) {
    fprintf(stderr, "pthread getspecific inline hack failed\n");
    abort();
  }

  while (num_unwanted--) {
    pthread_key_delete(unwanted[num_unwanted]);
  }
}
#endif

#ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
void scheme_register_tls_space(void *tls_space, int tls_index) XFORM_SKIP_PROC
{
  scheme_tls_delta = (unsigned long)tls_space;
  scheme_tls_index = tls_index;
}
Thread_Local_Variables *scheme_external_get_thread_local_variables() XFORM_SKIP_PROC
{
  return scheme_get_thread_local_variables();
}
#endif

void scheme_setup_thread_local_key_if_needed() XFORM_SKIP_PROC
{
#ifdef IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS
  scheme_thread_local_key = 0;
  if (pthread_key_create(&scheme_thread_local_key, NULL)) {
    fprintf(stderr, "pthread key create failed\n");
    abort();
  }
#endif
#ifdef IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS
  {
    void **base;

    __asm { mov ecx, FS:[0x2C]
            mov base, ecx }
    scheme_tls_delta -= (unsigned long)base[scheme_tls_index];
    scheme_tls_index *= sizeof(void*);
  }
#endif
}

int scheme_main_stack_setup(int no_auto_statics, Scheme_Nested_Main _main, void *data) XFORM_SKIP_PROC
{
  scheme_setup_thread_local_key_if_needed();
  scheme_init_os_thread();
  return do_main_stack_setup(no_auto_statics, _main, data);
}

void scheme_set_stack_bounds(void *base, void *deepest, int no_auto_statics) XFORM_SKIP_PROC
{
  scheme_set_stack_base(base, no_auto_statics);

#ifdef USE_STACK_BOUNDARY_VAR
  if (deepest) {
    scheme_stack_boundary = (unsigned long)deepest;
  }
#endif
}

extern unsigned long scheme_get_stack_base() XFORM_SKIP_PROC
{
#if !defined(MZ_PRECISE_GC) && !defined(USE_SENORA_GC)
  if (GC_stackbottom)
    return (unsigned long)GC_stackbottom;
  else {
    struct GC_stack_base b;
    GC_get_stack_base(&b);
    return (unsigned long)b.mem_base;
  }
#else
  return (unsigned long)GC_get_stack_base();
#endif
}

void scheme_out_of_memory_abort()
{
  scheme_log_abort("Racket virtual machine has run out of memory; aborting");
  if (more_report_out_of_memory)
    more_report_out_of_memory();
  abort();
}

void scheme_set_report_out_of_memory(Scheme_Report_Out_Of_Memory_Proc p)
{
  more_report_out_of_memory = p;
}

#ifdef OS_X
#include <mach/mach.h>
# ifdef MZ_PRECISE_GC
extern void GC_attach_current_thread_exceptions_to_handler();
# endif
#endif

#ifdef IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS
void* scheme_dbg_get_thread_local_variables() XFORM_SKIP_PROC {
  return pthread_getspecific(scheme_thread_local_key);
}
#endif

void *scheme_get_os_thread_like()
{
#if defined(IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS) || defined(IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS)
  return scheme_get_thread_local_variables();
#else
  return NULL;
#endif
}

void scheme_init_os_thread_like(void *other) XFORM_SKIP_PROC
{
#if defined(IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS) || defined(IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS)
  Thread_Local_Variables *vars;
  if (other)
    vars = (Thread_Local_Variables *)other;
  else {
    vars = (Thread_Local_Variables *)malloc(sizeof(Thread_Local_Variables));
    memset(vars, 0, sizeof(Thread_Local_Variables));
  }
# ifdef IMPLEMENT_THREAD_LOCAL_VIA_PTHREADS
  pthread_setspecific(scheme_thread_local_key, vars);
# elif defined(IMPLEMENT_THREAD_LOCAL_VIA_WIN_TLS)
  *scheme_get_thread_local_variables_ptr() = vars;
# else
  TlsSetValue(scheme_thread_local_key, vars);
# endif
#endif
#ifdef OS_X
# ifdef MZ_PRECISE_GC
  GC_attach_current_thread_exceptions_to_handler();
# endif
#endif
}

void scheme_init_os_thread() XFORM_SKIP_PROC
{
  scheme_init_os_thread_like(NULL);
}

/************************************************************************/
/*                           memory utils                               */
/************************************************************************/

void scheme_dont_gc_ptr(void *p)
{
  int i, oldsize;
  void **naya;
  int *nayac;

  /* look for existing: */
  for (i = 0; i < dgc_size; i++) {
    if (dgc_array[i] == p) {
      dgc_count[i]++;
      return;
    }
  }

  /* look for empty slot: */
  for (i = 0; i < dgc_size; i++) {
    if (!dgc_array[i]) {
      dgc_array[i] = p;
      dgc_count[i] = 1;
      return;
    }
  }

  /* Make more room: */
  oldsize = dgc_size;
  if (!dgc_array) {
    REGISTER_SO(dgc_array);
    REGISTER_SO(dgc_count);
    dgc_size = 50;
  } else
    dgc_size *= 2;

  naya = MALLOC_N(void*, dgc_size);
  nayac = MALLOC_N(int, dgc_size);

  for (i = 0; i < oldsize; i++) {
    naya[i] = dgc_array[i];
    nayac[i] = dgc_count[i];
  }

  for (; i < dgc_size; i++) {
    naya[i] = NULL;
    nayac[i] = 0;
  }

  dgc_array = naya;
  dgc_count = nayac;

  dgc_array[oldsize] = p;
  dgc_count[oldsize] = 1;
}

void scheme_gc_ptr_ok(void *p)
{
  int i;
  
  for (i = 0; i < dgc_size; i++) {
    if (dgc_array[i] == p) {
      if (!(--dgc_count[i]))
	dgc_array[i] = NULL;
      break;
    }
  }
}

#ifdef NO_GC
void *
scheme_malloc (size_t size)
{
  void *space;

  space = MALLOC(size);
  if (!space)
    scheme_raise_out_of_memory(NULL, NULL);

  return (space);
}
#endif

void *
scheme_calloc (size_t num, size_t size)
{
  void *space;
  
  space = MALLOC(num*size);
  if (!space)
    scheme_raise_out_of_memory(NULL, NULL);
#ifdef NO_GC
  memset(space, 0, (num*size));
#endif

  return (space);
}

char *
scheme_strdup(const char *str)
{
  char *naya;
  long len;

  len = strlen(str) + 1;
  naya = (char *)scheme_malloc_atomic (len * sizeof (char));
  memcpy (naya, str, len);

  return naya;
}

char *
scheme_strdup_eternal(const char *str)
{
  char *naya;
  long len;

  len = strlen(str) + 1;
  naya = (char *)scheme_malloc_eternal(len * sizeof (char));
  memcpy (naya, str, len);

  return naya;
}

/************************************************************************/
/*                               cptr                                   */
/************************************************************************/

Scheme_Object *scheme_make_cptr(void *cptr, Scheme_Object *typetag)
{
  Scheme_Object *o;

  o = (Scheme_Object *)scheme_malloc_small_tagged(sizeof(Scheme_Cptr));
  o->type = scheme_cpointer_type;
  SCHEME_CPTR_VAL(o) = cptr;
  SCHEME_CPTR_TYPE(o) = (void *)typetag;

  return o;
}

Scheme_Object *scheme_make_external_cptr(GC_CAN_IGNORE void *cptr, Scheme_Object *typetag)
{
  Scheme_Object *o;
  o = scheme_make_cptr(NULL, typetag);
  SCHEME_CPTR_VAL(o) = cptr;
  return o;
}

Scheme_Object *scheme_make_offset_cptr(void *cptr, long offset, Scheme_Object *typetag)
{
  Scheme_Object *o;

  o = (Scheme_Object *)scheme_malloc_small_tagged(sizeof(Scheme_Offset_Cptr));
  o->type = scheme_offset_cpointer_type;
  SCHEME_CPTR_VAL(o) = cptr;
  SCHEME_CPTR_TYPE(o) = (void *)typetag;
  ((Scheme_Offset_Cptr *)o)->offset = offset;

  return o;
}

Scheme_Object *scheme_make_offset_external_cptr(GC_CAN_IGNORE void *cptr, long offset, Scheme_Object *typetag)
{
  Scheme_Object *o;
  o = scheme_make_offset_cptr(NULL, offset, typetag);
  SCHEME_CPTR_VAL(o) = cptr;
  return o;
}


/************************************************************************/
/*                            allocation                                */
/************************************************************************/

#ifndef MZ_PRECISE_GC
static Scheme_Hash_Table *immobiles;
#endif

void **scheme_malloc_immobile_box(void *p)
{
#ifdef MZ_PRECISE_GC
  return GC_malloc_immobile_box(p);
#else
  void **b;

  if (!immobiles) {
    REGISTER_SO(immobiles);
    immobiles = scheme_make_hash_table(SCHEME_hash_ptr);
  }

  b = scheme_malloc(sizeof(void *));
  *b = p;
  scheme_hash_set(immobiles, (Scheme_Object *)(void *)b, scheme_true);

  return b;
#endif
}

void scheme_free_immobile_box(void **b)
{
#ifdef MZ_PRECISE_GC
  GC_free_immobile_box(b);
#else
  if (immobiles) {
    scheme_hash_set(immobiles, (Scheme_Object *)(void *)b, NULL);
  }
#endif
}

THREAD_LOCAL_DECL(static void (*save_oom)(void));

static void raise_out_of_memory(void)
{
  GC_out_of_memory = save_oom;
  scheme_raise_out_of_memory(NULL, NULL);
}

void *scheme_malloc_fail_ok(void *(*f)(size_t), size_t s)
{
  void *v;

  save_oom = GC_out_of_memory;
  GC_out_of_memory = raise_out_of_memory;
  v = f(s);
  GC_out_of_memory = save_oom;

  return v;
}

void scheme_end_stubborn_change(void *p)
{
#ifndef MZ_PRECISE_GC
  GC_end_stubborn_change(p);
#endif
}

void *scheme_malloc_eternal(size_t n)
{
#ifdef USE_SENORA_GC
  return GC_malloc_atomic_uncollectable(n);
#else
  void *s;

  s = MALLOC(n);
  if (!s) {
    if (GC_out_of_memory)
      GC_out_of_memory();
    else {
      if (scheme_console_printf)
	scheme_console_printf("out of memory\n");
      else
	printf("out of memory\n");
      exit(1);
    }
  }
	

  memset(s, 0, n);

  return s;
#endif
}

#ifdef MZ_PRECISE_GC
void *scheme_malloc_uncollectable(size_t size_in_bytes)
{
  void *p;
  p = scheme_malloc(size_in_bytes);
  scheme_dont_gc_ptr(p);
  return p;
}
#endif

void scheme_register_static(void *ptr, long size) XFORM_SKIP_PROC
{
#if defined(MZ_PRECISE_GC) || defined(USE_SENORA_GC)
  /* Always register for precise and Senora GC: */
  GC_add_roots((char *)ptr, (char *)(((char *)ptr) + size + 1));
#else
# ifdef GC_MIGHT_USE_REGISTERED_STATICS
  if (use_registered_statics) {
    GC_add_roots((char *)ptr, (char *)(((char *)ptr) + size + 1));
  }
# endif
#endif
}

#ifdef USE_TAGGED_ALLOCATION

struct GC_Set *tagged, *real_tagged, *tagged_atomic, *tagged_eternal, *tagged_uncollectable, *stacks, *envunbox;
struct GC_Set *tagged_while_counting;

static void trace_count(void *, int);
static void trace_path(void *, unsigned long, void *);
static void trace_init(void);
static void trace_done(void);
static void trace_stack_count(void *, int);
static void trace_stack_path(void *, unsigned long, void *);
static void finalize_object(void *);

#define TRACE_FUNCTIONS trace_init, trace_done, trace_count, trace_path

static void init_tagged_counting(void)
{
  if (!tagged_while_counting)
    tagged_while_counting = GC_new_set("counting", 
				       NULL, NULL, NULL, NULL, NULL,
				       0);
}

void *scheme_malloc_tagged(size_t s)
{
  if (!tagged) {
    init_tagged_counting();
    real_tagged = tagged = GC_new_set("tagged", TRACE_FUNCTIONS, 
				      finalize_object, 
				      0);
  }

  return GC_malloc_specific(s, tagged);
}

void *scheme_malloc_atomic_tagged(size_t s)
{
  if (!tagged_atomic) {
    init_tagged_counting();
    tagged_atomic = GC_new_set("tagged", TRACE_FUNCTIONS, 
			       finalize_object, 
			       SGC_ATOMIC_SET);
  }

  return GC_malloc_specific(s, tagged_atomic);
}

void *scheme_malloc_stubborn_tagged(size_t s)
{
  return scheme_malloc_tagged(s);
}

void *scheme_malloc_envunbox(size_t s)
{
  if (!envunbox)
    envunbox = GC_new_set("envunbox", 
			  NULL, NULL, NULL, NULL, NULL,
			  0);

  return GC_malloc_specific(s, envunbox);
}

void *scheme_malloc_stack(size_t s)
{
  if (!stacks)
    stacks = GC_new_set("envunbox", 
			trace_init, trace_done, trace_stack_count, trace_stack_path, 
			NULL,
			SGC_ATOMIC_SET);

  return GC_malloc_specific(s, stacks);
}

void *scheme_malloc_eternal_tagged(size_t s)
{
  if (!tagged_eternal) {
    init_tagged_counting();
    tagged_eternal = GC_new_set("tagged", TRACE_FUNCTIONS,
				finalize_object,
				SGC_UNCOLLECTABLE_SET | SGC_ATOMIC_SET);
  }

  return GC_malloc_specific(s, tagged_eternal);
}

void *scheme_malloc_uncollectable_tagged(size_t s)
{
  if (!tagged_uncollectable) {
    init_tagged_counting();
    tagged_uncollectable = GC_new_set("tagged", TRACE_FUNCTIONS, 
				      finalize_object,
				      SGC_UNCOLLECTABLE_SET);
  }

  return GC_malloc_specific(s, tagged_uncollectable);
}

#endif

/************************************************************************/
/*                         code allocation                              */
/************************************************************************/

/* We're not supposed to use mprotect() or VirtualProtect() on memory
   from malloc(); Posix says that mprotect() only works on memory from
   mmap(), and VirtualProtect() similarly requires alignment with a
   corresponding VirtualAlloc. So we implement a little allocator here
   for code chunks. */

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

/* Max of desired alignment and 2 * sizeof(long): */
#define CODE_HEADER_SIZE 16


THREAD_LOCAL_DECL(long scheme_code_page_total);

#if defined(MZ_JIT_USE_MPROTECT) && !defined(MAP_ANON)
static int fd, fd_created;
#endif

#define LOG_CODE_MALLOC(lvl, s) /* if (lvl > 1) s */
#define CODE_PAGE_OF(p) ((void *)(((unsigned long)p) & ~(page_size - 1)))

#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)


static long get_page_size()
{
# ifdef PAGESIZE
  const long page_size = PAGESIZE;
# else
  SHARED_OK static unsigned long page_size = -1;
  if (page_size == -1) {
#  ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    page_size = info.dwPageSize;
#  else
    page_size = sysconf (_SC_PAGESIZE);
#  endif
  }
# endif

  return page_size;
}

static void *malloc_page(long size)
{
  void *r;

#ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
  {
    DWORD old;
    r = (void *)VirtualAlloc(NULL, size, 
                             MEM_COMMIT | MEM_RESERVE, 
                             /* A note in gc/os_dep.c says that VirtualAlloc
                                doesn't like PAGE_EXECUTE_READWRITE. In case
                                that's true, we use a separate VirtualProtect step. */
                             PAGE_READWRITE);
    if (r)
      VirtualProtect(r, size, PAGE_EXECUTE_READWRITE, &old);
  }
#else
# ifdef MAP_ANON
  r = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANON, -1, 0);
# else
  if (!fd_created) {
    fd_created = 1;
    fd = open("/dev/zero", O_RDWR);
  }
  r = mmap(NULL, size, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE, fd, 0);
# endif
  if (r  == (void *)-1)
    r = NULL;
#endif

  if (!r)
    scheme_raise_out_of_memory(NULL, NULL);

  return r;
}

static void free_page(void *p, long size)
{
#ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
  VirtualFree(p, 0, MEM_RELEASE);
#else
  munmap(p, size);
#endif
}

static void init_free_list()
{
  long page_size = get_page_size();
  int pos = 0;
  int cnt = 2;
  long last_v = page_size, v;

  /* Compute size that fits 2 objects per page, then 3 per page, etc.
     Keeping CODE_HEADER_SIZE alignment gives us a small number of
     buckets. */
  while (1) {
    v = (page_size - CODE_HEADER_SIZE) / cnt;
    v = (v / CODE_HEADER_SIZE) * CODE_HEADER_SIZE;
    if (v != last_v) {
      free_list[pos].size = v;
      free_list[pos].elems = NULL;
      free_list[pos].count = 0;
      last_v = v;
      pos++;
      if (v == CODE_HEADER_SIZE)
        break;
    }
    cnt++;
  }

  free_list_bucket_count = pos;
}

static long free_list_find_bucket(long size)
{
  /* binary search */
  int lo = 0, hi = free_list_bucket_count - 1, mid;

  while (lo + 1 < hi) {
    mid = (lo + hi) / 2;
    if (free_list[mid].size > size) {
      lo = mid;
    } else {
      hi = mid;
    }
  }

  if (free_list[hi].size == size)
    return hi;
  else
    return lo;
}
#endif

void *scheme_malloc_code(long size)
{
#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)

  long size2, bucket, sz, page_size;
  void *p, *pg, *prev;

# ifdef MZ_USE_PLACES
  mzrt_mutex_lock(free_list_mutex);
# endif

  if (size < CODE_HEADER_SIZE) {
    /* ensure CODE_HEADER_SIZE alignment 
       and room for free-list pointers */
    size = CODE_HEADER_SIZE;
  }

  page_size = get_page_size();

  if (!free_list) {
    free_list = (struct free_list_entry *)malloc_page(page_size);
    scheme_code_page_total += page_size;
    init_free_list();
  }

  if (size > free_list[0].size) {
    /* allocate large object on its own page(s) */
    sz = size + CODE_HEADER_SIZE;
    sz = (sz + page_size - 1) & ~(page_size - 1);
    pg = malloc_page(sz);
    scheme_code_page_total += sz;
    *(long *)pg = sz;
    LOG_CODE_MALLOC(1, printf("allocated large %p (%ld) [now %ld]\n", 
                              pg, size + CODE_HEADER_SIZE, scheme_code_page_total));
    p = ((char *)pg) + CODE_HEADER_SIZE;
  }
  else {
    bucket = free_list_find_bucket(size);
    size2 = free_list[bucket].size;

    if (!free_list[bucket].elems) {
      /* add a new page's worth of items to the free list */
      int i, count = 0;
      pg = malloc_page(page_size);
      scheme_code_page_total += page_size;
      LOG_CODE_MALLOC(2, printf("new page for %ld / %ld at %p [now %ld]\n", 
            size2, bucket, pg, scheme_code_page_total));
      sz = page_size - size2;
      for (i = CODE_HEADER_SIZE; i <= sz; i += size2) {
        p = ((char *)pg) + i;
        prev = free_list[bucket].elems;
        ((void **)p)[0] = prev;
        ((void **)p)[1] = NULL;
        if (prev)
          ((void **)prev)[1] = p;
        free_list[bucket].elems = p;
        count++;
      }
      ((long *)pg)[0] = bucket; /* first long of page indicates bucket */
      ((long *)pg)[1] = 0; /* second long indicates number of allocated on page */
      free_list[bucket].count = count;
    }

    p = free_list[bucket].elems;
    prev = ((void **)p)[0];
    free_list[bucket].elems = prev;
    --free_list[bucket].count;
    if (prev)
      ((void **)prev)[1] = NULL;
    ((long *)CODE_PAGE_OF(p))[1] += 1;

    LOG_CODE_MALLOC(0, printf("allocated %ld (->%ld / %ld)\n", size, size2, bucket));
  }

# ifdef MZ_USE_PLACES
  mzrt_mutex_unlock(free_list_mutex);
# endif

  return p;
#else
  return malloc(size); /* good luck! */
#endif
}

void scheme_free_code(void *p)
{
#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)
  long size, size2, bucket, page_size;
  int per_page, n;
  void *prev;

# ifdef MZ_USE_PLACES
  mzrt_mutex_lock(free_list_mutex);
# endif

  page_size = get_page_size();

  size = *(long *)CODE_PAGE_OF(p);
  
  if (size >= page_size) {
    /* it was a large object on its own page(s) */
    scheme_code_page_total -= size;
    LOG_CODE_MALLOC(1, printf("freeing large %p (%ld) [%ld left]\n", 
          p, size, scheme_code_page_total));
    free_page((char *)p - CODE_HEADER_SIZE, size);
  }
  else {
    bucket = size;

    if ((bucket < 0) || (bucket >= free_list_bucket_count)) {
      printf("bad free: %p\n", (char *)p + CODE_HEADER_SIZE);
      abort();    
    }

    size2 = free_list[bucket].size;

    LOG_CODE_MALLOC(0, printf("freeing %ld / %ld\n", size2, bucket));

    /* decrement alloc count for this page: */
    per_page = (page_size - CODE_HEADER_SIZE) / size2;
    n = ((long *)CODE_PAGE_OF(p))[1];
    /* double-check: */
    if ((n < 1) || (n > per_page)) {
      printf("bad free: %p\n", (char *)p + CODE_HEADER_SIZE);
      abort();
    }
    n--;
    ((long *)CODE_PAGE_OF(p))[1] = n;

    /* add to free list: */
    prev = free_list[bucket].elems;
    ((void **)p)[0] = prev;
    ((void **)p)[1] = NULL;
    if (prev)
      ((void **)prev)[1] = p;
    free_list[bucket].elems = p;
    free_list[bucket].count++;

    /* Free whole page if it's completely on the free list, and if there
       are enough buckets on other pages. */
    if ((n == 0) && ((free_list[bucket].count - per_page) >= (per_page / 2))) {
      /* remove same-page elements from free list, then free page */
      int i;
      long sz;
      void *pg;

      sz = page_size - size2;
      pg = CODE_PAGE_OF(p);
      for (i = CODE_HEADER_SIZE; i <= sz; i += size2) {
        p = ((char *)pg) + i;
        prev = ((void **)p)[1];
        if (prev)
          ((void **)prev)[0] = ((void **)p)[0];
        else
          free_list[bucket].elems = ((void **)p)[0];
        prev = ((void **)p)[0];
        if (prev)
          ((void **)prev)[1] = ((void **)p)[1];
        --free_list[bucket].count;
      }

      scheme_code_page_total -= page_size;
      LOG_CODE_MALLOC(2, printf("freeing page at %p [%ld left]\n", 
            CODE_PAGE_OF(p), scheme_code_page_total));
      free_page(CODE_PAGE_OF(p), page_size);
    }
  }
# ifdef MZ_USE_PLACES
  mzrt_mutex_unlock(free_list_mutex);
# endif

#else
  free(p);
#endif
}

#ifndef MZ_PRECISE_GC

/* When using the CGC allocator, we know how GCable memory is
   allocated, and we expect mprotect(), etc., to work on it. The JIT
   currently takes advantage of that combination, so we support it
   with scheme_malloc_gcable_code() --- but only in CGC mode. */

#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)
static unsigned long jit_prev_page = 0, jit_prev_length = 0;
#endif

void *scheme_malloc_gcable_code(long size)
{
  void *p;
  p = scheme_malloc(size);
  
#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)
  {
    /* [This chunk of code moved from our copy of GNU lightning to here.] */
    unsigned long page, length, page_size;
    void *end;

    page_size = get_page_size();
    
    end = ((char *)p) + size;

    page = (long) p & ~(page_size - 1);
    length = ((char *) end - (char *) page + page_size - 1) & ~(page_size - 1);
    
    /* Simple-minded attempt at optimizing the common case where a single
       chunk of memory is used to compile multiple functions.  */
    if (!(page >= jit_prev_page && page + length <= jit_prev_page + jit_prev_length)) {
      
# ifdef MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC
      {
        DWORD old;
        VirtualProtect((void *)page, length, PAGE_EXECUTE_READWRITE, &old);
      }
# else
      {
        int r;
        r = mprotect ((void *) page, length, PROT_READ | PROT_WRITE | PROT_EXEC);
        if (r == -1) {
          scheme_log_abort("mprotect for generate-code page failed; aborting");
        }
      }
# endif

      /* See if we can extend the previously mprotect'ed memory area towards
         higher addresses: the starting address remains the same as before.  */
      if (page >= jit_prev_page && page <= jit_prev_page + jit_prev_length)
        jit_prev_length = page + length - jit_prev_page;
      
      /* See if we can extend the previously mprotect'ed memory area towards
         lower addresses: the highest address remains the same as before. */
      else if (page < jit_prev_page && page + length >= jit_prev_page 
               && page + length <= jit_prev_page + jit_prev_length)
        jit_prev_length += jit_prev_page - page, jit_prev_page = page;
      
      /* Nothing to do, replace the area.  */
      else
        jit_prev_page = page, jit_prev_length = length;
    }
  }
#endif

  return p;
}

void scheme_notify_code_gc()
{
#if defined(MZ_JIT_USE_MPROTECT) || defined(MZ_JIT_USE_WINDOWS_VIRTUAL_ALLOC)
  jit_prev_page = 0;
  jit_prev_length = 0;
#endif
}
#endif

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

/************************************************************************/
/*                           finalization                               */
/************************************************************************/

typedef struct Finalization {
  MZTAG_IF_REQUIRED
  void (*f)(void *o, void *data);
  void *data;
  struct Finalization *next, *prev;
} Finalization;

typedef struct Finalizations {
  MZTAG_IF_REQUIRED
  short lifetime;
  Finalization *scheme_first, *scheme_last;
  void (*ext_f)(void *o, void *data);
  void *ext_data;
  Finalization *prim_first, *prim_last;
} Finalizations;

#ifdef MZ_PRECISE_GC

#include "../gc2/gc2_dump.h"

START_XFORM_SKIP;

#define MARKS_FOR_SALLOC_C
#include "mzmark.c"

END_XFORM_SKIP;

#define GC_register_eager_finalizer(o, level, f, d, of, od) GC_set_finalizer(o, 1, level, f, d, of, od)
#define GC_register_finalizer(o, f, d, of, od) GC_set_finalizer(o, 1, 3, f, d, of, od)

#endif

THREAD_LOCAL_DECL(static int current_lifetime);

void scheme_reset_finalizations(void)
{
  current_lifetime++;
}

static void do_next_finalization(void *o, void *data)
{
  Finalizations *fns = *(Finalizations **)data;
  Finalization *fn;

  if (fns->lifetime != current_lifetime)
    return;

  if (fns->scheme_first) {
    if (fns->scheme_first->next || fns->ext_f || fns->prim_first) {
      /* Re-install low-level finalizer and run a scheme finalizer */
      GC_register_eager_finalizer(o, fns->scheme_first->next ? 1 : 2, 
				  do_next_finalization, data, NULL, NULL);
    }

    fn = fns->scheme_first;
    fns->scheme_first = fn->next;
    if (!fn->next)
      fns->scheme_last = NULL;
    else
      fn->next->prev = NULL;

    fn->f(o, fn->data);
    return;
  }

  if (fns->ext_f)
    fns->ext_f(o, fns->ext_data);

  for (fn = fns->prim_first; fn; fn = fn->next) {
    fn->f(o, fn->data);
  }
}

/* Makes gc2 xformer happy: */
typedef void (*finalizer_function)(void *p, void *data);
THREAD_LOCAL_DECL(static int traversers_registered);
THREAD_LOCAL_DECL(static Finalizations **save_fns_ptr);

static void add_finalizer(void *v, void (*f)(void*,void*), void *data, 
			  int prim, int ext,
			  void (**ext_oldf)(void *p, void *data),
			  void **ext_olddata,
			  int no_dup, int rmve)
{
  finalizer_function oldf;
  void *olddata;
  Finalizations *fns, **fns_ptr, *prealloced;
  Finalization *fn;

  if (!traversers_registered) {
#ifdef MZ_PRECISE_GC
    GC_REG_TRAV(scheme_rt_finalization, mark_finalization);
    GC_REG_TRAV(scheme_rt_finalizations, mark_finalizations);
    traversers_registered = 1;
#endif
    REGISTER_SO(save_fns_ptr);
  }

#ifndef MZ_PRECISE_GC
  if (v != GC_base(v))
    return;
#endif

  /* Allocate everything first so that we're not changing
     finalizations when finalizations could run: */

  if (save_fns_ptr) {
    fns_ptr = save_fns_ptr;
    save_fns_ptr = NULL;
  } else
    fns_ptr = MALLOC_ONE(Finalizations*);

  if (!ext && !rmve) {
    fn = MALLOC_ONE_RT(Finalization);
#ifdef MZTAG_REQUIRED
    fn->type = scheme_rt_finalization;
#endif
    fn->f = f;
    fn->data = data;
  } else
    fn = NULL;

  if (!rmve) {
    prealloced = MALLOC_ONE_RT(Finalizations); /* may not need this... */
#ifdef MZTAG_REQUIRED
    prealloced->type = scheme_rt_finalizations;
#endif
  } else
    prealloced = NULL;

  GC_register_eager_finalizer(v, prim ? 2 : 1, do_next_finalization, fns_ptr, &oldf, &olddata);

  if (oldf) {
    if (oldf != do_next_finalization) {
      /* This happens if an extenal use of GC_ routines conflicts with us. */
      scheme_warning("warning: non-Racket finalization on object dropped!");
    } else {
      *fns_ptr = *(Finalizations **)olddata;
      save_fns_ptr = (Finalizations **)olddata;
      *save_fns_ptr = NULL;
    }
  } else if (rmve) {
    GC_register_finalizer(v, NULL, NULL, NULL, NULL);
    save_fns_ptr = fns_ptr;
    return;
  }
  
  if (!(*fns_ptr)) {
    prealloced->lifetime = current_lifetime;
    *fns_ptr = prealloced;
  }
  fns = *fns_ptr;

  if (ext) {
    if (ext_oldf)
      *ext_oldf = fns->ext_f;
    fns->ext_f = f;
    if (ext_olddata)
      *ext_olddata = fns->ext_data;
    fns->ext_data = data;

    if (!f && !fns->prim_first && !fns->scheme_first) {
      /* Removed all finalization */
      GC_register_finalizer(v, NULL, NULL, NULL, NULL);
      save_fns_ptr = fns_ptr;
      *save_fns_ptr = NULL;
    }
  } else {
    if (prim) {
      if (no_dup) {
	/* Make sure it's not already here */
	Finalization *fnx;
	for (fnx = fns->prim_first; fnx; fnx = fnx->next) {
	  if (fnx->f == f && fnx->data == data) {
	    if (rmve) {
	      if (fnx->prev)
		fnx->prev->next = fnx->next;
	      else
		fns->prim_first = fnx->next;
	      if (fnx->next)
		fnx->next->prev = fnx->prev;
	      else
		fns->prim_last = fnx->prev;
	    }
	    fn = NULL;
	    break;
	  }
	}
      }
      if (fn) {
	fn->next = fns->prim_first;
	fns->prim_first = fn;
	if (!fn->next)
	  fns->prim_last = fn;
	else
	  fn->next->prev = fn;
      }
      /* Removed all finalization? */
      if (!fns->ext_f && !fns->prim_first && !fns->scheme_first) {
	GC_register_finalizer(v, NULL, NULL, NULL, NULL);
	save_fns_ptr = fns_ptr;
	*save_fns_ptr = NULL;
      }
    } else {
      fn->next = fns->scheme_first;
      fns->scheme_first = fn;
      if (!fn->next)
	fns->scheme_last = fn;
      else
	fn->next->prev = fn;
    }
  }
}

#ifndef MZ_PRECISE_GC
void scheme_weak_reference(void **p)
{
  scheme_weak_reference_indirect(p, *p);
}

void scheme_weak_reference_indirect(void **p, void *v)
{
  if (GC_base(v) == v)
    GC_register_late_disappearing_link(p, v);
}

void scheme_unweak_reference(void **p)
{
  GC_unregister_disappearing_link(p);
}
#endif

void scheme_add_finalizer(void *p, void (*f)(void *p, void *data), void *data)
{
  add_finalizer(p, f, data, 1, 0, NULL, NULL, 0, 0);
}

void scheme_add_finalizer_once(void *p, void (*f)(void *p, void *data), void *data)
{
  add_finalizer(p, f, data, 1, 0, NULL, NULL, 1, 0);
}

void scheme_subtract_finalizer(void *p, void (*f)(void *p, void *data), void *data)
{
  add_finalizer(p, f, data, 1, 0, NULL, NULL, 1, 1);
}

void scheme_add_scheme_finalizer(void *p, void (*f)(void *p, void *data), void *data)
{
  add_finalizer(p, f, data, 0, 0, NULL, NULL, 0, 0);
}

void scheme_add_scheme_finalizer_once(void *p, void (*f)(void *p, void *data), void *data)
{
  add_finalizer(p, f, data, 0, 0, NULL, NULL, 1, 0);
}

void scheme_register_finalizer(void *p, void (*f)(void *p, void *data), 
			       void *data, void (**oldf)(void *p, void *data), 
			       void **olddata)
{
  add_finalizer(p, f, data, 0, 1, oldf, olddata, 0, 0);
}

void scheme_remove_all_finalization(void *p)
{
  GC_register_finalizer(p, NULL, NULL, NULL, NULL);
}

void scheme_collect_garbage(void)
{
  GC_gcollect();
}

unsigned long scheme_get_deeper_address(void)
{
  int v, *vp;
  vp = &v;
  return (unsigned long)vp;
}

/************************************************************************/
/*                             GC_dump                                  */
/************************************************************************/

#ifndef MZ_PRECISE_GC
# ifdef __cplusplus
extern "C" 
{
# endif
  extern MZ_DLLIMPORT void GC_dump(void);
# ifdef __cplusplus
};
# endif
#endif

#ifdef USE_TAGGED_ALLOCATION
#define NUM_TYPE_SLOTS (_scheme_last_type_ + 5) /* extra space for externally defined */

static long scheme_memory_count[NUM_TYPE_SLOTS];
static long scheme_memory_actual_count[NUM_TYPE_SLOTS];
static long scheme_memory_size[NUM_TYPE_SLOTS];
static long scheme_memory_actual_size[NUM_TYPE_SLOTS];
static unsigned long scheme_memory_hi[NUM_TYPE_SLOTS];
static unsigned long scheme_memory_lo[NUM_TYPE_SLOTS];
static long scheme_envunbox_count, scheme_envunbox_size;
static long bad_seeds;
static Scheme_Hash_Table *smc_ht;
static int trace_path_type;

# define OBJ_BUFFER_SIZE 1048576
static void *obj_buffer[OBJ_BUFFER_SIZE];
static int obj_buffer_pos;
static int obj_type;

# define NUM_RECORDED_APP_SIZES 5
static int app_sizes[NUM_RECORDED_APP_SIZES+1];
static int app_arg_kinds[NUM_RECORDED_APP_SIZES][NUM_RECORDED_APP_SIZES][5];

void count_tagged(void *p, int size, void *data)
{
  int which = SCHEME_TYPE((Scheme_Object *)p);
  if ((which >= 0) && (which < _scheme_last_type_)) {
    scheme_count_memory((Scheme_Object *)p, smc_ht);
  } else if (which >= scheme_num_types())
    bad_seeds++;
  else {
    if (which >= NUM_TYPE_SLOTS)
      which = NUM_TYPE_SLOTS - 1;
    scheme_memory_count[which]++;
    scheme_memory_size[which] += size;
  }

  if (which == obj_type) {
    if (obj_buffer_pos < OBJ_BUFFER_SIZE) {
      obj_buffer[obj_buffer_pos++] = p;
    }
  }

  if (which == scheme_application_type) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)p;
    int cnt;
    cnt = app->num_args;
    if (cnt >= NUM_RECORDED_APP_SIZES) {
      cnt = NUM_RECORDED_APP_SIZES;
    } else {
      int i, devals, kind;
      devals = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *));
      for (i = 0; i <= cnt; i++) {
	kind = ((char *)app + devals)[i];
	if ((kind >= 0) && (kind <= 4)) {
	  app_arg_kinds[cnt][i][kind]++;
	}
      }
    }
    app_sizes[cnt]++;
  }
}

void count_envunbox(void *p, int size, void *data)
{
  scheme_envunbox_count++;
  scheme_envunbox_size += size;
}

static void trace_count(void *p, int size)
{
  int which = SCHEME_TYPE((Scheme_Object *)p);
  if ((which >= 0) && (which <= _scheme_last_type_)) {
   /* fall through to below */ 
  } else if (which >= scheme_num_types()) {
    bad_seeds++;
    return;
  } else {
    if (which >= NUM_TYPE_SLOTS)
      which = NUM_TYPE_SLOTS - 1;
   /* fall through to below */ 
  }

  {
    unsigned long s = (unsigned long)p;
    scheme_memory_actual_count[which]++;
    scheme_memory_actual_size[which] += size;
    if (!scheme_memory_lo[which] || (s < scheme_memory_lo[which]))
      scheme_memory_lo[which] = s;
    if (!scheme_memory_hi[which] || (s > scheme_memory_hi[which]))
      scheme_memory_hi[which] = s;
  }
}

static void trace_stack_count(void *p, int size)
{
  /* Do nothing */
}

static void trace_path(void *p, unsigned long src, void *path_data)
{
  if ((trace_path_type > -1)
      && ((int)SCHEME_TYPE((Scheme_Object *)p) == trace_path_type))
    GC_store_path(p, src, path_data);
}

static void trace_stack_path(void *p, unsigned long src, void *path_data)
{
  if (trace_path_type == -2)
    GC_store_path(p, src, path_data);
}

static void trace_init(void)
{
  /* do nothing */
}

static void trace_done(void)
{
  /* do nothing */
}

static void finalize_object(void *p)
{
  ((Scheme_Object *)p)->type = _scheme_values_types_;
}

static Scheme_Object *local_thread;
static size_t local_thread_size;

static int skip_foreign_thread(void *p, size_t size)
{
  if (p == local_thread)
    local_thread_size = size;
  else if (size == local_thread_size) {
    if ((*(Scheme_Type *)p) == scheme_thread_type) {
      /* Has tag and right size; let's assume that it's really a thread. */
      Scheme_Custodian *local, *here;
      
      local = *((Scheme_Thread *)local_thread)->mref;
      here = *((Scheme_Thread *)p)->mref;

      /* If p belongs to the local thread's custodian, we'll see the
	 local thread's custodian while walking up from here: */
      while (here) {
	if (here == local)
	  return 0;
	if (here->parent)
	  here = *here->parent;
	else
	  here = NULL;
      }

      /* Must be a foreign thread: */
      return 1;
    }
  }

  return 0;
}

#endif

HOOK_SHARED_OK void (*scheme_external_dump_info)(void);
HOOK_SHARED_OK void (*scheme_external_dump_arg)(Scheme_Object *arg);
HOOK_SHARED_OK char *(*scheme_external_dump_type)(void *v);

#ifdef USE_TAGGED_ALLOCATION
static void count_managed(Scheme_Custodian *m, int *c, int *a, int *u, int *t,
			  int *ipt, int *opt, int *th)
{
  int i;

  *t += 1;
  *c += m->count;
  *a += m->alloc;
  for (i = m->count; i--; ) {
    if (m->boxes[i]) {
      Scheme_Object *o = (*(m->boxes[i]));
      (*u)++;
      if (SCHEME_THREADP(o))
	(*th)++;
      else if (SCHEME_INPORTP(o))
	(*ipt)++;
      else if (SCHEME_OUTPORTP(o))
	(*opt)++;
    }
  }

  if (*m->sibling)
    count_managed(*m->sibling, c, a, u, t, ipt, opt, th);
  if (*m->children)
    count_managed(*m->children, c, a, u, t, ipt, opt, th);
}
#endif

#if defined(MZ_PRECISE_GC)
# ifdef MZ_GC_BACKTRACE
#  define MZ_PRECISE_GC_TRACE 1
# else
#  define MZ_PRECISE_GC_TRACE 0
# endif
#else
# define MZ_PRECISE_GC_TRACE 0
#endif

#if MZ_PRECISE_GC_TRACE
char *(*GC_get_xtagged_name)(void *p) = NULL;
static Scheme_Object *cons_accum_result;
static void cons_onto_list(void *p)
{
  cons_accum_result = scheme_make_pair((Scheme_Object *)p, cons_accum_result);
}
#endif

#if defined(USE_TAGGED_ALLOCATION) || MZ_PRECISE_GC_TRACE

# ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#  ifdef DOS_FILE_SYSTEM
extern void gc_fprintf(FILE *ignored, const char *c, ...);
#   define object_console_printf gc_fprintf
#  endif
# endif

#ifndef object_console_printf
# define object_console_printf fprintf
#endif

extern int (*scheme_check_print_is_obj)(Scheme_Object *o);
static int check_home(Scheme_Object *o)
{
#ifdef MZ_PRECISE_GC
  return (SCHEME_INTP(o) || GC_is_tagged(o) 
	  || SAME_OBJ(o, scheme_true) 
	  || SAME_OBJ(o, scheme_false)
	  || SAME_OBJ(o, scheme_null)
	  || SAME_OBJ(o, scheme_eof)
	  || SAME_OBJ(o, scheme_void));
#else
  /* GC_set(o) */
  return 1;
#endif
}

static void print_tagged_value(const char *prefix, 
			       void *v, int xtagged, unsigned long diff, int max_w,
			       const char *suffix)
{
  char buffer[256];
  char *type, *sep, diffstr[30];
  long len;
  
  sep = "";
  
  scheme_check_print_is_obj = check_home;

  if (!xtagged) {
    type = scheme_write_to_string_w_max((Scheme_Object *)v, &len, max_w);
    if (!scheme_strncmp(type, "#<thread", 8) 
	&& ((type[8] == '>') || (type[8] == ':'))) {
      char *run, *sus, *kill, *clean, *deq, *all, *t2;
      int state = ((Scheme_Thread *)v)->running, len2;
	    
      run = (state & MZTHREAD_RUNNING) ? "+run" : "";
      sus = (state & MZTHREAD_SUSPENDED) ? "+suspended" : "";
      kill = (state & MZTHREAD_KILLED) ? "+killed" : "";
      clean = (state & MZTHREAD_NEED_KILL_CLEANUP) ? "+cleanup" : "";
      deq = (((Scheme_Thread *)v)->next || ((Scheme_Thread *)v)->prev) ? "" : "+deq";
      all = !state ? "defunct" : "";

      sprintf(buffer, "[%d=%s%s%s%s%s%s]",
	      state, run, sus, kill, clean, all, deq);

      len2 = strlen(buffer);
      t2 = (char *)scheme_malloc_atomic(len + len2 + 1);
      memcpy(t2, type, len);
      memcpy(t2 + len, buffer, len2 + 1);
      len += len2;
      type = t2;
    } else if (!scheme_strncmp(type, "#<continuation>", 15)) {
      char *t2;
      int len2;
	    
      sprintf(buffer, "[%s%.100s]",
              (((Scheme_Cont *)v)->composable 
               ? "delim;"
               : ""),
              (((Scheme_Cont *)v)->prompt_tag
               ? (SCHEME_CDR(((Scheme_Cont *)v)->prompt_tag)
                  ? SCHEME_SYM_VAL(SCHEME_CDR(((Scheme_Cont *)v)->prompt_tag))
                  : "<anonymous>")
               : "NULL"));
      
      len2 = strlen(buffer);
      t2 = (char *)scheme_malloc_atomic(len + len2 + 1);
      memcpy(t2, type, len);
      memcpy(t2 + len, buffer, len2 + 1);
      len += len2;
      type = t2;
    } else if (!scheme_strncmp(type, "#<custodian>", 13)) {
      char *t2;
      int len2;

      sprintf(buffer, "[%d]",
              ((Scheme_Custodian *)v)->elems);

      len2 = strlen(buffer);
      t2 = (char *)scheme_malloc_atomic(len + len2 + 1);
      memcpy(t2, type, len);
      memcpy(t2 + len, buffer, len2 + 1);
      len += len2;
      type = t2;      
    } else if (!scheme_strncmp(type, "#<namespace", 11)) {
      char *t2;
      int len2;
	    
      sprintf(buffer, "[%ld/%ld:%.100s]",
	      ((Scheme_Env *)v)->phase,
              ((Scheme_Env *)v)->mod_phase,
	      (((Scheme_Env *)v)->module
	       ? scheme_write_to_string(((Scheme_Env *)v)->module->modname, NULL)
	       : "(toplevel)"));
	    
      len2 = strlen(buffer);
      t2 = (char *)scheme_malloc_atomic(len + len2 + 1);
      memcpy(t2, type, len);
      memcpy(t2 + len, buffer, len2 + 1);
      len += len2;
      type = t2;
    } else if (!scheme_strncmp(type, "#<global-variable-code", 22)) {
      Scheme_Bucket *b = (Scheme_Bucket *)v;
      Scheme_Object *bsym = (Scheme_Object *)b->key;
      char *t2;
      int len2;

      len2 = SCHEME_SYM_LEN(bsym);
      t2 = scheme_malloc_atomic(len + len2 + 3);
      memcpy(t2, type, len);
      memcpy(t2 + len + 1, SCHEME_SYM_VAL(bsym), len2);
      t2[len] = '[';
      t2[len + 1 + len2] = ']';
      t2[len + 1 + len2 + 1] = 0;
      len += len2;
      type = t2;
    } else if (!scheme_strncmp(type, "#<hash-table>", 13)
	       || !scheme_strncmp(type, "#<hash-table:", 13)) {
      char *t2;
      int len2;
      int htype, size, count;

      if (SCHEME_HASHTP((Scheme_Object *)v)) {
	htype = 'n';
	size = ((Scheme_Hash_Table *)v)->size;
	count = ((Scheme_Hash_Table *)v)->count;
      } else {
	htype = 'b';
	size = ((Scheme_Bucket_Table *)v)->size;
	count = ((Scheme_Bucket_Table *)v)->count;
      }
      
      sprintf(buffer, "[%c:%d:%d]", htype, count, size);

      len2 = strlen(buffer);
      t2 = scheme_malloc_atomic(len + len2 + 1);
      memcpy(t2, type, len);
      memcpy(t2 + len, buffer, len2 + 1);
      len += len2;
      type = t2;
    } else if (!scheme_strncmp(type, "#<syntax-code", 13)) {
      char *t2, *t3;
      long len2, len3;

      t2 = scheme_write_to_string_w_max(SCHEME_IPTR_VAL(v), &len2, 32);
      
      len3 = len + len2 + 2 + 2;
      t3 = (char *)scheme_malloc_atomic(len3);
      memcpy(t3, type, len);
      t3[len] = (SCHEME_PINT_VAL(v) / 10) + '0';
      t3[len + 1] = (SCHEME_PINT_VAL(v) % 10) + '0';
      t3[len + 2] = '=';
      memcpy(t3 + len + 3, t2, len2);
      t3[len + len2 + 3] = 0;
      type = t3;
      len = len3;
#ifdef MZTAG_REQUIRED
    } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_rt_meta_cont)) {
      Scheme_Meta_Continuation *mc = (Scheme_Meta_Continuation *)v;
      Scheme_Object *pt;
      long len2, len3;
      char *t2, *t3;

      pt = mc->prompt_tag;
      if (pt) {
        t3 = scheme_write_to_string_w_max(pt, &len3, max_w);
      } else {
        t3 = "#f";
        len3 = 2;
      }

      len2 = 32 + len3;
      t2 = (char *)scheme_malloc_atomic(len2);
      sprintf(t2, "#<meta-continuation>[%d;%s]", mc->pseudo, t3);
      type = t2;
      len = strlen(t2);
    } else if (SAME_TYPE(SCHEME_TYPE(v), scheme_rt_compact_port)) {
      
#endif
    } else if (!scheme_strncmp(type, "#<syntax", 8)) {
      char *t2, *t3;
      long len2, len3;

      t2 = scheme_write_to_string_w_max(SCHEME_STX_VAL(v), &len2, 32);
      
      len3 = len + len2 + 2;
      t3 = (char *)scheme_malloc_atomic(len3);
      memcpy(t3, type, len);
      t3[len] = '=';
      memcpy(t3 + len + 1, t2, len2);
      t3[len + len2 + 1] = 0;
      type = t3;
      len = len3;
    }

    sep = "=";
  } else if (scheme_external_dump_type) {
    type = scheme_external_dump_type(v);
    if (*type)
      sep = ":";
  } else
    type = "";
  
  if (diff)
    sprintf(diffstr, "%lx", diff);
  
  object_console_printf(stderr,
			"%s%p%s%s%s%s%s", 
			prefix,
			v, 
			sep,
			type,
			diff ? "+" : "",
			diff ? diffstr : "",
			suffix);

  scheme_check_print_is_obj = NULL;
}
# ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
# endif
#endif

Scheme_Object *scheme_dump_gc_stats(int c, Scheme_Object *p[])
{
  Scheme_Object *result = scheme_void;
#ifdef USE_TAGGED_ALLOCATION
  void *initial_trace_root = NULL;
  int (*inital_root_skip)(void *, size_t) = NULL;
#endif
#if MZ_PRECISE_GC_TRACE
  int trace_for_tag = 0;
  int flags = 0;
  int path_length_limit = 1000;
  GC_for_each_found_proc for_each_found = NULL;
#else
# ifndef USE_TAGGED_ALLOCATION
#  define flags 0
#  define trace_for_tag 0
#  define path_length_limit 1000
#  define for_each_found NULL
#  define GC_get_xtagged_name NULL
#  define print_tagged_value NULL
# endif
#endif

#if 0
  /* Syntax-object debugging support: */
  if ((c == 1) && SCHEME_STXP(p[0])) {
    return scheme_explode_syntax(p[0], scheme_make_hash_table(SCHEME_hash_ptr));
  }

  if (c && SAME_TYPE(SCHEME_TYPE(p[0]), scheme_compilation_top_type)) {
    Scheme_Hash_Table *ht;
    Scheme_Compilation_Top *top;
    Scheme_Object *vec, *v, *lst = scheme_null;
    Scheme_Module *m;
    Resolve_Prefix *prefix;
    int i, j;

    ht = scheme_make_hash_table(SCHEME_hash_ptr);

    top = (Scheme_Compilation_Top *)p[0];

    j = 0;
    while (1) {
      if (j)
        m = scheme_extract_compiled_module(p[0]);
      else
        m = NULL;

      if (m) {
        if (j == 1) {
          prefix = m->prefix;
        } else {
          int k = j - 2;
          if (k >= SCHEME_VEC_SIZE(m->et_body))
            break;
          v = SCHEME_VEC_ELS(m->et_body)[k];
          prefix = (Resolve_Prefix *)SCHEME_VEC_ELS(v)[3];
        }
      } else {
        if (j)
          break;
        prefix = top->prefix;
      }
      
      vec = scheme_make_vector(prefix->num_stxes, NULL);
      for (i = 0; i < prefix->num_stxes; i++) {
        v = scheme_explode_syntax(prefix->stxes[i], ht);
        SCHEME_VEC_ELS(vec)[i] = v;
      }

      lst = scheme_make_pair(vec, lst);
      j++;
    }

    return scheme_reverse(lst);
  }
#endif

  scheme_start_atomic();

  if (scheme_external_dump_arg)
    scheme_external_dump_arg(c ? p[0] : NULL);

  scheme_console_printf("Begin Dump\n");

#ifdef USE_TAGGED_ALLOCATION
  trace_path_type = -1;
  obj_type = -1;
  if (c && SCHEME_SYMBOLP(p[0])) {
    Scheme_Object *sym;
    char *s;
    int i, maxpos, just_objects;

    sym = p[0];
    s = scheme_symbol_val(sym);

    maxpos = scheme_num_types();
    if (maxpos > NUM_TYPE_SLOTS-1)
      maxpos = NUM_TYPE_SLOTS-1;

    just_objects = ((c > 1)
		    && SCHEME_SYMBOLP(p[1])
		    && !strcmp(SCHEME_SYM_VAL(p[1]), "objects"));

    for (i = 0; i < maxpos; i++) {
      void *tn = scheme_get_type_name(i);
      if (tn && !strcmp(tn, s)) {
	if (just_objects)
	  obj_type = i;
	else
	  trace_path_type = i;
	break;
      }
    }
    if (SAME_OBJ(p[0], scheme_intern_symbol("stack"))) {
      trace_path_type = -2;
    }

    if ((c > 2)
	&& SCHEME_SYMBOLP(p[1])
	&& !strcmp(SCHEME_SYM_VAL(p[1]), "from")) {
      initial_trace_root = p[2];
      if (SCHEME_THREADP(p[2])) {
	local_thread = p[2];
	local_thread_size = 0;
	inital_root_skip = skip_foreign_thread;
      }
    }
  }

  {
    int i;
    int stack_c, roots_c, uncollectable_c, final_c;
    long total_count = 0, total_size = 0;
    long total_actual_count = 0, total_actual_size = 0;
    long traced;
    int no_walk = 0;

    no_walk = 1 /* (!c || !SAME_OBJ(p[0], scheme_true)) */;
    
    for (i = 0; i < NUM_TYPE_SLOTS; i++) {
      scheme_memory_count[i] = scheme_memory_size[i] = 0;
      scheme_memory_actual_size[i] = scheme_memory_actual_count[i] = 0;
      scheme_memory_hi[i] = scheme_memory_lo[i] = 0;
    }
    scheme_envunbox_count = scheme_envunbox_size = 0;
    bad_seeds = 0;
    for (i = 0; i <= NUM_RECORDED_APP_SIZES; i++) {
      app_sizes[i] = 0;
    }
    {
      int j, k;
      for (i = 0; i < NUM_RECORDED_APP_SIZES; i++) {
	for (j = 0; j <= i; j++) {
	  for (k = 0; k <= 4; k++) {
	    app_arg_kinds[i][j][k] = 0;
	  }
	}
      }
    }

    traced = GC_trace_count(&stack_c, &roots_c, &uncollectable_c, &final_c);
    GC_dump();

    scheme_console_printf("\ntraced: %ld\n", traced);

    tagged = tagged_while_counting;
    
    if (!no_walk)
      smc_ht = scheme_make_hash_table(SCHEME_hash_ptr);
    
    if (tagged) 
      GC_for_each_element(real_tagged, count_tagged, NULL);
    if (tagged_eternal) 
      GC_for_each_element(tagged_eternal, count_tagged, NULL);
    if (tagged_uncollectable) 
      GC_for_each_element(tagged_uncollectable, count_tagged, NULL);
    if (tagged_atomic)
      GC_for_each_element(tagged_atomic, count_tagged, NULL);
    if (envunbox)
      GC_for_each_element(envunbox, count_envunbox, NULL);

    tagged = real_tagged;

    scheme_console_printf("Begin Racket\n");
    scheme_console_printf("%30.30s %10s %10s %10s %8s - %8s\n",
			  "TYPE", "COUNT", "ESTM-SIZE", "TRACE-SIZE", 
			  "LO-LOC", "HI-LOC");
    for (i = 0; i < NUM_TYPE_SLOTS; i++) {
      if (scheme_memory_count[i] || scheme_memory_actual_count[i]) {
	scheme_console_printf("%30.30s %10ld %10ld %10ld %8lx - %8lx\n",
			      (i < NUM_TYPE_SLOTS-1)
			      ? scheme_get_type_name(i)
			      : "other",
			      scheme_memory_actual_count[i],
			      scheme_memory_size[i],
			      scheme_memory_actual_size[i],
			      scheme_memory_lo[i],
			      scheme_memory_hi[i]);
	if (scheme_memory_actual_count[i] != scheme_memory_count[i]) {
	  scheme_console_printf("%30.30s reach count: %10ld\n",
				"", scheme_memory_count[i]);
	}
	total_count += scheme_memory_count[i];
	total_size += scheme_memory_size[i];
	total_actual_count += scheme_memory_actual_count[i];
	total_actual_size += scheme_memory_actual_size[i];
      }
    }

    scheme_console_printf("%30.30s %10ld %10ld          -\n",
			  "envunbox", scheme_envunbox_count, scheme_envunbox_size);
    total_count += scheme_envunbox_count;
    total_size += scheme_envunbox_size;

    scheme_console_printf("%30.30s          - %10ld          -\n",
			  "miscellaneous", 
			  scheme_misc_count + scheme_type_table_count);
    total_size += scheme_misc_count + scheme_type_table_count;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "roots", roots_c);
    total_actual_size += roots_c;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "stack", stack_c);
    total_actual_size += stack_c;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "unreached-uncollectable", uncollectable_c);
    total_actual_size += uncollectable_c;

    scheme_console_printf("%30.30s          -          - %10ld\n",
			  "finalization", final_c);
    total_actual_size += final_c;

    scheme_console_printf("%30.30s %10ld %10ld %10ld\n",
			  "total", total_count, total_size, 
			  total_actual_size);
    scheme_console_printf("End Racket\n");

    scheme_console_printf("Begin Apps\n");
    for (i = 0; i < NUM_RECORDED_APP_SIZES; i++) {
      int j, k;
      scheme_console_printf("  %d%s: %d", i, 
			    (i == NUM_RECORDED_APP_SIZES ? "+" : ""), 
			    app_sizes[i]);
      for (j = 0; j <= i; j++) {
	scheme_console_printf(" (");
	for (k = 0; k <= 4; k++) {
	  if (k)
	    scheme_console_printf(",");
	  scheme_console_printf("%d", app_arg_kinds[i][j][k]);
	}
	scheme_console_printf(")");
      }
      scheme_console_printf("\n");
    }
    scheme_console_printf("End Apps\n");

    {
      Scheme_Custodian *m = (Scheme_Custodian *)scheme_get_param(scheme_current_config(), MZCONFIG_CUSTODIAN);
      int c = 0, a = 0, u = 0, t = 0, ipt = 0, opt = 0, th = 0;

      while (*m->parent)
	m = *m->parent;

      count_managed(m, &c, &a, &u, &t, &ipt, &opt, &th);

      scheme_console_printf("custodians: %d  managed: actual: %d   breadth: %d   room: %d\n"
			    "                        input-ports: %d  output-ports: %d  threads: %d\n"
			    "stacks: %d\n", 
			    t, u, c, a, ipt, opt, th,
			    scheme_num_copied_stacks);
    }

    if (bad_seeds)
      scheme_console_printf("ERROR: %ld illegal tags found\n", bad_seeds);

    smc_ht = NULL;
  }

#else

# if MZ_PRECISE_GC_TRACE
  cons_accum_result = scheme_void;
  if (c && SCHEME_SYMBOLP(p[0])) {
    Scheme_Object *sym;
    char *s;
    int i, maxpos;

    sym = p[0];
    s = scheme_symbol_val(sym);

    maxpos = scheme_num_types();

    for (i = 0; i < maxpos; i++) {
      void *tn;
      tn = scheme_get_type_name(i);
      if (tn && !strcmp(tn, s)) {
	trace_for_tag = i;
	flags |= GC_DUMP_SHOW_TRACE;
	break;
      }
    }

    if (!strcmp("fnl", s))
      flags |= GC_DUMP_SHOW_FINALS;

    if (!strcmp("peek", s) && (c == 3)) {
      long n;
      scheme_end_atomic();
      if (scheme_get_int_val(p[1], &n)) {
	if (GC_is_tagged_start((void *)n)) {
	  return (Scheme_Object *)n;
	} else
	  return p[2];
      }
    }
    
    if (!strcmp("next", s) && (c == 2)) {
      void *pt;
      scheme_end_atomic();
      if (SCHEME_FALSEP(p[1]))
	pt = GC_next_tagged_start(NULL);
      else
	pt = GC_next_tagged_start((void *)p[1]);
      if (pt)
	return (Scheme_Object *)pt;
      else
	return scheme_false;
    }

    if (!strcmp("addr", s) && (c == 2)) {
      scheme_end_atomic();      
      return scheme_make_integer_value((long)p[1]);
    }
  } else if (c && SCHEME_INTP(p[0])) {
    trace_for_tag = SCHEME_INT_VAL(p[0]);
    flags |= GC_DUMP_SHOW_TRACE;
  } else if (c && SCHEME_THREADP(p[0])) {
    Scheme_Thread *t = (Scheme_Thread *)p[0];
    void **var_stack, *limit;
    long delta;

    scheme_console_printf("Thread: %p\n", t);
    if (t->running) {
      if (scheme_current_thread == t) {
        scheme_console_printf(" swapped in\n");
        var_stack = GC_variable_stack;
        delta = 0;
        limit = (void *)scheme_get_current_thread_stack_start();
      } else {
        scheme_console_printf(" swapped out\n");
        var_stack = (void **)t->jmpup_buf.gc_var_stack;
        delta = (long)t->jmpup_buf.stack_copy - (long)t->jmpup_buf.stack_from;
        /* FIXME: stack direction */
        limit = (char *)t->jmpup_buf.stack_copy + t->jmpup_buf.stack_size;
      }
      GC_dump_variable_stack(var_stack, delta, limit, NULL,
                             scheme_get_type_name,
                             GC_get_xtagged_name,
                             print_tagged_value);
    } else {
      scheme_console_printf(" done\n");
    }
    scheme_end_atomic();
    return scheme_void;
  }

  if ((c > 1) && SCHEME_INTP(p[1]))
    path_length_limit = SCHEME_INT_VAL(p[1]);
  else if ((c > 1) && SCHEME_SYMBOLP(p[1]) && !strcmp("cons", SCHEME_SYM_VAL(p[1]))) {
    for_each_found = cons_onto_list;
    cons_accum_result = scheme_null;
    flags -= (flags & GC_DUMP_SHOW_TRACE);
  }
  scheme_console_printf("Begin Dump\n");
#endif

# ifdef MZ_PRECISE_GC
  GC_dump_with_traces(flags, 
		      scheme_get_type_name,
		      GC_get_xtagged_name,
		      for_each_found,
		      trace_for_tag,
		      print_tagged_value,
		      path_length_limit);
# else
  GC_dump();
# endif
#endif

  if (scheme_external_dump_info)
    scheme_external_dump_info();

#ifdef USE_TAGGED_ALLOCATION
  {
    void **ps = NULL;
    int l;
    int max_w;
    Scheme_Object *w;

    GC_inital_root_skip = inital_root_skip;
    GC_initial_trace_root = initial_trace_root;
    GC_trace_path();
    GC_inital_root_skip = NULL;
    GC_initial_trace_root = NULL;
    
    w = scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PRINT_WIDTH);
    if (SCHEME_INTP(w))
      max_w = SCHEME_INT_VAL(w);
    else
      max_w = 10000;

    scheme_console_printf("Begin Paths\n");

    while ((ps = GC_get_next_path(ps, &l))) {
      int i, j;
      if (l)
	scheme_console_printf("$%s", ps[0]);
      for (i = 1, j = 2; i < l; i++, j += 2) {
	void *v = ps[j];
	unsigned long diff = (unsigned long)ps[j + 1];
	struct GC_Set *home;

	home = GC_set(v);
	if (home
	    && ((home == real_tagged)
		|| (home == tagged_atomic)
		|| (home == tagged_uncollectable)
		|| (home == tagged_eternal))) {
	  print_tagged_value("\n  ->", v, 0, diff, max_w, "");
	} else
	  print_tagged_value("\n  ->", v, 1, diff, max_w, "");
      }
      scheme_console_printf("\n");
    }

    GC_clear_paths();

    scheme_console_printf("End Paths\n");
  }

  scheme_console_printf("Begin Help\n");
  scheme_console_printf(" (dump-memory-stats sym) - prints paths to instances of type named by sym.\n");
  scheme_console_printf("   Examples: (dump-memory-stats '<pair>), (dump-memory-stats 'frame).\n");
  scheme_console_printf("   If sym is 'stack, prints paths to thread stacks.\n");
  scheme_console_printf(" (dump-memory-stats sym 'objects) - prints all instances of type named by sym.\n");
  scheme_console_printf(" (dump-memory-stats sym 'from from-v) - prints paths, paths through from-v first.\n");
  scheme_console_printf("End Help\n");

  if (obj_type >= 0) {
    result = scheme_null;
    while (obj_buffer_pos--) {
      result = scheme_make_pair((Scheme_Object *)(obj_buffer[obj_buffer_pos]), result);
    }
  }
#endif

# if MZ_PRECISE_GC_TRACE
  scheme_console_printf("Begin Help\n");
  scheme_console_printf(" (dump-memory-stats sym) - prints paths to instances of type named by sym.\n");
  scheme_console_printf("   Example: (dump-memory-stats '<pair>)\n");
  scheme_console_printf(" (dump-memory-stats 'fnl) - prints not-yet-finalized objects.\n");
  scheme_console_printf(" (dump-memory-stats num) - prints paths to objects with tag num.\n");
  scheme_console_printf(" (dump-memory-stats -num) - prints paths to objects of size num.\n");
  scheme_console_printf(" (dump-memory-stats sym/num len) - limits path to size len.\n");
  scheme_console_printf(" (dump-memory-stats sym/num 'cons) - builds list instead of showing paths.\n");
  scheme_console_printf(" (dump-memory-stats 'peek num v) - returns value if num is address of object, v otherwise.\n");
  scheme_console_printf(" (dump-memory-stats 'next v) - next tagged object after v, #f if none; start with #f.\n");
  scheme_console_printf(" (dump-memory-stats 'addr v) - returns the address of v.\n");
  scheme_console_printf(" (dump-memory-stats thread) - shows information about the thread.\n");
  scheme_console_printf("End Help\n");

  result = cons_accum_result;
  cons_accum_result = scheme_void;
# endif

  scheme_console_printf("End Dump\n");

  scheme_end_atomic();

  return result;
}



#ifdef MEMORY_COUNTING_ON

long scheme_count_closure(Scheme_Object **o, mzshort len, Scheme_Hash_Table *ht)
{
#if 0
  int i;
  int s = 0;

  for (i = 0; i < len; i++) {
    if (!scheme_lookup_in_table(ht, (const char *)o[i])) {
      scheme_hash_set(ht, o[i], scheme_true);
      if (GC_size(o[i]) == sizeof(Scheme_Object *)) {
	/* May be an environment box */
	Scheme_Object *d = *(Scheme_Object **)o[i];
	if (GC_size(d) >= sizeof(Scheme_Type)) {
	  /* Ok - probably it is a box. */
	  s += sizeof(Scheme_Object *);
	  s += scheme_count_memory(d, ht);
	} else {
	  /* Not an environment box. */
	  s += scheme_count_memory(o[i], ht);
	}
      } else {
	s += scheme_count_memory(o[i], ht);
      }
    }
  }

  return s;
#endif
  return 0;
}


#if 0
void scheme_check_home(Scheme_Object *root)
{
  struct GC_Set *home;
  home = GC_set(root);
  if ((home != real_tagged)
      && (home != tagged_atomic)
      && (home != tagged_uncollectable)
      && (home != tagged_eternal)) {
    scheme_console_printf("Check: bad Scheme object: %lx\n", (unsigned long)root);
  }
}
#endif

#define FORCE_SUBPARTS 0
#define FORCE_KNOWN_SUBPARTS 1
#define CAN_TRACE_HOME 1

long scheme_count_memory(Scheme_Object *root, Scheme_Hash_Table *ht)
{
  Scheme_Type type;
  long s = sizeof(Scheme_Simple_Object), e = 0;
  int need_align = 0;
  struct GC_Set *home;

  if (!root || SCHEME_INTP(root))
    return 0;

  type = SCHEME_TYPE(root);

  if (type >= _scheme_last_type_)
    return 0;

  if (ht && scheme_hash_get(ht, root))
    return 0;

  home = GC_set(root);
#if CAN_TRACE_HOME
  if ((home != real_tagged)
      && (home != tagged_atomic)
      && (home != tagged_uncollectable)
      && (home != tagged_eternal)) {
    scheme_console_printf("Bad Scheme object: %lx\n", (unsigned long)root);
    return 0;
  }
#endif

  if (ht)
    scheme_hash_set(ht, root, scheme_true);

#define COUNT(x) (ht ? scheme_count_memory((Scheme_Object *)x, ht) : 0)

  switch (type) {
  case scheme_variable_type:
    s = sizeof(Scheme_Bucket);
#if FORCE_SUBPARTS
    e = COUNT(((Scheme_Bucket *)root)->key)
      + COUNT(((Scheme_Bucket *)root)->val);
#endif
    break;
  case scheme_local_type: 
  case scheme_local_unbox_type:
    s = sizeof(Scheme_Local);
    break;
  case scheme_syntax_type:
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_IPTR_VAL(root));
#endif
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)root;
      int i;

      s = sizeof(Scheme_App_Rec) + (app->num_args * sizeof(Scheme_Object *))
	+ (app->num_args + 1);
      need_align = 1;
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(app->args[0]);
      for (i = 1; i <= app->num_args; i++) {
	e += COUNT(app->args[i]);
      }
#endif
    }
    break;
  case scheme_sequence_type:
  case scheme_case_lambda_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)root;
      int i;

      s = sizeof(Scheme_Sequence) + (seq->count - 1) * sizeof(Scheme_Object *);

#if FORCE_KNOWN_SUBPARTS
      for (i = e = 0; i < seq->count; i++) {
	e += COUNT(seq->array[i]);
      }
#endif
    }
    break;
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *rec = (Scheme_Branch_Rec *)root;
      
      s = sizeof(Scheme_Branch_Rec);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(rec->test) + COUNT(rec->tbranch) + COUNT(rec->fbranch);
#endif
    }
    break;
  case scheme_unclosed_procedure_type:
  case scheme_compiled_unclosed_procedure_type:
    {
      Scheme_Closure_Data *data = 
	(Scheme_Closure_Data *)root;

      s = sizeof(Scheme_Closure_Data);
      s += data->closure_size * sizeof(mzshort);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(data->code);
#endif
    }
    break;
  case scheme_let_value_type:
    {
      Scheme_Let_Value *let = (Scheme_Let_Value *)root;

      s = sizeof(Scheme_Let_Value);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->value) + COUNT(let->body);
#endif
    }
    break;
  case scheme_compiled_let_value_type:
    {
      Scheme_Compiled_Let_Value *let = (Scheme_Compiled_Let_Value *)root;

      s = sizeof(Scheme_Compiled_Let_Value);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->value) + COUNT(let->body);
#endif
    }
    break;
  case scheme_let_void_type:
    {
      Scheme_Let_Void *let = (Scheme_Let_Void *)root;

      s = sizeof(Scheme_Let_Void);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->body);
#endif
    }
    break;
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *let = (Scheme_Let_Header *)root;

      s = sizeof(Scheme_Let_Header);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->body);
#endif
    }
    break;
  case scheme_letrec_type:
    {
      Scheme_Letrec *let = (Scheme_Letrec *)root;
      int i;

      s = sizeof(Scheme_Letrec);
      s += let->count * sizeof(Scheme_Object *);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(let->body);
      for (i = 0; i < let->count; i++) {
	e += COUNT(let->procs[i]);
      }
#endif
    }
    break;
  case scheme_char_type:
    s = sizeof(Scheme_Small_Object);
    break;
  case scheme_integer_type:
    s = 0;
    break;
  case scheme_double_type:
    s = sizeof(Scheme_Double);
    break;
  case scheme_float_type:
    break;
  case scheme_char_string_type:
    s += (SCHEME_CHAR_STRTAG_VAL(root) + 1) * sizeof(mzchar);
    need_align = 1;
    break;
  case scheme_byte_string_type:
    s += SCHEME_BYTE_STRTAG_VAL(root) + 1;
    need_align = 1;
    break;
  case scheme_symbol_type:
    s = sizeof(Scheme_Symbol) + SCHEME_SYM_LEN(root) - 1;
    need_align = 1;
    break;
  case scheme_null_type: 
    break;
  case scheme_pair_type:
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_CAR(root)) + COUNT(SCHEME_CDR(root));
#endif
    break;
  case scheme_vector_type:
    {
      int count = SCHEME_VEC_SIZE(root), i;
      Scheme_Object **array = SCHEME_VEC_ELS(root);

      s += count * sizeof(Scheme_Object*);

#if FORCE_KNOWN_SUBPARTS
      for (i = e = 0; i < count; i++) {
	e += COUNT(array[i]);
      }
#endif
    }
    break;
  case scheme_prim_type:
    {
      if (((Scheme_Primitive_Proc *)root)->pp.flags & SCHEME_PRIM_IS_MULTI_RESULT)
	s = sizeof(Scheme_Prim_W_Result_Arity);
      else
	s = sizeof(Scheme_Primitive_Proc);
    }	
    break;
  case scheme_closure_type:
    {
      Scheme_Closure_Data *data;
      Scheme_Object **vals;
      
      data = SCHEME_COMPILED_CLOS_CODE(root);
      vals = SCHEME_COMPILED_CLOS_ENV(root);

      s += (data->closure_size * sizeof(Scheme_Object *));
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(data) + scheme_count_closure(vals, data->closure_size, ht);
#endif
    }
    break;
  case scheme_closed_prim_type:
    {
      if (((Scheme_Closed_Primitive_Proc *)root)->pp.flags & SCHEME_PRIM_IS_MULTI_RESULT)
	s = sizeof(Scheme_Closed_Prim_W_Result_Arity);
      else
	s = sizeof(Scheme_Closed_Primitive_Proc);
    }	
    break;
  case scheme_cont_type:
    {
      Scheme_Cont *c = (Scheme_Cont *)root;
      Scheme_Saved_Stack *rs;

      s = sizeof(Scheme_Cont);

      for (rs = c->runstack_copied; rs; rs = rs->prev) {
	s += sizeof(Scheme_Saved_Stack);
      }
    }
    break;
  case scheme_input_port_type: 
    scheme_count_input_port(root, &s, &e, ht);
    break;
  case scheme_output_port_type:
    scheme_count_output_port(root, &s, &e, ht);
    break;
  case scheme_eof_type:
  case scheme_true_type: 
  case scheme_false_type:
  case scheme_void_type:
  case scheme_undefined_type:
    /* Only one */
    break;
  case scheme_syntax_compiler_type:
    break;
  case scheme_macro_type:
  case scheme_set_macro_type:
    s = sizeof(Scheme_Small_Object);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_PTR_VAL(root));
#endif
    break;
  case scheme_box_type:
    s = sizeof(Scheme_Small_Object);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(SCHEME_BOX_VAL(root));
#endif
    break;
  case scheme_will_executor_type:
    s = sizeof(Scheme_Simple_Object);
    break;
  case scheme_custodian_type: 
    {
      Scheme_Custodian *m = (Scheme_Custodian *)root;

      s = sizeof(Scheme_Custodian);
      e = m->alloc * (sizeof(Scheme_Object **)
		      + sizeof(Scheme_Custodian_Reference *)
		      + sizeof(void *)
		      + sizeof(void *));
    }
    break;
  case scheme_thread_type:
    {
      Scheme_Thread *p = (Scheme_Thread *)root;
      Scheme_Saved_Stack *saved;

      s = sizeof(Scheme_Thread)
	+ ((p->runstack_size + p->tail_buffer_size) * sizeof(Scheme_Object *));

#if FORCE_KNOWN_SUBPARTS
      e = COUNT(p->init_config);
#endif

      /* Check stack: */
      for (saved = p->runstack_saved; saved; saved = saved->prev) {
	s += (saved->runstack_size * sizeof(Scheme_Object *));
      }
    }
    break;
  case scheme_namespace_type:
    {
      Scheme_Env *env = (Scheme_Env *)root;

      s = sizeof(Scheme_Env);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(env->toplevel);
#endif
    }
    break;
  case scheme_config_type:
    {
      s = sizeof(Scheme_Config) + (sizeof(Scheme_Object *) * __MZCONFIG_BUILTIN_COUNT__);
#if FORCE_SUBPARTS
      {
	Scheme_Config *c = (Scheme_Config *)root;
	int i;

	e = COUNT(c->extensions) + COUNT(c->base);

	for (i = 0; i < __MZCONFIG_BUILTIN_COUNT__; i++) {
	  e += COUNT(*c->configs[i]);
	}
      }
#endif
    }
    break;
  case scheme_proc_struct_type:
  case scheme_structure_type:
    {
      Scheme_Object **slots = ((Scheme_Structure *)root)->slots;
      int i, count = SCHEME_STRUCT_NUM_SLOTS(root);

      s = sizeof(Scheme_Structure) + (count - 1) * sizeof(Scheme_Object *);
#if FORCE_KNOWN_SUBPARTS
      for (i = e = 0; i < count; i++) {
	e += COUNT(slots[i]);
      }
      e += COUNT(((Scheme_Structure *)root)->stype);
#endif
    }
    break;
  case scheme_bignum_type:
    {
      int count = SCHEME_BIGLEN(root);

      if (count < 0)
	count = -count;

      s = sizeof(Small_Bignum) + (count - 1) * sizeof(bigdig);
    }
    break;
  case scheme_escaping_cont_type:
    s = sizeof(Scheme_Escaping_Cont);
    break;
  case scheme_sema_type:
    s = sizeof(Scheme_Sema);
    break;
  case scheme_compilation_top_type:
    s = sizeof(Scheme_Compilation_Top);
    break;
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)root;

      s = sizeof(Scheme_Hash_Table)
	+ ht->size * sizeof(Scheme_Object *);
      
#if FORCE_SUBPARTS
      {
	int i;
	for (i = e = 0; i < ht->size; i++) {
	  if (ht->buckets[i]) {
	    if (ht->by_address)
	      e += COUNT(ht->buckets[i]);
	    else
	      e += COUNT(ht->buckets[i]->val);
	  }
	}
      }
#endif
    }
    break;
  case scheme_weak_box_type:
    s = sizeof(Scheme_Small_Object);
    e = COUNT(SCHEME_BOX_VAL(root));
    break;
  case scheme_complex_type:
    s = sizeof(Scheme_Complex);
    e = COUNT(((Scheme_Complex *)root)->r) + COUNT(((Scheme_Complex *)root)->i);
    break;
  case scheme_rational_type:
    s = sizeof(Scheme_Rational);
#if FORCE_KNOWN_SUBPARTS
    e = COUNT(((Scheme_Rational *)root)->num) 
      + COUNT(((Scheme_Rational *)root)->denom);
#endif
    break;
  case scheme_struct_type_type:
    {
      Scheme_Struct_Type *st = (Scheme_Struct_Type *)root;
      s = sizeof(Scheme_Struct_Type) + st->name_pos * sizeof(Scheme_Object*);
#if FORCE_KNOWN_SUBPARTS
      e = COUNT(st->name);
      if (st->name_pos)
	e += COUNT(st->parent_types[st->name_pos - 1]);
#endif
    }
    break;
  case scheme_listener_type:
    s = sizeof(Scheme_Small_Object);
    break;
  case scheme_random_state_type:
    s = 130; /* wild guess */
    break;
  case scheme_eval_waiting_type:
  case scheme_tail_call_waiting_type:
    /* Only one */
    break;
  case scheme_multiple_values_type:
    /* Only one */
    break;
  case scheme_placeholder_type:
    s = 0; /* Infrequent */
    break;
  default:
    s = 0;
    break;
  }

  if (need_align) {
    /* Round up to sizeof(void*) boundary: */
    if (s & (sizeof(void*) - 1))
      s += sizeof(void*) - (s & (sizeof(void*) - 1));
  }

  scheme_memory_count[type]++;
  scheme_memory_size[type] += s;

  return s;
}

long scheme_count_envbox(Scheme_Object *root, Scheme_Hash_Table *ht)
{
#if CAN_TRACE_HOME
  if (GC_set(root) != envunbox) {
    scheme_console_printf("Bad envunbox object: %lx\n", (unsigned long)root);
    return 0;
  }
#endif

  if (ht)
    return scheme_count_memory(SCHEME_ENVBOX_VAL(root), ht) + 4;
  else
    return 4;
}

#endif
