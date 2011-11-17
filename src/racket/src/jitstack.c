/*
  Racket
  Copyright (c) 2006-2011 PLT Scheme Inc.

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
*/

#include "schpriv.h"
#include "schmach.h"
#ifdef MZ_USE_DWARF_LIBUNWIND
# include "unwind/libunwind.h"
#endif
#include "future.h"

#ifdef MZ_USE_JIT

#include "jit.h"

#include "codetab.inc"

/* The Stack_Cache_Elem structure type (define in schthread.h)
   must have a size of  4 words. */

THREAD_LOCAL_DECL(static Stack_Cache_Elem stack_cache_stack[STACK_CACHE_SIZE]);
THREAD_LOCAL_DECL(static intptr_t stack_cache_stack_pos = 0);

void *scheme_decrement_cache_stack_pos(void *p)
{
  Stack_Cache_Elem *r;
  r = stack_cache_stack + stack_cache_stack_pos;
  stack_cache_stack_pos--;
  r->orig_result = p;
  return r;
}

void scheme_register_stack_cache_stack(void)
{
  REGISTER_SO(stack_cache_stack);
}

/*========================================================================*/
/*                              stack trace                               */
/*========================================================================*/

typedef void *(*Get_Stack_Proc)();

#ifdef MZ_USE_JIT_PPC
# ifdef _CALL_DARWIN
#  define RETURN_ADDRESS_OFFSET 2
# else
#  define RETURN_ADDRESS_OFFSET 1
# endif
#endif
#ifdef MZ_USE_JIT_I386
# define RETURN_ADDRESS_OFFSET 1
#endif

#define CACHE_STACK_MIN_TRIGGER 128

#define USE_STACK_CHECK 0

#if USE_STACK_CHECK
static void check_stack(void)
{
  void *p, *q;
  uintptr_t stack_end;
  int pos = stack_cache_stack_pos;
  Get_Stack_Proc gs;

  gs = (Get_Stack_Proc)get_stack_pointer_code;
  p = gs();

  stack_end = (uintptr_t)(scheme_current_thread->next 
			      ? scheme_current_thread->stack_start 
			      : scheme_current_thread->o_start);

  while (STK_COMP((uintptr_t)p, stack_end)) {
    q = ((void **)p)[RETURN_ADDRESS_OFFSET];

    if (q == stack_cache_pop_code) {
      if (!pos)
	abort();
      else {
	if (stack_cache_stack[pos].stack_frame != (void *)(((void **)p) + RETURN_ADDRESS_OFFSET)) {
	  abort();
	}
	--pos;
      }
    }

    q = *(void **)p;
    if (STK_COMP((uintptr_t)q, (uintptr_t)p))
      break;
    p = q;
  }
}
#endif

MZ_DO_NOT_INLINE(uintptr_t scheme_approx_sp());
uintptr_t scheme_approx_sp()
{
  uintptr_t p;
  p = (uintptr_t)&p;
  return p;
}

Scheme_Object *scheme_native_stack_trace(void)
{
  void *p, *q;
  uintptr_t stack_end, real_stack_end, stack_start, halfway;
  Scheme_Object *name, *last = NULL, *first = NULL, *tail;
  int prev_had_name = 0;
#ifdef MZ_USE_DWARF_LIBUNWIND
  unw_context_t cx;
  unw_cursor_t c;
  int manual_unw = 0;
  unw_word_t stack_addr;
#else
  Get_Stack_Proc gs;
#endif
  int use_unw = 0;
  int shift_cache_to_next = 0;
  int added_list_elem;

  if (!sjc.get_stack_pointer_code)
    return NULL;

#if USE_STACK_CHECK
  check_stack();
#endif

  stack_start = scheme_approx_sp();

  real_stack_end = (uintptr_t)scheme_current_thread->stack_start;
  if (stack_cache_stack_pos) {
    stack_end = (uintptr_t)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    stack_end -= (RETURN_ADDRESS_OFFSET << JIT_LOG_WORD_SIZE);
    tail = stack_cache_stack[stack_cache_stack_pos].cache;
  } else {
    stack_end = real_stack_end;
    tail = scheme_null;
  }

#ifdef MZ_USE_DWARF_LIBUNWIND
  unw_set_safe_pointer_range(stack_start, stack_end);
  unw_reset_bad_ptr_flag();
#endif

#ifdef MZ_USE_DWARF_LIBUNWIND
  unw_getcontext(&cx);
  unw_init_local(&c, &cx);
  use_unw = 1;
  p = NULL;
#else
  gs = (Get_Stack_Proc)sjc.get_stack_pointer_code;
  p = gs();
#endif

  halfway = STK_DIFF(stack_end, (uintptr_t)p) / 2;
  if (halfway < CACHE_STACK_MIN_TRIGGER)
    halfway = stack_end;
  else {
#ifdef STACK_GROWS_DOWN
    halfway += (uintptr_t)p;
#else
    halfway += stack_end;
#endif
  }

  while (1) {
#ifdef MZ_USE_DWARF_LIBUNWIND
    if (use_unw) {
      q = (void *)unw_get_ip(&c);
    } else {
      q = NULL;
    }
#endif

    if (!use_unw) {
      if (!(STK_COMP((uintptr_t)p, stack_end)
	    && STK_COMP(stack_start, (uintptr_t)p)))
	break;
      q = ((void **)p)[RETURN_ADDRESS_OFFSET];
      /* p is the frame pointer for the function called by q,
	 not for q. */
    }

    name = find_symbol((uintptr_t)q);
#ifdef MZ_USE_DWARF_LIBUNWIND
    if (name) manual_unw = 1;
#endif

    if (SCHEME_FALSEP(name) || SCHEME_VOIDP(name)) {
      /* Code uses special calling convention */
#ifdef MZ_USE_JIT_PPC
      /* JIT_LOCAL2 has the next return address */
      q = ((void **)p)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
#endif
#ifdef MZ_USE_JIT_I386

# ifdef MZ_USE_DWARF_LIBUNWIND
      if (use_unw) {
	q = (void *)unw_get_frame_pointer(&c);
      } else
# endif
	q = *(void **)p;

      /* q is now the frame pointer for the former q,
	 so we can find the actual q */
      if (STK_COMP((uintptr_t)q, real_stack_end)
	  && STK_COMP(stack_start, (uintptr_t)q)) {
	if (SCHEME_VOIDP(name)) {
	  /* JIT_LOCAL2 has the next return address */
	  q = ((void **)q)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
	} else {
	  /* Push after local stack of return-address proc
	     has the next return address */
	  q = ((void **)q)[-(3 + LOCAL_FRAME_SIZE + 1)];
	}
      } else {
	q = NULL;
      }
#endif
      name = find_symbol((uintptr_t)q);
    } else if (SCHEME_EOFP(name)) {
      /* Stub (to mark start of running a module body, for example) */
      /* JIT_LOCAL2 has the name to use */
#ifdef MZ_USE_JIT_PPC
      name = *(Scheme_Object **)((void **)p)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
#endif
#ifdef MZ_USE_JIT_I386
      void *np;
# ifdef MZ_USE_DWARF_LIBUNWIND
      if (use_unw) {
	np = (void *)unw_get_frame_pointer(&c);
      } else
# endif
	np = *(void **)p;

      if (STK_COMP((uintptr_t)np, real_stack_end)
	  && STK_COMP(stack_start, (uintptr_t)np)) {
        name = *(Scheme_Object **)((void **)np)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
      } else
        name = NULL;
#endif
    }

    if (name && !SCHEME_NULLP(name)) { /* null is used to help unwind without a true name */
      name = scheme_make_pair(name, scheme_null);
      if (last)
	SCHEME_CDR(last) = name;
      else
	first = name;
      last = name;
      if (shift_cache_to_next) {
        stack_cache_stack[stack_cache_stack_pos].cache = last;
        shift_cache_to_next = 0;
      }
      added_list_elem = 1;
    } else 
      added_list_elem = 0;

    /* Cache the result halfway up the stack, if possible. Only cache
       on frames where the previous frame had a return address with a
       name, because an arbitrary frame's return address on the stack
       might not be used (depending on how the C compiler optimized the
       code); any frame whose procedure has a name is JITted code, so
       it will use the return address from the stack. */
    if (STK_COMP((uintptr_t)halfway, (uintptr_t)p)
	&& prev_had_name) {
      int pos;

      if (stack_cache_stack_pos >= (STACK_CACHE_SIZE - 1)) {
	/* Make room on the stack */
	void **z;
	z = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
	*z = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
	--stack_cache_stack_pos;
      }

      pos = ++stack_cache_stack_pos;
      stack_cache_stack[pos].orig_return_address = ((void **)p)[RETURN_ADDRESS_OFFSET];
      stack_cache_stack[pos].stack_frame = (void *)(((void **)p) + RETURN_ADDRESS_OFFSET);
      stack_cache_stack[pos].cache = last;
      ((void **)p)[RETURN_ADDRESS_OFFSET] = sjc.stack_cache_pop_code;
      if (!added_list_elem)
        shift_cache_to_next = 1;

      halfway = stack_end;
    }

    prev_had_name = !!name;

#ifdef MZ_USE_DWARF_LIBUNWIND
    if (use_unw) {
      if (manual_unw) {
        /* A JIT-generated function, so we unwind ourselves... */
	void **pp;
	pp = (void **)unw_get_frame_pointer(&c);
	if (!(STK_COMP((uintptr_t)pp, stack_end)
	      && STK_COMP(stack_start, (uintptr_t)pp)))
	  break;
	stack_addr = (unw_word_t)&(pp[RETURN_ADDRESS_OFFSET+1]);
	unw_manual_step(&c, &pp[RETURN_ADDRESS_OFFSET], &pp[0],
			&stack_addr, &pp[-1], &pp[-2], &pp[-3]);
	manual_unw = 0;
      } else {
        void *prev_q = q;
        unw_step(&c);
        q = (void *)unw_get_ip(&c);
        if ((q == prev_q)
	    || unw_reset_bad_ptr_flag())
          break;
      }
    }
#endif

    if (!use_unw) {
      q = *(void **)p;
      if (STK_COMP((uintptr_t)q, (uintptr_t)p))
        break;
      p = q;
    }
  }

  if (shift_cache_to_next)
    stack_cache_stack[stack_cache_stack_pos].cache = scheme_null;

#ifdef MZ_USE_DWARF_LIBUNWIND
  unw_destroy_local(&c);
#endif

  if (last)
    SCHEME_CDR(last) = tail;
  else
    first = tail;

  if (SCHEME_NULLP(first))
    return NULL;

  return first;
}

#if 0
/* Sometimes useful for debugging Racket: */
void scheme_dump_stack_trace(void)
{
  void *p, *q;
  uintptr_t stack_end, stack_start;
  Get_Stack_Proc gs;
  Scheme_Object *name;

  gs = (Get_Stack_Proc)sjc.get_stack_pointer_code;
  p = gs();
  stack_start = scheme_approx_sp();

  stack_end = (uintptr_t)scheme_current_thread->stack_start;

  while (STK_COMP((uintptr_t)p, stack_end)
         && STK_COMP(stack_start, (uintptr_t)p)) {
    name = find_symbol((uintptr_t)q);
    if (SCHEME_FALSEP(name)) {
      /* Code uses special calling convention */
#ifdef MZ_USE_JIT_PPC
      /* JIT_LOCAL2 has the next return address */
      q = ((void **)p)[JIT_LOCAL2 >> JIT_LOG_WORD_SIZE];
#endif
#ifdef MZ_USE_JIT_I386
      /* Push after local stack of return-address proc
	 has the next return address */
      q = *(void **)p;
      q = ((void **)q)[-(3 + LOCAL_FRAME_SIZE + 1)];
#endif
      name = find_symbol((uintptr_t)q);
    }

    if (name) {
      printf(" scheme\n");
    } else {
      printf(" %p\n", q);
    }

    q = *(void **)p;
    if (STK_COMP((uintptr_t)q, (uintptr_t)p))
      break;
    p = q;
  }
}
#endif

void scheme_flush_stack_cache()
  XFORM_SKIP_PROC
{
  void **p;

  while (stack_cache_stack_pos) {
    p = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    *p = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
    --stack_cache_stack_pos;
  }
}

void scheme_jit_longjmp(mz_jit_jmp_buf b, int v)
  XFORM_SKIP_PROC
{
  uintptr_t limit;
  void **p;

  limit = b->stack_frame;

  while (stack_cache_stack_pos
	 && STK_COMP((uintptr_t)stack_cache_stack[stack_cache_stack_pos].stack_frame,
		     limit)) {
    p = (void **)stack_cache_stack[stack_cache_stack_pos].stack_frame;
    *p = stack_cache_stack[stack_cache_stack_pos].orig_return_address;
    --stack_cache_stack_pos;
  }

  scheme_mz_longjmp(b->jb, v);
}

void scheme_jit_setjmp_prepare(mz_jit_jmp_buf b)
  XFORM_SKIP_PROC
{
  void *p;
  p = &p;
  b->stack_frame = (uintptr_t)p;
}

void scheme_clean_native_symtab(void)
{
#ifndef MZ_PRECISE_GC
  clear_symbols_for_collected();
  jit_notify_freed_code();
#endif
}

#ifdef MZ_PRECISE_GC
void scheme_jit_release_native_code(void *fnlized, void *p)
{
  Scheme_Object *len;

  len = SCHEME_BOX_VAL(fnlized);

  scheme_jit_malloced -= SCHEME_INT_VAL(len);

  /* Remove name mapping: */
  scheme_jit_add_symbol((uintptr_t)p, (uintptr_t)p + SCHEME_INT_VAL(len), NULL, 1);
  /* Free memory: */
  scheme_free_code(p);
  jit_notify_freed_code();
}
#endif

typedef void *(*Module_Run_Proc)(Scheme_Env *menv, Scheme_Env *env, Scheme_Object **name);
typedef void *(*Module_Exprun_Proc)(Scheme_Env *menv, int set_ns, Scheme_Object **name);
typedef void *(*Module_Start_Proc)(struct Start_Module_Args *a, Scheme_Object **name);

void *scheme_module_run_start(Scheme_Env *menv, Scheme_Env *env, Scheme_Object *name)
{
  Module_Run_Proc proc = (Module_Run_Proc)sjc.module_run_start_code;
  if (proc)
    return proc(menv, env, &name);
  else
    return scheme_module_run_finish(menv, env);
}

void *scheme_module_exprun_start(Scheme_Env *menv, int set_ns, Scheme_Object *name)
{
  Module_Exprun_Proc proc = (Module_Exprun_Proc)sjc.module_exprun_start_code;
  if (proc)
    return proc(menv, set_ns, &name);
  else
    return scheme_module_exprun_finish(menv, set_ns);
}

void *scheme_module_start_start(struct Start_Module_Args *a, Scheme_Object *name)
{
  Module_Start_Proc proc = (Module_Start_Proc)sjc.module_start_start_code;
  if (proc)
    return proc(a, &name);
  else
    return scheme_module_start_finish(a);
}

#endif
