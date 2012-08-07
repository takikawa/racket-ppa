/*
  Racket
  Copyright (c) 2004-2012 PLT Scheme Inc.
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

/* This file implements bytecode optimzation.

   See "eval.c" for an overview of compilation passes. */

#include "schpriv.h"
#include "schrunst.h"
#include "schmach.h"

#define cons(a,b) scheme_make_pair(a,b)

/* Controls for inlining algorithm: */
#define OPT_ESTIMATE_FUTURE_SIZES   1
#define OPT_DISCOURAGE_EARLY_INLINE 1
#define OPT_LIMIT_FUNCTION_RESIZE   0
#define OPT_BRANCH_ADDS_NO_SIZE     1
#define OPT_DELAY_GROUP_PROPAGATE   0

#define MAX_PROC_INLINE_SIZE 256
#define CROSS_MODULE_INLINE_SIZE 8

#define SCHEME_PRIM_IS_UNSAFE_NONMUTATING (SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL | SCHEME_PRIM_IS_UNSAFE_OMITABLE)

struct Optimize_Info
{
  MZTAG_IF_REQUIRED
  short flags;
  struct Optimize_Info *next;
  int original_frame, new_frame;
  Scheme_Object *consts;
  Comp_Prefix *cp;

  /* Propagated up and down the chain: */
  int size, vclock, psize;
  short inline_fuel;
  char letrec_not_twice, enforce_const, use_psize, has_nonleaf;
  Scheme_Hash_Table *top_level_consts;

  /* Set by expression optimization: */
  int single_result, preserves_marks; /* negative means "tentative", due to fixpoint in progress */

  char **stat_dists; /* (pos, depth) => used? */
  int *sd_depths;
  int used_toplevel;
  char *use;

  int transitive_use_pos; /* set to pos + 1 when optimizing a letrec-bound procedure */
  mzshort **transitive_use;
  int *transitive_use_len;

  Scheme_Object *context; /* for logging */
};

static char *get_closure_flonum_map(Scheme_Closure_Data *data, int arg_n, int *ok);
static void set_closure_flonum_map(Scheme_Closure_Data *data, char *flonum_map);
static void merge_closure_flonum_map(Scheme_Closure_Data *data1, Scheme_Closure_Data *data2);
static int closure_body_size(Scheme_Closure_Data *data, int check_assign,
                             Optimize_Info *info, int *is_leaf);
static int closure_has_top_level(Scheme_Closure_Data *data);
static int closure_argument_flags(Scheme_Closure_Data *data, int i);

static int optimize_info_is_ready(Optimize_Info *info, int pos);

static void optimize_propagate(Optimize_Info *info, int pos, Scheme_Object *value, int single_use);
static Scheme_Object *optimize_info_lookup(Optimize_Info *info, int pos, int *closure_offset, int *single_use,
                                           int once_used_ok, int context, int *potential_size, int *_mutated);
static Scheme_Object *optimize_info_mutated_lookup(Optimize_Info *info, int pos, int *is_mutated);
static void optimize_info_used_top(Optimize_Info *info);

static void optimize_mutated(Optimize_Info *info, int pos);
static void optimize_produces_flonum(Optimize_Info *info, int pos);
static Scheme_Object *optimize_reverse(Optimize_Info *info, int pos, int unless_mutated, int disrupt_single_use);
static int optimize_is_used(Optimize_Info *info, int pos);
static int optimize_any_uses(Optimize_Info *info, int start_pos, int end_pos);
static int optimize_is_mutated(Optimize_Info *info, int pos);
static int optimize_is_flonum_arg(Optimize_Info *info, int pos, int depth);
static int optimize_is_flonum_valued(Optimize_Info *info, int pos);
static int env_uses_toplevel(Optimize_Info *frame);
static void env_make_closure_map(Optimize_Info *frame, mzshort *size, mzshort **map);

static Optimize_Info *optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags);
static int optimize_info_get_shift(Optimize_Info *info, int pos);
static void optimize_info_done(Optimize_Info *info, Optimize_Info *parent);

static Scheme_Object *estimate_closure_size(Scheme_Object *e);
static Scheme_Object *no_potential_size(Scheme_Object *value);

static Scheme_Object *optimize_clone(int dup_ok, Scheme_Object *obj, Optimize_Info *info, int delta, int closure_depth);
static Scheme_Object *optimize_shift(Scheme_Object *obj, int delta, int after_depth);

#define IS_COMPILED_PROC(vals_expr) (SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_compiled_unclosed_procedure_type) \
                                     || SAME_TYPE(SCHEME_TYPE(vals_expr), scheme_case_lambda_sequence_type))

static int compiled_proc_body_size(Scheme_Object *o, int less_args);

typedef struct Scheme_Once_Used {
  Scheme_Object so;
  Scheme_Object *expr;
  int pos;
  int vclock;

  int used;
  int delta;
  int cross_lambda;
  Optimize_Info *info;

  struct Scheme_Once_Used *next;
} Scheme_Once_Used;

static Scheme_Once_Used *make_once_used(Scheme_Object *val, int pos, int vclock, Scheme_Once_Used *prev);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_optimize()
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

/*========================================================================*/
/*                                  utils                                 */
/*========================================================================*/

static int is_current_inspector_call(Scheme_Object *a)
{
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)a;
    if (!app->num_args
        && SAME_OBJ(app->args[0], scheme_current_inspector_proc))
      return 1;
  }
  return 0;
}

static int is_proc_spec_proc(Scheme_Object *p)
{
  Scheme_Type vtype;

  if (SCHEME_PROCP(p)) {
    p = scheme_get_or_check_arity(p, -1);
    if (SCHEME_INTP(p)) {
      return (SCHEME_INT_VAL(p) >= 1);
    } else if (SCHEME_STRUCTP(p)
               && scheme_is_struct_instance(scheme_arity_at_least, p)) {
      p = ((Scheme_Structure *)p)->slots[0];
      if (SCHEME_INTP(p))
        return (SCHEME_INT_VAL(p) >= 1);
    }
    return 0;
  }

  vtype = SCHEME_TYPE(p);

  if (vtype == scheme_unclosed_procedure_type) {
    if (((Scheme_Closure_Data *)p)->num_params >= 1)
      return 1;
  }

  return 0;
}

static void note_match(int actual, int expected, Optimize_Info *warn_info)
{
  if (!warn_info || (expected == -1))
    return;

  if (actual != expected) {
    scheme_log(NULL,
               SCHEME_LOG_WARNING,
               0,
               "warning%s: optimizer detects %d values produced when %d expected",
               scheme_optimize_context_to_string(warn_info->context),
               actual, expected);
  }
}

int scheme_omittable_expr(Scheme_Object *o, int vals, int fuel, int resolved,
                          Optimize_Info *warn_info, int deeper_than, int no_id)
     /* Checks whether the bytecode `o' returns `vals' values with no
        side-effects and without pushing and using continuation marks.
        -1 for vals means that any return count is ok.
        Also used with fully resolved expression by `module' to check
        for "functional" bodies.
        If warn_info is supplied, complain when a mismatch is detected.
        If no_id is 1, then an identifier doesn't count as omittable,
        unless the identifier is a consistent top-level; currently, this
        is used to imply the absece of a continuation-mark impersonator. */
{
  Scheme_Type vtype;

  /* FIXME: can overflow the stack */

 try_again:

  vtype = SCHEME_TYPE(o);

  if ((vtype > _scheme_compiled_values_types_)
      || ((vtype == scheme_local_type)
          && !no_id
          && !(SCHEME_GET_LOCAL_FLAGS(o) == SCHEME_LOCAL_CLEAR_ON_READ)
          && (SCHEME_LOCAL_POS(o) > deeper_than))
      || ((vtype == scheme_local_unbox_type)
          && !no_id
          && !(SCHEME_GET_LOCAL_FLAGS(o) == SCHEME_LOCAL_CLEAR_ON_READ)
          && (SCHEME_LOCAL_POS(o) > deeper_than))
      || (vtype == scheme_unclosed_procedure_type)
      || (vtype == scheme_compiled_unclosed_procedure_type)
      || (vtype == scheme_inline_variant_type)
      || (vtype == scheme_case_lambda_sequence_type)
      || (vtype == scheme_case_lambda_sequence_type)
      || (vtype == scheme_quote_syntax_type)
      || (vtype == scheme_varref_form_type)
      || (vtype == scheme_compiled_quote_syntax_type)) {
    note_match(1, vals, warn_info);
    return ((vals == 1) || (vals < 0));
  }

  if (vtype == scheme_toplevel_type) {
    note_match(1, vals, warn_info);
    if (!no_id && resolved && ((vals == 1) || (vals < 0))) {
      if (SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK)
        return 1;
      else
        return 0;
    }
  }

  if (vtype == scheme_compiled_toplevel_type) {
    note_match(1, vals, warn_info);
    if ((vals == 1) || (vals < 0)) {
      if (!no_id && (SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_READY)
        return 1;
      else if ((SCHEME_TOPLEVEL_FLAGS(o) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_CONST)
        return 1;
      else
        return 0;
    }
  }

  if (vtype == scheme_case_lambda_sequence_type) {
    note_match(1, vals, warn_info);
    return 1;
  }

  if (vtype == scheme_compiled_quote_syntax_type) {
    note_match(1, vals, warn_info);
    return ((vals == 1) || (vals < 0));
  }

  if (vtype == scheme_branch_type) {
    Scheme_Branch_Rec *b;
    b = (Scheme_Branch_Rec *)o;
    return (scheme_omittable_expr(b->test, 1, fuel - 1, resolved, warn_info, deeper_than, 0)
	    && scheme_omittable_expr(b->tbranch, vals, fuel - 1, resolved, warn_info, deeper_than, no_id)
	    && scheme_omittable_expr(b->fbranch, vals, fuel - 1, resolved, warn_info, deeper_than, no_id));
  }

#if 0
  /* We can't do this because a set! to a lexical is turned into
     a let_value_type! */
  if (vtype == scheme_let_value_type) {
    Scheme_Let_Value *lv = (Scheme_Let_Value *)o;
    return (scheme_omittable_expr(lv->value, lv->count, fuel - 1, resolved, warn_info, deeper_than, no_id)
	    && scheme_omittable_expr(lv->body, vals, fuel - 1, resolved, warn_info, deeper_than, no_id));
  }
#endif

  if (vtype == scheme_let_one_type) {
    Scheme_Let_One *lo = (Scheme_Let_One *)o;
    return (scheme_omittable_expr(lo->value, 1, fuel - 1, resolved, warn_info, deeper_than + 1, 0)
	    && scheme_omittable_expr(lo->body, vals, fuel - 1, resolved, warn_info, deeper_than + 1, no_id));
  }

  if (vtype == scheme_let_void_type) {
    Scheme_Let_Void *lv = (Scheme_Let_Void *)o;
    /* recognize (letrec ([x <omittable>]) ...): */
    if (SAME_TYPE(SCHEME_TYPE(lv->body), scheme_let_value_type)) {
      Scheme_Let_Value *lv2 = (Scheme_Let_Value *)lv->body;
      if ((lv2->count == 1)
          && (lv2->position == 0)
          && scheme_omittable_expr(lv2->value, 1, fuel - 1, resolved, warn_info,
                                   deeper_than + 1 + lv->count,
                                   0)) {
        o = lv2->body;
        deeper_than += 1;
      } else
        o = lv->body;
    } else
      o = lv->body;
    deeper_than += lv->count;
    goto try_again;
  }

  if (vtype == scheme_compiled_let_void_type) {
    /* recognize another (let ([x <omittable>]) ...) pattern: */
    Scheme_Let_Header *lh = (Scheme_Let_Header *)o;
    if ((lh->count == 1) && (lh->num_clauses == 1)) {
      if (SAME_TYPE(SCHEME_TYPE(lh->body), scheme_compiled_let_value_type)) {
        Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
        if (scheme_omittable_expr(lv->value, 1, fuel - 1, resolved, warn_info, deeper_than + 1, 0)) {
          o = lv->body;
          deeper_than++;
          goto try_again;
        }
      }
    }
  }

  if (vtype == scheme_letrec_type) {
    o = ((Scheme_Letrec *)o)->body;
    goto try_again;
  }

  if (vtype == scheme_application_type) {
    /* Look for multiple values, or for `make-struct-type'.
       (The latter is especially useful to Honu.) */
    Scheme_App_Rec *app = (Scheme_App_Rec *)o;
    if ((app->num_args >= 4) && (app->num_args <= 10)
        && SAME_OBJ(scheme_make_struct_type_proc, app->args[0])) {
      note_match(5, vals, warn_info);
      if ((vals == 5) || (vals < 0)) {
        /* Look for (make-struct-type sym #f non-neg-int non-neg-int [omitable null]) */
        if (SCHEME_SYMBOLP(app->args[1])
            && SCHEME_FALSEP(app->args[2])
            && SCHEME_INTP(app->args[3])
            && (SCHEME_INT_VAL(app->args[3]) >= 0)
            && SCHEME_INTP(app->args[4])
            && (SCHEME_INT_VAL(app->args[4]) >= 0)
            && ((app->num_args < 5)
                || scheme_omittable_expr(app->args[5], 1, fuel - 1, resolved, warn_info,
                                         deeper_than + (resolved ? app->num_args : 0), 0))
            && ((app->num_args < 6)
                || SCHEME_NULLP(app->args[6]))
            && ((app->num_args < 7)
                || SCHEME_FALSEP(app->args[7])
                || is_current_inspector_call(app->args[7]))
            && ((app->num_args < 8)
                || SCHEME_FALSEP(app->args[8])
                || is_proc_spec_proc(app->args[8]))
            && ((app->num_args < 9)
                || SCHEME_NULLP(app->args[9]))) {
          return 1;
        }
      }
    }

    if (SCHEME_PRIMP(app->args[0])) {
      if ((SCHEME_PRIM_PROC_FLAGS(app->args[0]) & SCHEME_PRIM_IS_OMITABLE)
          && (app->num_args >= ((Scheme_Primitive_Proc *)app->args[0])->mina)
          && (app->num_args <= ((Scheme_Primitive_Proc *)app->args[0])->mu.maxa)
          && ((vals < 0) 
              || ((vals == 1) && !(SCHEME_PRIM_PROC_FLAGS(app->args[0]) & SCHEME_PRIM_IS_MULTI_RESULT))
              || (SAME_OBJ(scheme_values_func, app->args[0])
                  && (vals == app->num_args)))) {
        int i;
        for (i = app->num_args; i--; ) {
          if (!scheme_omittable_expr(app->args[i + 1], 1, fuel - 1, resolved, warn_info,
                                     deeper_than + (resolved ? app->num_args : 0), 0))
            return 0;
        }
        return 1;
      } else if (!(SCHEME_PRIM_PROC_FLAGS(app->args[0]) & SCHEME_PRIM_IS_MULTI_RESULT)) {
        note_match(1, vals, warn_info);
      } else if (SAME_OBJ(scheme_values_func, app->args[0])) {
        note_match(app->num_args, vals, warn_info);
      }
    }
  
    return 0;
  }

  if (vtype == scheme_application2_type) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
    if (SCHEME_PRIMP(app->rator)) {
      if ((SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_OMITABLE)
          && (1 >= ((Scheme_Primitive_Proc *)app->rator)->mina)
          && (1 <= ((Scheme_Primitive_Proc *)app->rator)->mu.maxa)
          && ((vals < 0) 
              || ((vals == 1) && !(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT))
              || ((vals == 1) && SAME_OBJ(scheme_values_func, app->rator)))) {
        if (scheme_omittable_expr(app->rand, 1, fuel - 1, resolved, warn_info,
                                  deeper_than + (resolved ? 1 : 0), 0))
          return 1;
      } else if (!(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT)
                 || SAME_OBJ(scheme_values_func, app->rator)) {
        note_match(1, vals, warn_info);
      }
    }
    return 0;
  }

  if (vtype == scheme_application3_type) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
    if (SCHEME_PRIMP(app->rator)) {
      if ((SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_OMITABLE)
          && (2 >= ((Scheme_Primitive_Proc *)app->rator)->mina)
          && (2 <= ((Scheme_Primitive_Proc *)app->rator)->mu.maxa)
          && ((vals < 0) 
              || ((vals == 1) && !(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT))
              || ((vals == 2) && SAME_OBJ(scheme_values_func, app->rator)))) {
        if (scheme_omittable_expr(app->rand1, 1, fuel - 1, resolved, warn_info,
                                  deeper_than + (resolved ? 2 : 0), 0)
            && scheme_omittable_expr(app->rand2, 1, fuel - 1, resolved, warn_info,
                                     deeper_than + (resolved ? 2 : 0), 0))
          return 1;
      } else if (!(SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_MULTI_RESULT)) {
        note_match(1, vals, warn_info);
      } else if (SAME_OBJ(scheme_values_func, app->rator)) {
        note_match(2, vals, warn_info);
      }
    }
    return 0;
  }

  return 0;
}

static int single_valued_noncm_expression(Scheme_Object *expr, int fuel)
/* Non-omittable but single-valued expresions that are not sensitive
   to being in tail position. */
{
  Scheme_Object *rator = NULL;

 switch (SCHEME_TYPE(expr)) {
 case scheme_compiled_toplevel_type:
   return 1;
 case scheme_application_type:
   rator = ((Scheme_App_Rec *)expr)->args[0];
   break;
 case scheme_application2_type:
   rator = ((Scheme_App2_Rec *)expr)->rator;
   break;
 case scheme_application3_type:
   rator = ((Scheme_App2_Rec *)expr)->rator;
   break;
 case scheme_compiled_let_void_type:
   {
     Scheme_Let_Header *lh = (Scheme_Let_Header *)expr;
     Scheme_Compiled_Let_Value *clv;
     if ((lh->count == 1) && (lh->num_clauses == 1) && (fuel > 0)) {
       clv = (Scheme_Compiled_Let_Value *)lh->body;
       return single_valued_noncm_expression(clv->body, fuel - 1);
     }
   }
   break;
 }

 if (rator && SCHEME_PRIMP(rator)) {
   int opt;
   opt = ((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_OPT_MASK;
   if (opt >= SCHEME_PRIM_OPT_NONCM)
     return 1;
 }

 return 0;
}

static int is_movable_prim(Scheme_Object *rator, int n, int cross_lambda)
/* A -1 return means that the arguments must be movable without
   changing space complexity. */
{
  if (rator && SCHEME_PRIMP(rator)) {
    if (((Scheme_Prim_Proc_Header *)rator)->flags & SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL) {
      /* Although it's semantically ok to return -1 even when cross_lambda,
         doing so risks duplicating a computation if the relevant `lambda'
         is later inlined. */
      if (cross_lambda) return 0;
      return -1;
    }
  }

  if (SAME_OBJ(scheme_void_proc, rator))
    return -1;

  if (!cross_lambda
      /* Note that none of these have space-safety issues, since they
         return values that contain all arguments: */
      && (SAME_OBJ(scheme_list_proc, rator)
          || (SAME_OBJ(scheme_cons_proc, rator) && (n == 2))
          || SAME_OBJ(scheme_list_star_proc, rator)
          || SAME_OBJ(scheme_vector_proc, rator)
          || SAME_OBJ(scheme_vector_immutable_proc, rator)
          || (SAME_OBJ(scheme_box_proc, rator) && (n == 1))))
    return 1;

  return 0;
}

static int movable_expression(Scheme_Object *expr, Optimize_Info *info, int delta, int cross_lambda, 
                              int check_space, int fuel)
/* An expression that can't necessarily be constant-folded,
   but can be delayed because it has no side-effects (or is unsafe);
   also not sensitive to being in tail position */
{
  int can_move;

  if (fuel < 0) return 0;

  switch (SCHEME_TYPE(expr)) {
  case scheme_toplevel_type:
    return ((SCHEME_TOPLEVEL_FLAGS(expr) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED);
  case scheme_compiled_quote_syntax_type:
    return 1;
  case scheme_local_type:
    {
      /* Ok if not mutable */
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos + delta < 0)
        return 1;
      else if (!optimize_is_mutated(info, pos + delta)) {
        if (check_space) {
          if (optimize_is_flonum_valued(info, pos + delta))
            return 1;
          /* the value of the identifier might be something that would
             retain significant memory, so we can't delay evaluation */
          return 0;
        }
        return 1;
      }
    }
    break;
  case scheme_application_type:
    can_move = is_movable_prim(((Scheme_App_Rec *)expr)->args[0], ((Scheme_App_Rec *)expr)->num_args, cross_lambda);
    if (can_move) {
      int i;
      for (i = ((Scheme_App_Rec *)expr)->num_args; i--; ) {
        if (!movable_expression(((Scheme_App_Rec *)expr)->args[i+1], info, delta, cross_lambda, 
                                check_space || (can_move < 0), fuel - 1))
          return 0;
      }
      return 1;
    }
    break;
  case scheme_application2_type:
    can_move = is_movable_prim(((Scheme_App2_Rec *)expr)->rator, 1, cross_lambda);
    if (can_move) {
      if (movable_expression(((Scheme_App2_Rec *)expr)->rand, info, delta, cross_lambda, 
                             check_space || (can_move < 0), fuel - 1))
        return 1;
    }
    break;
  case scheme_application3_type:
    can_move = is_movable_prim(((Scheme_App3_Rec *)expr)->rator, 2, cross_lambda);
    if (can_move) {
      if (movable_expression(((Scheme_App3_Rec *)expr)->rand1, info, delta, cross_lambda, 
                             check_space || (can_move < 0), fuel - 1)
          && movable_expression(((Scheme_App3_Rec *)expr)->rand2, info, delta, cross_lambda, 
                                check_space || (can_move < 0), fuel - 1))
        return 1;
    }
    break;
  default:
    if (SCHEME_TYPE(expr) > _scheme_compiled_values_types_)
      return 1;
  }

  return 0;
}

int scheme_is_compiled_procedure(Scheme_Object *o, int can_be_closed, int can_be_liftable)
{
  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compiled_unclosed_procedure_type)) {
    if (!can_be_closed || !can_be_liftable) {
      Scheme_Closure_Data *data;
      data = (Scheme_Closure_Data *)o;
      /* Because == 0 is like a constant */
      if (!can_be_closed && !data->closure_size)
        return 0;
      /* Because procs that reference only globals are lifted: */
      if (!can_be_liftable && (data->closure_size == 1) && closure_has_top_level(data))
        return 0;
    }
    return 1;
  } else
    return 0;
}

/*========================================================================*/
/*                   applications, branches, sequences                    */
/*========================================================================*/

static Scheme_Object *finish_optimize_application(Scheme_App_Rec *app, Optimize_Info *info, int context, int rator_flags);
static Scheme_Object *finish_optimize_application2(Scheme_App2_Rec *app, Optimize_Info *info, int context, int rator_flags);
static Scheme_Object *finish_optimize_application3(Scheme_App3_Rec *app, Optimize_Info *info, int context, int rator_flags);

static Scheme_Object *try_optimize_fold(Scheme_Object *f, Scheme_Object *o, Optimize_Info *info)
{
  if ((SCHEME_PRIMP(f)
       && ((((Scheme_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_OPT_MASK)
           == SCHEME_PRIM_OPT_FOLDING))
      || (SCHEME_CLSD_PRIMP(f)
	  && ((((Scheme_Closed_Primitive_Proc *)f)->pp.flags & SCHEME_PRIM_OPT_MASK)
              == SCHEME_PRIM_OPT_FOLDING))) {
    Scheme_Object *args;

    switch (SCHEME_TYPE(o)) {
    case scheme_application_type:
      {
	Scheme_App_Rec *app = (Scheme_App_Rec *)o;
	int i;

	args = scheme_null;
	for (i = app->num_args; i--; ) {
	  args = scheme_make_pair(app->args[i + 1], args);
	}
      }
      break;
    case scheme_application2_type:
      {
	Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
	args = scheme_make_pair(app->rand, scheme_null);
      }
      break;
    case scheme_application3_type:
    default:
      {
	Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
	args = scheme_make_pair(app->rand1,
				scheme_make_pair(app->rand2,
						 scheme_null));
      }
      break;
    }

    return scheme_try_apply(f, args, info->context);
  }

  return NULL;
}

static int estimate_expr_size(Scheme_Object *expr, int sz, int fuel)
{
  Scheme_Type t;

  if (sz > 128)
    return sz;
  if (fuel < 0)
    return sz + 128;

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_local_type:
    {
      sz += 1;
      break;
    }
  case scheme_case_lambda_sequence_type:
    {
      int max_sz = sz + 1, a_sz;
      Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)expr;
      int i;
      for (i = cl->count; i--; ) {
        a_sz = estimate_expr_size(cl->array[i], sz, fuel);
        if (a_sz > max_sz) max_sz = a_sz;
      }
      sz = max_sz;
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;

      sz = estimate_expr_size(app->rator, sz, fuel - 1);
      sz = estimate_expr_size(app->rand, sz, fuel - 1);
      sz++;

      break;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i;

      for (i = app->num_args + 1; i--; ) {
        sz = estimate_expr_size(app->args[i], sz, fuel - 1);
      }
      sz++;

      break;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;

      sz = estimate_expr_size(app->rator, sz, fuel - 1);
      sz = estimate_expr_size(app->rand1, sz, fuel - 1);
      sz = estimate_expr_size(app->rand2, sz, fuel - 1);
      sz++;

      break;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv;
      int i;

      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;
        sz = estimate_expr_size(lv->value, sz, fuel - 1);
	body = lv->body;
        sz++;
      }
      sz = estimate_expr_size(body, sz, fuel - 1);
      break;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      int i;

      for (i = seq->count; i--; ) {
	sz = estimate_expr_size(seq->array[i], sz, fuel - 1);
      }

      break;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;

      sz = estimate_expr_size(b->test, sz, fuel - 1);
      sz = estimate_expr_size(b->tbranch, sz, fuel - 1);
      sz = estimate_expr_size(b->fbranch, sz, fuel - 1);
      break;
    }
  case scheme_compiled_unclosed_procedure_type:
    {
      sz = estimate_expr_size(((Scheme_Closure_Data *)expr)->code, sz, fuel - 1);
      sz++;
      break;
    }
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    /* FIXME: other syntax types not covered */
  default:
    sz += 1;
    break;
  }

  return sz;
}

static Scheme_Object *estimate_closure_size(Scheme_Object *e)
{
  int sz;
  sz = estimate_expr_size(e, 0, 32);
  return scheme_box(scheme_make_integer(sz));
}

static Scheme_Object *no_potential_size(Scheme_Object *v)
{
  if (v && SCHEME_BOXP(v))
    return NULL;
  else
    return v;
}

static Scheme_Object *apply_inlined(Scheme_Object *p, Scheme_Closure_Data *data, Optimize_Info *info,
				    int argc, Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                    int context,
                                    int nested_count, Scheme_Object *orig, Scheme_Object *le_prev, intptr_t prev_offset)
{
  Scheme_Let_Header *lh;
  Scheme_Compiled_Let_Value *lv, *prev = NULL;
  Scheme_Object *val;
  int i, expected;
  int *flags, flag;
  Optimize_Info *sub_info;

  expected = data->num_params;

  if (!expected) {
    info = optimize_info_add_frame(info, 0, 0, 0);
    info->inline_fuel >>= 1;
    p = scheme_optimize_expr(p, info, context);
    info->next->single_result = info->single_result;
    info->next->preserves_marks = info->preserves_marks;
    optimize_info_done(info, NULL);

    if (le_prev) {
      *((Scheme_Object **)(((char *)le_prev) + prev_offset)) = p;
      return orig;
    } else
      return p;
  }

  lh = MALLOC_ONE_TAGGED(Scheme_Let_Header);
  lh->iso.so.type = scheme_compiled_let_void_type;
  lh->count = expected;
  lh->num_clauses = expected;

  for (i = 0; i < expected; i++) {
    lv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
    lv->iso.so.type = scheme_compiled_let_value_type;
    lv->count = 1;
    lv->position = i;

    if ((i == expected - 1)
        && (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)) {
      int j;
      Scheme_Object *l = scheme_null;

      for (j = argc; j-- > i; ) {
        if (app)
          val = app->args[j + 1];
        else if (app3)
          val = (j ? app3->rand2 : app3->rand1);
        else if (app2)
          val = app2->rand;
        else
          val = scheme_false;

        l = cons(val, l);
      }
      l = cons(scheme_list_proc, l);
      val = scheme_make_application(l);
    } else if (app)
      val = app->args[i + 1];
    else if (app3)
      val = (i ? app3->rand2 : app3->rand1);
    else
      val = app2->rand;

    if (nested_count)
      val = optimize_shift(val, nested_count, 0);
    lv->value = val;

    flag = closure_argument_flags(data, i);
    flags = (int *)scheme_malloc_atomic(sizeof(int));
    flags[0] = flag;
    lv->flags = flags;

    if (prev)
      prev->body = (Scheme_Object *)lv;
    else
      lh->body = (Scheme_Object *)lv;
    prev = lv;
  }

  if (prev)
    prev->body = p;
  else
    lh->body = p;

  sub_info = optimize_info_add_frame(info, 0, 0, 0);
  sub_info->inline_fuel >>= 1;

  p = scheme_optimize_lets((Scheme_Object *)lh, sub_info, 1, context);

  info->single_result = sub_info->single_result;
  info->preserves_marks = sub_info->preserves_marks;
  optimize_info_done(sub_info, NULL);

  if (le_prev) {
    *((Scheme_Object **)(((char *)le_prev) + prev_offset)) = p;
    return orig;
  } else
    return p;
}

int scheme_check_leaf_rator(Scheme_Object *le, int *_flags)
{
  if (le && SCHEME_PRIMP(le)) {
    int opt;
    opt = ((Scheme_Prim_Proc_Header *)le)->flags & SCHEME_PRIM_OPT_MASK;
    if (opt >= SCHEME_PRIM_OPT_NONCM) {
      if (_flags)
        *_flags = (CLOS_PRESERVES_MARKS | CLOS_SINGLE_RESULT);
      if (opt >= SCHEME_PRIM_OPT_IMMEDIATE) {
        return 1;
      }
    }
  }

  return 0;
}

#if 0
# define LOG_INLINE(x) x
#else
# define LOG_INLINE(x) /*empty*/
#endif

Scheme_Object *optimize_for_inline(Optimize_Info *info, Scheme_Object *le, int argc,
				   Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                   int *_flags, int context, int optimized_rator)
/* Zero or one of app, app2 and app3 should be non-NULL.
   If app, we're inlining a general application. If app2, we're inlining an
   application with a single argument and if app3, we're inlining an
   application with two arguments.
   If not app, app2, or app3, just return a known procedure, if any,
   and do not check arity. */
{
  int offset = 0, single_use = 0, psize = 0;
  Scheme_Object *bad_app = NULL, *prev = NULL, *orig_le = le;
  intptr_t prev_offset = 0;
  int nested_count = 0, outside_nested = 0, already_opt = optimized_rator, nonleaf;

  if ((info->inline_fuel < 0) && info->has_nonleaf)
    return NULL;

  /* Move inside `let' bindings, so we can convert ((let (....) proc) arg ...)
     to (let (....) (proc arg ...)) */
  while (optimized_rator && SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_let_void_type)) {
    Scheme_Let_Header *lh;
    int i;

    lh = (Scheme_Let_Header *)le;
    prev = le;
    prev_offset = (intptr_t)&(((Scheme_Let_Header *)0x0)->body);
    le = lh->body;
    for (i = 0; i < lh->num_clauses; i++) {
      prev = le;
      prev_offset = (intptr_t)&(((Scheme_Compiled_Let_Value *)0x0)->body);
      le = ((Scheme_Compiled_Let_Value *)le)->body;
    }
    nested_count += lh->count;
  }

  if (SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_unclosed_procedure_type)) {
    /* Found a `((lambda' */
    single_use = 1;
  }

  if (!optimized_rator && SAME_TYPE(SCHEME_TYPE(le), scheme_local_type)) {
    /* Check for inlining: */
    le = optimize_info_lookup(info, SCHEME_LOCAL_POS(le), &offset, &single_use, 0, 0, &psize, NULL);
    outside_nested = 1;
    already_opt = 1;
  }

  if (le) {
    while (SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_toplevel_type)) {
      int pos;
      pos = SCHEME_TOPLEVEL_POS(le);
      single_use = 0;
      if (info->cp->inline_variants) {
        Scheme_Object *iv;
        iv = scheme_hash_get(info->cp->inline_variants, scheme_make_integer(pos));
        if (iv && SCHEME_TRUEP(iv)) {
          Scheme_Hash_Table *iv_ht = NULL;
          if (SCHEME_HASHTP(iv)) {
            iv_ht = (Scheme_Hash_Table *)iv;
            iv = scheme_hash_get(iv_ht, scheme_make_integer(argc));
            if (!iv)
              iv = scheme_hash_get(iv_ht, scheme_false);
          }
          if (SAME_TYPE(SCHEME_TYPE(iv), scheme_inline_variant_type)) {
            int has_cases = 0;
            Scheme_Object *orig_iv = iv;
            iv = scheme_unresolve(iv, argc, &has_cases);
            if (has_cases) {
              if (!iv_ht) {
                iv_ht = scheme_make_hash_table(SCHEME_hash_ptr);
                scheme_hash_set(iv_ht, scheme_false, orig_iv);
                scheme_hash_set(info->cp->inline_variants, scheme_make_integer(pos), (Scheme_Object *)iv_ht);
              }
              scheme_hash_set(iv_ht, scheme_make_integer(argc), iv ? iv : scheme_false);
            } else
              scheme_hash_set(info->cp->inline_variants, scheme_make_integer(pos), iv ? iv : scheme_false);
          }
          if (iv && SCHEME_TRUEP(iv)) {
            le = iv;
            break;
          }
        }
      }
      if (SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_toplevel_type) && info->top_level_consts) {
        le = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        if (le && SCHEME_BOXP(le)) {
          psize = SCHEME_INT_VAL(SCHEME_BOX_VAL(le));
          le = NULL;
        }
        if (!le)
          break;
        outside_nested = 1;
        already_opt = 1;
      } else
        break;
    }
  }

  if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_case_lambda_sequence_type)) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)le;
    Scheme_Object *cp;
    int i, count;

    if (!app && !app2 && !app3)
      return le;

    count = cl->count;
    for (i = 0; i < count; i++) {
      cp = cl->array[i];
      if (SAME_TYPE(SCHEME_TYPE(cp), scheme_compiled_unclosed_procedure_type)) {
        Scheme_Closure_Data *data = (Scheme_Closure_Data *)cp;
        if ((data->num_params == argc)
            || ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
                && (argc + 1 >= data->num_params))) {
          le = cp;
          break;
        }
      } else {
        scheme_signal_error("internal error: strange case-lambda");
      }
    }
    if (i >= count)
      bad_app = le;
  }

  nonleaf = 1;

  if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_unclosed_procedure_type) && (info->inline_fuel >= 0)) {
    Scheme_Closure_Data *data = (Scheme_Closure_Data *)le;
    int sz;

    if (!app && !app2 && !app3)
      return le;

    *_flags = SCHEME_CLOSURE_DATA_FLAGS(data);

    if ((data->num_params == argc)
        || ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
            && (argc + 1 >= data->num_params))
        || (!app && !app2 && !app3)) {
      int threshold, is_leaf = 0;

      if (!already_opt) {
        /* We have an immediate `lambda' that wasn't optimized, yet.
           Go optimize it, first. */
        return NULL;
      }

      sz = closure_body_size(data, 1, info, &is_leaf);
      if (is_leaf) {
        /* encourage inlining of leaves: */
        sz >>= 2;
      }
      threshold = info->inline_fuel * (2 + argc);

      /* Do we have enough fuel? */
      if ((sz >= 0) && (single_use || (sz <= threshold))) {
        Optimize_Info *sub_info;
        if (nested_count) {
          sub_info = optimize_info_add_frame(info, nested_count, nested_count, 0);
          sub_info->vclock++;
          /* We could propagate bound values in sub_info, but relevant inlining
             and propagatation has probably already happened when the rator was
             optimized. */
        } else
          sub_info = info;

	/* If optimize_clone succeeds, inlining succeeds. */
        le = optimize_clone(single_use, data->code, sub_info,
                            offset + (outside_nested ? nested_count : 0),
                            data->num_params);

	if (le) {
	  LOG_INLINE(fprintf(stderr, "Inline %d[%d]<=%d@%d %d %s\n", sz, is_leaf, threshold, info->inline_fuel,
                             single_use, scheme_write_to_string(data->name ? data->name : scheme_false, NULL)));
	  scheme_log(NULL,
		     SCHEME_LOG_DEBUG,
		     0,
		     "mzc optimizer: inlining: involving: %s%s size: %d threshold: %d",
		     scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		     scheme_optimize_context_to_string(info->context),
		     sz,
		     threshold);
          le = apply_inlined(le, data, sub_info, argc, app, app2, app3, context,
                             nested_count, orig_le, prev, prev_offset);
          if (nested_count)
            optimize_info_done(sub_info, NULL);
          return le;
	} else {
          LOG_INLINE(fprintf(stderr, "No inline %s\n", scheme_write_to_string(data->name ? data->name : scheme_false, NULL)));
	  scheme_log(NULL,
		     SCHEME_LOG_DEBUG,
		     0,
		     "mzc optimizer: no inlining: involving: %s%s size: %d threshold: %d",
		     scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		     scheme_optimize_context_to_string(info->context),
		     sz,
		     threshold);
        }
      } else {
        LOG_INLINE(fprintf(stderr, "No fuel %s %d[%d]>%d@%d %d\n", scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
                           sz, is_leaf, threshold,
                           info->inline_fuel, info->use_psize));
	scheme_log(NULL,
		   SCHEME_LOG_DEBUG,
		   0,
		   "mzc optimizer: no inlining, out of fuel: involving: %s%s size: %d threshold: %d",
		   scheme_write_to_string(data->name ? data->name : scheme_false, NULL),
		   scheme_optimize_context_to_string(info->context),
		   sz,
		   threshold);
      }
    } else {
      /* Issue warning below */
      bad_app = (Scheme_Object *)data;
      nonleaf = 0;
    }
  }

  if (scheme_check_leaf_rator(le, _flags))
    nonleaf = 0;

  if (le && SCHEME_PROCP(le) && (app || app2 || app3)) {
    Scheme_Object *a[1];
    a[0] = le;
    if (!scheme_check_proc_arity(NULL, argc, 0, 1, a))  {
      bad_app = le;
      nonleaf = 0;
    }
  }

  if (psize) {
    LOG_INLINE(fprintf(stderr, "Potential inline %d %d\n", psize, info->inline_fuel * (argc + 2)));
    /* If we inline, the enclosing function will get larger, so we increase
       its potential size. */
    if (psize <= (info->inline_fuel * (argc + 2)))
      info->psize += psize;
  }

  if (nonleaf)
    info->has_nonleaf = 1;

  if (bad_app) {
    int len;
    const char *pname, *context;
    pname = scheme_get_proc_name(bad_app, &len, 0);
    context = scheme_optimize_context_to_string(info->context);
    scheme_log(NULL,
               SCHEME_LOG_WARNING,
               0,
               "warning%s: optimizer detects procedure incorrectly applied to %d arguments%s%s",
               context,
               argc,
               pname ? ": " : "",
               pname ? pname : "");
  }

  return NULL;
}

static int is_flonum_expression(Scheme_Object *expr, Optimize_Info *info)
{
  if (scheme_expr_produces_flonum(expr))
    return 1;

  if (SAME_TYPE(SCHEME_TYPE(expr), scheme_local_type)) {
    if (optimize_is_flonum_valued(info, SCHEME_LOCAL_POS(expr)))
      return 1;
  }

  return 0;
}

static void register_flonum_argument_types(Scheme_App_Rec *app, Scheme_App2_Rec *app2, Scheme_App3_Rec *app3,
                                           Optimize_Info *info)
{
  Scheme_Object *rator, *rand, *le;
  int n, i;

  if (app) {
    rator = app->args[0];
    n = app->num_args;
  } else if (app2) {
    rator = app2->rator;
    n = 1;
  } else {
    rator = app3->rator;
    n = 2;
  }

  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type)) {
    rator = optimize_reverse(info, SCHEME_LOCAL_POS(rator), 1, 0);
    if (rator) {
      int offset, single_use;
      le = optimize_info_lookup(info, SCHEME_LOCAL_POS(rator), &offset, &single_use, 0, 0, NULL, NULL);
      if (le && SAME_TYPE(SCHEME_TYPE(le), scheme_compiled_unclosed_procedure_type)) {
        Scheme_Closure_Data *data = (Scheme_Closure_Data *)le;
        char *map;
        int ok;

        map = get_closure_flonum_map(data, n, &ok);

        if (ok) {
          for (i = 0; i < n; i++) {
            int is_flonum;

            if (app)
              rand = app->args[i+1];
            else if (app2)
              rand = app2->rand;
            else {
              if (!i)
                rand = app3->rand1;
              else
                rand = app3->rand2;
            }

            is_flonum = is_flonum_expression(rand, info);
            if (is_flonum) {
              if (!map) {
                map = MALLOC_N_ATOMIC(char, n);
                memset(map, 1, n);
                memset(map, 0, i);
              }
            }
            if (map && !is_flonum)
              map[i] = 0;
          }

          set_closure_flonum_map(data, map);
        }
      }
    }
  }
}

char *scheme_optimize_context_to_string(Scheme_Object *context)
{
  if (context) {
    Scheme_Object *mod, *func;
    const char *ctx, *prefix, *mctx, *mprefix;
    char *all;
    int clen, plen, mclen, mplen, len;

    if (SCHEME_PAIRP(context)) {
      func = SCHEME_CAR(context);
      mod = SCHEME_CDR(context);
    } else if (SAME_TYPE(SCHEME_TYPE(context), scheme_module_type)) {
      func = scheme_false;
      mod = context;
    } else {
      func = context;
      mod = scheme_false;
    }

    if (SAME_TYPE(SCHEME_TYPE(func), scheme_compiled_unclosed_procedure_type)) {
      Scheme_Object *name;

      name = ((Scheme_Closure_Data *)func)->name;
      if (name) {
        if (SCHEME_VECTORP(name)) {
          Scheme_Object *port;
          int print_width = 1024;
          intptr_t plen;

          port = scheme_make_byte_string_output_port();

          scheme_write_proc_context(port, print_width,
                                    SCHEME_VEC_ELS(name)[0],
                                    SCHEME_VEC_ELS(name)[1], SCHEME_VEC_ELS(name)[2],
                                    SCHEME_VEC_ELS(name)[3], SCHEME_VEC_ELS(name)[4],
                                    SCHEME_TRUEP(SCHEME_VEC_ELS(name)[6]));

          ctx = scheme_get_sized_byte_string_output(port, &plen);
          prefix = " in: ";
        } else {
          ctx = scheme_get_proc_name(func, &len, 0);
          prefix = " in: ";
        }
      } else {
        ctx = "";
        prefix = "";
      }
    } else {
      ctx = "";
      prefix = "";
    }

    if (SAME_TYPE(SCHEME_TYPE(mod), scheme_module_type)) {
      mctx = scheme_display_to_string(((Scheme_Module *)mod)->modsrc, NULL);
      mprefix = " in module: ";
    } else {
      mctx = "";
      mprefix = "";
    }

    clen = strlen(ctx);
    plen = strlen(prefix);
    mclen = strlen(mctx);
    mplen = strlen(mprefix);

    if (!clen && !mclen)
      return "";

    all = scheme_malloc_atomic(clen + plen + mclen + mplen + 1);
    memcpy(all, prefix, plen);
    memcpy(all + plen, ctx, clen);
    memcpy(all + plen + clen, mprefix, mplen);
    memcpy(all + plen + clen + mplen, mctx, mclen);
    all[clen + plen + mclen + mplen] = 0;
    return all;
  } else
    return "";
}

static void reset_rator(Scheme_Object *app, Scheme_Object *a)
{
  switch (SCHEME_TYPE(app)) {
  case scheme_application_type:
    ((Scheme_App_Rec *)app)->args[0] = a;
    break;
  case scheme_application2_type:
    ((Scheme_App2_Rec *)app)->rator = a;
    break;
  case scheme_application3_type:
    ((Scheme_App3_Rec *)app)->rator = a;
    break;
  }
}

static Scheme_Object *check_app_let_rator(Scheme_Object *app, Scheme_Object *rator, Optimize_Info *info,
                                          int argc, int context)
{
  /* Convert ((let (....) E) arg ...) to (let (....) (E arg ...)), in case
     the `let' is immediately apparent. We check for this pattern again
     in optimize_for_inline() after optimizing a rator. */
  if (SAME_TYPE(SCHEME_TYPE(rator), scheme_compiled_let_void_type)) {
    Scheme_Let_Header *head = (Scheme_Let_Header *)rator;
    Scheme_Compiled_Let_Value *clv;
    int i;

    /* Handle ((let ([f ...]) f) arg ...) specially, so we can
       adjust the flags for `f': */
    if ((head->count == 1) && (head->num_clauses == 1)) {
      clv = (Scheme_Compiled_Let_Value *)head->body;
      rator = clv->body;
      if (SAME_TYPE(SCHEME_TYPE(rator), scheme_local_type)
          && (SCHEME_LOCAL_POS(rator) == 0)
          && scheme_is_compiled_procedure(clv->value, 1, 1)) {

        reset_rator(app, scheme_false);
        app = optimize_shift(app, 1, 0);
        reset_rator(app, scheme_make_local(scheme_local_type, 0, 0));

        clv->body = app;

        if (clv->flags[0] & SCHEME_WAS_APPLIED_EXCEPT_ONCE) {
          clv->flags[0] -= SCHEME_WAS_APPLIED_EXCEPT_ONCE;
          clv->flags[0] |= SCHEME_WAS_ONLY_APPLIED;
        }

        return scheme_optimize_expr((Scheme_Object *)head, info, context);
      }
    }

    clv = NULL;
    rator = head->body;
    for (i = head->num_clauses; i--; ) {
      clv = (Scheme_Compiled_Let_Value *)rator;
      rator = clv->body;
    }

    reset_rator(app, scheme_false);
    app = optimize_shift(app, head->count, 0);
    reset_rator(app, rator);

    if (clv)
      clv->body = app;
    else
      head->body = app;

    return scheme_optimize_expr((Scheme_Object *)head, info, context);
  }

  return NULL;
}

static int is_nonmutating_primitive(Scheme_Object *rator, int n)
{
  if (SCHEME_PRIMP(rator)
      && (SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_OMITABLE)
      && (n >= ((Scheme_Primitive_Proc *)rator)->mina)
      && (n <= ((Scheme_Primitive_Proc *)rator)->mu.maxa))
    return 1;

  return 0;
}

#define IS_NAMED_PRIM(p, nm) (!strcmp(((Scheme_Primitive_Proc *)p)->name, nm))

int scheme_wants_flonum_arguments(Scheme_Object *rator, int argpos, int rotate_mode)
/* In rotate mode, we really want to know whether any argument wants to be lifted out. */
{
  if (SCHEME_PRIMP(rator)) {
    if (SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_UNSAFE_NONMUTATING) {
      if (IS_NAMED_PRIM(rator, "unsafe-flabs")
          || IS_NAMED_PRIM(rator, "unsafe-flsqrt")
          || IS_NAMED_PRIM(rator, "unsafe-fl+")
          || IS_NAMED_PRIM(rator, "unsafe-fl-")
          || IS_NAMED_PRIM(rator, "unsafe-fl*")
          || IS_NAMED_PRIM(rator, "unsafe-fl/")
          || IS_NAMED_PRIM(rator, "unsafe-fl<")
          || IS_NAMED_PRIM(rator, "unsafe-fl<=")
          || IS_NAMED_PRIM(rator, "unsafe-fl=")
          || IS_NAMED_PRIM(rator, "unsafe-fl>")
          || IS_NAMED_PRIM(rator, "unsafe-fl>=")
          || IS_NAMED_PRIM(rator, "unsafe-flmin")
          || IS_NAMED_PRIM(rator, "unsafe-flmax")
          || (!rotate_mode && IS_NAMED_PRIM(rator, "unsafe-fl->fx"))
          || (rotate_mode && IS_NAMED_PRIM(rator, "unsafe-flvector-ref"))
          || (rotate_mode && IS_NAMED_PRIM(rator, "unsafe-fx->fl")))
        return 1;
    } else if (SCHEME_PRIM_IS_SOMETIMES_INLINED(rator)) {
      if (!rotate_mode) {
        if (IS_NAMED_PRIM(rator, "flabs")
            || IS_NAMED_PRIM(rator, "flsqrt")
            || IS_NAMED_PRIM(rator, "fltruncate")
            || IS_NAMED_PRIM(rator, "flround")
            || IS_NAMED_PRIM(rator, "flfloor")
            || IS_NAMED_PRIM(rator, "flceiling")
            || IS_NAMED_PRIM(rator, "flsin")
            || IS_NAMED_PRIM(rator, "flcos")
            || IS_NAMED_PRIM(rator, "fltan")
            || IS_NAMED_PRIM(rator, "flasin")
            || IS_NAMED_PRIM(rator, "flacos")
            || IS_NAMED_PRIM(rator, "flatan")
            || IS_NAMED_PRIM(rator, "fllog")
            || IS_NAMED_PRIM(rator, "flexp")
            || IS_NAMED_PRIM(rator, "fl+")
            || IS_NAMED_PRIM(rator, "fl-")
            || IS_NAMED_PRIM(rator, "fl*")
            || IS_NAMED_PRIM(rator, "fl/")
            || IS_NAMED_PRIM(rator, "fl<")
            || IS_NAMED_PRIM(rator, "fl<=")
            || IS_NAMED_PRIM(rator, "fl=")
            || IS_NAMED_PRIM(rator, "fl>")
            || IS_NAMED_PRIM(rator, "flmin")
            || IS_NAMED_PRIM(rator, "flmax"))
          return 1;
      }
      if ((rotate_mode || (argpos == 2))
          && IS_NAMED_PRIM(rator, "unsafe-flvector-set!"))
        return 1;
      if (!rotate_mode && (argpos == 2)
          && IS_NAMED_PRIM(rator, "flvector-set!"))
        return 1;
    }
  }

  return 0;
}

static int produces_unboxed(Scheme_Object *rator, int *non_fl_args, int argc, int for_args)
{
  if (SCHEME_PRIMP(rator)) {
    if (SCHEME_PRIM_PROC_FLAGS(rator) & SCHEME_PRIM_IS_UNSAFE_NONMUTATING) {
      if (((argc == 1)
           && (IS_NAMED_PRIM(rator, "unsafe-flabs")
               || IS_NAMED_PRIM(rator, "unsafe-flsqrt")
               || IS_NAMED_PRIM(rator, "unsafe-flreal-part")
               || IS_NAMED_PRIM(rator, "unsafe-flimag-part")))
          || ((argc == 2)
              && (IS_NAMED_PRIM(rator, "unsafe-fl+")
                  || IS_NAMED_PRIM(rator, "unsafe-fl-")
                  || IS_NAMED_PRIM(rator, "unsafe-fl*")
                  || IS_NAMED_PRIM(rator, "unsafe-fl/")
                  || IS_NAMED_PRIM(rator, "unsafe-flmin")
                  || IS_NAMED_PRIM(rator, "unsafe-flmax")
                  || (for_args
                      && (IS_NAMED_PRIM(rator, "unsafe-fl<")
                          || IS_NAMED_PRIM(rator, "unsafe-fl<=")
                          || IS_NAMED_PRIM(rator, "unsafe-fl=")
                          || IS_NAMED_PRIM(rator, "unsafe-fl>")
                          || IS_NAMED_PRIM(rator, "unsafe-fl>="))))))
        return 1;
      if (((argc == 2) && IS_NAMED_PRIM(rator, "unsafe-flvector-ref"))
          || ((argc == 1) && IS_NAMED_PRIM(rator, "unsafe-fx->fl"))) {
        if (non_fl_args) *non_fl_args = 1;
        return 1;
      }
    } else if ((argc == 1) && SCHEME_PRIM_IS_SOMETIMES_INLINED(rator)) {
      if (IS_NAMED_PRIM(rator, "flabs")
          || IS_NAMED_PRIM(rator, "flsqrt")
          || IS_NAMED_PRIM(rator, "fltruncate")
          || IS_NAMED_PRIM(rator, "flround")
          || IS_NAMED_PRIM(rator, "flfloor")
          || IS_NAMED_PRIM(rator, "flceiling")
          || IS_NAMED_PRIM(rator, "flsin")
          || IS_NAMED_PRIM(rator, "flcos")
          || IS_NAMED_PRIM(rator, "fltan")
          || IS_NAMED_PRIM(rator, "flasin")
          || IS_NAMED_PRIM(rator, "flacos")
          || IS_NAMED_PRIM(rator, "flatan")
          || IS_NAMED_PRIM(rator, "fllog")
          || IS_NAMED_PRIM(rator, "flexp")
          || IS_NAMED_PRIM(rator, "flimag-part")
          || IS_NAMED_PRIM(rator, "flreal-part"))
        return 1;
      if (IS_NAMED_PRIM(rator, "->fl")) {
        if (non_fl_args) *non_fl_args = 1;
        return 1;
      }
    } else if ((argc ==2) && SCHEME_PRIM_IS_SOMETIMES_INLINED(rator)) {
      if (IS_NAMED_PRIM(rator, "flabs")
          || IS_NAMED_PRIM(rator, "flsqrt")
          || IS_NAMED_PRIM(rator, "fl+")
          || IS_NAMED_PRIM(rator, "fl-")
          || IS_NAMED_PRIM(rator, "fl*")
          || IS_NAMED_PRIM(rator, "fl/")
          || IS_NAMED_PRIM(rator, "flmin")
          || IS_NAMED_PRIM(rator, "flmax")
          || (for_args
              && (IS_NAMED_PRIM(rator, "fl<")
                  || IS_NAMED_PRIM(rator, "fl<=")
                  || IS_NAMED_PRIM(rator, "fl=")
                  || IS_NAMED_PRIM(rator, "fl>")
                  || IS_NAMED_PRIM(rator, "fl>="))))
        return 1;
      if (IS_NAMED_PRIM(rator, "flvector-ref")) {
        if (non_fl_args) *non_fl_args = 1;
        return 1;
      }
    }
  }

  return 0;
}

static int is_unboxed_argument(Scheme_Object *rand, int fuel, Optimize_Info *info, int lifted)
{
  if (fuel > 0) {
    switch (SCHEME_TYPE(rand)) {
    case scheme_local_type:
      {
        /* Ok if not mutable */
        int pos = SCHEME_LOCAL_POS(rand);
        if (pos < lifted)
          return 1;
        else if (!optimize_is_mutated(info, pos - lifted))
          return 1;
      }
      break;
    case scheme_application_type:
      {
        Scheme_App_Rec *app = (Scheme_App_Rec *)rand;
        int non_fl_args = 0;
        if (produces_unboxed(app->args[0], &non_fl_args, app->num_args, 1)) {
          int i;
          for (i = app->num_args; i--; ) {
            fuel--;
            if (!is_unboxed_argument(app->args[i+1], fuel, info, lifted))
              return 0;
          }
          return 1;
        }
      }
      break;
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)rand;
        int non_fl_args = 0;
        if (produces_unboxed(app->rator, &non_fl_args, 1, 1)) {
          if (is_unboxed_argument(app->rand, fuel - 1, info, lifted))
            return 1;
        }
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)rand;
        int non_fl_args = 0;
        if (produces_unboxed(app->rator, &non_fl_args, 2, 1)) {
          if (is_unboxed_argument(app->rand1, fuel - 1, info, lifted)
              && is_unboxed_argument(app->rand2, fuel - 2, info, lifted))
            return 1;
        }
      }
      break;
    default:
      if (SCHEME_TYPE(rand) > _scheme_compiled_values_types_)
        return 1;
      break;
    }
  }

  return 0;
}

int scheme_expr_produces_flonum(Scheme_Object *expr)
{
  while (1) {
    switch (SCHEME_TYPE(expr)) {
    case scheme_application_type:
      {
        Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
        return produces_unboxed(app->args[0], NULL, app->num_args, 0);
      }
      break;
    case scheme_application2_type:
      {
        Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;
        return produces_unboxed(app->rator, NULL, 1, 0);
      }
      break;
    case scheme_application3_type:
      {
        Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;
        return produces_unboxed(app->rator, NULL, 2, 0);
      }
      break;
    case scheme_compiled_let_void_type:
      {
        Scheme_Let_Header *lh = (Scheme_Let_Header *)expr;
        int i;
        expr = lh->body;
        for (i = 0; i < lh->num_clauses; i++) {
          expr = ((Scheme_Compiled_Let_Value *)expr)->body;
        }
        /* check expr again */
      }
      break;
    default:
      if (SCHEME_FLOATP(expr))
        return 1;
      return 0;
    }
  }
}

static Scheme_Object *check_unbox_rotation(Scheme_Object *_app, Scheme_Object *rator, int count, Optimize_Info *info)
{
  Scheme_Object *result = _app, *rand, *new_rand;
  Scheme_Let_Header *inner_head = NULL;
  Scheme_Compiled_Let_Value *inner = NULL;
  int i, lifted = 0;

  if (scheme_wants_flonum_arguments(rator, 0, 1)) {
    for (i = 0; i < count; i++) {
      if (count == 1)
        rand = ((Scheme_App2_Rec *)_app)->rand;
      else if (count == 2) {
        if (i == 0)
          rand = ((Scheme_App3_Rec *)_app)->rand1;
        else
          rand = ((Scheme_App3_Rec *)_app)->rand2;
      } else
        rand = ((Scheme_App_Rec *)_app)->args[i + 1];

      if (!is_unboxed_argument(rand, 32, info, lifted)) {
        int delta;

        if (SAME_TYPE(SCHEME_TYPE(rand), scheme_compiled_let_void_type)) {
          /* Rotate (<unboxed-arg-proc> (let* ([x <arg>]...) <expr>))
             to (let* ([x <arg>]...) (<unboxed-arg-proc> <expr>)) */
          Scheme_Let_Header *top_head = (Scheme_Let_Header *)rand, *head;
          Scheme_Compiled_Let_Value *clv, *prev;
          Scheme_Object *e;
          int i;

          top_head = head = (Scheme_Let_Header *)rand;
          prev = NULL;
          e = rand;
          delta = 0;
          while (SAME_TYPE(SCHEME_TYPE(e), scheme_compiled_let_void_type)) {
            head = (Scheme_Let_Header *)e;
            delta += head->count;
            prev = NULL;

            clv = (Scheme_Compiled_Let_Value *)head->body;
            prev = NULL;
            for (i = head->num_clauses; i--; clv = (Scheme_Compiled_Let_Value *)clv->body) {
              prev = clv;
            }
            e = (Scheme_Object *)clv;
          }

          if (prev)
            new_rand = prev->body;
          else
            new_rand = head->body;

          if (inner)
            inner->body = (Scheme_Object *)top_head;
          else if (inner_head)
            inner_head->body = (Scheme_Object *)top_head;
          else
            result = (Scheme_Object *)top_head;

          inner = prev;
          inner_head = head;
        } else {
          /* Rotate (<unboxed-arg-proc> <arg>) to
             (let ([x <arg>]) (<unboxed-arg-proc> x)) */
          Scheme_Let_Header *head;
          Scheme_Compiled_Let_Value *lv;
          int *flags;

          head = MALLOC_ONE_TAGGED(Scheme_Let_Header);
          head->iso.so.type = scheme_compiled_let_void_type;
          head->count = 1;
          head->num_clauses = 1;

          lv = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
          lv->iso.so.type = scheme_compiled_let_value_type;
          lv->count = 1;
          lv->position = 0;
          lv->value = rand;

          flags = (int *)scheme_malloc_atomic(sizeof(int));
          flags[0] = (SCHEME_WAS_USED | (1 << SCHEME_USE_COUNT_SHIFT));
          if (scheme_wants_flonum_arguments(rator, i, 0))
            flags[0] |= SCHEME_WAS_FLONUM_ARGUMENT;
          lv->flags = flags;

          head->body = (Scheme_Object *)lv;

          new_rand = scheme_make_local(scheme_local_type, 0, 0);

          if (inner)
            inner->body = (Scheme_Object *)head;
          else if (inner_head)
            inner_head->body = (Scheme_Object *)head;
          else
            result = (Scheme_Object *)head;

          inner = lv;
          inner_head = head;

          delta = 1;
        }

        if (delta) {
          lifted += delta;
          if (count == 1)
            ((Scheme_App2_Rec *)_app)->rand = scheme_false;
          else if (count == 2) {
            if (i == 0)
              ((Scheme_App3_Rec *)_app)->rand1 = scheme_false;
            else
              ((Scheme_App3_Rec *)_app)->rand2 = scheme_false;
          } else
            ((Scheme_App_Rec *)_app)->args[i + 1] = scheme_false;

          _app = optimize_shift(_app, delta, 0);
        }

        if (count == 1)
          ((Scheme_App2_Rec *)_app)->rand = new_rand;
        else if (count == 2) {
          if (i == 0)
            ((Scheme_App3_Rec *)_app)->rand1 = new_rand;
          else
            ((Scheme_App3_Rec *)_app)->rand2 = new_rand;
        } else
          ((Scheme_App_Rec *)_app)->args[i + 1] = new_rand;

        if (inner)
          inner->body = _app;
        else
          inner_head->body = _app;
      }
    }
  }

  return result;
}

static Scheme_Object *finish_optimize_app(Scheme_Object *o, Optimize_Info *info, int context, int rator_flags)
{
  switch(SCHEME_TYPE(o)) {
  case scheme_application_type:
    return finish_optimize_application((Scheme_App_Rec *)o, info, context, rator_flags);
  case scheme_application2_type:
    return finish_optimize_application2((Scheme_App2_Rec *)o, info, context, rator_flags);
  case scheme_application3_type:
    return finish_optimize_application3((Scheme_App3_Rec *)o, info, context, rator_flags);
  default:
    return o; /* may be a constant due to constant-folding */
  }
}

static Scheme_Object *direct_apply(Scheme_Object *expr, Scheme_Object *rator, Scheme_Object *last_rand)
{
  if (SAME_OBJ(rator, scheme_apply_proc)) {
    switch(SCHEME_TYPE(last_rand)) {
    case scheme_application_type:
      rator = ((Scheme_App_Rec *)last_rand)->args[0];
      break;
    case scheme_application2_type:
      rator = ((Scheme_App2_Rec *)last_rand)->rator;
      break;
    case scheme_application3_type:
      rator = ((Scheme_App3_Rec *)last_rand)->rator;
      break;
    case scheme_pair_type:
      if (scheme_is_list(last_rand))
        rator = scheme_list_proc;
      else
        rator = NULL;
      break;
    case scheme_null_type:
      rator = scheme_list_proc;
      break;
    default:
      rator = NULL;
      break;
    }

    if (rator && SAME_OBJ(rator, scheme_list_proc)) {
      /* Convert (apply f arg1 ... (list arg2 ...))
         to (f arg1 ... arg2 ...) */
      Scheme_Object *l = scheme_null;
      int i;

      switch(SCHEME_TYPE(last_rand)) {
      case scheme_application_type:
        for (i = ((Scheme_App_Rec *)last_rand)->num_args; i--; ) {
          l = scheme_make_pair(((Scheme_App_Rec *)last_rand)->args[i+1], l);
        }
        break;
      case scheme_application2_type:
        l = scheme_make_pair(((Scheme_App2_Rec *)last_rand)->rand, l);
        break;
      case scheme_application3_type:
        l = scheme_make_pair(((Scheme_App3_Rec *)last_rand)->rand2, l);
        l = scheme_make_pair(((Scheme_App3_Rec *)last_rand)->rand1, l);
        break;
      case scheme_pair_type:
        l = last_rand;
        break;
      case scheme_null_type:
        l = scheme_null;
        break;
      }

      switch(SCHEME_TYPE(expr)) {
      case scheme_application_type:
        for (i = ((Scheme_App_Rec *)expr)->num_args - 1; i--; ) {
          l = scheme_make_pair(((Scheme_App_Rec *)expr)->args[i+1], l);
        }
        break;
      default:
      case scheme_application3_type:
        l = scheme_make_pair(((Scheme_App3_Rec *)expr)->rand1, l);
        break;
      }

      return scheme_make_application(l);
    }
  }

  return NULL;
}

static Scheme_Object *optimize_application(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Object *le;
  Scheme_App_Rec *app;
  int i, n, rator_flags = 0, sub_context = 0;

  app = (Scheme_App_Rec *)o;

  /* Check for (apply ... (list ...)) early: */
  le = direct_apply((Scheme_Object *)app, app->args[0], app->args[app->num_args]);
  if (le) return scheme_optimize_expr(le, info, context);

  le = check_app_let_rator(o, app->args[0], info, app->num_args, context);
  if (le) return le;

  n = app->num_args + 1;

  for (i = 0; i < n; i++) {
    if (!i) {
      le = optimize_for_inline(info, app->args[i], n - 1, app, NULL, NULL, &rator_flags, context, 0);
      if (le)
	return le;
    }

    sub_context = 0;
    if ((i > 0) && scheme_wants_flonum_arguments(app->args[0], i - 1, 0))
      sub_context = OPT_CONTEXT_FLONUM_ARG;

    le = scheme_optimize_expr(app->args[i], info, sub_context);
    app->args[i] = le;

    if (!i) {
      /* Maybe found "((lambda" after optimizing; try again */
      le = optimize_for_inline(info, app->args[i], n - 1, app, NULL, NULL, &rator_flags, context, 1);
      if (le)
        return le;
    }
  }

  /* Check for (apply ... (list ...)) after some optimizations: */
  le = direct_apply((Scheme_Object *)app, app->args[0], app->args[app->num_args]);
  if (le) return finish_optimize_app(le, info, context, rator_flags);

  return finish_optimize_application(app, info, context, rator_flags);
}

static Scheme_Object *finish_optimize_application(Scheme_App_Rec *app, Optimize_Info *info, int context, int rator_flags)
{
  Scheme_Object *le;
  int all_vals = 1, i;

  for (i = app->num_args; i--; ) {
    if (SCHEME_TYPE(app->args[i+1]) < _scheme_compiled_values_types_)
      all_vals = 0;
  }

  info->size += 1;
  if (!is_nonmutating_primitive(app->args[0], app->num_args))
    info->vclock += 1;

  if (all_vals) {
    le = try_optimize_fold(app->args[0], (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  if (!app->num_args && SAME_OBJ(app->args[0], scheme_list_proc))
    return scheme_null;

  register_flonum_argument_types(app, NULL, NULL, info);

  return check_unbox_rotation((Scheme_Object *)app, app->args[0], app->num_args, info);
}

static Scheme_Object *lookup_constant_proc(Optimize_Info *info, Scheme_Object *rand)
{
  Scheme_Object *c = NULL;

  if (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(rand)))
    c = rand;
  if (SAME_TYPE(SCHEME_TYPE(rand), scheme_local_type)) {
    int offset;
    Scheme_Object *expr;
    expr = optimize_reverse(info, SCHEME_LOCAL_POS(rand), 0, 0);
    c = optimize_info_lookup(info, SCHEME_LOCAL_POS(expr), &offset, NULL, 0, 0, NULL, NULL);
  }
  if (SAME_TYPE(SCHEME_TYPE(rand), scheme_compiled_toplevel_type)) {
    if (info->top_level_consts) {
      int pos;

      while (1) {
        pos = SCHEME_TOPLEVEL_POS(rand);
        c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        c = no_potential_size(c);
        if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_compiled_toplevel_type))
          rand = c;
        else
          break;
      }
    }
  }

  if (c && SAME_TYPE(scheme_noninline_proc_type, SCHEME_TYPE(c))) {
    c = SCHEME_BOX_VAL(c);

    while (SAME_TYPE(SCHEME_TYPE(c), scheme_compiled_let_void_type)) {
      /* This must be (let ([x <proc>]) <proc>); see scheme_is_statically_proc() */
      Scheme_Let_Header *lh = (Scheme_Let_Header *)c;
      Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
      c = lv->body;
    }
  }

  if (c && (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(c))
            || SAME_TYPE(scheme_case_lambda_sequence_type, SCHEME_TYPE(c))))
    return c;

  return NULL;
}

static Scheme_Object *optimize_application2(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_App2_Rec *app;
  Scheme_Object *le;
  int rator_flags = 0, sub_context = 0;

  app = (Scheme_App2_Rec *)o;

  le = check_app_let_rator(o, app->rator, info, 1, context);
  if (le) return le;

  le = optimize_for_inline(info, app->rator, 1, NULL, app, NULL, &rator_flags, context, 0);
  if (le)
    return le;

  le = scheme_optimize_expr(app->rator, info, sub_context);
  app->rator = le;

  {
    /* Maybe found "((lambda" after optimizing; try again */
    le = optimize_for_inline(info, app->rator, 1, NULL, app, NULL, &rator_flags, context, 1);
    if (le)
      return le;
  }

  if (scheme_wants_flonum_arguments(app->rator, 0, 0))
    sub_context |= OPT_CONTEXT_FLONUM_ARG;

  le = scheme_optimize_expr(app->rand, info, sub_context);
  app->rand = le;

  return finish_optimize_application2(app, info, context, rator_flags);
}

static Scheme_Object *finish_optimize_application2(Scheme_App2_Rec *app, Optimize_Info *info, int context, int rator_flags)
{
  Scheme_Object *le;

  info->size += 1;

  if (SCHEME_TYPE(app->rand) > _scheme_compiled_values_types_) {
    le = try_optimize_fold(app->rator, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  if (SAME_OBJ(scheme_procedure_p_proc, app->rator)) {
    if (lookup_constant_proc(info, app->rand)) {
      info->preserves_marks = 1;
      info->single_result = 1;
      return scheme_true;
    }
  }

  if (SAME_OBJ(scheme_varref_const_p_proc, app->rator)) {
    if (SAME_TYPE(SCHEME_TYPE(app->rand), scheme_varref_form_type)) {
      Scheme_Object *var = SCHEME_PTR1_VAL(app->rand);
      if (SAME_OBJ(var, scheme_true)) {
        return scheme_true;
      } else if (SAME_OBJ(var, scheme_false)) {
        return scheme_false;
      } else if (scheme_compiled_propagate_ok(var, info)) {
        /* can propagate => is a constant */
        return scheme_true;
      }
    }
  }

  if ((SAME_OBJ(scheme_values_func, app->rator)
       || SAME_OBJ(scheme_list_star_proc, app->rator))
      && (scheme_omittable_expr(app->rand, 1, -1, 0, info, -1, 0)
          || single_valued_noncm_expression(app->rand, 5))) {
    info->preserves_marks = 1;
    info->single_result = 1;
    return app->rand;
  }

  if (!is_nonmutating_primitive(app->rator, 1))
    info->vclock += 1;

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  /* Check for things like (cXr (cons X Y)): */
  if (SCHEME_PRIMP(app->rator)
      && (SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_UNARY_INLINED)) {
    Scheme_Object *rand, *inside = NULL, *alt = NULL;

    rand = app->rand;

    /* We can go inside a `let', which is useful in case the argument
       was a function call that has been inlined. */
    while (SAME_TYPE(SCHEME_TYPE(rand), scheme_compiled_let_void_type)) {
      Scheme_Let_Header *head = (Scheme_Let_Header *)rand;
      int i;
      inside = rand;
      rand = head->body;
      for (i = head->num_clauses; i--; ) {
        inside = rand;
	rand = ((Scheme_Compiled_Let_Value *)rand)->body;
      }
    }

    if (SAME_TYPE(SCHEME_TYPE(rand), scheme_application2_type)) {
      Scheme_App2_Rec *app2 = (Scheme_App2_Rec *)rand;
      if (SAME_OBJ(scheme_list_proc, app2->rator)) {
        if (IS_NAMED_PRIM(app->rator, "car")) {
          /* (car (list X)) */
          if (scheme_omittable_expr(app2->rand, 1, 5, 0, NULL, -1, 0)
              || single_valued_noncm_expression(app2->rand, 5)) {
            alt = app2->rand;
          }
        } else if (IS_NAMED_PRIM(app->rator, "cdr")) {
          /* (cdr (list X)) */
          if (scheme_omittable_expr(app2->rand, 1, 5, 0, NULL, -1, 0))
            alt = scheme_null;
        }
      }
    } else if (SAME_TYPE(SCHEME_TYPE(rand), scheme_application3_type)) {
      Scheme_App3_Rec *app3 = (Scheme_App3_Rec *)rand;
      if (IS_NAMED_PRIM(app->rator, "car")) {
        if (SAME_OBJ(scheme_cons_proc, app3->rator)
            || SAME_OBJ(scheme_list_proc, app3->rator)
            || SAME_OBJ(scheme_list_star_proc, app3->rator)) {
          /* (car ({cons|list|list*} X Y)) */
          if ((scheme_omittable_expr(app3->rand1, 1, 5, 0, NULL, -1, 0)
               || single_valued_noncm_expression(app3->rand1, 5))
              && scheme_omittable_expr(app3->rand2, 1, 5, 0, NULL, -1, 0)) {
            alt = app3->rand1;
          }
        }
      } else if (IS_NAMED_PRIM(app->rator, "cdr")) {
        /* (cdr (cons X Y)) */
        if (SAME_OBJ(scheme_cons_proc, app3->rator)) {
          if ((scheme_omittable_expr(app3->rand2, 1, 5, 0, NULL, -1, 0)
               || single_valued_noncm_expression(app3->rand2, 5))
              && scheme_omittable_expr(app3->rand1, 1, 5, 0, NULL, -1, 0)) {
            alt = app3->rand2;
          }
        }
      } else if (IS_NAMED_PRIM(app->rator, "cadr")) {
        if (SAME_OBJ(scheme_list_proc, app3->rator)) {
          /* (cadr (list X Y)) */
          if ((scheme_omittable_expr(app3->rand2, 1, 5, 0, NULL, -1, 0)
               || single_valued_noncm_expression(app3->rand2, 5))
              && scheme_omittable_expr(app3->rand1, 1, 5, 0, NULL, -1, 0)) {
            alt = app3->rand2;
          }
        }
      }
    }

    if (alt) {
      if (inside) {
        if (SAME_TYPE(SCHEME_TYPE(inside), scheme_compiled_let_void_type))
          ((Scheme_Let_Header *)inside)->body = alt;
        else
          ((Scheme_Compiled_Let_Value *)inside)->body = alt;
        return app->rand;
      }
      return alt;
    }
  }

  register_flonum_argument_types(NULL, app, NULL, info);

  return check_unbox_rotation((Scheme_Object *)app, app->rator, 1, info);
}

static Scheme_Object *optimize_application3(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_App3_Rec *app;
  Scheme_Object *le;
  int rator_flags = 0, sub_context = 0;

  app = (Scheme_App3_Rec *)o;

  /* Check for (apply ... (list ...)) early: */
  le = direct_apply((Scheme_Object *)app, app->rator, app->rand2);
  if (le) return scheme_optimize_expr(le, info, context);

  le = check_app_let_rator(o, app->rator, info, 2, context);
  if (le) return le;

  le = optimize_for_inline(info, app->rator, 2, NULL, NULL, app, &rator_flags, context, 0);
  if (le)
    return le;

  le = scheme_optimize_expr(app->rator, info, sub_context);
  app->rator = le;

  {
    /* Maybe found "((lambda" after optimizing; try again */
    le = optimize_for_inline(info, app->rator, 2, NULL, NULL, app, &rator_flags, context, 1);
    if (le)
      return le;
  }

  /* 1st arg */

  if (scheme_wants_flonum_arguments(app->rator, 0, 0))
    sub_context |= OPT_CONTEXT_FLONUM_ARG;

  le = scheme_optimize_expr(app->rand1, info, sub_context);
  app->rand1 = le;

  /* 2nd arg */

  if (scheme_wants_flonum_arguments(app->rator, 1, 0))
    sub_context |= OPT_CONTEXT_FLONUM_ARG;
  else
    sub_context &= ~OPT_CONTEXT_FLONUM_ARG;

  le = scheme_optimize_expr(app->rand2, info, sub_context);
  app->rand2 = le;

  /* Check for (apply ... (list ...)) after some optimizations: */
  le = direct_apply((Scheme_Object *)app, app->rator, app->rand2);
  if (le) return finish_optimize_app(le, info, context, rator_flags);

  return finish_optimize_application3(app, info, context, rator_flags);
}

static Scheme_Object *finish_optimize_application3(Scheme_App3_Rec *app, Optimize_Info *info, int context, int rator_flags)
{
  Scheme_Object *le;
  int all_vals = 1;

  info->size += 1;

  if (SCHEME_TYPE(app->rand1) < _scheme_compiled_values_types_)
    all_vals = 0;
  if (SCHEME_TYPE(app->rand2) < _scheme_compiled_values_types_)
    all_vals = 0;


  if (all_vals) {
    le = try_optimize_fold(app->rator, (Scheme_Object *)app, info);
    if (le)
      return le;
  }

  if (!is_nonmutating_primitive(app->rator, 2))
    info->vclock += 1;

  /* Check for (call-with-values (lambda () M) N): */
  if (SAME_OBJ(app->rator, scheme_call_with_values_proc)) {
    if (SAME_TYPE(SCHEME_TYPE(app->rand1), scheme_compiled_unclosed_procedure_type)) {
      Scheme_Closure_Data *data = (Scheme_Closure_Data *)app->rand1;

      if (!data->num_params) {
        /* Convert to apply-values form: */
        return scheme_optimize_apply_values(app->rand2, data->code, info,
                                            ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SINGLE_RESULT)
                                             ? ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE)
                                                ? -1
                                                : 1)
                                             : 0),
                                            context);
      }
    }
  }

  if (SAME_OBJ(scheme_procedure_arity_includes_proc, app->rator)) {
    if (SCHEME_INTP(app->rand2)) {
      Scheme_Object *proc;
      Scheme_Case_Lambda *cl;
      int i, cnt;

      proc = lookup_constant_proc(info, app->rand1);
      if (proc) {
        if (SAME_TYPE(SCHEME_TYPE(proc), scheme_compiled_unclosed_procedure_type)) {
          cnt = 1;
          cl = NULL;
        } else {
          cl = (Scheme_Case_Lambda *)proc;
          cnt = cl->count;
        }

        for (i = 0; i < cnt; i++) {
          if (cl) proc = cl->array[i];

          if (SAME_TYPE(SCHEME_TYPE(proc), scheme_compiled_unclosed_procedure_type)) {
            Scheme_Closure_Data *data = (Scheme_Closure_Data *)proc;
            int n = SCHEME_INT_VAL(app->rand2), ok;
            if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST) {
              ok = ((data->num_params - 1) <= n);
            } else {
              ok = (data->num_params == n);
            }
            if (ok) {
              info->preserves_marks = 1;
              info->single_result = 1;
              return scheme_true;
            }
          } else {
            break;
          }
        }

        if (i == cnt) {
          info->preserves_marks = 1;
          info->single_result = 1;
          return scheme_false;
        }
      }
    }
  }

  info->preserves_marks = !!(rator_flags & CLOS_PRESERVES_MARKS);
  info->single_result = !!(rator_flags & CLOS_SINGLE_RESULT);
  if (rator_flags & CLOS_RESULT_TENTATIVE) {
    info->preserves_marks = -info->preserves_marks;
    info->single_result = -info->single_result;
  }

  /* Ad hoc optimization of (unsafe-fx+ <x> 0), etc. */
  if (SCHEME_PRIMP(app->rator)
      && (SCHEME_PRIM_PROC_FLAGS(app->rator) & SCHEME_PRIM_IS_UNSAFE_NONMUTATING)) {
    int z1, z2;

    z1 = SAME_OBJ(app->rand1, scheme_make_integer(0));
    z2 = SAME_OBJ(app->rand2, scheme_make_integer(0));
    if (IS_NAMED_PRIM(app->rator, "unsafe-fx+")) {
      if (z1)
        return app->rand2;
      else if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fx-")) {
      if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fx*")) {
      if (z1 || z2)
        return scheme_make_integer(0);
      if (SAME_OBJ(app->rand1, scheme_make_integer(1)))
        return app->rand2;
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fx/")
               || IS_NAMED_PRIM(app->rator, "unsafe-fxquotient")) {
      if (z1)
        return scheme_make_integer(0);
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fxremainder")
               || IS_NAMED_PRIM(app->rator, "unsafe-fxmodulo")) {
      if (z1)
        return scheme_make_integer(0);
      if (SAME_OBJ(app->rand2, scheme_make_integer(1)))
        return scheme_make_integer(0);
    }

    z1 = (SCHEME_FLOATP(app->rand1) && (SCHEME_FLOAT_VAL(app->rand1) == 0.0));
    z2 = (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 0.0));

    if (IS_NAMED_PRIM(app->rator, "unsafe-fl+")) {
      if (z1)
        return app->rand2;
      else if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl-")) {
      if (z2)
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl*")) {
      if (SCHEME_FLOATP(app->rand1) && (SCHEME_FLOAT_VAL(app->rand1) == 1.0))
        return app->rand2;
      if (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 1.0))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-fl/")
               || IS_NAMED_PRIM(app->rator, "unsafe-flquotient")) {
      if (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 1.0))
        return app->rand1;
    } else if (IS_NAMED_PRIM(app->rator, "unsafe-flremainder")
               || IS_NAMED_PRIM(app->rator, "unsafe-flmodulo")) {
      if (SCHEME_FLOATP(app->rand2) && (SCHEME_FLOAT_VAL(app->rand2) == 1.0))
        return scheme_make_double(0.0);
    }
  }

  register_flonum_argument_types(NULL, NULL, app, info);

  return check_unbox_rotation((Scheme_Object *)app, app->rator, 2, info);
}

Scheme_Object *scheme_optimize_apply_values(Scheme_Object *f, Scheme_Object *e,
                                            Optimize_Info *info,
                                            int e_single_result,
                                            int context)
/* f and e are already optimized */
{
  Scheme_Object *f_is_proc = NULL;

  info->preserves_marks = 0;
  info->single_result = 0;

  {
    Scheme_Object *rev;
    if (SAME_TYPE(SCHEME_TYPE(f), scheme_local_type)) {
      rev = optimize_reverse(info, SCHEME_LOCAL_POS(f), 1, 0);
    } else
      rev = f;

    if (rev) {
      int rator2_flags;
      Scheme_Object *o_f;
      o_f = optimize_for_inline(info, rev, 1, NULL, NULL, NULL, &rator2_flags, context, 0);
      if (o_f) {
        f_is_proc = rev;

        if (SAME_TYPE(SCHEME_TYPE(o_f), scheme_compiled_unclosed_procedure_type)) {
          Scheme_Closure_Data *data2 = (Scheme_Closure_Data *)o_f;
          int flags = SCHEME_CLOSURE_DATA_FLAGS(data2);
          info->preserves_marks = !!(flags & CLOS_PRESERVES_MARKS);
          info->single_result = !!(flags & CLOS_SINGLE_RESULT);
          if (flags & CLOS_RESULT_TENTATIVE) {
            info->preserves_marks = -info->preserves_marks;
            info->single_result = -info->single_result;
          }
        }
      }
    }

    if (!f_is_proc && SCHEME_PROCP(f)) {
      f_is_proc = f;
    }
  }

  if (f_is_proc && (e_single_result > 0)) {
    /* Just make it an application (N M): */
    Scheme_App2_Rec *app2;
    Scheme_Object *cloned, *f_cloned;

    app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
    app2->iso.so.type = scheme_application2_type;

    /* We'd like to try to inline here. The problem is that
       e (the argument) has been optimized already,
       which means it's in the wrong coordinate system.
       If we can shift-clone it, then it will be back in the right
       coordinates. */

    cloned = optimize_clone(1, e, info, 0, 0);
    if (cloned) {
      if (SAME_TYPE(SCHEME_TYPE(f_is_proc), scheme_compiled_unclosed_procedure_type))
        f_cloned = optimize_clone(1, f_is_proc, info, 0, 0);
      else {
        /* Otherwise, no clone is needed; in the case of a lexical
           variable, we already reversed it. */
        f_cloned = f_is_proc;
      }

      if (f_cloned) {
        app2->rator = f_cloned;
        app2->rand = cloned;
        info->inline_fuel >>= 1; /* because we've already optimized the rand */
        return optimize_application2((Scheme_Object *)app2, info, context);
      }
    }

    app2->rator = f;
    app2->rand = e;
    return (Scheme_Object *)app2;
  }

  {
    Scheme_Object *av;
    av = scheme_alloc_object();
    av->type = scheme_apply_values_type;
    SCHEME_PTR1_VAL(av) = f;
    SCHEME_PTR2_VAL(av) = e;
    return av;
  }
}

static Scheme_Object *optimize_sequence(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Sequence *s = (Scheme_Sequence *)o;
  Scheme_Object *le;
  int i, count, prev_size;
  int drop = 0, preserves_marks = 0, single_result = 0;

  count = s->count;
  for (i = 0; i < count; i++) {
    prev_size = info->size;

    le = scheme_optimize_expr(s->array[i], info,
                              ((i + 1 == count)
                               ? scheme_optimize_tail_context(context)
                               : 0));
    if (i == s->count - 1) {
      single_result = info->single_result;
      preserves_marks = info->preserves_marks;
    }

    /* Inlining and constant propagation can expose
       omittable expressions. */
    if ((i + 1 != count)
	&& scheme_omittable_expr(le, -1, -1, 0, NULL, -1, 0)) {
      drop++;
      info->size = prev_size;
      s->array[i] = NULL;
    } else {
      s->array[i] = le;
    }
  }

  info->preserves_marks = preserves_marks;
  info->single_result = single_result;

  if (drop + 1 == s->count) {
    return s->array[drop];
  } else if (drop) {
    Scheme_Sequence *s2;
    int j = 0;

    s2 = scheme_malloc_sequence(s->count - drop);
    s2->so.type = s->so.type;
    s2->count = s->count - drop;

    for (i = 0; i < s->count; i++) {
      if (s->array[i]) {
	s2->array[j++] = s->array[i];
      }
    }

    s = s2;
  }

  return (Scheme_Object *)s;
}

XFORM_NONGCING static int small_inline_number(Scheme_Object *o)
{
  if (SCHEME_BIGNUMP(o))
    return SCHEME_BIGLEN(o) < 32;
  else if (SCHEME_COMPLEXP(o))
    return (small_inline_number(scheme_complex_real_part(o))
            && small_inline_number(scheme_complex_imaginary_part(o)));
  else if (SCHEME_RATIONALP(o))
    return (small_inline_number(scheme_rational_numerator(o))
            && small_inline_number(scheme_rational_denominator(o)));
  else
    return 1;
}

#define STR_INLINE_LIMIT 256

int scheme_compiled_duplicate_ok(Scheme_Object *fb, int cross_module)
{
  return (SCHEME_VOIDP(fb)
	  || SAME_OBJ(fb, scheme_true)
	  || SCHEME_FALSEP(fb)
	  || (SCHEME_SYMBOLP(fb) 
              && (!cross_module || (!SCHEME_SYM_WEIRDP(fb)
                                    && (SCHEME_SYM_LEN(fb) < STR_INLINE_LIMIT))))
	  || (SCHEME_KEYWORDP(fb)
              && (!cross_module || (SCHEME_KEYWORD_LEN(fb) < STR_INLINE_LIMIT)))
	  || SCHEME_EOFP(fb)
	  || SCHEME_INTP(fb)
	  || SCHEME_NULLP(fb)
	  || (!cross_module && SAME_TYPE(SCHEME_TYPE(fb), scheme_local_type))
          || SCHEME_PRIMP(fb)
          /* Values that are hashed by the printer and/or interned on 
             read to avoid duplication: */
	  || SCHEME_CHARP(fb)
          || (SCHEME_CHAR_STRINGP(fb) 
              && (!cross_module || (SCHEME_CHAR_STRLEN_VAL(fb) < STR_INLINE_LIMIT)))
          || (SCHEME_BYTE_STRINGP(fb)
              && (!cross_module || (SCHEME_BYTE_STRLEN_VAL(fb) < STR_INLINE_LIMIT)))
          || SAME_TYPE(SCHEME_TYPE(fb), scheme_regexp_type)
          || (SCHEME_NUMBERP(fb)
              && (!cross_module || small_inline_number(fb))));
}

static int equivalent_exprs(Scheme_Object *a, Scheme_Object *b)
{
  if (SAME_OBJ(a, b))
    return 1;
  if (SAME_TYPE(SCHEME_TYPE(a), scheme_local_type)
      && SAME_TYPE(SCHEME_TYPE(b), scheme_local_type)
      && (SCHEME_LOCAL_POS(a) == SCHEME_LOCAL_POS(b)))
    return 1;

  return 0;
}

static Scheme_Object *optimize_branch(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_Branch_Rec *b;
  Scheme_Object *t, *tb, *fb;
  int preserves_marks = 1, single_result = 1;

  b = (Scheme_Branch_Rec *)o;

  t = b->test;
  tb = b->tbranch;
  fb = b->fbranch;

  if (context & OPT_CONTEXT_BOOLEAN) {
    /* For test position, convert (if <expr> #t #f) to <expr> */
    if (SAME_OBJ(tb, scheme_true) && SAME_OBJ(fb, scheme_false))
      return scheme_optimize_expr(t, info, context);

    /* Convert (if <id> <id> expr) to (if <id> #t expr) */
    if (SAME_TYPE(SCHEME_TYPE(t), scheme_local_type)
        && SAME_TYPE(SCHEME_TYPE(tb), scheme_local_type)
        && (SCHEME_LOCAL_POS(t) == SCHEME_LOCAL_POS(tb))) {
      b->tbranch = tb = scheme_true;
    }
  }

  t = scheme_optimize_expr(t, info, OPT_CONTEXT_BOOLEAN);

  /* Try optimize: (if (not x) y z) => (if x z y) */
  while (1) {
    if (SAME_TYPE(SCHEME_TYPE(t), scheme_application2_type)) {
      Scheme_App2_Rec *app;

      app = (Scheme_App2_Rec *)t;
      if (SAME_PTR(scheme_not_prim, app->rator)) {
	t = tb;
	tb = fb;
	fb = t;
	t = app->rand;
      } else
	break;
    } else
      break;
  }

  info->vclock += 1; /* model branch as clock increment */

  if (SCHEME_TYPE(t) > _scheme_compiled_values_types_) {
    info->size -= 1;
    if (SCHEME_FALSEP(t))
      return scheme_optimize_expr(fb, info, scheme_optimize_tail_context(context));
    else
      return scheme_optimize_expr(tb, info, scheme_optimize_tail_context(context));
  } else if (SAME_TYPE(SCHEME_TYPE(t), scheme_compiled_quote_syntax_type)
             || SAME_TYPE(SCHEME_TYPE(t), scheme_compiled_unclosed_procedure_type)) {
    info->size -= 1; /* could be more precise for better for procedure size */
    return scheme_optimize_expr(tb, info, scheme_optimize_tail_context(context));
  }

  tb = scheme_optimize_expr(tb, info, scheme_optimize_tail_context(context));

  if (!info->preserves_marks)
    preserves_marks = 0;
  else if (info->preserves_marks < 0)
    preserves_marks = -1;
  if (!info->single_result)
    single_result = 0;
  else if (info->single_result < 0)
    single_result = -1;

  fb = scheme_optimize_expr(fb, info, scheme_optimize_tail_context(context));

  if (!info->preserves_marks)
    preserves_marks = 0;
  else if (preserves_marks && (info->preserves_marks < 0))
    preserves_marks = -1;
  if (!info->single_result)
    single_result = 0;
  else if (single_result && (info->single_result < 0))
    single_result = -1;

  info->vclock += 1;  /* model join as clock increment */
  info->preserves_marks = preserves_marks;
  info->single_result = single_result;

  /* Try optimize: (if x x #f) => x */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_local_type)
      && SAME_TYPE(SCHEME_TYPE(tb), scheme_local_type)
      && (SCHEME_LOCAL_POS(t) == SCHEME_LOCAL_POS(tb))
      && SCHEME_FALSEP(fb)) {
    info->size -= 2;
    return t;
  }

  /* Try optimize: (if <omitable-expr> v v) => v */
  if (scheme_omittable_expr(t, 1, 20, 0, NULL, -1, 0)
      && equivalent_exprs(tb, fb)) {
    info->size -= 2; /* could be more precise */
    return tb;
  }

  /* Convert: (if (if M N #f) M2 K) => (if M (if N M2 K) K)
     for simple constants K. This is useful to expose simple
     tests to the JIT. */
  if (SAME_TYPE(SCHEME_TYPE(t), scheme_branch_type)
      && scheme_compiled_duplicate_ok(fb, 0)) {
    Scheme_Branch_Rec *b2 = (Scheme_Branch_Rec *)t;
    if (SCHEME_FALSEP(b2->fbranch)) {
      Scheme_Branch_Rec *b3;
      b3 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b3->so.type = scheme_branch_type;
      b3->test = b2->tbranch;
      b3->tbranch = tb;
      b3->fbranch = fb;
      t = b2->test;
      tb = (Scheme_Object *)b3;
    }
  }

  b->test = t;
  b->tbranch = tb;
  b->fbranch = fb;

  if (OPT_BRANCH_ADDS_NO_SIZE) {
    /* Seems to work better to not to increase the size
       specifically for `if' */
  } else {
    info->size += 1;
  }

  return o;
}

static int omittable_key(Scheme_Object *k, Optimize_Info *info)
{
  /* A key is not omittable if it might refer to a chaperoned/impersonated
     continuation mark key, so that's why we pass 1 for `no_id': */
  return scheme_omittable_expr(k, 1, 20, 0, info, -1, 1);
}

static Scheme_Object *optimize_wcm(Scheme_Object *o, Optimize_Info *info, int context)
{
  Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)o;
  Scheme_Object *k, *v, *b;

  k = scheme_optimize_expr(wcm->key, info, 0);

  v = scheme_optimize_expr(wcm->val, info, 0);

  b = scheme_optimize_expr(wcm->body, info, scheme_optimize_tail_context(context));

  if (omittable_key(k, info)
      && scheme_omittable_expr(v, 1, 20, 0, info, -1, 0)
      && scheme_omittable_expr(b, -1, 20, 0, info, -1, 0))
    return b;

  /* info->single_result is already set */
  info->preserves_marks = 0;

  wcm->key = k;
  wcm->val = v;
  wcm->body = b;

  info->size += 1;

  return (Scheme_Object *)wcm;
}

/*========================================================================*/
/*                            other syntax                                */
/*========================================================================*/

static Scheme_Object *
define_values_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *vars = SCHEME_VEC_ELS(data)[0];
  Scheme_Object *val = SCHEME_VEC_ELS(data)[1];

  optimize_info_used_top(info);
  val = scheme_optimize_expr(val, info, 0);

  SCHEME_VEC_ELS(data)[0] = vars;
  SCHEME_VEC_ELS(data)[1] = val;

  return data;
}

static Scheme_Object *
set_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *var, *val;

  var = sb->var;
  val = sb->val;

  val = scheme_optimize_expr(val, info, 0);

  info->preserves_marks = 1;
  info->single_result = 1;

  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    int pos, delta;

    pos = SCHEME_LOCAL_POS(var);

    /* Register that we use this variable: */
    optimize_info_lookup(info, pos, NULL, NULL, 0, 0, NULL, NULL);

    /* Offset: */
    delta = optimize_info_get_shift(info, pos);
    if (delta)
      var = scheme_make_local(scheme_local_type, pos + delta, 0);
  } else {
    optimize_info_used_top(info);
  }

  info->vclock++;

  sb->var = var;
  sb->val = val;

  return (Scheme_Object *)sb;
}

static Scheme_Object *
set_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data, *naya;
  Scheme_Object *var, *val;

  naya = MALLOC_ONE_TAGGED(Scheme_Set_Bang);
  memcpy(naya, sb, sizeof(Scheme_Set_Bang));

  var = naya->var;
  val = naya->val;

  val = optimize_clone(dup_ok, val, info, delta, closure_depth);
  if (!val) return NULL;
  if (SAME_TYPE(SCHEME_TYPE(var), scheme_local_type)) {
    var = optimize_clone(dup_ok, var, info, delta, closure_depth);
    if (!var) return NULL;
  }

  naya->var = var;
  naya->val = val;

  return (Scheme_Object *)naya;
}

static Scheme_Object *set_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Set_Bang *sb = (Scheme_Set_Bang *)data;
  Scheme_Object *e;

  e = optimize_shift(sb->var, delta, after_depth);
  sb->var = e;

  e = optimize_shift(sb->val, delta, after_depth);
  sb->val = e;

  return (Scheme_Object *)sb;
}

static Scheme_Object *
ref_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *v;

  optimize_info_used_top(info);

  v = SCHEME_PTR1_VAL(data);
  if (SAME_TYPE(SCHEME_TYPE(v), scheme_local_type)) {
    int is_mutated = 0;
    optimize_info_mutated_lookup(info, SCHEME_LOCAL_POS(v), &is_mutated);
    SCHEME_PTR1_VAL(data) = (is_mutated ? scheme_false : scheme_true);
  }

  info->preserves_marks = 1;
  info->single_result = 1;
  info->size++;

  return data;
}

static Scheme_Object *
ref_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Object *v;

  v = optimize_shift(SCHEME_PTR1_VAL(data), delta, after_depth);
  SCHEME_PTR1_VAL(data) = v;

  v = optimize_shift(SCHEME_PTR2_VAL(data), delta, after_depth);
  SCHEME_PTR2_VAL(data) = v;

  return data;
}

static Scheme_Object *
ref_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Object *naya;
  Scheme_Object *a, *b;

  a = SCHEME_PTR1_VAL(data);
  a = optimize_clone(dup_ok, a, info, delta, closure_depth);
  if (!a) return NULL;

  b = SCHEME_PTR2_VAL(data);
  b = optimize_clone(dup_ok, a, info, delta, closure_depth);
  if (!b) return NULL;

  naya = scheme_alloc_object();
  naya->type = scheme_varref_form_type;
  SCHEME_PTR1_VAL(naya) = a;
  SCHEME_PTR2_VAL(naya) = b;

  return naya;
}

static Scheme_Object *
apply_values_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *f, *e;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  f = scheme_optimize_expr(f, info, 0);
  e = scheme_optimize_expr(e, info, 0);

  info->size += 1;
  info->vclock += 1;

  return scheme_optimize_apply_values(f, e, info, info->single_result, context);
}

static Scheme_Object *
apply_values_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Object *e;

  e = optimize_shift(SCHEME_PTR1_VAL(data), delta, after_depth);
  SCHEME_PTR1_VAL(data) = e;

  e = optimize_shift(SCHEME_PTR2_VAL(data), delta, after_depth);
  SCHEME_PTR2_VAL(data) = e;

  return data;
}

static Scheme_Object *
apply_values_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Object *f, *e;

  f = SCHEME_PTR1_VAL(data);
  e = SCHEME_PTR2_VAL(data);

  f = optimize_clone(dup_ok, f, info, delta, closure_depth);
  if (!f) return NULL;
  e = optimize_clone(dup_ok, e, info, delta, closure_depth);
  if (!e) return NULL;

  data = scheme_alloc_object();
  data->type = scheme_apply_values_type;
  SCHEME_PTR1_VAL(data) = f;
  SCHEME_PTR2_VAL(data) = e;

  return data;
}

static Scheme_Object *
case_lambda_optimize(Scheme_Object *expr, Optimize_Info *info, int context)
{
  Scheme_Object *le;
  int i;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)expr;
  mzshort **tus, *tu;
  int *tu_lens, tup, tu_count = 0;

  if (info->transitive_use_pos) {
    /* We'll need to merge transitive_use arrays */
    tup = info->transitive_use_pos - 1;
    tus = (mzshort **)MALLOC_N(mzshort*, seq->count);
    tu_lens = (int*)MALLOC_N_ATOMIC(int, seq->count);
  } else {
    tup = 0;
    tus = NULL;
    tu_lens = NULL;
  }

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = scheme_optimize_expr(le, info, 0);
    seq->array[i] = le;

    if (tus) {
      tus[i] = info->transitive_use[tup];
      tu_lens[i] = info->transitive_use_len[tup];
      if (tus[i]) {
        tu_count += tu_lens[i];
      }
      info->transitive_use[tup] = NULL;
      info->transitive_use_len[tup] = 0;
    }
  }

  info->preserves_marks = 1;
  info->single_result = 1;
  info->size += 1;

  if (tu_count) {
    tu = MALLOC_N_ATOMIC(mzshort, tu_count);
    tu_count = 0;
    for (i = 0; i < seq->count; i++) {
      if (tus[i]) {
        memcpy(tu + tu_count, tus[i], tu_lens[i] * sizeof(mzshort));
        tu_count += tu_lens[i];
      }
    }
    info->transitive_use[tup] = tu;
    info->transitive_use_len[tup] = tu_count;
  }

  return expr;
}

static Scheme_Object *
case_lambda_clone(int dup_ok, Scheme_Object *data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Object *le;
  int i, sz;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)data;
  Scheme_Case_Lambda *seq2;

  sz = sizeof(Scheme_Case_Lambda) + ((seq->count - mzFLEX_DELTA) * sizeof(Scheme_Object*));
  seq2 = (Scheme_Case_Lambda *)scheme_malloc_tagged(sz);
  memcpy(seq2, seq, sz);

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = optimize_clone(dup_ok, le, info, delta, closure_depth);
    if (!le) return NULL;
    seq2->array[i] = le;
  }

  return (Scheme_Object *)seq2;
}

static Scheme_Object *
case_lambda_shift(Scheme_Object *data, int delta, int after_depth)
{
  Scheme_Object *le;
  int i;
  Scheme_Case_Lambda *seq = (Scheme_Case_Lambda *)data;

  for (i = 0; i < seq->count; i++) {
    le = seq->array[i];
    le = optimize_shift(le, delta, after_depth);
    seq->array[i] = le;
  }

  return data;
}

static Scheme_Object *
begin0_optimize(Scheme_Object *obj, Optimize_Info *info, int context)
{
  int i, count;

  count = ((Scheme_Sequence *)obj)->count;

  for (i = 0; i < count; i++) {
    Scheme_Object *le;
    le = scheme_optimize_expr(((Scheme_Sequence *)obj)->array[i], info,
                              (!i
                               ? scheme_optimize_result_context(context)
                               : 0));
    ((Scheme_Sequence *)obj)->array[i] = le;
  }

  /* Optimization of expression 0 has already set single_result */
  info->preserves_marks = 1;

  info->size += 1;

  return obj;
}

static Scheme_Object *do_define_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info)
{
  Scheme_Object *val;
  Optimize_Info *einfo;

  val = SCHEME_VEC_ELS(data)[3];

  einfo = scheme_optimize_info_create(info->cp);
  if (info->inline_fuel < 0)
    einfo->inline_fuel = -1;

  val = scheme_optimize_expr(val, einfo, 0);

  SCHEME_VEC_ELS(data)[3] = val;

  return data;
}

static Scheme_Object *define_syntaxes_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  return do_define_syntaxes_optimize(data, info);
}

static Scheme_Object *begin_for_syntax_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Object *l, *a;
  Optimize_Info *einfo;

  l = SCHEME_VEC_ELS(data)[2];

  while (!SCHEME_NULLP(l)) {
    einfo = scheme_optimize_info_create(info->cp);
    if (info->inline_fuel < 0)
      einfo->inline_fuel = -1;
    
    a = SCHEME_CAR(l);
    a = scheme_optimize_expr(a, einfo, 0);
    SCHEME_CAR(l) = a;

    l = SCHEME_CDR(l);
  }

  return data;
}

/*========================================================================*/
/*                    let, let-values, letrec, etc.                       */
/*========================================================================*/

static int is_liftable_prim(Scheme_Object *v)
{
  if (SCHEME_PRIMP(v)) {
    if ((((Scheme_Primitive_Proc *)v)->pp.flags & SCHEME_PRIM_OPT_MASK)
        >= SCHEME_PRIM_OPT_IMMEDIATE)
      return 1;
  }

  return 0;
}

int scheme_is_liftable(Scheme_Object *o, int bind_count, int fuel, int as_rator)
{
  Scheme_Type t = SCHEME_TYPE(o);

  switch (t) {
  case scheme_compiled_unclosed_procedure_type:
    return !as_rator;
  case scheme_case_lambda_sequence_type:
    return !as_rator;
  case scheme_compiled_toplevel_type:
    return 1;
  case scheme_local_type:
    if (SCHEME_LOCAL_POS(o) > bind_count)
      return 1;
    break;
  case scheme_branch_type:
    if (fuel) {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)o;
      if (scheme_is_liftable(b->test, bind_count, fuel - 1, 0)
	  && scheme_is_liftable(b->tbranch, bind_count, fuel - 1, as_rator)
	  && scheme_is_liftable(b->fbranch, bind_count, fuel - 1, as_rator))
	return 1;
    }
    break;
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)o;
      int i;
      if (!is_liftable_prim(app->args[0]))
        return 0;
      if (0) /* not resolved, yet */
        if (bind_count >= 0)
          bind_count += app->num_args;
      for (i = app->num_args + 1; i--; ) {
	if (!scheme_is_liftable(app->args[i], bind_count, fuel - 1, 1))
	  return 0;
      }
      return 1;
    }
    break;
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)o;
      if (!is_liftable_prim(app->rator))
        return 0;
      if (0) /* not resolved, yet */
        if (bind_count >= 0)
          bind_count += 1;
      if (scheme_is_liftable(app->rator, bind_count, fuel - 1, 1)
	  && scheme_is_liftable(app->rand, bind_count, fuel - 1, 1))
	return 1;
    }
    break;
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)o;
      if (!is_liftable_prim(app->rator))
        return 0;
      if (0) /* not resolved, yet */
        if (bind_count >= 0)
          bind_count += 2;
      if (scheme_is_liftable(app->rator, bind_count, fuel - 1, 1)
	  && scheme_is_liftable(app->rand1, bind_count, fuel - 1, 1)
	  && scheme_is_liftable(app->rand2, bind_count, fuel - 1, 1))
	return 1;
    }
    break;
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *lh = (Scheme_Let_Header *)o;
      int i;
      int post_bind = !(SCHEME_LET_FLAGS(lh) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

      if (post_bind) {
        o = lh->body;
        for (i = lh->num_clauses; i--; ) {
          if (!scheme_is_liftable(((Scheme_Compiled_Let_Value *)o)->value, bind_count, fuel - 1, as_rator))
            return 0;
          o = ((Scheme_Compiled_Let_Value *)o)->body;
        }
        if (scheme_is_liftable(o, bind_count + lh->count, fuel - 1, as_rator))
          return 1;
      }
      break;
    }
  default:
    if (t > _scheme_compiled_values_types_)
      return 1;
  }

  return 0;
}

int scheme_compiled_propagate_ok(Scheme_Object *value, Optimize_Info *info)
{
  if (scheme_compiled_duplicate_ok(value, 0))
    return 1;

  if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_unclosed_procedure_type)) {
    int sz;
    sz = closure_body_size((Scheme_Closure_Data *)value, 1, info, NULL);
    if ((sz >= 0) && (sz <= MAX_PROC_INLINE_SIZE))
      return 1;
  }

  if (SAME_TYPE(scheme_case_lambda_sequence_type, SCHEME_TYPE(value))) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)value;
    int i;
    for (i = cl->count; i--; ) {
      if (!scheme_compiled_propagate_ok(cl->array[i], info))
        return 0;
    }
    return 1;
  }

  if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_toplevel_type)) {
    if ((SCHEME_TOPLEVEL_FLAGS(value) & SCHEME_TOPLEVEL_FLAGS_MASK) >= SCHEME_TOPLEVEL_FIXED)
      return 1;
    if (info->top_level_consts) {
      int pos;
      pos = SCHEME_TOPLEVEL_POS(value);
      value = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
      value = no_potential_size(value);
      if (value)
        return 1;
    }
  }

  return 0;
}

int scheme_is_statically_proc(Scheme_Object *value, Optimize_Info *info)
{
  while (1) {
    if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_unclosed_procedure_type))
      return 1;
    else if (SAME_TYPE(SCHEME_TYPE(value), scheme_case_lambda_sequence_type)) {
      return 1;
    } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_let_void_type)) {
      /* Look for (let ([x <proc>]) <proc>), which is generated for optional arguments. */
      Scheme_Let_Header *lh = (Scheme_Let_Header *)value;
      if (lh->num_clauses == 1) {
        Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
        if (scheme_omittable_expr(lv->value, lv->count, 20, 0, NULL, -1, 0)) {
          value = lv->body;
          info = NULL;
        } else
          break;
      } else
        break;
    } else
      break;
  }

  return 0;
}

Scheme_Object *scheme_make_noninline_proc(Scheme_Object *e)
{
  Scheme_Object *ni;

  ni = scheme_alloc_small_object();
  ni->type = scheme_noninline_proc_type;
  SCHEME_PTR_VAL(ni) = e;

  return ni;
}

static int is_values_apply(Scheme_Object *e)
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    return SAME_OBJ(scheme_values_func, app->args[0]);
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
    return SAME_OBJ(scheme_values_func, app->rator);
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    return SAME_OBJ(scheme_values_func, app->rator);
  }

  return 0;
}

static void unpack_values_application(Scheme_Object *e, Scheme_Compiled_Let_Value *naya,
                                      int rev_bind_order)
{
  if (SAME_TYPE(SCHEME_TYPE(e), scheme_application_type)) {
    Scheme_App_Rec *app = (Scheme_App_Rec *)e;
    int i;
    for (i = 0; i < app->num_args; i++) {
      if (rev_bind_order)
        naya->value = app->args[app->num_args - i];
      else
        naya->value = app->args[i + 1];
      naya = (Scheme_Compiled_Let_Value *)naya->body;
    }
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application2_type)) {
    Scheme_App2_Rec *app = (Scheme_App2_Rec *)e;
    naya->value = app->rand;
  } else if (SAME_TYPE(SCHEME_TYPE(e), scheme_application3_type)) {
    Scheme_App3_Rec *app = (Scheme_App3_Rec *)e;
    naya->value = (rev_bind_order ? app->rand2 : app->rand1);
    naya = (Scheme_Compiled_Let_Value *)naya->body;
    naya->value = (rev_bind_order ? app->rand1 : app->rand2);
  }
}

static Scheme_Object *make_clones(Scheme_Compiled_Let_Value *retry_start,
                                  Scheme_Compiled_Let_Value *pre_body,
                                  Optimize_Info *body_info)
{
  Scheme_Compiled_Let_Value *clv;
  Scheme_Object *value, *clone, *pr;
  Scheme_Object *last = NULL, *first = NULL;

  clv = retry_start;
  while (1) {
    value = clv->value;
    if (IS_COMPILED_PROC(value)) {
      clone = optimize_clone(1, value, body_info, 0, 0);
      if (clone) {
        pr = scheme_make_raw_pair(scheme_make_raw_pair(value, clone), NULL);
      } else
        pr = scheme_make_raw_pair(NULL, NULL);
      if (last)
        SCHEME_CDR(last) = pr;
      else
        first = pr;
      last = pr;
    }
    if (clv == pre_body)
      break;
    clv = (Scheme_Compiled_Let_Value *)clv->body;
  }

  return first;
}

static int set_one_code_flags(Scheme_Object *value, int flags,
                              Scheme_Object *first, Scheme_Object *second,
                              int set_flags, int mask_flags, int just_tentative,
                              int merge_flonum)
{
  Scheme_Case_Lambda *cl, *cl2, *cl3;
  Scheme_Closure_Data *data, *data2, *data3;
  int i, count;

  if (SAME_TYPE(scheme_compiled_unclosed_procedure_type, SCHEME_TYPE(value))) {
    count = 1;
    cl = NULL;
    cl2 = NULL;
    cl3 = NULL;
  } else {
    cl = (Scheme_Case_Lambda *)value;
    cl2 = (Scheme_Case_Lambda *)first;
    cl3 = (Scheme_Case_Lambda *)second;
    count = cl->count;
  }

  for (i = 0; i < count; i++) {
    if (cl) {
      data = (Scheme_Closure_Data *)cl->array[i];
      data2 = (Scheme_Closure_Data *)cl2->array[i];
      data3 = (Scheme_Closure_Data *)cl3->array[i];
    } else {
      data = (Scheme_Closure_Data *)value;
      data2 = (Scheme_Closure_Data *)first;
      data3 = (Scheme_Closure_Data *)second;
    }

    if (merge_flonum) {
      merge_closure_flonum_map(data, data2);
      merge_closure_flonum_map(data, data3);
      merge_closure_flonum_map(data, data2);
    }

    if (!just_tentative || (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE)) {
      flags = (flags & SCHEME_CLOSURE_DATA_FLAGS(data));
      SCHEME_CLOSURE_DATA_FLAGS(data2) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data2) & mask_flags);
      SCHEME_CLOSURE_DATA_FLAGS(data3) = set_flags | (SCHEME_CLOSURE_DATA_FLAGS(data3) & mask_flags);
    }
  }

  return flags;
}

static int set_code_flags(Scheme_Compiled_Let_Value *retry_start,
                          Scheme_Compiled_Let_Value *pre_body,
                          Scheme_Object *clones,
                          int set_flags, int mask_flags, int just_tentative,
                          int merge_flonum)
{
  Scheme_Compiled_Let_Value *clv;
  Scheme_Object *value, *first;
  int flags = CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS;

  /* The first in a clone pair is the one that is consulted for
     references. The second one is the clone, and it's the one whose
     flags are updated by optimization. So consult the clone, and set
     flags in both. */

  clv = retry_start;
  while (clones) {
    value = clv->value;
    if (IS_COMPILED_PROC(value)) {
      first = SCHEME_CAR(clones);

      if (first)
        flags = set_one_code_flags(value, flags,
                                   SCHEME_CAR(first), SCHEME_CDR(first),
                                   set_flags, mask_flags, just_tentative,
                                   merge_flonum);

      clones = SCHEME_CDR(clones);
    }

    if (clv == pre_body)
      break;
    clv = (Scheme_Compiled_Let_Value *)clv->body;
  }

  return flags;
}

static int compiled_proc_body_size(Scheme_Object *o, int less_args)
{
  int bsz;

  if (SAME_TYPE(SCHEME_TYPE(o), scheme_compiled_unclosed_procedure_type)) {
    bsz = closure_body_size((Scheme_Closure_Data *)o, 0, NULL, NULL);
    if (less_args) bsz -= ((Scheme_Closure_Data *)o)->num_params;
    return bsz;
  } else if (SAME_TYPE(SCHEME_TYPE(o), scheme_case_lambda_sequence_type)) {
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)o;
    int i, sz = 0;
    for (i = cl->count; i--; ) {
      bsz = closure_body_size((Scheme_Closure_Data *)cl->array[i], 0, NULL, NULL);
      if (less_args) {
        bsz -= ((Scheme_Closure_Data *)cl->array[i])->num_params;
        if (bsz > sz) sz = bsz;
      } else
        sz += bsz;
    }
    return sz;
  } else
    return 0;
}

static int expr_size(Scheme_Object *o, Optimize_Info *info)
{
  return compiled_proc_body_size(o, 0) + 1;
}

int scheme_might_invoke_call_cc(Scheme_Object *value)
{
  return !scheme_is_liftable(value, -1, 10, 0);
}

static int worth_lifting(Scheme_Object *v)
{
  Scheme_Type lhs;
  lhs = SCHEME_TYPE(v);
  if ((lhs == scheme_compiled_unclosed_procedure_type)
      || (lhs == scheme_case_lambda_sequence_type)
      || (lhs == scheme_local_type)
      || (lhs == scheme_compiled_toplevel_type)
      || (lhs == scheme_compiled_quote_syntax_type)
      || (lhs > _scheme_compiled_values_types_))
    return 1;
  return 0;
}

Scheme_Object *
scheme_optimize_lets(Scheme_Object *form, Optimize_Info *info, int for_inline, int context)
{
  Optimize_Info *sub_info, *body_info, *rhs_info;
  Scheme_Let_Header *head = (Scheme_Let_Header *)form;
  Scheme_Compiled_Let_Value *clv, *pre_body, *retry_start, *prev_body;
  Scheme_Object *body, *value, *ready_pairs = NULL, *rp_last = NULL, *ready_pairs_start;
  Scheme_Once_Used *first_once_used = NULL, *last_once_used = NULL, *once_used;
  int i, j, pos, is_rec, not_simply_let_star = 0, undiscourage, split_shift, skip_opts = 0;
  int did_set_value, checked_once;
  int remove_last_one = 0, inline_fuel, rev_bind_order;
  int post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

# define pos_EARLIER(a, b) (rev_bind_order ? ((a) > (b)) : ((a) < (b)))

  if (context & OPT_CONTEXT_BOOLEAN) {
    /* Special case: (let ([x M]) (if x x N)), where x is not in N,
       to (if M #t N), since we're in a test position. */
    if (!(SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE) && (head->count == 1) && (head->num_clauses == 1)) {
      clv = (Scheme_Compiled_Let_Value *)head->body;
      if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_branch_type)
          && (((clv->flags[0] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT)
              == 2)) {
        Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)clv->body;
        if (SAME_TYPE(SCHEME_TYPE(b->test), scheme_local_type)
            && SAME_TYPE(SCHEME_TYPE(b->tbranch), scheme_local_type)
            && !SCHEME_LOCAL_POS(b->test)
            && !SCHEME_LOCAL_POS(b->tbranch)) {
          Scheme_Branch_Rec *b3;

          b3 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
          b3->so.type = scheme_branch_type;
          b3->test = clv->value;
          b3->tbranch = scheme_true;
          if (post_bind) {
            /* still need a `let' around N: */
            b3->fbranch = (Scheme_Object *)head;
            clv->value = scheme_false;
            clv->flags[0] = 0; /* variable now unused */
            clv->body = b->fbranch;
          } else {
            b3->fbranch = b->fbranch;
          }

          if (post_bind)
            sub_info = info;
          else
            sub_info = optimize_info_add_frame(info, 1, 0, 0);

          form = scheme_optimize_expr((Scheme_Object *)b3, sub_info, context);

          if (!post_bind) {
            info->single_result = sub_info->single_result;
            info->preserves_marks = sub_info->preserves_marks;
            optimize_info_done(sub_info, NULL);
          }

          return form;
        }
      }
    }
  }

  /* Special case: (let ([x E]) x) where E is lambda, case-lambda, or
     a constant. (If we allowed arbitrary E here, it would affect the
     tailness of E.) */
  if (!(SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE) && (head->count == 1) && (head->num_clauses == 1)) {
    clv = (Scheme_Compiled_Let_Value *)head->body;
    if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_local_type)
        && (((Scheme_Local *)clv->body)->position == 0)) {
      if (worth_lifting(clv->value)) {
        if (post_bind) {
	  /* Just drop the let */
	  return scheme_optimize_expr(clv->value, info, context);
	} else {
	  info = optimize_info_add_frame(info, 1, 0, 0);
	  body = scheme_optimize_expr(clv->value, info, context);
          info->next->single_result = info->single_result;
          info->next->preserves_marks = info->preserves_marks;
	  optimize_info_done(info, NULL);
	  return body;
	}
      }
    }
  }

  is_rec = (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE);

  if (!is_rec) {
    int try_again;
    do {
      try_again = 0;
      /* (let ([x (let~ ([y M]) N)]) P) => (let~ ([y M]) (let ([x N]) P))
         or (let ([x (begin M ... N)]) P) => (begin M ... (let ([x N]) P)) */
      if (post_bind) {
        if (head->num_clauses == 1) {
          clv = (Scheme_Compiled_Let_Value *)head->body; /* ([x ...]) */
          if (SAME_TYPE(SCHEME_TYPE(clv->value), scheme_compiled_let_void_type)) {
            Scheme_Let_Header *lh = (Scheme_Let_Header *)clv->value; /* (let~ ([y ...]) ...) */

            value = clv->body; /* = P */
            if (lh->count)
              value = optimize_shift(value, lh->count, head->count);
            if (value) {
              clv->body = value;

              if (!lh->num_clauses) {
                clv->value = lh->body;
                lh->body = (Scheme_Object *)head;
              } else {
                body = lh->body;
                for (i = lh->num_clauses - 1; i--; ) {
                  body = ((Scheme_Compiled_Let_Value *)body)->body;
                }
                clv->value = ((Scheme_Compiled_Let_Value *)body)->body; /* N */
                ((Scheme_Compiled_Let_Value *)body)->body = (Scheme_Object *)head;
              }

              head = lh;
              form = (Scheme_Object *)head;
              is_rec = (SCHEME_LET_FLAGS(head) & SCHEME_LET_RECURSIVE);
              post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));
              try_again = 1;
            }
          } else if (SAME_TYPE(SCHEME_TYPE(clv->value), scheme_sequence_type)) {
            Scheme_Sequence *seq = (Scheme_Sequence *)clv->value; /* (begin M ... N) */

            clv->value = seq->array[seq->count - 1];
            seq->array[seq->count - 1] = (Scheme_Object *)head;

            return scheme_optimize_expr((Scheme_Object *)seq, info, context);
          }
        }
      }
    } while (try_again);
  }

  split_shift = 0;
  if (is_rec) {
    /* Check whether we should break a prefix out into its own
       letrec set. */
    body = head->body;
    j = 0;
    for (i = 0; i < head->num_clauses - 1; i++) {
      pre_body = (Scheme_Compiled_Let_Value *)body;
      if (SCHEME_CLV_FLAGS(pre_body) & SCHEME_CLV_NO_GROUP_LATER_USES) {
        /* yes --- break group here */
        Scheme_Let_Header *h2;

        j += pre_body->count;
        i++;

        h2 = MALLOC_ONE_TAGGED(Scheme_Let_Header);
        h2->iso.so.type = scheme_compiled_let_void_type;
        h2->count = head->count - j;
        h2->num_clauses = head->num_clauses - i;
        h2->body = pre_body->body;
        SCHEME_LET_FLAGS(h2) = SCHEME_LET_RECURSIVE;

        head->count = j;
        head->num_clauses = i;

        pre_body->body = (Scheme_Object *)h2;

        split_shift = h2->count;

        body = head->body;
        for (j = 0; j < i; j++) {
          pre_body = (Scheme_Compiled_Let_Value *)body;
          pre_body->position -= split_shift;
          body = pre_body->body;
        }

        break;
      } else {
        j += pre_body->count;
        body = pre_body->body;
      }
    }
  }

  body_info = optimize_info_add_frame(info, head->count, head->count,
                                      post_bind ? SCHEME_POST_BIND_FRAME : 0);
  if (post_bind)
    rhs_info = optimize_info_add_frame(info, 0, 0, 0);
  else if (split_shift)
    rhs_info = optimize_info_add_frame(body_info, split_shift, 0, 0);
  else
    rhs_info = body_info;

  body = head->body;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;
    for (j = pre_body->count; j--; ) {
      if (pre_body->flags[j] & SCHEME_WAS_SET_BANGED) {
	optimize_mutated(body_info, pos + j);
      } else if (is_rec) {
        /* Indicate that it's not yet ready, so it cannot be inlined: */
        Scheme_Object *rp;
        rp = scheme_make_raw_pair(scheme_false, NULL);
        if (rp_last)
          SCHEME_CDR(rp_last) = rp;
        else
          ready_pairs = rp;
        rp_last = rp;
        optimize_propagate(body_info, pos+j, rp_last, 0);
      }
    }
    body = pre_body->body;
  }

  if (OPT_ESTIMATE_FUTURE_SIZES) {
    if (is_rec && !body_info->letrec_not_twice) {
      /* For each identifier bound to a procedure, register an initial
         size estimate, which is used to discourage early loop unrolling
         at the expense of later inlining. */
      body = head->body;
      pre_body = NULL;
      for (i = head->num_clauses; i--; ) {
        pre_body = (Scheme_Compiled_Let_Value *)body;
        pos = pre_body->position;

        if ((pre_body->count == 1)
            && IS_COMPILED_PROC(pre_body->value)
            && !(pre_body->flags[0] & SCHEME_WAS_SET_BANGED)) {
          optimize_propagate(body_info, pos, estimate_closure_size(pre_body->value), 0);
        }

        body = pre_body->body;
      }
      rhs_info->use_psize = 1;
    }
  }

  rev_bind_order = 0;
  if (is_rec)
    rev_bind_order = 1;
  else if (head->num_clauses > 1) {
    int pos;
    body = head->body;
    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;
    body = pre_body->body;
    for (i = head->num_clauses - 1; i--; ) {
      pre_body = (Scheme_Compiled_Let_Value *)body;
      if (pre_body->position < pos) {
        rev_bind_order = 1;
        break;
      } else if (pre_body->position > pos) {
        break;
      }
      body = pre_body->body;
    }
  }

  prev_body = NULL;
  body = head->body;
  pre_body = NULL;
  retry_start = NULL;
  ready_pairs_start = NULL;
  did_set_value = 0;
  for (i = head->num_clauses; i--; ) {
    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;

    if ((pre_body->count == 1)
        && IS_COMPILED_PROC(pre_body->value)
        && !optimize_is_used(body_info, pos)) {
      if (!body_info->transitive_use) {
        mzshort **tu;
        int *tu_len;
        tu = (mzshort **)scheme_malloc(sizeof(mzshort *) * head->count);
        tu_len = (int *)scheme_malloc_atomic(sizeof(int) * head->count);
        memset(tu_len, 0, sizeof(int) * head->count);
        body_info->transitive_use = tu;
        body_info->transitive_use_len = tu_len;
      }
      body_info->transitive_use_pos = pos + 1;
    }

    if (is_rec && OPT_DISCOURAGE_EARLY_INLINE && !rhs_info->letrec_not_twice
        && IS_COMPILED_PROC(pre_body->value)) {
      inline_fuel = rhs_info->inline_fuel;
      if (inline_fuel > 2)
        rhs_info->inline_fuel = 2;
      rhs_info->letrec_not_twice++;
      undiscourage = 1;
    } else {
      inline_fuel = 0;
      undiscourage = 0;
    }

    if (!skip_opts) {
      value = scheme_optimize_expr(pre_body->value, rhs_info, 0);
      pre_body->value = value;
    } else {
      value = pre_body->value;
      --skip_opts;
    }

    if (undiscourage) {
      rhs_info->inline_fuel = inline_fuel;
      --rhs_info->letrec_not_twice;
    }

    body_info->transitive_use_pos = 0;

    if (is_rec && !not_simply_let_star) {
      /* Keep track of whether we can simplify to let*: */
      if (scheme_might_invoke_call_cc(value)
          || optimize_any_uses(body_info, 0, pos+pre_body->count))
        not_simply_let_star = 1;
    }

    /* Change (let-values ([(id ...) (values e ...)]) body)
       to (let-values ([id e] ...) body) for simple e. */
    if ((pre_body->count != 1)
        && is_values_apply(value)
        && scheme_omittable_expr(value, pre_body->count, -1, 0, info,
                                 (is_rec
                                  ? (pre_body->position + pre_body->count)
                                  : -1),
                                 0)) {
      if (!pre_body->count && !i) {
        /* We want to drop the clause entirely, but doing it
           here messes up the loop for letrec. So wait and
           remove it at the end. */
        remove_last_one = 1;
      } else {
        Scheme_Compiled_Let_Value *naya;
        Scheme_Object *rest = pre_body->body;
        int *new_flags;
        int cnt;

        /* This conversion may reorder the expressions. */
        if (pre_body->count) {
          if (rev_bind_order)
            cnt = 0;
          else
            cnt = pre_body->count - 1;

          while (1) {
            naya = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
            naya->iso.so.type = scheme_compiled_let_value_type;
            naya->body = rest;
            naya->count = 1;
            naya->position = pre_body->position + cnt;
            new_flags = (int *)scheme_malloc_atomic(sizeof(int));
            new_flags[0] = pre_body->flags[cnt];
            naya->flags = new_flags;
            rest = (Scheme_Object *)naya;

            if (rev_bind_order) {
              cnt++;
              if (cnt >= pre_body->count)
                break;
            } else {
              if (!cnt)
                break;
              cnt--;
            }
          }
        }

        naya = (Scheme_Compiled_Let_Value *)rest;
        unpack_values_application(value, naya, rev_bind_order);
        if (prev_body)
          prev_body->body = (Scheme_Object *)naya;
        else
          head->body = (Scheme_Object *)naya;
        head->num_clauses += (pre_body->count - 1);
        i += (pre_body->count - 1);
        if (pre_body->count) {
          /* We're backing up. Since the RHSs have been optimized
             already, don re-optimize. */
          skip_opts = pre_body->count - 1;
          pre_body = naya;
          body = (Scheme_Object *)naya;
          value = pre_body->value;
          pos = pre_body->position;
        } else {
          /* We've dropped this clause entirely. */
          i++;
          if (i > 0) {
            body = (Scheme_Object *)naya;
            continue;
          } else
            break;
        }
      }
    }

    checked_once = 0;

    if ((pre_body->count == 1)
	&& !(pre_body->flags[0] & SCHEME_WAS_SET_BANGED)) {
      int indirect = 0, indirect_binding = 0;

      while (indirect < 10) {
        if (SAME_TYPE(SCHEME_TYPE(value), scheme_sequence_type)) {
          Scheme_Sequence *seq = (Scheme_Sequence *)value;
          value = seq->array[seq->count - 1];
          indirect++;
        } else if (SAME_TYPE(SCHEME_TYPE(value), scheme_compiled_let_void_type)) {
          Scheme_Let_Header *head2 = (Scheme_Let_Header *)value;
          int i;

          if (head2->num_clauses < 10) {
            value = head2->body;
            for (i = head2->num_clauses; i--; ) {
              value = ((Scheme_Compiled_Let_Value *)value)->body;
            }
          }
          indirect++;
          if (head2->count)
            indirect_binding = 1;
        } else
          break;
      }

      if (indirect_binding) {
        /* only allow constants */
        if (SCHEME_TYPE(value) < _scheme_compiled_values_types_)
          value = NULL;
      }

      if (value && SAME_TYPE(SCHEME_TYPE(value), scheme_local_type)) {
        /* Don't optimize reference to a local binding
           that's not available yet, or that's mutable. */
        int vpos;
        vpos = SCHEME_LOCAL_POS(value);
        if (!post_bind && (vpos < head->count) && !pos_EARLIER(vpos, pos))
          value = NULL;
        else {
          /* Convert value back to a pre-optimized local coordinates.
             Unless post_bind, this must be done with respect to
             body_info, not rhs_info, because we attach the value to
             body_info: */
          value = optimize_reverse(post_bind ? rhs_info : body_info, vpos, 1, 0);

          /* Double-check that the value is ready, because we might be
             nested in the RHS of a `letrec': */
          if (value)
            if (!optimize_info_is_ready(body_info, SCHEME_LOCAL_POS(value)))
              value = NULL;
        }
      }

      if (value && (scheme_compiled_propagate_ok(value, body_info))) {
        int cnt;

        if (is_rec)
          cnt = 2;
        else
          cnt = ((pre_body->flags[0] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);

        optimize_propagate(body_info, pos, value, cnt == 1);
	did_set_value = 1;
        checked_once = 1;
      } else if (value && !is_rec) {
        int cnt;

        if (scheme_expr_produces_flonum(value))
          optimize_produces_flonum(body_info, pos);

        if (!indirect) {
          checked_once = 1;
          cnt = ((pre_body->flags[0] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
          if (cnt == 1) {
            /* used only once; we may be able to shift the expression to the use
               site, instead of binding to a temporary */
            once_used = make_once_used(value, pos, rhs_info->vclock, NULL);
            if (!last_once_used)
              first_once_used = once_used;
            else
              last_once_used->next = once_used;
            last_once_used = once_used;
            optimize_propagate(body_info, pos, (Scheme_Object *)once_used, 1);
          }
        }
      }
    }

    if (!checked_once) {
      /* Didn't handle once-used check in case of copy propagation, so check here. */
      int i, cnt;
      for (i = pre_body->count; i--; ) {
        if (!(pre_body->flags[i] & SCHEME_WAS_SET_BANGED)) {
          cnt = ((pre_body->flags[i] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
          if (cnt == 1) {
            /* Need to register as once-used, in case of copy propagation */
            once_used = make_once_used(NULL, pos+i, rhs_info->vclock, NULL);
            if (!last_once_used)
              first_once_used = once_used;
            else
              last_once_used->next = once_used;
            last_once_used = once_used;
            optimize_propagate(body_info, pos+i, (Scheme_Object *)once_used, 1);
          }
        }
      }
    }

    if (!retry_start) {
      retry_start = pre_body;
      ready_pairs_start = ready_pairs;
    }

    /* Re-optimize to inline letrec bindings? */
    if (is_rec
	&& !body_info->letrec_not_twice
	&& ((i < 1)
	    || (!scheme_is_compiled_procedure(((Scheme_Compiled_Let_Value *)pre_body->body)->value, 1, 1)
		&& !scheme_is_liftable(((Scheme_Compiled_Let_Value *)pre_body->body)->value, head->count, 5, 1)))) {
      Scheme_Object *prop_later = NULL;

      if (did_set_value) {
	/* Next RHS ends a reorderable sequence.
	   Re-optimize from retry_start to pre_body, inclusive.
           For procedures, assume CLOS_SINGLE_RESULT and CLOS_PRESERVES_MARKS for all,
           but then assume not for all if any turn out not (i.e., approximate fix point). */
        int flags;
        Scheme_Object *clones, *cl, *cl_first;
        /* Reset "ready" flags: */
        for (rp_last = ready_pairs_start; !SAME_OBJ(rp_last, ready_pairs); rp_last = SCHEME_CDR(rp_last)) {
          SCHEME_CAR(rp_last) = scheme_false;
        }
        /* Set-flags loop: */
        clones = make_clones(retry_start, pre_body, rhs_info);
        (void)set_code_flags(retry_start, pre_body, clones,
                             CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE,
                             0xFFFF,
                             0,
                             0);
        /* Re-optimize loop: */
        clv = retry_start;
        cl = clones;
	while (1) {
	  value = clv->value;
          if (cl) {
            cl_first = SCHEME_CAR(cl);
            if (!cl_first)
              cl = SCHEME_CDR(cl);
          } else
            cl_first = NULL;
	  if (cl_first && SAME_OBJ(value, SCHEME_CAR(cl_first))) {
            /* Try optimization. */
	    Scheme_Object *self_value;
            int sz;
            char use_psize;

            if ((clv->count == 1)
                && rhs_info->transitive_use
                && !optimize_is_used(body_info, clv->position)) {
              body_info->transitive_use[clv->position] = NULL;
              body_info->transitive_use_pos = clv->position + 1;
            }

            cl = SCHEME_CDR(cl);
	    self_value = SCHEME_CDR(cl_first);

            /* Drop old size, and remove old inline fuel: */
            sz = compiled_proc_body_size(value, 0);
            rhs_info->size -= (sz + 1);

            /* Setting letrec_not_twice prevents inlinining
               of letrec bindings in this RHS. There's a small
               chance that we miss some optimizations, but we
               avoid the possibility of N^2 behavior. */
            if (!OPT_DISCOURAGE_EARLY_INLINE)
              rhs_info->letrec_not_twice++;
            use_psize = rhs_info->use_psize;
            rhs_info->use_psize = info->use_psize;

            value = scheme_optimize_expr(self_value, rhs_info, 0);

            if (!OPT_DISCOURAGE_EARLY_INLINE)
              --rhs_info->letrec_not_twice;
            rhs_info->use_psize = use_psize;

            clv->value = value;

            if (!(clv->flags[0] & SCHEME_WAS_SET_BANGED)) {
              if (scheme_compiled_propagate_ok(value, rhs_info)) {
                /* Register re-optimized as the value for the binding, but
                   maybe only if it didn't grow too much: */
                int new_sz;
                if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE)
                  new_sz = compiled_proc_body_size(value, 0);
                else
                  new_sz = 0;
                if (new_sz <= sz)
                  optimize_propagate(body_info, clv->position, value, 0);
                else if (!OPT_LIMIT_FUNCTION_RESIZE
                         || (new_sz < 4 * sz))
                  prop_later = scheme_make_raw_pair(scheme_make_pair(scheme_make_integer(clv->position),
                                                                     value),
                                                    prop_later);
              }
            }

            body_info->transitive_use_pos = 0;
	  }
	  if (clv == pre_body)
	    break;
          {
            /* Since letrec is really letrec*, the variables
               for this binding are now ready: */
            int i;
            for (i = clv->count; i--; ) {
              if (!(clv->flags[i] & SCHEME_WAS_SET_BANGED)) {
                SCHEME_CAR(ready_pairs_start) = scheme_true;
                ready_pairs_start = SCHEME_CDR(ready_pairs_start);
              }
            }
          }
	  clv = (Scheme_Compiled_Let_Value *)clv->body;
	}
        /* Check flags loop: */
        flags = set_code_flags(retry_start, pre_body, clones, 0, 0xFFFF, 0, 0);
        /* Reset-flags loop: */
        (void)set_code_flags(retry_start, pre_body, clones,
                             (flags & (CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS)),
                             ~(CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE),
                             1,
                             1);
      }
      retry_start = NULL;
      ready_pairs_start = NULL;
      did_set_value = 0;

      while (prop_later) {
        value = SCHEME_CAR(prop_later);
        optimize_propagate(body_info,
                           SCHEME_INT_VAL(SCHEME_CAR(value)),
                           SCHEME_CDR(value),
                           0);
        prop_later = SCHEME_CDR(prop_later);
      }
    }

    if (is_rec) {
      /* Since letrec is really letrec*, the variables
         for this binding are now ready: */
      int i;
      for (i = pre_body->count; i--; ) {
        if (!(pre_body->flags[i] & SCHEME_WAS_SET_BANGED)) {
          SCHEME_CAR(ready_pairs) = scheme_true;
          ready_pairs = SCHEME_CDR(ready_pairs);
        }
      }
    }

    if (remove_last_one) {
      head->num_clauses -= 1;
      body = (Scheme_Object *)pre_body->body;
      if (prev_body) {
        prev_body->body = body;
        pre_body = prev_body;
      } else {
        head->body = body;
        pre_body = NULL;
      }
      break;
    }

    prev_body = pre_body;
    body = pre_body->body;
  }

  if (post_bind)
    optimize_info_done(rhs_info, body_info);
  else if (split_shift)
    optimize_info_done(rhs_info, body_info);

  body = scheme_optimize_expr(body, body_info, scheme_optimize_tail_context(context));
  if (head->num_clauses)
    pre_body->body = body;
  else
    head->body = body;

  info->single_result = body_info->single_result;
  info->preserves_marks = body_info->preserves_marks;
  info->vclock = body_info->vclock;

  /* Clear used flags where possible */
  body = head->body;
  for (i = head->num_clauses; i--; ) {
    int used = 0, j;

    pre_body = (Scheme_Compiled_Let_Value *)body;
    pos = pre_body->position;

    for (j = pre_body->count; j--; ) {
      if (optimize_is_used(body_info, pos+j)) {
        used = 1;
        break;
      }
    }

    if (!used
        && (scheme_omittable_expr(pre_body->value, pre_body->count, -1, 0, info, -1, 0)
            || ((pre_body->count == 1)
                && first_once_used
                && (first_once_used->pos == pos)
                && first_once_used->used))) {
      for (j = pre_body->count; j--; ) {
        if (pre_body->flags[j] & SCHEME_WAS_USED) {
          pre_body->flags[j] -= SCHEME_WAS_USED;
        }

        if (first_once_used && (first_once_used->pos == (pos + j)))
          first_once_used = first_once_used->next;
      }
      if (pre_body->count == 1) {
        /* Drop expr and deduct from size to aid further inlining. */
        int sz;
        sz = expr_size(pre_body->value, info);
        pre_body->value = scheme_false;
        info->size -= sz;
      }
    } else {
      for (j = pre_body->count; j--; ) {
        pre_body->flags[j] |= SCHEME_WAS_USED;
        if (optimize_is_flonum_arg(body_info, pos+j, 0))
          pre_body->flags[j] |= SCHEME_WAS_FLONUM_ARGUMENT;

        if (first_once_used && (first_once_used->pos == (pos+j))) {
          if (first_once_used->vclock < 0) {
            /* single-use no longer true, due to copy propagation */
            pre_body->flags[j] |= SCHEME_USE_COUNT_MASK;
          }
          first_once_used = first_once_used->next;
        }
      }
      info->size += 1;
    }
    body = pre_body->body;
  }

  /* Optimized away all clauses? */
  if (!head->num_clauses) {
    optimize_info_done(body_info, NULL);
    return head->body;
  }

  if (is_rec && !not_simply_let_star) {
    /* We can simplify letrec to let* */
    SCHEME_LET_FLAGS(head) -= SCHEME_LET_RECURSIVE;
    SCHEME_LET_FLAGS(head) |= SCHEME_LET_STAR;
  }

  {
    int extract_depth = 0;

    value = NULL;

    /* Check again for (let ([x <proc>]) x). */
    if (!is_rec && (head->count == 1) && (head->num_clauses == 1)) {
      clv = (Scheme_Compiled_Let_Value *)head->body;
      if (SAME_TYPE(SCHEME_TYPE(clv->body), scheme_local_type)
          && (((Scheme_Local *)clv->body)->position == 0)) {
        if (worth_lifting(clv->value)) {
          value = clv->value;
          extract_depth = 1;
        }
      }
    }

    /* Check for (let ([unused #f] ...) <proc>) */
    if (!value) {
      if (head->count == head->num_clauses) {
        body = head->body;
        for (i = head->num_clauses; i--; ) {
          pre_body = (Scheme_Compiled_Let_Value *)body;
          if ((pre_body->count != 1)
              || !SCHEME_FALSEP(pre_body->value)
              || (pre_body->flags[0] & SCHEME_WAS_USED))
            break;
          body = pre_body->body;
        }
        if (i < 0) {
          if (worth_lifting(body)) {
            value = body;
            extract_depth = head->count;
            rhs_info = body_info;
            post_bind = 0;
          }
        }
      }
    }

    if (value) {
      value = optimize_clone(1, value, rhs_info, 0, 0);

      if (value) {
        sub_info = optimize_info_add_frame(info, post_bind ? 0 : extract_depth, 0, 0);
        sub_info->inline_fuel = 0;
        value = scheme_optimize_expr(value, sub_info, context);
        info->single_result = sub_info->single_result;
        info->preserves_marks = sub_info->preserves_marks;
        optimize_info_done(sub_info, NULL);
        return value;
      }
    }
  }

  optimize_info_done(body_info, NULL);

  return form;
}

/*========================================================================*/
/*                             closures                                   */
/*========================================================================*/

static Scheme_Object *
optimize_closure_compilation(Scheme_Object *_data, Optimize_Info *info, int context)
{
  Scheme_Closure_Data *data;
  Scheme_Object *code, *ctx;
  Closure_Info *cl;
  mzshort dcs, *dcm;
  int i, cnt;
  Scheme_Once_Used *first_once_used = NULL, *last_once_used = NULL;

  data = (Scheme_Closure_Data *)_data;

  info->single_result = 1;
  info->preserves_marks = 1;

  info = optimize_info_add_frame(info, data->num_params, data->num_params,
                                 SCHEME_LAMBDA_FRAME);

  info->vclock += 1; /* model delayed evaluation as vclock increment */

  /* For reporting warnings: */
  if (info->context && SCHEME_PAIRP(info->context))
    ctx = scheme_make_pair((Scheme_Object *)data,
                           SCHEME_CDR(info->context));
  else if (info->context)
    ctx = scheme_make_pair((Scheme_Object *)data, info->context);
  else
    ctx = (Scheme_Object *)data;
  info->context = ctx;

  cl = (Closure_Info *)data->closure_map;
  for (i = 0; i < data->num_params; i++) {
    if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
      optimize_mutated(info, i);

    cnt = ((cl->local_flags[i] & SCHEME_USE_COUNT_MASK) >> SCHEME_USE_COUNT_SHIFT);
    if (cnt == 1) {
      last_once_used = make_once_used(NULL, i, info->vclock, last_once_used);
      if (!first_once_used) first_once_used = last_once_used;
      optimize_propagate(info, i, (Scheme_Object *)last_once_used, 1);
    }
  }

  code = scheme_optimize_expr(data->code, info, 0);

  for (i = 0; i < data->num_params; i++) {
    if (optimize_is_flonum_arg(info, i, 1))
      cl->local_flags[i] |= SCHEME_WAS_FLONUM_ARGUMENT;
  }

  while (first_once_used) {
    if (first_once_used->vclock < 0) {
      /* no longer used once, due to binding propagation */
      cl->local_flags[first_once_used->pos] |= SCHEME_USE_COUNT_MASK;
    }
    first_once_used = first_once_used->next;
  }

  if (info->single_result)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_SINGLE_RESULT;
  else if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_SINGLE_RESULT)
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_SINGLE_RESULT;

  if (info->preserves_marks)
    SCHEME_CLOSURE_DATA_FLAGS(data) |= CLOS_PRESERVES_MARKS;
  else if (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_PRESERVES_MARKS)
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_PRESERVES_MARKS;

  if ((info->single_result > 0) && (info->preserves_marks > 0)
      && (SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_RESULT_TENTATIVE))
    SCHEME_CLOSURE_DATA_FLAGS(data) -= CLOS_RESULT_TENTATIVE;

  data->code = code;

  /* Remembers positions of used vars (and unsets usage for this level) */
  env_make_closure_map(info, &dcs, &dcm);
  cl->base_closure_size = dcs;
  cl->base_closure_map = dcm;
  if (env_uses_toplevel(info))
    cl->has_tl = 1;
  else
    cl->has_tl = 0;
  cl->body_size = info->size;
  cl->body_psize = info->psize;
  cl->has_nonleaf = info->has_nonleaf;

  info->size++;

  data->closure_size = (cl->base_closure_size
			+ (cl->has_tl ? 1 : 0));

  optimize_info_done(info, NULL);

  return (Scheme_Object *)data;
}

static char *get_closure_flonum_map(Scheme_Closure_Data *data, int arg_n, int *ok)
{
  Closure_Info *cl = (Closure_Info *)data->closure_map;

  if ((SCHEME_CLOSURE_DATA_FLAGS(data) & CLOS_HAS_REST)
      || (arg_n != data->num_params)) {
    *ok = 0;
    return NULL;
  }

  if (cl->has_flomap && !cl->flonum_map) {
    *ok = 0;
    return NULL;
  }

  *ok = 1;
  return cl->flonum_map;
}

static void set_closure_flonum_map(Scheme_Closure_Data *data, char *flonum_map)
{
  Closure_Info *cl = (Closure_Info *)data->closure_map;
  int i;

  if (!cl->flonum_map) {
    cl->has_flomap = 1;
    cl->flonum_map = flonum_map;
  }

  if (flonum_map) {
    for (i = data->num_params; i--; ) {
      if (flonum_map[i]) break;
    }

    if (i < 0) {
      cl->flonum_map = NULL;
    }
  }
}

static void merge_closure_flonum_map(Scheme_Closure_Data *data1, Scheme_Closure_Data *data2)
{
  Closure_Info *cl1 = (Closure_Info *)data1->closure_map;
  Closure_Info *cl2 = (Closure_Info *)data2->closure_map;

  if (cl1->has_flomap) {
    if (!cl1->flonum_map || !cl2->has_flomap) {
      cl2->has_flomap = 1;
      cl2->flonum_map = cl1->flonum_map;
    } else if (cl2->flonum_map) {
      int i;
      for (i = data1->num_params; i--; ) {
        if (cl1->flonum_map[i] != cl2->flonum_map[i]) {
          cl2->flonum_map = NULL;
          cl1->flonum_map = NULL;
          break;
        }
      }
    } else {
      cl1->flonum_map = NULL;
    }
  } else if (cl2->has_flomap) {
    cl1->has_flomap = 1;
    cl1->flonum_map = cl2->flonum_map;
  }
}

static Scheme_Object *clone_closure_compilation(int dup_ok, Scheme_Object *_data, Optimize_Info *info, int delta, int closure_depth)
{
  Scheme_Closure_Data *data, *data2;
  Scheme_Object *body;
  Closure_Info *cl;
  int *flags, sz;
  char *flonum_map;

  data = (Scheme_Closure_Data *)_data;

  body = optimize_clone(dup_ok, data->code, info, delta, closure_depth + data->num_params);
  if (!body) return NULL;

  data2 = MALLOC_ONE_TAGGED(Scheme_Closure_Data);
  memcpy(data2, data, sizeof(Scheme_Closure_Data));

  data2->code = body;

  cl = MALLOC_ONE_RT(Closure_Info);
  memcpy(cl, data->closure_map, sizeof(Closure_Info));
  data2->closure_map = (mzshort *)cl;

  /* We don't have to update base_closure_map, because
     it will get re-computed as the closure is re-optimized. */

  sz = sizeof(int) * data2->num_params;
  flags = (int *)scheme_malloc_atomic(sz);
  memcpy(flags, cl->local_flags, sz);
  cl->local_flags = flags;

  if (cl->flonum_map) {
    sz = data2->num_params;
    flonum_map = (char *)scheme_malloc_atomic(sz);
    memcpy(flonum_map, cl->flonum_map, sz);
    cl->flonum_map = flonum_map;
  }

  return (Scheme_Object *)data2;
}

static Scheme_Object *shift_closure_compilation(Scheme_Object *_data, int delta, int after_depth)
{
  Scheme_Object *expr;
  Scheme_Closure_Data *data = (Scheme_Closure_Data *)_data;
  Closure_Info *cl;
  int i, sz;
  mzshort *naya;

  after_depth += data->num_params;

  expr = optimize_shift(data->code, delta, after_depth);
  data->code = expr;

  /* In case the result is not going to be re-optimized, we need
     to update base_closure_map. */

  sz = data->closure_size;
  cl = (Closure_Info *)data->closure_map;
  naya = MALLOC_N_ATOMIC(mzshort, sz);

  for (i = 0; i < sz; i++) {
    naya[i] = cl->base_closure_map[i];
    if (naya[i] >= after_depth)
      naya[i] += delta;
  }

  cl->base_closure_map = naya;

  return _data;
}

static int closure_body_size(Scheme_Closure_Data *data, int check_assign,
                             Optimize_Info *info, int *is_leaf)
{
  int i;
  Closure_Info *cl;

  cl = (Closure_Info *)data->closure_map;

  if (check_assign) {
    /* Don't try to inline if any arguments are mutated: */
    for (i = data->num_params; i--; ) {
      if (cl->local_flags[i] & SCHEME_WAS_SET_BANGED)
	return -1;
    }
  }

  if (is_leaf)
    *is_leaf = !cl->has_nonleaf;

  return cl->body_size + ((info && info->use_psize) ? cl->body_psize : 0);
}

static int closure_has_top_level(Scheme_Closure_Data *data)
{
  Closure_Info *cl;

  cl = (Closure_Info *)data->closure_map;

  return cl->has_tl;
}

static int closure_argument_flags(Scheme_Closure_Data *data, int i)
{
  return ((Closure_Info *)data->closure_map)->local_flags[i];
}

/*========================================================================*/
/*                              modules                                   */
/*========================================================================*/

static int set_code_closure_flags(Scheme_Object *clones,
                                  int set_flags, int mask_flags,
                                  int just_tentative)
{
  Scheme_Object *clone, *orig, *first;
  int flags = CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS;

  /* The first in a clone pair is the one that is consulted for
     references. The second one is the original, and its the one whose
     flags are updated by optimization. So consult the original, and set
     flags in both. */

  while (clones) {
    first = SCHEME_CAR(clones);
    clone = SCHEME_CAR(first);
    orig = SCHEME_CDR(first);

    flags = set_one_code_flags(orig, flags,
                               orig, clone,
                               set_flags, mask_flags, just_tentative,
                               0);

    clones = SCHEME_CDR(clones);
  }

  return flags;
}

static Scheme_Object *is_cross_module_inline_candidiate(Scheme_Object *e, Optimize_Info *info,
                                                        int size_override)
{
  if (IS_COMPILED_PROC(e)) {
    if (size_override || (compiled_proc_body_size(e, 1) < CROSS_MODULE_INLINE_SIZE))
      return optimize_clone(0, e, info, 0, 0);
  }

  return NULL;
}

static int is_general_compiled_proc(Scheme_Object *e)
{
  /* recognize (begin <omitable>* <proc>) */
  if (SCHEME_TYPE(e) == scheme_sequence_type) {
    Scheme_Sequence *seq = (Scheme_Sequence *)e;
    if (seq->count > 0) {
      int i;
      for (i = seq->count - 1; i--; ) {
        if (!scheme_omittable_expr(seq->array[i], -1, 20, 0, NULL, -1, 0))
          return 0;
      }
    }
    e = seq->array[seq->count - 1];
  }

  /* recognize (let ([x <proc>]) x) */
  if (SCHEME_TYPE(e) == scheme_compiled_let_void_type) {
    Scheme_Let_Header *lh = (Scheme_Let_Header *)e;
    if (!(SCHEME_LET_FLAGS(lh) & SCHEME_LET_RECURSIVE)
        && (lh->count == 1) 
        && (lh->num_clauses == 1)
        && SAME_TYPE(SCHEME_TYPE(lh->body), scheme_compiled_let_value_type)) {
      Scheme_Compiled_Let_Value *lv = (Scheme_Compiled_Let_Value *)lh->body;
      if (IS_COMPILED_PROC(lv->value)) {
        if (SAME_TYPE(SCHEME_TYPE(lv->body), scheme_local_type))
          return (SCHEME_LOCAL_POS(lv->body) == 0);
      }
    }
  }

  if (IS_COMPILED_PROC(e))
    return 1;

  return 0;
}

static Scheme_Object *
module_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  Scheme_Module *m = (Scheme_Module *)data;
  Scheme_Object *e, *vars, *old_context;
  int start_simltaneous = 0, i_m, cnt;
  Scheme_Object *cl_first = NULL, *cl_last = NULL;
  Scheme_Hash_Table *consts = NULL, *fixed_table = NULL, *re_consts = NULL;
  Scheme_Hash_Table *originals = NULL;
  int cont, next_pos_ready = -1, inline_fuel, is_proc_def;
  Comp_Prefix *prev_cp;

  if (!m->comp_prefix) {
    /* already resolved */
    return (Scheme_Object *)m;
  }

  old_context = info->context;
  info->context = (Scheme_Object *)m;

  prev_cp = info->cp;
  info->cp = m->comp_prefix;

  cnt = SCHEME_VEC_SIZE(m->bodies[0]);

  if (OPT_ESTIMATE_FUTURE_SIZES) {
    if (info->enforce_const) {
      /* For each identifier bound to a procedure, register an initial
         size estimate, which is used to discourage early loop unrolling
         at the expense of later inlining. */
      for (i_m = 0; i_m < cnt; i_m++) {
        e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
        if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type))  {
          int n;

          vars = SCHEME_VEC_ELS(e)[0];
          e = SCHEME_VEC_ELS(e)[1];

          n = scheme_list_length(vars);
          if (n == 1) {
            if (IS_COMPILED_PROC(e)) {
              Scheme_Toplevel *tl;

              tl = (Scheme_Toplevel *)SCHEME_CAR(vars);

              if (!(SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_MUTATED)) {
                int pos;
                if (!consts)
                  consts = scheme_make_hash_table(SCHEME_hash_ptr);
                pos = tl->position;
                scheme_hash_set(consts,
                                scheme_make_integer(pos),
                                estimate_closure_size(e));
              }
            }
          }
        }
      }

      if (consts) {
        info->top_level_consts = consts;
        consts = NULL;
      }
    }
  }

  for (i_m = 0; i_m < cnt; i_m++) {
    /* Optimize this expression: */
    e = SCHEME_VEC_ELS(m->bodies[0])[i_m];

    is_proc_def = 0;
    if (OPT_DISCOURAGE_EARLY_INLINE && info->enforce_const) {
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
        Scheme_Object *e2;
        e2 = SCHEME_VEC_ELS(e)[1];
        if (is_general_compiled_proc(e2))
          is_proc_def = 1;
      }
    }

    if (is_proc_def && OPT_DISCOURAGE_EARLY_INLINE) {
      info->use_psize = 1;
      inline_fuel = info->inline_fuel;
      if (inline_fuel > 2)
        info->inline_fuel = 2;
    } else
      inline_fuel = 0;
    e = scheme_optimize_expr(e, info, 0);
    if (is_proc_def && OPT_DISCOURAGE_EARLY_INLINE) {
      info->use_psize = 0;
      info->inline_fuel = inline_fuel;
    }
    SCHEME_VEC_ELS(m->bodies[0])[i_m] = e;

    if (info->enforce_const) {
      /* If this expression/definition can't have any side effect
	 (including raising an exception), then continue the group of
	 simultaneous definitions: */
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type))  {
	int n, cnst = 0, sproc = 0;

	vars = SCHEME_VEC_ELS(e)[0];
	e = SCHEME_VEC_ELS(e)[1];

	n = scheme_list_length(vars);
	cont = scheme_omittable_expr(e, n, -1, 0, info, -1, 0);

        if (n == 1) {
          if (scheme_compiled_propagate_ok(e, info))
            cnst = 1;
          else if (scheme_is_statically_proc(e, info)) {
            cnst = 1;
            sproc = 1;
          }
        }

	if (cnst) {
	  Scheme_Toplevel *tl;

	  tl = (Scheme_Toplevel *)SCHEME_CAR(vars);

	  if (!(SCHEME_TOPLEVEL_FLAGS(tl) & SCHEME_TOPLEVEL_MUTATED)) {
	    Scheme_Object *e2;

            if (sproc) {
              e2 = scheme_make_noninline_proc(e);
            } else if (IS_COMPILED_PROC(e)) {
	      e2 = optimize_clone(1, e, info, 0, 0);
              if (e2) {
                Scheme_Object *pr;
                pr = scheme_make_raw_pair(scheme_make_raw_pair(e2, e), NULL);
                if (cl_last)
                  SCHEME_CDR(cl_last) = pr;
                else
                  cl_first = pr;
                cl_last = pr;
              } else
                e2 = scheme_make_noninline_proc(e);
	    } else {
	      e2 = e;
	    }

	    if (e2) {
	      int pos;
	      if (!consts)
		consts = scheme_make_hash_table(SCHEME_hash_ptr);
	      pos = tl->position;
	      scheme_hash_set(consts, scheme_make_integer(pos), e2);
              if (!re_consts)
                re_consts = scheme_make_hash_table(SCHEME_hash_ptr);
              scheme_hash_set(re_consts, scheme_make_integer(i_m),
                              scheme_make_integer(pos));
	    } else {
	      /* At least mark it as fixed */
              
	      if (!fixed_table) {
		fixed_table = scheme_make_hash_table(SCHEME_hash_ptr);
		if (!consts)
		  consts = scheme_make_hash_table(SCHEME_hash_ptr);
		scheme_hash_set(consts, scheme_false, (Scheme_Object *)fixed_table);
	      }
	      scheme_hash_set(fixed_table, scheme_make_integer(tl->position), scheme_true);
	    }
	  }
	} else {
	  /* The binding is not inlinable/propagatable, but unless it's
	     set!ed, it is constant after evaluating the definition. We
	     map the top-level position to indicate constantness. */
	  Scheme_Object *l, *a;
	  int pos;

	  for (l = vars; !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	    a = SCHEME_CAR(l);

	    /* Test for set!: */
	    if (!(SCHEME_TOPLEVEL_FLAGS(a) & SCHEME_TOPLEVEL_MUTATED)) {
	      pos = SCHEME_TOPLEVEL_POS(a);

              next_pos_ready = pos;
	    }
	  }
	}
      } else {
	cont = scheme_omittable_expr(e, -1, -1, 0, NULL, -1, 0);
      }
      if (i_m + 1 == cnt)
	cont = 0;
    } else
      cont = 1;

    if (!cont) {
      Scheme_Object *prop_later = NULL;
      /* If we have new constants, re-optimize to inline: */
      if (consts) {
        int flags;

	if (!info->top_level_consts) {
	  info->top_level_consts = consts;
	} else {
	  int i;
	  for (i = 0; i < consts->size; i++) {
	    if (consts->vals[i]) {
	      scheme_hash_set(info->top_level_consts,
			      consts->keys[i],
			      consts->vals[i]);
	    }
	  }
	}

        /* Same as in letrec: assume CLOS_SINGLE_RESULT and
           CLOS_PRESERVES_MARKS for all, but then assume not for all
           if any turn out not (i.e., approximate fix point). */
        (void)set_code_closure_flags(cl_first,
                                     CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE,
                                     0xFFFF,
                                     0);

	while (1) {
	  /* Re-optimize this expression. We can optimize anything without
             shift-cloning, since there are no local variables in scope. */
          int old_sz, new_sz;

          e = SCHEME_VEC_ELS(m->bodies[0])[start_simltaneous];

          if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE) {
            if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
              Scheme_Object *sub_e;
              sub_e = SCHEME_VEC_ELS(e)[1];
              old_sz = compiled_proc_body_size(sub_e, 0);
            } else
              old_sz = 0;
          } else
            old_sz = 0;

          e = scheme_optimize_expr(e, info, 0);
	  SCHEME_VEC_ELS(m->bodies[0])[start_simltaneous] = e;

          if (re_consts) {
            /* Install optimized closures into constant table ---
               unless, maybe, they grow too much: */
            Scheme_Object *rpos;
            rpos = scheme_hash_get(re_consts, scheme_make_integer(start_simltaneous));
            if (rpos) {
              Scheme_Object *old_e;

              e = SCHEME_VEC_ELS(e)[1];

              old_e = scheme_hash_get(info->top_level_consts, rpos);
              if (old_e && IS_COMPILED_PROC(old_e)) {
                if (!originals)
                  originals = scheme_make_hash_table(SCHEME_hash_ptr);
                scheme_hash_set(originals, scheme_make_integer(start_simltaneous), old_e);
              }

              if (!scheme_compiled_propagate_ok(e, info)
                  && scheme_is_statically_proc(e, info)) {
                /* If we previously installed a procedure for inlining,
                   don't replace that with a worse approximation. */
                if (IS_COMPILED_PROC(old_e))
                  e = NULL;
                else
                  e = scheme_make_noninline_proc(e);
              }

              if (e) {
                if (OPT_DELAY_GROUP_PROPAGATE || OPT_LIMIT_FUNCTION_RESIZE)
                  new_sz = compiled_proc_body_size(e, 0);
                else
                  new_sz = 0;

                if (!old_sz
                    || (new_sz <= old_sz)
                    || (!OPT_DELAY_GROUP_PROPAGATE && !OPT_LIMIT_FUNCTION_RESIZE))
                  scheme_hash_set(info->top_level_consts, rpos, e);
                else if (!OPT_LIMIT_FUNCTION_RESIZE
                         || (new_sz < 4 * old_sz))
                  prop_later = scheme_make_raw_pair(scheme_make_pair(rpos, e), prop_later);
              }
            }
          }

	  if (start_simltaneous == i_m)
	    break;
          start_simltaneous++;
	}

        flags = set_code_closure_flags(cl_first, 0, 0xFFFF, 0);
        (void)set_code_closure_flags(cl_first,
                                     (flags & (CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS)),
                                     ~(CLOS_SINGLE_RESULT | CLOS_PRESERVES_MARKS | CLOS_RESULT_TENTATIVE),
                                     1);
      }

      cl_last = cl_first = NULL;
      consts = NULL;
      re_consts = NULL;
      start_simltaneous = i_m + 1;

      while (prop_later) {
        e = SCHEME_CAR(prop_later);
        scheme_hash_set(info->top_level_consts, SCHEME_CAR(e), SCHEME_CDR(e));
        prop_later = SCHEME_CDR(prop_later);
      }
    }

    if (next_pos_ready > -1) {
      if (!fixed_table) {
        fixed_table = scheme_make_hash_table(SCHEME_hash_ptr);
        if (!consts)
          consts = scheme_make_hash_table(SCHEME_hash_ptr);
        scheme_hash_set(consts, scheme_false, (Scheme_Object *)fixed_table);
      }
      scheme_hash_set(fixed_table, scheme_make_integer(next_pos_ready), scheme_true);
      next_pos_ready = -1;
    }
  }

  /* For functions that are potentially inlineable, perhaps 
     before optimization, insert inline_variant records: */
  if (info->enforce_const) {
    for (i_m = 0; i_m < cnt; i_m++) {
      /* Optimize this expression: */
      e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
      if (SAME_TYPE(SCHEME_TYPE(e), scheme_define_values_type)) {
        int size_override;
        size_override = SCHEME_IMMUTABLEP(e);
        vars = SCHEME_VEC_ELS(e)[0];
        if (SCHEME_PAIRP(vars) && SCHEME_NULLP(SCHEME_CDR(vars))) {
          Scheme_Object *sub_e, *alt_e;
          sub_e = SCHEME_VEC_ELS(e)[1];
          alt_e = is_cross_module_inline_candidiate(sub_e, info, 0);
          if (!alt_e && originals) {
            alt_e = scheme_hash_get(originals, scheme_make_integer(i_m));
            if (SAME_OBJ(alt_e, sub_e) && !size_override)
              alt_e = NULL;
            else if (alt_e)
              alt_e = is_cross_module_inline_candidiate(alt_e, info, size_override);
          }
          if (alt_e) {
            Scheme_Object *iv;
            iv = scheme_make_vector(3, scheme_false);
            iv->type = scheme_inline_variant_type;
            SCHEME_VEC_ELS(iv)[0] = sub_e;
            SCHEME_VEC_ELS(iv)[1] = alt_e;
            SCHEME_VEC_ELS(e)[1] = iv;
          }
        }
      }
    }
  }

  /* Check one more time for expressions that we can omit: */
  {
    int can_omit = 0;
    for (i_m = 0; i_m < cnt; i_m++) {
      /* Optimize this expression: */
      e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
      if (scheme_omittable_expr(e, -1, -1, 0, NULL, -1, 0)) {
        can_omit++;
      }
    }
    if (can_omit) {
      Scheme_Object *vec;
      int j = 0;
      vec = scheme_make_vector(cnt - can_omit, NULL);
      for (i_m = 0; i_m < cnt; i_m++) {
        /* Optimize this expression: */
        e = SCHEME_VEC_ELS(m->bodies[0])[i_m];
        if (!scheme_omittable_expr(e, -1, -1, 0, NULL, -1, 0)) {
          SCHEME_VEC_ELS(vec)[j++] = e;
        }
      }
      m->bodies[0] = vec;
    }
    cnt -= can_omit;
  }

  info->context = old_context;
  info->cp = prev_cp;

  /* Exp-time body was optimized during compilation */

  {
    /* optimize submodules */
    int k;
    Scheme_Object *p;
    for (k = 0; k < 2; k++) {
      p = (k ? m->post_submodules : m->pre_submodules);
      if (p) {
        while (!SCHEME_NULLP(p)) {
          scheme_optimize_expr(SCHEME_CAR(p), info, 0);
          p = SCHEME_CDR(p);
        }
      }
    }
  }

  return data;
}

static Scheme_Object *
top_level_require_optimize(Scheme_Object *data, Optimize_Info *info, int context)
{
  return data;
}



/*========================================================================*/
/*                            expressions                                 */
/*========================================================================*/

static Scheme_Object *optimize_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *expr = (Scheme_Object *)p->ku.k.p1;
  Optimize_Info *info = (Optimize_Info *)p->ku.k.p2;
  int context = p->ku.k.i1;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return scheme_optimize_expr(expr, info, context);
}

Scheme_Object *scheme_optimize_expr(Scheme_Object *expr, Optimize_Info *info, int context)
{
  Scheme_Type type = SCHEME_TYPE(expr);

#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)expr;
    p->ku.k.p2 = (void *)info;
    p->ku.k.i1 = context;

    return scheme_handle_stack_overflow(optimize_k);
  }
#endif

  info->preserves_marks = 1;
  info->single_result = 1;

  switch (type) {
  case scheme_local_type:
    {
      Scheme_Object *val;
      int pos, delta, is_mutated = 0;

      info->size += 1;

      pos = SCHEME_LOCAL_POS(expr);

      val = optimize_info_lookup(info, pos, NULL, NULL,
                                 (context & OPT_CONTEXT_NO_SINGLE) ? 0 : 1,
                                 context, NULL, &is_mutated);

      if (val) {
        if (SAME_TYPE(SCHEME_TYPE(val), scheme_once_used_type)) {
          Scheme_Once_Used *o = (Scheme_Once_Used *)val;
          if (((o->vclock == info->vclock)
               && single_valued_noncm_expression(o->expr, 5))
              || ((o->vclock != info->vclock)
                  && movable_expression(o->expr, info, o->delta, o->cross_lambda, 0, 5))) {
            val = optimize_clone(1, o->expr, info, o->delta, 0);
            if (val) {
              info->size -= 1;
              o->used = 1;
              return scheme_optimize_expr(val, info, context);
            }
          }
          /* Can't move expression, so lookup again to mark as used
             and to perform any copy propagation that might apply. */
          val = optimize_info_lookup(info, pos, NULL, NULL, 0, context, NULL, NULL);
          if (val)
            return val;
        } else {
          if (SAME_TYPE(SCHEME_TYPE(val), scheme_compiled_toplevel_type)) {
            info->size -= 1;
            return scheme_optimize_expr(val, info, context);
          }
          return val;
        }
      } else if (is_mutated) {
        info->vclock += 1;
      }

      delta = optimize_info_get_shift(info, pos);
      if (delta)
	expr = scheme_make_local(scheme_local_type, pos + delta, 0);

      return expr;
    }
  case scheme_application_type:
    return optimize_application(expr, info, context);
  case scheme_application2_type:
    return optimize_application2(expr, info, context);
  case scheme_application3_type:
    return optimize_application3(expr, info, context);
  case scheme_sequence_type:
  case scheme_splice_sequence_type:
    return optimize_sequence(expr, info, context);
  case scheme_branch_type:
    return optimize_branch(expr, info, context);
  case scheme_with_cont_mark_type:
    return optimize_wcm(expr, info, context);
  case scheme_compiled_unclosed_procedure_type:
    return optimize_closure_compilation(expr, info, context);
  case scheme_compiled_let_void_type:
    return scheme_optimize_lets(expr, info, 0, context);
  case scheme_compiled_toplevel_type:
    info->size += 1;
    if (info->top_level_consts) {
      int pos;
      Scheme_Object *c;

      while (1) {
        pos = SCHEME_TOPLEVEL_POS(expr);
        c = scheme_hash_get(info->top_level_consts, scheme_make_integer(pos));
        c = no_potential_size(c);
        if (c && SAME_TYPE(SCHEME_TYPE(c), scheme_compiled_toplevel_type))
          expr = c;
        else
          break;
      }

      if (c) {
	if (scheme_compiled_duplicate_ok(c, 0))
	  return c;

	/* We can't inline, but mark the top level as a constant,
	   so we can direct-jump and avoid null checks in JITed code: */
	expr = scheme_toplevel_to_flagged_toplevel(expr, SCHEME_TOPLEVEL_CONST);
      } else {
	/* false is mapped to a table of non-constant ready values: */
	c = scheme_hash_get(info->top_level_consts, scheme_false);
	if (c) {
	  c = scheme_hash_get((Scheme_Hash_Table *)c, scheme_make_integer(pos));

	  if (c) {
	    /* We can't inline, but mark the top level as ready and fixed,
	       so we can avoid null checks in JITed code, etc: */
	    expr = scheme_toplevel_to_flagged_toplevel(expr, SCHEME_TOPLEVEL_FIXED);
	  }
	}
        if (!c)
          info->vclock += 1;
      }
    } else {
      info->vclock += 1;
    }
    optimize_info_used_top(info);
    return expr;
  case scheme_compiled_quote_syntax_type:
    info->size += 1;
    optimize_info_used_top(info);
    return expr;
  case scheme_variable_type:
  case scheme_module_variable_type:
    scheme_signal_error("got top-level in wrong place");
    return 0;
  case scheme_define_values_type:
    return define_values_optimize(expr, info, context);
  case scheme_varref_form_type:
    return ref_optimize(expr, info, context);
  case scheme_set_bang_type:
    return set_optimize(expr, info, context);
  case scheme_define_syntaxes_type:
    return define_syntaxes_optimize(expr, info, context);
  case scheme_begin_for_syntax_type:
    return begin_for_syntax_optimize(expr, info, context);
  case scheme_case_lambda_sequence_type:
    return case_lambda_optimize(expr, info, context);
  case scheme_begin0_sequence_type:
    return begin0_optimize(expr, info, context);
  case scheme_apply_values_type:
    return apply_values_optimize(expr, info, context);
  case scheme_require_form_type:
    return top_level_require_optimize(expr, info, context);
  case scheme_module_type:
    return module_optimize(expr, info, context);
  default:
    info->size += 1;
    return expr;
  }
}

Scheme_Object *optimize_clone(int dup_ok, Scheme_Object *expr, Optimize_Info *info, int delta, int closure_depth)
/* Past closure_depth, need to reverse optimize to unoptimized with respect to info;
   delta is the amount to skip in info to get to the frame that bound the code.
   If dup_ok is 1, then the old copy will be dropped, so it's ok to "duplicate"
   any constant. */
{
  int t;

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_local_type:
    {
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos >= closure_depth) {
	expr = optimize_reverse(info, pos + delta - closure_depth, 0, !dup_ok);
	if (closure_depth)
	  expr = scheme_make_local(scheme_local_type, SCHEME_LOCAL_POS(expr) + closure_depth, 0);
      }
      return expr;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr, *app2;

      app2 = MALLOC_ONE_TAGGED(Scheme_App2_Rec);
      app2->iso.so.type = scheme_application2_type;

      expr = optimize_clone(dup_ok, app->rator, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rator = expr;

      expr = optimize_clone(dup_ok, app->rand, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand = expr;

      return (Scheme_Object *)app2;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr, *app2;
      int i;

      app2 = scheme_malloc_application(app->num_args + 1);

      for (i = app->num_args + 1; i--; ) {
	expr = optimize_clone(dup_ok, app->args[i], info, delta, closure_depth);
	if (!expr) return NULL;
	app2->args[i] = expr;
      }

      return (Scheme_Object *)app2;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr, *app2;

      app2 = MALLOC_ONE_TAGGED(Scheme_App3_Rec);
      app2->iso.so.type = scheme_application3_type;

      expr = optimize_clone(dup_ok, app->rator, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rator = expr;

      expr = optimize_clone(dup_ok, app->rand1, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand1 = expr;

      expr = optimize_clone(dup_ok, app->rand2, info, delta, closure_depth);
      if (!expr) return NULL;
      app2->rand2 = expr;

      return (Scheme_Object *)app2;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr, *head2;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv, *lv2, *prev = NULL;
      int i, *flags, sz;
      int post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

      head2 = MALLOC_ONE_TAGGED(Scheme_Let_Header);
      head2->iso.so.type = scheme_compiled_let_void_type;
      head2->count = head->count;
      head2->num_clauses = head->num_clauses;
      SCHEME_LET_FLAGS(head2) = SCHEME_LET_FLAGS(head);

      /* Build let-value change: */
      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;

	sz = sizeof(int) * lv->count;
	flags = (int *)scheme_malloc_atomic(sz);
	memcpy(flags, lv->flags, sz);

	lv2 = MALLOC_ONE_TAGGED(Scheme_Compiled_Let_Value);
        SCHEME_CLV_FLAGS(lv2) |= (SCHEME_CLV_FLAGS(lv) & 0x1);
	lv2->iso.so.type = scheme_compiled_let_value_type;
	lv2->count = lv->count;
	lv2->position = lv->position;
	lv2->flags = flags;

	expr = optimize_clone(dup_ok, lv->value, info, delta,
                                     closure_depth + (post_bind ? 0 : head->count));
	if (!expr) return NULL;
	lv2->value = expr;

	if (prev)
	  prev->body = (Scheme_Object *)lv2;
	else
	  head2->body = (Scheme_Object *)lv2;
	prev = lv2;

	body = lv->body;
      }
      if (prev)
	prev->body = body;
      else
	head2->body = body;

      expr = optimize_clone(dup_ok, body, info, delta, closure_depth + head->count);
      if (!expr) return NULL;

      if (prev)
	prev->body = expr;
      else
	head2->body = expr;

      return (Scheme_Object *)head2;
    }
  case scheme_sequence_type:
  case scheme_begin0_sequence_type:
  case scheme_splice_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr, *seq2;
      int i;

      seq2 = scheme_malloc_sequence(seq->count);
      seq2->so.type = seq->so.type;
      seq2->count = seq->count;

      for (i = seq->count; i--; ) {
	expr = optimize_clone(dup_ok, seq->array[i], info, delta, closure_depth);
	if (!expr) return NULL;
	seq2->array[i] = expr;
      }

      return (Scheme_Object *)seq2;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr, *b2;

      b2 = MALLOC_ONE_TAGGED(Scheme_Branch_Rec);
      b2->so.type = scheme_branch_type;

      expr = optimize_clone(dup_ok, b->test, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->test = expr;

      expr = optimize_clone(dup_ok, b->tbranch, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->tbranch = expr;

      expr = optimize_clone(dup_ok, b->fbranch, info, delta, closure_depth);
      if (!expr) return NULL;
      b2->fbranch = expr;

      return (Scheme_Object *)b2;
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr, *wcm2;

      wcm2 = MALLOC_ONE_TAGGED(Scheme_With_Continuation_Mark);
      wcm2->so.type = scheme_with_cont_mark_type;

      expr = optimize_clone(dup_ok, wcm->key, info, delta, closure_depth);
      if (!expr) return NULL;
      wcm2->key = expr;

      expr = optimize_clone(dup_ok, wcm->val, info, delta, closure_depth);
      if (!expr) return NULL;
      wcm2->val = expr;

      expr = optimize_clone(dup_ok, wcm->body, info, delta, closure_depth);
      if (!expr) return NULL;
      wcm2->body = expr;

      return (Scheme_Object *)wcm2;
    }
  case scheme_compiled_unclosed_procedure_type:
    return clone_closure_compilation(dup_ok, expr, info, delta, closure_depth);
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    return expr;
  case scheme_define_values_type:
  case scheme_define_syntaxes_type:
  case scheme_begin_for_syntax_type:
  case scheme_boxenv_type:
    return NULL;
  case scheme_require_form_type:
    return NULL;
  case scheme_varref_form_type:
    return ref_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_set_bang_type:
    return set_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_apply_values_type:
    return apply_values_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_case_lambda_sequence_type:
    return case_lambda_clone(dup_ok, expr, info, delta, closure_depth);
  case scheme_module_type:
    return NULL;
  default:
    if (t > _scheme_compiled_values_types_) {
      if (dup_ok || scheme_compiled_duplicate_ok(expr, 0))
	return expr;
    }
  }

  return NULL;
}

Scheme_Object *optimize_shift(Scheme_Object *expr, int delta, int after_depth)
/* Shift lexical addresses deeper by delta if already deeper than after_depth;
   can mutate. */
{
  int t;

  /* FIXME: need stack check */

  t = SCHEME_TYPE(expr);

  switch(t) {
  case scheme_local_type:
  case scheme_local_unbox_type:
    {
      int pos = SCHEME_LOCAL_POS(expr);
      if (pos >= after_depth) {
        expr = scheme_make_local(t, SCHEME_LOCAL_POS(expr) + delta, 0);
      }
      return expr;
    }
  case scheme_application_type:
    {
      Scheme_App_Rec *app = (Scheme_App_Rec *)expr;
      int i;

      for (i = app->num_args + 1; i--; ) {
	expr = optimize_shift(app->args[i], delta, after_depth);
	app->args[i] = expr;
      }

      return (Scheme_Object *)app;
    }
  case scheme_application2_type:
    {
      Scheme_App2_Rec *app = (Scheme_App2_Rec *)expr;

      expr = optimize_shift(app->rator, delta, after_depth);
      app->rator = expr;

      expr = optimize_shift(app->rand, delta, after_depth);
      app->rand = expr;

      return (Scheme_Object *)app;
    }
  case scheme_application3_type:
    {
      Scheme_App3_Rec *app = (Scheme_App3_Rec *)expr;

      expr = optimize_shift(app->rator, delta, after_depth);
      app->rator = expr;

      expr = optimize_shift(app->rand1, delta, after_depth);
      app->rand1 = expr;

      expr = optimize_shift(app->rand2, delta, after_depth);
      app->rand2 = expr;

      return (Scheme_Object *)app;
    }
  case scheme_compiled_let_void_type:
    {
      Scheme_Let_Header *head = (Scheme_Let_Header *)expr;
      Scheme_Object *body;
      Scheme_Compiled_Let_Value *lv = NULL;
      int i;
      int post_bind = !(SCHEME_LET_FLAGS(head) & (SCHEME_LET_RECURSIVE | SCHEME_LET_STAR));

      /* Build let-value change: */
      body = head->body;
      for (i = head->num_clauses; i--; ) {
	lv = (Scheme_Compiled_Let_Value *)body;

	expr = optimize_shift(lv->value, delta, after_depth + (post_bind ? 0 : head->count));
	lv->value = expr;

        body = lv->body;
      }
      expr = optimize_shift(body, delta, after_depth + head->count);

      if (head->num_clauses)
	lv->body = expr;
      else
	head->body = expr;

      return (Scheme_Object *)head;
    }
  case scheme_sequence_type:
  case scheme_splice_sequence_type:
  case scheme_begin0_sequence_type:
    {
      Scheme_Sequence *seq = (Scheme_Sequence *)expr;
      int i;

      for (i = seq->count; i--; ) {
	expr = optimize_shift(seq->array[i], delta, after_depth);
	seq->array[i] = expr;
      }

      return (Scheme_Object *)seq;
    }
  case scheme_branch_type:
    {
      Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)expr;

      expr = optimize_shift(b->test, delta, after_depth);
      b->test = expr;

      expr = optimize_shift(b->tbranch, delta, after_depth);
      b->tbranch = expr;

      expr = optimize_shift(b->fbranch, delta, after_depth);
      b->fbranch = expr;

      return (Scheme_Object *)b;
    }
  case scheme_with_cont_mark_type:
    {
      Scheme_With_Continuation_Mark *wcm = (Scheme_With_Continuation_Mark *)expr;

      expr = optimize_shift(wcm->key, delta, after_depth);
      wcm->key = expr;

      expr = optimize_shift(wcm->val, delta, after_depth);
      wcm->val = expr;

      expr = optimize_shift(wcm->body, delta, after_depth);
      wcm->body = expr;

      return (Scheme_Object *)wcm;
    }
  case scheme_compiled_unclosed_procedure_type:
    return shift_closure_compilation(expr, delta, after_depth);
  case scheme_compiled_toplevel_type:
  case scheme_compiled_quote_syntax_type:
    return expr;
  case scheme_set_bang_type:
    return set_shift(expr, delta, after_depth);
  case scheme_varref_form_type:
    return ref_shift(expr, delta, after_depth);
  case scheme_apply_values_type:
    return apply_values_shift(expr, delta, after_depth);
  case scheme_case_lambda_sequence_type:
    return case_lambda_shift(expr, delta, after_depth);
  case scheme_boxenv_type:
  case scheme_define_values_type:
  case scheme_define_syntaxes_type:
  case scheme_begin_for_syntax_type:
  case scheme_require_form_type:
  case scheme_module_type:
    scheme_signal_error("optimize_shift: no shift available for %d", SCHEME_TYPE(expr));
    return NULL;
  default:
    return expr;
  }

  return NULL;
}

/*========================================================================*/
/*                 compile-time env for optimization                      */
/*========================================================================*/

Optimize_Info *scheme_optimize_info_create(Comp_Prefix *cp)
{
  Optimize_Info *info;

  info = MALLOC_ONE_RT(Optimize_Info);
#ifdef MZTAG_REQUIRED
  info->type = scheme_rt_optimize_info;
#endif
  info->inline_fuel = 32;
  info->cp = cp;

  return info;
}

void scheme_optimize_info_enforce_const(Optimize_Info *oi, int enforce_const)
{
  oi->enforce_const = enforce_const;
}

void scheme_optimize_info_set_context(Optimize_Info *oi, Scheme_Object *ctx)
{
  oi->context = ctx;
}

void scheme_optimize_info_never_inline(Optimize_Info *oi)
{
  oi->inline_fuel = -1;
}

static void register_transitive_use(Optimize_Info *info, int pos, int j);

static void register_stat_dist(Optimize_Info *info, int i, int j)
{
  if (!info->stat_dists) {
    int k, *ia;
    char **ca;
    ca = MALLOC_N(char*, info->new_frame);
    info->stat_dists = ca;
    ia = MALLOC_N_ATOMIC(int, info->new_frame);
    info->sd_depths = ia;
    for (k = info->new_frame; k--; ) {
      info->sd_depths[k] = 0;
    }
  }

  if (i >= info->new_frame)
    scheme_signal_error("internal error: bad stat-dist index");

  if (info->sd_depths[i] <= j) {
    char *naya, *a;
    int k;

    naya = MALLOC_N_ATOMIC(char, (j + 1));
    for (k = j + 1; k--; ) {
      naya[k] = 0;
    }
    a = info->stat_dists[i];
    for (k = info->sd_depths[i]; k--; ) {
      naya[k] = a[k];
    }

    info->stat_dists[i] = naya;
    info->sd_depths[i] = j + 1;
  }

  if (info->transitive_use && info->transitive_use[i]) {
    /* We're using a procedure that we weren't sure would be used.
       Transitively mark everything that the procedure uses --- unless
       a transitive accumulation is in effect, in which case we
       don't follow this one now, leaving it to be triggered when
       the one we're accumulating is triggered. */
    if (!info->transitive_use_pos) {
      mzshort *map = info->transitive_use[i];
      int len = info->transitive_use_len[i];
      int k;

      info->transitive_use[i] = NULL;

      for (k = 0; k < len; k++) {
        register_transitive_use(info, map[k], 0);
      }
    }
  }

  info->stat_dists[i][j] = 1;
}

static Scheme_Object *transitive_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Optimize_Info *info = (Optimize_Info *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  register_transitive_use(info, (int)p->ku.k.i1, (int)p->ku.k.i2);

  return scheme_false;
}

static void register_transitive_use(Optimize_Info *info, int pos, int j)
{
#ifdef DO_STACK_CHECK
# include "mzstkchk.h"
  {
    Scheme_Thread *p = scheme_current_thread;

    p->ku.k.p1 = (void *)info;
    p->ku.k.i1 = pos;
    p->ku.k.i2 = j;

    scheme_handle_stack_overflow(transitive_k);

    return;
  }
#endif

  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME)
      j++;
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    info = info->next;
  }

  if (info->sd_depths[pos] <= j) {
    scheme_signal_error("bad transitive position depth: %d vs. %d",
                        info->sd_depths[pos], j);
  }

  register_stat_dist(info, pos, j);
}

static void env_make_closure_map(Optimize_Info *info, mzshort *_size, mzshort **_map)
{
  /* A closure map lists the captured variables for a closure; the
     indices are resolved two new indices in the second phase of
     compilation. */
  Optimize_Info *frame;
  int i, j, pos = 0, lpos = 0, tu;
  mzshort *map, size;

  /* Count vars used by this closure (skip args): */
  j = 1;
  for (frame = info->next; frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (frame->stat_dists) {
      for (i = 0; i < frame->new_frame; i++) {
	if (frame->sd_depths[i] > j) {
	  if (frame->stat_dists[i][j]) {
	    pos++;
	  }
	}
      }
    }
  }

  size = pos;
  *_size = size;
  map = MALLOC_N_ATOMIC(mzshort, size);
  *_map = map;

  if (info->next && info->next->transitive_use_pos) {
    info->next->transitive_use[info->next->transitive_use_pos - 1] = map;
    info->next->transitive_use_len[info->next->transitive_use_pos - 1] = size;
    tu = 1;
  } else
    tu = 0;

  /* Build map, unmarking locals and marking deeper in parent frame */
  j = 1; pos = 0;
  for (frame = info->next; frame; frame = frame->next) {
    if (frame->flags & SCHEME_LAMBDA_FRAME)
      j++;

    if (frame->stat_dists) {
      for (i = 0; i < frame->new_frame; i++) {
	if (frame->sd_depths[i] > j) {
	  if (frame->stat_dists[i][j]) {
	    map[pos++] = lpos;
	    frame->stat_dists[i][j] = 0; /* This closure's done with these vars... */
            if (!tu)
              frame->stat_dists[i][j - 1] = 1; /* ... but ensure previous keeps */
	  }
	}
	lpos++;
      }
    } else
      lpos += frame->new_frame;
  }
}

static int env_uses_toplevel(Optimize_Info *frame)
{
  int used;

  used = frame->used_toplevel;

  if (used) {
    /* Propagate use to an enclosing lambda, if any: */
    frame = frame->next;
    while (frame) {
      if (frame->flags & SCHEME_LAMBDA_FRAME) {
	frame->used_toplevel = 1;
	break;
      }
      frame = frame->next;
    }
  }

  return used;
}

static void optimize_info_used_top(Optimize_Info *info)
{
  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME) {
      info->used_toplevel = 1;
      break;
    }
    info = info->next;
  }
}

static void optimize_propagate(Optimize_Info *info, int pos, Scheme_Object *value, int single_use)
{
  /* A raw-pair `value' is an indicator for whether a letrec-bound
     variable is ready. */
  Scheme_Object *p;

  p = scheme_make_vector(4, NULL);
  SCHEME_VEC_ELS(p)[0] = info->consts;
  SCHEME_VEC_ELS(p)[1] = scheme_make_integer(pos);
  SCHEME_VEC_ELS(p)[2] = value;
  SCHEME_VEC_ELS(p)[3] = (single_use ? scheme_true : scheme_false);

  info->consts = p;
}

static Scheme_Once_Used *make_once_used(Scheme_Object *val, int pos, int vclock, Scheme_Once_Used *prev)
{
  Scheme_Once_Used *o;

  o = MALLOC_ONE_TAGGED(Scheme_Once_Used);
  o->so.type = scheme_once_used_type;

  o->expr = val;
  o->pos = pos;
  o->vclock = vclock;

  if (prev)
    prev->next = o;

  return o;
}

static void register_use(Optimize_Info *info, int pos, int flag)
/* pos must be in immediate frame */
{
  if (!info->use) {
    char *use;
    use = (char *)scheme_malloc_atomic(info->new_frame);
    memset(use, 0, info->new_frame);
    info->use = use;
  }
  info->use[pos] |= flag;
}

static void optimize_mutated(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  register_use(info, pos, 0x1);
}

static void optimize_produces_flonum(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  register_use(info, pos, 0x4);
}

static Scheme_Object *optimize_reverse(Optimize_Info *info, int pos, int unless_mutated, int disrupt_single_use)
/* pos is in new-frame counts, and we want to produce an old-frame reference if
   it's not mutated */
{
  int delta = 0;

  while (1) {
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    delta += info->original_frame;
    info = info->next;
  }

  if (unless_mutated)
    if (info->use && (info->use[pos] & 0x1))
      return NULL;

  if (disrupt_single_use) {
    Scheme_Object *p, *n;
    p = info->consts;
    while (p) {
      n = SCHEME_VEC_ELS(p)[1];
      if (SCHEME_INT_VAL(n) == pos) {
        if (SCHEME_TRUEP(SCHEME_VEC_ELS(p)[3])) {
          SCHEME_VEC_ELS(p)[3] = scheme_false; /* disable "single use" mark */
        }
        n = SCHEME_VEC_ELS(p)[2];
        if (SAME_TYPE(SCHEME_TYPE(n), scheme_once_used_type)) {
          ((Scheme_Once_Used *)n)->expr = NULL;
          ((Scheme_Once_Used *)n)->vclock = -1;
        }
        break;
      }
      p = SCHEME_VEC_ELS(p)[0];
    }
  }

  return scheme_make_local(scheme_local_type, pos + delta, 0);
}

static int optimize_is_used(Optimize_Info *info, int pos)
/* pos must be in immediate frame */
{
  int i;

  if (info->stat_dists) {
    for (i = info->sd_depths[pos]; i--; ) {
      if (info->stat_dists[pos][i])
	return 1;
    }
  }

  return 0;
}

static int check_use(Optimize_Info *info, int pos, int flag)
/* pos is in new-frame counts */
{
  while (1) {
    if (pos < info->new_frame)
      break;
    pos -= info->new_frame;
    info = info->next;
  }

  if (info->use && (info->use[pos] & flag))
    return 1;

  return 0;
}

static int optimize_is_mutated(Optimize_Info *info, int pos)
/* pos is in new-frame counts */
{
  return check_use(info, pos, 0x1);
}

static int optimize_is_flonum_arg(Optimize_Info *info, int pos, int depth)
/* pos is in new-frame counts */
{
  return check_use(info, pos, 0x2);
}

static int optimize_is_flonum_valued(Optimize_Info *info, int pos)
/* pos is in new-frame counts */
{
  return check_use(info, pos, 0x4);
}

static int optimize_any_uses(Optimize_Info *info, int start_pos, int end_pos)
{
  int j, i;

  if (info->stat_dists) {
    for (i = start_pos; i < end_pos; i++) {
      for (j = info->sd_depths[i]; j--; ) {
        if (info->stat_dists[i][j])
          return 1;
      }
    }
  }

  if (info->transitive_use) {
    for (i = info->new_frame; i--; ) {
      if (info->transitive_use[i]) {
        for (j = info->transitive_use_len[i]; j--; ) {
          if ((info->transitive_use[i][j] >= start_pos)
              && (info->transitive_use[i][j] < end_pos))
            return 1;
        }
      }
    }
  }

  return 0;
}

static Scheme_Object *do_optimize_info_lookup(Optimize_Info *info, int pos, int j, int *closure_offset, int *single_use,
                                              int *not_ready, int once_used_ok, int context, int *potential_size,
                                              int disrupt_single_use, int *is_mutated, int just_test)
{
  Scheme_Object *p, *n;
  int delta = 0, orig_j = j;

  while (info) {
    if (info->flags & SCHEME_LAMBDA_FRAME)
      j++;
    if (pos < info->original_frame)
      break;
    pos -= info->original_frame;
    delta += info->new_frame;
    info = info->next;
  }

  if (context & OPT_CONTEXT_FLONUM_ARG)
    register_use(info, pos, 0x2);

  if (is_mutated)
    if (info->use && (info->use[pos] & 0x1))
      *is_mutated = 1;

  if (just_test) return NULL;

  p = info->consts;
  while (p) {
    n = SCHEME_VEC_ELS(p)[1];
    if (SCHEME_INT_VAL(n) == pos) {
      n = SCHEME_VEC_ELS(p)[2];
      if (info->flags & SCHEME_POST_BIND_FRAME)
        delta += info->new_frame;
      if (SCHEME_RPAIRP(n)) {
        /* This was a letrec-bound identifier that may or may not be ready,
           but which wasn't replaced with more information. */
        if (not_ready)
          *not_ready = SCHEME_TRUEP(SCHEME_CAR(n));
        break;
      }
      if (SCHEME_BOXP(n)) {
        /* A potential-size record: */
        if (potential_size)
          *potential_size = (int)SCHEME_INT_VAL(SCHEME_BOX_VAL(n));
        break;
      }
      if (single_use)
        *single_use = SCHEME_TRUEP(SCHEME_VEC_ELS(p)[3]);
      if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_unclosed_procedure_type)) {
	if (!closure_offset)
	  break;
	else
          *closure_offset = delta;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_case_lambda_sequence_type)) {
        if (!closure_offset)
	  break;
	else
          *closure_offset = delta;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_compiled_toplevel_type)) {
        /* Ok */
      } else if (closure_offset) {
        /* Inlining can deal procedures and top-levels, but not other things. */
        return NULL;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_once_used_type)) {
        Scheme_Once_Used *o;

        if (disrupt_single_use) {
          ((Scheme_Once_Used *)n)->expr = NULL;
          ((Scheme_Once_Used *)n)->vclock = -1;
        }

        if (!once_used_ok)
          break;

        o = (Scheme_Once_Used *)n;
        if (!o->expr) break; /* disrupted or not available */

        o->delta = delta;
        o->info = info;
        o->cross_lambda = (j != orig_j);
        return (Scheme_Object *)o;
      } else if (SAME_TYPE(SCHEME_TYPE(n), scheme_local_type)) {
	int pos, cross_lambda = (j != orig_j);

	pos = SCHEME_LOCAL_POS(n);
	if (info->flags & SCHEME_LAMBDA_FRAME)
	  j--; /* because it will get re-added on recur */
        else if (info->flags & SCHEME_POST_BIND_FRAME)
          info = info->next; /* bindings are relative to next frame */

	/* Marks local as used; we don't expect to get back
	   a value, because chaining would normally happen on the
	   propagate-call side. Chaining there also means that we
	   avoid stack overflow here. */
        if (single_use) {
          if (!*single_use)
            single_use = NULL;
        }

        /* If the referenced variable is not single-use, then
           the variable it is replaced by is no longer single-use */
        disrupt_single_use = !SCHEME_TRUEP(SCHEME_VEC_ELS(p)[3]);

	n = do_optimize_info_lookup(info, pos, j, NULL, single_use, NULL,
                                    once_used_ok && !disrupt_single_use, context,
                                    potential_size, disrupt_single_use, NULL, 0);

	if (!n) {
	  /* Return shifted reference to other local: */
	  delta += optimize_info_get_shift(info, pos);
	  n = scheme_make_local(scheme_local_type, pos + delta, 0);
	} else if (SAME_TYPE(SCHEME_TYPE(n), scheme_once_used_type)) {
          /* Need to adjust delta: */
          delta = optimize_info_get_shift(info, pos);
          ((Scheme_Once_Used *)n)->delta += delta;
          if (cross_lambda) ((Scheme_Once_Used *)n)->cross_lambda = 1;
        }
      }
      return n;
    }
    p = SCHEME_VEC_ELS(p)[0];
  }

  if (!closure_offset)
    register_stat_dist(info, pos, j);

  return NULL;
}

static Scheme_Object *optimize_info_lookup(Optimize_Info *info, int pos, int *closure_offset, int *single_use,
                                           int once_used_ok, int context, int *potential_size, int *is_mutated)
{
  return do_optimize_info_lookup(info, pos, 0, closure_offset, single_use, NULL, once_used_ok, context,
                                 potential_size, 0, is_mutated, 0);
}

static int optimize_info_is_ready(Optimize_Info *info, int pos)
{
  int closure_offset, single_use, ready = 1;

  do_optimize_info_lookup(info, pos, 0, &closure_offset, &single_use, &ready, 0, 0, NULL, 0, NULL, 0);

  return ready;
}

static Scheme_Object *optimize_info_mutated_lookup(Optimize_Info *info, int pos, int *is_mutated)
{
  return do_optimize_info_lookup(info, pos, 0, NULL, NULL, NULL, 0, 0, NULL, 0, is_mutated, 1);
}

static Optimize_Info *optimize_info_add_frame(Optimize_Info *info, int orig, int current, int flags)
{
  Optimize_Info *naya;

  naya = scheme_optimize_info_create(info->cp);
  naya->flags = (short)flags;
  naya->next = info;
  naya->original_frame = orig;
  naya->new_frame = current;
  naya->inline_fuel = info->inline_fuel;
  naya->letrec_not_twice = info->letrec_not_twice;
  naya->enforce_const = info->enforce_const;
  naya->top_level_consts = info->top_level_consts;
  naya->context = info->context;
  naya->vclock = info->vclock;
  naya->use_psize = info->use_psize;

  return naya;
}

static int optimize_info_get_shift(Optimize_Info *info, int pos)
{
  int delta = 0;

  while (info) {
    if (pos < info->original_frame)
      break;
    pos -= info->original_frame;
    delta += (info->new_frame - info->original_frame);
    info = info->next;
  }

  if (!info)
    scheme_signal_error("error looking for local-variable offset");

  return delta;
}

static void optimize_info_done(Optimize_Info *info, Optimize_Info *parent)
{
  if (!parent) parent = info->next;

  parent->size += info->size;
  parent->vclock = info->vclock;
  parent->psize += info->psize;
  if (info->has_nonleaf)
    parent->has_nonleaf = 1;
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_optimize.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_once_used_type, mark_once_used);
  GC_REG_TRAV(scheme_rt_optimize_info, mark_optimize_info);
}

END_XFORM_SKIP;

#endif
