#include "schpriv.h"
#include "schmach.h"

/* globals */
READ_ONLY Scheme_Object *scheme_vector_proc;
READ_ONLY Scheme_Object *scheme_vector_p_proc;
READ_ONLY Scheme_Object *scheme_make_vector_proc;
READ_ONLY Scheme_Object *scheme_vector_immutable_proc;
READ_ONLY Scheme_Object *scheme_vector_length_proc;
READ_ONLY Scheme_Object *scheme_vector_star_length_proc;
READ_ONLY Scheme_Object *scheme_vector_ref_proc;
READ_ONLY Scheme_Object *scheme_vector_star_ref_proc;
READ_ONLY Scheme_Object *scheme_vector_set_proc;
READ_ONLY Scheme_Object *scheme_vector_star_set_proc;
READ_ONLY Scheme_Object *scheme_vector_cas_proc;
READ_ONLY Scheme_Object *scheme_list_to_vector_proc;
READ_ONLY Scheme_Object *scheme_unsafe_vector_length_proc;
READ_ONLY Scheme_Object *scheme_unsafe_vector_star_length_proc;
READ_ONLY Scheme_Object *scheme_unsafe_vector_star_ref_proc;
READ_ONLY Scheme_Object *scheme_unsafe_vector_star_set_proc;
READ_ONLY Scheme_Object *scheme_unsafe_string_length_proc;
READ_ONLY Scheme_Object *scheme_unsafe_string_ref_proc;
READ_ONLY Scheme_Object *scheme_unsafe_string_set_proc;
READ_ONLY Scheme_Object *scheme_unsafe_byte_string_length_proc;
READ_ONLY Scheme_Object *scheme_unsafe_bytes_ref_proc;
READ_ONLY Scheme_Object *scheme_unsafe_bytes_set_proc;
READ_ONLY Scheme_Object *scheme_unsafe_struct_ref_proc;
READ_ONLY Scheme_Object *scheme_unsafe_struct_star_ref_proc;
READ_ONLY Scheme_Object *scheme_unsafe_struct_set_proc;
READ_ONLY Scheme_Object *scheme_unsafe_struct_star_set_proc;

/* locals */
static Scheme_Object *vector_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_star_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_vector (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_copy_bang(int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *vector_to_values (int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_vector(int argc, Scheme_Object **argv);
static Scheme_Object *chaperone_vector_star(int argc, Scheme_Object **argv);
static Scheme_Object *impersonate_vector(int argc, Scheme_Object **argv);
static Scheme_Object *impersonate_vector_star(int argc, Scheme_Object **argv);
static Scheme_Object *unsafe_chaperone_vector(int argc, Scheme_Object **argv);
static Scheme_Object *unsafe_impersonate_vector(int argc, Scheme_Object **argv);

static Scheme_Object *unsafe_vector_len (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_star_len (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_star_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_star_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_star_cas (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_struct_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_struct_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_struct_star_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_struct_star_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_struct_star_cas (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_string_len (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_string_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_string_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_len (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_set (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_copy_bang (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bytes_immutable_bang (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_string_immutable_bang (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_vector_immutable_bang (int argc, Scheme_Object *argv[]);

void
scheme_init_vector (Scheme_Startup_Env *env)
{
  Scheme_Object *p;

  REGISTER_SO(scheme_vector_p_proc);
  p = scheme_make_folding_prim(vector_p, "vector?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("vector?", p, env);
  scheme_vector_p_proc = p;

  REGISTER_SO(scheme_make_vector_proc);
  p = scheme_make_immed_prim(scheme_checked_make_vector, "make-vector", 1, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("make-vector", p, env);
  scheme_make_vector_proc = p;

  REGISTER_SO(scheme_vector_proc);
  p = scheme_make_immed_prim(vector, "vector", 0, -1);
  scheme_vector_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("vector", p, env);

  REGISTER_SO(scheme_vector_immutable_proc);
  p = scheme_make_immed_prim(vector_immutable, "vector-immutable", 0, -1);
  scheme_vector_immutable_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("vector-immutable", p, env);

  
  REGISTER_SO(scheme_vector_length_proc);
  p = scheme_make_folding_prim(vector_length, "vector-length", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector-length", p, env);
  scheme_vector_length_proc = p;

  REGISTER_SO(scheme_vector_star_length_proc);
  p = scheme_make_folding_prim(vector_star_length, "vector*-length", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector*-length", p, env);
  scheme_vector_star_length_proc = p;

  REGISTER_SO(scheme_vector_ref_proc);
  p = scheme_make_noncm_prim(scheme_checked_vector_ref, 
                             "vector-ref", 
                             2, 2);
  scheme_vector_ref_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector-ref", p, env);

  REGISTER_SO(scheme_vector_star_ref_proc);
  p = scheme_make_noncm_prim(scheme_checked_vector_star_ref, 
                             "vector*-ref", 
                             2, 2);
  scheme_vector_star_ref_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector*-ref", p, env);

  REGISTER_SO(scheme_vector_set_proc);
  p = scheme_make_noncm_prim(scheme_checked_vector_set,
                             "vector-set!", 
                             3, 3);
  scheme_vector_set_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector-set!", p, env);

  REGISTER_SO(scheme_vector_star_set_proc);
  p = scheme_make_noncm_prim(scheme_checked_vector_star_set,
                             "vector*-set!", 
                             3, 3);
  scheme_vector_star_set_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector*-set!", p, env);

  REGISTER_SO(scheme_vector_cas_proc);
  p = scheme_make_noncm_prim(scheme_checked_vector_cas,
                             "vector-cas!",
                             4, 4);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("vector-cas!", p, env);
  scheme_vector_cas_proc = p;

  p = scheme_make_immed_prim(vector_to_list, "vector->list", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector->list", p, env);

  REGISTER_SO(scheme_list_to_vector_proc);
  p = scheme_make_immed_prim(list_to_vector, "list->vector", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_list_to_vector_proc = p;
  scheme_addto_prim_instance("list->vector", p, env);
  
  scheme_addto_prim_instance("vector-fill!", 
			     scheme_make_immed_prim(vector_fill, 
						    "vector-fill!", 
						    2, 2), 
			     env);
  scheme_addto_prim_instance("vector-copy!", 
			     scheme_make_immed_prim(vector_copy_bang, 
						    "vector-copy!", 
						    3, 5), 
			     env);

  p = scheme_make_immed_prim(vector_to_immutable, "vector->immutable-vector", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector->immutable-vector", p, env);

  p = scheme_make_prim_w_arity2(vector_to_values, "vector->values", 
                                1, 3,
                                0, -1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("vector->values", p, env);

  scheme_addto_prim_instance("chaperone-vector",
                             scheme_make_prim_w_arity(chaperone_vector,
                                                      "chaperone-vector",
                                                      3, -1),
                             env);

  scheme_addto_prim_instance("chaperone-vector*",
			     scheme_make_prim_w_arity(chaperone_vector_star,
						      "chaperone-vector*",
						      3, -1),
			     env);

  scheme_addto_prim_instance("impersonate-vector",
                             scheme_make_prim_w_arity(impersonate_vector,
                                                      "impersonate-vector",
                                                      3, -1),
                             env);

  scheme_addto_prim_instance("impersonate-vector*",
			     scheme_make_prim_w_arity(impersonate_vector_star,
						      "impersonate-vector*",
						      3, -1),
			     env);
}

void
scheme_init_unsafe_vector (Scheme_Startup_Env *env)
{
  Scheme_Object *p;

  REGISTER_SO(scheme_unsafe_vector_length_proc);
  p = scheme_make_immed_prim(unsafe_vector_len, "unsafe-vector-length", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("unsafe-vector-length", p, env);
  scheme_unsafe_vector_length_proc = p;

  REGISTER_SO(scheme_unsafe_vector_star_length_proc);
  p = scheme_make_immed_prim(unsafe_vector_star_len, "unsafe-vector*-length", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("unsafe-vector*-length", p, env);
  scheme_unsafe_vector_star_length_proc = p;

  p = scheme_make_immed_prim(unsafe_vector_ref, "unsafe-vector-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance("unsafe-vector-ref", p, env);

  REGISTER_SO(scheme_unsafe_vector_star_ref_proc);
  p = scheme_make_immed_prim(unsafe_vector_star_ref, "unsafe-vector*-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_addto_prim_instance("unsafe-vector*-ref", p, env);
  scheme_unsafe_vector_star_ref_proc = p;

  p = scheme_make_immed_prim(unsafe_vector_set, "unsafe-vector-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-vector-set!", p, env);  

  REGISTER_SO(scheme_unsafe_vector_star_set_proc);
  p = scheme_make_immed_prim(unsafe_vector_star_set, "unsafe-vector*-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-vector*-set!", p, env);
  scheme_unsafe_vector_star_set_proc = p;

  p = scheme_make_immed_prim(unsafe_vector_star_cas, "unsafe-vector*-cas!", 4, 4);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-vector*-cas!", p, env);

  REGISTER_SO(scheme_unsafe_struct_ref_proc);
  p = scheme_make_immed_prim(unsafe_struct_ref, "unsafe-struct-ref", 2, 2);
  scheme_unsafe_struct_ref_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance("unsafe-struct-ref", p, env);

  REGISTER_SO(scheme_unsafe_struct_ref_proc);
  p = scheme_make_immed_prim(unsafe_struct_star_ref, "unsafe-struct*-ref", 2, 2);
  scheme_unsafe_struct_star_ref_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_addto_prim_instance("unsafe-struct*-ref", p, env);

  REGISTER_SO(scheme_unsafe_struct_set_proc);
  p = scheme_make_immed_prim(unsafe_struct_set, "unsafe-struct-set!", 3, 3);
  scheme_unsafe_struct_set_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-struct-set!", p, env);

  REGISTER_SO(scheme_unsafe_struct_star_set_proc);
  p = scheme_make_immed_prim(unsafe_struct_star_set, "unsafe-struct*-set!", 3, 3);
  scheme_unsafe_struct_star_set_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-struct*-set!", p, env);

  p = scheme_make_immed_prim(unsafe_struct_star_cas, "unsafe-struct*-cas!", 4, 4);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-struct*-cas!", p, env);

  REGISTER_SO(scheme_unsafe_string_length_proc);
  p = scheme_make_immed_prim(unsafe_string_len, "unsafe-string-length", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("unsafe-string-length", p, env);
  scheme_unsafe_string_length_proc = p;

  REGISTER_SO(scheme_unsafe_string_ref_proc);
  p = scheme_make_immed_prim(unsafe_string_ref, "unsafe-string-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE
                                                            | SCHEME_PRIM_IS_OMITABLE);
  scheme_addto_prim_instance("unsafe-string-ref", p, env);
  scheme_unsafe_string_ref_proc = p;

  REGISTER_SO(scheme_unsafe_string_set_proc);
  p = scheme_make_immed_prim(unsafe_string_set, "unsafe-string-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-string-set!", p, env);
  scheme_unsafe_string_set_proc = p;

  REGISTER_SO(scheme_unsafe_byte_string_length_proc);
  p = scheme_make_immed_prim(unsafe_bytes_len, "unsafe-bytes-length", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("unsafe-bytes-length", p, env);
  scheme_unsafe_byte_string_length_proc = p;

  REGISTER_SO(scheme_unsafe_bytes_ref_proc);
  p = scheme_make_immed_prim(unsafe_bytes_ref, "unsafe-bytes-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("unsafe-bytes-ref", p, env);
  scheme_unsafe_bytes_ref_proc = p;

  REGISTER_SO(scheme_unsafe_bytes_set_proc);
  p = scheme_make_immed_prim(unsafe_bytes_set, "unsafe-bytes-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-bytes-set!", p, env);
  scheme_unsafe_bytes_set_proc = p;

  scheme_addto_prim_instance("unsafe-bytes-copy!",
			     scheme_make_prim_w_arity(unsafe_bytes_copy_bang,
						      "unsafe-bytes-copy!",
						      3, 5),
			     env);

  scheme_addto_prim_instance("unsafe-bytes->immutable-bytes!",
			     scheme_make_prim_w_arity(unsafe_bytes_immutable_bang,
						      "unsafe-bytes->immutable-bytes!",
						      1, 1),
			     env);

  scheme_addto_prim_instance("unsafe-string->immutable-string!",
			     scheme_make_prim_w_arity(unsafe_string_immutable_bang,
						      "unsafe-string->immutable-string!",
						      1, 1),
			     env);

  scheme_addto_prim_instance("unsafe-vector*->immutable-vector!",
			     scheme_make_prim_w_arity(unsafe_vector_immutable_bang,
						      "unsafe-vector*->immutable-vector!",
						      1, 1),
			     env);

  scheme_addto_prim_instance("unsafe-impersonate-vector",
			     scheme_make_prim_w_arity(unsafe_impersonate_vector,
						      "unsafe-impersonate-vector",
						      2, -1),
			     env);

  scheme_addto_prim_instance("unsafe-chaperone-vector",
			     scheme_make_prim_w_arity(unsafe_chaperone_vector,
						      "unsafe-chaperone-vector",
						      2, -1),
			     env);
}

#define VECTOR_BYTES(size) (sizeof(Scheme_Vector) + ((size) - mzFLEX_DELTA) * sizeof(Scheme_Object *))
#define REV_VECTOR_BYTES(size) (((size) - (sizeof(Scheme_Vector) - (mzFLEX_DELTA * sizeof(Scheme_Object *)))) / sizeof(Scheme_Object *))

Scheme_Object *
scheme_make_vector (intptr_t size, Scheme_Object *fill)
{
  Scheme_Object *vec;
  intptr_t i;

  if (size < 0) {
    vec = scheme_make_integer(size);
    scheme_wrong_contract("make-vector", "exact-nonnegative-integer?", -1, 0, &vec);
  }

  if (size < 1024) {
    vec = (Scheme_Object *)scheme_malloc_tagged(VECTOR_BYTES(size));
  } else {
    size_t sz;
    sz = VECTOR_BYTES(size);
    if (REV_VECTOR_BYTES(sz) != size)
      /* overflow */
      scheme_raise_out_of_memory(NULL, NULL);
    else
      vec = (Scheme_Object *)scheme_malloc_fail_ok(scheme_malloc_tagged, sz);
  }

  vec->type = scheme_vector_type;
  SCHEME_VEC_SIZE(vec) = size;

  if (fill) {
    for (i = 0; i < size; i++) {
      SCHEME_VEC_ELS(vec)[i] = fill;
    }
  }

  return vec;
}

/* locals */

static Scheme_Object *
vector_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_CHAPERONE_VECTORP(argv[0]) ? scheme_true : scheme_false);
}

Scheme_Object *
scheme_checked_make_vector (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec, *fill;
  intptr_t len;

  len = scheme_extract_index("make-vector", 0, argc, argv, -1, 0);

  if ((len == -1) 
      /* also watch for overflow: */
      || (REV_VECTOR_BYTES(VECTOR_BYTES(len)) != len)) {
    scheme_raise_out_of_memory("make-vector", "making vector of length %s",
			       scheme_make_provided_string(argv[0], 1, NULL));
  }

  if (argc == 2)
    fill = argv[1];
  else
    fill = scheme_make_integer(0);

  vec = scheme_make_vector(len, fill);

  return vec;
}

static Scheme_Object *
vector (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec;
  int i;

  vec = scheme_make_vector (argc, 0);
  for (i = 0; i < argc ; i++) {
    SCHEME_VEC_ELS(vec)[i] = argv[i];
  }

  return vec;
}

static Scheme_Object *
vector_immutable (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec;

  vec = vector(argc, argv);
  SCHEME_SET_IMMUTABLE(vec);

  return vec;
}

static Scheme_Object *
vector_length (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];

  if (SCHEME_NP_CHAPERONEP(vec))
    vec = SCHEME_CHAPERONE_VAL(vec);

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_contract("vector-length", "vector?", 0, argc, argv);

  return scheme_make_integer(SCHEME_VEC_SIZE(vec));
}

Scheme_Object *scheme_vector_length(Scheme_Object *v)
{
  Scheme_Object *a[1];
  a[0] = v;
  return vector_length(1, a);
}

static Scheme_Object *
vector_star_length (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_contract("vector*-length", "(and/c vector? (not/c impersonator?))", 0, argc, argv);

  return scheme_make_integer(SCHEME_VEC_SIZE(vec));
}

Scheme_Object *scheme_vector_star_length(Scheme_Object *v)
{
  Scheme_Object *a[1];
  a[0] = v;
  return vector_star_length(1, a);
}

void scheme_bad_vec_index(char *name, Scheme_Object *i, const char *which, Scheme_Object *vec, 
                          intptr_t bottom, intptr_t len)
{
  const char *type;

#ifdef MZ_LONG_DOUBLE
#define BAD_EXTFLVEC_INDEX  (SCHEME_EXTFLVECTORP(vec)? "extflvector" : NULL)
#else
#define BAD_EXTFLVEC_INDEX  NULL
#endif

  type = (SCHEME_CHAPERONE_VECTORP(vec) 
          ? "vector" 
          : (SCHEME_FLVECTORP(vec)
             ? "flvector"
             : (SCHEME_FXVECTORP(vec)
                ? "fxvector"
                : BAD_EXTFLVEC_INDEX
                )));


  scheme_out_of_range(name, type, which, i, vec, bottom, len-1);
}

static Scheme_Object *
bad_index(char *name, const char *which, Scheme_Object *i, Scheme_Object *vec, int bottom)
{
  scheme_bad_vec_index(name, i, which, vec, bottom, 
                       (SCHEME_NP_CHAPERONEP(vec)
                        ? SCHEME_VEC_SIZE(SCHEME_CHAPERONE_VAL(vec))
                        : SCHEME_VEC_SIZE(vec)));
  return NULL;
}

static Scheme_Object *chaperone_vector_ref_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return scheme_chaperone_vector_ref(o, p->ku.k.i1);
}

static Scheme_Object *chaperone_vector_ref_overflow(Scheme_Object *o, int i)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = (void *)o;
  p->ku.k.i1 = i;

  return scheme_handle_stack_overflow(chaperone_vector_ref_k);
}

Scheme_Object *scheme_chaperone_vector_ref2(Scheme_Object *o, int i, Scheme_Object *outermost)
{
  if (!SCHEME_NP_CHAPERONEP(o)) {
    return SCHEME_VEC_ELS(o)[i];
  } else {
    Scheme_Chaperone *px = (Scheme_Chaperone *)o;
    Scheme_Object *a[4], *red, *orig;

#ifdef DO_STACK_CHECK
    {
# include "mzstkchk.h"
      return chaperone_vector_ref_overflow(o, i);
    }
#endif

    if(SCHEME_FALSEP(px->redirects)) {
      /* unsafe chaperones */
      return scheme_chaperone_vector_ref2(px->val, i, outermost);
    }

    orig = scheme_chaperone_vector_ref2(px->prev, i, outermost);

    if (SCHEME_REDIRECTS_PROP_ONLY_VECTORP(px->redirects)) {
      /* chaperone was on property accessors */
      /* or vector chaperone is property only */
      return orig;
    }
    red = SCHEME_CAR(px->redirects);

    if (SCHEME_CHAPERONE_FLAGS(px) & SCHEME_VEC_CHAPERONE_STAR) {
      a[0] = outermost;
      a[1] = px->prev;
      a[2] = scheme_make_integer(i);
      a[3] = orig;
      o = _scheme_apply(red, 4, a);
    }
    else {
      a[0] = px->prev;
      a[1] = scheme_make_integer(i);
      a[2] = orig;
      o = _scheme_apply(red, 3, a);
    }

    if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
      if (!scheme_chaperone_of(o, orig))
        scheme_wrong_chaperoned("vector-ref", "result", orig, o);

    return o;
  }
}

Scheme_Object *scheme_chaperone_vector_ref(Scheme_Object *o, int i)
{
  return scheme_chaperone_vector_ref2(o, i, o);
}

Scheme_Object *
scheme_checked_vector_ref (int argc, Scheme_Object *argv[])
{
  intptr_t i, len;
  Scheme_Object *vec;

  vec = argv[0];
  if (SCHEME_CHAPERONEP(vec))
    vec = SCHEME_CHAPERONE_VAL(vec);

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_contract("vector-ref", "vector?", 0, argc, argv);

  len = SCHEME_VEC_SIZE(vec);

  i = scheme_extract_index("vector-ref", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector-ref", "", argv[1], argv[0], 0);

  if (!SAME_OBJ(vec, argv[0]))
    /* chaperone */
    return scheme_chaperone_vector_ref(argv[0], i);
  else
    return (SCHEME_VEC_ELS(vec))[i];
}

void scheme_chaperone_vector_set(Scheme_Object *o, int i, Scheme_Object *v)
{
  Scheme_Object *outermost = o;
  while (1) {
    if (!SCHEME_NP_CHAPERONEP(o)) {
      SCHEME_VEC_ELS(o)[i] = v;
      return;
    } else {
      Scheme_Chaperone *px = (Scheme_Chaperone *)o;
      Scheme_Object *a[4], *red;
      int chap_star = SCHEME_CHAPERONE_FLAGS(px) & SCHEME_VEC_CHAPERONE_STAR ? 1 : 0;

      red = px->redirects;
      if (SCHEME_FALSEP(red)) {
	o = px->val;
	continue;
      }

      o = px->prev;

      if (!SCHEME_REDIRECTS_PROP_ONLY_VECTORP(red)) {
	/* not a property only chaperone */
	red = SCHEME_CDR(px->redirects);

	if (chap_star) {
	  a[0] = outermost;
	  a[1] = o;
	  a[2] = scheme_make_integer(i);
	  a[3] = v;
	  v = _scheme_apply(red, 4, a);
	}
	else {
	  a[0] = o;
	  a[1] = scheme_make_integer(i);
	  a[2] = v;
	  v = _scheme_apply(red, 3, a);
	}

	if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
	  if (!scheme_chaperone_of(v, a[2 + chap_star]))
	    scheme_wrong_chaperoned("vector-set!", "value", a[2 + chap_star], v);
      }
    }
  }
}

Scheme_Object *
scheme_checked_vector_star_ref (int argc, Scheme_Object *argv[])
{
  intptr_t i, len;
  Scheme_Object *vec;

  vec = argv[0];
  if (!SCHEME_VECTORP(vec))
    scheme_wrong_contract("vector*-ref", "(and/c vector? (not impersonator?))", 0, argc, argv);

  len = SCHEME_VEC_SIZE(vec);

  i = scheme_extract_index("vector*-ref", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector*-ref", "", argv[1], argv[0], 0);

  return (SCHEME_VEC_ELS(vec))[i];
}

Scheme_Object *
scheme_checked_vector_set(int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];
  intptr_t i, len;

  if (SCHEME_CHAPERONEP(vec))
    vec = SCHEME_CHAPERONE_VAL(vec);

  if (!SCHEME_MUTABLE_VECTORP(vec))
    scheme_wrong_contract("vector-set!", "(and/c vector? (not/c immutable?))", 0, argc, argv);

  len = SCHEME_VEC_SIZE(vec);

  i = scheme_extract_index("vector-set!", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector-set!", "", argv[1], argv[0], 0);

  if (!SAME_OBJ(vec, argv[0]))
    scheme_chaperone_vector_set(argv[0], i, argv[2]);
  else
    SCHEME_VEC_ELS(vec)[i] = argv[2];

  return scheme_void;
}

Scheme_Object *
scheme_checked_vector_star_set(int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];
  intptr_t i, len;

  if (!SCHEME_MUTABLE_VECTORP(vec))
    scheme_wrong_contract("vector*-set!", "(and/c vector? (not/c immutable?) (not/c impersonator?))", 0, argc, argv);

  len = SCHEME_VEC_SIZE(vec);

  i = scheme_extract_index("vector*-set!", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector*-set!", "", argv[1], argv[0], 0);

  SCHEME_VEC_ELS(vec)[i] = argv[2];

  return scheme_void;
}

Scheme_Object *
scheme_checked_vector_cas(int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];
  intptr_t i, len;

  if (!SCHEME_MUTABLE_VECTORP(vec))
    scheme_wrong_contract("vector-cas!", "(and/c vector? (not/c immutable?) (not/c impersonator?))", 0, argc, argv);

  len = SCHEME_VEC_SIZE(vec);

  i = scheme_extract_index("vector-cas!", 1, argc, argv, len, 0);

  if (i >= len)
    return bad_index("vector-cas!", "", argv[1], argv[0], 0);

  return unsafe_vector_star_cas(argc, argv);
}

# define cons(car, cdr) scheme_make_pair(car, cdr)

Scheme_Object *
scheme_vector_to_list (Scheme_Object *vec)
{
  int i;
  Scheme_Object *pair = scheme_null;

  i = SCHEME_VEC_SIZE(vec);

  if (i < 0xFFF) {
    for (; i--; ) {
      pair = cons(SCHEME_VEC_ELS(vec)[i], pair);
    }
  } else {
    for (; i--; ) {
      if (!(i & 0xFFF))
	SCHEME_USE_FUEL(0xFFF);
      pair = cons(SCHEME_VEC_ELS(vec)[i], pair);
    }
  }

  return pair;
}


Scheme_Object *
chaperone_vector_to_list (Scheme_Object *vec)
{
  int i;
  Scheme_Object *pair = scheme_null;

  i = SCHEME_VEC_SIZE(SCHEME_CHAPERONE_VAL(vec));

  for (; i--; ) {
    if (!(i & 0xFFF))
      SCHEME_USE_FUEL(0xFFF);
    pair = cons(scheme_chaperone_vector_ref(vec, i), pair);
  }

  return pair;
}

static Scheme_Object *
vector_to_list (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];

  if (SCHEME_NP_CHAPERONEP(vec))
    vec = SCHEME_CHAPERONE_VAL(vec);

  if (!SCHEME_VECTORP(vec)) {
    scheme_wrong_contract("vector->list", "vector?", 0, argc, argv);
    return NULL;
  }

  if (!SAME_OBJ(vec, argv[0]))
    return chaperone_vector_to_list(argv[0]);
  else
    return scheme_vector_to_list(vec);
}

static Scheme_Object *
list_to_vector (int argc, Scheme_Object *argv[])
{
  return scheme_list_to_vector(argv[0]);
}

Scheme_Object *
scheme_list_to_vector (Scheme_Object *list)
{
  intptr_t len, i;
  Scheme_Object *vec, *orig = list;

  len = scheme_proper_list_length(list);
  if (len < 0)
    scheme_wrong_contract("list->vector", "list?", -1, 0, &orig);

  vec = scheme_make_vector(len, NULL);
  for (i = 0; i < len; i++) {
    SCHEME_VEC_ELS(vec)[i] = SCHEME_CAR(list);
    list = SCHEME_CDR(list);
  }

  return vec;
}

static Scheme_Object *
vector_fill (int argc, Scheme_Object *argv[])
{
  int i, sz;
  Scheme_Object *v, *vec = argv[0];

  if (SCHEME_NP_CHAPERONEP(vec))
    vec = SCHEME_CHAPERONE_VAL(vec);
  
  if (!SCHEME_MUTABLE_VECTORP(vec))
    scheme_wrong_contract("vector-fill!", "(and/c vector? (not/c immutable?))", 0, argc, argv);

  v = argv[1];
  sz = SCHEME_VEC_SIZE(vec);
  if (SAME_OBJ(vec, argv[0])) {
    for (i = 0; i < sz; i++) {
      SCHEME_VEC_ELS(argv[0])[i] = v;
    }
  } else {
    for (i = 0; i < sz; i++) {
      scheme_chaperone_vector_set(argv[0], i, v);
    }
  }

  return scheme_void;
}

static Scheme_Object *vector_copy_bang(int argc, Scheme_Object *argv[])
{
  Scheme_Object *s1, *s2;
  intptr_t istart, ifinish;
  intptr_t ostart, ofinish;
  int slow = 0;

  s1 = argv[0];
  if (SCHEME_NP_CHAPERONEP(s1)) {
    slow = 1;
    s1 = SCHEME_CHAPERONE_VAL(s1);
  }
  if (!SCHEME_MUTABLE_VECTORP(s1))
    scheme_wrong_contract("vector-copy!", "(and/c vector? (not/c immutable?))", 0, argc, argv);

  scheme_do_get_substring_indices("vector-copy!", s1, 
                                  argc, argv, 1, 5, 
                                  &ostart, &ofinish, SCHEME_VEC_SIZE(s1));

  s2 = argv[2];
  if (SCHEME_NP_CHAPERONEP(s2)) {
    slow = 1;
    s2 = SCHEME_CHAPERONE_VAL(s2);
  }
  if (!SCHEME_VECTORP(s2))
    scheme_wrong_contract("vector-copy!", "vector?", 2, argc, argv);

  scheme_do_get_substring_indices("vector-copy!", s2, 
                                  argc, argv, 3, 4, 
                                  &istart, &ifinish, SCHEME_VEC_SIZE(s2));

  if ((ofinish - ostart) < (ifinish - istart)) {
    scheme_contract_error("vector-copy!",
                          "not enough room in target vector",
                          "target vector", 1, argv[2],
                          "starting index", 1, scheme_make_integer(ostart),
                          "element count", 1, scheme_make_integer(ofinish - ostart),
                          NULL);
    return NULL;
  }

  if (slow) {
    int i, o;
    if ((s2 == s1)
        && (((istart <= ostart) && (ifinish > ostart))
            || ((ostart <= istart) && (ofinish > istart)))
        && (istart < ostart)) {
      /* ranges overlap and shifting up: copy from end */
      for (i = ifinish, o = ofinish; i-- > istart; ) {
        o--;
        scheme_chaperone_vector_set(argv[0], o, scheme_chaperone_vector_ref(argv[2], i));
      }
    } else {
      for (i = istart, o = ostart; i < ifinish; i++, o++) {
        scheme_chaperone_vector_set(argv[0], o, scheme_chaperone_vector_ref(argv[2], i));
      }
    }
  } else {
    memmove(SCHEME_VEC_ELS(s1) + ostart,
            SCHEME_VEC_ELS(s2) + istart,
            (ifinish - istart) * sizeof(Scheme_Object*));
  }
  
  return scheme_void;
}

Scheme_Object *scheme_chaperone_vector_copy(Scheme_Object *vec)
{
  int len;
  Scheme_Object *a[3], *vec2;

  if (SCHEME_NP_CHAPERONEP(vec))
    len = SCHEME_VEC_SIZE(SCHEME_CHAPERONE_VAL(vec));
  else
    len = SCHEME_VEC_SIZE(vec);

  vec2 = scheme_make_vector(len, NULL);
  a[0] = vec2;
  a[1] = scheme_make_integer(0);
  a[2] = vec;

  (void)vector_copy_bang(3, a);

  return vec2;
}

static Scheme_Object *vector_to_immutable (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec, *ovec, *v;
  intptr_t len, i;

  vec = argv[0];
  if (SCHEME_NP_CHAPERONEP(vec))
    vec = SCHEME_CHAPERONE_VAL(vec);  

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_contract("vector->immutable-vector", "vector?", 0, argc, argv);

  if (SCHEME_IMMUTABLEP(vec))
    return argv[0];

  ovec = vec;
  len = SCHEME_VEC_SIZE(ovec);

  vec = scheme_make_vector(len, NULL);
  if (!SAME_OBJ(ovec, argv[0])) {
    for (i = 0; i < len; i++) {
      v = scheme_chaperone_vector_ref(argv[0], i);
      SCHEME_VEC_ELS(vec)[i] = v;
    }
  } else {
    for (i = 0; i < len; i++) {
      SCHEME_VEC_ELS(vec)[i] = SCHEME_VEC_ELS(ovec)[i];
    }
  }
  SCHEME_SET_IMMUTABLE(vec);

  return vec;  
}

static Scheme_Object *vector_to_values (int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p;
  Scheme_Object *vec, **a, *plain_vec;
  intptr_t len, start, finish, i;

  vec = argv[0];
  if (SCHEME_NP_CHAPERONEP(vec))
    vec = SCHEME_CHAPERONE_VAL(vec);  

  if (!SCHEME_VECTORP(vec))
    scheme_wrong_contract("vector->values", "vector?", 0, argc, argv);

  len = SCHEME_VEC_SIZE(vec);

  if (argc > 1)
    start = scheme_extract_index("vector->values", 1, argc, argv, len + 1, 0);
  else
    start = 0;
  if (argc > 2)
    finish = scheme_extract_index("vector->values", 2, argc, argv, len + 1, 0);
  else
    finish = len;

  if (!(start <= len)) {
    bad_index("vector->values", "starting ", argv[1], argv[0], 0);
  }
  if (!(finish >= start && finish <= len)) {
    bad_index("vector->values", "ending ", argv[2], argv[0], start);
  }

  len = finish - start;
  if (len == 1) {
    if (!SAME_OBJ(vec, argv[0]))
      return scheme_chaperone_vector_ref(argv[0], start);
    else
      return SCHEME_VEC_ELS(vec)[start];
  }

  if (!SAME_OBJ(vec, argv[0])) {
    plain_vec = scheme_make_vector(len, NULL);
    for (i = 0; i < len; i++) {
      vec = scheme_chaperone_vector_ref(argv[0], start + i);
      SCHEME_VEC_ELS(plain_vec)[i] = vec;
    }
    vec = plain_vec;
    start = 0;
  }

  p = scheme_current_thread;
  if (p->values_buffer && (p->values_buffer_size >= len))
    a = p->values_buffer;
  else {
    a = MALLOC_N(Scheme_Object *, len);
    p->values_buffer = a;
    p->values_buffer_size = len;
  }

  p->ku.multiple.array = a;
  p->ku.multiple.count = len;

  for (i = 0; i < len; i++) {
    a[i] = SCHEME_VEC_ELS(vec)[start + i];
  }

  return SCHEME_MULTIPLE_VALUES;
}

static Scheme_Object *do_chaperone_vector(const char *name, int is_impersonator, int pass_self, int unsafe, int argc, Scheme_Object **argv)
{
  Scheme_Chaperone *px;
  Scheme_Object *val = argv[0];
  Scheme_Object *redirects;
  Scheme_Object *props;

  if (SCHEME_CHAPERONEP(val)) {
    val = SCHEME_CHAPERONE_VAL(val);
  }

  if (!SCHEME_VECTORP(val)
      || (is_impersonator && !SCHEME_MUTABLEP(val)))
    scheme_wrong_contract(name, is_impersonator ? "(and/c vector? (not/c immutable?))" : "vector?", 0, argc, argv);

  if (unsafe) {
    /* We cannot dispatch the operations on an unsafe vector chaperone to a chaperoned vector because of the invariant
       that the val field of a vector chaperone must point to a non-chaperoned vector.
       To ensure this we error if the second argument passed to `unsafe-chaperone-vector` is not a unchaperoned vector */
    if (!SCHEME_VECTORP(argv[1])) {
      scheme_wrong_contract(name, "(and/c vector? (not/c impersonator?))", 1, argc, argv);
    }
    val = argv[1];
  }
  else {
    /* allow false for interposition procedures */
    scheme_check_proc_arity2(name, 3 + (pass_self ? 1 : 0), 1, argc, argv, 1);
    scheme_check_proc_arity2(name, 3 + (pass_self ? 1 : 0), 2, argc, argv, 1);

    /* but only allow `#f` if both are `#f` */
    if (SCHEME_FALSEP(argv[1]) != SCHEME_FALSEP(argv[2])) {
      scheme_contract_error(name,
                            "accessor and mutator wrapper must be both `#f` or neither `#f`",
                            "accessor wrapper", 1, argv[1],
                            "mutator wrapper", 1, argv[2],
                            NULL);
    }
  }

  props = scheme_parse_chaperone_props(name, unsafe ? 2 : 3, argc, argv);

  /*
     Regular vector chaperones store redirect procedures in a pair, (cons getter setter).
     Property only vector chaperones have no redirection procedures, and redirects is assigned an empty vector.
     Unsafe vector chaperones dispatch operations to another vector stored in a box in redirects.
   */
  if (SCHEME_FALSEP(argv[1])) {
    redirects = scheme_make_vector(0, NULL);
  }
  else if (unsafe) {
    redirects = scheme_false;
  }
  else {
    redirects = scheme_make_pair(argv[1], argv[2]);
  }

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  px->iso.so.type = scheme_chaperone_type;
  px->props = props;
  px->val = val;
  px->prev = argv[0];
  px->redirects = redirects;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  /* Use flag to tell if the chaperone is a chaperone* */
  if (pass_self) {
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_VEC_CHAPERONE_STAR;
  }
  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_vector(int argc, Scheme_Object **argv)
{
  return do_chaperone_vector("chaperone-vector", 0, 0, 0, argc, argv);
}

static Scheme_Object *chaperone_vector_star(int argc, Scheme_Object **argv)
{
  return do_chaperone_vector("chaperone-vector*", 0, 1, 0, argc, argv);
}

static Scheme_Object *impersonate_vector(int argc, Scheme_Object **argv)
{
  return do_chaperone_vector("impersonate-vector", 1, 0, 0, argc, argv);
}

static Scheme_Object *impersonate_vector_star(int argc, Scheme_Object **argv)
{
  return do_chaperone_vector("impersonate-vector*", 1, 1, 0, argc, argv);
}

static Scheme_Object *unsafe_chaperone_vector(int argc, Scheme_Object **argv)
{
  return do_chaperone_vector("unsafe-chaperone-vector", 0, 0, 1, argc, argv);
}

static Scheme_Object *unsafe_impersonate_vector(int argc, Scheme_Object **argv)
{
  return do_chaperone_vector("unsafe-impersonate-vector", 1, 0, 1, argc, argv);
}

/************************************************************/
/*                        unsafe                            */
/************************************************************/

static Scheme_Object *unsafe_vector_len (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];
  intptr_t n;
  if (SCHEME_NP_CHAPERONEP(vec)) vec = SCHEME_CHAPERONE_VAL(vec);
  n = SCHEME_VEC_SIZE(vec);
  return scheme_make_integer(n);
}

static Scheme_Object *unsafe_vector_ref (int argc, Scheme_Object *argv[])
{
  if (SCHEME_NP_CHAPERONEP(argv[0]))
    return scheme_chaperone_vector_ref(argv[0], SCHEME_INT_VAL(argv[1]));
  else
    return SCHEME_VEC_ELS(argv[0])[SCHEME_INT_VAL(argv[1])];
}

static Scheme_Object *unsafe_vector_set (int argc, Scheme_Object *argv[])
{
  if (SCHEME_NP_CHAPERONEP(argv[0]))
    scheme_chaperone_vector_set(argv[0], SCHEME_INT_VAL(argv[1]), argv[2]);
  else
    SCHEME_VEC_ELS(argv[0])[SCHEME_INT_VAL(argv[1])] = argv[2];
  return scheme_void;
}

static Scheme_Object *unsafe_vector_star_len (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];
  intptr_t n;
  n = SCHEME_VEC_SIZE(vec);
  return scheme_make_integer(n);
}

static Scheme_Object *unsafe_vector_star_ref (int argc, Scheme_Object *argv[])
{
  return SCHEME_VEC_ELS(argv[0])[SCHEME_INT_VAL(argv[1])];
}

static Scheme_Object *unsafe_vector_star_set (int argc, Scheme_Object *argv[])
{
  SCHEME_VEC_ELS(argv[0])[SCHEME_INT_VAL(argv[1])] = argv[2];
  return scheme_void;
}

static Scheme_Object *unsafe_vector_star_cas (int argc, Scheme_Object *argv[])
{
  Scheme_Object *vec = argv[0];
  Scheme_Object *idx = argv[1];
  Scheme_Object *ov = argv[2];
  Scheme_Object *nv = argv[3];

#ifdef MZ_USE_FUTURES
  return mzrt_cas((volatile uintptr_t *)(SCHEME_VEC_ELS(vec) + SCHEME_INT_VAL(idx)),
                  (uintptr_t)ov, (uintptr_t)nv)
    ? scheme_true : scheme_false;
#else
  /* For cooperative threading, no atomicity required */
  if (SCHEME_VEC_ELS(vec)[SCHEME_INT_VAL(idx)] == ov) {
    SCHEME_VEC_ELS(vec)[SCHEME_INT_VAL(idx)] = nv;
    return scheme_true;
  } else {
    return scheme_false;
  }
#endif
}

static Scheme_Object *unsafe_struct_ref (int argc, Scheme_Object *argv[])
{
  if (SCHEME_CHAPERONEP(argv[0]))
    return scheme_struct_ref(argv[0], SCHEME_INT_VAL(argv[1]));
  else
    return ((Scheme_Structure *)argv[0])->slots[SCHEME_INT_VAL(argv[1])];
}

static Scheme_Object *unsafe_struct_set (int argc, Scheme_Object *argv[])
{
  if (SCHEME_CHAPERONEP(argv[0]))
    scheme_struct_set(argv[0], SCHEME_INT_VAL(argv[1]), argv[2]);
  else
    ((Scheme_Structure *)argv[0])->slots[SCHEME_INT_VAL(argv[1])] = argv[2];
  return scheme_void;
}

static Scheme_Object *unsafe_struct_star_ref (int argc, Scheme_Object *argv[])
{
  return ((Scheme_Structure *)argv[0])->slots[SCHEME_INT_VAL(argv[1])];
}

static Scheme_Object *unsafe_struct_star_set (int argc, Scheme_Object *argv[])
{
  ((Scheme_Structure *)argv[0])->slots[SCHEME_INT_VAL(argv[1])] = argv[2];
  return scheme_void;
}

static Scheme_Object *unsafe_struct_star_cas (int argc, Scheme_Object *argv[])
{
  Scheme_Object *s = argv[0];
  Scheme_Object *idx = argv[1];
  Scheme_Object *ov = argv[2];
  Scheme_Object *nv = argv[3];

#ifdef MZ_USE_FUTURES
  return (mzrt_cas((volatile uintptr_t *)(&((Scheme_Structure *)s)->slots[SCHEME_INT_VAL(idx)]),
                   (uintptr_t)ov, (uintptr_t)nv)
          ? scheme_true : scheme_false);
#else
  /* For cooperative threading, no atomicity required */
  if (((Scheme_Structure *)s)->slots[SCHEME_INT_VAL(idx)] == ov) {
    ((Scheme_Structure *)s)->slots[SCHEME_INT_VAL(idx)] = nv;
    return scheme_true;
  } else {
    return scheme_false;
  }
#endif
}

static Scheme_Object *unsafe_string_len (int argc, Scheme_Object *argv[])
{
  intptr_t n = SCHEME_CHAR_STRLEN_VAL(argv[0]);
  return scheme_make_integer(n);
}

static Scheme_Object *unsafe_string_ref (int argc, Scheme_Object *argv[])
{
  mzchar v;
  v = SCHEME_CHAR_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])];
  return scheme_make_ascii_character(v);
}

static Scheme_Object *unsafe_string_set (int argc, Scheme_Object *argv[])
{
  SCHEME_CHAR_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])] = SCHEME_CHAR_VAL(argv[2]);
  return scheme_void;
}

static Scheme_Object *unsafe_bytes_len (int argc, Scheme_Object *argv[])
{
  intptr_t n = SCHEME_BYTE_STRLEN_VAL(argv[0]);
  return scheme_make_integer(n);
}

static Scheme_Object *unsafe_bytes_ref (int argc, Scheme_Object *argv[])
{
  intptr_t v;
  v = (unsigned char)SCHEME_BYTE_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])];
  return scheme_make_integer(v);
}

static Scheme_Object *unsafe_bytes_set (int argc, Scheme_Object *argv[])
{
  SCHEME_BYTE_STR_VAL(argv[0])[SCHEME_INT_VAL(argv[1])] = (char)SCHEME_INT_VAL(argv[2]);
  return scheme_void;
}

static Scheme_Object *unsafe_bytes_copy_bang (int argc, Scheme_Object *argv[])
{
  Scheme_Object *s1, *s2;
  intptr_t istart, ifinish;
  intptr_t ostart;

  s1 = argv[0];
  ostart = SCHEME_INT_VAL(argv[1]);
  s2 = argv[2];
  if (argc > 3) {
    istart = SCHEME_INT_VAL(argv[3]);
    if (argc > 4)
      ifinish = SCHEME_INT_VAL(argv[4]);
    else
      ifinish = SCHEME_BYTE_STRLEN_VAL(s2);
  } else {
    istart = 0;
    ifinish = SCHEME_BYTE_STRLEN_VAL(s2);
  }

  memmove(SCHEME_BYTE_STR_VAL(s1) XFORM_OK_PLUS ostart,
	  SCHEME_BYTE_STR_VAL(s2) XFORM_OK_PLUS istart,
	  (ifinish - istart));

  return scheme_void;
}

static Scheme_Object *unsafe_bytes_immutable_bang (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_BYTE_STRINGP(o))
    scheme_wrong_contract("unsafe-bytes->immutable-bytes!", "bytes?", 0, argc, argv);

  SCHEME_SET_IMMUTABLE(o);

  return o;
}

static Scheme_Object *unsafe_string_immutable_bang (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_CHAR_STRINGP(o))
    scheme_wrong_contract("unsafe-string->immutable-string!", "string?", 0, argc, argv);

  SCHEME_SET_IMMUTABLE(o);

  return o;
}

static Scheme_Object *unsafe_vector_immutable_bang (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (!SCHEME_VECTORP(o))
    scheme_wrong_contract("unsafe-vector*->immutable-vector!", "(and/c vector? (not/c impersonator?))",
                          0, argc, argv);

  SCHEME_SET_IMMUTABLE(o);

  return o;
}
