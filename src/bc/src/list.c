#include "schpriv.h"
#include "schmach.h"

/* read only globals */
READ_ONLY Scheme_Object scheme_null[1];
READ_ONLY Scheme_Object *scheme_null_p_proc;
READ_ONLY Scheme_Object *scheme_pair_p_proc;
READ_ONLY Scheme_Object *scheme_mpair_p_proc;
READ_ONLY Scheme_Object *scheme_car_proc;
READ_ONLY Scheme_Object *scheme_cdr_proc;
READ_ONLY Scheme_Object *scheme_cons_proc;
READ_ONLY Scheme_Object *scheme_mcons_proc;
READ_ONLY Scheme_Object *scheme_list_p_proc;
READ_ONLY Scheme_Object *scheme_list_proc;
READ_ONLY Scheme_Object *scheme_list_star_proc;
READ_ONLY Scheme_Object *scheme_list_pair_p_proc;
READ_ONLY Scheme_Object *scheme_append_proc;
READ_ONLY Scheme_Object *scheme_box_proc;
READ_ONLY Scheme_Object *scheme_box_immutable_proc;
READ_ONLY Scheme_Object *scheme_box_p_proc;
READ_ONLY Scheme_Object *scheme_hash_ref_proc;
READ_ONLY Scheme_Object *scheme_hash_proc;
READ_ONLY Scheme_Object *scheme_hasheq_proc;
READ_ONLY Scheme_Object *scheme_hasheqv_proc;
READ_ONLY Scheme_Object *scheme_unsafe_cons_list_proc;
READ_ONLY Scheme_Object *scheme_unsafe_car_proc;
READ_ONLY Scheme_Object *scheme_unsafe_cdr_proc;
READ_ONLY Scheme_Object *scheme_unsafe_mcar_proc;
READ_ONLY Scheme_Object *scheme_unsafe_mcdr_proc;
READ_ONLY Scheme_Object *scheme_unsafe_unbox_proc;
READ_ONLY Scheme_Object *scheme_unsafe_unbox_star_proc;
READ_ONLY Scheme_Object *scheme_unsafe_set_box_star_proc;

/* read only locals */
ROSYM static Scheme_Object *weak_symbol;
ROSYM static Scheme_Object *equal_symbol;

ROSYM static Scheme_Hash_Tree *empty_hash;
ROSYM static Scheme_Hash_Tree *empty_hasheq;
ROSYM static Scheme_Hash_Tree *empty_hasheqv;

/* locals */
static Scheme_Object *pair_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *mpair_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cons_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *mcons_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *null_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_star_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_pair_p_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *immutablep (int argc, Scheme_Object *argv[]);
static Scheme_Object *length_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *append_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *reverse_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *assv (int argc, Scheme_Object *argv[]);
static Scheme_Object *assq (int argc, Scheme_Object *argv[]);
static Scheme_Object *assoc (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdddr_prim (int argc, Scheme_Object *argv[]);

static Scheme_Object *cddddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaddr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caddar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cddaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cdaaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *cadaar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caadar_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaadr_prim (int argc, Scheme_Object *argv[]);
static Scheme_Object *caaaar_prim (int argc, Scheme_Object *argv[]);

static Scheme_Object *box (int argc, Scheme_Object *argv[]);
static Scheme_Object *immutable_box (int argc, Scheme_Object *argv[]);
static Scheme_Object *box_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *unbox (int argc, Scheme_Object *argv[]);
static Scheme_Object *unbox_star (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_box (int argc, Scheme_Object *argv[]);
static Scheme_Object *set_box_star (int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_box_cas (int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_box(int argc, Scheme_Object **argv);
static Scheme_Object *impersonate_box(int argc, Scheme_Object **argv);

static Scheme_Object *make_hash(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_hasheq(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_hasheqv(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_weak_hash(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_weak_hasheq(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_weak_hasheqv(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_make_immutable_hash(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_make_immutable_hasheq(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_make_immutable_hasheqv(int argc, Scheme_Object *argv[]);
static Scheme_Object *direct_hash(int argc, Scheme_Object *argv[]);
static Scheme_Object *direct_hasheq(int argc, Scheme_Object *argv[]);
static Scheme_Object *direct_hasheqv(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_copy(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_eq_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_eqv_p(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_equal_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_weak_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_ref_key(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_put_bang(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_table_put(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_remove_bang(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_remove(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_clear_bang(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_clear(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_map(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_table_for_each(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_table_iterate_start(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_table_iterate_next(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_table_iterate_key(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_table_iterate_value(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_table_iterate_pair(int argc, Scheme_Object *argv[]);
Scheme_Object *scheme_hash_table_iterate_key_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *hash_keys_subset_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *eq_hash_code(int argc, Scheme_Object *argv[]);
static Scheme_Object *equal_hash_code(int argc, Scheme_Object *argv[]);
static Scheme_Object *equal_hash2_code(int argc, Scheme_Object *argv[]);
static Scheme_Object *eqv_hash_code(int argc, Scheme_Object *argv[]);
static Scheme_Object *chaperone_hash(int argc, Scheme_Object **argv);
static Scheme_Object *impersonate_hash(int argc, Scheme_Object **argv);

static Scheme_Object *make_weak_box(int argc, Scheme_Object *argv[]);
static Scheme_Object *weak_box_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *weak_boxp(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_ephemeron(int argc, Scheme_Object *argv[]);
static Scheme_Object *ephemeron_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *ephemeronp(int argc, Scheme_Object *argv[]);
static Scheme_Object *impersonator_ephemeron(int argc, Scheme_Object *argv[]);

static Scheme_Object *make_graph(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_placeholder(int argc, Scheme_Object *argv[]);
static Scheme_Object *placeholder_set(int argc, Scheme_Object *argv[]);
static Scheme_Object *placeholder_get(int argc, Scheme_Object *argv[]);
static Scheme_Object *placeholder_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_hash_placeholder(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_hasheq_placeholder(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_hasheqv_placeholder(int argc, Scheme_Object *argv[]);
static Scheme_Object *table_placeholder_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *unsafe_cons_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_car (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_cdr (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_list_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_list_tail (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_mcar (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_mcdr (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_set_mcar (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_set_mcdr (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_unbox (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_unbox_star (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_set_box (int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_set_box_star (int argc, Scheme_Object *argv[]);

/* unsafe_hash_table */
static Scheme_Object *unsafe_hash_table_iterate_start(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_table_iterate_next(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_table_iterate_key(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_table_iterate_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_table_iterate_pair(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_table_iterate_key_value(int argc, Scheme_Object *argv[]);

/* unsafe_hash_tree */
static Scheme_Object *unsafe_hash_tree_iterate_start(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_tree_iterate_next(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_tree_iterate_key(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_tree_iterate_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_tree_iterate_pair(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_hash_tree_iterate_key_value(int argc, Scheme_Object *argv[]);

/* unsafe_bucket_table */
static Scheme_Object *unsafe_bucket_table_iterate_start(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bucket_table_iterate_next(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bucket_table_iterate_key(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bucket_table_iterate_value(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bucket_table_iterate_pair(int argc, Scheme_Object *argv[]);
static Scheme_Object *unsafe_bucket_table_iterate_key_value(int argc, Scheme_Object *argv[]);

static Scheme_Object *chaperone_hash_key(const char *name, Scheme_Object *table, Scheme_Object *key);
static void chaperone_hash_key_value(const char *name, Scheme_Object *obj, Scheme_Object *k, Scheme_Object **_chap_key, Scheme_Object **_chap_val, int ischap);
static Scheme_Object *chaperone_hash_tree_set(Scheme_Object *table, Scheme_Object *key, Scheme_Object *val);
static Scheme_Object *chaperone_hash_clear(const char *name, Scheme_Object *table);

void
scheme_init_list (Scheme_Startup_Env *env)
{
  Scheme_Object *p;
  
  scheme_null->type = scheme_null_type;

  scheme_addto_prim_instance ("null", scheme_null, env);

  REGISTER_SO(scheme_pair_p_proc);
  p = scheme_make_folding_prim(pair_p_prim, "pair?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance ("pair?", p, env);
  scheme_pair_p_proc = p;

  REGISTER_SO(scheme_mpair_p_proc);
  p = scheme_make_folding_prim(mpair_p_prim, "mpair?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance ("mpair?", p, env);
  scheme_mpair_p_proc = p;

  REGISTER_SO(scheme_cons_proc);
  p = scheme_make_immed_prim(cons_prim, "cons", 2, 2);
  scheme_cons_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance ("cons", p, env);

  REGISTER_SO(scheme_car_proc);
  p = scheme_make_folding_prim(scheme_checked_car, "car", 1, 1, 1);
  scheme_car_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("car", p, env);

  REGISTER_SO(scheme_cdr_proc);
  p = scheme_make_folding_prim(scheme_checked_cdr, "cdr", 1, 1, 1);
  scheme_cdr_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("cdr", p, env);

  REGISTER_SO(scheme_mcons_proc);
  p = scheme_make_immed_prim(mcons_prim, "mcons", 2, 2);
  scheme_mcons_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("mcons", p, env);

  p = scheme_make_immed_prim(scheme_checked_mcar, "mcar", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("mcar", p, env);

  p = scheme_make_immed_prim(scheme_checked_mcdr, "mcdr", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("mcdr", p, env);

  p = scheme_make_immed_prim(scheme_checked_set_mcar, "set-mcar!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance ("set-mcar!", p, env);

  p = scheme_make_immed_prim(scheme_checked_set_mcdr, "set-mcdr!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance ("set-mcdr!", p, env);

  REGISTER_SO(scheme_null_p_proc);
  p = scheme_make_folding_prim(null_p_prim, "null?", 1, 1, 1);
  scheme_null_p_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance ("null?", p, env);

  REGISTER_SO(scheme_list_p_proc);
  p = scheme_make_folding_prim(list_p_prim, "list?", 1, 1, 1);
  scheme_list_p_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance ("list?", p, env);

  REGISTER_SO(scheme_list_proc);
  p = scheme_make_immed_prim(list_prim, "list", 0, -1);
  scheme_list_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance ("list", p, env);

  REGISTER_SO(scheme_list_star_proc);
  p = scheme_make_immed_prim(list_star_prim, "list*", 1, -1);
  scheme_list_star_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance ("list*", p, env);

  REGISTER_SO(scheme_list_pair_p_proc);
  p = scheme_make_folding_prim(list_pair_p_prim, "list-pair?", 1, 1, 1);
  scheme_list_pair_p_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance ("list-pair?", p, env);

  p = scheme_make_folding_prim(immutablep, "immutable?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("immutable?", p, env);

  p = scheme_make_immed_prim(length_prim, "length", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("length", p, env);

  REGISTER_SO(scheme_append_proc);
  p = scheme_make_immed_prim(append_prim, "append", 0, -1);
  scheme_append_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("append", p, env);
  
  scheme_addto_prim_instance ("reverse",
			      scheme_make_immed_prim(reverse_prim,
						     "reverse",
						     1, 1),
			      env);

  p = scheme_make_immed_prim(scheme_checked_list_tail, "list-tail", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance ("list-tail", p, env);

  p = scheme_make_immed_prim(scheme_checked_list_ref, "list-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("list-ref",p, env);

  scheme_addto_prim_instance ("assq",
			      scheme_make_immed_prim(assq,
						     "assq",
						     2, 2),
			      env);
  scheme_addto_prim_instance ("assv",
			      scheme_make_immed_prim(assv,
						     "assv",
						     2, 2),
			      env);
  scheme_addto_prim_instance ("assoc",
			      scheme_make_immed_prim(assoc,
						     "assoc",
						     2, 2),
			      env);

  p = scheme_make_folding_prim(scheme_checked_caar, "caar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("caar", p, env);

  p = scheme_make_folding_prim(scheme_checked_cadr, "cadr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("cadr", p, env);

  p = scheme_make_folding_prim(scheme_checked_cdar, "cdar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("cdar", p, env);

  p = scheme_make_folding_prim(scheme_checked_cddr, "cddr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("cddr", p, env);

  p = scheme_make_folding_prim(caaar_prim, "caaar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("caaar", p, env);

  p = scheme_make_folding_prim(caadr_prim, "caadr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("caadr", p, env);

  p = scheme_make_folding_prim(cadar_prim, "cadar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cadar", p, env);

  p = scheme_make_folding_prim(cdaar_prim, "cdaar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cdaar", p, env);

  p = scheme_make_folding_prim(cdadr_prim, "cdadr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cdadr", p, env);

  p = scheme_make_folding_prim(cddar_prim, "cddar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cddar", p, env);

  p = scheme_make_folding_prim(caddr_prim, "caddr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("caddr", p, env);

  p = scheme_make_folding_prim(cdddr_prim, "cdddr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("cdddr", p, env);

  p = scheme_make_folding_prim(cddddr_prim, "cddddr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("cddddr", p, env);

  p = scheme_make_folding_prim(cadddr_prim, "cadddr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("cadddr", p, env);

  p = scheme_make_folding_prim(cdaddr_prim, "cdaddr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cdaddr", p, env);

  p = scheme_make_folding_prim(cddadr_prim, "cddadr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cddadr", p, env);

  p = scheme_make_folding_prim(cdddar_prim, "cdddar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cdddar", p, env);

  p = scheme_make_folding_prim(caaddr_prim, "caaddr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("caaddr", p, env);

  p = scheme_make_folding_prim(cadadr_prim, "cadadr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cadadr", p, env);

  p = scheme_make_folding_prim(caddar_prim, "caddar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("caddar", p, env);

  p = scheme_make_folding_prim(cdaadr_prim, "cdaadr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cdaadr", p, env);

  p = scheme_make_folding_prim(cdadar_prim, "cdadar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cdadar", p, env);

  p = scheme_make_folding_prim(cddaar_prim, "cddaar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cddaar", p, env);

  p = scheme_make_folding_prim(cdaaar_prim, "cdaaar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cdaaar", p, env);

  p = scheme_make_folding_prim(cadaar_prim, "cadaar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("cadaar", p, env);

  p = scheme_make_folding_prim(caadar_prim, "caadar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("caadar", p, env);
  
  p = scheme_make_folding_prim(caaadr_prim, "caaadr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("caaadr", p, env);

  p = scheme_make_folding_prim(caaaar_prim, "caaaar", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance ("caaaar", p, env);

  REGISTER_SO(scheme_box_proc);
  p = scheme_make_immed_prim(box, "box", 1, 1);
  scheme_box_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("box", p, env);

  REGISTER_SO(scheme_box_immutable_proc);
  p = scheme_make_immed_prim(immutable_box, "box-immutable", 1, 1);
  scheme_box_immutable_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("box-immutable", p, env);
  
  REGISTER_SO(scheme_box_p_proc);
  p = scheme_make_folding_prim(box_p, "box?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("box?", p, env);
  scheme_box_p_proc = p;

  p = scheme_make_noncm_prim(unbox, "unbox", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);  
  scheme_addto_prim_instance("unbox", p, env);

  p = scheme_make_immed_prim(set_box, "set-box!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("set-box!", p, env);

  p = scheme_make_noncm_prim(unbox_star, "unbox*", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("unbox*", p, env);

  p = scheme_make_immed_prim(set_box_star, "set-box*!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("set-box*!", p, env);

  p = scheme_make_immed_prim(scheme_box_cas, "box-cas!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("box-cas!", p, env);

  scheme_addto_prim_instance("chaperone-box",
                             scheme_make_prim_w_arity(chaperone_box,
                                                      "chaperone-box",
                                                      3, -1),
                             env);
  scheme_addto_prim_instance("impersonate-box",
                             scheme_make_prim_w_arity(impersonate_box,
                                                      "impersonate-box",
                                                      3, -1),
                             env);

  
  p = scheme_make_immed_prim(make_hash, "make-hash", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-hash", p, env);

  p = scheme_make_immed_prim(make_hasheq, "make-hasheq", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-hasheq", p, env);

  p = scheme_make_immed_prim(make_hasheqv, "make-hasheqv", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-hasheqv", p, env);

  p = scheme_make_immed_prim(make_weak_hash, "make-weak-hash", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-weak-hash", p, env);

  p = scheme_make_immed_prim(make_weak_hasheq, "make-weak-hasheq", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-weak-hasheq", p, env);

  p = scheme_make_immed_prim(make_weak_hasheqv, "make-weak-hasheqv", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-weak-hasheqv", p, env);

  p = scheme_make_immed_prim(scheme_make_immutable_hash, "make-immutable-hash", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-immutable-hash", p, env);

  p = scheme_make_immed_prim(scheme_make_immutable_hasheq, "make-immutable-hasheq", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-immutable-hasheq", p, env);

  p = scheme_make_immed_prim(scheme_make_immutable_hasheqv, "make-immutable-hasheqv", 0, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("make-immutable-hasheqv", p, env);

  REGISTER_SO(scheme_hash_proc);
  p = scheme_make_immed_prim(direct_hash, "hash", 0, -1);
  scheme_hash_proc = p;
  /* not SCHEME_PRIM_IS_EVEN_ARITY_OMITABLE_ALLOCATION, because `equal?`-hashing functions are called */
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_ARITY_0_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("hash", p, env);

  REGISTER_SO(scheme_hasheq_proc);
  p = scheme_make_immed_prim(direct_hasheq, "hasheq", 0, -1);
  scheme_hasheq_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_EVEN_ARITY_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("hasheq", p, env);

  REGISTER_SO(scheme_hasheqv_proc);
  p = scheme_make_immed_prim(direct_hasheqv, "hasheqv", 0, -1);
  scheme_hasheqv_proc = p;
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_EVEN_ARITY_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance("hasheqv", p, env);
  
  p = scheme_make_folding_prim(hash_p, "hash?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance ("hash?", p, env);


  scheme_addto_prim_instance("hash-eq?",
			     scheme_make_folding_prim(scheme_hash_eq_p,
						      "hash-eq?",
						      1, 1, 1),
			     env);
  scheme_addto_prim_instance("hash-eqv?",
			     scheme_make_folding_prim(scheme_hash_eqv_p,
						      "hash-eqv?",
						      1, 1, 1),
			     env);
  scheme_addto_prim_instance("hash-equal?",
			     scheme_make_folding_prim(scheme_hash_equal_p,
						      "hash-equal?",
						      1, 1, 1),
			     env);
  scheme_addto_prim_instance("hash-weak?",
			     scheme_make_folding_prim(hash_weak_p,
						      "hash-weak?",
						      1, 1, 1),
			     env);

  p = scheme_make_immed_prim(scheme_checked_hash_count, "hash-count", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_PRODUCES_FIXNUM);
  scheme_addto_prim_instance("hash-count", p, env);

  scheme_addto_prim_instance("hash-copy",
			     scheme_make_noncm_prim(hash_table_copy,
						    "hash-copy",
						    1, 1),
			     env);
  scheme_addto_prim_instance("hash-set!",
			     scheme_make_noncm_prim(hash_table_put_bang,
						    "hash-set!",
						    3, 3),
			     env);
  scheme_addto_prim_instance("hash-set",
			     scheme_make_noncm_prim(scheme_hash_table_put,
						    "hash-set",
						    3, 3),
			     env);
  REGISTER_SO(scheme_hash_ref_proc);
  scheme_hash_ref_proc = scheme_make_prim_w_arity(scheme_checked_hash_ref, "hash-ref", 2, 3);
  scheme_addto_prim_instance("hash-ref", scheme_hash_ref_proc, env);
  scheme_addto_prim_instance("hash-ref-key",
                             scheme_make_noncm_prim(hash_table_ref_key,
                                                    "hash-ref-key",
                                                    2, 3),
                             env);
  scheme_addto_prim_instance("hash-remove!",
			     scheme_make_noncm_prim(hash_table_remove_bang,
						    "hash-remove!",
						    2, 2),
			     env);
  scheme_addto_prim_instance("hash-remove",
			     scheme_make_noncm_prim(hash_table_remove,
						    "hash-remove",
						    2, 2),
			     env);
  scheme_addto_prim_instance("hash-clear!",
			     scheme_make_noncm_prim(hash_table_clear_bang,
						    "hash-clear!",
						    1, 1),
			     env);
  scheme_addto_prim_instance("hash-clear",
			     scheme_make_noncm_prim(hash_table_clear,
						    "hash-clear",
						    1, 1),
			     env);
  scheme_addto_prim_instance("hash-map",
			     scheme_make_noncm_prim(hash_table_map,
						    "hash-map",
						    2, 3),
			     env);
  scheme_addto_prim_instance("hash-for-each",
			     scheme_make_noncm_prim(hash_table_for_each,
						    "hash-for-each",
						    2, 3),
			     env);

  scheme_addto_prim_instance("hash-iterate-first",
			     scheme_make_immed_prim(scheme_hash_table_iterate_start,
						    "hash-iterate-first",
                                                    1, 1),
			     env);
  scheme_addto_prim_instance("hash-iterate-next",
			     scheme_make_immed_prim(scheme_hash_table_iterate_next,
						    "hash-iterate-next",
                                                    2, 2),
			     env);
  scheme_addto_prim_instance("hash-iterate-value",
			     scheme_make_noncm_prim(scheme_hash_table_iterate_value,
						    "hash-iterate-value",
                                                    2, 3),
			     env);
  scheme_addto_prim_instance("hash-iterate-key",
			     scheme_make_noncm_prim(scheme_hash_table_iterate_key,
						    "hash-iterate-key",
                                                    2, 3),
			     env);
  scheme_addto_prim_instance("hash-iterate-pair",
			     scheme_make_immed_prim(scheme_hash_table_iterate_pair,
						    "hash-iterate-pair",
                                                    2, 3),
			     env);
  scheme_addto_prim_instance("hash-iterate-key+value",
			     scheme_make_prim_w_arity2(scheme_hash_table_iterate_key_value,
                                                       "hash-iterate-key+value",
                                                       2, 3, 2, 2),
			     env);

  scheme_addto_prim_instance("hash-keys-subset?",
			     scheme_make_immed_prim(hash_keys_subset_p,
                                                    "hash-keys-subset?",
                                                    2, 2),
			     env);
  
  scheme_addto_prim_instance("chaperone-hash",
                             scheme_make_prim_w_arity(chaperone_hash,
                                                      "chaperone-hash",
                                                      5, -1),
                             env);
  scheme_addto_prim_instance("impersonate-hash",
                             scheme_make_prim_w_arity(impersonate_hash,
                                                      "impersonate-hash",
                                                      5, -1),
                             env);

  scheme_addto_prim_instance("eq-hash-code",
			     scheme_make_immed_prim(eq_hash_code,
						    "eq-hash-code",
						    1, 1),
			     env);
  scheme_addto_prim_instance("eqv-hash-code",
			     scheme_make_immed_prim(eqv_hash_code,
						    "eqv-hash-code",
						    1, 1),
			     env);
  scheme_addto_prim_instance("equal-hash-code",
			     scheme_make_noncm_prim(equal_hash_code,
						    "equal-hash-code",
						    1, 1),
			     env);
  scheme_addto_prim_instance("equal-secondary-hash-code",
			     scheme_make_noncm_prim(equal_hash2_code,
						    "equal-secondary-hash-code",
						    1, 1),
			     env);

  scheme_addto_prim_instance("make-weak-box",
			     scheme_make_immed_prim(make_weak_box,
						    "make-weak-box",
						    1, 1),
			     env);
  
  p = scheme_make_immed_prim(weak_box_value, "weak-box-value", 1, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED);
  scheme_addto_prim_instance("weak-box-value", p, env);

  scheme_addto_prim_instance("weak-box?",
			     scheme_make_folding_prim(weak_boxp,
						      "weak-box?",
						      1, 1, 1),
			     env);

  scheme_addto_prim_instance("make-ephemeron",
			     scheme_make_immed_prim(make_ephemeron,
						    "make-ephemeron",
						    2, 2),
			     env);
  scheme_addto_prim_instance("ephemeron-value",
			     scheme_make_immed_prim(ephemeron_value,
						    "ephemeron-value",
						    1, 3),
			     env);
  scheme_addto_prim_instance("ephemeron?",
			     scheme_make_folding_prim(ephemeronp,
						      "ephemeron?",
						      1, 1, 1),
			     env);
  scheme_addto_prim_instance("impersonator-ephemeron",
			     scheme_make_immed_prim(impersonator_ephemeron,
                                                    "impersonator-ephemeron",
                                                    1, 1),
			     env);

  scheme_addto_prim_instance("make-reader-graph",
			     scheme_make_prim_w_arity(make_graph,
						      "make-reader-graph",
						      1, 1),
			     env);
  scheme_addto_prim_instance("make-placeholder",
			     scheme_make_prim_w_arity(make_placeholder,
						      "make-placeholder",
						      1, 1),
			     env);
  scheme_addto_prim_instance("placeholder-get",
			     scheme_make_prim_w_arity(placeholder_get,
						      "placeholder-get",
						      1, 1),
			     env);
  scheme_addto_prim_instance("placeholder-set!",
			     scheme_make_prim_w_arity(placeholder_set,
						      "placeholder-set!",
						      2, 2),
			     env);
  scheme_addto_prim_instance("placeholder?",
			     scheme_make_folding_prim(placeholder_p,
						      "placeholder?",
						      1, 1, 1),
			     env);
  scheme_addto_prim_instance("make-hash-placeholder",
			     scheme_make_prim_w_arity(make_hash_placeholder,
						      "make-hash-placeholder",
						      1, 1),
			     env);
  scheme_addto_prim_instance("make-hasheq-placeholder",
			     scheme_make_prim_w_arity(make_hasheq_placeholder,
						      "make-hasheq-placeholder",
						      1, 1),
			     env);
  scheme_addto_prim_instance("make-hasheqv-placeholder",
			     scheme_make_prim_w_arity(make_hasheqv_placeholder,
						      "make-hasheqv-placeholder",
						      1, 1),
			     env);
  scheme_addto_prim_instance("hash-placeholder?",
			     scheme_make_folding_prim(table_placeholder_p,
						      "hash-placeholder?",
						      1, 1, 1),
			     env);

  REGISTER_SO(weak_symbol);
  REGISTER_SO(equal_symbol);

  weak_symbol = scheme_intern_symbol("weak");
  equal_symbol = scheme_intern_symbol("equal");

  REGISTER_SO(empty_hash);
  REGISTER_SO(empty_hasheq);
  REGISTER_SO(empty_hasheqv);
  empty_hash = scheme_make_hash_tree(1);
  empty_hasheq = scheme_make_hash_tree(0);
  empty_hasheqv = scheme_make_hash_tree(2);
}

void
scheme_init_unsafe_list (Scheme_Startup_Env *env)
{
  Scheme_Object *p;
  
  scheme_null->type = scheme_null_type;

  REGISTER_SO(scheme_unsafe_cons_list_proc);
  p = scheme_make_immed_prim(unsafe_cons_list, "unsafe-cons-list", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE_ALLOCATION);
  scheme_addto_prim_instance ("unsafe-cons-list", p, env);
  scheme_unsafe_cons_list_proc = p;

  REGISTER_SO(scheme_unsafe_car_proc);
  p = scheme_make_folding_prim(unsafe_car, "unsafe-car", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_IS_UNSAFE_NONALLOCATE
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("unsafe-car", p, env);
  scheme_unsafe_car_proc = p;

  REGISTER_SO(scheme_unsafe_cdr_proc);
  p = scheme_make_folding_prim(unsafe_cdr, "unsafe-cdr", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_IS_UNSAFE_NONALLOCATE
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("unsafe-cdr", p, env);
  scheme_unsafe_cdr_proc = p;

  p = scheme_make_immed_prim(unsafe_set_mcar, "unsafe-set-immutable-car!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance ("unsafe-set-immutable-car!", p, env);

  p = scheme_make_immed_prim(unsafe_set_mcdr, "unsafe-set-immutable-cdr!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance ("unsafe-set-immutable-cdr!", p, env);

  p = scheme_make_folding_prim(unsafe_list_ref, "unsafe-list-ref", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_IS_UNSAFE_NONALLOCATE);
  scheme_addto_prim_instance ("unsafe-list-ref", p, env);

  p = scheme_make_folding_prim(unsafe_list_tail, "unsafe-list-tail", 2, 2, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
                                                            | SCHEME_PRIM_IS_UNSAFE_NONALLOCATE);
  scheme_addto_prim_instance ("unsafe-list-tail", p, env);

  REGISTER_SO(scheme_unsafe_mcar_proc);
  p = scheme_make_immed_prim(unsafe_mcar, "unsafe-mcar", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("unsafe-mcar", p, env);
  scheme_unsafe_mcar_proc = p;

  REGISTER_SO(scheme_unsafe_mcdr_proc);
  p = scheme_make_immed_prim(unsafe_mcdr, "unsafe-mcdr", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance ("unsafe-mcdr", p, env);
  scheme_unsafe_mcdr_proc = p;

  p = scheme_make_immed_prim(unsafe_set_mcar, "unsafe-set-mcar!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance ("unsafe-set-mcar!", p, env);

  p = scheme_make_immed_prim(unsafe_set_mcdr, "unsafe-set-mcdr!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED);
  scheme_addto_prim_instance ("unsafe-set-mcdr!", p, env);
  
  REGISTER_SO(scheme_unsafe_unbox_proc);
  p = scheme_make_immed_prim(unsafe_unbox, "unsafe-unbox", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("unsafe-unbox", p, env);
  scheme_unsafe_unbox_proc = p;

  REGISTER_SO(scheme_unsafe_unbox_star_proc);
  p = scheme_make_immed_prim(unsafe_unbox_star, "unsafe-unbox*", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_UNSAFE_OMITABLE
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("unsafe-unbox*", p, env);
  scheme_unsafe_unbox_star_proc = p;

  p = scheme_make_immed_prim(unsafe_set_box, "unsafe-set-box!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("unsafe-set-box!", p, env);

  REGISTER_SO(scheme_unsafe_set_box_star_proc);
  p = scheme_make_immed_prim(unsafe_set_box_star, "unsafe-set-box*!", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("unsafe-set-box*!", p, env);
  scheme_unsafe_set_box_star_proc = p;

  p = scheme_make_prim_w_arity(scheme_box_cas, "unsafe-box*-cas!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED);
  scheme_addto_prim_instance("unsafe-box*-cas!", p, env);

}

void
scheme_init_unsafe_hash (Scheme_Startup_Env *env)
{
  Scheme_Object *p;

  /* unsafe-hash-iterate-first ---------------------------------------- */
  p = scheme_make_immed_prim(unsafe_hash_table_iterate_start,
			     "unsafe-mutable-hash-iterate-first", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance ("unsafe-mutable-hash-iterate-first", p, env);

  p = scheme_make_immed_prim(unsafe_hash_tree_iterate_start,
			     "unsafe-immutable-hash-iterate-first", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance ("unsafe-immutable-hash-iterate-first", p, env);

  p = scheme_make_immed_prim(unsafe_bucket_table_iterate_start,
			     "unsafe-weak-hash-iterate-first", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance ("unsafe-weak-hash-iterate-first", p, env);

  /* For the rest, only immutable variants can have
     SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL, because a key can disappear
     from mutable variants and trigger an error. */

  /* unsafe-hash-iterate-next ---------------------------------------- */
  p = scheme_make_immed_prim(unsafe_hash_table_iterate_next,
			     "unsafe-mutable-hash-iterate-next", 2, 2);
  scheme_addto_prim_instance ("unsafe-mutable-hash-iterate-next", p, env);

  p = scheme_make_immed_prim(unsafe_hash_tree_iterate_next,
			     "unsafe-immutable-hash-iterate-next", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance ("unsafe-immutable-hash-iterate-next", p, env);

  p = scheme_make_immed_prim(unsafe_bucket_table_iterate_next,
			     "unsafe-weak-hash-iterate-next", 2, 2);
  scheme_addto_prim_instance ("unsafe-weak-hash-iterate-next", p, env);

  /* unsafe-hash-iterate-key ---------------------------------------- */
  p = scheme_make_noncm_prim(unsafe_hash_table_iterate_key,
			     "unsafe-mutable-hash-iterate-key", 2, 3);
  scheme_addto_prim_instance ("unsafe-mutable-hash-iterate-key", p, env);
  
  p = scheme_make_noncm_prim(unsafe_hash_tree_iterate_key,
			     "unsafe-immutable-hash-iterate-key", 2, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
				 | SCHEME_PRIM_IS_UNSAFE_NONALLOCATE);
  scheme_addto_prim_instance ("unsafe-immutable-hash-iterate-key", p, env);
  
  p = scheme_make_noncm_prim(unsafe_bucket_table_iterate_key,
			     "unsafe-weak-hash-iterate-key", 2, 3);
  scheme_addto_prim_instance ("unsafe-weak-hash-iterate-key", p, env);

  /* unsafe-hash-iterate-value ---------------------------------------- */
  p = scheme_make_noncm_prim(unsafe_hash_table_iterate_value,
			     "unsafe-mutable-hash-iterate-value", 2, 3);
  scheme_addto_prim_instance ("unsafe-mutable-hash-iterate-value", p, env);

  p = scheme_make_noncm_prim(unsafe_hash_tree_iterate_value,
			     "unsafe-immutable-hash-iterate-value", 2, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
				 | SCHEME_PRIM_IS_UNSAFE_NONALLOCATE);
  scheme_addto_prim_instance ("unsafe-immutable-hash-iterate-value", p, env);

  p = scheme_make_noncm_prim(unsafe_bucket_table_iterate_value,
			     "unsafe-weak-hash-iterate-value", 2, 3);
  scheme_addto_prim_instance ("unsafe-weak-hash-iterate-value", p, env);

  /* unsafe-hash-iterate-key+value ---------------------------------------- */
  p = scheme_make_prim_w_arity2(unsafe_hash_table_iterate_key_value,
				"unsafe-mutable-hash-iterate-key+value", 
				2, 3, 2, 2);
  scheme_addto_prim_instance ("unsafe-mutable-hash-iterate-key+value", p, env);
  
  p = scheme_make_prim_w_arity2(unsafe_hash_tree_iterate_key_value,
				"unsafe-immutable-hash-iterate-key+value", 
				2, 3, 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL
				 | SCHEME_PRIM_IS_UNSAFE_NONALLOCATE);
  scheme_addto_prim_instance ("unsafe-immutable-hash-iterate-key+value", p, env);

  p = scheme_make_prim_w_arity2(unsafe_bucket_table_iterate_key_value,
				"unsafe-weak-hash-iterate-key+value", 
				2, 3, 2, 2);
  scheme_addto_prim_instance ("unsafe-weak-hash-iterate-key+value", p, env);

  /* unsafe-hash-iterate-pair ---------------------------------------- */
  p = scheme_make_immed_prim(unsafe_hash_table_iterate_pair,
			     "unsafe-mutable-hash-iterate-pair", 
			     2, 3);
  scheme_addto_prim_instance ("unsafe-mutable-hash-iterate-pair", p, env);
  
  p = scheme_make_immed_prim(unsafe_hash_tree_iterate_pair,
			     "unsafe-immutable-hash-iterate-pair", 
			     2, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |=
    scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNSAFE_FUNCTIONAL);
  scheme_addto_prim_instance ("unsafe-immutable-hash-iterate-pair", p, env);

  p = scheme_make_immed_prim(unsafe_bucket_table_iterate_pair,
			     "unsafe-weak-hash-iterate-pair", 
			     2, 3);
  scheme_addto_prim_instance ("unsafe-weak-hash-iterate-pair", p, env);
}

Scheme_Object *scheme_make_pair(Scheme_Object *car, Scheme_Object *cdr)
{
#ifdef MZ_PRECISE_GC
  return GC_malloc_pair(car, cdr);
#else
  Scheme_Object *cons;
  cons = scheme_alloc_object();
  cons->type = scheme_pair_type;
  SCHEME_CAR(cons) = car;
  SCHEME_CDR(cons) = cdr;
  return cons;
#endif
}

Scheme_Object *scheme_make_list_pair(Scheme_Object *car, Scheme_Object *cdr)
{
  GC_CAN_IGNORE Scheme_Object *r;
  r = scheme_make_pair(car, cdr);
  SCHEME_PAIR_FLAGS(r) |= PAIR_IS_LIST;
  return r;
}

Scheme_Object *scheme_make_mutable_pair(Scheme_Object *car, Scheme_Object *cdr)
{
  Scheme_Object *cons;
#ifdef MZ_PRECISE_GC
  cons = GC_malloc_pair(car, cdr);
#else
  cons = scheme_alloc_object();
  SCHEME_CAR(cons) = car;
  SCHEME_CDR(cons) = cdr;
#endif
  cons->type = scheme_mutable_pair_type;
  return cons;
}

Scheme_Object *scheme_make_raw_pair(Scheme_Object *car, Scheme_Object *cdr)
{
  Scheme_Object *cons;

  /* A raw pair is like a pair, but some of our low-level debugging
     tools expect pairs to always contain tagged values. A raw pair
     contains arbitrary pointers. */

#ifdef MZ_PRECISE_GC
  cons = GC_malloc_pair(car, cdr);
#else
  cons = scheme_alloc_object();
  SCHEME_CAR(cons) = car;
  SCHEME_CDR(cons) = cdr;
#endif

  cons->type = scheme_raw_pair_type;
  return cons;
}

# define cons(car, cdr) scheme_make_pair(car, cdr)
# define lcons(car, cdr) scheme_make_list_pair(car, cdr)

Scheme_Object *scheme_build_list(int size, Scheme_Object **argv)
{
  Scheme_Object *pair = scheme_null;
  int i;

  for (i = size; i--; ) {
    pair = lcons(argv[i], pair);
  }

  return pair;
}

Scheme_Object *scheme_build_list_offset(int size, Scheme_Object **argv, int delta)
/* if size < 0, clears originals in argv for space safety */
{
  Scheme_Object *pair = scheme_null;
  int i;

  if (size < 0) {
    /* clearing mode: */
    size = -size;
    for (i = size; i-- > delta; ) {
      pair = lcons(argv[i], pair);
      argv[i] = NULL;
    }
  } else {
    for (i = size; i-- > delta; ) {
      pair = lcons(argv[i], pair);
    }
  }

  return pair;
}

Scheme_Object *scheme_alloc_list(int size)
{
  Scheme_Object *pair = scheme_null;
  int i;

  for (i = size; i--; ) {
    pair = lcons(scheme_false, pair);
  }

  return pair;
}

int
scheme_list_length (Scheme_Object *list)
{
  int len;

  len = 0;
  while (!SCHEME_NULLP(list)) {
    len++;
    if (SCHEME_PAIRP(list))
      list = SCHEME_CDR(list);
    else
      list = scheme_null;
  }

  return len;
}

int
scheme_proper_list_length (Scheme_Object *list)
{
  int len;

  if (!scheme_is_list(list))
    return -1;

  len = 0;
  while (SCHEME_PAIRP(list)) {
    len++;
    list = SCHEME_CDR(list);
  }

  return len;
}

Scheme_Object *
scheme_named_map_1(char *name, Scheme_Object *(*fun)(Scheme_Object*, Scheme_Object*),
		   Scheme_Object *lst, Scheme_Object *form)
{
  Scheme_Object *first = scheme_null, *last = NULL, *pr;

  while (SCHEME_STX_PAIRP(lst)) {
    Scheme_Object *v;
    v = SCHEME_STX_CAR(lst);
    v = fun(v, form);
    pr = lcons(v, scheme_null);
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;

    lst = SCHEME_STX_CDR(lst);
  }

  if (!SCHEME_STX_NULLP(lst))
    scheme_wrong_syntax(name, lst, form, "bad syntax (" IMPROPER_LIST_FORM ")");

  return first;
}

Scheme_Object *
scheme_map_1 (Scheme_Object *(*fun)(Scheme_Object*), Scheme_Object *lst)
{
  return scheme_named_map_1("map",
			    (Scheme_Object *(*)(Scheme_Object *, Scheme_Object *))fun,
			    lst, NULL);
}

Scheme_Object *
scheme_car (Scheme_Object *pair)
{
  return (SCHEME_CAR (pair));
}

Scheme_Object *
scheme_cdr (Scheme_Object *pair)
{
  return (SCHEME_CDR (pair));
}

Scheme_Object *
scheme_cadr (Scheme_Object *pair)
{
  return (SCHEME_CAR (SCHEME_CDR (pair)));
}

Scheme_Object *
scheme_caddr (Scheme_Object *pair)
{
  return (SCHEME_CAR (SCHEME_CDR (SCHEME_CDR (pair))));
}

Scheme_Object *scheme_copy_list(Scheme_Object *l)
{
  return scheme_vector_to_list(scheme_list_to_vector(l));
}

/* local functions */

static Scheme_Object *
pair_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_PAIRP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
mpair_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_MUTABLE_PAIRP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
cons_prim (int argc, Scheme_Object *argv[])
{
  return cons(argv[0], argv[1]);
}

static Scheme_Object *
mcons_prim (int argc, Scheme_Object *argv[])
{
  return scheme_make_mutable_pair(argv[0], argv[1]);
}

Scheme_Object *
scheme_checked_car (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PAIRP(argv[0]))
    scheme_wrong_contract("car", "pair?", 0, argc, argv);
  return (SCHEME_CAR (argv[0]));
}

Scheme_Object *
scheme_checked_cdr (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PAIRP(argv[0]))
    scheme_wrong_contract("cdr", "pair?", 0, argc, argv);

  return (SCHEME_CDR (argv[0]));
}

Scheme_Object *
scheme_checked_mcar (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MPAIRP(argv[0]))
    scheme_wrong_contract("mcar", "mpair?", 0, argc, argv);
  return (SCHEME_MCAR (argv[0]));
}

Scheme_Object *
scheme_checked_mcdr (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MUTABLE_PAIRP(argv[0]))
    scheme_wrong_contract("mcdr", "mpair?", 0, argc, argv);

  return (SCHEME_MCDR (argv[0]));
}

Scheme_Object *
scheme_checked_set_mcar (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MPAIRP(argv[0]))
    scheme_wrong_contract("set-mcar!", "mpair?", 0, argc, argv);

  SCHEME_MCAR(argv[0]) = argv[1];
  return scheme_void;
}

Scheme_Object *
scheme_checked_set_mcdr (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_MPAIRP(argv[0]))
    scheme_wrong_contract("set-mcdr!", "mpair?", 0, argc, argv);

  SCHEME_MCDR(argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *
null_p_prim (int argc, Scheme_Object *argv[])
{
  return (SCHEME_NULLP(argv[0]) ? scheme_true : scheme_false);
}

int scheme_is_list(Scheme_Object *obj1)
{
  Scheme_Object *obj2;
  int flags;

  if (SCHEME_PAIRP(obj1)) {
    flags = SCHEME_PAIR_FLAGS(obj1);
    if (flags & PAIR_FLAG_MASK) {
      if (flags & PAIR_IS_LIST)
        return 1;
      else
        return 0;
    }
  } else if (SCHEME_NULLP(obj1))
    return 1;
  else
    return 0;

  obj2 = obj1;

  /* There's no fuel check in this loop. Checking a very long list
     could interfere with thread switching --- but only once, because
     another query on the same list will take half as long. */
  
  while (1) {
    obj1 = SCHEME_CDR(obj1);

    if (SCHEME_NULLP(obj1)){
      flags = PAIR_IS_LIST;
      break;
    }
    if (!SCHEME_PAIRP(obj1)) {
      flags = PAIR_IS_NON_LIST;
      break;
    }

    /* Known list or non-list? */
    flags = SCHEME_PAIR_FLAGS(obj1);
    if (flags & PAIR_FLAG_MASK)
      break;

    obj1 = SCHEME_CDR(obj1);

    if (SCHEME_NULLP(obj1)){
      flags = PAIR_IS_LIST;
      break;
    }
    if (!SCHEME_PAIRP(obj1)) {
      flags = PAIR_IS_NON_LIST;
      break;
    }

    /* Known list or non-list? */
    flags = SCHEME_PAIR_FLAGS(obj1);
    if (flags & PAIR_FLAG_MASK)
      break;

    obj2 = SCHEME_CDR(obj2);
  }

  /* Propagate info further up the chain. */
#ifdef MZ_USE_FUTURES
  {
    short orig_flags = flags & (~PAIR_FLAG_MASK);
    while (!mzrt_cas16(&SCHEME_PAIR_FLAGS(obj2), orig_flags, flags)) {
      orig_flags = SCHEME_PAIR_FLAGS(obj2);
      flags = orig_flags | (flags & PAIR_FLAG_MASK);
    }
  }
#else
  /* no fuel check, so flags could not have changed */
  SCHEME_PAIR_FLAGS(obj2) |= (flags & PAIR_FLAG_MASK);
#endif

  return (flags & PAIR_IS_LIST);
}

static Scheme_Object *
list_p_prim (int argc, Scheme_Object *argv[])
{
  return (scheme_is_list(argv[0])
          ? scheme_true
          : scheme_false);
}

#define NORMAL_LIST_INIT() l = scheme_null
#define STAR_LIST_INIT() --argc; l = argv[argc]

#define LIST_BODY(INIT, cons)                    \
  int i;                                         \
  GC_CAN_IGNORE Scheme_Object *l;                \
  INIT;                                          \
  for (i = argc ; i--; ) {                       \
    l = cons(argv[i], l);                        \
  }                                              \
  return l

static Scheme_Object *
list_prim (int argc, Scheme_Object *argv[])
{
  LIST_BODY(NORMAL_LIST_INIT(), lcons);
}

static Scheme_Object *
list_star_prim (int argc, Scheme_Object *argv[])
{
  LIST_BODY(STAR_LIST_INIT(), cons);
}

static Scheme_Object *
list_pair_p_prim (int argc, Scheme_Object *argv[])
{
  return ((SCHEME_PAIRP(argv[0])
           && scheme_is_list(argv[0]))
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *
immutablep (int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  return ((!SCHEME_INTP(v)
	   && ((SCHEME_IMMUTABLEP(v)
                && (SCHEME_VECTORP(v)
                    || SCHEME_BYTE_STRINGP(v)
                    || SCHEME_CHAR_STRINGP(v)
                    || SCHEME_BOXP(v)
                    || SCHEME_HASHTP(v)))
               || SCHEME_HASHTRP(v)
               || (SCHEME_NP_CHAPERONEP(v)
                   && (SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(v))
                       || ((SCHEME_VECTORP(SCHEME_CHAPERONE_VAL(v))
                            || SCHEME_BOXP(SCHEME_CHAPERONE_VAL(v)))
                           && SCHEME_IMMUTABLEP(SCHEME_CHAPERONE_VAL(v)))))))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *
length_prim (int argc, Scheme_Object *argv[])
{
  int l;

  if (!scheme_is_list(argv[0]))
    scheme_wrong_contract("length", "list?", 0, argc, argv);

  l = scheme_list_length(argv[0]);

  return scheme_make_integer(l);
}

Scheme_Object *scheme_checked_length(Scheme_Object *v)
{
  return length_prim(1, &v);
}

Scheme_Object *
scheme_append(Scheme_Object *l1, Scheme_Object *l2)
{
  Scheme_Object *first, *last, *orig1, *v;

  orig1 = l1;

  first = last = NULL;
  while (SCHEME_PAIRP(l1)) {
    v = cons(SCHEME_CAR(l1), scheme_null);
    if (!first)
      first = v;
    else
      SCHEME_CDR(last) = v;
    last = v;
    l1 = SCHEME_CDR(l1);

    SCHEME_USE_FUEL(1);
  }

  if (!SCHEME_NULLP(l1))
    scheme_wrong_contract("append", "list?", -1, 0, &orig1);

  if (!last)
    return l2;

  SCHEME_CDR(last) = l2;

  return first;
}

Scheme_Object *scheme_reverse(Scheme_Object *l)
{
  Scheme_Object *a[1];
  a[0] = l;
  return reverse_prim(1, a);
}

static Scheme_Object *
append_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *res;
  int i;

  if (!argc)
    return scheme_null;

  res = argv[argc - 1];
  for (i = argc - 1; i--;  ) {
    res = scheme_append(argv[i], res);
  }

  return res;
}

static Scheme_Object *
reverse_prim (int argc, Scheme_Object *argv[])
{
  Scheme_Object *lst, *last;

  last = scheme_null;
  lst = argv[0];
  while (!SCHEME_NULLP (lst)) {
    if (!SCHEME_PAIRP(lst))
      scheme_wrong_contract("reverse", "list?", 0, argc, argv);
    last = lcons(SCHEME_CAR (lst), last);
    lst = SCHEME_CDR (lst);

    SCHEME_USE_FUEL(1);
  }
  return (last);
}

static void past_end(const char *name, Scheme_Object *lst, Scheme_Object *index, Scheme_Object *orig_lst)
{
  scheme_contract_error(name,
                        (SCHEME_NULLP(lst) 
                         ? "index too large for list"
                         : "index reaches a non-pair"),
                        "index", 1, index,
                        "in", 1, orig_lst,
                        NULL);
}

#define OCCASIONAL_CHECK ((int)0xFF)
#ifdef PALMOS_STUFF
# define LISTREF_BIGNUM_SLICE 1000
#else
# define LISTREF_BIGNUM_SLICE 1000000
#endif

static Scheme_Object *
do_list_ref(char *name, int takecar, int argc, Scheme_Object *argv[])
{
  intptr_t i, k;
  Scheme_Object *lst, *index, *bnindex;

  lst = argv[0];
  index = argv[1];

  if (takecar && !SCHEME_PAIRP(lst)) {
    scheme_wrong_contract(name, "pair?", 0, argc, argv);;
  }

  if (SCHEME_BIGNUMP(index)) {
    bnindex = index;
    k = 0;
  } else if (!SCHEME_INTP(index)) {
    scheme_wrong_contract(name, "exact-nonnegative-integer?", 1, argc, argv);
    return NULL;
  } else {
    bnindex = NULL;
    k = SCHEME_INT_VAL(index);
  }

  if ((bnindex && !SCHEME_BIGPOS(bnindex))
      || (!bnindex && (k < 0))) {
    scheme_wrong_contract(name, "exact-nonnegative-integer?", 1, argc, argv);
    return NULL;
  }

  do {
    if (bnindex) {
      if (SCHEME_INTP(bnindex)) {
	k = SCHEME_INT_VAL(bnindex);
	bnindex = 0;
      } else {
	k = LISTREF_BIGNUM_SLICE;
	bnindex = scheme_bin_minus(bnindex, scheme_make_integer(LISTREF_BIGNUM_SLICE));
      }
    }

    for (i = 0; i < k; i++) {
      if (!SCHEME_PAIRP(lst)) {
        past_end(name, lst, index, argv[0]);
	return NULL;
      }
      lst = SCHEME_CDR(lst);
      if (!(i & OCCASIONAL_CHECK))
	SCHEME_USE_FUEL(OCCASIONAL_CHECK);
    }
  } while(bnindex);

  if (takecar) {
    if (!SCHEME_PAIRP(lst)) {
      past_end(name, lst, index, argv[0]);
      return NULL;
    }

    return SCHEME_CAR(lst);
  } else
    return lst;
}

Scheme_Object *
scheme_checked_list_tail(int argc, Scheme_Object *argv[])
{
  return do_list_ref("list-tail", 0, argc, argv);
}

Scheme_Object *
scheme_checked_list_ref(int argc, Scheme_Object *argv[])
{
  return do_list_ref("list-ref", 1, argc, argv);
}

static void mem_past_end(const char *name, Scheme_Object *s_arg, Scheme_Object *arg)
{
  scheme_contract_error(name,
                        "reached a non-pair",
                        "in", 1, arg,
                        "looking for", 1, s_arg,
                        NULL);
}

static void ass_non_pair(const char *name, Scheme_Object *np, Scheme_Object *s_arg, Scheme_Object *arg)
{
  scheme_contract_error(name,
                        "found a non-pair element",
                        "at", 1, np,
                        "in", 1, arg,
                        "looking for", 1, s_arg,
                        NULL);
}

#define GEN_ASS(name, scheme_name, comp) \
static Scheme_Object * \
name (int argc, Scheme_Object *argv[]) \
{ \
  Scheme_Object *pair, *list, *turtle;			\
  list = turtle = argv[1]; \
  while (SCHEME_PAIRP (list)) \
    { \
      pair = SCHEME_CAR (list); \
      if (!SCHEME_PAIRP (pair)) {\
        ass_non_pair(#scheme_name, pair, argv[0], argv[1]); \
      } \
      if (comp (argv[0], SCHEME_CAR (pair))) \
	{ \
          return (pair); \
	} \
      list = SCHEME_CDR (list); \
      if (SCHEME_PAIRP(list)) { \
        pair = SCHEME_CAR (list); \
        if (SCHEME_PAIRP(pair)) { \
          if (comp (argv[0], SCHEME_CAR (pair))) \
	    return pair; \
          list = SCHEME_CDR (list); \
          if (SAME_OBJ(list, turtle)) break; \
          turtle = SCHEME_CDR (turtle); \
          SCHEME_USE_FUEL(1); \
        } \
      } \
    } \
  if (!SCHEME_NULLP(list)) {\
    mem_past_end(#scheme_name, argv[0], argv[1]); \
  } \
  return (scheme_false); \
}

GEN_ASS(assv, assv, scheme_eqv)
GEN_ASS(assq, assq, SAME_OBJ)
GEN_ASS(assoc, assoc, scheme_equal)

#define LISTFUNC2(name, C, D, pred)                  \
Scheme_Object * \
scheme_checked_ ## name (int argc, Scheme_Object *argv[]) \
{ \
  if (!(SCHEME_PAIRP(argv[0]) \
	&& SCHEME_PAIRP(D(argv[0])))) \
      scheme_wrong_contract(#name, pred, 0, argc, argv); \
  return C(D(argv[0])); \
}

LISTFUNC2(cddr, SCHEME_CDR, SCHEME_CDR, "(cons/c any/c pair?)")
LISTFUNC2(cadr, SCHEME_CAR, SCHEME_CDR, "(cons/c any/c pair?)")
LISTFUNC2(cdar, SCHEME_CDR, SCHEME_CAR, "(cons/c pair? any/c)")
LISTFUNC2(caar, SCHEME_CAR, SCHEME_CAR, "(cons/c pair? any/c)")

#define LISTFUNC3(name, B, C, D, pred)               \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!((SCHEME_PAIRP(argv[0])) \
	&& SCHEME_PAIRP(D(argv[0])) \
	&& SCHEME_PAIRP(C(D(argv[0]))))) \
    scheme_wrong_contract(#name, pred, 0, argc, argv); \
  return B (C (D (argv[0]))); \
}

LISTFUNC3(cdddr, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR, "(cons/c any/c (cons/c any/c pair?))")

LISTFUNC3(caddr, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR, "(cons/c any/c (cons/c any/c pair?))")
LISTFUNC3(cdadr, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR, "(cons/c any/c (cons/c pair? any/c))")
LISTFUNC3(cddar, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR, "(cons/c (cons/c any/c pair?) any/c)")

LISTFUNC3(cdaar, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR, "(cons/c (cons/c pair? any/c) any/c)")
LISTFUNC3(cadar, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR, "(cons/c (cons/c any/c pair?) any/c)")
LISTFUNC3(caadr, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR, "(cons/c any/c (cons/c pair? any/c))")

LISTFUNC3(caaar, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR, "(cons/c (cons/c pair? any/c) any/c)")


#define LISTFUNC4(name, A, B, C, D, pred)            \
static Scheme_Object * \
name ## _prim (int argc, Scheme_Object *argv[]) \
{ \
  if (!(SCHEME_PAIRP(argv[0]) \
	&& SCHEME_PAIRP(D (argv[0])) \
	&& SCHEME_PAIRP(C(D(argv[0]))) \
	&&SCHEME_PAIRP(B(C(D(argv[0]))))))\
    scheme_wrong_contract(#name, pred, 0, argc, argv); \
  return A(B(C(D(argv[0]))));\
}

LISTFUNC4(cddddr, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR, "(cons/c any/c (cons/c any/c (cons/c any/c pair?)))")

LISTFUNC4(cadddr, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR, "(cons/c any/c (cons/c any/c (cons/c any/c pair?)))")
LISTFUNC4(cdaddr, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR, "(cons/c any/c (cons/c any/c (cons/c pair? any/c)))")
LISTFUNC4(cddadr, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR, "(cons/c any/c (cons/c (cons/c any/c pair?) any/c))")
LISTFUNC4(cdddar, SCHEME_CDR, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR, "(cons/c (cons/c any/c (cons/c any/c pair?)) any/c)")

LISTFUNC4(caaddr, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR, "(cons/c any/c (cons/c any/c (cons/c pair? any/c)))")
LISTFUNC4(cadadr, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR, "(cons/c any/c (cons/c (cons/c any/c pair?) any/c))")
LISTFUNC4(caddar, SCHEME_CAR, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR, "(cons/c (cons/c any/c (cons/c any/c pair?)) any/c)")
LISTFUNC4(cdaadr, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR, "(cons/c any/c (cons/c (cons/c pair? any/c) any/c))")
LISTFUNC4(cdadar, SCHEME_CDR, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR, "(cons/c (cons/c any/c (cons/c pair? any/c)) any/c)")
LISTFUNC4(cddaar, SCHEME_CDR, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR, "(cons/c (cons/c (cons/c any/c pair?) any/c) any/c)")

LISTFUNC4(cdaaar, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR, "(cons/c (cons/c (cons/c pair? any/c) any/c) any/c)")
LISTFUNC4(cadaar, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR, SCHEME_CAR, "(cons/c (cons/c (cons/c any/c pair?) any/c) any/c)")
LISTFUNC4(caadar, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR, SCHEME_CAR, "(cons/c (cons/c any/c (cons/c pair? any/c)) any/c)")
LISTFUNC4(caaadr, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR, SCHEME_CDR, "(cons/c any/c (cons/c (cons/c pair? any/c) any/c))")

LISTFUNC4(caaaar, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR, SCHEME_CAR, "(cons/c (cons/c (cons/c pair? any/c) any/c) any/c)")

Scheme_Object *scheme_box(Scheme_Object *v)
{
  Scheme_Object *obj;

  obj = scheme_alloc_small_object();
  obj->type = scheme_box_type;
  SCHEME_BOX_VAL(obj) = v;

  return obj;
}

static Scheme_Object *chaperone_unbox_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return scheme_unbox(o);
}

static Scheme_Object *chaperone_unbox_overflow(Scheme_Object *o)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = (void *)o;

  return scheme_handle_stack_overflow(chaperone_unbox_k);
}

static Scheme_Object *chaperone_unbox(Scheme_Object *obj)
{
  Scheme_Chaperone *px = (Scheme_Chaperone *)obj;
  Scheme_Object *a[2], *orig;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    return chaperone_unbox_overflow(obj);
  }
#endif

  orig = scheme_unbox(px->prev);

  if (SCHEME_VECTORP(px->redirects)) {
    /* chaperone was on property accessors */
    return orig;
  }

  a[0] = px->prev;
  a[1] = orig;
  obj = _scheme_apply(SCHEME_CAR(px->redirects), 2, a);

  if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
    if (!scheme_chaperone_of(obj, orig))
      scheme_wrong_chaperoned("unbox", "result", orig, obj);

  return obj;
}

Scheme_Object *scheme_unbox(Scheme_Object *obj)
{
  if (!SCHEME_BOXP(obj)) {
    if (SCHEME_NP_CHAPERONEP(obj)
        && SCHEME_BOXP(SCHEME_CHAPERONE_VAL(obj)))
      return chaperone_unbox(obj);

    scheme_wrong_contract("unbox", "box?", 0, 1, &obj);
  }

  return SCHEME_BOX_VAL(obj);
}

Scheme_Object *scheme_unbox_star(Scheme_Object *obj)
{
  if (!SCHEME_BOXP(obj))
    scheme_wrong_contract("unbox*", "(and/c box? (not/c impersonator?))", 0, 1, &obj);

  return SCHEME_BOX_VAL(obj);
}

static void bad_cas_box(Scheme_Object *box)
{
  scheme_wrong_contract("box-cas!", "(and/c box? (not/c immutable?) (not/c impersonator?))", 0, 1, &box);
}

Scheme_Object *scheme_box_cas(int argc, Scheme_Object *argv[])
XFORM_SKIP_PROC
{
  Scheme_Object *box = argv[0];
  Scheme_Object *ov = argv[1];
  Scheme_Object *nv = argv[2];

  /* This procedure is used for both the safe and unsafe version, but
     the JIT elides the checking for the unsafe version. */

  if (!SCHEME_MUTABLE_BOXP(box)) {
    bad_cas_box(box);
    return NULL;
  }

#ifdef MZ_USE_FUTURES
  return mzrt_cas((volatile uintptr_t *)(&SCHEME_BOX_VAL(box)),
                  (uintptr_t)ov, (uintptr_t)nv)
    ? scheme_true : scheme_false;
#else
  /* For cooperative threading, no atomicity required */
  if (SCHEME_BOX_VAL(box) == ov) {
    SCHEME_BOX_VAL(box) = nv;
    return scheme_true;
  } else {
    return scheme_false;
  }
#endif
}

static void chaperone_set_box(Scheme_Object *obj, Scheme_Object *v)
{
  Scheme_Chaperone *px;
  Scheme_Object *a[2];

  while (1) {
    if (SCHEME_BOXP(obj)) {
      SCHEME_BOX_VAL(obj) = v;
      return;
    } else {
      px = (Scheme_Chaperone *)obj;
      
      obj = px->prev;
      a[0] = obj;
      a[1] = v;
      v = _scheme_apply(SCHEME_CDR(px->redirects), 2, a);

      if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
        if (!scheme_chaperone_of(v, a[1]))
          scheme_wrong_chaperoned("set-box!", "value", a[1], v);
    }
  }
}

void scheme_set_box(Scheme_Object *b, Scheme_Object *v)
{
  if (!SCHEME_MUTABLE_BOXP(b)) {
    if (SCHEME_NP_CHAPERONEP(b)
        && SCHEME_MUTABLE_BOXP(SCHEME_CHAPERONE_VAL(b))) {
      chaperone_set_box(b, v);
      return;
    }

    scheme_wrong_contract("set-box!", "(and/c box? (not/c immutable?))", 0, 1, &b);
  }
  SCHEME_BOX_VAL(b) = v;
}

void scheme_set_box_star(Scheme_Object *b, Scheme_Object *v)
{
  if (!SCHEME_MUTABLE_BOXP(b))
    scheme_wrong_contract("set-box*!", "(and/c box? (not/c immutable?) (not/c impersonator?))", 0, 1, &b);

  SCHEME_BOX_VAL(b) = v;
}

static Scheme_Object *box(int c, Scheme_Object *p[])
{
  return scheme_box(p[0]);
}

static Scheme_Object *immutable_box(int c, Scheme_Object *p[])
{
  Scheme_Object *obj;

  obj = scheme_box(p[0]);
  SCHEME_SET_IMMUTABLE(obj);

  return obj;
}

static Scheme_Object *box_p(int c, Scheme_Object *p[])
{
  return SCHEME_CHAPERONE_BOXP(p[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *unbox(int c, Scheme_Object *p[])
{
  return scheme_unbox(p[0]);
}

static Scheme_Object *unbox_star(int c, Scheme_Object *p[])
{
  return scheme_unbox_star(p[0]);
}

static Scheme_Object *set_box(int c, Scheme_Object *p[])
{
  scheme_set_box(p[0], p[1]);
  return scheme_void;
}

static Scheme_Object *set_box_star(int c, Scheme_Object *p[])
{
  scheme_set_box_star(p[0], p[1]);
  return scheme_void;
}

static Scheme_Object *do_chaperone_box(const char *name, int is_impersonator, int argc, Scheme_Object **argv)
{
  Scheme_Chaperone *px;
  Scheme_Object *val = argv[0];
  Scheme_Object *redirects;
  Scheme_Object *props;

  if (SCHEME_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!SCHEME_BOXP(val) || (is_impersonator && !SCHEME_MUTABLEP(val)))
    scheme_wrong_contract(name, is_impersonator ? "(and/c box? (not/c immutable?))" : "box?", 0, argc, argv);
  scheme_check_proc_arity(name, 2, 1, argc, argv);
  scheme_check_proc_arity(name, 2, 2, argc, argv);

  redirects = scheme_make_pair(argv[1], argv[2]);
  
  props = scheme_parse_chaperone_props(name, 3, argc, argv);

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  px->redirects = redirects;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_box(int argc, Scheme_Object **argv)
{
  return do_chaperone_box("chaperone-box", 0, argc, argv);
}

static Scheme_Object *impersonate_box(int argc, Scheme_Object **argv)
{
  return do_chaperone_box("impersonate-box", 1, argc, argv);
}

int scheme_compare_equal(void *v1, void *v2)
{
  return !scheme_equal((Scheme_Object *)v1, (Scheme_Object *)v2);
}

static void make_hash_indices_for_equal(void *v, intptr_t *_stk_h1, intptr_t *_stk_h2)
{
  if (_stk_h1)
    *_stk_h1 = scheme_equal_hash_key((Scheme_Object *)v);
  if (_stk_h2)
    *_stk_h2 = scheme_equal_hash_key2((Scheme_Object *)v);
}

static int compare_eqv(void *v1, void *v2)
{
  return !scheme_eqv((Scheme_Object *)v1, (Scheme_Object *)v2);
}

static void make_hash_indices_for_eqv(void *v, intptr_t *_stk_h1, intptr_t *_stk_h2)
{
  if (_stk_h1)
    *_stk_h1 = scheme_eqv_hash_key((Scheme_Object *)v);
  if (_stk_h2)
    *_stk_h2 = scheme_eqv_hash_key2((Scheme_Object *)v);
}

Scheme_Bucket_Table *scheme_make_weak_equal_table(void)
{
  Scheme_Object *sema;
  Scheme_Bucket_Table *t;
  
  t = scheme_make_bucket_table(20, SCHEME_hash_weak_ptr);
  
  sema = scheme_make_sema(1);
  t->mutex = sema;
  t->compare = scheme_compare_equal;
  t->make_hash_indices = make_hash_indices_for_equal;

  return t;
}

Scheme_Bucket_Table *scheme_make_nonlock_equal_bucket_table(void)
{
  Scheme_Bucket_Table *t;
  
  t = scheme_make_bucket_table(20, SCHEME_hash_ptr);
  
  t->compare = scheme_compare_equal;
  t->make_hash_indices = make_hash_indices_for_equal;

  return t;
}

Scheme_Bucket_Table *scheme_make_weak_eqv_table(void)
{
  Scheme_Object *sema;
  Scheme_Bucket_Table *t;
  
  t = scheme_make_bucket_table(20, SCHEME_hash_weak_ptr);
  
  sema = scheme_make_sema(1);
  t->mutex = sema;
  t->compare = compare_eqv;
  t->make_hash_indices = make_hash_indices_for_eqv;

  return t;
}

static Scheme_Object *fill_table(Scheme_Object *ht, const char *who,
                                 int argc, Scheme_Object **argv)
{
  Scheme_Object *l, *a, *args[3];

  if (argc) {
    l = argv[0];
    if (scheme_proper_list_length(l) >= 0) {
      for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
        a = SCHEME_CAR(l);
        if (!SCHEME_PAIRP(a))
          break;
      }
    }
    
    if (!SCHEME_NULLP(l))
      scheme_wrong_contract(who, "(listof pair?)", 0, argc, argv);
    
    args[0] = ht;

    for (l = argv[0]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      args[1] = SCHEME_CAR(a);
      args[2] = SCHEME_CDR(a);
      hash_table_put_bang(3, args);
    }
  }

  return ht;
}

static Scheme_Object *make_hash(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ht;
  ht = (Scheme_Object *)scheme_make_hash_table_equal();
  return fill_table(ht, "make-hash", argc, argv);
}

static Scheme_Object *make_hasheq(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ht;
  ht = (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
  return fill_table(ht, "make-hasheq", argc, argv);
}

static Scheme_Object *make_hasheqv(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ht;
  ht = (Scheme_Object *)scheme_make_hash_table_eqv();
  return fill_table(ht, "make-hasheqv", argc, argv);
}

static Scheme_Object *make_weak_hash(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ht;
  ht = (Scheme_Object *)scheme_make_weak_equal_table();
  return fill_table(ht, "make-weak-hash", argc, argv);
}

static Scheme_Object *make_weak_hasheq(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ht;
  ht = (Scheme_Object *)scheme_make_bucket_table(20, SCHEME_hash_weak_ptr);
  return fill_table(ht, "make-weak-hasheq", argc, argv);
}

static Scheme_Object *make_weak_hasheqv(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ht;
  ht = (Scheme_Object *)scheme_make_weak_eqv_table();
  return fill_table(ht, "make-weak-hasheqv", argc, argv);
}

static Scheme_Object *make_immutable_table(const char *who, int kind, int argc, Scheme_Object *argv[])
{
  Scheme_Object *l = (argc ? argv[0] : scheme_null), *a;
  Scheme_Hash_Tree *ht;

  if (scheme_proper_list_length(l) >= 0) {
    for (; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
      a = SCHEME_CAR(l);
      if (!SCHEME_PAIRP(a))
	break;
    }
  }

  if (!SCHEME_NULLP(l))
    scheme_wrong_contract(who, "(listof pair?)", 0, argc, argv);

  ht = scheme_make_hash_tree(kind);

  for (l = (argc ? argv[0] : scheme_null); SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    ht = scheme_hash_tree_set(ht, SCHEME_CAR(a), SCHEME_CDR(a));
  }

  return (Scheme_Object *)ht;
}

Scheme_Object *scheme_make_immutable_hash(int argc, Scheme_Object *argv[])
{
  return make_immutable_table("make-immutable-hash", 1, argc, argv);
}

Scheme_Object *scheme_make_immutable_hasheq(int argc, Scheme_Object *argv[])
{
  return make_immutable_table("make-immutable-hasheq", 0, argc, argv);
}

Scheme_Object *scheme_make_immutable_hasheqv(int argc, Scheme_Object *argv[])
{
  return make_immutable_table("make-immutable-hasheqv", 2, argc, argv);
}

static Scheme_Object *direct_table(const char *who, int kind, Scheme_Hash_Tree *empty, int argc, Scheme_Object *argv[])
{
  int i;
  Scheme_Hash_Tree *ht;

  if (argc & 0x1) {
    scheme_contract_error(who,
                          "key does not have a value (i.e., an odd number of arguments were provided)",
                          "key", 1, argv[argc-1],
                          NULL);
    return NULL;
  }

  if (!argc)
    ht = scheme_make_hash_tree(kind);
  else
    ht = empty;

  for (i = 0; i < argc; i += 2) {
    ht = scheme_hash_tree_set(ht, argv[i], argv[i+1]);
  }

  return (Scheme_Object *)ht;
}

static Scheme_Object *direct_hash(int argc, Scheme_Object *argv[])
{
  return direct_table("hash", 1, empty_hash, argc, argv);
}

static Scheme_Object *direct_hasheq(int argc, Scheme_Object *argv[])
{
  return direct_table("hasheq", 0, empty_hasheq, argc, argv);
}

static Scheme_Object *direct_hasheqv(int argc, Scheme_Object *argv[])
{
  return direct_table("hasheqv", 2, empty_hasheqv, argc, argv);
}

Scheme_Hash_Table *scheme_make_hash_table_equal()
{
  Scheme_Hash_Table *t;
  Scheme_Object *sema;

  t = scheme_make_hash_table(SCHEME_hash_ptr);

  sema = scheme_make_sema(1);
  t->mutex = sema;
  t->compare = scheme_compare_equal;
  t->make_hash_indices = make_hash_indices_for_equal;

  return t;
}

Scheme_Hash_Table *scheme_make_hash_table_eqv()
{
  Scheme_Hash_Table *t;

  t = scheme_make_hash_table(SCHEME_hash_ptr);

  t->compare = compare_eqv;
  t->make_hash_indices = make_hash_indices_for_eqv;

  return t;
}

Scheme_Object *scheme_checked_hash_count(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_CHAPERONEP(v)) 
    v = SCHEME_CHAPERONE_VAL(v);

  if (SCHEME_HASHTP(v)) {
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)v;
    return scheme_make_integer(t->count);
  } else if (SCHEME_HASHTRP(v)) {
    Scheme_Hash_Tree *t = (Scheme_Hash_Tree *)v;
    return scheme_make_integer(t->count);
  } else if (SCHEME_BUCKTP(v)) {
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)v;
    int count = 0, weak, i;
    Scheme_Bucket **buckets, *bucket;
    const char *key;

    if (t->mutex) scheme_wait_sema(t->mutex,0);

    buckets = t->buckets;
    weak = t->weak;

    for (i = t->size; i--; ) {
      bucket = buckets[i];
      if (bucket) {
	if (weak) {
	  key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	} else {
	  key = bucket->key;
	}
	if (key)
	  count++;
      }
      SCHEME_USE_FUEL(1);
    }

    if (t->mutex) scheme_post_sema(t->mutex);

    return scheme_make_integer(count);
  } else {
    scheme_wrong_contract("hash-count", "hash?", 0, argc, argv);
    return NULL;
  }
}

static Scheme_Object *hash_table_copy(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_NP_CHAPERONEP(v) && (SCHEME_HASHTP(SCHEME_CHAPERONE_VAL(v))
                                  || SCHEME_BUCKTP(SCHEME_CHAPERONE_VAL(v))))
    return scheme_chaperone_hash_table_copy(v);

  if (SCHEME_HASHTP(v)) {
    Scheme_Object *o;
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex,0);
    o = (Scheme_Object *)scheme_clone_hash_table(t);
    if (t->mutex) scheme_post_sema(t->mutex);
    return o;
  } else if (SCHEME_BUCKTP(v)) {
    Scheme_Object *o;
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex,0);
    o = (Scheme_Object *)scheme_clone_bucket_table(t);
    if (t->mutex) scheme_post_sema(t->mutex);
    return o;
  } else if (SCHEME_HASHTRP(v)) {
    return scheme_hash_tree_copy(v);
  } else {
    scheme_wrong_contract("hash-copy", "hash?", 0, argc, argv);
    return NULL;
  }
}

Scheme_Object *scheme_hash_tree_copy(Scheme_Object *v)
{
  Scheme_Hash_Tree *t;
  Scheme_Hash_Table *naya;
  mzlonglong i;
  Scheme_Object *k, *val;

  if (SCHEME_NP_CHAPERONEP(v))
    t = (Scheme_Hash_Tree *)SCHEME_CHAPERONE_VAL(v);
  else
    t = (Scheme_Hash_Tree *)v;

  if (scheme_is_hash_tree_equal((Scheme_Object *)t))
    naya = scheme_make_hash_table_equal();
  else if (scheme_is_hash_tree_eqv((Scheme_Object *)t))
    naya = scheme_make_hash_table_eqv();
  else
    naya = scheme_make_hash_table(SCHEME_hash_ptr);

  for (i = scheme_hash_tree_next(t, -1); i != -1; i = scheme_hash_tree_next(t, i)) {
    scheme_hash_tree_index(t, i, &k, &val);
    if (!SAME_OBJ((Scheme_Object *)t, v))
      val = scheme_chaperone_hash_traversal_get(v, k, &k);
    if (val)
      scheme_hash_set(naya, k, val);
  }

  return (Scheme_Object *)naya;
}

static Scheme_Object *hash_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_CHAPERONEP(o)) 
    o = SCHEME_CHAPERONE_VAL(o);

  if (SCHEME_HASHTP(o) || SCHEME_HASHTRP(o) || SCHEME_BUCKTP(o))
    return scheme_true;
  else
    return scheme_false;
}

Scheme_Object *scheme_hash_eq_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_CHAPERONEP(o)) 
    o = SCHEME_CHAPERONE_VAL(o);

  if (SCHEME_HASHTP(o)) {
    if ((((Scheme_Hash_Table *)o)->compare != scheme_compare_equal)
        && (((Scheme_Hash_Table *)o)->compare != compare_eqv))
      return scheme_true;
  } else if (SCHEME_HASHTRP(o)) {
    if (SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(o)))
      return scheme_true;
  } else if (SCHEME_BUCKTP(o)) {
    if ((((Scheme_Bucket_Table *)o)->compare != scheme_compare_equal)
        && (((Scheme_Bucket_Table *)o)->compare != compare_eqv))
      return scheme_true;
  } else {
    scheme_wrong_contract("hash-eq?", "hash?", 0, argc, argv);
  }
  
  return scheme_false;
}

Scheme_Object *scheme_hash_eqv_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_CHAPERONEP(o)) 
    o = SCHEME_CHAPERONE_VAL(o);

  if (SCHEME_HASHTP(o)) {
    if (((Scheme_Hash_Table *)o)->compare == compare_eqv)
      return scheme_true;
  } else if (SCHEME_HASHTRP(o)) {
    if (SAME_TYPE(scheme_eqv_hash_tree_type, SCHEME_HASHTR_TYPE(o)))
      return scheme_true;
  } else if (SCHEME_BUCKTP(o)) {
    if (((Scheme_Bucket_Table *)o)->compare == compare_eqv)
      return scheme_true;
  } else {
    scheme_wrong_contract("hash-eqv?", "hash?", 0, argc, argv);
  }
  
  return scheme_false;
}

Scheme_Object *scheme_hash_equal_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_CHAPERONEP(o)) 
    o = SCHEME_CHAPERONE_VAL(o);

  if (SCHEME_HASHTP(o)) {
    if (((Scheme_Hash_Table *)o)->compare == scheme_compare_equal)
      return scheme_true;
  } else if (SCHEME_HASHTRP(o)) {
    if (SAME_TYPE(scheme_hash_tree_type, SCHEME_HASHTR_TYPE(o)))
      return scheme_true;
  } else if (SCHEME_BUCKTP(o)) {
    if (((Scheme_Bucket_Table *)o)->compare == scheme_compare_equal)
      return scheme_true;
  } else {
    scheme_wrong_contract("hash-equal?", "hash?", 0, argc, argv);
  }
  
  return scheme_false;
}

static Scheme_Object *hash_weak_p(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_CHAPERONEP(o)) 
    o = SCHEME_CHAPERONE_VAL(o);

  if (SCHEME_BUCKTP(o))
    return scheme_true;
  else if (SCHEME_HASHTP(o) || SCHEME_HASHTRP(o))
    return scheme_false;
  
  scheme_wrong_contract("hash-weak?", "hash?", 0, argc, argv);
   
  return NULL;
}

int scheme_is_hash_table_equal(Scheme_Object *o)
{
  return (((Scheme_Hash_Table *)o)->compare == scheme_compare_equal);
}

int scheme_is_hash_table_eqv(Scheme_Object *o)
{
  return (((Scheme_Hash_Table *)o)->compare == compare_eqv);
}

int scheme_is_hash_tree_equal(Scheme_Object *o)
{
  return SAME_TYPE(scheme_hash_tree_type, SCHEME_HASHTR_TYPE(o));
}

int scheme_is_hash_tree_eqv(Scheme_Object *o)
{
  return SAME_TYPE(scheme_eqv_hash_tree_type, SCHEME_HASHTR_TYPE(o));
}

static Scheme_Object *hash_table_put_bang(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_NP_CHAPERONEP(v) && (SCHEME_HASHTP(SCHEME_CHAPERONE_VAL(v))
                                  || SCHEME_BUCKTP(SCHEME_CHAPERONE_VAL(v))))
    scheme_chaperone_hash_set(v, argv[1], argv[2]);
  else if (SCHEME_BUCKTP(v)) {
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex,0);
    scheme_add_to_table(t, (char *)argv[1], (void *)argv[2], 0);
    if (t->mutex) scheme_post_sema(t->mutex);
  } else if (!SCHEME_HASHTP(v) || !SCHEME_MUTABLEP(v)) {
    scheme_wrong_contract("hash-set!", "(and/c hash? (not/c immutable?))", 0, argc, argv);
  } else if (((Scheme_Hash_Table *)v)->mutex) {
    scheme_wait_sema(((Scheme_Hash_Table *)v)->mutex, 0);
    scheme_hash_set((Scheme_Hash_Table *)v, argv[1], argv[2]);
    scheme_post_sema(((Scheme_Hash_Table *)v)->mutex);
  } else {
    scheme_hash_set((Scheme_Hash_Table *)v, argv[1], argv[2]);
  }

  return scheme_void;
}

Scheme_Object *scheme_hash_table_put(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_NP_CHAPERONEP(v) && SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(v)))
    return chaperone_hash_tree_set(v, argv[1], argv[2]);

  if (!SCHEME_HASHTRP(v)) {
    scheme_wrong_contract("hash-set", "(and hash? immutable?)", 0, argc, argv);
    return NULL;
  }

  return (Scheme_Object *)scheme_hash_tree_set((Scheme_Hash_Tree *)v, argv[1], argv[2]);
}

static Scheme_Object *hash_failed(const char *who, const char *contract_error_message,
                                  int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  if (argc == 3) {
    v = argv[2];
    if (SCHEME_PROCP(v)) {
      if (!scheme_check_proc_arity(NULL, 0, 2, argc, argv)) {
        scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
                         "%s: arity mismatch for failure procedure;\n"
                         " given procedure does not accept zero arguments\n"
                         "  procedure: %V",
                         who, v);
        return NULL;
      }
      return _scheme_tail_apply(v, 0, NULL);
    } else
      return v;
  } else {
    scheme_contract_error(who,
                          contract_error_message,
                          "key", 1, argv[1],
                          NULL);
    return scheme_void;
  }
}

static Scheme_Object *hash_ref_failed(int argc, Scheme_Object *argv[])
{
  return hash_failed("hash-ref", "no value found for key", argc, argv);
}

static int scheme_chaperoned_hashp(Scheme_Object *v)
{
  return (SCHEME_NP_CHAPERONEP(v) && (SCHEME_HASHTP(SCHEME_CHAPERONE_VAL(v))
                                      || SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(v))
                                      || SCHEME_BUCKTP(SCHEME_CHAPERONE_VAL(v))));
}

static Scheme_Object *gen_hash_table_get(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = argv[0];
  
  if (SCHEME_HASHTP(v)) {
    if (((Scheme_Hash_Table *)v)->mutex) {
      Scheme_Hash_Table *t = (Scheme_Hash_Table *)v;
      scheme_wait_sema(t->mutex, 0);
      v = scheme_hash_get(t, argv[1]);
      scheme_post_sema(t->mutex);
    } else {
      v = scheme_hash_get((Scheme_Hash_Table *)v, argv[1]);
    }
  } else if (SCHEME_HASHTRP(v)) {
    v = scheme_hash_tree_get((Scheme_Hash_Tree *)v, argv[1]);
  } else if (scheme_chaperoned_hashp(v))
    v = scheme_chaperone_hash_get(v, argv[1]);
  else if (SCHEME_BUCKTP(v)) {
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    v = (Scheme_Object *)scheme_lookup_in_table(t, (char *)argv[1]);
    if (t->mutex) scheme_post_sema(t->mutex);
  } else {
    scheme_wrong_contract("hash-ref", "hash?", 0, argc, argv);
    return NULL;
  }

  if (v)
    return v;
  else 
    return hash_ref_failed(argc, argv);
}

Scheme_Object *scheme_checked_hash_ref(int argc, Scheme_Object *argv[]) XFORM_ASSERT_NO_CONVERSION
{
  Scheme_Object *v;

  /* fast path is designed to avoid need for XFORM */
  v = argv[0];
  if (SCHEME_HASHTP(v)) {
    if (!((Scheme_Hash_Table *)v)->make_hash_indices) {
      v = scheme_eq_hash_get((Scheme_Hash_Table *)v, argv[1]);
      if (v)
        return v;
      else
        return hash_ref_failed(argc, argv);
    }
  } else if (SCHEME_HASHTRP(v)) {
    if (SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(v))) {
      v = scheme_eq_hash_tree_get((Scheme_Hash_Tree *)v, argv[1]);
      if (v)
        return v;
      else
        return hash_ref_failed(argc, argv);
    }
  }

  return gen_hash_table_get(argc, argv);
}

Scheme_Object *hash_table_ref_key(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_HASHTP(v)) {
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    v = scheme_hash_get_key(t, argv[1]);
    if (t->mutex) scheme_post_sema(t->mutex);
  } else if (SCHEME_HASHTRP(v)) {
    v = scheme_hash_tree_get_key((Scheme_Hash_Tree *)v, argv[1]);
  } else if (scheme_chaperoned_hashp(v)) {
    v = scheme_chaperone_hash_get_key(v, argv[1]);
  } else if (SCHEME_BUCKTP(v)) {
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    v = (Scheme_Object *)scheme_lookup_key_in_table(t, (char *)argv[1]);
    if (t->mutex) scheme_post_sema(t->mutex);
  } else {
    scheme_wrong_contract("hash-ref-key", "hash?", 0, argc, argv);
    return NULL;
  }

  if (v)
    return v;
  else
    return hash_failed("hash-ref-key", "hash does not contain key", argc, argv);
}

static Scheme_Object *hash_table_remove_bang(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = argv[0];

  if (SCHEME_NP_CHAPERONEP(v) && (SCHEME_HASHTP(SCHEME_CHAPERONE_VAL(v))
                                  || SCHEME_BUCKTP(SCHEME_CHAPERONE_VAL(v)))) {
    scheme_chaperone_hash_set(v, argv[1], NULL);
    return scheme_void;
  }
  
  if (!(SCHEME_HASHTP(v) && SCHEME_MUTABLEP(v)) && !SCHEME_BUCKTP(v))
    scheme_wrong_contract("hash-remove!", "(and/c hash? (not/c immutable?))", 0, argc, argv);

  if (SCHEME_BUCKTP(v)) {
    Scheme_Bucket *b;
    Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    b = scheme_bucket_or_null_from_table((Scheme_Bucket_Table *)v, (char *)argv[1], 0);
    if (b) {
      HT_EXTRACT_WEAK(b->key) = NULL;
      b->val = NULL;
    }
    if (t->mutex) scheme_post_sema(t->mutex);
  } else{
    Scheme_Hash_Table *t = (Scheme_Hash_Table *)v;
    if (t->mutex) scheme_wait_sema(t->mutex, 0);
    scheme_hash_set(t, argv[1], NULL);
    if (t->mutex) scheme_post_sema(t->mutex);
  }

  return scheme_void;
}

static Scheme_Object *hash_table_remove(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v = argv[0];

  if (SCHEME_NP_CHAPERONEP(v) && SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(v)))
    return chaperone_hash_tree_set(v, argv[1], NULL);

  if (!SCHEME_HASHTRP(v))
    scheme_wrong_contract("hash-remove", "(and/c hash? immutable?)", 0, argc, argv);

  return (Scheme_Object *)scheme_hash_tree_set((Scheme_Hash_Tree *)v, argv[1], NULL);
}

static Scheme_Object *hash_table_clear_bang(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *v2;

  v = argv[0];

  v2 = (SCHEME_NP_CHAPERONEP(v) ? SCHEME_CHAPERONE_VAL(v) : v);

  if (!(SCHEME_HASHTP(v2) && SCHEME_MUTABLEP(v2)) && !SCHEME_BUCKTP(v2))
    scheme_wrong_contract("hash-clear!", "(and/c hash? (not/c immutable?))", 0, argc, argv);

  if (SCHEME_NP_CHAPERONEP(v)) {
    if (chaperone_hash_clear("hash-clear!", v)) {
      /* A non-NULL result means that there were `hash-clear' implementations
         in the chaperone and all checking passed. */
      v = v2; /* and perform clear below */
    } else {
      /* Implement `(hash-clear! ht)' as `(hash-for-each ht (lambda (k) (hash-remove! ht k)))'
         to allow chaperones to interpose. */
      Scheme_Object *i, *a[2], *key;
      a[0] = v;
      while (1) {
        i = scheme_hash_table_iterate_start(1, a);
        if (SCHEME_FALSEP(i))
          break;
        
        a[1] = i;
        key = scheme_hash_table_iterate_key(2, a);
        a[1] = key;
        
        hash_table_remove_bang(2, a);
      }

      return scheme_void;
    }
  }

  if (SCHEME_BUCKTP(v)) {
    scheme_clear_bucket_table((Scheme_Bucket_Table *)v);
  } else{
    scheme_clear_hash_table((Scheme_Hash_Table *)v);
  }

  return scheme_void;
}

static Scheme_Object *hash_table_clear(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v, *v2;

  v = argv[0];

  v2 = (SCHEME_NP_CHAPERONEP(v) ? SCHEME_CHAPERONE_VAL(v) : v);

  if (!SCHEME_HASHTRP(v2))
    scheme_wrong_contract("hash-clear", "(and/c hash? immutable?)", 0, argc, argv);

  if (SCHEME_NP_CHAPERONEP(v)) {
    v2 = chaperone_hash_clear("hash-clear", v);
    if (v2)
      return v2;
    else {
      /* NULL result means that a `hash-clear' implementation was not
         available, so we need to fold a remove over all keys: */
      Scheme_Object *i, *a[2], *key;
      while (1) {
        a[0] = v;
        i = scheme_hash_table_iterate_start(1, a);
        if (SCHEME_FALSEP(i))
          return v;
        
        a[1] = i;
        key = scheme_hash_table_iterate_key(2, a);
        a[1] = key;
        
        v = hash_table_remove_bang(2, a);
      }
    }
  } else {
    return (Scheme_Object *)scheme_make_hash_tree_of_type(SCHEME_HASHTR_TYPE(v));
  }
}

static void no_post_key(const char *name, Scheme_Object *key, int chap)
{
  scheme_contract_error(name,
                        (chap
                         ? "no value found for post-chaperone key"
                         : "no value found for post-impersonator key"),
                        "key", 1, key,
                        NULL);
}

static Scheme_Object *do_map_hash_table(int argc,
					Scheme_Object *argv[],
					char *name,
					int keep,
                                        int try_sorted)
{
  int i;
  Scheme_Object *f, **sorted_keys;
  Scheme_Object *first, *last = NULL, *v, *p[2], *obj, *chaperone;

  obj = argv[0];
  if (SCHEME_NP_CHAPERONEP(obj)) {
    chaperone = obj;
    obj = SCHEME_CHAPERONE_VAL(chaperone);
  } else
    chaperone = NULL;

  if (!(SCHEME_HASHTP(obj) || SCHEME_BUCKTP(obj) || SCHEME_HASHTRP(obj)))
    scheme_wrong_contract(name, "hash?", 0, argc, argv);
  scheme_check_proc_arity(name, 2, 1, argc, argv);

  f = argv[1];

  if (keep)
    first = scheme_null;
  else
    first = scheme_void;

  /* In simple cases, sort keys. This is useful for quasiquote
     expansion over hash tables, for example. */
  if (try_sorted && !chaperone && (SCHEME_HASHTP(obj) || SCHEME_HASHTRP(obj)))
    sorted_keys = scheme_extract_sorted_keys(obj);
  else
    sorted_keys = NULL;

  if (sorted_keys) {
    if (sorted_keys) {
      int i, count;
      count = (SCHEME_HASHTP(obj) ? ((Scheme_Hash_Table *)obj)->count : ((Scheme_Hash_Tree *)obj)->count);
      for (i = 0; i < count; i++) {
        if (SCHEME_HASHTP(obj))
          v = scheme_hash_get((Scheme_Hash_Table *)obj, sorted_keys[i]);
        else
          v = scheme_hash_tree_get((Scheme_Hash_Tree *)obj, sorted_keys[i]);
        if (v) {
          p[0] = sorted_keys[i];
          p[1] = v;
          v = _scheme_apply(f, 2, p);
          if (keep) {
            v = lcons(v, scheme_null);
            if (last)
              SCHEME_CDR(last) = v;
            else
              first = v;
            last = v;
          }
        }
      }
    }
  } else if (SCHEME_BUCKTP(obj)) {
    Scheme_Bucket_Table *hash;
    Scheme_Bucket *bucket;

    hash = (Scheme_Bucket_Table *)obj;

    for (i = hash->size; i--; ) {
      bucket = hash->buckets[i];
      if (bucket && bucket->val && bucket->key) {
	if (hash->weak)
	  p[0] = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
	else
	  p[0] = (Scheme_Object *)bucket->key;
        if (chaperone) {
          v = chaperone_hash_key(name, chaperone, p[0]);
          p[0] = v;
          v = scheme_chaperone_hash_get(chaperone, v);
          if (!v)
            no_post_key(name, p[0], 0);
        } else
          v = (Scheme_Object *)bucket->val;
        if (v) {
          p[1] = v;
          if (keep) {
            v = _scheme_apply(f, 2, p);
            v = lcons(v, scheme_null);
            if (last)
              SCHEME_CDR(last) = v;
            else
              first = v;
            last = v;
          } else
            _scheme_apply_multi(f, 2, p);
        }
      }
    }
  } else if (SCHEME_HASHTP(obj)) {
    Scheme_Hash_Table *hash;

    hash = (Scheme_Hash_Table *)obj;

    for (i = hash->size; i--; ) {
      if (hash->vals[i]) {
	p[0] = hash->keys[i];
        if (chaperone) {
          v = chaperone_hash_key(name, chaperone, p[0]);
          p[0] = v;
          v = scheme_chaperone_hash_get(chaperone, v);
          if (!v)
            no_post_key(name, p[0], 0);
        } else {
          v = hash->vals[i];
        }
        if (v) {
          p[1] = v;
          if (keep) {
            v = _scheme_apply(f, 2, p);
            v = lcons(v, scheme_null);
            if (last)
              SCHEME_CDR(last) = v;
            else
              first = v;
            last = v;
          } else
            _scheme_apply_multi(f, 2, p);
        }
      }
    }
  } else {
    Scheme_Object *ik, *iv;
    Scheme_Hash_Tree *hash;
    mzlonglong pos;

    hash = (Scheme_Hash_Tree *)obj;

    pos = scheme_hash_tree_next(hash, -1);
    while (pos != -1) {
      scheme_hash_tree_index(hash, pos, &ik, &iv);
      p[0] = ik;
      if (chaperone) {
        ik = chaperone_hash_key(name, chaperone, ik);
        iv = scheme_chaperone_hash_get(chaperone, ik);
        if (!iv)
          no_post_key(name, ik, 1);
      }
      if (iv) {
        p[1] = iv;
        if (keep) {
          v = _scheme_apply(f, 2, p);
          v = lcons(v, scheme_null);
          if (last)
            SCHEME_CDR(last) = v;
          else
            first = v;
          last = v;
        } else
          _scheme_apply_multi(f, 2, p);
      }
      pos = scheme_hash_tree_next(hash, pos);
    }
  }

  return first;
}

static Scheme_Object *hash_table_map(int argc, Scheme_Object *argv[])
{
  return do_map_hash_table(argc, argv, "hash-map", 1, (argc > 2) && SCHEME_TRUEP(argv[2]));
}

static Scheme_Object *hash_table_for_each(int argc, Scheme_Object *argv[])
{
  return do_map_hash_table(argc, argv, "hash-for-each", 0, (argc > 2) && SCHEME_TRUEP(argv[2]));
}

static Scheme_Object *hash_table_next(const char *name, mzlonglong start, int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];

  if (SCHEME_NP_CHAPERONEP(o))
    o = SCHEME_CHAPERONE_VAL(o);

  if (SCHEME_HASHTP(o)) {
    return scheme_hash_table_next((Scheme_Hash_Table *)o, start);
  } else if (SCHEME_HASHTRP(o)) {
    return scheme_hash_tree_next_pos((Scheme_Hash_Tree *)o, start);
  } else if (SCHEME_BUCKTP(o)) {
    return scheme_bucket_table_next((Scheme_Bucket_Table *)o, start);
  } else {
    scheme_wrong_contract(name, "hash?", 0, argc, argv);
    return NULL;
  }
}

Scheme_Object *scheme_hash_table_iterate_start(int argc, Scheme_Object *argv[])
{
  return hash_table_next("hash-iterate-first", -1, argc, argv);
}

#define HASH_POS_TOO_BIG ((mzlonglong)1) << 62

Scheme_Object *scheme_hash_table_iterate_next(int argc, Scheme_Object *argv[])
{
  Scheme_Object *p = argv[1], *v;
  mzlonglong pos;

  if (!scheme_get_long_long_val(p, &pos))
    pos = HASH_POS_TOO_BIG;
  else if (pos < 0)
    pos = HASH_POS_TOO_BIG;

  v = hash_table_next("hash-iterate-next", pos, argc, argv);

  if (v) return v;

  if (SCHEME_INTP(p)) {
    if (SCHEME_INT_VAL(p) >= 0)
      p = NULL;
  } else if (SCHEME_BIGNUMP(p)) {
    if (SCHEME_BIGPOS(p))
      p = NULL;
  }

  if (p)
    scheme_wrong_contract("hash-iterate-next", 
			  "exact-nonnegative-integer?", 1, argc, argv);  

  if (0) {
    scheme_contract_error("hash-iterate-next", "no element at index", 
                          "index", 1, argv[1],
                          NULL);
    return NULL;
  } else
    return scheme_false;
}

static int hash_table_index(const char *name, int argc, Scheme_Object *argv[],
                            Scheme_Object **_k, Scheme_Object **_v,
                            Scheme_Object *bad_index_v)
{
  Scheme_Object *p = argv[1], *obj = argv[0];
  mzlonglong pos;
  int res;

  if (SCHEME_NP_CHAPERONEP(obj))
    obj = SCHEME_CHAPERONE_VAL(obj);

  if (!scheme_get_long_long_val(p, &pos) || (pos < 0))
    pos = HASH_POS_TOO_BIG;

  if (SCHEME_HASHTP(obj)) {
    res = scheme_hash_table_index((Scheme_Hash_Table *)obj, pos, _k, _v);
  } else if (SCHEME_HASHTRP(obj)) {
    res = scheme_hash_tree_index((Scheme_Hash_Tree *)obj, pos, _k, _v);
  } else if (SCHEME_BUCKTP(obj)) {
    res = scheme_bucket_table_index((Scheme_Bucket_Table *)obj, pos, _k, _v);
  } else {
    scheme_wrong_contract(name, "hash?", 0, argc, argv);
    return 0;
  }

  if (res) return 1;

  if ((SCHEME_INTP(p)
       && (SCHEME_INT_VAL(p) >= 0))
      || (SCHEME_BIGNUMP(p)
          && SCHEME_BIGPOS(p))) {
    if (bad_index_v) {
      if (_k) *_k = bad_index_v;
      if (_v) *_v = bad_index_v;
      return 0;
    } else {
      scheme_contract_error(name, "no element at index",
                            "index", 1, p,
                            NULL);
      return 0;
    }
  }

  scheme_wrong_contract(name, "exact-nonnegative-integer?", 1, argc, argv);  
  return 0;
}

Scheme_Object *scheme_hash_table_iterate_key(int argc, Scheme_Object *argv[])
{
  const char *name = "hash-iterate-key";
  Scheme_Object *key;
  if (hash_table_index(name, argc, argv, &key, NULL, ((argc > 2) ? argv[2] : NULL))) {
    Scheme_Object *obj = argv[0];
    if (SCHEME_NP_CHAPERONEP(obj))
      return chaperone_hash_key(name, obj, key);
  }
  return key;
}

Scheme_Object *scheme_hash_table_iterate_value(int argc, Scheme_Object *argv[])
{
  const char *name = "hash-iterate-value";
  Scheme_Object *key, *val;
  if (hash_table_index(name, argc, argv, &key, &val, ((argc > 2) ? argv[2] : NULL))) {
    Scheme_Object *obj = argv[0];
    if (SCHEME_NP_CHAPERONEP(obj)) {
      Scheme_Object *chap_key, *chap_val;
      int ischap = SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(obj));
      chaperone_hash_key_value(name, obj, key, &chap_key, &chap_val, ischap);
      return chap_val;
    }
  }
  return val;
}

Scheme_Object *scheme_hash_table_iterate_pair(int argc, Scheme_Object *argv[])
{
  const char *name = "hash-iterate-pair";
  Scheme_Object *key, *val;
  if (hash_table_index(name, argc, argv, &key, &val, ((argc > 2) ? argv[2] : NULL))) {
    Scheme_Object *obj = argv[0];
    if (SCHEME_NP_CHAPERONEP(obj)) {
      Scheme_Object *chap_key, *chap_val;
      int ischap = SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(obj));
      chaperone_hash_key_value(name, obj, key, &chap_key, &chap_val, ischap);
      return scheme_make_pair(chap_key, chap_val);
    }
  }
  return scheme_make_pair(key, val);
}

Scheme_Object *scheme_hash_table_iterate_key_value(int argc, Scheme_Object *argv[])
{
  const char *name = "hash-iterate-key+value";
  Scheme_Object *key, *val, *res[2];
  if (hash_table_index(name, argc, argv, &key, &val, ((argc > 2) ? argv[2] : NULL))) {
    Scheme_Object *obj = argv[0];
    if (SCHEME_NP_CHAPERONEP(obj)) {
      int ischap = SCHEME_HASHTRP(SCHEME_CHAPERONE_VAL(obj));
      chaperone_hash_key_value(name, obj, key, &key, &val, ischap);
    }
  }
  res[0] = key;
  res[1] = val;
  return scheme_values(2, res);
}

static Scheme_Object *hash_keys_subset_p_slow(int argc, Scheme_Object *argv[])
{
  Scheme_Object *b[2], *i1, *c2;
  int kind1, kind2;
  
  if (SCHEME_HASHTRP(argv[0]) && SCHEME_HASHTRP(argv[1])) {
    if (SAME_TYPE(SCHEME_HASHTR_TYPE(argv[0]), SCHEME_HASHTR_TYPE(argv[1])))
      return (scheme_hash_tree_subset_of((Scheme_Hash_Tree *)argv[0], (Scheme_Hash_Tree *)argv[1])
              ? scheme_true
              : scheme_false);
  }

  b[0] = argv[1];
  if (!SCHEME_TRUEP(hash_p(1, argv)))
    scheme_wrong_type("hash-keys-subset?", "hash?", 0 , argc, argv);
  if (!SCHEME_TRUEP(hash_p(1, b)))
    scheme_wrong_type("hash-keys-subset?", "hash?", 1, argc, argv);

  if (SCHEME_TRUEP(scheme_hash_eq_p(1, argv)))
    kind1 = 0;
  else if (SCHEME_TRUEP(scheme_hash_equal_p(1, argv)))
    kind1 = 1;
  else
    kind1 = 2;
  
  if (SCHEME_TRUEP(scheme_hash_eq_p(1, b)))
    kind2 = 0;
  else if (SCHEME_TRUEP(scheme_hash_equal_p(1, b)))
    kind2 = 1;
  else
    kind2 = 2;

  if (kind1 != kind2) {
    scheme_contract_error("hash-keys-subset?",
                          "given hash tables do not use the same key comparison",
                          "first table", 1, argv[0],
                          "second table", 1, argv[1],
                          NULL);
    return NULL;
  }

  i1 = scheme_checked_hash_count(1, argv);
  c2 = scheme_checked_hash_count(1, b);
  if (SCHEME_INT_VAL(i1) > SCHEME_INT_VAL(c2))
    return scheme_false;

  i1 = scheme_hash_table_iterate_start(1, argv);
  b[0] = argv[0];
  while (!SCHEME_FALSEP(i1)) {
    b[1] = i1;
    c2 = scheme_hash_table_iterate_key(2, b);

    if (!scheme_chaperone_hash_get(argv[1], c2))
        return scheme_false;
    
    i1 = scheme_hash_table_iterate_next(2, b);
  }

  return scheme_true;
}

static Scheme_Object *hash_keys_subset_p(int argc, Scheme_Object *argv[]) XFORM_ASSERT_NO_CONVERSION
{
  if (SCHEME_HASHTRP(argv[0])
      && SCHEME_HASHTRP(argv[1])
      && SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(argv[0]))
      && SAME_TYPE(scheme_eq_hash_tree_type, SCHEME_HASHTR_TYPE(argv[1]))) {
    if (scheme_eq_hash_tree_subset_of((Scheme_Hash_Tree *)argv[0], (Scheme_Hash_Tree *)argv[1]))
      return scheme_true;
    else
      return scheme_false;
  } else
    return hash_keys_subset_p_slow(argc, argv);
}


static Scheme_Object *do_chaperone_hash(const char *name, int is_impersonator, int argc, Scheme_Object **argv)
{
  Scheme_Chaperone *px;
  Scheme_Object *val = argv[0];
  Scheme_Object *redirects, *clear, *equal_key_wrap;
  Scheme_Object *props;
  int start_props = 5;

  if (SCHEME_CHAPERONEP(val))
    val = SCHEME_CHAPERONE_VAL(val);

  if (!SCHEME_HASHTP(val) 
      && (is_impersonator || !SCHEME_HASHTRP(val))
      && !SCHEME_BUCKTP(val))
    scheme_wrong_contract(name, is_impersonator ? "(and/c hash? (not/c immutable?))" : "hash?", 0, argc, argv);
  scheme_check_proc_arity(name, 2, 1, argc, argv); /* ref */
  scheme_check_proc_arity(name, 3, 2, argc, argv); /* set! */
  scheme_check_proc_arity(name, 2, 3, argc, argv); /* remove */
  scheme_check_proc_arity(name, 2, 4, argc, argv); /* key */

  if ((argc > 5) && (SCHEME_FALSEP(argv[5]) || SCHEME_PROCP(argv[5]))) {
    scheme_check_proc_arity2(name, 1, 5, argc, argv, 1); /* clear */
    clear = argv[5];
    start_props++;
  } else
    clear = scheme_false;

  if ((argc > 6) && (SCHEME_FALSEP(argv[6]) || SCHEME_PROCP(argv[6]))) {
    scheme_check_proc_arity2(name, 2, 6, argc, argv, 1); /* clear */
    equal_key_wrap = argv[6];
    start_props++;
  } else
    equal_key_wrap = scheme_false;

  /* The allocation of this vector is used to detect when two
     chaperoned immutable hash tables can be
     `{chaperone,impersonator}-of?` when they're not eq. */
  redirects = scheme_make_vector(6, NULL);
  SCHEME_VEC_ELS(redirects)[0] = argv[1];
  SCHEME_VEC_ELS(redirects)[1] = argv[2];
  SCHEME_VEC_ELS(redirects)[2] = argv[3];
  SCHEME_VEC_ELS(redirects)[3] = argv[4];
  SCHEME_VEC_ELS(redirects)[4] = clear;
  SCHEME_VEC_ELS(redirects)[5] = equal_key_wrap;
  redirects = scheme_box(redirects); /* so it doesn't look like a struct chaperone */

  props = scheme_parse_chaperone_props(name, start_props, argc, argv);
  
  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  px->iso.so.type = scheme_chaperone_type;
  px->val = val;
  px->prev = argv[0];
  px->props = props;
  px->redirects = redirects;

  if (is_impersonator)
    SCHEME_CHAPERONE_FLAGS(px) |= SCHEME_CHAPERONE_IS_IMPERSONATOR;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_hash(int argc, Scheme_Object **argv)
{
  return do_chaperone_hash("chaperone-hash", 0, argc, argv);
}

static Scheme_Object *impersonate_hash(int argc, Scheme_Object **argv)
{
  return do_chaperone_hash("impersonate-hash", 1, argc, argv);
}

static Scheme_Object *transfer_chaperone(Scheme_Object *chaperone, Scheme_Object *v)
{
  Scheme_Chaperone *px;

  px = MALLOC_ONE_TAGGED(Scheme_Chaperone);
  memcpy(px, chaperone, sizeof(Scheme_Chaperone));
  px->prev = v;
  if (SCHEME_CHAPERONEP(v))
    px->val = SCHEME_CHAPERONE_VAL(v);
  else
    px->val = v;

  return (Scheme_Object *)px;
}

static Scheme_Object *chaperone_hash_op(const char *who, Scheme_Object *o, Scheme_Object *k, 
                                        Scheme_Object *v, int mode, Scheme_Object *key_wraps);

static Scheme_Object *chaperone_hash_op_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Object *k = (Scheme_Object *)p->ku.k.p2;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p3;
  const char *who = (const char *)p->ku.k.p4;
  Scheme_Object *key_wraps = (Scheme_Object *)p->ku.k.p5;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;
  p->ku.k.p5 = NULL;

  o = chaperone_hash_op(who, o, k, v, p->ku.k.i1, key_wraps);
  
  if (!o)
    return scheme_false;
  else
    return scheme_box(o);
}

static Scheme_Object *chaperone_hash_op_overflow(const char *who, Scheme_Object *o, Scheme_Object *k, 
                                                 Scheme_Object *v, int mode, Scheme_Object *key_wraps)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)k;
  p->ku.k.p3 = (void *)v;
  p->ku.k.p4 = (void *)who;
  p->ku.k.i1 = mode;
  p->ku.k.p5 = (void *)key_wraps;

  o = scheme_handle_stack_overflow(chaperone_hash_op_k);

  if (SCHEME_FALSEP(o)) 
    return NULL;
  else
    return SCHEME_BOX_VAL(o);
}

static Scheme_Object *chaperone_hash_op(const char *who, Scheme_Object *o, Scheme_Object *k,
                                        Scheme_Object *v, int mode, Scheme_Object *key_wraps)
{
  Scheme_Object *wraps = NULL;

  while (1) {
    if (!SCHEME_NP_CHAPERONEP(o)) {
      if (SCHEME_NULLP(key_wraps))
        key_wraps = NULL;
      else
        key_wraps = scheme_make_raw_pair((Scheme_Object *)who, key_wraps);
      if (mode == 0 || mode == 5) {
        /* hash-ref or hash-ref-key */
        Scheme_Object *interned_key = NULL;
        if (SCHEME_HASHTP(o)) {
          Scheme_Hash_Table *t = (Scheme_Hash_Table *)o;
          if (t->mutex) scheme_wait_sema(t->mutex, 0);
          v = scheme_hash_get_w_key_wraps(t, k, key_wraps, &interned_key);
          if (t->mutex) scheme_post_sema(t->mutex);
        } else if (SCHEME_HASHTRP(o))
          v = scheme_hash_tree_get_w_key_wraps((Scheme_Hash_Tree *)o, k, key_wraps, &interned_key);
        else {
          Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)o;
          if (t->mutex) scheme_wait_sema(t->mutex, 0);
          v = scheme_lookup_in_table_w_key_wraps(t, (const char *)k, key_wraps, &interned_key);
          if (t->mutex) scheme_post_sema(t->mutex);
        }
        if (mode == 0)
          return v;
        else
          return interned_key;
      } else if ((mode == 1) || (mode == 2)) {
        /* hash-set! or hash-remove! */
        if (SCHEME_HASHTP(o)) {
          Scheme_Hash_Table *t = (Scheme_Hash_Table *)o;
          if (t->mutex) scheme_wait_sema(t->mutex, 0);
          scheme_hash_set_w_key_wraps(t, k, v, key_wraps);
          if (t->mutex) scheme_post_sema(t->mutex);
        } else if (SCHEME_HASHTRP(o)) {
          o = (Scheme_Object *)scheme_hash_tree_set_w_key_wraps((Scheme_Hash_Tree *)o, k, v, key_wraps);
          while (wraps) {
            o = transfer_chaperone(SCHEME_CAR(wraps), o);
            wraps = SCHEME_CDR(wraps);
          }
          return o;
        } else if (!v) {
          Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)o;
          Scheme_Bucket *b;
          if (t->mutex) scheme_wait_sema(t->mutex, 0);
          b = scheme_bucket_or_null_from_table_w_key_wraps(t, (char *)k, 0, key_wraps);
          if (t->mutex) scheme_post_sema(t->mutex);
          if (b) {
            HT_EXTRACT_WEAK(b->key) = NULL;
            b->val = NULL;
          }
        } else {
          Scheme_Bucket_Table *t = (Scheme_Bucket_Table *)o;
          if (t->mutex) scheme_wait_sema(t->mutex, 0);
          scheme_add_to_table_w_key_wraps(t, (const char *)k, v, 0, key_wraps);
          if (t->mutex) scheme_post_sema(t->mutex);
        }
        return scheme_void;
      } else if (mode == 3)
        return k;
      else {
        /* mode == 4, hash-clear */
        if (SCHEME_HASHTRP(o)) {
          o = (Scheme_Object *)scheme_make_hash_tree_of_type(SCHEME_HASHTR_TYPE(o));
          while (wraps) {
            o = transfer_chaperone(SCHEME_CAR(wraps), o);
            wraps = SCHEME_CDR(wraps);
          }
          return o;
        } else
          return scheme_void;
      }
    } else {
      Scheme_Chaperone *px = (Scheme_Chaperone *)o;
      Scheme_Object *a[3], *red, *orig;
      const char *what;

#ifdef DO_STACK_CHECK
      {
# include "mzstkchk.h"
        return chaperone_hash_op_overflow(who, o, k, v, mode, key_wraps);
      }
#endif

      if ((mode != 3) && (mode != 4)) {
        red = SCHEME_BOX_VAL(px->redirects);
        red = SCHEME_VEC_ELS(red)[5];
        if (!SCHEME_FALSEP(red))
          key_wraps = scheme_make_pair((Scheme_Object *)px, key_wraps);
      }

      if (mode == 0)
        orig = NULL;
      else if (mode == 3) {
        orig = chaperone_hash_op(who, px->prev, k, v, mode, key_wraps);
        k = orig;
      } else if ((mode == 2) || (mode == 5))
        orig = k;
      else if (mode == 4)
        orig = scheme_void;
      else
        orig = v;

      if (SCHEME_VECTORP(px->redirects)) {
        /* chaperone was on property accessors */
        o = orig;
      } else {
        int red_idx;

        red_idx = (mode == 5) ? 0 : mode;
        red = SCHEME_BOX_VAL(px->redirects);
        red = SCHEME_VEC_ELS(red)[red_idx];

        if ((mode == 4) && SCHEME_FALSEP(red))
          return NULL; /* => fall back to a sequence of removes */

        a[0] = px->prev;
        a[1] = k;
        a[2] = orig;

        if ((mode == 0) || (mode == 1) || (mode == 5)) {
          /* hash-ref, hash-set!, or hash-ref-key */
          Scheme_Object **vals;
          int cnt;
          Scheme_Thread *p;

          o = _scheme_apply_multi(red, ((red_idx == 0) ? 2 : 3), a);

          if (SAME_OBJ(o, SCHEME_MULTIPLE_VALUES)) {
            p = scheme_current_thread;
            cnt = p->ku.multiple.count;
            vals = p->ku.multiple.array;
            p->ku.multiple.array = NULL;
            if (SAME_OBJ(vals, p->values_buffer))
              p->values_buffer = NULL;
            p = NULL;
          } else {
            vals = NULL;
            cnt = 1;
          }

          if (cnt != 2)
            scheme_raise_exn(MZEXN_FAIL_CONTRACT_ARITY,
                             "%s: chaperone did not return 2 values\n"
                             "  chaperone procedure: %V\n"
                             "  number of returned values: %d",
                             who,
                             red,
                             cnt);

          if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
            if (!scheme_chaperone_of(vals[0], k))
              scheme_wrong_chaperoned(who, "key", k, vals[0]);
          k = vals[0];
          o = vals[1];

          if (mode == 0) {
            /* hash-ref */
            red = o;
            if (!scheme_check_proc_arity(NULL, 3, 1, 2, vals))
              scheme_raise_exn(MZEXN_FAIL_CONTRACT,
                               "%s: chaperone produced a second value that does not match the expected contract\n"
                               "  expected: (procedure-arity-includes/c 3)\n"
                               "  received: %V",
                               who,
                               red);

            orig = chaperone_hash_op(who, px->prev, k, v, mode, key_wraps);
            if (!orig) return NULL;

            /* hash-ref */
            a[0] = px->prev;
            a[1] = k;
            a[2] = orig;
            o = _scheme_apply(red, 3, a);
            what = "result";
          } else if (mode == 5) {
            /* hash-ref-key */
            orig = chaperone_hash_op(who, px->prev, k, v, mode, key_wraps);
            if (!orig) return NULL;

            red = SCHEME_BOX_VAL(px->redirects);
            red = SCHEME_VEC_ELS(red)[3];

            a[0] = px->prev;
            a[1] = orig;

            o = scheme_apply(red, 2, a);
            what = "key";
          } else
            what = "value";
        } else if (mode == 4) {
          /* hash-clear */
          (void)_scheme_apply_multi(red, 1, a);
          o = scheme_void;
          what = "void";
        } else {
          /* hash-remove and key extraction */
          o = _scheme_apply(red, 2, a);
          what = "key";
        }

        if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR))
          if (!scheme_chaperone_of(o, orig))
            scheme_wrong_chaperoned(who, what, orig, o);
      }

      if ((mode == 0) || (mode == 3) || (mode == 5))
        return o;
      else {
        if (mode == 1)
          v = o;
        else
          k = o;
        if (SCHEME_HASHTRP(px->val))
          wraps = scheme_make_raw_pair((Scheme_Object *)px, wraps);
        o = px->prev;
      }
    }
  }
}

Scheme_Object *scheme_chaperone_hash_get(Scheme_Object *table, Scheme_Object *key)
{
  return chaperone_hash_op("hash-ref", table, key, NULL, 0, scheme_null);
}

Scheme_Object *scheme_chaperone_hash_get_key(Scheme_Object *table, Scheme_Object *key)
{
  return chaperone_hash_op("hash-ref-key", table, key, NULL, 5, scheme_null);
}

void scheme_chaperone_hash_set(Scheme_Object *table, Scheme_Object *key, Scheme_Object *val)
{
  (void)chaperone_hash_op(val ? "hash-set!" : "hash-remove!", table, key, val, val ? 1 : 2, scheme_null);
}

Scheme_Object *chaperone_hash_tree_set(Scheme_Object *table, Scheme_Object *key, Scheme_Object *val)
{
  return chaperone_hash_op(val ? "hash-set" : "hash-remove", table, key, val, val ? 1 : 2, scheme_null);
}

static Scheme_Object *chaperone_hash_key(const char *name, Scheme_Object *table, Scheme_Object *key)
{
  return chaperone_hash_op(name, table, key, NULL, 3, scheme_null);
}

Scheme_Object *scheme_chaperone_hash_key(const char *name, Scheme_Object *table, Scheme_Object *key)
{
  return chaperone_hash_key(name, table, key);
}

static void chaperone_hash_key_value(const char *name, Scheme_Object *obj, Scheme_Object *key,
                                     Scheme_Object **_chap_key, Scheme_Object **_chap_val,
                                     int ischap)
{
  Scheme_Object *chap_key, *chap_val;
  chap_key = chaperone_hash_key(name, obj, key);
  chap_val = scheme_chaperone_hash_get(obj, chap_key);
  if (!chap_val)
    no_post_key(name, chap_key, ischap);
  *_chap_key = chap_key;
  *_chap_val = chap_val;
}

void scheme_chaperone_hash_key_value(const char *name, Scheme_Object *obj, Scheme_Object *k,
                                     Scheme_Object **_chap_key, Scheme_Object **_chap_val,
                                     int ischap)
{
  return chaperone_hash_key_value(name, obj, k, _chap_key, _chap_val, ischap);
}

static Scheme_Object *chaperone_hash_clear(const char *name, Scheme_Object *table)
{
  return chaperone_hash_op(name, table, NULL, NULL, 4, scheme_null);
}

Scheme_Object *scheme_chaperone_hash_traversal_get(Scheme_Object *table, Scheme_Object *key,
                                                   Scheme_Object **alt_key)
{
  key = chaperone_hash_key("hash-table-iterate-key", table, key);
  *alt_key = key;
  return chaperone_hash_op("hash-ref", table, key, NULL, 0, scheme_null);
}


Scheme_Object *scheme_chaperone_hash_table_filtered_copy(Scheme_Object *obj,
                                                         Hash_Table_Element_Filter_Proc filter)
{
  Scheme_Object *a[3], *v, *v2, *idx, *key, *val;
  int is_eq, is_eqv;

  if (SCHEME_CHAPERONEP(obj))
    v = SCHEME_CHAPERONE_VAL(obj);
  else
    v = obj;

  a[0] = obj;
  is_eq = SCHEME_TRUEP(scheme_hash_eq_p(1, a));
  is_eqv = SCHEME_TRUEP(scheme_hash_eqv_p(1, a));
  
  if (SCHEME_HASHTP(v)) {
    if (is_eq)
      v2 = make_hasheq(0, NULL);
    else if (is_eqv)
      v2 = make_hasheqv(0, NULL);
    else
      v2 = make_hash(0, NULL);
  } else if (SCHEME_HASHTRP(v)) {
    if (is_eq)
      v2 = scheme_make_immutable_hasheq(0, NULL);
    else if (is_eqv)
      v2 = scheme_make_immutable_hasheqv(0, NULL);
    else
      v2 = scheme_make_immutable_hash(0, NULL);
  } else {
    if (is_eq)
      v2 = make_weak_hasheq(0, NULL);
    else if (is_eqv)
      v2 = make_weak_hasheqv(0, NULL);
    else
      v2 = make_weak_hash(0, NULL);
  }

  idx = scheme_hash_table_iterate_start(1, a);
  while (SCHEME_TRUEP(idx)) {
    a[0] = v;
    a[1] = idx;
    key = scheme_hash_table_iterate_key(2, a);

    val = scheme_chaperone_hash_get(obj, key);
    if (filter && val) {
      key = filter(key);
      if (!key) return NULL;
      val = filter(val);
      if (!val) return NULL;
    }
    if (val) {
      a[0] = v2;
      a[1] = key;
      a[2] = val;
      if (SCHEME_HASHTRP(v2))
        v2 = scheme_hash_table_put(2, a);
      else
        (void)hash_table_put_bang(2, a);
    }

    a[0] = v;
    a[1] = idx;
    idx = scheme_hash_table_iterate_next(2, a);
  }

  return v2;
}

Scheme_Object *scheme_chaperone_hash_table_copy(Scheme_Object *obj)
{
  return scheme_chaperone_hash_table_filtered_copy(obj, NULL);
}

static Scheme_Object *eq_hash_code(int argc, Scheme_Object *argv[])
{
  intptr_t v;

  if (SCHEME_INTP(argv[0]))
    return argv[0];

#ifdef MZ_PRECISE_GC
  v = scheme_hash_key(argv[0]);
#else
  v = ((intptr_t)argv[0]) >> 2;
#endif

  return scheme_make_integer(v);
}

static Scheme_Object *equal_hash_code(int argc, Scheme_Object *argv[])
{
  intptr_t v;

  if (SCHEME_INTP(argv[0]))
    return argv[0];

  v = scheme_equal_hash_key(argv[0]);

  return scheme_make_integer(v);
}

static Scheme_Object *equal_hash2_code(int argc, Scheme_Object *argv[])
{
  intptr_t v;

  v = scheme_equal_hash_key2(argv[0]);

  return scheme_make_integer(v);
}

static Scheme_Object *eqv_hash_code(int argc, Scheme_Object *argv[])
{
  intptr_t v;

  if (SCHEME_INTP(argv[0]))
    return argv[0];

  v = scheme_eqv_hash_key(argv[0]);

  return scheme_make_integer(v);
}

Scheme_Object *scheme_make_weak_box(Scheme_Object *v)
{
#ifdef MZ_PRECISE_GC
  return (Scheme_Object *)GC_malloc_weak_box(v, NULL, 0, 0);
#else
  Scheme_Small_Object *obj;

  obj = MALLOC_ONE_TAGGED_WEAK(Scheme_Small_Object);
  obj->iso.so.type = scheme_weak_box_type;

  obj->u.ptr_val = v;
  scheme_weak_reference((void **)(void *)&obj->u.ptr_val);

  return (Scheme_Object *)obj;
#endif
}

Scheme_Object *scheme_make_late_weak_box(Scheme_Object *v)
{
#ifdef MZ_PRECISE_GC
  return (Scheme_Object *)GC_malloc_weak_box(v, NULL, 0, 1);
#else
  Scheme_Small_Object *obj;

  obj = MALLOC_ONE_TAGGED_WEAK(Scheme_Small_Object);

  obj->iso.so.type = scheme_weak_box_type;

  obj->u.ptr_val = v;
  scheme_late_weak_reference((void **)(void *)&obj->u.ptr_val);

  return (Scheme_Object *)obj;
#endif
}

static Scheme_Object *make_weak_box(int argc, Scheme_Object *argv[])
{
  return scheme_make_weak_box(argv[0]);
}

static Scheme_Object *weak_box_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  if (!SCHEME_WEAKP(argv[0]))
    scheme_wrong_contract("weak-box-value", "weak-box?", 0, argc, argv);

  o = SCHEME_BOX_VAL(argv[0]);
  if (!o)
    return ((argc > 1) ? argv[1] : scheme_false);
  else
    return o;
}

Scheme_Object *scheme_weak_box_value(Scheme_Object *obj)
{
  Scheme_Object *a[1];
  a[0] = obj;
  return weak_box_value(1, a);
}

static Scheme_Object *weak_boxp(int argc, Scheme_Object *argv[])
{
  return (SCHEME_WEAKP(argv[0]) ? scheme_true : scheme_false);
}

Scheme_Object * scheme_make_null (void)
{
  return scheme_null;
}

static Scheme_Object *make_graph(int argc, Scheme_Object *argv[])
{
  return scheme_resolve_placeholders(argv[0]);
}

static Scheme_Object *make_placeholder(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ph;

  ph = scheme_alloc_small_object();
  ph->type = scheme_placeholder_type;
  SCHEME_PTR_VAL(ph) = argv[0];

  return ph;
}

static Scheme_Object *placeholder_set(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_placeholder_type))
    scheme_wrong_contract("placeholder-set!", "placeholder?", 0, argc, argv);
  SCHEME_PTR_VAL(argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *placeholder_get(int argc, Scheme_Object *argv[])
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_placeholder_type))
    scheme_wrong_contract("placeholder-get", "placeholder?", 0, argc, argv);
  return SCHEME_PTR_VAL(argv[0]);
}

static Scheme_Object *placeholder_p(int c, Scheme_Object *p[])
{
  return (SAME_TYPE(SCHEME_TYPE(p[0]), scheme_placeholder_type)
          ? scheme_true 
          : scheme_false);
}

static Scheme_Object *do_make_hash_placeholder(const char *who, int kind, int argc, Scheme_Object *argv[])
{
  Scheme_Object *l, *a, *ph;

  for (l = argv[0]; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    a = SCHEME_CAR(l);
    if (!SCHEME_PAIRP(a))
      break;
  }

  if (!SCHEME_NULLP(l)) {
    scheme_wrong_contract(who, "(listof pair?)", 0, argc, argv);
  }

  ph = scheme_alloc_object();
  ph->type = scheme_table_placeholder_type;
  SCHEME_IPTR_VAL(ph) = argv[0];
  SCHEME_PINT_VAL(ph) = kind;

  return ph;
}

static Scheme_Object *make_hash_placeholder(int argc, Scheme_Object *argv[])
{
  return do_make_hash_placeholder("make-hash-placeholder", 1, argc, argv);
}

static Scheme_Object *make_hasheq_placeholder(int argc, Scheme_Object *argv[])
{
  return do_make_hash_placeholder("make-hash-placeholder", 0, argc, argv);
}

static Scheme_Object *make_hasheqv_placeholder(int argc, Scheme_Object *argv[])
{
  return do_make_hash_placeholder("make-hasheqv-placeholder", 2, argc, argv);
}

static Scheme_Object *table_placeholder_p(int c, Scheme_Object *p[])
{
  return (SAME_TYPE(SCHEME_TYPE(p[0]), scheme_table_placeholder_type)
          ? scheme_true 
          : scheme_false);
}



/************************************************************/
/*                      ephemerons                          */
/************************************************************/

typedef struct Scheme_Ephemeron {
  Scheme_Object so;
  Scheme_Object *key, *val;
  struct Scheme_Ephemeron *next;
} Scheme_Ephemeron;

#ifndef MZ_PRECISE_GC

static Scheme_Ephemeron *ephemerons, *done_ephemerons; /* not registered as a root! */

#ifdef USE_SENORA_GC
extern void *GC_base(void *d);
# define GC_is_marked(p) GC_base(p)
# define GC_did_mark_stack_overflow() 0
# define GC_mark_overflow_recover(ptr) /**/
#else
extern MZGC_DLLIMPORT void *GC_base(void *);
extern MZGC_DLLIMPORT int GC_is_marked(void *);
extern MZGC_DLLIMPORT int GC_did_mark_stack_overflow(void);
extern MZGC_DLLIMPORT void GC_mark_overflow_recover(void *p);
#endif
extern MZGC_DLLIMPORT void GC_push_all_stack(void *, void *);
extern MZGC_DLLIMPORT void GC_flush_mark_stack(void);

#endif

Scheme_Object *scheme_make_ephemeron(Scheme_Object *key, Scheme_Object *val)
{
#ifdef MZ_PRECISE_GC
  return GC_malloc_ephemeron(key, val);
#else
  Scheme_Ephemeron *e;
  int can_gc = 1;

  if (SCHEME_INTP(key) || !GC_base(key))
    can_gc = 0;

  if (can_gc) {
    e = (Scheme_Ephemeron *)scheme_malloc_atomic(sizeof(Scheme_Ephemeron));
  } else {
    e = (Scheme_Ephemeron *)scheme_malloc(sizeof(Scheme_Ephemeron));
  }
  e->so.type = scheme_ephemeron_type;
  e->key = key;
  e->val = val;
  if (can_gc) {
    e->next = ephemerons;
    ephemerons = e;
  }

  return (Scheme_Object *)e;
#endif
}

Scheme_Object *scheme_ephemeron_value(Scheme_Object *o)
{
  return ((Scheme_Ephemeron *)o)->val;
}

Scheme_Object *scheme_ephemeron_key(Scheme_Object *o)
{
  return ((Scheme_Ephemeron *)o)->key;
}

#ifndef MZ_PRECISE_GC

static void set_ephemerons(Scheme_Ephemeron *ae, Scheme_Ephemeron *be)
{
  if (be) {
    Scheme_Ephemeron *e;
    for (e = be; e->next; e = e->next) { }
    e->next = ae;
    ae = be;
  }

  ephemerons = ae;
}

static int mark_ephemerons()
{
  Scheme_Ephemeron *e, *ae, *be, *next;
  int did_one, mix, ever_done = 0;

  mix = scheme_get_milliseconds();
  mix = mix >> 8;

  do {
    did_one = 0;
    ae = be = NULL;

    for (e = ephemerons; e; e = next) {
      next = e->next;

      if (e->key) {      
	if (!GC_is_marked(e) || !GC_is_marked(e->key)) {
	  /* No reason to mark, yet. Randomly put this one back
	     into one of the keep lists: */
	  if (mix & 0x1) {
	    e->next = ae;
	    ae = e;
	  } else {
	    e->next = be;
	    be = e;
	  }
	  mix += ((intptr_t)e >> 5) + ((intptr_t)e >> 2);
	} else {
	  did_one = 1;
	  ever_done = 1;
	  GC_push_all_stack(&e->val, &e->val + 1);
	  if (GC_did_mark_stack_overflow()) {
            GC_mark_overflow_recover(e->val);
	  } else {
	    GC_flush_mark_stack();
	    if (GC_did_mark_stack_overflow()) {
              GC_mark_overflow_recover(e->val);
	    }
	  }
	  /* Done with this one: */
	  e->next = done_ephemerons;
	  done_ephemerons = e;
	}
      } else {
	/* Ephemeron previously done, so drop it. This case
	   shouldn't happen, because it should have been
	   dropped earlier. */
      }
    }

    /* Combine ae & be back into ephemerons list: */
    set_ephemerons(ae, be);
  } while (did_one);

  return ever_done;
}

#endif

static Scheme_Object *make_ephemeron(int argc, Scheme_Object **argv)
{
  return scheme_make_ephemeron(argv[0], argv[1]);
}

static Scheme_Object *ephemeron_value(int argc, Scheme_Object **argv)
{
  Scheme_Object *v;

  /* If there's an argv[2], it will stay live until here, since
     there's no way for the GC to know that we're not using that
     value. */

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_ephemeron_type))
    scheme_wrong_contract("ephemeron-value", "ephemeron?", 0, argc, argv);
  v = scheme_ephemeron_value(argv[0]);

  if (!v)
    return ((argc > 1) ? argv[1] : scheme_false);
  else
    return v;
}

static Scheme_Object *ephemeronp(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_ephemeron_type)
	  ? scheme_true 
	  : scheme_false);
}

static Scheme_Object *impersonator_ephemeron(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj = argv[0];

  if (SCHEME_CHAPERONEP(obj)) {
    return scheme_make_ephemeron(SCHEME_CHAPERONE_VAL(obj), obj);
  } else {
    /* This is a useless ephemeron, but we create one for consistency
       with the case that we have an impersonator: */
    return scheme_make_ephemeron(obj, obj);
  }
}

#ifndef MZ_PRECISE_GC

int scheme_propagate_ephemeron_marks()
{
  return mark_ephemerons();
}

void scheme_clear_ephemerons()
{
  Scheme_Ephemeron *e;

  for (e = ephemerons; e; e = e->next) {
    e->val = NULL;
    e->key = NULL;
  }

  ephemerons = done_ephemerons;
  done_ephemerons = NULL;
}

extern MZGC_DLLIMPORT void (*GC_custom_finalize)();

void scheme_init_ephemerons(void)
{
  /* symbol.c will overwrite this, later */
  GC_custom_finalize = scheme_clear_ephemerons;
}

#endif

/************************************************************/
/*                        unsafe                            */
/************************************************************/

static Scheme_Object *unsafe_cons_list(int argc, Scheme_Object *argv[])
{
  return lcons(argv[0], argv[1]);
}


static Scheme_Object *unsafe_car (int argc, Scheme_Object *argv[])
{
  if (scheme_current_thread->constant_folding) return scheme_checked_car(argc, argv);
  return SCHEME_CAR(argv[0]);
}

static Scheme_Object *unsafe_cdr (int argc, Scheme_Object *argv[])
{
  if (scheme_current_thread->constant_folding) return scheme_checked_cdr(argc, argv);
  return SCHEME_CDR(argv[0]);
}

static Scheme_Object *unsafe_list_ref (int argc, Scheme_Object *argv[])
{
  int i;
  Scheme_Object *v;

  if (scheme_current_thread->constant_folding) return scheme_checked_list_ref(argc, argv);
  
  v = argv[0];
  i = SCHEME_INT_VAL(argv[1]);
  while (i--) {
    v = SCHEME_CDR(v);
  }
  
  return SCHEME_CAR(v);
}

static Scheme_Object *unsafe_list_tail (int argc, Scheme_Object *argv[])
{
  int i;
  Scheme_Object *v;

  if (scheme_current_thread->constant_folding) return scheme_checked_list_tail(argc, argv);
  
  v = argv[0];
  i = SCHEME_INT_VAL(argv[1]);
  while (i--) {
    v = SCHEME_CDR(v);
  }
  
  return v;
}

static Scheme_Object *unsafe_mcar (int argc, Scheme_Object *argv[])
{
  return SCHEME_CAR(argv[0]);
}

static Scheme_Object *unsafe_mcdr (int argc, Scheme_Object *argv[])
{
  return SCHEME_CDR(argv[0]);
}

static Scheme_Object *unsafe_set_mcar (int argc, Scheme_Object *argv[])
{
  SCHEME_CAR(argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *unsafe_set_mcdr (int argc, Scheme_Object *argv[])
{
  SCHEME_CDR(argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *unsafe_unbox_star (int argc, Scheme_Object *argv[])
{
  return SCHEME_BOX_VAL(argv[0]);
}

static Scheme_Object *unsafe_unbox (int argc, Scheme_Object *argv[])
{
  if (SCHEME_NP_CHAPERONEP(argv[0]))
    return chaperone_unbox(argv[0]);
  else
    return SCHEME_BOX_VAL(argv[0]);
}

static Scheme_Object *unsafe_set_box_star (int argc, Scheme_Object *argv[])
{
  SCHEME_BOX_VAL(argv[0]) = argv[1];
  return scheme_void;
}

static Scheme_Object *unsafe_set_box (int argc, Scheme_Object *argv[])
{
  if (SCHEME_NP_CHAPERONEP(argv[0]))
    chaperone_set_box(argv[0], argv[1]);
  else
    SCHEME_BOX_VAL(argv[0]) = argv[1];
  return scheme_void;
}

/************************************************************/
/*               unsafe hash table iteration ops            */
/************************************************************/

/* unsafe_hash_table, ie SCHEME_HASHTP, ops */
Scheme_Object *unsafe_hash_table_iterate_start(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  
  if (SCHEME_NP_CHAPERONEP(o)) o = SCHEME_CHAPERONE_VAL(o);

  return scheme_hash_table_next((Scheme_Hash_Table *)o, -1);
}

Scheme_Object *unsafe_hash_table_iterate_next(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0], *res;
  
  if (SCHEME_NP_CHAPERONEP(o)) o = SCHEME_CHAPERONE_VAL(o);

  res = scheme_hash_table_next((Scheme_Hash_Table *)o, SCHEME_INT_VAL(argv[1]));
  
  if (res)
    return res;
  else if (0) {
    scheme_contract_error("unsafe-mutable-hash-iterate-next", 
			  "no element at index", 
			  "index", 1, argv[1],
			  NULL);
    return NULL;
  } else
    return scheme_false;
}

Scheme_Object *unsafe_hash_table_iterate_key(int argc, Scheme_Object *argv[])
  XFORM_ASSERT_NO_CONVERSION
{
  GC_CAN_IGNORE const char *name = "unsafe-mutable-hash-iterate-key";
  Scheme_Object *obj = argv[0], *key;
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);
  
  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Hash_Table *ht = (Scheme_Hash_Table *)SCHEME_CHAPERONE_VAL(obj);
    if (scheme_hash_table_index(ht, pos, &key, NULL))
      return chaperone_hash_key(name, obj, key);
  } else {
    if (scheme_hash_table_index((Scheme_Hash_Table *)obj, pos, &key, NULL))
      return key;
  }

  if (argc > 2)
    return argv[2];
  else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}

static Scheme_Object *unsafe_hash_table_iterate_value_slow(int argc, Scheme_Object *argv[])
{
  GC_CAN_IGNORE const char *name = "unsafe-mutable-hash-iterate-value";
  Scheme_Object *obj = argv[0], *key, *val;
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);
  
  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Object *chap_key, *chap_val;
    Scheme_Hash_Table *ht = (Scheme_Hash_Table *)SCHEME_CHAPERONE_VAL(obj);
    if (scheme_hash_table_index(ht, pos, &key, NULL)) {
      chaperone_hash_key_value(name, obj, key, &chap_key, &chap_val, 0);
      return chap_val;
    }
  } else {
    if (scheme_hash_table_index((Scheme_Hash_Table *)obj, pos, &key, &val))
      return val;
  }

  if (argc > 2)
    return argv[2];
  else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}

Scheme_Object *unsafe_hash_table_iterate_value(int argc, Scheme_Object *argv[])
  XFORM_ASSERT_NO_CONVERSION
{
  Scheme_Object *obj = argv[0], *key, *val;
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);

  if (!SCHEME_NP_CHAPERONEP(obj)
      && scheme_hash_table_index((Scheme_Hash_Table *)obj, pos, &key, &val))
    return val;
  else
    return unsafe_hash_table_iterate_value_slow(argc, argv);
}

Scheme_Object *unsafe_hash_table_iterate_pair(int argc, Scheme_Object *argv[])
{
  GC_CAN_IGNORE const char *name = "unsafe-mutable-hash-iterate-pair";
  Scheme_Object *obj = argv[0], *key, *val;
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);

  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Object *chap_key, *chap_val;
    Scheme_Hash_Table *ht = (Scheme_Hash_Table *)SCHEME_CHAPERONE_VAL(obj);
    if (scheme_hash_table_index(ht, pos, &key, NULL)) {
      chaperone_hash_key_value(name, obj, key, &chap_key, &chap_val, 0);
      return scheme_make_pair(chap_key, chap_val);
    }
  } else {
    if (scheme_hash_table_index((Scheme_Hash_Table *)obj, pos, &key, &val))
      return scheme_make_pair(key, val);
  }

  if (argc > 2)
    return scheme_make_pair(argv[2], argv[2]);
  else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}

Scheme_Object *unsafe_hash_table_iterate_key_value(int argc, Scheme_Object *argv[])
{
  GC_CAN_IGNORE const char *name = "unsafe-mutable-hash-iterate-key+value";
  Scheme_Object *obj = argv[0], *res[2];
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);

  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Object *key;
    Scheme_Hash_Table *ht = (Scheme_Hash_Table *)SCHEME_CHAPERONE_VAL(obj);
    if (scheme_hash_table_index(ht, pos, &key, NULL)) {
      chaperone_hash_key_value(name, obj, key, &res[0], &res[1], 0);
      return scheme_values(2, res);
    }
  } else {
    if (scheme_hash_table_index((Scheme_Hash_Table *)obj, pos, &res[0], &res[1]))
      return scheme_values(2, res);
  }
  
  if (argc > 2) {
    res[0] = argv[2];
    res[1] = argv[2];
    return scheme_values(2, res);
  } else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}

/* unsafe_hash_tree, ie SCHEME_HASHTRP, ops */
Scheme_Object *unsafe_hash_tree_iterate_start(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  
  if (SCHEME_NP_CHAPERONEP(o)) o = SCHEME_CHAPERONE_VAL(o);

  return scheme_unsafe_hash_tree_start((Scheme_Hash_Tree *)o);
}

Scheme_Object *unsafe_hash_tree_iterate_next(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  
  if (SCHEME_NP_CHAPERONEP(o)) o = SCHEME_CHAPERONE_VAL(o);

  return scheme_unsafe_hash_tree_next((Scheme_Hash_Tree *)o, argv[1]);
}

Scheme_Object *unsafe_hash_tree_iterate_key(int argc, Scheme_Object *argv[])
  XFORM_ASSERT_NO_CONVERSION
{
  Scheme_Object *obj = argv[0], *args = argv[1], *key;
  Scheme_Hash_Tree *subtree;
  int i;
  
  scheme_unsafe_hash_tree_subtree(obj, args, &subtree, &i);
  key = subtree->els[i];
  
  if (SCHEME_NP_CHAPERONEP(obj))
    return chaperone_hash_key("unsafe-immutable-hash-iterate-key", obj, key);
  else
    return key;
}

static Scheme_Object *unsafe_hash_tree_iterate_value_slow(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj = argv[0], *args = argv[1];
  Scheme_Hash_Tree *subtree;
  int i;

  scheme_unsafe_hash_tree_subtree(obj, args, &subtree, &i);

  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Object *chap_key, *chap_val;
    chaperone_hash_key_value("unsafe-immutable-hash-iterate-value",
                             obj, subtree->els[i], &chap_key, &chap_val, 0);
    return chap_val;
  } else
    return scheme_unsafe_hash_tree_access(subtree, i);
}

Scheme_Object *unsafe_hash_tree_iterate_value(int argc, Scheme_Object *argv[])
  XFORM_ASSERT_NO_CONVERSION
{
  Scheme_Object *obj = argv[0], *args = argv[1];
  Scheme_Hash_Tree *subtree;
  int i;

  if (!SCHEME_NP_CHAPERONEP(obj)) {
    scheme_unsafe_hash_tree_subtree(obj, args, &subtree, &i);
    return scheme_unsafe_hash_tree_access(subtree, i);
  } else
    return unsafe_hash_tree_iterate_value_slow(argc, argv);
}

Scheme_Object *unsafe_hash_tree_iterate_pair(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj = argv[0], *args = argv[1], *key;
  Scheme_Hash_Tree *subtree;
  int i;

  scheme_unsafe_hash_tree_subtree(obj, args, &subtree, &i);
  key = subtree->els[i];

  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Object *chap_key, *chap_val;
    chaperone_hash_key_value("unsafe-immutable-hash-iterate-pair",
                             obj, subtree->els[i], &chap_key, &chap_val, 0);
    return scheme_make_pair(chap_key, chap_val);
  } else
    return scheme_make_pair(key, scheme_unsafe_hash_tree_access(subtree, i));
}

Scheme_Object *unsafe_hash_tree_iterate_key_value(int argc, Scheme_Object *argv[])
{
  Scheme_Object *obj = argv[0], *args = argv[1], *key, *res[2];
  Scheme_Hash_Tree *subtree;
  int i;

  scheme_unsafe_hash_tree_subtree(obj, args, &subtree, &i);
  key = subtree->els[i];

  if (SCHEME_NP_CHAPERONEP(obj)) {
    chaperone_hash_key_value("unsafe-immutable-hash-iterate-key+value",
                             obj, subtree->els[i], &res[0], &res[1], 0);
  } else {
    res[0] = key;
    res[1] = scheme_unsafe_hash_tree_access(subtree, i);
  }

  return scheme_values(2, res);
}

/* unsafe_bucket_table, ie SCHEME_BUCKTP, ops */
Scheme_Object *unsafe_bucket_table_iterate_start(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0];
  
  if (SCHEME_NP_CHAPERONEP(o)) o = SCHEME_CHAPERONE_VAL(o);

  return scheme_bucket_table_next((Scheme_Bucket_Table *)o, -1);
}

Scheme_Object *unsafe_bucket_table_iterate_next(int argc, Scheme_Object *argv[])
{
  Scheme_Object *o = argv[0], *res;

  if (SCHEME_NP_CHAPERONEP(o)) o = SCHEME_CHAPERONE_VAL(o);

  res = scheme_bucket_table_next((Scheme_Bucket_Table *)o,
				 SCHEME_INT_VAL(argv[1]));
  if (res)
    return res;
  else if (0) {
    scheme_contract_error("unsafe-weak-hash-iterate-next", 
			  "no element at index", 
			  "index", 1, argv[1],
			  NULL);
    return NULL;
  } else
    return scheme_false;
}

Scheme_Object *unsafe_bucket_table_iterate_key(int argc, Scheme_Object *argv[])
{
  GC_CAN_IGNORE const char *name = "unsafe-weak-hash-iterate-key";
  Scheme_Object *obj = argv[0], *key;
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);
  
  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)SCHEME_CHAPERONE_VAL(obj);
    if (scheme_bucket_table_index(ht, pos, &key, NULL))
      return chaperone_hash_key(name, obj, key);
  } else {
    if (scheme_bucket_table_index((Scheme_Bucket_Table *)obj, pos, &key, NULL))
      return key;
  }

  if (argc > 2)
    return argv[2];
  else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}

Scheme_Object *unsafe_bucket_table_iterate_value(int argc, Scheme_Object *argv[])
{
  GC_CAN_IGNORE const char *name = "unsafe-weak-hash-iterate-value";
  Scheme_Object *obj = argv[0], *key, *val;
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);
  
  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)SCHEME_CHAPERONE_VAL(obj);
    Scheme_Object *chap_key, *chap_val;
    if (scheme_bucket_table_index(ht, pos, &key, NULL)) {
      chaperone_hash_key_value(name, obj, key, &chap_key, &chap_val, 0);
      return chap_val;
    }
  } else {
    if (scheme_bucket_table_index((Scheme_Bucket_Table *)obj, pos, &key, &val))
      return val;
  }

  if (argc > 2)
    return argv[2];
  else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}

Scheme_Object *unsafe_bucket_table_iterate_pair(int argc, Scheme_Object *argv[])
{
  GC_CAN_IGNORE const char *name = "unsafe-weak-hash-iterate-pair";
  Scheme_Object *obj = argv[0], *key, *val;
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);

  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)SCHEME_CHAPERONE_VAL(obj);
    Scheme_Object *chap_key, *chap_val;
    if (scheme_bucket_table_index(ht, pos, &key, NULL)) {
      chaperone_hash_key_value(name, obj, key, &chap_key, &chap_val, 0);
      return scheme_make_pair(chap_key, chap_val);
    }
  } else {
    if(scheme_bucket_table_index((Scheme_Bucket_Table *)obj, pos, &key, &val))
      return scheme_make_pair(key, val);
  }

  if (argc > 2)
    return scheme_make_pair(argv[2], argv[2]);
  else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}

Scheme_Object *unsafe_bucket_table_iterate_key_value(int argc, Scheme_Object *argv[])
{
  GC_CAN_IGNORE const char *name = "unsafe-weak-hash-iterate-key+value";
  Scheme_Object *obj = argv[0], *res[2];
  mzlonglong pos = SCHEME_INT_VAL(argv[1]);

  if (SCHEME_NP_CHAPERONEP(obj)) {
    Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)SCHEME_CHAPERONE_VAL(obj);
    Scheme_Object *key;
    if (scheme_bucket_table_index(ht, pos, &key, NULL)) {
      chaperone_hash_key_value(name, obj, key, &res[0], &res[1], 0);
      return scheme_values(2, res);
    }
  } else {
    Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)obj;
    if (scheme_bucket_table_index(ht, pos, &res[0], &res[1]))
      return scheme_values(2, res);
  }

  if (argc > 2) {
    res[0] = argv[2];
    res[1] = argv[2];
    return scheme_values(2, res);
  } else {
    scheme_contract_error(name, "no element at index", "index", 1, argv[1], NULL);
    return NULL;
  }
}
