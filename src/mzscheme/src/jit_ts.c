#ifdef MZ_USE_FUTURES

# include "jit_ts_def.c"

/* s = Scheme_Object*
   i = int
   l = long
   S = Scheme_Object**
   v = void 
   b = Scheme_Bucket*
   n = Scheme_Native_Closure_Data* 
   p = void*, CGC only
   z = size_t
   m = MZ_MARK_STACK_TYPE */

define_ts_siS_s(_scheme_apply_multi_from_native, FSRC_RATOR)
define_ts_siS_s(_scheme_apply_from_native, FSRC_RATOR)
define_ts_siS_s(_scheme_tail_apply_from_native, FSRC_RATOR)
define_ts_s_s(scheme_force_value_same_mark, FSRC_OTHER)
define_ts_s_s(scheme_force_one_value_same_mark, FSRC_OTHER)
#if defined(INLINE_FP_OPS) && !defined(CAN_INLINE_ALLOC)
define_ts__s(malloc_double, FSRC_OTHER)
#endif
define_ts_s_s(scheme_box, FSRC_OTHER)
#ifndef CAN_INLINE_ALLOC
define_ts_ss_s(scheme_make_mutable_pair, FSRC_OTHER)
define_ts_Sl_s(make_list_star, FSRC_OTHER)
define_ts_Sl_s(make_list, FSRC_OTHER)
define_ts_ss_s(scheme_make_pair, FSRC_OTHER)
define_ts_s_s(make_one_element_ivector, FSRC_OTHER)
define_ts_s_s(make_one_element_vector, FSRC_OTHER)
define_ts_ss_s(make_two_element_ivector, FSRC_OTHER)
define_ts_ss_s(make_two_element_vector, FSRC_OTHER)
define_ts_l_s(make_ivector, FSRC_OTHER)
define_ts_l_s(make_vector, FSRC_OTHER)
#endif
#ifdef JIT_PRECISE_GC
define_ts_z_p(GC_malloc_one_small_dirty_tagged, FSRC_OTHER)
define_ts_z_p(GC_malloc_one_small_tagged, FSRC_OTHER)
#endif
define_ts_n_s(scheme_make_native_closure, FSRC_OTHER)
define_ts_n_s(scheme_make_native_case_closure, FSRC_OTHER)
define_ts_bsi_v(call_set_global_bucket, FSRC_OTHER)
define_ts_s_s(scheme_make_envunbox, FSRC_OTHER)
define_ts_s_s(make_global_ref, FSRC_OTHER)
define_ts_iiS_v(lexical_binding_wrong_return_arity, FSRC_OTHER)
define_ts_ss_m(scheme_set_cont_mark, FSRC_OTHER)
define_ts_iiS_v(call_wrong_return_arity, FSRC_OTHER)
define_ts_b_v(scheme_unbound_global, FSRC_OTHER)
define_ts_Sl_s(scheme_delayed_rename, FSRC_OTHER)
define_ts_iS_s(scheme_checked_car, FSRC_OTHER)
define_ts_iS_s(scheme_checked_cdr, FSRC_OTHER)
define_ts_iS_s(scheme_checked_caar, FSRC_OTHER)
define_ts_iS_s(scheme_checked_cadr, FSRC_OTHER)
define_ts_iS_s(scheme_checked_cdar, FSRC_OTHER)
define_ts_iS_s(scheme_checked_cddr, FSRC_OTHER)
define_ts_iS_s(scheme_checked_mcar, FSRC_OTHER)
define_ts_iS_s(scheme_checked_mcdr, FSRC_OTHER)
define_ts_iS_s(scheme_checked_set_mcar, FSRC_OTHER)
define_ts_iS_s(scheme_checked_set_mcdr, FSRC_OTHER)
define_ts_s_s(scheme_unbox, FSRC_OTHER)
define_ts_s_s(scheme_vector_length, FSRC_OTHER)
define_ts_s_s(scheme_flvector_length, FSRC_OTHER)
define_ts_s_s(tail_call_with_values_from_multiple_result, FSRC_OTHER)
define_ts_s_v(raise_bad_call_with_values, FSRC_OTHER)
define_ts_s_s(call_with_values_from_multiple_result_multi, FSRC_OTHER)
define_ts_s_s(call_with_values_from_multiple_result, FSRC_OTHER)
define_ts_iS_s(scheme_checked_vector_ref, FSRC_OTHER)
define_ts_iS_s(scheme_checked_vector_set, FSRC_OTHER)
define_ts_iS_s(scheme_checked_string_ref, FSRC_OTHER)
define_ts_iS_s(scheme_checked_string_set, FSRC_OTHER)
define_ts_iS_s(scheme_checked_byte_string_ref, FSRC_OTHER)
define_ts_iS_s(scheme_checked_byte_string_set, FSRC_OTHER)
define_ts_iS_s(scheme_checked_flvector_ref, FSRC_OTHER)
define_ts_iS_s(scheme_checked_flvector_set, FSRC_OTHER)
define_ts_iS_s(scheme_checked_syntax_e, FSRC_OTHER)
define_ts_iS_s(scheme_extract_checked_procedure, FSRC_OTHER)
define_ts_S_s(apply_checked_fail, FSRC_OTHER)
define_ts_iSi_s(scheme_build_list_offset, FSRC_OTHER)
define_ts_siS_v(wrong_argument_count, FSRC_OTHER)
#else
# define ts__scheme_apply_multi_from_native _scheme_apply_multi_from_native
# define ts__scheme_apply_from_native _scheme_apply_from_native
# define ts__scheme_tail_apply_from_native _scheme_tail_apply_from_native
# define ts__scheme_tail_apply_from_native_fixup_args _scheme_tail_apply_from_native_fixup_args
# define ts_scheme_force_value_same_mark scheme_force_value_same_mark
# define ts_scheme_force_one_value_same_mark scheme_force_one_value_same_mark
# define ts_scheme_force_value_same_mark scheme_force_value_same_mark
# define ts_scheme_force_one_value_same_mark scheme_force_one_value_same_mark
# define ts_malloc_double malloc_double
# define ts_scheme_box scheme_box
# define ts_scheme_make_mutable_pair scheme_make_mutable_pair
# define ts_make_list_star make_list_star
# define ts_make_list make_list
# define ts_scheme_make_pair scheme_make_pair
# define ts_make_one_element_ivector make_one_element_ivector
# define ts_make_one_element_vector make_one_element_vector
# define ts_make_two_element_ivector make_two_element_ivector
# define ts_make_two_element_vector make_two_element_vector
# define ts_make_ivector make_ivector
# define ts_make_vector make_vector
# define ts_GC_malloc_one_small_dirty_tagged GC_malloc_one_small_dirty_tagged
# define ts_GC_malloc_one_small_tagged GC_malloc_one_small_tagged
# define ts_scheme_make_native_closure scheme_make_native_closure
# define ts_scheme_make_native_case_closure scheme_make_native_case_closure
# define ts_call_set_global_bucket call_set_global_bucket
# define ts_scheme_make_envunbox scheme_make_envunbox
# define ts_make_global_ref make_global_ref
# define ts_lexical_binding_wrong_return_arity lexical_binding_wrong_return_arity
# define ts_scheme_set_cont_mark scheme_set_cont_mark
# define ts_call_wrong_return_arity call_wrong_return_arity
# define ts_scheme_unbound_global scheme_unbound_global
# define ts_scheme_delayed_rename scheme_delayed_rename
# define ts_scheme_checked_car scheme_checked_car
# define ts_scheme_checked_cdr scheme_checked_cdr
# define ts_scheme_checked_caar scheme_checked_caar
# define ts_scheme_checked_cadr scheme_checked_cadr
# define ts_scheme_checked_cdar scheme_checked_cdar
# define ts_scheme_checked_cddr scheme_checked_cddr
# define ts_scheme_checked_mcar scheme_checked_mcar
# define ts_scheme_checked_mcdr scheme_checked_mcdr
# define ts_scheme_checked_set_mcar scheme_checked_set_mcar
# define ts_scheme_checked_set_mcdr scheme_checked_set_mcdr
# define ts_scheme_unbox scheme_unbox
# define ts_scheme_vector_length scheme_vector_length
# define ts_scheme_flvector_length scheme_flvector_length
# define ts_tail_call_with_values_from_multiple_result tail_call_with_values_from_multiple_result
# define ts_raise_bad_call_with_values raise_bad_call_with_values
# define ts_call_with_values_from_multiple_result_multi call_with_values_from_multiple_result_multi
# define ts_call_with_values_from_multiple_result call_with_values_from_multiple_result
# define ts_scheme_checked_vector_ref scheme_checked_vector_ref
# define ts_scheme_checked_vector_set scheme_checked_vector_set
# define ts_scheme_checked_string_ref scheme_checked_string_ref
# define ts_scheme_checked_string_set scheme_checked_string_set
# define ts_scheme_checked_byte_string_ref scheme_checked_byte_string_ref
# define ts_scheme_checked_byte_string_set scheme_checked_byte_string_set
# define ts_scheme_checked_flvector_ref scheme_checked_flvector_ref
# define ts_scheme_checked_flvector_set scheme_checked_flvector_set
# define ts_scheme_checked_syntax_e scheme_checked_syntax_e
# define ts_scheme_extract_checked_procedure scheme_extract_checked_procedure
# define ts_apply_checked_fail apply_checked_fail
# define ts_scheme_build_list_offset scheme_build_list_offset
# define ts_wrong_argument_count wrong_argument_count
#endif
