
enum {

  /* compiled object types: (internal) */
  scheme_toplevel_type,                 /* 0 */
  scheme_local_type,                    /* 1 */
  scheme_local_unbox_type,              /* 2 */
  scheme_syntax_type,                   /* 3 */
  scheme_application_type,              /* 4 */
  scheme_application2_type,             /* 5 */
  scheme_application3_type,             /* 6 */
  scheme_sequence_type,                 /* 7 */
  scheme_branch_type,                   /* 8 */
  scheme_unclosed_procedure_type,       /* 9 */
  scheme_let_value_type,                /* 10 */
  scheme_let_void_type,                 /* 11 */
  scheme_letrec_type,                   /* 12 */
  scheme_let_one_type,                  /* 13 */
  scheme_with_cont_mark_type,           /* 14 */
  scheme_quote_syntax_type,             /* 15 */

  _scheme_values_types_, /* All following types are values */
  
  /* intermediate compiled: */
  scheme_compiled_unclosed_procedure_type,/* 17 */
  scheme_compiled_let_value_type,       /* 18 */
  scheme_compiled_let_void_type,        /* 19 */
  scheme_compiled_syntax_type,          /* 20 */
  scheme_compiled_toplevel_type,        /* 21 */
  scheme_compiled_quote_syntax_type,    /* 22 */

  scheme_quote_compilation_type, /* used while writing, only */

  /* Registered in prefix table: */
  scheme_variable_type,                 /* 24 */
  scheme_module_variable_type, /* link replaces with scheme_variable_type */

  _scheme_compiled_values_types_,       /* 26 */

  /* procedure types */
  scheme_prim_type,                     /* 27 */
  scheme_closed_prim_type,              /* 28 */
  scheme_closure_type,                  /* 29 */
  scheme_case_closure_type,             /* 30 */
  scheme_cont_type,                     /* 31 */
  scheme_escaping_cont_type,            /* 32 */
  scheme_proc_struct_type,              /* 33 */
  scheme_native_closure_type,           /* 34 */

  /* structure types (overlaps with procs) */
  scheme_structure_type,                /* 35 */

  /* basic types */
  scheme_char_type,                     /* 36 */
  scheme_integer_type,                  /* 37 */
  scheme_bignum_type,                   /* 38 */
  scheme_rational_type,                 /* 39 */
  scheme_float_type,                    /* 40 */
  scheme_double_type,                   /* 41 */
  scheme_complex_type,                  /* 42 */
  scheme_char_string_type,              /* 43 */
  scheme_byte_string_type,              /* 44 */
  scheme_unix_path_type,                /* 45 */
  scheme_windows_path_type,             /* 46 */
  scheme_symbol_type,                   /* 47 */
  scheme_keyword_type,                  /* 48 */
  scheme_null_type,                     /* 49 */
  scheme_pair_type,                     /* 50 */
  scheme_mutable_pair_type,             /* 51 */
  scheme_vector_type,                   /* 52 */
  scheme_inspector_type,                /* 53 */
  scheme_input_port_type,               /* 54 */
  scheme_output_port_type,              /* 55 */
  scheme_eof_type,                      /* 56 */
  scheme_true_type,                     /* 57 */
  scheme_false_type,                    /* 58 */
  scheme_void_type,                     /* 59 */
  scheme_syntax_compiler_type,          /* 60 */
  scheme_macro_type,                    /* 61 */
  scheme_box_type,                      /* 62 */
  scheme_thread_type,                   /* 63 */
  scheme_stx_offset_type,               /* 64 */
  scheme_cont_mark_set_type,            /* 65 */
  scheme_sema_type,                     /* 66 */
  scheme_hash_table_type,               /* 67 */
  scheme_hash_tree_type,                /* 68 */
  scheme_cpointer_type,                 /* 69 */
  scheme_offset_cpointer_type,          /* 70 */
  scheme_weak_box_type,                 /* 71 */
  scheme_ephemeron_type,                /* 72 */
  scheme_struct_type_type,              /* 73 */
  scheme_module_index_type,             /* 74 */
  scheme_set_macro_type,                /* 75 */
  scheme_listener_type,                 /* 76 */
  scheme_namespace_type,                /* 77 */
  scheme_config_type,                   /* 78 */
  scheme_stx_type,                      /* 79 */
  scheme_will_executor_type,            /* 80 */
  scheme_custodian_type,                /* 81 */
  scheme_random_state_type,             /* 82 */
  scheme_regexp_type,                   /* 83 */
  scheme_bucket_type,                   /* 84 */
  scheme_bucket_table_type,             /* 85 */
  scheme_subprocess_type,               /* 86 */
  scheme_compilation_top_type,          /* 87 */
  scheme_wrap_chunk_type,               /* 88 */
  scheme_eval_waiting_type,             /* 89 */
  scheme_tail_call_waiting_type,        /* 90 */
  scheme_undefined_type,                /* 91 */
  scheme_struct_property_type,          /* 92 */
  scheme_multiple_values_type,          /* 93 */
  scheme_placeholder_type,              /* 94 */
  scheme_table_placeholder_type,        /* 95 */
  scheme_case_lambda_sequence_type,     /* 96 */
  scheme_begin0_sequence_type,          /* 97 */
  scheme_rename_table_type,             /* 98 */
  scheme_rename_table_set_type,         /* 99 */
  scheme_module_type,                   /* 100 */
  scheme_svector_type,                  /* 101 */
  scheme_lazy_macro_type,               /* 102 */
  scheme_resolve_prefix_type,           /* 103 */
  scheme_security_guard_type,           /* 104 */
  scheme_indent_type,                   /* 105 */
  scheme_udp_type,                      /* 106 */
  scheme_udp_evt_type,                  /* 107 */
  scheme_tcp_accept_evt_type,           /* 108 */
  scheme_id_macro_type,                 /* 109 */
  scheme_evt_set_type,                  /* 110 */
  scheme_wrap_evt_type,                 /* 111 */
  scheme_handle_evt_type,               /* 112 */
  scheme_nack_guard_evt_type,           /* 113 */
  scheme_semaphore_repost_type,         /* 114 */
  scheme_channel_type,                  /* 115 */
  scheme_channel_put_type,              /* 116 */
  scheme_thread_resume_type,            /* 117 */
  scheme_thread_suspend_type,           /* 118 */
  scheme_thread_dead_type,              /* 119 */
  scheme_poll_evt_type,                 /* 120 */
  scheme_nack_evt_type,                 /* 121 */
  scheme_module_registry_type,          /* 122 */
  scheme_thread_set_type,               /* 123 */
  scheme_string_converter_type,         /* 124 */
  scheme_alarm_type,                    /* 125 */
  scheme_thread_recv_evt_type,          /* 126 */
  scheme_thread_cell_type,              /* 127 */
  scheme_channel_syncer_type,           /* 128 */
  scheme_special_comment_type,          /* 129 */
  scheme_write_evt_type,                /* 130 */
  scheme_always_evt_type,               /* 131 */
  scheme_never_evt_type,                /* 132 */
  scheme_progress_evt_type,             /* 133 */
  scheme_certifications_type,           /* 134 */
  scheme_already_comp_type,             /* 135 */
  scheme_readtable_type,                /* 136 */
  scheme_intdef_context_type,           /* 137 */
  scheme_lexical_rib_type,              /* 138 */
  scheme_thread_cell_values_type,       /* 139 */
  scheme_global_ref_type,               /* 140 */
  scheme_cont_mark_chain_type,          /* 141 */
  scheme_raw_pair_type,                 /* 142 */
  scheme_prompt_type,                   /* 143 */
  scheme_prompt_tag_type,               /* 144 */
  scheme_expanded_syntax_type,          /* 145 */
  scheme_delay_syntax_type,             /* 146 */
  scheme_cust_box_type,                 /* 147 */
  scheme_resolved_module_path_type,     /* 148 */
  scheme_module_phase_exports_type,     /* 149 */
  scheme_logger_type,                   /* 150 */
  scheme_log_reader_type,               /* 151 */

#ifdef MZTAG_REQUIRED
  _scheme_last_normal_type_,            /* 152 */

  scheme_rt_weak_array,                 /* 153 */

  scheme_rt_comp_env,                   /* 154 */
  scheme_rt_constant_binding,           /* 155 */
  scheme_rt_resolve_info,               /* 156 */
  scheme_rt_optimize_info,              /* 157 */
  scheme_rt_compile_info,               /* 158 */
  scheme_rt_cont_mark,                  /* 159 */
  scheme_rt_saved_stack,                /* 160 */
  scheme_rt_reply_item,                 /* 161 */
  scheme_rt_closure_info,               /* 162 */
  scheme_rt_overflow,                   /* 163 */
  scheme_rt_overflow_jmp,               /* 164 */
  scheme_rt_meta_cont,                  /* 165 */
  scheme_rt_dyn_wind_cell,              /* 166 */
  scheme_rt_dyn_wind_info,              /* 167 */
  scheme_rt_dyn_wind,                   /* 168 */
  scheme_rt_dup_check,                  /* 169 */
  scheme_rt_thread_memory,              /* 170 */
  scheme_rt_input_file,                 /* 171 */
  scheme_rt_input_fd,                   /* 172 */
  scheme_rt_oskit_console_input,        /* 173 */
  scheme_rt_tested_input_file,          /* 174 */
  scheme_rt_tested_output_file,         /* 175 */
  scheme_rt_indexed_string,             /* 176 */
  scheme_rt_output_file,                /* 177 */
  scheme_rt_load_handler_data,          /* 178 */
  scheme_rt_pipe,                       /* 179 */
  scheme_rt_beos_process,               /* 180 */
  scheme_rt_system_child,               /* 181 */
  scheme_rt_tcp,                        /* 182 */
  scheme_rt_write_data,                 /* 183 */
  scheme_rt_tcp_select_info,            /* 184 */
  scheme_rt_namespace_option,           /* 185 */
  scheme_rt_param_data,                 /* 186 */
  scheme_rt_will,                       /* 187 */
  scheme_rt_struct_proc_info,           /* 188 */
  scheme_rt_linker_name,                /* 189 */
  scheme_rt_param_map,                  /* 190 */
  scheme_rt_finalization,               /* 191 */
  scheme_rt_finalizations,              /* 192 */
  scheme_rt_cpp_object,                 /* 193 */
  scheme_rt_cpp_array_object,           /* 194 */
  scheme_rt_stack_object,               /* 195 */
  scheme_rt_preallocated_object,        /* 196 */
  scheme_thread_hop_type,               /* 197 */
  scheme_rt_srcloc,                     /* 198 */
  scheme_rt_evt,                        /* 199 */
  scheme_rt_syncing,                    /* 200 */
  scheme_rt_comp_prefix,                /* 201 */
  scheme_rt_user_input,                 /* 202 */
  scheme_rt_user_output,                /* 203 */
  scheme_rt_compact_port,               /* 204 */
  scheme_rt_read_special_dw,            /* 205 */
  scheme_rt_regwork,                    /* 206 */
  scheme_rt_buf_holder,                 /* 207 */
  scheme_rt_parameterization,           /* 208 */
  scheme_rt_print_params,               /* 209 */
  scheme_rt_read_params,                /* 210 */
  scheme_rt_native_code,                /* 211 */
  scheme_rt_native_code_plus_case,      /* 212 */
  scheme_rt_jitter_data,                /* 213 */
  scheme_rt_module_exports,             /* 214 */
  scheme_rt_delay_load_info,            /* 215 */
  scheme_rt_marshal_info,               /* 216 */
  scheme_rt_unmarshal_info,             /* 217 */
  scheme_rt_runstack,                   /* 218 */
  scheme_rt_sfs_info,                   /* 219 */
  scheme_rt_validate_clearing,          /* 220 */
  scheme_rt_rb_node,                    /* 221 */
#endif

  scheme_place_type,                    /* 222 */
  scheme_engine_type,                   /* 223 */

  _scheme_last_type_
};
