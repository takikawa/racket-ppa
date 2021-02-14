#include "schpriv.h"
#include "racket_version.h"
#include "schrktio.h"
#include <string.h>
#include <ctype.h>
#ifdef NO_ERRNO_GLOBAL
# define errno -1
#else
# include <errno.h>
#endif

#ifndef SCHEME_PLATFORM_LIBRARY_SUBPATH
# include "schsys.h"
#endif

#ifndef SPLS_SUFFIX
# define SPLS_SUFFIX ""
#endif

#include "schustr.inc"

#ifdef MACOS_UNICODE_SUPPORT
# define mzLOCALE_IS_UTF_8(s) (!s || !(*s))
#endif
#ifdef WINDOWS_UNICODE_SUPPORT
# define mzLOCALE_IS_UTF_8(s) (!s || !(*s))
#endif
#ifndef mzLOCALE_IS_UTF_8
# define mzLOCALE_IS_UTF_8(s) (!(rktio_convert_properties(scheme_rktio) & RKTIO_CONVERTER_SUPPORTED))
#endif

#ifdef WINDOWS_UNICODE_SUPPORT
# define WIN_UTF16_AS_WTF16(utf16) utf16
#else
# define WIN_UTF16_AS_WTF16(utf16) 0
#endif

#define mzICONV_KIND 0
#define mzUTF8_KIND 1
#define mzUTF8_TO_UTF16_KIND 2
#define mzUTF16_TO_UTF8_KIND 3

typedef struct Scheme_Converter {
  Scheme_Object so;
  short closed;
  short kind;
  rktio_converter_t *cd;
  int permissive, wtf;
  Scheme_Custodian_Reference *mref;
} Scheme_Converter;

Scheme_Object *scheme_system_type_proc;

static Scheme_Object *make_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_ci_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_lt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_ci_gt_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_titlecase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_foldcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_upcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_locale_downcase (int argc, Scheme_Object *argv[]);
static Scheme_Object *substring (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_append (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_append_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_copy_bang (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_to_immutable (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_c (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_kc (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_d (int argc, Scheme_Object *argv[]);
static Scheme_Object *string_normalize_kd (int argc, Scheme_Object *argv[]);

static Scheme_Object *make_shared_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *shared_byte_string (int argc, Scheme_Object *argv[]);

static Scheme_Object *make_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_p (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_length (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_eq (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_lt (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_gt (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_substring (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_append (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_list (int argc, Scheme_Object *argv[]);
static Scheme_Object *list_to_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_copy (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_copy_bang (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_fill (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_immutable (int argc, Scheme_Object *argv[]);

static Scheme_Object *byte_string_utf8_index (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_utf8_ref (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_utf8_length (int argc, Scheme_Object *argv[]);

static Scheme_Object *byte_string_to_char_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_char_string_locale (int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_to_char_string_latin1 (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_to_byte_string (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_to_byte_string_locale (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_to_byte_string_latin1 (int argc, Scheme_Object *argv[]);
static Scheme_Object *char_string_utf8_length (int argc, Scheme_Object *argv[]);

static Scheme_Object *version(int argc, Scheme_Object *argv[]);
static Scheme_Object *format(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_printf(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_eprintf(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_fprintf(int argc, Scheme_Object *argv[]);
static Scheme_Object *banner(int argc, Scheme_Object *argv[]);
static Scheme_Object *env_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_getenv_names(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[]);
static Scheme_Object *env_copy(int argc, Scheme_Object *argv[]);
static Scheme_Object *env_make(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_environment_variables(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_type(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[]);
static Scheme_Object *cmdline_args(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_locale(int argc, Scheme_Object *argv[]);
static Scheme_Object *locale_string_encoding(int argc, Scheme_Object *argv[]);
static Scheme_Object *system_language_country(int argc, Scheme_Object *argv[]);

static Scheme_Object *byte_string_open_converter(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_close_converter(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_convert(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_string_convert_end(int argc, Scheme_Object *argv[]);
static Scheme_Object *byte_converter_p(int argc, Scheme_Object *argv[]);

static Scheme_Object *path_lt (int argc, Scheme_Object *argv[]);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static int mz_char_strcmp(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, int locale, int size_shortcut);
static int mz_char_strcmp_ci(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, int locale, int size_shortcut);
static int mz_strcmp(const char *who, unsigned char *str1, intptr_t l1, unsigned char *str2, intptr_t l2);

XFORM_NONGCING static intptr_t utf8_decode_x(const unsigned char *s, intptr_t start, intptr_t end,
                                             unsigned int *us, intptr_t dstart, intptr_t dend,
                                             intptr_t *ipos, intptr_t *jpos,
                                             char compact, char utf16,
                                             int *state, int might_continue, int permissive, int wtf);
XFORM_NONGCING static intptr_t utf8_encode_x(const unsigned int *us, intptr_t start, intptr_t end,
                                             unsigned char *s, intptr_t dstart, intptr_t dend,
                                             intptr_t *_ipos, intptr_t *_opos, char utf16, int wtf);

static char *string_to_from_locale(int to_bytes,
				   char *in, intptr_t delta, intptr_t len,
				   intptr_t *olen, int perm,
				   int *no_cvt);

static void cache_locale_or_close(int to_bytes, rktio_converter_t *cd, char *le);

#define portable_isspace(x) (((x) < 128) && isspace(x))

ROSYM static Scheme_Object *sys_symbol, *sys_os_symbol, *sys_arch_symbol;
ROSYM static Scheme_Object *link_symbol, *machine_symbol, *vm_symbol, *gc_symbol;
ROSYM static Scheme_Object *so_suffix_symbol, *so_mode_symbol, *word_symbol;
ROSYM static Scheme_Object *os_symbol, *os_star_symbol, *arch_symbol;
ROSYM static Scheme_Object *fs_change_symbol, *target_machine_symbol, *cross_symbol;
ROSYM static Scheme_Object *racket_symbol, *cgc_symbol, *_3m_symbol, *cs_symbol;
ROSYM static Scheme_Object *force_symbol, *infer_symbol;
ROSYM static Scheme_Object *platform_3m_path, *platform_cgc_path, *platform_cs_path;
READ_ONLY static Scheme_Object *zero_length_char_string;
READ_ONLY static Scheme_Object *zero_length_char_immutable_string;
READ_ONLY static Scheme_Object *zero_length_byte_string;

SHARED_OK static char *embedding_banner;
SHARED_OK static Scheme_Object *vers_str;
SHARED_OK static Scheme_Object *banner_str;

THREAD_LOCAL_DECL(static Scheme_Object *fs_change_props);

THREAD_LOCAL_DECL(static char *cached_locale_encoding_name);
THREAD_LOCAL_DECL(struct rktio_converter_t *cached_locale_to_converter);
THREAD_LOCAL_DECL(struct rktio_converter_t *cached_locale_from_converter);

READ_ONLY static Scheme_Object *complete_symbol, *continues_symbol, *aborts_symbol, *error_symbol;

READ_ONLY Scheme_Object *scheme_string_p_proc;
READ_ONLY Scheme_Object *scheme_byte_string_p_proc;

READ_ONLY static int cross_compile_mode;

/* These two locale variables are only valid when reset_locale()
   is called after continuation marks (and hence parameterization)
   may have changed. Similarly, setlocale() is only up-to-date
   when reset_locale() has been called. */
THREAD_LOCAL_DECL(static int locale_on);
THREAD_LOCAL_DECL(static void *current_locale_name_ptr);
static void reset_locale(void);

#define current_locale_name ((const mzchar *)current_locale_name_ptr)

static const mzchar empty_char_string[1] = { 0 };
static const mzchar xes_char_string[2] = { 0x78787878, 0 };

void
scheme_init_string (Scheme_Startup_Env *env)
{
  Scheme_Object *p;

  REGISTER_SO(sys_symbol);
  REGISTER_SO(sys_os_symbol);
  REGISTER_SO(sys_arch_symbol);
  sys_symbol = scheme_intern_symbol(SYSTEM_TYPE_NAME);
  sys_os_symbol = scheme_intern_symbol(SCHEME_OS);
  sys_arch_symbol = scheme_intern_symbol(SCHEME_ARCH);

  REGISTER_SO(link_symbol);
  REGISTER_SO(machine_symbol);
  REGISTER_SO(gc_symbol);
  REGISTER_SO(vm_symbol);
  REGISTER_SO(so_suffix_symbol);
  REGISTER_SO(so_mode_symbol);
  REGISTER_SO(word_symbol);
  REGISTER_SO(os_symbol);
  REGISTER_SO(os_star_symbol);
  REGISTER_SO(arch_symbol);
  REGISTER_SO(fs_change_symbol);
  REGISTER_SO(target_machine_symbol);
  REGISTER_SO(cross_symbol);
  link_symbol = scheme_intern_symbol("link");
  machine_symbol = scheme_intern_symbol("machine");
  vm_symbol = scheme_intern_symbol("vm");
  gc_symbol = scheme_intern_symbol("gc");
  so_suffix_symbol = scheme_intern_symbol("so-suffix");
  so_mode_symbol = scheme_intern_symbol("so-mode");
  word_symbol = scheme_intern_symbol("word");
  os_symbol = scheme_intern_symbol("os");
  os_star_symbol = scheme_intern_symbol("os*");
  arch_symbol = scheme_intern_symbol("arch");
  fs_change_symbol = scheme_intern_symbol("fs-change");
  target_machine_symbol = scheme_intern_symbol("target-machine");
  cross_symbol = scheme_intern_symbol("cross");

  REGISTER_SO(racket_symbol);
  REGISTER_SO(cgc_symbol);
  REGISTER_SO(_3m_symbol);
  REGISTER_SO(cs_symbol);
  racket_symbol = scheme_intern_symbol("racket");
  cgc_symbol = scheme_intern_symbol("cgc");
  _3m_symbol = scheme_intern_symbol("3m");
  cs_symbol = scheme_intern_symbol("cs");

  REGISTER_SO(force_symbol);
  REGISTER_SO(infer_symbol);
  force_symbol = scheme_intern_symbol("force");
  infer_symbol = scheme_intern_symbol("infer");

  REGISTER_SO(zero_length_char_string);
  REGISTER_SO(zero_length_char_immutable_string);
  REGISTER_SO(zero_length_byte_string);
  zero_length_char_string = scheme_alloc_char_string(0, 0);
  zero_length_char_immutable_string = scheme_alloc_char_string(0, 0);
  SCHEME_SET_CHAR_STRING_IMMUTABLE(zero_length_char_immutable_string);
  zero_length_byte_string = scheme_alloc_byte_string(0, 0);

  REGISTER_SO(complete_symbol);
  REGISTER_SO(continues_symbol);
  REGISTER_SO(aborts_symbol);
  REGISTER_SO(error_symbol);
  complete_symbol = scheme_intern_symbol("complete");
  continues_symbol = scheme_intern_symbol("continues");
  aborts_symbol = scheme_intern_symbol("aborts");
  error_symbol = scheme_intern_symbol("error");

  REGISTER_SO(platform_3m_path);
# ifdef DOS_FILE_SYSTEM
#  define MZ3M_SUBDIR "\\3m"
#  define MZCS_SUBDIR "\\cs"
# else
#  define MZ3M_SUBDIR "/3m"
#  define MZCS_SUBDIR "/cs"
#endif
  REGISTER_SO(platform_3m_path);
  REGISTER_SO(platform_cgc_path);
  REGISTER_SO(platform_cs_path);
  platform_cgc_path = scheme_make_path(SCHEME_PLATFORM_LIBRARY_SUBPATH SPLS_SUFFIX);
  platform_3m_path = scheme_make_path(SCHEME_PLATFORM_LIBRARY_SUBPATH SPLS_SUFFIX MZ3M_SUBDIR);
  platform_cs_path = scheme_make_path(SCHEME_PLATFORM_LIBRARY_SUBPATH SPLS_SUFFIX MZCS_SUBDIR);

  REGISTER_SO(embedding_banner);
  REGISTER_SO(vers_str);
  REGISTER_SO(banner_str);

  vers_str = scheme_make_utf8_string(scheme_version());
  SCHEME_SET_CHAR_STRING_IMMUTABLE(vers_str);
  banner_str = scheme_make_utf8_string(scheme_banner());
  SCHEME_SET_CHAR_STRING_IMMUTABLE(banner_str);

  REGISTER_SO(scheme_string_p_proc);
  p = scheme_make_folding_prim(string_p, "string?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("string?", p, env);
  scheme_string_p_proc = p;

  scheme_addto_prim_instance("make-string",
			     scheme_make_immed_prim(make_string,
						    "make-string",
						    1, 2),
			     env);
  scheme_addto_prim_instance("string",
			     scheme_make_immed_prim(string,
						    "string",
						    0, -1),
			     env);
  
  p = scheme_make_folding_prim(string_length, "string-length", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            |SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("string-length", p,
			     env);

  p = scheme_make_immed_prim(scheme_checked_string_ref, "string-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("string-ref", p, env);

  p = scheme_make_immed_prim(scheme_checked_string_set, "string-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("string-set!", p, env);

  p = scheme_make_immed_prim(string_eq, "string=?", 1, -1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_BOOL
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("string=?", p, env);

  scheme_addto_prim_instance("string-locale=?",
			     scheme_make_immed_prim(string_locale_eq,
						    "string-locale=?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-ci=?",
			     scheme_make_immed_prim(string_ci_eq,
						    "string-ci=?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-locale-ci=?",
			     scheme_make_immed_prim(string_locale_ci_eq,
						    "string-locale-ci=?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string<?",
			     scheme_make_immed_prim(string_lt,
						    "string<?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-locale<?",
			     scheme_make_immed_prim(string_locale_lt,
						    "string-locale<?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string>?",
			     scheme_make_immed_prim(string_gt,
						    "string>?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-locale>?",
			     scheme_make_immed_prim(string_locale_gt,
						    "string-locale>?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string<=?",
			     scheme_make_immed_prim(string_lt_eq,
						    "string<=?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string>=?",
			     scheme_make_immed_prim(string_gt_eq,
						    "string>=?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-ci<?",
			     scheme_make_immed_prim(string_ci_lt,
						    "string-ci<?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-locale-ci<?",
			     scheme_make_immed_prim(string_locale_ci_lt,
						    "string-locale-ci<?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-ci>?",
			     scheme_make_immed_prim(string_ci_gt,
						    "string-ci>?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-locale-ci>?",
			     scheme_make_immed_prim(string_locale_ci_gt,
						    "string-locale-ci>?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-ci<=?",
			     scheme_make_immed_prim(string_ci_lt_eq,
						    "string-ci<=?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("string-ci>=?",
			     scheme_make_immed_prim(string_ci_gt_eq,
						    "string-ci>=?",
						    1, -1),
			     env);

  scheme_addto_prim_instance("substring",
			     scheme_make_immed_prim(substring,
						    "substring",
						    2, 3),
			     env);

  p = scheme_make_immed_prim(string_append, "string-append", 0, -1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("string-append", p, env);

  p = scheme_make_immed_prim(string_append_immutable, "string-append-immutable", 0, -1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("string-append-immutable", p, env);

  scheme_addto_prim_instance("string->list",
			     scheme_make_immed_prim(string_to_list,
						    "string->list",
						    1, 1),
			     env);
  scheme_addto_prim_instance("list->string",
			     scheme_make_immed_prim(list_to_string,
						    "list->string",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-copy",
			     scheme_make_immed_prim(string_copy,
						    "string-copy",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-copy!",
			     scheme_make_immed_prim(string_copy_bang,
						    "string-copy!",
						    3, 5),
			     env);
  scheme_addto_prim_instance("string-fill!",
			     scheme_make_immed_prim(string_fill,
						    "string-fill!",
						    2, 2),
			     env);

  p = scheme_make_immed_prim(string_to_immutable, "string->immutable-string", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("string->immutable-string", p, env);
  
  scheme_addto_prim_instance("string-normalize-nfc",
			     scheme_make_immed_prim(string_normalize_c,
						    "string-normalize-nfc",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-normalize-nfkc",
			     scheme_make_immed_prim(string_normalize_kc,
						    "string-normalize-nfkc",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-normalize-nfd",
			     scheme_make_immed_prim(string_normalize_d,
						    "string-normalize-nfd",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-normalize-nfkd",
			     scheme_make_immed_prim(string_normalize_kd,
						    "string-normalize-nfkd",
						    1, 1),
			     env);

  scheme_addto_prim_instance("string-upcase",
			     scheme_make_immed_prim(string_upcase,
						    "string-upcase",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-downcase",
			     scheme_make_immed_prim(string_downcase,
						    "string-downcase",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-titlecase",
			     scheme_make_immed_prim(string_titlecase,
						    "string-titlecase",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-foldcase",
			     scheme_make_immed_prim(string_foldcase,
						    "string-foldcase",
						    1, 1),
			     env);

  scheme_addto_prim_instance("string-locale-upcase",
			     scheme_make_immed_prim(string_locale_upcase,
						    "string-locale-upcase",
						    1, 1),
			     env);
  scheme_addto_prim_instance("string-locale-downcase",
			     scheme_make_immed_prim(string_locale_downcase,
						    "string-locale-downcase",
						    1, 1),
			     env);

  scheme_addto_prim_instance("current-locale",
			     scheme_register_parameter(current_locale,
						       "current-locale",
						       MZCONFIG_LOCALE),
			     env);
  scheme_addto_prim_instance("locale-string-encoding",
			     scheme_make_immed_prim(locale_string_encoding,
						    "locale-string-encoding",
						    0, 0),
			     env);
  scheme_addto_prim_instance("system-language+country",
			     scheme_make_immed_prim(system_language_country,
						    "system-language+country",
						    0, 0),
			     env);

  scheme_addto_prim_instance("bytes-converter?",
			     scheme_make_immed_prim(byte_converter_p,
						    "bytes-converter?",
						    1, 1),
			     env);
  scheme_addto_prim_instance("bytes-convert",
			     scheme_make_prim_w_arity2(byte_string_convert,
						       "bytes-convert",
						       2, 7,
						       3, 3),
			     env);
  scheme_addto_prim_instance("bytes-convert-end",
			     scheme_make_prim_w_arity2(byte_string_convert_end,
						       "bytes-convert-end",
						       1, 4,
						       2, 2),
			     env);
  scheme_addto_prim_instance("bytes-open-converter",
			     scheme_make_immed_prim(byte_string_open_converter,
						    "bytes-open-converter",
						    2, 2),
			     env);
  scheme_addto_prim_instance("bytes-close-converter",
			     scheme_make_immed_prim(byte_string_close_converter,
						    "bytes-close-converter",
						    1, 1),
			     env);

  scheme_addto_prim_instance("format",
			     scheme_make_noncm_prim(format,
                                                    "format",
                                                    1, -1),
			     env);
  scheme_addto_prim_instance("printf",
			     scheme_make_noncm_prim(sch_printf,
                                                    "printf",
                                                    1, -1),
			     env);
  scheme_addto_prim_instance("eprintf",
			     scheme_make_noncm_prim(sch_eprintf,
                                                    "eprintf",
                                                    1, -1),
			     env);
  scheme_addto_prim_instance("fprintf",
			     scheme_make_noncm_prim(sch_fprintf,
                                                    "fprintf",
                                                    2, -1),
			     env);

  scheme_addto_prim_instance("byte?",
			     scheme_make_folding_prim(byte_p,
						      "byte?",
						      1, 1, 1),
			     env);

  REGISTER_SO(scheme_byte_string_p_proc);
  p = scheme_make_folding_prim(byte_string_p, "bytes?", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_IS_OMITABLE
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("bytes?", p, env);
  scheme_byte_string_p_proc = p;

  scheme_addto_prim_instance("make-bytes",
			     scheme_make_immed_prim(make_byte_string,
						    "make-bytes",
						    1, 2),
			     env);
  scheme_addto_prim_instance("bytes",
			     scheme_make_immed_prim(byte_string,
						    "bytes",
						    0, -1),
			     env);

  ADD_PRIM_W_ARITY("make-shared-bytes", make_shared_byte_string, 1, 2, env);
  ADD_PRIM_W_ARITY("shared-bytes", shared_byte_string, 0, -1, env);

  p = scheme_make_folding_prim(byte_string_length, "bytes-length", 1, 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_UNARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("bytes-length", p, env);

  p = scheme_make_immed_prim(scheme_checked_byte_string_ref, "bytes-ref", 2, 2);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_FIXNUM
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("bytes-ref", p, env);

  p = scheme_make_immed_prim(scheme_checked_byte_string_set, "bytes-set!", 3, 3);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_NARY_INLINED
                                                            | SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("bytes-set!", p, env);

  p = scheme_make_immed_prim(byte_string_eq, "bytes=?", 1, -1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_IS_BINARY_INLINED
                                                            | SCHEME_PRIM_PRODUCES_BOOL);
  scheme_addto_prim_instance("bytes=?", p, env);

  scheme_addto_prim_instance("bytes<?",
			     scheme_make_immed_prim(byte_string_lt,
						    "bytes<?",
						    1, -1),
			     env);
  scheme_addto_prim_instance("bytes>?",
			     scheme_make_immed_prim(byte_string_gt,
						    "bytes>?",
						    1, -1),
			     env);

  scheme_addto_prim_instance("subbytes",
			     scheme_make_immed_prim(byte_substring,
						    "subbytes",
						    2, 3),
			     env);

  p = scheme_make_immed_prim(byte_string_append, "bytes-append", 0, -1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("bytes-append", p, env);
  
  scheme_addto_prim_instance("bytes->list",
			     scheme_make_immed_prim(byte_string_to_list,
						    "bytes->list",
						    1, 1),
			     env);
  scheme_addto_prim_instance("list->bytes",
			     scheme_make_immed_prim(list_to_byte_string,
						    "list->bytes",
						    1, 1),
			     env);
  scheme_addto_prim_instance("bytes-copy",
			     scheme_make_immed_prim(byte_string_copy,
						    "bytes-copy",
						    1, 1),
			     env);
  scheme_addto_prim_instance("bytes-copy!",
			     scheme_make_immed_prim(byte_string_copy_bang,
						    "bytes-copy!",
						    3, 5),
			     env);
  scheme_addto_prim_instance("bytes-fill!",
			     scheme_make_immed_prim(byte_string_fill,
						    "bytes-fill!",
						    2, 2),
			     env);

  p = scheme_make_immed_prim(byte_string_to_immutable, "bytes->immutable-bytes", 1, 1);
  SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_AD_HOC_OPT);
  scheme_addto_prim_instance("bytes->immutable-bytes", p, env);

  p = scheme_make_immed_prim(byte_string_utf8_index, "bytes-utf-8-index", 2, 5);
  /* Incorrect, since the result can be #f:
     SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_PRODUCES_FIXNUM); */
  scheme_addto_prim_instance("bytes-utf-8-index", p, env);

  p = scheme_make_immed_prim(byte_string_utf8_length, "bytes-utf-8-length", 1, 4);
  /* Incorrect, since the result can be #f:
     SCHEME_PRIM_PROC_FLAGS(p) |= scheme_intern_prim_opt_flags(SCHEME_PRIM_PRODUCES_FIXNUM); */
  scheme_addto_prim_instance("bytes-utf-8-length", p, env);

  scheme_addto_prim_instance("bytes-utf-8-ref",
			     scheme_make_immed_prim(byte_string_utf8_ref,
						    "bytes-utf-8-ref",
						    2, 5),
			     env);

  scheme_addto_prim_instance("bytes->string/utf-8",
			     scheme_make_immed_prim(byte_string_to_char_string,
						    "bytes->string/utf-8",
						    1, 4),
			     env);
  scheme_addto_prim_instance("bytes->string/locale",
			     scheme_make_immed_prim(byte_string_to_char_string_locale,
						    "bytes->string/locale",
						    1, 4),
			     env);
  scheme_addto_prim_instance("bytes->string/latin-1",
			     scheme_make_immed_prim(byte_string_to_char_string_latin1,
						    "bytes->string/latin-1",
						    1, 4),
			     env);
  scheme_addto_prim_instance("string->bytes/utf-8",
			     scheme_make_immed_prim(char_string_to_byte_string,
						    "string->bytes/utf-8",
						    1, 4),
			     env);
  scheme_addto_prim_instance("string->bytes/locale",
			     scheme_make_immed_prim(char_string_to_byte_string_locale,
						    "string->bytes/locale",
						    1, 4),
			     env);
  scheme_addto_prim_instance("string->bytes/latin-1",
			     scheme_make_immed_prim(char_string_to_byte_string_latin1,
						    "string->bytes/latin-1",
						    1, 4),
			     env);

  scheme_addto_prim_instance("string-utf-8-length",
			     scheme_make_immed_prim(char_string_utf8_length,
						    "string-utf-8-length",
						    1, 3),
			     env);


  /* In principle, `version' could be foldable, but it invites
     more problems than it solves... */

  scheme_addto_prim_instance("version",
			     scheme_make_immed_prim(version,
						    "version",
						    0, 0),
			     env);
  scheme_addto_prim_instance("banner",
			     scheme_make_immed_prim(banner,
						    "banner",
						    0, 0),
			     env);

  /* Environment variables */

  scheme_addto_prim_instance("environment-variables?",
			     scheme_make_folding_prim(env_p,
                                                      "environment-variables?",
                                                      1, 1, 1),
			     env);

  scheme_addto_prim_instance("current-environment-variables",
			     scheme_register_parameter(current_environment_variables,
						       "current-environment-variables",
						       MZCONFIG_CURRENT_ENV_VARS),
			     env);  

  scheme_addto_prim_instance("environment-variables-ref",
			     scheme_make_immed_prim(sch_getenv,
						    "environment-variables-ref",
						    2, 2),
			     env);

  scheme_addto_prim_instance("environment-variables-set!",
			     scheme_make_prim_w_arity(sch_putenv,
                                                      "environment-variables-set!",
                                                      3, 4),
			     env);

  scheme_addto_prim_instance("environment-variables-names",
			     scheme_make_immed_prim(sch_getenv_names,
						    "environment-variables-names",
						    1, 1),
			     env);

  scheme_addto_prim_instance("environment-variables-copy",
			     scheme_make_immed_prim(env_copy,
						    "environment-variables-copy",
						    1, 1),
			     env);

  scheme_addto_prim_instance("make-environment-variables",
			     scheme_make_immed_prim(env_make,
						    "make-environment-variables",
						    0, -1),
			     env);

  /* Don't make these folding, since they're platform-specific: */

  REGISTER_SO(scheme_system_type_proc);
  scheme_system_type_proc = scheme_make_immed_prim(system_type,
                                                   "system-type",
                                                   0, 1);
  scheme_addto_prim_instance("system-type", scheme_system_type_proc, env);

  scheme_addto_prim_instance("system-library-subpath",
			     scheme_make_immed_prim(system_library_subpath,
						    "system-library-subpath",
						    0, 1),
			     env);

  scheme_addto_prim_instance("current-command-line-arguments",
			     scheme_register_parameter(cmdline_args,
						       "current-command-line-arguments",
						       MZCONFIG_CMDLINE_ARGS),
			     env);


  scheme_addto_prim_instance("path<?",
			     scheme_make_immed_prim(path_lt,
						    "path<?",
						    1, -1),
			     env);

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

void scheme_init_string_places(void) {
  REGISTER_SO(current_locale_name_ptr);
  current_locale_name_ptr = (void *)xes_char_string;

  REGISTER_SO(fs_change_props);
  {
    int supported, scalable, low_latency, file_level;
    Scheme_Object *s;
    scheme_fs_change_properties(&supported, &scalable, &low_latency, &file_level);
    fs_change_props = scheme_make_vector(4, scheme_false);
    if (supported) {
      s = scheme_intern_symbol("supported");
      SCHEME_VEC_ELS(fs_change_props)[0] = s;
    }
    if (scalable) {
      s = scheme_intern_symbol("scalable");
      SCHEME_VEC_ELS(fs_change_props)[1] = s;
    }
    if (low_latency) {
      s = scheme_intern_symbol("low-latency");
      SCHEME_VEC_ELS(fs_change_props)[2] = s;
    }
    if (file_level) {
      s = scheme_intern_symbol("file-level");
      SCHEME_VEC_ELS(fs_change_props)[3] = s;
    }
    SCHEME_SET_IMMUTABLE(fs_change_props);
  }
}

/**********************************************************************/
/*                     UTF-8 char constructors                        */
/**********************************************************************/

Scheme_Object *scheme_make_sized_offset_utf8_string(char *chars, intptr_t d, intptr_t len)
{
  intptr_t ulen;
  mzchar *us;

  if (len) {
    ulen = scheme_utf8_decode((unsigned char *)chars, d, d + len,
			      NULL, 0, -1,
			      NULL, 0 /* not UTF-16 */, 0xFFFD);
    us = scheme_malloc_atomic(sizeof(mzchar) * (ulen + 1));
    scheme_utf8_decode((unsigned char *)chars, d, d + len,
                       us, 0, -1,
                       NULL, 0 /* not UTF-16 */, 0xFFFD);
    us[ulen] = 0;
  } else {
    us = (mzchar *)empty_char_string;
    ulen = 0;
  }
  return scheme_make_sized_offset_char_string(us, 0, ulen, 0);
}

Scheme_Object *
scheme_make_sized_utf8_string(char *chars, intptr_t len)
{
  return scheme_make_sized_offset_utf8_string(chars, 0, len);
}

Scheme_Object *
scheme_make_immutable_sized_utf8_string(char *chars, intptr_t len)
{
  Scheme_Object *s;

  s = scheme_make_sized_offset_utf8_string(chars, 0, len);
  if (len)
    SCHEME_SET_CHAR_STRING_IMMUTABLE(s);

  return s;
}

Scheme_Object *
scheme_make_utf8_string(const char *chars)
{
  return scheme_make_sized_offset_utf8_string((char *)chars, 0, -1);
}

Scheme_Object *
scheme_make_locale_string(const char *chars)
{
  return scheme_byte_string_to_char_string_locale(scheme_make_byte_string((char *)chars));
}

Scheme_Object *scheme_append_strings(Scheme_Object *s1, Scheme_Object *s2)
{
  Scheme_Object *a[2];
  a[0] = s1;
  a[1] = s2;
  return string_append(2, a);
}

/**********************************************************************/
/*                         index helpers                              */
/**********************************************************************/

intptr_t scheme_extract_index(const char *name, int pos, int argc, Scheme_Object **argv, intptr_t top, int false_ok)
{
  intptr_t i;
  int is_top = 0;

  if (SCHEME_INTP(argv[pos])) {
    i = SCHEME_INT_VAL(argv[pos]);
  } else if (SCHEME_BIGNUMP(argv[pos])) {
    if (SCHEME_BIGPOS(argv[pos])) {
      i = top; /* out-of-bounds */
      is_top = 1;
    } else
      i = -1; /* negative */
  } else
    i = -1;

  if (!is_top && (i < 0))
    scheme_wrong_contract(name,
                          (false_ok ? "(or/c exact-nonnegative-integer? #f)" : "exact-nonnegative-integer?"),
                          pos, argc, argv);

  return i;
}

void scheme_get_substring_indices(const char *name, Scheme_Object *str,
                                  int argc, Scheme_Object **argv,
                                  int spos, int fpos, intptr_t *_start, intptr_t *_finish)
{
  intptr_t len;
  intptr_t start, finish;

  if (SCHEME_CHAPERONE_VECTORP(str))
    len = SCHEME_CHAPERONE_VEC_SIZE(str);
  else if (SCHEME_CHAR_STRINGP(str))
    len = SCHEME_CHAR_STRTAG_VAL(str);
  else
    len = SCHEME_BYTE_STRTAG_VAL(str);

  if (argc > spos)
    start = scheme_extract_index(name, spos, argc, argv, len + 1, 0);
  else
    start = 0;
  if (argc > fpos)
    finish = scheme_extract_index(name, fpos, argc, argv, len + 1, 0);
  else
    finish = len;

  if (!(start <= len)) {
    scheme_out_of_range(name, NULL, (fpos < 100) ? "starting " : "", argv[spos], str, 0, len);
  }
  if (!(finish >= start && finish <= len)) {
    scheme_out_of_range(name, NULL, "ending ", argv[fpos], str, start, len);
  }

  *_start = start;
  *_finish = finish;
}

void scheme_do_get_substring_indices(const char *name, Scheme_Object *str,
                                     int argc, Scheme_Object **argv,
                                     int spos, int fpos, intptr_t *_start, intptr_t *_finish, intptr_t len)
{
  if (argc > spos) {
    if (SCHEME_INTP(argv[spos])) {
      intptr_t start = SCHEME_INT_VAL(argv[spos]);
      if ((start >= 0) && (start < len)) {
        *_start = start;
        if (argc > fpos) {
          intptr_t finish = SCHEME_INT_VAL(argv[fpos]);
          if ((finish >= start) && (finish <= len)) {
            *_finish = finish;
            return;
          }
        } else {
          *_finish = len;
          return;
        }
      }
    }
  } else {
    *_start = 0;
    *_finish = len;
    return;
  }

  scheme_get_substring_indices(name, str, argc, argv, spos, fpos, _start, _finish);
}

/**********************************************************************/
/*                          char strings                              */
/**********************************************************************/

#define SCHEME_X_STR_VAL(x) SCHEME_CHAR_STR_VAL(x)
#define SCHEME_X_STRTAG_VAL(x) SCHEME_CHAR_STRTAG_VAL(x)
#define SCHEME_X_STRINGP(x) SCHEME_CHAR_STRINGP(x)
#define SCHEME_MUTABLE_X_STRINGP(x) SCHEME_MUTABLE_CHAR_STRINGP(x)
#define SCHEME_SET_X_STRING_IMMUTABLE(x) SCHEME_SET_CHAR_STRING_IMMUTABLE(x)
#define scheme_x_string_type scheme_char_string_type
#define X(a, b) a##_char##b
#define X_(a, b) a##_##b
#define X__(a) a
#define EMPTY (mzchar *)"\0\0\0"
#define Xchar mzchar
#define uXchar mzchar
#define XSTR ""
#define IS_STR "string?"
#define XSTRINGSTR "string"
#define SUBXSTR "substring"
#define CHARP(x) SCHEME_CHARP(x)
#define CHAR_VAL(x) SCHEME_CHAR_VAL(x)
#define CHAR_STR "char?"
#define MAKE_CHAR(x) _scheme_make_char(x)
#define xstrlen scheme_char_strlen
#include "strops.inc"

#define GEN_STRING_COMP(name, scheme_name, comp, op, ul, size_shortcut)     \
static Scheme_Object * name (int argc, Scheme_Object *argv[]) \
{  mzchar *s, *prev; int i, sl, pl; int falz = 0;\
   if (!SCHEME_CHAR_STRINGP(argv[0])) \
    scheme_wrong_contract(scheme_name, "string?", 0, argc, argv); \
   prev = SCHEME_CHAR_STR_VAL(argv[0]); pl = SCHEME_CHAR_STRTAG_VAL(argv[0]); \
   for (i = 1; i < argc; i++) { \
     if (!SCHEME_CHAR_STRINGP(argv[i])) \
      scheme_wrong_contract(scheme_name, "string?", i, argc, argv); \
     s = SCHEME_CHAR_STR_VAL(argv[i]); sl = SCHEME_CHAR_STRTAG_VAL(argv[i]); \
     if (!falz) if (!(comp(scheme_name, \
                           prev, pl, \
                           s, sl, ul, size_shortcut) op 0)) falz = 1; \
     prev = s; pl = sl; \
  } \
  return falz ? scheme_false : scheme_true; \
}

GEN_STRING_COMP(string_eq, "string=?", mz_char_strcmp, ==, 0, 1)
GEN_STRING_COMP(string_lt, "string<?", mz_char_strcmp, <, 0, 0)
GEN_STRING_COMP(string_gt, "string>?", mz_char_strcmp, >, 0, 0)
GEN_STRING_COMP(string_lt_eq, "string<=?", mz_char_strcmp, <=, 0, 0)
GEN_STRING_COMP(string_gt_eq, "string>=?", mz_char_strcmp, >=, 0, 0)

GEN_STRING_COMP(string_ci_eq, "string-ci=?", mz_char_strcmp_ci, ==, 0, 0)
GEN_STRING_COMP(string_ci_lt, "string-ci<?", mz_char_strcmp_ci, <, 0, 0)
GEN_STRING_COMP(string_ci_gt, "string-ci>?", mz_char_strcmp_ci, >, 0, 0)
GEN_STRING_COMP(string_ci_lt_eq, "string-ci<=?", mz_char_strcmp_ci, <=, 0, 0)
GEN_STRING_COMP(string_ci_gt_eq, "string-ci>=?", mz_char_strcmp_ci, >=, 0, 0)

GEN_STRING_COMP(string_locale_eq, "string-locale=?", mz_char_strcmp, ==, 1, 0)
GEN_STRING_COMP(string_locale_lt, "string-locale<?", mz_char_strcmp, <, 1, 0)
GEN_STRING_COMP(string_locale_gt, "string-locale>?", mz_char_strcmp, >, 1, 0)
GEN_STRING_COMP(string_locale_ci_eq, "string-locale-ci=?", mz_char_strcmp_ci, ==, 1, 0)
GEN_STRING_COMP(string_locale_ci_lt, "string-locale-ci<?", mz_char_strcmp_ci, <, 1, 0)
GEN_STRING_COMP(string_locale_ci_gt, "string-locale-ci>?", mz_char_strcmp_ci, >, 1, 0)

Scheme_Object *scheme_string_eq_2(Scheme_Object *str1, Scheme_Object *str2)
{
  Scheme_Object *a[2];
  a[0] = str1;
  a[1] = str2;       
  return string_eq(2, a);
}

Scheme_Object *string_append_immutable(int argc, Scheme_Object *argv[])
{
  Scheme_Object *r;

  r = do_string_append("string-append-immutable", argc, argv);

  if (r == zero_length_char_string)
    return zero_length_char_immutable_string;

  SCHEME_SET_CHAR_STRING_IMMUTABLE(r);

  return r;
}

/**********************************************************************/
/*                         byte strings                               */
/**********************************************************************/

#define SCHEME_BYTEP(x) ((SCHEME_INTP(x)) && (SCHEME_INT_VAL(x) >= 0) && (SCHEME_INT_VAL(x) <= 255))

static Scheme_Object *
byte_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_BYTEP(argv[0]) ? scheme_true : scheme_false);
}

#define SCHEME_X_STR_VAL(x) SCHEME_BYTE_STR_VAL(x)
#define SCHEME_X_STRTAG_VAL(x) SCHEME_BYTE_STRTAG_VAL(x)
#define SCHEME_X_STRINGP(x) SCHEME_BYTE_STRINGP(x)
#define SCHEME_MUTABLE_X_STRINGP(x) SCHEME_MUTABLE_BYTE_STRINGP(x)
#define SCHEME_SET_X_STRING_IMMUTABLE(x) SCHEME_SET_BYTE_STRING_IMMUTABLE(x)
#define scheme_x_string_type scheme_byte_string_type
#define X(a, b) a##_byte##b
#define X_(a, b) a##_byte_##b
#define X__(a) byte_##a
#define EMPTY ""
#define Xchar char
#define uXchar unsigned char
#define XSTR "byte "
#define IS_STR "bytes?"
#define XSTRINGSTR "bytes"
#define SUBXSTR "subbytes"
#define CHARP(x) SCHEME_BYTEP(x)
#define CHAR_VAL(x) SCHEME_INT_VAL(x)
#define CHAR_STR "byte?"
#define MAKE_CHAR(x) scheme_make_integer_value(x)
#define xstrlen strlen
#define GENERATING_BYTE
#include "strops.inc"
#undef GENERATING_BYTE

/* comparisons */

#define GEN_BYTE_STRING_PATH_COMP(name, scheme_name, comp, op, PRED, contract)     \
static Scheme_Object * name (int argc, Scheme_Object *argv[]) \
{  char *s, *prev; int i, sl, pl; int falz = 0;\
  if (!PRED(argv[0])) \
    scheme_wrong_contract(scheme_name, contract, 0, argc, argv); \
   prev = SCHEME_BYTE_STR_VAL(argv[0]); pl = SCHEME_BYTE_STRTAG_VAL(argv[0]); \
   for (i = 1; i < argc; i++) { \
     if (!PRED(argv[i])) \
      scheme_wrong_contract(scheme_name, contract, i, argc, argv); \
     s = SCHEME_BYTE_STR_VAL(argv[i]); sl = SCHEME_BYTE_STRTAG_VAL(argv[i]); \
     if (!falz) if (!(comp(scheme_name, \
                           (unsigned char *)prev, pl, \
                           (unsigned char *)s, sl) op 0)) falz = 1; \
     prev = s; pl = sl; \
  } \
  return falz ? scheme_false : scheme_true; \
}

#define GEN_BYTE_STRING_COMP(name, scheme_name, comp, op) \
  GEN_BYTE_STRING_PATH_COMP(name, scheme_name, comp, op, SCHEME_BYTE_STRINGP, "bytes?") \

GEN_BYTE_STRING_COMP(byte_string_eq, "bytes=?", mz_strcmp, ==)
GEN_BYTE_STRING_COMP(byte_string_lt, "bytes<?", mz_strcmp, <)
GEN_BYTE_STRING_COMP(byte_string_gt, "bytes>?", mz_strcmp, >)

GEN_BYTE_STRING_PATH_COMP(path_lt, "path<?", mz_strcmp, <, SCHEME_PATHP, "path?")

Scheme_Object *scheme_byte_string_eq_2(Scheme_Object *str1, Scheme_Object *str2)
{
  Scheme_Object *a[2];
  a[0] = str1;
  a[1] = str2;       
  return byte_string_eq(2, a);
}

/**********************************************************************/
/*                   byte string <-> char string                      */
/**********************************************************************/

/************************* bytes->string *************************/

static Scheme_Object *
do_byte_string_to_char_string(const char *who,
			      Scheme_Object *bstr,
			      intptr_t istart, intptr_t ifinish,
			      int perm, int as_locale)
{
  int i, ulen;
  char *chars;
  unsigned int *v;

  chars = SCHEME_BYTE_STR_VAL(bstr);

  ulen = utf8_decode_x((unsigned char *)chars, istart, ifinish,
		       NULL, 0, -1,
		       NULL, NULL, 0, 0,
		       NULL, 0, 
		       (perm > -1) ? 0xD800 : 0, 0);
  if (ulen < 0) {
    scheme_contract_error(who,
                          "string is not a well-formed UTF-8 encoding",
                          "string", 1, bstr,
                          NULL);
  }

  v = (unsigned int *)scheme_malloc_atomic((ulen + 1) * sizeof(unsigned int));
  utf8_decode_x((unsigned char *)chars, istart, ifinish,
		v, 0, -1,
		NULL, NULL, 0, 0,
		NULL, 0, 
		(perm > -1) ? 0xD800 : 0, 0);
  
  if (perm > -1) {
    for (i = 0; i < ulen; i++) {
      if (v[i] == 0xD800)
	v[i] = perm;
    }
  }
  v[ulen] = 0;

  return scheme_make_sized_char_string(v, ulen, 0);
}

static Scheme_Object *
do_byte_string_to_char_string_locale(const char *who,
				     Scheme_Object *bstr,
				     intptr_t istart, intptr_t ifinish,
				     int perm)
{
  char *us;
  intptr_t olen;

  reset_locale();

  if (mzLOCALE_IS_UTF_8(current_locale_name) || !locale_on
      || !(rktio_convert_properties(scheme_rktio) & RKTIO_CONVERTER_SUPPORTED))
    return do_byte_string_to_char_string(who, bstr, istart, ifinish, perm, 1);

  if (istart < ifinish) {
    int no_cvt;

    us = string_to_from_locale(0, SCHEME_BYTE_STR_VAL(bstr),
			       istart, ifinish - istart,
			       &olen, perm, &no_cvt);

    if (!us) {
      if (no_cvt) {
	return do_byte_string_to_char_string(who, bstr, istart, ifinish, perm, 1);
      } else {
	scheme_contract_error(who,
                              "byte string is not a valid encoding for the current locale",
                              "byte string", 1, bstr,
                              NULL);
      }
    }
    ((mzchar *)us)[olen] = 0;
  } else {
    us = "\0\0\0";
    olen = 0;
  }

  return scheme_make_sized_char_string((mzchar *)us, olen, 0);
}

static Scheme_Object *
do_string_to_vector(const char *who, int mode, int argc, Scheme_Object *argv[])
{
  int permc;
  intptr_t istart, ifinish;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract(who, "bytes?", 0, argc, argv);

  if ((argc < 2) || SCHEME_FALSEP(argv[1]))
    permc = -1;
  else {
    if (!SCHEME_CHARP(argv[1]))
      scheme_wrong_contract(who, "(or/c char? #f)", 1, argc, argv);
    permc = SCHEME_CHAR_VAL(argv[1]);
  }

  scheme_get_substring_indices(who, argv[0], argc, argv,
			       2, 3,
			       &istart, &ifinish);

  if (mode == 0)
    return do_byte_string_to_char_string(who, argv[0], istart, ifinish, permc, 0);
  else if (mode == 1)
    return do_byte_string_to_char_string_locale(who, argv[0], istart, ifinish, permc);
  else {
    /* Latin-1 */
    mzchar *us;
    unsigned char *s;
    intptr_t i, len;
    len = ifinish - istart;
    s = (unsigned char *)SCHEME_BYTE_STR_VAL(argv[0]);
    us = (mzchar *)scheme_malloc_atomic((len + 1) * sizeof(mzchar));
    for (i = istart; i < ifinish; i++) {
      us[i - istart] = s[i];
    }
    us[len] = 0;

    return scheme_make_sized_char_string(us, len, 0);
  }
}


static Scheme_Object *
byte_string_to_char_string (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("bytes->string/utf-8", 0, argc, argv);
}

static Scheme_Object *
byte_string_to_char_string_locale (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("bytes->string/locale", 1, argc, argv);
}

static Scheme_Object *
byte_string_to_char_string_latin1 (int argc, Scheme_Object *argv[])
{
  return do_string_to_vector("bytes->string/latin-1", 2, argc, argv);
}

Scheme_Object *scheme_byte_string_to_char_string(Scheme_Object *o)
{
  return do_byte_string_to_char_string("s->s", o, 0, SCHEME_BYTE_STRLEN_VAL(o), 0xFFFD, 0);
}

Scheme_Object *scheme_byte_string_to_char_string_locale(Scheme_Object *o)
{
  return do_byte_string_to_char_string_locale("s->s", o, 0, SCHEME_BYTE_STRLEN_VAL(o), 0xFFFD);
}

/************************* string->bytes *************************/

static Scheme_Object *do_char_string_to_byte_string(Scheme_Object *s, intptr_t istart, intptr_t ifinish, 
						    int as_locale)
{
  char *bs;
  int slen;

  slen = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(s), istart, ifinish,
			    NULL, 0,
			    0 /* UTF-16 */);
  bs = (char *)scheme_malloc_atomic(slen + 1);
  scheme_utf8_encode(SCHEME_CHAR_STR_VAL(s), istart, ifinish,
		     (unsigned char *)bs, 0,
		     0 /* UTF-16 */);
  bs[slen] = 0;

  return scheme_make_sized_byte_string(bs, slen, 0);
}

static Scheme_Object *
do_char_string_to_byte_string_locale(const char *who,
				     Scheme_Object *cstr,
				     intptr_t istart, intptr_t ifinish,
				     int perm)
{
  char *s;
  intptr_t olen;

  reset_locale();

  if (mzLOCALE_IS_UTF_8(current_locale_name) || !locale_on
      || !(rktio_convert_properties(scheme_rktio) & RKTIO_CONVERTER_SUPPORTED))
    return do_char_string_to_byte_string(cstr, istart, ifinish, 1);

  if (istart < ifinish) {
    int no_cvt;

    s = string_to_from_locale(1, (char *)SCHEME_CHAR_STR_VAL(cstr),
			      istart, ifinish - istart,
			      &olen, perm, &no_cvt);

    if (!s) {
      if (no_cvt) {
	return do_char_string_to_byte_string(cstr, istart, ifinish, 1);
      } else {
	scheme_contract_error(who,
                              "string cannot be encoded for the current locale",
                              "string", 1, cstr, 
                              NULL);
      }
    }
    s[olen] = 0;
  } else {
    s = "";
    olen = 0;
  }

  return scheme_make_sized_byte_string(s, olen, 0);
}


Scheme_Object *scheme_char_string_to_byte_string(Scheme_Object *s)
{
  return do_char_string_to_byte_string(s, 0, SCHEME_CHAR_STRLEN_VAL(s), 0);
}

Scheme_Object *scheme_char_string_to_byte_string_locale(Scheme_Object *s)
{
  return do_char_string_to_byte_string_locale("s->s", s, 0, SCHEME_CHAR_STRLEN_VAL(s), '?');
}

static Scheme_Object *do_chars_to_bytes(const char *who, int mode,
					int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish;
  int permc;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  if ((argc < 2) || SCHEME_FALSEP(argv[1]))
    permc = -1;
  else {
    if (!SCHEME_BYTEP(argv[1]))
      scheme_wrong_contract(who, "(or/c byte? #f)", 1, argc, argv);
    permc = SCHEME_INT_VAL(argv[1]);
  }

  scheme_get_substring_indices(who, argv[0], argc, argv,
			       2, 3, &istart, &ifinish);

  if (mode == 1)
    return do_char_string_to_byte_string_locale(who, argv[0], istart, ifinish, permc);
  else if (mode == 0)
    return do_char_string_to_byte_string(argv[0], istart, ifinish, 0);
  else {
    /* Latin-1 */
    mzchar *us;
    unsigned char *s;
    intptr_t i, len;
    len = ifinish - istart;
    us = SCHEME_CHAR_STR_VAL(argv[0]);
    s = (unsigned char *)scheme_malloc_atomic(len + 1);
    for (i = istart; i < ifinish; i++) {
      if (us[i] < 256)
	s[i - istart] = us[i];
      else if (permc >= 0) {
	s[i - istart] = permc;
      } else {
	scheme_contract_error(who,
                              "string cannot be encoded in Latin-1",
                              "string", 1, argv[0],
                              NULL);
      }
    }
    s[len] = 0;

    return scheme_make_sized_byte_string((char *)s, len, 0);
  }
}

static Scheme_Object *char_string_to_byte_string(int argc, Scheme_Object *argv[])
{
  return do_chars_to_bytes("string->bytes/utf-8", 0, argc, argv);
}

static Scheme_Object *char_string_to_byte_string_locale(int argc, Scheme_Object *argv[])
{
  return do_chars_to_bytes("string->bytes/locale", 1, argc, argv);
}

static Scheme_Object *char_string_to_byte_string_latin1(int argc, Scheme_Object *argv[])
{
  return do_chars_to_bytes("string->bytes/latin-1", 2, argc, argv);
}

/************************* Other *************************/

static Scheme_Object *char_string_utf8_length (int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish, len;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("string-utf-8-length", "string?", 0, argc, argv);

  scheme_get_substring_indices("string-utf-8-length", argv[0], argc, argv,
			       1, 2, &istart, &ifinish);

  len = scheme_utf8_encode(SCHEME_CHAR_STR_VAL(argv[0]), istart, ifinish,
			   NULL, 0, 0);

  return scheme_make_integer(len);
}

static Scheme_Object *
byte_string_utf8_length (int argc, Scheme_Object *argv[])
{
  int len, perm;
  intptr_t istart, ifinish;
  char *chars;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-utf-8-length", "string?", 0, argc, argv);

  chars = SCHEME_BYTE_STR_VAL(argv[0]);

  if ((argc > 1) && !SCHEME_FALSEP(argv[1])) {
    if (!SCHEME_CHARP(argv[1]))
      scheme_wrong_contract("bytes-utf-8-length", "(or/c char? #f)", 1, argc, argv);
    perm = 1;
  } else
    perm = 0;

  scheme_get_substring_indices("bytes-utf-8-length", argv[0], argc, argv,
			       2, 3,
			       &istart, &ifinish);

  len = scheme_utf8_decode((unsigned char *)chars, istart, ifinish,
			   NULL, 0, -1,
			   NULL, 0, perm);

  if (len < 0)
    return scheme_false;
  else
    return scheme_make_integer(len);
}

static Scheme_Object *
byte_string_utf8_index(int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish, pos = -1, opos, ipos;
  int result, perm;
  char *chars;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-utf-8-index", "bytes?", 0, argc, argv);

  chars = SCHEME_BYTE_STR_VAL(argv[0]);

  if (SCHEME_INTP(argv[1])) {
    pos = SCHEME_INT_VAL(argv[1]);
  } else if (SCHEME_BIGNUMP(argv[1])) {
    if (SCHEME_BIGPOS(argv[1]))
      pos = 0x7FFFFFFF;
  }

  if (pos < 0) {
    scheme_wrong_contract("bytes-utf-8-index", "exact-nonnegative-integer?", 1, argc, argv);
  }

  if ((argc > 2) && !SCHEME_FALSEP(argv[2])) {
    if (!SCHEME_CHARP(argv[2]))
      scheme_wrong_contract("bytes-utf-8-index", "(or/c char? #f)", 1, argc, argv);
    perm = 1;
  } else
    perm = 0;

  scheme_get_substring_indices("bytes-utf-8-index", argv[0], argc, argv,
			       3, 4,
			       &istart, &ifinish);

  result = utf8_decode_x((unsigned char *)chars, istart, ifinish,
			 NULL, 0, pos,
			 &ipos, &opos,
			 0, 0, NULL, 0, perm ? 1 : 0, 0);

  if (((result < 0) && (result != -3))
      || ((ipos == ifinish) && (opos <= pos)))
    return scheme_false;
  else
    return scheme_make_integer(ipos);
}

static Scheme_Object *
byte_string_utf8_ref(int argc, Scheme_Object *argv[])
{
  intptr_t istart, ifinish, pos = -1, opos, ipos;
  char *chars;
  unsigned int us[1];
  Scheme_Object *perm;

  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-utf-8-ref", "bytes?", 0, argc, argv);

  chars = SCHEME_BYTE_STR_VAL(argv[0]);

  if (SCHEME_INTP(argv[1])) {
    pos = SCHEME_INT_VAL(argv[1]);
  } else if (SCHEME_BIGNUMP(argv[1])) {
    if (SCHEME_BIGPOS(argv[1]))
      pos = 0x7FFFFFFF;
  }

  if (pos < 0) {
    scheme_wrong_contract("bytes-utf-8-ref", "exact-nonnegative-integer?", 1, argc, argv);
  }

  if ((argc > 2) && !SCHEME_FALSEP(argv[2])) {
    if (!SCHEME_CHARP(argv[2]))
      scheme_wrong_contract("bytes-utf-8-ref", "(or/c char? #f)", 1, argc, argv);
    perm = argv[2];
  } else
    perm = 0;

  scheme_get_substring_indices("bytes-utf-8-ref", argv[0], argc, argv,
			       3, 4,
			       &istart, &ifinish);

  if (pos > 0) {
    utf8_decode_x((unsigned char *)chars, istart, ifinish,
		  NULL, 0, pos,
		  &ipos, &opos,
		  0, 0, NULL, 0, perm ? 1 : 0, 0);
    if (opos < pos)
      return scheme_false;
    istart = ipos;
  }

  utf8_decode_x((unsigned char *)chars, istart, ifinish,
		us, 0, 1,
		&ipos, &opos,
		0, 0, NULL, 0, perm ? 0xFFFFFF : 0, 0);

  if (opos < 1)
    return scheme_false;
  else if (us[0] == 0xFFFFFF)
    return perm;
  else
    return scheme_make_character(us[0]);
}

/********************************************************************/
/*                            format                                */
/********************************************************************/

void scheme_do_format(const char *procname, Scheme_Object *port,
		      const mzchar *format, int flen,
		      int fpos, int offset, int argc, Scheme_Object **argv)
{
  int i, start, end;
  int used = offset;
  int num_err = 0, char_err = 0, end_ok = 0;
  Scheme_Object *a[2];

  if (!format) {
    if (!SCHEME_CHAR_STRINGP(argv[fpos])) {
      scheme_wrong_contract(procname, "string?", fpos, argc, argv);
      return;
    }
    format = SCHEME_CHAR_STR_VAL(argv[fpos]);
    flen = SCHEME_CHAR_STRTAG_VAL(argv[fpos]);
  } else if (flen == -1)
    flen = strlen((char *)format);

  /* Check string first: */
  end = flen - 1;
  for (i = 0; i < end; i++) {
    if (format[i] == '~') {
      i++;
      if (scheme_isspace(format[i])) {
	/* skip spaces... */
      } else switch (format[i]) {
      case '~':
	if (i == end)
	  end_ok = 1;
	break;
      case '%':
      case 'n':
      case 'N':
	break;
      case 'a':
      case 'A':
      case 's':
      case 'S':
      case 'v':
      case 'V':
      case 'e':
      case 'E':
	used++;
	break;
      case '.':
        switch (format[i+1]) {
        case 'a':
        case 'A':
        case 's':
        case 'S':
        case 'v':
        case 'V':
          break;
        default:
	  scheme_contract_error(procname, 
                                "ill-formed pattern string",
                                "explanation", 0, "tag `~.' not followed by `a', `s', or `v'",
                                "pattern string", 1, argv[fpos],
                                NULL);
          break;
        }
        used++;
        break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	if (!num_err && !char_err && (used < argc)) {
	  Scheme_Object *o = argv[used];
	  if (!SCHEME_EXACT_REALP(o)
	      && (!SCHEME_COMPLEXP(o)
		  || !SCHEME_EXACT_REALP(scheme_complex_real_part(o))))
	    num_err = used + 1;
	}
	used++;
	break;
      case 'c':
      case 'C':
	if (!num_err && !char_err && (used < argc)) {
	  if (!SCHEME_CHARP(argv[used]))
	    char_err = used + 1;
	}
	used++;
	break;
      default:
	{
	  char buffer[64];
	  sprintf(buffer, "tag `~%c' not allowed", format[i]);          
	  scheme_contract_error(procname, 
                                "ill-formed pattern string",
                                "explanation", 0, buffer,
                                "pattern string", 1, argv[fpos],
                                NULL);
	  return;
	}
      }
    }
  }
  if ((format[end] == '~') && !end_ok) {
    scheme_contract_error(procname, 
                          "ill-formed pattern string",
                          "explanation", 0, "cannot end in `~'",
                          "pattern string", 1, argv[fpos],
                          NULL);
    return;
  }
  if (used != argc) {
    char *args;
    intptr_t alen;

    args = scheme_make_args_string("", -1, argc, argv, &alen);

    if (used > argc) {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: format string requires %d arguments, given %d%t",
		       procname, used - offset, argc - offset, args, alen);
    } else {
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: format string requires %d arguments, given %d%t",
		       procname, used - offset, argc - offset, args, alen);
    }
    return;
  }
  if (num_err || char_err) {
    int pos = (num_err ? num_err : char_err) - 1;
    char *args, *bstr;
    intptr_t alen;
    intptr_t blen;
    char *type = (num_err ? "exact-number" : "character");
    Scheme_Object *bad = argv[pos];

    args = scheme_make_args_string("other ", pos, argc, argv, &alen);
    bstr = scheme_make_provided_string(bad, 1, &blen);
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "%s: format string requires argument of type <%s>, given %t%t",
		     procname, type,
		     bstr, blen,
		     args, alen);
    return;
  }

  for (used = offset, i = start = 0; i < flen; i++) {
    if (format[i] == '~') {
      if (start < i) {
	(void)scheme_put_char_string(procname, port, format, start, i - start);
      }
      i++;
      if (scheme_isspace(format[i])) {
	/* skip spaces (at most one newline) */
	do {
	  if ((format[i] == '\n') || (format[i] == '\r')) {
	    /* got one */
	    if ((format[i] == '\r') && (format[i + 1] == '\n'))
	      i++; /* Windows-style CR-NL */
	    i++;
	    while (portable_isspace(format[i])
		   && !((format[i] == '\n') || (format[i] == '\r'))) {
	      i++;
	    }
	    break;
	  } else
	    i++;
	} while (scheme_isspace(format[i]));
	--i; /* back up over something */
      } else switch (format[i]) {
      case '~':
	scheme_write_byte_string("~", 1, port);
	break;
      case '%':
      case 'n':
      case 'N':
	scheme_write_byte_string("\n", 1, port);
	break;
      case 'c':
      case 'C':
      case 'a':
      case 'A':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_display_proc, 2, a);
	break;
      case 's':
      case 'S':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_write_proc, 2, a);
	break;
      case 'v':
      case 'V':
	a[0] = argv[used++];
	a[1] = port;
	_scheme_apply(scheme_print_proc, 2, a);
	break;
      case 'e':
      case 'E':
	{
	  intptr_t len;
	  char *s;
	  s = scheme_make_provided_string(argv[used++], 0, &len);
	  scheme_write_byte_string(s, len, port);
	}
	break;
      case '.':
	{
	  intptr_t len;
	  char *s;
          len = scheme_get_print_width();
          i++;
          switch (format[i]) {
          case 'a':
          case 'A':
            s = scheme_display_to_string_w_max(argv[used++], &len, len);
            break;
          case 's':
          case 'S':
            s = scheme_write_to_string_w_max(argv[used++], &len, len);
            break;
          case 'v':
          case 'V':
            s = scheme_print_to_string_w_max(argv[used++], &len, len);
            break;
          default:
            s = "???";
            len = 3;
          }
	  scheme_write_byte_string(s, len, port);
	}
	break;
      case 'x':
      case 'X':
      case 'o':
      case 'O':
      case 'b':
      case 'B':
	{
	  char *s;
	  int radix;

	  switch(format[i]) {
	  case 'x':
	  case 'X':
	    radix = 16;
	    break;
	  case 'o':
	  case 'O':
	    radix = 8;
	    break;
	  default:
	  case 'b':
	  case 'B':
	    radix = 2;
	    break;
	  }
	  s = scheme_number_to_string(radix, argv[used++]);

	  scheme_write_byte_string(s, strlen(s), port);
	}
	break;
      }
      SCHEME_USE_FUEL(1);
      start = i + 1;
    }
  }

  SCHEME_USE_FUEL(flen);

  if (start < i) {
    (void)scheme_put_char_string(procname, port, format, start, i - start);
  }
}

char *scheme_format(mzchar *format, int flen, int argc, Scheme_Object **argv, intptr_t *rlen)
{
  Scheme_Object *port;
  port = scheme_make_byte_string_output_port();
  scheme_do_format("format", port, format, flen, 0, 0, argc, argv);
  return scheme_get_sized_byte_string_output(port, rlen);
}

void scheme_printf(mzchar *format, int flen, int argc, Scheme_Object **argv)
{
  scheme_do_format("printf", scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT),
		   format, flen, 0, 0, argc, argv);
}

char *scheme_format_utf8(char *format, int flen, int argc, Scheme_Object **argv, intptr_t *rlen)
{
  mzchar *s;
  intptr_t srlen;
  if (flen == -1)
    flen = strlen(format);
  s = scheme_utf8_decode_to_buffer_len((unsigned char *)format, flen, NULL, 0, &srlen);
  if (s)
    return scheme_format(s, srlen, argc, argv, rlen);
  else
    return "";
}

void scheme_printf_utf8(char *format, int flen, int argc, Scheme_Object **argv)
{
  mzchar *s;
  intptr_t srlen;
  if (flen == -1)
    flen = strlen(format);
  s = scheme_utf8_decode_to_buffer_len((unsigned char *)format, flen, NULL, 0, &srlen);
  if (s)
    scheme_printf(s, srlen, argc, argv);
}


static Scheme_Object *
format(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  char *s;
  intptr_t len;

  port = scheme_make_byte_string_output_port();

  scheme_do_format("format", port, NULL, 0, 0, 1, argc, argv);

  s = scheme_get_sized_byte_string_output(port, &len);
  return scheme_make_sized_utf8_string(s, len);
}

#ifdef INSTRUMENT_PRIMITIVES
extern int g_print_prims;
#endif

static Scheme_Object *
sch_printf(int argc, Scheme_Object *argv[])
{
  scheme_do_format("printf", scheme_get_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT),
		   NULL, 0, 0, 1, argc, argv);
  return scheme_void;
}

static Scheme_Object *
sch_eprintf(int argc, Scheme_Object *argv[])
{
  scheme_do_format("eprintf", scheme_get_param(scheme_current_config(), MZCONFIG_ERROR_PORT),
		   NULL, 0, 0, 1, argc, argv);
  return scheme_void;
}

static Scheme_Object *
sch_fprintf(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPUT_PORTP(argv[0]))
    scheme_wrong_contract("fprintf", "output-port?", 0, argc, argv);

  scheme_do_format("fprintf", argv[0], NULL, 0, 1, 2, argc, argv);
  return scheme_void;
}

/********************************************************************/
/*                              misc                                */
/********************************************************************/

static Scheme_Object *
version(int argc, Scheme_Object *argv[])
{
  return vers_str;
}

static Scheme_Object *
banner(int argc, Scheme_Object *argv[])
{
  return banner_str;
}

char *scheme_version(void)
{
  return MZSCHEME_VERSION;
}

#ifdef MZ_PRECISE_GC
# define VERSION_SUFFIX " [bc]"
#else
# ifdef USE_SENORA_GC
#  define VERSION_SUFFIX " [cgc]"
# else
#  define VERSION_SUFFIX " [cgc/b]"
# endif
#endif

char *scheme_banner(void)
{
  if (embedding_banner)
    return embedding_banner;
  else
    return ("Welcome to Racket"
            " v" MZSCHEME_VERSION VERSION_SUFFIX
            ".\n");
}

void scheme_set_banner(char *s)
{
  embedding_banner = s;
}

int scheme_byte_string_has_null(Scheme_Object *o)
{
  const char *s = SCHEME_BYTE_STR_VAL(o);
  int i = SCHEME_BYTE_STRTAG_VAL(o);
  while (i--) {
    if (!s[i])
      return 1;
  }
  return 0;
}

int scheme_any_string_has_null(Scheme_Object *o)
{
  if (SCHEME_BYTE_STRINGP(o))
    return scheme_byte_string_has_null(o);
  else {
    const mzchar *s = SCHEME_CHAR_STR_VAL(o);
    int i = SCHEME_CHAR_STRTAG_VAL(o);
    while (i--) {
      if (!s[i])
	return 1;
    }
    return 0;
  }
}

/***********************************************************************/
/* Environment Variables                                               */
/***********************************************************************/

/* A `scheme_environment_variables_type` record wraps a hash table
   that maps normalized keys to (cons <key> <val>), where the key
   in the pair preserves its original case. */

#define SCHEME_ENVVARS_TABLE(ev) ((Scheme_Hash_Tree *)SCHEME_PTR_VAL(ev))

Scheme_Object *scheme_make_environment_variables(Scheme_Hash_Tree *ht)
{
  Scheme_Object *ev;

  ev = scheme_alloc_small_object();
  ev->type = scheme_environment_variables_type;
  SCHEME_PTR_VAL(ev) = (Scheme_Object *)ht;

  return ev;
}

static Scheme_Object *env_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_environment_variables_type)
          ? scheme_true
          : scheme_false);
}

static Scheme_Object *current_environment_variables(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = scheme_param_config2("current-environment-variables",
                           scheme_make_integer(MZCONFIG_CURRENT_ENV_VARS),
                           argc, argv,
                           -1, env_p, "environment-variables?", 0);

  return v;
}

static int sch_bool_getenv(const char* name);

void scheme_init_getenv(void)
{
  if (sch_bool_getenv("PLTNOMZJIT")) {
    scheme_set_startup_use_jit(0);
  }
  if (sch_bool_getenv("PLT_SHOW_BUILTIN_CONTEXT")) {
    scheme_keep_builtin_context = 1;
  }
}

static int sch_bool_getenv(const char* name)
{
  if (rktio_getenv(scheme_rktio, name))
    return 1;
  else
    return 0;
}

int byte_string_ok_name(Scheme_Object *o)
{
  const char *s = SCHEME_BYTE_STR_VAL(o);
  int i = SCHEME_BYTE_STRTAG_VAL(o);

  while (i--) {
    if (!s[i])
      return 0;
  }

  return rktio_is_ok_envvar_name(scheme_rktio, s);
    
  return 1;
}

static Scheme_Object *normalize_env_case(Scheme_Object *bs)
{
  if (rktio_are_envvar_names_case_insensitive(scheme_rktio)) {
    bs = scheme_byte_string_to_char_string(bs);
    bs = string_locale_downcase(1, &bs);
    bs = scheme_char_string_to_byte_string(bs);
  }
  return bs;
}

char *scheme_getenv(char *name)
{
  char *s;
  s = rktio_getenv(scheme_rktio, name);
  if (s)
    return scheme_strdup_and_free(s);
  else
    return NULL;
}

static Scheme_Object *sch_getenv(int argc, Scheme_Object *argv[])
{
  char *name;
  char *value;
  Scheme_Object *bs, *ev, *val;
  Scheme_Hash_Tree *ht;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_environment_variables_type))
    scheme_wrong_contract("environment-variables-ref", "environment-variables?", 0, argc, argv);

  bs = argv[1];
  if (!SCHEME_BYTE_STRINGP(bs)
      || !byte_string_ok_name(bs))
    scheme_wrong_contract("environment-variables-ref", "bytes-environment-variable-name?", 1, argc, argv);

  ev = argv[0];
  ht = SCHEME_ENVVARS_TABLE(ev);

  if (!ht) {
    name = SCHEME_BYTE_STR_VAL(bs);

    value = rktio_getenv(scheme_rktio, name);
    if (value) {
      val = scheme_make_byte_string(value);
      free(value);
    } else
    val = scheme_false;

    return val;
  } else {
    bs = normalize_env_case(bs);
    val = scheme_hash_tree_get(ht, bs);
    return val ? SCHEME_CDR(val) : scheme_false;
  }
}

static Scheme_Object *sch_putenv(int argc, Scheme_Object *argv[])
{
  Scheme_Object *varbs, *valbs, *norm_varbs, *ev;
  Scheme_Hash_Tree *ht;
  char *var;
  char *val;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_environment_variables_type))
    scheme_wrong_contract("environment-variables-set!", "environment-variables?", 0, argc, argv);
  
  varbs = argv[1];
  if (!SCHEME_BYTE_STRINGP(varbs)
      || !byte_string_ok_name(varbs))
    scheme_wrong_contract("environment-variables-set!", "bytes-environment-variable-name?", 1, argc, argv);

  valbs = argv[2];
  if (!SCHEME_FALSEP(valbs)
      && (!SCHEME_BYTE_STRINGP(valbs)
          || scheme_byte_string_has_null(valbs)))
    scheme_wrong_contract("environment-variables-set!", "(or/c bytes-no-nuls? #f)", 2, argc, argv);
  if (argc > 3)
    scheme_check_proc_arity("environment-variables-set!", 0, 3, argc, argv);

  ev = argv[0];
  ht = SCHEME_ENVVARS_TABLE(ev);

  if (ht) {
    norm_varbs = normalize_env_case(varbs);

    if (SCHEME_FALSEP(valbs)) {
      ht = scheme_hash_tree_set(ht, norm_varbs, NULL);
    } else {
      if (SAME_OBJ(varbs, norm_varbs)) {
        varbs = byte_string_to_immutable(1, &varbs);
        norm_varbs = varbs;
      } else {
        varbs = byte_string_to_immutable(1, &varbs);
        norm_varbs = byte_string_to_immutable(1, &norm_varbs);
      }
      valbs = byte_string_to_immutable(1, &valbs);
      ht = scheme_hash_tree_set(ht, norm_varbs, scheme_make_pair(varbs, valbs));
    }

    SCHEME_PTR_VAL(ev) = (Scheme_Object *)ht;

    return scheme_void;
  } else {
    var = SCHEME_BYTE_STR_VAL(varbs);

    if (SCHEME_FALSEP(valbs)) {
      val = NULL;
    } else {
      val = SCHEME_BYTE_STR_VAL(valbs);
    }

    if (!rktio_setenv(scheme_rktio, var, val)) {
      if (argc > 3)
        return _scheme_tail_apply(argv[3], 0, NULL);
      else {
        scheme_raise_exn(MZEXN_FAIL,
                         "environment-variables-set!: change failed\n"
                         "  system error: %R");
      }
    }

    return scheme_void;
  }
}

static Scheme_Object *env_copy(int argc, Scheme_Object *argv[])
{
  Scheme_Hash_Tree *ht;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_environment_variables_type))
    scheme_wrong_contract("environment-variables-copy", "environment-variables?", 0, argc, argv);

  ht = SCHEME_ENVVARS_TABLE(argv[0]);
  if (ht)
    return scheme_make_environment_variables(ht);
  
  /* copy system environment variables into a hash table: */
  ht = scheme_make_hash_tree(SCHEME_hashtr_equal);

  {
    intptr_t i;
    rktio_envvars_t *envvars;
    Scheme_Object *var, *val, *norm_var;

    envvars = rktio_envvars(scheme_rktio);
    for (i = rktio_envvars_count(scheme_rktio, envvars); i--; ) {
      var = scheme_make_immutable_sized_byte_string(rktio_envvars_name_ref(scheme_rktio, envvars, i), -1, 1);
      val = scheme_make_immutable_sized_byte_string(rktio_envvars_value_ref(scheme_rktio, envvars, i), -1, 1);
      norm_var = normalize_env_case(var);
      if (!SAME_OBJ(var, norm_var))
        norm_var = byte_string_to_immutable(1, &norm_var);
      ht = scheme_hash_tree_set(ht, norm_var, scheme_make_pair(var, val));
    }

    rktio_envvars_free(scheme_rktio, envvars);
  }

  return scheme_make_environment_variables(ht);
}

static Scheme_Object *env_make(int argc, Scheme_Object *argv[])
{
  Scheme_Hash_Tree *ht;
  Scheme_Object *varbs, *valbs, *norm_varbs;
  int i;

  ht = scheme_make_hash_tree(SCHEME_hashtr_equal);

  for (i = 0; i < argc; i += 2) {
    varbs = argv[i];
    if (!SCHEME_BYTE_STRINGP(varbs)
        || !byte_string_ok_name(varbs))
      scheme_wrong_contract("make-environment-variables", "bytes-environment-variable-name?", i, argc, argv);

    if (i+1 >= argc) {
      scheme_contract_error("make-environment-variables",
                            "key does not have a value (i.e., an odd number of arguments were provided)",
                            "key", 1, argv[i],
                            NULL);
      return NULL;
    }

    valbs = argv[i+1];
    if (!SCHEME_FALSEP(valbs)
        && (!SCHEME_BYTE_STRINGP(valbs)
            || scheme_byte_string_has_null(valbs)))
      scheme_wrong_contract("make-environment-variables", "(or/c bytes-no-nuls? #f)", i+1, argc, argv);

    varbs = byte_string_to_immutable(1, &varbs);
    valbs = byte_string_to_immutable(1, &valbs);
    norm_varbs = normalize_env_case(varbs);
    if (!SAME_OBJ(varbs, norm_varbs))
      norm_varbs = byte_string_to_immutable(1, &norm_varbs);
    ht = scheme_hash_tree_set(ht, norm_varbs, scheme_make_pair(varbs, valbs));
  }

  return scheme_make_environment_variables(ht);
}

static Scheme_Object *sch_getenv_names(int argc, Scheme_Object *argv[])
{
  Scheme_Object *ev, *r = scheme_null, *key, *val;
  Scheme_Hash_Tree *ht;
  mzlonglong i;

  ev = argv[0];
  if (!SAME_TYPE(SCHEME_TYPE(ev), scheme_environment_variables_type))
    scheme_wrong_contract("environment-variables-names", "environment-variables?", 0, argc, argv);

  ht = SCHEME_ENVVARS_TABLE(ev);
  if (!ht) {
    ev = env_copy(1, argv);
    ht = SCHEME_ENVVARS_TABLE(ev);
  }

  for (i = scheme_hash_tree_next(ht, -1); i != -1; i = scheme_hash_tree_next(ht, i)) {
    scheme_hash_tree_index(ht, i, &key, &val);
    r = scheme_make_pair(SCHEME_CAR(val), r);
  }

  return r;
}

rktio_envvars_t *scheme_environment_variables_to_envvars(Scheme_Object *ev)
{
  Scheme_Hash_Tree *ht = SCHEME_ENVVARS_TABLE(ev);
  rktio_envvars_t *envvars;
  mzlonglong i;
  Scheme_Object *key, *val;

  if (!ht)
    return NULL;

  envvars = rktio_empty_envvars(scheme_rktio);
  
  for (i = scheme_hash_tree_next(ht, -1); i != -1; i = scheme_hash_tree_next(ht, i)) {
    scheme_hash_tree_index(ht, i, &key, &val);
    
    rktio_envvars_set(scheme_rktio,
                      envvars,
                      SCHEME_BYTE_STR_VAL(SCHEME_CAR(val)),
                      SCHEME_BYTE_STR_VAL(SCHEME_CDR(val)));
  }
  
  return envvars;
}

/***********************************************************************/
/* End Environment Variables                                           */
/***********************************************************************/

void scheme_set_cross_compile_mode(int v)
{
  cross_compile_mode = v;
}

static void machine_details(char *s);

#include "systype.inc"

static Scheme_Object *system_type(int argc, Scheme_Object *argv[])
{
  if (argc) {
    if (SAME_OBJ(argv[0], link_symbol)) {
      return scheme_intern_symbol(MZ_SYSTEM_TYPE_LINK);
    }

    if (SAME_OBJ(argv[0], machine_symbol)) {
      char buff[1024];
      
      machine_details(buff);
    
      return scheme_make_utf8_string(buff);
    }

    if (SAME_OBJ(argv[0], gc_symbol)) {
#ifdef MZ_PRECISE_GC
      return _3m_symbol;
#else
      return cgc_symbol;
#endif
    }

    if (SAME_OBJ(argv[0], vm_symbol)) {
      return racket_symbol;
    }

    if (SAME_OBJ(argv[0], so_suffix_symbol)) {
      return scheme_make_byte_string(MZ_SYSTEM_TYPE_SO_SUFFIX);
    }

    if (SAME_OBJ(argv[0], so_mode_symbol)) {
      return scheme_intern_symbol(MZ_SYSTEM_TYPE_SO_MODE);
    }


    if (SAME_OBJ(argv[0], word_symbol)) {
      return scheme_make_integer(sizeof(void*)*8);
    }

    if (SAME_OBJ(argv[0], fs_change_symbol)) {
      return fs_change_props;
    }

    if (SAME_OBJ(argv[0], target_machine_symbol)) {
      return racket_symbol;
    }

    if (SAME_OBJ(argv[0], cross_symbol)) {
      return (cross_compile_mode ? force_symbol : infer_symbol);
    }

    if (SAME_OBJ(argv[0], os_star_symbol)) {
      return sys_os_symbol;
    }

    if (SAME_OBJ(argv[0], arch_symbol)) {
      return sys_arch_symbol;
    }

    if (!SAME_OBJ(argv[0], os_symbol)) {
      scheme_wrong_contract("system-type",
                            ("(or/c 'os 'os* 'arch 'word 'link 'machine 'target-machine\n"
                             "      'vm 'gc 'so-suffix 'so-mode 'word 'fs-change 'cross)"),
                            0, argc, argv);
      return NULL;
    }
  }

  return sys_symbol;
}

static Scheme_Object *system_library_subpath(int argc, Scheme_Object *argv[])
{
  if (argc > 0) {
    if (SCHEME_FALSEP(argv[0]))
      return platform_cgc_path;
    
    if (SAME_OBJ(cgc_symbol, argv[0]))
      return platform_cgc_path;

    if (SAME_OBJ(_3m_symbol, argv[0]))
      return platform_3m_path;

    if (SAME_OBJ(cs_symbol, argv[0]))
      return platform_cs_path;

    scheme_wrong_contract("system-library-subpath", "(or/c 'cgc '3m 'cs #f)", 0, argc, argv);
    return NULL;
  } else {
#ifdef MZ_PRECISE_GC
    return platform_3m_path;
#else
    return platform_cgc_path;
#endif
  }
}

const char *scheme_system_library_subpath()
{
  return SCHEME_PLATFORM_LIBRARY_SUBPATH SPLS_SUFFIX;
}

/* Our own strncpy - which would be really stupid, except the one for
   the implementation in Solaris 2.6 is broken (it doesn't always stop
   at the null terminator). */
int scheme_strncmp(const char *a, const char *b, int len)
{
  while (len-- && (*a == *b) && *a) {
    a++;
    b++;
  }

  if (len < 0)
    return 0;
  else
    return *a - *b;
}

static Scheme_Object *ok_cmdline(int argc, Scheme_Object **argv)
{
  if (SCHEME_CHAPERONE_VECTORP(argv[0])) {
    Scheme_Object *vec = argv[0], *vec2, *str;
    int i, size = SCHEME_CHAPERONE_VEC_SIZE(vec);

    if (!size)
      return vec;

    /* Make sure vector and strings are immutable: */
    vec2 = scheme_make_vector(size, NULL);
    if (size)
      SCHEME_SET_VECTOR_IMMUTABLE(vec2);
    for (i = 0; i < size; i++) {
      if (SCHEME_VECTORP(vec))
        str = SCHEME_VEC_ELS(vec)[i];
      else
        str = scheme_chaperone_vector_ref(vec, i);
      if (!SCHEME_CHAR_STRINGP(str))
        return NULL;
      if (!SCHEME_IMMUTABLE_CHAR_STRINGP(str)) {
	str = scheme_make_sized_char_string(SCHEME_CHAR_STR_VAL(str), SCHEME_CHAR_STRLEN_VAL(str), 0);
	SCHEME_SET_CHAR_STRING_IMMUTABLE(str);
      }
      SCHEME_VEC_ELS(vec2)[i] = str;
    }

    return vec2;
  }

  return NULL;
}

static Scheme_Object *cmdline_args(int argc, Scheme_Object *argv[])
{
  return scheme_param_config2("current-command-line-arguments",
                              scheme_make_integer(MZCONFIG_CMDLINE_ARGS),
                              argc, argv,
                              -1, ok_cmdline, "(vectorof string?)", 1);
}

/**********************************************************************/
/*                           locale ops                               */
/**********************************************************************/

static Scheme_Object *ok_locale(int argc, Scheme_Object **argv)
{
  if (SCHEME_FALSEP(argv[0]))
    return argv[0];
  else if (SCHEME_CHAR_STRINGP(argv[0])) {
    if (SCHEME_IMMUTABLEP(argv[0]))
      return argv[0];
    else {
      Scheme_Object *str = argv[0];
      str = scheme_make_immutable_sized_char_string(SCHEME_CHAR_STR_VAL(str), SCHEME_CHAR_STRLEN_VAL(str), 1);
      return str;
    }
  }

  return NULL;
}

static Scheme_Object *current_locale(int argc, Scheme_Object *argv[])
{
  Scheme_Object *v;

  v = scheme_param_config2("current-locale",
                           scheme_make_integer(MZCONFIG_LOCALE),
                           argc, argv,
                           -1, ok_locale, "(or/c #f string?)", 1);

  return v;
}

static Scheme_Object *locale_string_encoding(int argc, Scheme_Object *argv[])
{
  char *enc;
  Scheme_Object *s;
  
  reset_locale();
  if (mzLOCALE_IS_UTF_8(current_locale_name) || !locale_on)
    return scheme_make_utf8_string("UTF-8");

  enc = rktio_locale_encoding(scheme_rktio);
  s = scheme_make_utf8_string(enc);
  free(enc);

  return s;
}

static Scheme_Object *system_language_country(int argc, Scheme_Object *argv[])
{
  char *lc;
  Scheme_Object *s;

  lc = rktio_system_language_country(scheme_rktio);
  s = scheme_make_utf8_string(lc);
  free(lc);

  return s;
}

static void do_convert_close(rktio_converter_t *cd, int cache_mode, const char *to_e, const char *from_e)
/* If `cache_mode` is -1, then `to_e` needs to be freed (or cached).
   If `cache_mode` is 1, then `from_e` needs to be freed (or cached). */
{
  if (cache_mode == -1)
    cache_locale_or_close(1, cd, (char *)to_e);
  else if (cache_mode == 1)
    cache_locale_or_close(0, cd, (char *)from_e);
  else if (!cache_mode)
    rktio_converter_close(scheme_rktio, cd);
}

static char *do_convert(rktio_converter_t *cd,
			/* if !cd and either from_e or to_e can be NULL, then
			   reset_locale() must have been called */
			const char *from_e, const char *to_e,
			/* 1 => UCS-4 -> UTF-8; 2 => UTF-8 -> UCS-4; 0 => other */
			int to_from_utf8,
			/* in can be NULL to output just a shift; in that case,
			   id should be 0, too */
			char *in, int id, int iilen,
			char *out, int od, int iolen,
			/* if grow, then reallocate when out isn't big enough */
			int grow,
			/* if add_end_shift, add a shift sequence to the end;
			   not useful if in is already NULL to indicate a shift */
			int add_end_shift,
			/* extra specifies the length of a terminator,
			   not included in iolen or *oolen */
			int extra,
			/* these two report actual read/wrote sizes: */
			intptr_t *oilen, intptr_t *oolen,
			/* status is set to
			   0 for complete,
			   -1 for partial input,
			   -2 for error,
			   1 for more avail */
			int *status)
{
  int dip, dop, close_it = 0, cache_mode = 0, mz_utf8 = 0;
  intptr_t il, ol, r;
  GC_CAN_IGNORE char *ip, *op;

  /* Defaults: */
  *status = -1;
  if (oilen)
    *oilen = 0;
  *oolen = 0;

  if (!cd) {
    if (rktio_convert_properties(scheme_rktio) & RKTIO_CONVERTER_SUPPORTED) {
      char *tmp_from_e = NULL, *tmp_to_e = NULL;

      if (!to_e && !strcmp(from_e, MZ_UCS4_NAME))
        cache_mode = -1;
      else if (!from_e && !strcmp(to_e, MZ_UCS4_NAME))
        cache_mode = 1;

      if (!from_e) {
	tmp_from_e = rktio_locale_encoding(scheme_rktio);
        from_e = tmp_from_e;
      }
      if (!to_e) {
	tmp_to_e = rktio_locale_encoding(scheme_rktio);
        to_e = tmp_to_e;
      }

      if ((cache_mode == -1)
          && cached_locale_to_converter
          && !strcmp(to_e, cached_locale_encoding_name)) {
        cd = cached_locale_to_converter;
        cached_locale_to_converter = NULL;
      } else if ((cache_mode == 1)
                 && cached_locale_from_converter
                 && !strcmp(from_e, cached_locale_encoding_name)) {
        cd = cached_locale_from_converter;
        cached_locale_from_converter = NULL;
      } else {
        cd = rktio_converter_open(scheme_rktio, to_e, from_e);
      }
      close_it = 1;
      if (tmp_from_e && ((cache_mode != 1) || !cd)) free(tmp_from_e);
      if (tmp_to_e && ((cache_mode != -1) || !cd)) free(tmp_to_e);
    } else if (to_from_utf8) {
      /* Assume UTF-8 */
      mz_utf8 = 1;
    }
  }

  if (!cd && !mz_utf8) {
    if (out) {
      while (extra--) {
	out[extra] = 0;
      }
    }
    return out;
  }

  /* The converter is ready. Allocate out space, if necessary */

  if (!out) {
    if (iolen <= 0)
      iolen = iilen;
    out = (char *)scheme_malloc_atomic(iolen + extra);
    od = 0;
  }

  /* il and ol are the number of available chars */
  il = iilen;
  ol = iolen;
  /* dip and dop are the number of characters read so far;
     we use these and NULL out the ip and op pointers
     for the sake of precise GC */
  dip = 0;
  dop = 0;
  if (!in)
    add_end_shift = 0;

  while (1) {
    int icerr;

    if (mz_utf8) {
      /* Use our UTF-8 routines as if they were iconv */
      if (to_from_utf8 == 1) {
	/* UCS-4 -> UTF-8 */
	/* We assume that in + id and iilen are mzchar-aligned */
	int opos, uid, uilen;
	uid = (id + dip) >> 2;
	uilen = (iilen - dip) >> 2;
	opos = scheme_utf8_encode((const unsigned int *)in, uid, uilen,
				  NULL, 0,
				  0);
	if (opos <= iolen) {
	  opos = scheme_utf8_encode((const unsigned int *)in, uid, uilen,
				    (unsigned char *)out, od + dop,
				    0);
	  dop += opos;
	  dip += iilen;
	  icerr = 0;
	  r = (size_t)opos;
	} else {
	  icerr = E2BIG;
	  r = (size_t)-1;
	}
      } else {
	/* UTF-8 -> UCS-4 */
	/* We assume that out + od is mzchar-aligned */
	intptr_t ipos, opos;

	r = utf8_decode_x((unsigned char *)in, id + dip, iilen,
			  (unsigned int *)out, (od + dop) >> 2, iolen >> 2,
			  &ipos, &opos,
			  0, 0, NULL, 0, 0, 0);
	
	opos <<= 2;
	dop = (opos - od);
	dip = (ipos - id);

	if ((r == -1) || (r == -2)) {
	  r = (size_t)-1;
	  icerr = RKTIO_ERROR_CONVERT_BAD_SEQUENCE;
	} else if (r == -3) {
	  icerr = RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE;
	  r = (size_t)-1;
	} else
	  icerr = 0;
      }
    } else  {
      ip = in XFORM_OK_PLUS id + dip;
      op = out XFORM_OK_PLUS od + dop;
      r = rktio_convert(scheme_rktio, cd, &ip, &il, &op, &ol);
      dip = ip - (in XFORM_OK_PLUS id);
      dop = op - (out XFORM_OK_PLUS od);
      ip = op = NULL;
      icerr = rktio_get_last_error(scheme_rktio);
    }

    /* Record how many chars processed, now */
    if (oilen)
      *oilen = dip;
    *oolen = dop;

    /* Got all the chars? */
    if (r == RKTIO_CONVERT_ERROR) {
      if (icerr == RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE) {
	if (grow) {
	  /* Double the string size and try again */
	  char *naya;
	  naya = (char *)scheme_malloc_atomic((iolen * 2) + extra);
	  memcpy(naya, out + od, *oolen);
	  ol += iolen;
	  iolen += iolen;
	  out = naya;
	  od = 0;
	} else {
	  *status = 1;
	  if (close_it)
            do_convert_close(cd, cache_mode, to_e, from_e);
	  while (extra--) {
	    out[od + dop + extra] = 0;
	  }
	  return out;
	}
      } else {
	/* Either EINVAL (premature end) or EILSEQ (bad sequence) */
	if (icerr == RKTIO_ERROR_CONVERT_BAD_SEQUENCE)
	  *status = -2;
	if (close_it)
          do_convert_close(cd, cache_mode, to_e, from_e);
	while (extra--) {
	  out[od + dop + extra] = 0;
	}
	return out;
      }
    } else {
      /* All done... */
      if (add_end_shift) {
	add_end_shift = 0;
	in = NULL;
	dip = 0;
	id = 0;
	il = 0; /* should be redundant */
	oilen = NULL; /* so it doesn't get set to 0 */
      } else {
	*status = 0;
	if (close_it)
          do_convert_close(cd, cache_mode, to_e, from_e);
	while (extra--) {
	  out[od + dop + extra] = 0;
	}
	return out;
      }
    }
  }
}

#define MZ_SC_BUF_SIZE 32

static char *string_to_from_locale(int to_bytes,
				   char *in, intptr_t delta, intptr_t len,
				   intptr_t *olen, int perm,
				   int *no_cvt)
     /* Call this function only when iconv is available, and only when
	reset_locale() has been called */
{
  Scheme_Object *parts = scheme_null, *one;
  char *c, *le;
  intptr_t clen, used;
  int status;
  rktio_converter_t *cd;

  le = rktio_locale_encoding(scheme_rktio);
  if (cached_locale_encoding_name
      && !strcmp(le, cached_locale_encoding_name)
      && (to_bytes ? cached_locale_to_converter : cached_locale_from_converter)) {
    if (to_bytes) {
      cd = cached_locale_to_converter;
      cached_locale_to_converter = NULL;
    } else {
      cd = cached_locale_from_converter;
      cached_locale_from_converter = NULL;
    }
  } else {  
    if (to_bytes)
      cd = rktio_converter_open(scheme_rktio, le, MZ_UCS4_NAME);
    else
      cd = rktio_converter_open(scheme_rktio, MZ_UCS4_NAME, le);
  }

  if (!cd) {
    free(le);
    *no_cvt = 1;
    return NULL;
  }
  *no_cvt = 0;

  status = 0;

  while (len) {
    /* We might have conversion errors... */
    c = do_convert(cd, NULL, NULL, 0,
		   (char *)in, (to_bytes ? 4 : 1) * delta, (to_bytes ? 4 : 1) * len,
		   NULL, 0, (to_bytes ? 1 : 4) * (len + 1),
		   1 /* grow */, 1, (to_bytes ? 1 : 4) /* terminator size */,
		   &used, &clen,
		   &status);

    if (to_bytes)
      used >>= 2;

    if ((perm < 0) && (used < len)) {
      rktio_converter_close(scheme_rktio, cd);
      free(le);
      return NULL;
    }

    delta += used;
    len -= used;

    if (!len && SCHEME_NULLP(parts)) {
      if (to_bytes) {
	*olen = clen;
	c[*olen] = 0;
      } else {
	*olen = (clen >> 2);
	((mzchar *)c)[*olen] = 0;
      }
      cache_locale_or_close(to_bytes, cd, le);
      return c;
    }

    /* We can get here if there was some conversion error at some
       point. We're building up a list of parts. */

    if (to_bytes) {
      one = scheme_make_sized_byte_string(c, clen, 0);
    } else {
      one = scheme_make_sized_char_string((mzchar *)c, clen >> 2, 0);
    }

    parts = scheme_make_pair(one, parts);

    if (len) {
      /* Conversion error, so skip one char. */
      if (to_bytes) {
	char bc[1];
	bc[0] = perm;
	one = scheme_make_sized_byte_string(bc, 1, 1);
      } else {
	mzchar bc[1];
	bc[0] = perm;
	one = scheme_make_sized_char_string(bc, 1, 1);
      }
      parts = scheme_make_pair(one, parts);
      delta += 1;
      len -= 1;
    }
  }

  cache_locale_or_close(to_bytes, cd, le);

  if (to_bytes) {
    parts = append_all_byte_strings_backwards(parts);
    *olen = SCHEME_BYTE_STRTAG_VAL(parts);

    return SCHEME_BYTE_STR_VAL(parts);
  } else {
    parts = append_all_strings_backwards(parts);
    *olen = SCHEME_CHAR_STRTAG_VAL(parts);

    return (char *)SCHEME_CHAR_STR_VAL(parts);
  }
}

void cache_locale_or_close(int to_bytes, rktio_converter_t *cd, char *le)
{
  if (to_bytes ? cached_locale_to_converter : cached_locale_from_converter) {
    rktio_converter_close(scheme_rktio, cd);
    free(le);
  } else {
    if (!cached_locale_encoding_name || strcmp(le, cached_locale_encoding_name)) {
      scheme_clear_locale_cache();
      cached_locale_encoding_name = le;
    } else
      free(le);

    rktio_convert_reset(scheme_rktio, cd);
    if (to_bytes)
      cached_locale_to_converter = cd;
    else
      cached_locale_from_converter = cd;
  }
}

void scheme_clear_locale_cache(void)
{
  if (cached_locale_encoding_name) {
    if (cached_locale_to_converter) {
      rktio_converter_close(scheme_rktio, cached_locale_to_converter);
      cached_locale_to_converter = NULL;
    }
    if (cached_locale_from_converter) {
      rktio_converter_close(scheme_rktio, cached_locale_from_converter);
      cached_locale_from_converter = NULL;
    }
    free(cached_locale_encoding_name);
    cached_locale_encoding_name = NULL;
  }
}

static char *locale_recase(int to_up,
			   /* in must be null-terminated, iilen doesn't include it */
			   char *in, int id, int iilen,
			   /* iolen, in contrast, includes the terminator */
			   char *out, int od, int iolen,
			   intptr_t *oolen)
     /* Assumes that reset_locale() has been called */
{
  char *s, *s2;
  intptr_t len;
  s = rktio_locale_recase(scheme_rktio, to_up, in XFORM_OK_PLUS id);
  len = strlen(s);
  if ((len+1) < iolen) {
    memcpy(out XFORM_OK_PLUS od, s, len+1);
    s2 = out;
  } else {
    s2 = scheme_malloc_atomic(len+1);
    memcpy(s2, s, len+1);
  }
  free(s);
  *oolen = len;
  return s2;
}

int mz_locale_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UCS-4.
        Assumes that reset_locale() has been called. */
{
  intptr_t clen1, clen2, used1, used2, origl1, origl2;
  char *c1, *c2, buf1[MZ_SC_BUF_SIZE], buf2[MZ_SC_BUF_SIZE];
  char case_buf1[MZ_SC_BUF_SIZE], case_buf2[MZ_SC_BUF_SIZE];
  int status, got_more;

  /* First, convert UCS-4 to locale-specific encoding. If some
     characters don't fit into the encoding, then we'll have leftover
     characters. Count unconvertable charc as greater than anything
     that can be converted */

  origl1 = l1;
  origl2 = l2;

  /* Loop to check both convertable and unconvertable parts */
  while (1) {
    if (!origl1 && !origl2)
      return 0;
    if (!origl1)
      return -1;
    if (!origl2)
      return 1;

    /* Loop to get consistent parts of the wto strings, in case
       a conversion fails. */
    got_more = 0;
    l1 = origl1;
    l2 = origl2;
    while (1) {
      c1 = do_convert(NULL, MZ_UCS4_NAME, NULL, 1,
		      s1, d1 * 4, 4 * l1,
		      buf1, 0, MZ_SC_BUF_SIZE - 1,
		      1 /* grow */, 0, 1 /* terminator size */,
		      &used1, &clen1,
		      &status);
      c2 = do_convert(NULL, MZ_UCS4_NAME, NULL, 1,
		      s2, d2 * 4, 4 * l2,
		      buf2, 0, MZ_SC_BUF_SIZE - 1,
		      1 /* grow */, 0, 1 /* terminator size */,
		      &used2, &clen2,
		      &status);

      if ((used1 < 4 * l1) || (used2 < 4 * l2)) {
	if (got_more) {
	  /* Something went wrong. We've already tried to
	     even out the parts that work. Let's give up
	     on the first characters */
	  clen1 = clen2 = 0;
	  break;
	} else if (used1 == used2) {
	  /* Not everything, but both ended at the same point */
	  break;
	} else {
	  /* Pick the smallest */
	  if (used2 < used1) {
	    used1 = used2;
	    got_more = 1;
	  } else
	    got_more = 2;
	  l2 = (used1 >> 2);
	  l1 = (used1 >> 2);

	  if (!l1) {
	    /* Nothing to get this time. */
	    clen1 = clen2 = 0;
	    c1 = c2 = "";
	    used1 = used2 = 0;
	    break;
	  }
	}
      } else
	/* Got all that we wanted */
	break;
    }

    if (cvt_case) {
      if (clen1)
	c1 = locale_recase(0, c1, 0, clen1,
			   case_buf1, 0, MZ_SC_BUF_SIZE - 1,
			   &clen1);
      else
	c1 = NULL;
      if (clen2)
	c2 = locale_recase(0, c2, 0, clen2,
			   case_buf2, 0, MZ_SC_BUF_SIZE - 1,
			   &clen2);
      else
	c2 = NULL;
      /* There shouldn't have been conversion errors, but just in
	 case, care of NULL. */
      if (!c1) c1 = "";
      if (!c2) c2 = "";
    }

    /* Collate, finally. */
    status = rktio_locale_strcoll(scheme_rktio, c1, c2);

    /* If one is bigger than the other, we're done. */
    if (status)
      return status;

    /* Otherwise, is there more to check? */
    origl1 -= (used1 >> 2);
    origl2 -= (used2 >> 2);
    d1 += (used1 >> 2);
    d2 += (used2 >> 2);
    if (!origl1 && !origl2)
      return 0;

    /* There's more. It must be that the next character wasn't
       convertable in one of the encodings. */
    if (got_more)
      return ((got_more == 2) ? 1 : -1);

    if (!origl1)
      return -1;

    /* Compare an unconverable character directly. No case conversions
       if it's outside the locale. */
    if (((unsigned int *)s1)[d1] > ((unsigned int *)s2)[d2])
      return 1;
    else if (((unsigned int *)s1)[d1] < ((unsigned int *)s2)[d2])
      return -1;
    else {
      /* We've skipped one unconvertable char, and they still look the
	 same.  Now try again. */
      origl1 -= 1;
      origl2 -= 1;
      d1 += 1;
      d2 += 1;
    }
  }
}

int mz_native_strcoll(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case)
     /* The s1 and s2 arguments are actually UTF-16. */
{
  return rktio_strcoll_utf16(scheme_rktio,
                             (rktio_char16_t *)s1 XFORM_OK_PLUS d1, l1,
                             (rktio_char16_t *)s2 XFORM_OK_PLUS d2, l2,
                             cvt_case);
}

typedef int (*strcoll_proc)(char *s1, int d1, int l1, char *s2, int d2, int l2, int cvt_case);

int do_locale_comp(const char *who, const mzchar *us1, intptr_t ul1, const mzchar *us2, intptr_t ul2, int cvt_case)
{
  int xl1;
  int v, endres, utf16 = 0;
  GC_CAN_IGNORE strcoll_proc mz_strcoll = mz_locale_strcoll;

  if (current_locale_name
      && !*current_locale_name
      && (rktio_convert_properties(scheme_rktio) & RKTIO_CONVERT_STRCOLL_UTF16)) {
    utf16 = 1;
    mz_strcoll = mz_native_strcoll;
  }

  if (utf16) {
    us1 = (mzchar *)scheme_ucs4_to_utf16(us1, 0, ul1, NULL, 0, &ul1, 1);
    us2 = (mzchar *)scheme_ucs4_to_utf16(us2, 0, ul2, NULL, 0, &ul2, 1);
    ((short *)us1)[ul1] = 0;
    ((short *)us2)[ul2] = 0;
  }

  if (ul1 > ul2) {
    ul1 = ul2;
    endres = 1;
  } else {
    if (ul2 > ul1)
      endres = -1;
    else
      endres = 0;
  }

  /* Walk back through the strings looking for nul characters. If we
     find one, compare the part after the null character to update
     endres, then continue. Unfortunately, we do too much work if an
     earlier part of the string (tested later) determines the result,
     but hopefully nul characters are rare. */

  xl1 = 0;
  while (ul1--) {
    if ((utf16 && (!(((short *)us1)[ul1]) || !(((short *)us2)[ul1])))
	|| (!utf16 && (!(us1[ul1]) || !(us2[ul1])))) {
      if (utf16) {
	if (((short *)us1)[ul1])
	  endres = 1;
	else if (((short *)us2)[ul1])
	  endres = -1;
      } else {
	if (us1[ul1])
	  endres = 1;
	else if (us2[ul1])
	  endres = -1;
      }

      if (xl1)
	v = mz_strcoll((char *)us1, ul1 + 1, xl1, (char *)us2, ul1 + 1, xl1, cvt_case);
      else
	v = 0;

      if (v)
	endres = v;
      xl1 = 0;
    } else {
      xl1++;
    }
  }

  v = mz_strcoll((char *)us1, 0, xl1, (char *)us2, 0, xl1, cvt_case);
  if (v)
    endres = v;

  return endres;
}

mzchar *do_locale_recase(int to_up, mzchar *in, int delta, int len, intptr_t *olen)
{
  Scheme_Object *parts = scheme_null;
  char *c, buf[MZ_SC_BUF_SIZE], case_buf[MZ_SC_BUF_SIZE];
  intptr_t clen, used;
  int status;

  while (len) {
    /* We might have conversion errors... */
    c = do_convert(NULL, MZ_UCS4_NAME, NULL, 1,
		   (char *)in, 4 * delta, 4 * len,
		   buf, 0, MZ_SC_BUF_SIZE - 1,
		   1 /* grow */, 0, 1 /* terminator size */,
		   &used, &clen,
		   &status);

    used >>= 2;
    delta += used;
    len -= used;

    c = locale_recase(to_up, c, 0, clen,
		      case_buf, 0, MZ_SC_BUF_SIZE - 1,
		      &clen);
    if (!c)
      clen = 0;

    c = do_convert(NULL, NULL, MZ_UCS4_NAME, 2,
		   c, 0, clen,
		   NULL, 0, 0,
		   1 /* grow */, 0, sizeof(mzchar) /* terminator size */,
		   &used, &clen,
		   &status);

    if (!len && SCHEME_NULLP(parts)) {
      *olen = (clen >> 2);
      ((mzchar *)c)[*olen] = 0;
      return (mzchar *)c;
    }

    /* We can get here if there was some conversion error at some
       point. We're building up a list of parts. */

    parts = scheme_make_pair(scheme_make_sized_char_string((mzchar *)c, clen >> 2, 0),
			     parts);

    if (len) {
      /* Conversion error, so skip one char. */
      parts = scheme_make_pair(scheme_make_sized_offset_char_string(in, delta, 1, 1),
			       parts);
      delta += 1;
      len -= 1;
    }
  }

  parts = append_all_strings_backwards(parts);
  *olen = SCHEME_CHAR_STRTAG_VAL(parts);

  return SCHEME_CHAR_STR_VAL(parts);
}

mzchar *do_native_recase(int to_up, mzchar *in, int delta, int len, intptr_t *olen)
/* The in argument is actually UTF-16. */
{
  rktio_char16_t *s, *s2;
  intptr_t ol;

  s = rktio_recase_utf16(scheme_rktio, to_up, (rktio_char16_t *)in XFORM_OK_PLUS delta, len, &ol);

  s2 = scheme_malloc_atomic(sizeof(rktio_char16_t) * (ol+1));
  memcpy(s2, s, sizeof(rktio_char16_t) * (ol+1));
  free(s);
  
  *olen = ol;

  return (mzchar *)s2;
}

typedef mzchar *(*recase_proc)(int to_up, mzchar *in, int delta, int len, intptr_t *olen);

static Scheme_Object *mz_recase(const char *who, int to_up, mzchar *us, intptr_t ulen)
{
  intptr_t ulen1;
  int utf16 = 0, i, delta = 0;
  mzchar *us1;
  recase_proc mz_do_recase = do_locale_recase;
  Scheme_Object *s, *parts = scheme_null;

  reset_locale();

  if (current_locale_name
      && !*current_locale_name
      && (rktio_convert_properties(scheme_rktio) & RKTIO_CONVERT_RECASE_UTF16)) {
    utf16 = 1;
    mz_do_recase = do_native_recase;
  }

  if (utf16) {
    us = (mzchar *)scheme_ucs4_to_utf16(us, 0, ulen, NULL, 0, &ulen, 1);
    ((short *)us)[ulen] = 0;
  }

  /* If there are nuls in the string, then we have to make multiple
     calls to mz_do_recase */
  i = 0;
  while (1) {
    for (; i < ulen; i++) {
      if (utf16) {
	if (!((short *)us)[i])
	  break;
      } else if (!us[i])
	break;
    }

    us1 = mz_do_recase(to_up, us, delta, i - delta, &ulen1);

    if (utf16) {
      us1 = scheme_utf16_to_ucs4((unsigned short *)us1, 0, ulen1, NULL, 0, &ulen1, 1);
      us1[ulen1] = 0;
    }

    s = scheme_make_sized_char_string((mzchar *)us1, ulen1, 0);

    if (SCHEME_NULLP(parts) && (i == ulen))
      return s;

    parts = scheme_make_pair(s, parts);

    if (i == ulen)
      break;

    /* upcasing and encoding a nul char is easy: */
    s = scheme_make_sized_char_string((mzchar *)"\0\0\0\0", 1, 0);
    parts = scheme_make_pair(s, parts);
    i++;
    delta = i;

    if (i == ulen)
      break;
  }

  return append_all_strings_backwards(parts);
}

static Scheme_Object *
unicode_recase(const char *who, int to_up, int argc, Scheme_Object *argv[])
{
  intptr_t len;
  mzchar *chars;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  chars = SCHEME_CHAR_STR_VAL(argv[0]);
  len = SCHEME_CHAR_STRTAG_VAL(argv[0]);

  return mz_recase(who, to_up, chars, len);
}

static Scheme_Object *
string_locale_upcase(int argc, Scheme_Object *argv[])
{
  return unicode_recase("string-locale-upcase", 1, argc, argv);
}

static Scheme_Object *
string_locale_downcase(int argc, Scheme_Object *argv[])
{
  return unicode_recase("string-locale-downcase", 0, argc, argv);
}

static void reset_locale(void)
{
  Scheme_Object *v;
  const mzchar *name;

  /* This function needs to work before threads are set up: */
  if (scheme_current_thread) {
    v = scheme_get_param(scheme_current_config(), MZCONFIG_LOCALE);
  } else {
    v = scheme_make_immutable_sized_utf8_string("", 0);
  }
  locale_on = SCHEME_TRUEP(v);

  if (locale_on) {
    name = SCHEME_CHAR_STR_VAL(v);
    if ((current_locale_name != name)
        && (!current_locale_name
            || mz_char_strcmp("result-locale",
                              current_locale_name, scheme_char_strlen(current_locale_name),
                              name, SCHEME_CHAR_STRLEN_VAL(v),
                              0, 1))) {
      char *n, buf[32];

      n = scheme_utf8_encode_to_buffer(name, SCHEME_CHAR_STRLEN_VAL(v), buf, 32);

      rktio_set_locale(scheme_rktio, n);
    }

    current_locale_name_ptr = (void *)name;
  }
}

char *scheme_push_c_numeric_locale()
{
  return rktio_push_c_numeric_locale(scheme_rktio);
}

void scheme_pop_c_numeric_locale(char *prev)
{
  rktio_pop_c_numeric_locale(scheme_rktio, prev);
}

void scheme_set_default_locale(void)
{
  rktio_set_default_locale("");
}

static int find_special_casing(int ch)
{
  /* Binary search */
  int i, lo, hi, j;

  i = NUM_SPECIAL_CASINGS >> 1;
  lo = i;
  hi = NUM_SPECIAL_CASINGS - i - 1;

  while (1) {
    if (uchar_special_casings[i * 10] == ch)
      return i * 10;
    if (uchar_special_casings[i * 10] > ch) {
      j = i - lo;
      i = j + (lo >> 1);
      hi = lo - (i - j) - 1;
      lo = i - j;
    } else {
      j = i + 1;
      i = j + (hi >> 1);
      lo = i - j;
      hi = hi - (i - j) - 1;
    }
  }
}

static int is_final_sigma(int mode, mzchar *s, int d, int i, int len)
{
  int j;

  if (mode == 3)
    return 1;
  
  /* find a cased char before, skipping case-ignorable: */
  for (j = i - 1; j >= d; j--) {
    if (!scheme_iscaseignorable(s[j])) {
      if (scheme_iscased(s[j]))
	break;
      else
	return 0;
    }
  }
  if (j < d)
    return 0;

  /* next non-case-ignorable must not be cased: */
  for (j = i + 1; j < d + len; j++) {
    if (!scheme_iscaseignorable(s[j])) {
      return !scheme_iscased(s[j]);
    }
  }

  return 1;
}

mzchar *scheme_string_recase(mzchar *s, int d, int len, int mode, int inplace, int *_len)
{
  mzchar *t;
  int i, extra = 0, pos, special = 0, td, prev_was_cased = 0, xmode = mode;

  for (i = 0; i < len; i++) {
    if (scheme_isspecialcasing(s[d+i])) {
      pos = find_special_casing(s[d+i]);
      if (!uchar_special_casings[pos + 9] || is_final_sigma(xmode, s, d, i, len)) {
	special = 1;
	extra += (uchar_special_casings[pos + 1 + (xmode << 1)] - 1);
      }
    }
    if (mode == 2) {
      if (!scheme_iscaseignorable(s[d+i]))
	prev_was_cased = scheme_iscased(s[d+i]);
      xmode = (prev_was_cased ? 0 : 2);
    }
  }

  if (_len)
    *_len = len + extra;

  if (!extra && inplace) {
    t = s;
    td = d;
  } else {
    t = scheme_malloc_atomic(sizeof(mzchar) * (len + extra + 1));
    td = 0;
  }

  if (!special) {
    if (mode == 0) {
      for (i = 0; i < len; i++) {
	t[i+td] = scheme_tolower(s[i+d]);
      }
    } else if (mode == 1) {
      for (i = 0; i < len; i++) {
	t[i+td] = scheme_toupper(s[i+d]);
      }
    } else if (mode == 2) {
      prev_was_cased = 0;
      for (i = 0; i < len; i++) {
	if (!prev_was_cased)
	  t[i+td] = scheme_totitle(s[i+d]);
	else
	  t[i+td] = scheme_tolower(s[i+d]);
	if (!scheme_iscaseignorable(s[i+d]))
	  prev_was_cased = scheme_iscased(s[i+d]);
      }
    } else /* if (mode == 3) */ {
      for (i = 0; i < len; i++) {
	t[i+td] = scheme_tofold(s[i+d]);
      }
    }
  } else {
    int j = 0, c;
    prev_was_cased = 0;
    for (i = 0; i < len; i++) {
      if (mode == 0) {
	t[j+td] = scheme_tolower(s[i+d]);
      } else if (mode == 1) {
	t[j+td] = scheme_toupper(s[i+d]);
      } else if (mode == 2) {
	if (!prev_was_cased) {
	  xmode = 2;
	  t[j+td] = scheme_totitle(s[i+d]);
	} else {
	  xmode = 0;
	  t[j+td] = scheme_tolower(s[i+d]);
	}
	if (!scheme_iscaseignorable(s[i+d]))
	  prev_was_cased = scheme_iscased(s[i+d]);
      } else /* if (mode == 3) */ {
	t[j+td] = scheme_tofold(s[i+d]);
      }

      if (scheme_isspecialcasing(s[i+d])) {
	pos = find_special_casing(s[i+d]);
	if (!uchar_special_casings[pos + 9] || is_final_sigma(xmode, s, d, i, len)) {
	  c = uchar_special_casings[pos + 1 + (xmode << 1)];
	  pos = uchar_special_casings[pos + 2 + (xmode << 1)];
	  while (c--) {
	    t[(j++)+td] = uchar_special_casing_data[pos++];
	  }
	} else
	  j++;
      } else
	j++;
    }
  }
  t[len+extra+td] = 0;

  return t;
}

static Scheme_Object *string_recase (const char *name, int argc, Scheme_Object *argv[], int mode)
{
  mzchar *s;
  int len;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract(name, "string?", 0, argc, argv);
  
  s = SCHEME_CHAR_STR_VAL(argv[0]);
  len = SCHEME_CHAR_STRLEN_VAL(argv[0]);

  s = scheme_string_recase(s, 0, len, mode, 0, &len);

  return scheme_make_sized_char_string(s, len, 0);
}

static Scheme_Object *string_upcase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-upcase", argc, argv, 1);
}

static Scheme_Object *string_downcase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-downcase", argc, argv, 0);
}

static Scheme_Object *string_titlecase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-titlecase", argc, argv, 2);
}

static Scheme_Object *string_foldcase (int argc, Scheme_Object *argv[])
{
  return string_recase("string-foldcase", argc, argv, 3);
}

/**********************************************************************/
/*                          normalization                             */
/**********************************************************************/

#define MZ_JAMO_INITIAL_CONSONANT_START  0x1100
#define MZ_JAMO_INITIAL_CONSONANT_COUNT  19
#define MZ_JAMO_INITIAL_CONSONANT_END    (MZ_JAMO_INITIAL_CONSONANT_START + MZ_JAMO_INITIAL_CONSONANT_COUNT - 1)

#define MZ_JAMO_VOWEL_START              0x1161
#define MZ_JAMO_VOWEL_COUNT              21
#define MZ_JAMO_VOWEL_END                (MZ_JAMO_VOWEL_START + MZ_JAMO_VOWEL_COUNT - 1)

/* First in this range is not actually a consonant, but a placeholder for "no consonant" */
#define MZ_JAMO_TRAILING_CONSONANT_START 0x11A7
#define MZ_JAMO_TRAILING_CONSONANT_COUNT 28
#define MZ_JAMO_TRAILING_CONSONANT_END   (MZ_JAMO_TRAILING_CONSONANT_START + MZ_JAMO_TRAILING_CONSONANT_COUNT - 1)

#define MZ_JAMO_SYLLABLE_START           0xAC00
#define MZ_JAMO_SYLLABLE_END             (MZ_JAMO_SYLLABLE_START + 11171)

XFORM_NONGCING static mzchar get_composition(mzchar a, mzchar b)
{
  if ((a > 0xFFFF) || (b > 0xFFFF)) {
    /* Look in long-composes table. */
    mzlonglong key = ((((mzlonglong)a & 0x1F0000) << 21)
                      | (((mzlonglong)a & 0xFFFF) << 16)
                      | (((mzlonglong)b & 0x1F0000) << 16)
                      | ((mzlonglong)b & 0xFFFF));
    int pos = (LONG_COMPOSE_TABLE_SIZE >> 1), new_pos;
    int below_len = pos;
    int above_len = (LONG_COMPOSE_TABLE_SIZE - pos - 1);

    /* Binary search: */
    while (key != utable_canon_compose_long_pairs[pos]) {
      if (key > utable_canon_compose_long_pairs[pos]) {
        if (!above_len)
          return 0;
        new_pos = pos + (above_len >> 1) + 1;
        below_len = (new_pos - pos - 1);
        above_len = (above_len - below_len - 1);
        pos = new_pos;
      } else if (key < utable_canon_compose_long_pairs[pos]) {
        if (!below_len)
          return 0;
        new_pos = pos - ((below_len >> 1) + 1);
        above_len = (pos - new_pos - 1);
        below_len = (below_len - above_len - 1);
        pos = new_pos;
      }
    }
    
    return utable_canon_compose_long_result[pos];
  } else {
    uintptr_t key = (a << 16) | b;
    int pos = (COMPOSE_TABLE_SIZE >> 1), new_pos;
    int below_len = pos;
    int above_len = (COMPOSE_TABLE_SIZE - pos - 1);

    /* Binary search: */
    while (key != utable_compose_pairs[pos]) {
      if (key > utable_compose_pairs[pos]) {
        if (!above_len)
          return 0;
        new_pos = pos + (above_len >> 1) + 1;
        below_len = (new_pos - pos - 1);
        above_len = (above_len - below_len - 1);
        pos = new_pos;
      } else if (key < utable_compose_pairs[pos]) {
        if (!below_len)
          return 0;
        new_pos = pos - ((below_len >> 1) + 1);
        above_len = (pos - new_pos - 1);
        below_len = (below_len - above_len - 1);
        pos = new_pos;
      }
    }
    
    return utable_compose_result[pos];
  }
}

XFORM_NONGCING mzchar get_canon_decomposition(mzchar key, mzchar *b)
{
  int pos = (DECOMPOSE_TABLE_SIZE >> 1), new_pos;
  int below_len = pos;
  int above_len = (DECOMPOSE_TABLE_SIZE - pos - 1);

  /* Binary search: */
  while (key != utable_decomp_keys[pos]) {
    if (key > utable_decomp_keys[pos]) {
      if (!above_len)
	return 0;
      new_pos = pos + (above_len >> 1) + 1;
      below_len = (new_pos - pos - 1);
      above_len = (above_len - below_len - 1);
      pos = new_pos;
    } else if (key < utable_decomp_keys[pos]) {
      if (!below_len)
	return 0;
      new_pos = pos - ((below_len >> 1) + 1);
      above_len = (pos - new_pos - 1);
      below_len = (below_len - above_len - 1);
      pos = new_pos;
    }
  }

  pos = utable_decomp_indices[pos];
  if (pos < 0) {
    pos = -(pos + 1);
    pos <<= 1;
    *b = utable_compose_long_pairs[pos + 1];
    return utable_compose_long_pairs[pos];
  } else {
    key = utable_compose_pairs[pos];
    *b = (key & 0xFFFF);
    return (key >> 16);
  }
}

XFORM_NONGCING int get_kompat_decomposition(mzchar key, unsigned short **chars)
{
  int pos = (KOMPAT_DECOMPOSE_TABLE_SIZE >> 1), new_pos;
  int below_len = pos;
  int above_len = (KOMPAT_DECOMPOSE_TABLE_SIZE - pos - 1);

  /* Binary search: */
  while (key != utable_kompat_decomp_keys[pos]) {
    if (key > utable_kompat_decomp_keys[pos]) {
      if (!above_len)
	return 0;
      new_pos = pos + (above_len >> 1) + 1;
      below_len = (new_pos - pos - 1);
      above_len = (above_len - below_len - 1);
      pos = new_pos;
    } else if (key < utable_kompat_decomp_keys[pos]) {
      if (!below_len)
	return 0;
      new_pos = pos - ((below_len >> 1) + 1);
      above_len = (pos - new_pos - 1);
      below_len = (below_len - above_len - 1);
      pos = new_pos;
    }
  }

  *chars = utable_kompat_decomp_strs XFORM_OK_PLUS utable_kompat_decomp_indices[pos];
  return utable_kompat_decomp_lens[pos];
}

static Scheme_Object *normalize_c(Scheme_Object *o)
/* Assumes then given string is in normal form D */
{
  mzchar *s, *s2, tmp, last_c0 = 0;
  int len, i, j = 0, last_c0_pos = 0, last_cc = 0;

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  s2 = (mzchar *)scheme_malloc_atomic((len + 1) * sizeof(mzchar));
  memcpy(s2, s, len * sizeof(mzchar));
  
  for (i = 0; i < len; i++) {
    if ((i + 1 < len)
	&& (s2[i] >= MZ_JAMO_INITIAL_CONSONANT_START)
	&& (s2[i] <= MZ_JAMO_INITIAL_CONSONANT_END)
	&& (s2[i+1] >= MZ_JAMO_VOWEL_START)
	&& (s2[i+1] <= MZ_JAMO_VOWEL_END)) {
      /* Need Hangul composition */
      if ((i + 2 < len)
	  && (s2[i+2] > MZ_JAMO_TRAILING_CONSONANT_START)
	  && (s2[i+2] <= MZ_JAMO_TRAILING_CONSONANT_END)) {
	/* 3-char composition */
	tmp = (MZ_JAMO_SYLLABLE_START
	       + ((s2[i] - MZ_JAMO_INITIAL_CONSONANT_START) 
		  * MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT)
	       + ((s2[i+1] - MZ_JAMO_VOWEL_START)
		  * MZ_JAMO_TRAILING_CONSONANT_COUNT)
	       + (s2[i+2] - MZ_JAMO_TRAILING_CONSONANT_START));
	i += 2;
      } else {
	/* 2-char composition */
	tmp = (MZ_JAMO_SYLLABLE_START
	       + ((s2[i] - MZ_JAMO_INITIAL_CONSONANT_START) 
		  * MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT)
	       + ((s2[i+1] - MZ_JAMO_VOWEL_START)
		  * MZ_JAMO_TRAILING_CONSONANT_COUNT));
	i++;
      }
      last_c0 = tmp;
      last_c0_pos = j;
      last_cc = 0;
      s2[j++] = tmp;
    } else {
      int cc;
      
      cc = scheme_combining_class(s2[i]);
      if (last_c0 && (cc > last_cc))
	tmp = get_composition(last_c0, s2[i]);
      else
	tmp = 0;

      if (tmp) {
	/* Need to compose */
	s2[last_c0_pos] = tmp;
	last_c0 = tmp;
      } else if (!cc) {
	/* Reset last_c0... */
	tmp = s2[i];
	if (scheme_needs_maybe_compose(tmp)) {
	  last_c0 = tmp;
	  last_c0_pos = j;
	} else {
	  last_c0 = 0;
	}
	last_cc = -1;
	s2[j++] = tmp;
      } else {
	s2[j++] = s2[i];
	last_cc = cc;
      }
    }
  }

  s2[j] = 0;
  if (len - j > 16) {
    s = (mzchar *)scheme_malloc_atomic((j + 1) * sizeof(mzchar));
    memcpy(s, s2, (j + 1) * sizeof(mzchar));
    s2 = s;
  }

  return scheme_make_sized_char_string(s2, j, 0);
}

static Scheme_Object *normalize_d(Scheme_Object *o, int kompat)
{
  mzchar *s, tmp, *s2;
  int len, i, delta, j, swapped;

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  /* Run through string list to predict expansion: */
  delta = 0;
  for (i = 0; i < len; i++) {
    if (scheme_needs_decompose(s[i])) {
      int klen;
      mzchar snd;
      GC_CAN_IGNORE unsigned short *start;

      tmp = s[i];
      while (scheme_needs_decompose(tmp)) {
	if (kompat)
	  klen = get_kompat_decomposition(tmp, &start);
	else
	  klen = 0;
	if (klen) {
	  delta += (klen - 1);
	  break;
	} else {
	  tmp = get_canon_decomposition(tmp, &snd);
	  if (tmp) {
	    if (snd) {
	      delta++;
	      if (kompat) {
		klen = get_kompat_decomposition(snd, &start);
		if (klen)
		  delta += (klen - 1);
	      }
	    }
	  } else
	    break;
	}
      }
    } else if ((s[i] >= MZ_JAMO_SYLLABLE_START)
	       && (s[i] <= MZ_JAMO_SYLLABLE_END)) {
      tmp = s[i];
      tmp -= MZ_JAMO_SYLLABLE_START;
      if (tmp % MZ_JAMO_TRAILING_CONSONANT_COUNT)
	delta += 2;
      else
	delta += 1;
    }
  }

  s2 = (mzchar *)scheme_malloc_atomic((len + delta + 1) * sizeof(mzchar));

  j = 0;
  for (i = 0; i < len; i++) {
    if (scheme_needs_decompose(s[i])) {
      mzchar snd, tmp2;
      int snds = 0, klen = 0, k;
      GC_CAN_IGNORE unsigned short*start;

      tmp = s[i];
      while (scheme_needs_decompose(tmp)) {
	if (kompat)
	  klen = get_kompat_decomposition(tmp, &start);
	else
	  klen = 0;
	if (klen) {
	  for (k = 0; k < klen; k++) {
	    s2[j++] = start[k];
	  }
	  break;
	} else {
	  tmp2 = get_canon_decomposition(tmp, &snd);
	  if (tmp2) {
	    tmp = tmp2;
	    if (snd) {
	      if (kompat)
		klen = get_kompat_decomposition(snd, &start);
	      else
		klen = 0;
	      if (klen) {
		snds += klen;
		for (k = 0; k < klen; k++) {
		  s2[len + delta - snds + k] = start[k];
		}
		klen = 0;
	      } else {
		snds++;
		s2[len + delta - snds] = snd;
	      }
	    }
	  } else 
	    break;
	}
      }
      if (!klen)
	s2[j++] = tmp;
      memmove(s2 + j, s2 + len + delta - snds, snds * sizeof(mzchar));
      j += snds;
    } else if ((s[i] >= MZ_JAMO_SYLLABLE_START)
	       && (s[i] <= MZ_JAMO_SYLLABLE_END)) {
      int l, v, t;
      tmp = s[i];
      tmp -= MZ_JAMO_SYLLABLE_START;
      l = tmp / (MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT);
      v = (tmp % (MZ_JAMO_VOWEL_COUNT * MZ_JAMO_TRAILING_CONSONANT_COUNT)) / MZ_JAMO_TRAILING_CONSONANT_COUNT;
      t = tmp % MZ_JAMO_TRAILING_CONSONANT_COUNT;
      s2[j++] = MZ_JAMO_INITIAL_CONSONANT_START + l;
      s2[j++] = MZ_JAMO_VOWEL_START + v;
      if (t) {
	s2[j++] = MZ_JAMO_TRAILING_CONSONANT_START + t;
      }
    } else {
      s2[j++] = s[i];
    }
  }
  s2[j] = 0;
  len += delta;

  /* Reorder pass: */
  do {
    swapped = 0;
    for (i = 0; i < len; i++) {
      if ((i + 1 < len)
	  && scheme_combining_class(s2[i])
	  && scheme_combining_class(s2[i+1])
	  && (scheme_combining_class(s2[i+1]) < scheme_combining_class(s2[i]))) {
	/* Reorder and try again: */
	tmp = s2[i + 1];
	s2[i + 1] = s2[i];
	s2[i] = tmp;
	i--;
	swapped = 1;
      }
    }
  } while (swapped);

  return scheme_make_sized_char_string(s2, len, 0);
}

static Scheme_Object *do_string_normalize_c (const char *who, int argc, Scheme_Object *argv[], int kompat)
{
  Scheme_Object *o;
  mzchar *s, last_c0 = 0, snd;
  int len, i, last_cc = 0;

  o = argv[0];
  if (!SCHEME_CHAR_STRINGP(o))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  for (i = 0; i < len; i++) {
    if (scheme_needs_decompose(s[i])
	&& (kompat || get_canon_decomposition(s[i], &snd))) {
      /* Decomposition may expose a different composition */
      break;
    } else if ((i + 1 < len)
	&& scheme_combining_class(s[i])
	&& scheme_combining_class(s[i+1])
	&& (scheme_combining_class(s[i+1]) < scheme_combining_class(s[i]))) {
      /* Need to reorder */
      break;
    } else if ((s[i] >= MZ_JAMO_INITIAL_CONSONANT_START)
	       && (s[i] <= MZ_JAMO_INITIAL_CONSONANT_END)
	       && (s[i+1] >= MZ_JAMO_VOWEL_START)
	       && (s[i+1] <= MZ_JAMO_VOWEL_END)) {
      /* Need Hangul composition */
      break;
    } else if (last_c0 
	       && get_composition(last_c0, s[i])
	       && (scheme_combining_class(s[i]) > last_cc)) {
      /* Need to compose */
      break;
    } else {
      int cc;

      cc = scheme_combining_class(s[i]);

      if (!cc) {
	if (scheme_needs_maybe_compose(s[i]))
	  last_c0 = s[i];
	else
	  last_c0 = 0;
	last_cc = -1;
      } else
	last_cc = cc;
    }
  }

  if (i < len) {
    o = normalize_c(normalize_d(o, kompat));
  }

  return o;
}

static Scheme_Object *string_normalize_c (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_c("string-normalize-nfc", argc, argv, 0);
}

static Scheme_Object *string_normalize_kc (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_c("string-normalize-nfkc", argc, argv, 1);
}

static Scheme_Object *do_string_normalize_d (const char *who, int argc, Scheme_Object *argv[], int kompat)
{
  Scheme_Object *o;
  mzchar *s;
  int len, i;

  o = argv[0];
  if (!SCHEME_CHAR_STRINGP(o))
    scheme_wrong_contract(who, "string?", 0, argc, argv);

  s = SCHEME_CHAR_STR_VAL(o);
  len = SCHEME_CHAR_STRLEN_VAL(o);

  for (i = len; i--; ) {
    if (scheme_needs_decompose(s[i])) {
      /* Need to decompose */
      mzchar snd;
      if (kompat || get_canon_decomposition(s[i], &snd))
	break;
    } else if ((i + 1 < len)
	       && scheme_combining_class(s[i])
	       && scheme_combining_class(s[i+1])
	       && (scheme_combining_class(s[i+1]) < scheme_combining_class(s[i]))) {
      /* Need to reorder */
      break;
    } else if ((s[i] >= MZ_JAMO_SYLLABLE_START)
	       && (s[i] <= MZ_JAMO_SYLLABLE_END)) {
      /* Need Hangul decomposition */
      break;
    }
  }

  if (i >= 0) {
    o = normalize_d(o, kompat);
  }

  return o;
}

static Scheme_Object *string_normalize_d (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_d("string-normalize-nfd", argc, argv, 0);
}

static Scheme_Object *string_normalize_kd (int argc, Scheme_Object *argv[])
{
  return do_string_normalize_d("string-normalize-nfkd", argc, argv, 1);
}

/**********************************************************************/
/*                            strcmps                                 */
/**********************************************************************/

intptr_t scheme_char_strlen(const mzchar *s)
{
  intptr_t i;
  if ((intptr_t)s & 0x3)
    abort();
  for (i = 0; s[i]; i++) {
  }
  return i;
}

static int mz_char_strcmp(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, 
			  int use_locale, int size_shortcut)
{
  intptr_t endres;

  if (size_shortcut && (l1 != l2))
    return 1;

  if (use_locale) {
    reset_locale();
    if (locale_on) {
      return do_locale_comp(who, str1, l1, str2, l2, 0);
    }
  }

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

  while (l1--) {
    unsigned int a, b;

    a = *(str1++);
    b = *(str2++);

    a = a - b;
    if (a)
      return a;
  }

  return endres;
}

static int mz_char_strcmp_ci(const char *who, const mzchar *str1, intptr_t l1, const mzchar *str2, intptr_t l2, 
			     int use_locale, int size_shortcut)
{
  intptr_t p1, p2, sp1, sp2, a, b;
  mzchar spec1[SPECIAL_CASE_FOLD_MAX], spec2[SPECIAL_CASE_FOLD_MAX];

  if (size_shortcut && (l1 != l2))
    return 1;

  if (use_locale) {
    reset_locale();
    if (locale_on) {
      return do_locale_comp(who, str1, l1, str2, l2, 1);
    }
  }

  p1 = sp1 = 0;
  p2 = sp2 = 0;

  while (((p1 < l1) || sp1) && ((p2 < l2) || sp2)) {
    if (sp1) {
      a = spec1[--sp1];
    } else {
      a = str1[p1];
      if (scheme_isspecialcasing(a)) {
	int pos, i;
	pos = find_special_casing(a);
	sp1 = uchar_special_casings[pos + 7];
	pos = uchar_special_casings[pos + 8];
	for (i = sp1; i--; pos++) {
	  spec1[i] = uchar_special_casing_data[pos];
	}
	a = spec1[--sp1];
      } else {
	a = scheme_tofold(a);
      }
      p1++;
    }

    if (sp2) {
      b = spec2[--sp2];
    } else {
      b = str2[p2];
      if (scheme_isspecialcasing(b)) {
	int pos, i;
	pos = find_special_casing(b);
	sp2 = uchar_special_casings[pos + 7];
	pos = uchar_special_casings[pos + 8];
	for (i = sp2; i--; pos++) {
	  spec2[i] = uchar_special_casing_data[pos];
	}
	b = spec2[--sp2];
      } else {
	b = scheme_tofold(b);
      }
      p2++;
    }

    a = a - b;
    if (a)
      return a;
  }

  return ((p1 < l1) || sp1) - ((p2 < l2) || sp2);
}

static int mz_strcmp(const char *who, unsigned char *str1, intptr_t l1, unsigned char *str2, intptr_t l2)
{
  intptr_t endres;

  if (l1 > l2) {
    l1 = l2;
    endres = 1;
  } else {
    if (l2 > l1)
      endres = -1;
    else
      endres = 0;
  }

  while (l1--) {
    unsigned int a, b;

    a = *(str1++);
    b = *(str2++);

    a = a - b;
    if (a)
      return a;
  }

  return endres;
}

int scheme_string_compare(Scheme_Object *a, Scheme_Object *b)
{
  return mz_char_strcmp(NULL,
                        SCHEME_CHAR_STR_VAL(a),  SCHEME_CHAR_STRTAG_VAL(a),
                        SCHEME_CHAR_STR_VAL(b),  SCHEME_CHAR_STRTAG_VAL(b),
                        0, 0);
}

int scheme_bytes_compare(Scheme_Object *a, Scheme_Object *b)
{
  return mz_strcmp(NULL,
                   (unsigned char *)SCHEME_BYTE_STR_VAL(a),  SCHEME_BYTE_STRTAG_VAL(a),
                   (unsigned char *)SCHEME_BYTE_STR_VAL(b),  SCHEME_BYTE_STRTAG_VAL(b));
}

/**********************************************************************/
/*                  byte string conversion                            */
/**********************************************************************/

static void close_converter(Scheme_Object *o, void *data)
{
  Scheme_Converter *c = (Scheme_Converter *)o;

  if (!c->closed) {
    c->closed = 1;
    if (c->kind == mzICONV_KIND) {
      rktio_converter_close(scheme_rktio, c->cd);
      c->cd = NULL;
    }
    if (c->mref) {
      scheme_remove_managed(c->mref, (Scheme_Object *)c);
      c->mref = NULL;
    }
  }
}

Scheme_Object *scheme_open_converter(const char *from_e, const char *to_e)
{
  Scheme_Converter *c;
  rktio_converter_t *cd;
  int kind;
  int permissive, wtf;
  int need_regis = 1;
  Scheme_Custodian_Reference *mref;

  if (!*to_e || !*from_e)
    reset_locale();

  if ((!strcmp(from_e, "UTF-8")
       || !strcmp(from_e, "UTF-8-permissive")
       || (!*from_e && mzLOCALE_IS_UTF_8(current_locale_name)))
      && (!strcmp(to_e, "UTF-8")
	  || (!*to_e && mzLOCALE_IS_UTF_8(current_locale_name)))) {
    /* Use the built-in UTF-8<->UTF-8 converter: */
    kind = mzUTF8_KIND;
    if (!strcmp(from_e, "UTF-8-permissive"))
      permissive = 0xFFFD;
    else
      permissive = 0;
    cd = NULL;
    need_regis = 0;
    wtf = 0;
  } else if ((!strcmp(from_e, "platform-UTF-8")
	      || !strcmp(from_e, "platform-UTF-8-permissive"))
	     && !strcmp(to_e, "platform-UTF-16")) {
    kind = mzUTF8_TO_UTF16_KIND;
    if (!strcmp(from_e, "platform-UTF-8-permissive"))
      permissive = 0xFFFD;
    else
      permissive = 0;
    cd = NULL;
    need_regis = 0;
    wtf = WIN_UTF16_AS_WTF16(1);
  } else if ((!strcmp(from_e, "WTF-8")
	      || !strcmp(from_e, "WTF-8-permissive"))
	     && !strcmp(to_e, "WTF-16")) {
    kind = mzUTF8_TO_UTF16_KIND;
    if (!strcmp(from_e, "WTF-8-permissive"))
      permissive = 0xFFFD;
    else
      permissive = 0;
    cd = NULL;
    need_regis = 0;
    wtf = 1;
  } else if (!strcmp(from_e, "platform-UTF-16")
	     && !strcmp(to_e, "platform-UTF-8")) {
    kind = mzUTF16_TO_UTF8_KIND;
    permissive = 0;
    cd = NULL;
    need_regis = 0;
    wtf = WIN_UTF16_AS_WTF16(1);
  } else if (!strcmp(from_e, "WTF-16")
	     && !strcmp(to_e, "WTF-8")) {
    kind = mzUTF16_TO_UTF8_KIND;
    permissive = 0;
    cd = NULL;
    need_regis = 0;
    wtf = 1;
  } else {
    char *tmp_from_e = NULL, *tmp_to_e = NULL;

    if (!(rktio_convert_properties(scheme_rktio) & RKTIO_CONVERTER_SUPPORTED))
      return scheme_false;

    if (!*from_e || !*to_e)
      reset_locale();

    if (!*from_e) {
      tmp_from_e = rktio_locale_encoding(scheme_rktio);
      from_e = tmp_from_e;
    }
    if (!*to_e) {
      tmp_to_e = rktio_locale_encoding(scheme_rktio);
      to_e = tmp_to_e;
    }
    cd = rktio_converter_open(scheme_rktio, to_e, from_e);

    if (tmp_from_e) free(tmp_from_e);
    if (tmp_to_e) free(tmp_to_e);
    
    if (!cd)
      return scheme_false;

    kind = mzICONV_KIND;
    permissive = 0;
    wtf = 0;
  }

  c = MALLOC_ONE_TAGGED(Scheme_Converter);
  c->so.type = scheme_string_converter_type;
  c->closed = 0;
  c->kind = kind;
  c->permissive = permissive;
  c->wtf = wtf;
  c->cd = cd;
  if (!need_regis)
    mref = NULL;
  else
    mref = scheme_add_managed(NULL,
			      (Scheme_Object *)c,
			      close_converter,
			      NULL, 1);
  c->mref = mref;

  return (Scheme_Object *)c;
}

static Scheme_Object *byte_string_open_converter(int argc, Scheme_Object **argv)
{
  Scheme_Object *s1, *s2;
  char *from_e, *to_e;
  
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_contract("bytes-open-converter", "bytes?", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_contract("bytes-open-converter", "bytes?", 1, argc, argv);

  scheme_custodian_check_available(NULL, "bytes-open-converter", "converter");

  s1 = scheme_char_string_to_byte_string(argv[0]);
  s2 = scheme_char_string_to_byte_string(argv[1]);

  if (scheme_byte_string_has_null(s1))
    return scheme_false;
  if (scheme_byte_string_has_null(s2))
    return scheme_false;

  from_e = SCHEME_BYTE_STR_VAL(s1);
  to_e = SCHEME_BYTE_STR_VAL(s2);

  return scheme_open_converter(from_e, to_e);
}

static Scheme_Object *convert_one(const char *who, int opos, int argc, Scheme_Object *argv[])
{
  char *r, *instr;
  int status;
  intptr_t amt_read, amt_wrote;
  intptr_t istart, ifinish, ostart, ofinish;
  Scheme_Object *a[3], *status_sym;
  Scheme_Converter *c;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type))
    scheme_wrong_contract(who, "bytes-converter?", 0, argc, argv);

  if (opos > 1) {
    if (!SCHEME_BYTE_STRINGP(argv[1]))
      scheme_wrong_contract(who, "bytes?", 1, argc, argv);
    scheme_get_substring_indices(who, argv[1], argc, argv, 2, 3, &istart, &ifinish);
  } else {
    istart = 0;
    ifinish = 0;
  }

  if (argc > opos) {
    if (SCHEME_TRUEP(argv[opos])) {
      if (!SCHEME_MUTABLE_BYTE_STRINGP(argv[opos]))
	scheme_wrong_contract(who, "(and/c bytes? (not/c immutable?))", opos, argc, argv);
      r = SCHEME_BYTE_STR_VAL(argv[opos]);
      scheme_get_substring_indices(who, argv[opos], argc, argv, opos + 1, opos + 2, &ostart, &ofinish);
    } else {
      int ip;
      r = NULL;
      for (ip = opos + 1; ip <= opos + 2; ip++) {
	if (argc > ip) {
	  int ok = 0;
	  if (SCHEME_INTP(argv[ip]))
	    ok = SCHEME_INT_VAL(argv[ip]) >= 0;
	  else if (SCHEME_BIGNUMP(argv[ip]))
	    ok = SCHEME_BIGPOS(argv[ip]);
	  else if ((ip == opos + 2) && SCHEME_FALSEP(argv[ip]))
	    ok = 1;
	  if (!ok)
	    scheme_wrong_contract(who,
                                  ((ip == opos + 2)
                                   ? "(or/c exact-nonnegative-integer? #f)"
                                   : "exact-nonnegative-integer?"),
                                  ip, argc, argv);
	}
      }
      if ((argc > opos + 2) && SCHEME_TRUEP(argv[opos + 2])) {
	Scheme_Object *delta;
	if (scheme_bin_lt(argv[opos + 2], argv[opos + 1])) {
	  scheme_contract_error(who,
                                "ending index is less than the starting index",
                                "staring index", 1, argv[opos + 1],
                                "ending index", 1, argv[opos + 2],
                                NULL);
	}
	delta = scheme_bin_minus(argv[opos + 2], argv[opos + 1]);
	if (SCHEME_BIGNUMP(delta))
	  ofinish = -1;
	else
	  ofinish = SCHEME_INT_VAL(delta);
	ostart = 0;
      } else {
	ostart = 0;
	ofinish = -1;
      }
    }
  } else {
    r = NULL;
    ostart = 0;
    ofinish = -1;
  }

  c = (Scheme_Converter *)argv[0];
  if (c->closed)
    scheme_contract_error(who, "converter is closed", 
                          "converter", 1, argv[0],
                          NULL);

  instr = ((opos > 1) ? SCHEME_BYTE_STR_VAL(argv[1]) : NULL);

  if (c->kind == mzUTF16_TO_UTF8_KIND) {
    if (istart & 0x1) {
      /* Copy to word-align */
      char *c2;
      c2 = (char *)scheme_malloc_atomic(ifinish - istart);
      memcpy(c2, instr XFORM_OK_PLUS istart, ifinish - istart);
      ifinish = ifinish - istart;
      istart = 0;
      instr = c2;
    }

    status = utf8_encode_x((const unsigned int *)instr, istart >> 1, ifinish >> 1,
			   (unsigned char *)r, ostart, ofinish,
			   &amt_read, &amt_wrote, 1, c->wtf);
    
    amt_read -= (istart >> 1);

    if (amt_read) {
      if (!r) {
	/* Need to allocate, then do it again: */
	r = (char *)scheme_malloc_atomic(amt_wrote + 1);
	utf8_encode_x((const unsigned int *)instr, istart >> 1, ifinish >> 1,
		      (unsigned char *)r, ostart, ofinish,
		      NULL, NULL, 1, c->wtf);
	r[amt_wrote] = 0;
      }
      amt_read <<= 1;
    }

    /* We might get a -1 result because the input has an odd number of
       bytes, and 2nd+next-to-last bytes form an unpaired
       surrogate. In that case, the transformer normally needs one
       more byte: Windows is little-endian, so we need the byte to
       tell whether the surrogate is paired, and for all other
       platforms (where we assume that surrogates are paired), we need
       the byte to generate output. Technically, on a big-endian
       non-Windows machine, we could generate the first byte of UTF-8
       output and keep the byte as state, but we don't. */

    if (status != -1) {
      if (amt_read < ((ifinish - istart) & ~0x1)) {
	/* Must have run out of output space */
	status = 1;
      } else {
	/* Read all of input --- but it wasn't really all if there
	   was an odd number of bytes. */
	if ((ifinish - istart) & 0x1)
	  status = -1;
	else
	  status = 0;
      }
    }
  } else if (c->kind != mzICONV_KIND) {
    /* UTF-8 -> UTF-{8,16} "identity" converter, but maybe permissive */
    if (instr) {
      intptr_t _ostart, _ofinish;
      int utf16;

      if (c->kind == mzUTF8_TO_UTF16_KIND) {
	_ostart = ostart;
	_ofinish = ofinish;
	if (_ostart & 0x1)
	  _ostart++;
	_ostart >>= 1;
	if (_ofinish > 0)
	  _ofinish >>= 1;
	utf16 = 1;
      } else {
	_ostart = ostart;
	_ofinish = ofinish;
	utf16 = 0;
      }

      status = utf8_decode_x((unsigned char *)instr, istart, ifinish,
			     (unsigned int *)r, _ostart, _ofinish,
			     &amt_read, &amt_wrote,
			     1, utf16, NULL, 1, c->permissive, c->wtf);
      
      if (utf16) {
	_ostart <<= 1;
	amt_wrote <<= 1;
	if ((ostart & 0x1) && (amt_wrote > _ostart)) {
	  /* Shift down one byte: */
	  memmove(r XFORM_OK_PLUS ostart, r XFORM_OK_PLUS _ostart, amt_wrote - _ostart);
	}
      }

      amt_read -= istart;
      amt_wrote -= _ostart;
      if (status == -3) {
	/* r is not NULL; ran out of room */
	status = 1;
      } else {
	if (amt_wrote) {
	  if (!r) {
	    /* Need to allocate, then do it again: */
	    r = (char *)scheme_malloc_atomic(amt_wrote + 1);
	    utf8_decode_x((unsigned char *)instr, istart, ifinish,
			  (unsigned int *)r, ostart, _ofinish,
			  NULL, NULL,
			  1, utf16, NULL, 1, c->permissive, c->wtf);
	    r[amt_wrote] = 0;
	  }
	} else if (!r)
	  r = "";
	if (status > 0)
	  status = 0;
      }
    } else {
      r = "";
      status = 0;
      amt_read = 0;
      amt_wrote = 0;
    }
  } else {
    r = do_convert(c->cd, NULL, NULL, 0,
		   instr, istart, ifinish-istart,
		   r, ostart, ofinish-ostart,
		   !r, /* grow? */
		   0,
		   (r ? 0 : 1), /* terminator */
		   &amt_read, &amt_wrote,
		   &status);
  }

  if (status == 0) {
    /* Converted all input without error */
    status_sym = complete_symbol;
  } else if (status == 1) {
    /* Filled output, more input ready */
    status_sym = continues_symbol;
  } else if (status == -1) {
    /* Input ends in the middle of an encoding */
    status_sym = aborts_symbol;
  } else {
    /* Assert: status == -2 */
    /* Input has error (that won't be fixed by
       adding more characters */
    status_sym = error_symbol;
  }

  if (argc <= opos) {
    a[0] = scheme_make_sized_byte_string(r, amt_wrote, 0);
  } else {
    a[0] = scheme_make_integer(amt_wrote);
  }
  if (opos > 1) {
    a[1] = scheme_make_integer(amt_read);
    a[2] = status_sym;
    return scheme_values(3, a);
  } else {
    a[1] = status_sym;
    return scheme_values(2, a);
  }
}

static Scheme_Object *byte_string_convert(int argc, Scheme_Object *argv[])
{
  return convert_one("bytes-convert", 4, argc, argv);
}

static Scheme_Object *byte_string_convert_end(int argc, Scheme_Object *argv[])
{
  return convert_one("bytes-convert-end", 1, argc, argv);
}

void scheme_close_converter(Scheme_Object *conv)
{
  close_converter(conv, NULL);
}

static Scheme_Object *byte_string_close_converter(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type))
    scheme_wrong_contract("bytes-close-converter", "bytes-converter?", 0, argc, argv);

  scheme_close_converter(argv[0]);

  return scheme_void;
}

static Scheme_Object *
byte_converter_p(int argc, Scheme_Object *argv[])
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_string_converter_type)
	  ? scheme_true
	  : scheme_false);
}

/**********************************************************************/
/*                         utf8 converter                             */
/**********************************************************************/

static intptr_t utf8_decode_x(const unsigned char *s, intptr_t start, intptr_t end,
                              unsigned int *us, intptr_t dstart, intptr_t dend,
                              intptr_t *ipos, intptr_t *jpos,
                              char compact, char utf16, int *_state,
                              int might_continue, int permissive, int wtf)
     /* Results:
	non-negative => translation complete, = number of produced chars
	-1 => input ended in middle of encoding (only if might_continue)
	-2 => encoding error (only if permissive is 0)
	-3 => not enough output room

	ipos & jpos are filled with ending positions (between [d]start
	and [d]end) before return, unless they are NULL.

	compact => UTF-8 to UTF-8 or UTF-16 --- the latter if utf16;
	for utf16 and wtf, decode extended UTF-8 that allows surrogates

	_state provides initial state and is filled with ending state;
	when it's not NULL, the us must be NULL

	might_continue => allows -1 result without consuming characters

	permissive is non-zero => use permissive as value for bad byte
	sequences. When generating UTF-8, this must be an ASCII character
        or U+FFFD. */
{
  intptr_t i, j, oki;
  int failmode = -3, state;
  int init_doki;
  int nextbits, v;
  unsigned int sc;
  int pending_surrogate = 0;

  if (_state) {
    state = (*_state) & 0x7;
    init_doki = (((*_state) >> 3) & 0x7);
    nextbits = ((((*_state) >> 6) & 0xF) << 2);
    /* Need v to detect 0xD800 through 0xDFFF
       Note that we have 22 bits to work with, which is
       is enough to detect > 0x10FFFF */
    v = ((*_state) >> 10);
  } else {
    state = 0;
    init_doki = 0;
    nextbits = 0;
    v = 0;
  }

  /* In non-permissive mode, a negative result means ill-formed input.
     Permissive mode accepts anything and tries to convert it.  In
     that case, the strategy for illegal sequences is to convert
     anything bad to the given "permissive" value. */

  if (end < 0)
    end = strlen((char *)s);
  if (dend < 0)
    dend = 0x7FFFFFFF;

# define ENCFAIL i = oki; failmode = -2; break

  oki = start;
  j = dstart;
  i = start;
  if (j < dend) {
    while (i < end) {
      sc = s[i];
      if (sc < 0x80) {
	if (state) {
	  /* In a sequence, but didn't continue */
	  state = 0;
	  nextbits = 0;
	  if (permissive) {
	    v = permissive;
	    i = oki;
	    j += init_doki;
	  } else {
	    ENCFAIL;
	  }
	} else {
	  v = sc;
	}
      } else if ((sc & 0xC0) == 0x80) {
	/* Continues a sequence ... */
	if (state) {
	  /* ... and we're in one ... */
	  if (!nextbits || (sc & nextbits)) {
	    /* and we have required bits. */
	    v = (v << 6) + (sc & 0x3F);
	    nextbits = 0;
	    --state;
	    if (state) {
	      i++;
	      continue;
	    }
	    /* We finished. One last check: */
	    if ((((v >= 0xD800) && (v <= 0xDFFF))
		 || (v > 0x10FFFF))
		&& (!wtf
                    || !utf16
                    /* If WTF-16, just apply upper-limit check */
                    || (v > 0x10FFFF))) {
	      /* UTF-16 surrogates or other illegal code units */
	      if (permissive) {
		v = permissive;
		j += init_doki;
		i = oki;
	      } else {
		ENCFAIL;
	      }
	    }
	  } else {
	    /* ... but we're missing required bits. */
	    state = 0;
	    nextbits = 0;
	    if (permissive) {
	      v = permissive;
	      j += init_doki;
	      i = oki;
	    } else {
	      ENCFAIL;
	    }
	  }
	} else {
	  /* ... but we're not in one */
	  if (permissive) {
	    v = permissive;
	  } else {
	    ENCFAIL;
	  }
	}
      } else if (state) {
	/* bad: already in a sequence */
	state = 0;
	if (permissive) {
	  v = permissive;
	  i = oki;
	  j += init_doki;
	} else {
	  ENCFAIL;
	}
      } else {
	if ((sc & 0xE0) == 0xC0) {
	  if (sc & 0x1E) {
	    state = 1;
	    v = (sc & 0x1F);
	    i++;
	    continue;
	  }
	  /* else too small */
	} else if ((sc & 0xF0) == 0xE0) {
	  state = 2;
	  v = (sc & 0xF);
	  if (!v)
	    nextbits = 0x20;
	  i++;
	  continue;
	} else if ((sc & 0xF8) == 0xF0) {
	  v = (sc & 0x7);
	  if (v <= 4) {
	    state = 3;
	    if (!v)
	      nextbits = 0x30;
	    i++;
	    continue;
	  } 
	  /* Else will be larger than 0x10FFFF, so fail */
	}
	/* Too small, or 0xFF or 0xFe, or start of a 5- or 6-byte sequence */
	if (permissive) {
	  v = permissive;
	} else {
	  ENCFAIL;
	}
      }

      /* If we get here, we're supposed to output v */

      if (compact) {
	if (utf16) {
	  if (v > 0xFFFF) {
	    if (pending_surrogate) {
	      if (us)
		((unsigned short *)us)[j] = pending_surrogate;
	      j++; /* Accept previously written unpaired surrogate */
	      pending_surrogate = 0;
	    }
	    if (j + 1 >= dend)
	      break;
	    if (us) {
	      v -= 0x10000;
	      ((unsigned short *)us)[j] = 0xD800 | ((v >> 10) & 0x3FF);
	      ((unsigned short *)us)[j+1] = 0xDC00 | (v & 0x3FF);
	    }
	    j++;
	  } else if (wtf) {
	    /* We allow a surrogate by itself, but don't allow
	       a 0xDC00 after a 0xD800, otherwise multiple encodings can
	       map to the same thing. */
	    if ((v >= 0xD800) && (v <= 0xDFFF)) {
	      if (pending_surrogate && ((v & 0xDC00) == 0xDC00)) {
		/* This looks like a surrogate pair, so disallow it. */
		if (permissive) {
		  /* We need to fill in 6 permissive substitutions,
		     one for each input byte. If we can't put all 6,
		     then don't use any input. */
		  if (j + 5 >= dend) {
		    break;
		  } else {
		    int p;
		    if (us) {
		      for (p = 0; p < 5; p++) {
			if (j + p >= dend)
			  break;
			((unsigned short *)us)[j+p] = permissive;
		      }
		    }
		    j += 5;
		    v = permissive;
		  }
		} else {
		  ENCFAIL;
		}
		pending_surrogate = 0;
	      } else {
		if (pending_surrogate) {
		  if (us)
		    ((unsigned short *)us)[j] = pending_surrogate;
		  j++; /* Accept previousy written unpaired surrogate */
		  pending_surrogate = 0;
		  if (j >= dend)
		    break;
		}
		if ((v & 0xDC00) == 0xD800)
		  pending_surrogate = v;
		else
		  pending_surrogate = 0;
	      }
	    } else {
	      if (pending_surrogate) {
		if (us)
		  ((unsigned short *)us)[j] = pending_surrogate;
		j++; /* Accept previousy written unpaired surrogate */
		pending_surrogate = 0;
		if (j >= dend)
		  break;
	      }
	    }

	    if (pending_surrogate)
	      --j; /* don't accept unpaired surrogate, yet */
	    else if (us)
	      ((unsigned short *)us)[j] = v;
          } else {
	    if (us)
	      ((unsigned short *)us)[j] = v;
	  }
	} else {
	  intptr_t delta;
	  delta = (i - oki);
	  if (delta) {
	    if (j + delta + 1 < dend) {
	      if (us)
		memcpy(((char *)us) + j, s + oki, delta + 1);
	      j += delta;
	    } else
	      break;
	  } else if (v == 0xFFFD) {
            if (j + 3 < dend) {
              if (us) {
                ((unsigned char *)us)[j] = 0xEF;
                ((unsigned char *)us)[j+1] = 0xBF;
                ((unsigned char *)us)[j+2] = 0xBD;
              }
              j += 2;
            } else
              break;
          } else if (us) {
            ((unsigned char *)us)[j] = v;
          }
	}
      } else if (us) {
	us[j] = v;
      }
      j++;
      i++;
      oki = i;
      init_doki = 0;
      if (j >= dend)
	break;
    }
  }

  if (_state) {
    if (!state)
      *_state = 0;
    else
      *_state = (state 
		 | (((end - oki) + init_doki) << 3)
		 | ((nextbits >> 2) << 6)
		 | (v << 10));
  } else if (state) {
    if (might_continue || !permissive) {
      failmode = -1;
      i = end - 1; /* to ensure that failmode is returned */
    } else if (permissive) {
      if (pending_surrogate) {
        /* Unpaired surrogate before permissive replacements */
        if (utf16 && (j < dend)) {
          if (us)
            ((unsigned short *)us)[j] = pending_surrogate;
          j++;
        }
        pending_surrogate = 0;
      }
      for (i = oki; i < end; i++) {
	if (j < dend) {
	  if (us) {
	    if (compact) {
	      if (utf16)
		((unsigned short *)us)[j] = permissive;
	      else
		((unsigned char *)us)[j] = permissive;
	    } else
	      us[j] = permissive;
	  }
	  j++;
	} else
	  break;
      }
      oki = i;
    }
  }

  if (pending_surrogate) {
    if (!might_continue) {
      /* Accept unpaired surrogate at end of input */
      if (j < dend) {
        if (us)
          ((unsigned short *)us)[j] = pending_surrogate;
        j++;
      }
    } else {
      oki -= 3;
    }
  }

  if (ipos)
    *ipos = oki;
  if (jpos)
    *jpos = j;

  if (i < end)
    return failmode;

  if (pending_surrogate) {
    /* input must have ended right after surrogate */
    return -1;
  }

  return j - dstart;
}

intptr_t scheme_utf8_decode(const unsigned char *s, intptr_t start, intptr_t end,
                            unsigned int *us, intptr_t dstart, intptr_t dend,
                            intptr_t *ipos, char utf16, int permissive)
{
  return utf8_decode_x(s, start, end, us, dstart, dend,
		       ipos, NULL, utf16, utf16, NULL, 0, permissive, WIN_UTF16_AS_WTF16(utf16));
}

intptr_t scheme_utf8_decode_offset_prefix(const unsigned char *s, intptr_t start, intptr_t end,
                                          unsigned int *us, intptr_t dstart, intptr_t dend,
                                          intptr_t *ipos, char utf16, int permissive)
{
  return utf8_decode_x(s, start, end, us, dstart, dend,
		       ipos, NULL, utf16, utf16, NULL, 1, permissive, WIN_UTF16_AS_WTF16(utf16));
}

intptr_t scheme_utf8_decode_as_prefix(const unsigned char *s, intptr_t start, intptr_t end,
                                      unsigned int *us, intptr_t dstart, intptr_t dend,
                                      intptr_t *ipos, char utf16, int permissive)
     /* Always returns number of read characters, not error codes. */
{
  intptr_t opos;
  utf8_decode_x(s, start, end, us, dstart, dend,
		ipos, &opos, utf16, utf16, NULL, 1, permissive, WIN_UTF16_AS_WTF16(utf16));
  return opos - dstart;
}

intptr_t scheme_utf8_decode_all(const unsigned char *s, intptr_t len, unsigned int *us, int permissive)
{
  return utf8_decode_x(s, 0, len, us, 0, -1, NULL, NULL, 0, 0, NULL, 0, permissive, 0);
}

intptr_t scheme_utf8_decode_prefix(const unsigned char *s, intptr_t len, unsigned int *us, int permissive)
     /* us != NULL */
{
  {
    /* Try fast path (all ASCII) */
    intptr_t i;
    for (i = 0; i < len; i++) {
      if (s[i] < 128)
	us[i] = s[i];
      else
	break;
    }
    if (i == len)
      return len;
  }

  return utf8_decode_x(s, 0, len, us, 0, -1, NULL, NULL, 0, 0, NULL, 1, permissive, 0);
}

mzchar *scheme_utf8_decode_to_buffer_len(const unsigned char *s, intptr_t len,
					 mzchar *buf, intptr_t blen, intptr_t *_ulen)
{
  intptr_t ulen;

  ulen = utf8_decode_x(s, 0, len, NULL, 0, -1,
		       NULL, NULL, 0, 0,
		       NULL, 0, 0, 0);
  if (ulen < 0)
    return NULL;
  if (ulen + 1 > blen) {
    buf = (mzchar *)scheme_malloc_atomic((ulen + 1) * sizeof(mzchar));
  }
  utf8_decode_x(s, 0, len, buf, 0, -1,
		NULL, NULL, 0, 0,
		NULL, 0, 0, 0);
  buf[ulen] = 0;
  *_ulen = ulen;
  return buf;
}

mzchar *scheme_utf8_decode_to_buffer(const unsigned char *s, intptr_t len,
				     mzchar *buf, intptr_t blen)
{
  intptr_t ulen;
  return scheme_utf8_decode_to_buffer_len(s, len, buf, blen, &ulen);
}

intptr_t scheme_utf8_decode_count(const unsigned char *s, intptr_t start, intptr_t end,
			     int *_state, int might_continue, int permissive)
{
  intptr_t pos = 0;

  if (!_state || !*_state) {
    /* Try fast path (all ASCII): */
    intptr_t i;
    for (i = start; i < end; i++) {
      if (s[i] > 127)
	break;
    }
    if (i == end)
      return end - start;
  }

  utf8_decode_x(s, start, end,
		NULL, 0, -1,
		NULL, &pos,
		0, 0, _state,
		might_continue, permissive, 0);

  return pos;
}

static intptr_t utf8_encode_x(const unsigned int *us, intptr_t start, intptr_t end,
                              unsigned char *s, intptr_t dstart, intptr_t dend,
                              intptr_t *_ipos, intptr_t *_opos, char utf16, int wtf)
  /* Results:
        -1 => input ended in the middle of an encoding - only when utf16 and _opos
	non-negative => reports number of bytes/code-units produced */
{
  intptr_t i, j, done = start;

  if (dend < 0)
    dend = 0x7FFFFFFF;

  if (!s) {
    unsigned int wc;
    j = 0;
    for (i = start; i < end; i++) {
      if (utf16) {
	wc = ((unsigned short *)us)[i];
	if ((wc & 0xF800) == 0xD800) {
	  /* Unparse surrogates. We assume that the surrogates are
	     well formed, unless this is Windows or if we're at the
             end and _opos is 0. The well-formedness assumption was
             probably not a good idea, but note that it's explicitly
             documented to behave that way. */
# define UNPAIRED_MASK(wtf) (wtf ? 0xFC00 : 0xF800)
	  if (((i + 1) == end) && ((wc & UNPAIRED_MASK(wtf)) == 0xD800) && _opos) {
	    /* Ended in the middle of a surrogate pair */
	    *_opos = j;
	    if (_ipos)
	      *_ipos = i;
	    return -1;
	  }
	  if (wtf && ((wc & 0xFC00) != 0xD800)) {
	    /* Count as one */
	  } else if (wtf && ((i + 1 >= end)
                             || (((((unsigned short *)us)[i+1]) & 0xFC00) != 0xDC00))) {
	  } else {
            i++;
            wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)us)[i]) & 0x3FF);
            wc += 0x10000;
          }
	}
      } else {
	wc = us[i];
      }
      if (wc < 0x80) {
	j += 1;
      } else if (wc < 0x800) {
	j += 2;
      } else if (wc < 0x10000) {
	j += 3;
      } else if (wc < 0x200000) {
	j += 4;
      } else if (wc < 0x4000000) {
	j += 5;
      } else {
	j += 6;
      }
    }
    if (_ipos)
      *_ipos = i;
    if (_opos)
      *_opos = j + dstart;
    return j;
  } else {
    unsigned int wc;
    j = dstart;
    for (i = start; i < end; i++) {
      if (utf16) {
	wc = ((unsigned short *)us)[i];
	if ((wc & 0xF800) == 0xD800) {
	  /* Unparse surrogates. We assume that the surrogates are
	     well formed on non-Windows platforms, but when _opos,
	     we detect ending in the middle of an surrogate pair. */
	  if (((i + 1) == end) && ((wc & UNPAIRED_MASK(wtf)) == 0xD800) && _opos) {
	    /* Ended in the middle of a surrogate pair */
	    *_opos = j;
	    if (_ipos)
	      *_ipos = i;
	    return -1;
	  }
	  if (wtf && ((wc & 0xFC00) != 0xD800)) {
	    /* Let the misplaced surrogate through */
	  } else if (wtf && ((i + 1 >= end)
                             || (((((unsigned short *)us)[i+1]) & 0xFC00) != 0xDC00))) {
	    /* Let the misplaced surrogate through */
	  } else {
            i++;
            wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)us)[i]) & 0x3FF);
            wc += 0x10000;
          }
	}
      } else {
	wc = us[i];
      }

      if (wc < 0x80) {
	if (j + 1 > dend)
	  break;
	s[j++] = wc;
      } else if (wc < 0x800) {
	if (j + 2 > dend)
	  break;
	s[j++] = 0xC0 | ((wc & 0x7C0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x10000) {
	if (j + 3 > dend)
	  break;
	s[j++] = 0xE0 | ((wc & 0xF000) >> 12);
	s[j++] = 0x80 | ((wc & 0x0FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x200000) {
	if (j + 4 > dend)
	  break;
	s[j++] = 0xF0 | ((wc & 0x1C0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x03F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else if (wc < 0x4000000) {
	if (j + 5 > dend)
	  break;
	s[j++] = 0xF8 | ((wc & 0x3000000) >> 24);
	s[j++] = 0x80 | ((wc & 0x0FC0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x003F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x0000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      } else {
	if (j + 6 > dend)
	  break;
	s[j++] = 0xFC | ((wc & 0x40000000) >> 30);
	s[j++] = 0x80 | ((wc & 0x3F000000) >> 24);
	s[j++] = 0x80 | ((wc & 0x00FC0000) >> 18);
	s[j++] = 0x80 | ((wc & 0x0003F000) >> 12);
	s[j++] = 0x80 | ((wc & 0x00000FC0) >> 6);
	s[j++] = 0x80 | (wc & 0x3F);
      }
      done = i;
    }
    if (_ipos)
      *_ipos = done;
    if (_opos)
      *_opos = j;
    return j - dstart;
  }
}

intptr_t scheme_utf8_encode(const unsigned int *us, intptr_t start, intptr_t end,
		       unsigned char *s, intptr_t dstart,
		       char utf16)
{
  return utf8_encode_x(us, start, end,
		       s, dstart, -1,
		       NULL, NULL, utf16, WIN_UTF16_AS_WTF16(utf16));
}

intptr_t scheme_utf8_encode_all(const unsigned int *us, intptr_t len, unsigned char *s)
{
  return utf8_encode_x(us, 0, len, s, 0, -1, NULL, NULL, 0 /* utf16 */, 0);
}

char *scheme_utf8_encode_to_buffer_len(const mzchar *s, intptr_t len,
				       char *buf, intptr_t blen,
				       intptr_t *_slen)
{
  intptr_t slen;

  /* ASCII with len < blen is a common case: */
  if (len < blen) {
    for (slen = 0; slen < len; slen++) {
      if (s[slen] > 127)
        break;
      else
        buf[slen] = s[slen];
    }
    if (slen == len) {
      buf[slen] = 0;
      *_slen = slen;
      return buf;
    }
  }

  slen = utf8_encode_x(s, 0, len, NULL, 0, -1, NULL, NULL, 0, 0);
  if (slen + 1 > blen) {
    buf = (char *)scheme_malloc_atomic(slen + 1);
  }
  utf8_encode_x(s, 0, len, (unsigned char *)buf, 0, -1, NULL, NULL, 0, 0);
  buf[slen] = 0;
  *_slen = slen;
  return buf;
}

char *scheme_utf8_encode_to_buffer(const mzchar *s, intptr_t len,
				   char *buf, intptr_t blen)
{
  intptr_t slen;
  return scheme_utf8_encode_to_buffer_len(s, len, buf, blen, &slen);
}

unsigned short *scheme_ucs4_to_utf16(const mzchar *text, intptr_t start, intptr_t end,
				     unsigned short *buf, intptr_t bufsize,
				     intptr_t *ulen, intptr_t term_size)
{
  mzchar v;
  intptr_t extra, i, j;
  unsigned short *utf16;

  /* Count characters that fall outside UCS-2: */
  for (i = start, extra = 0; i < end; i++) {
    if (text[i] > 0xFFFF)
      extra++;
  }

  if ((end - start) + extra + term_size < bufsize)
    utf16 = buf;
  else
    utf16 = (unsigned short *)scheme_malloc_atomic(sizeof(unsigned short) * ((end - start) + extra + term_size));

  for (i = start, j = 0; i < end; i++) {
    v = text[i];
    if (v > 0xFFFF) {
      v -= 0x10000;
      utf16[j++] = 0xD800 | ((v >> 10) & 0x3FF);
      utf16[j++] = 0xDC00 | (v & 0x3FF);
    } else
      utf16[j++] = v;
  }

  *ulen = j;

  return utf16;
}

mzchar *scheme_utf16_to_ucs4(const unsigned short *text, intptr_t start, intptr_t end,
			     mzchar *buf, intptr_t bufsize,
			     intptr_t *ulen, intptr_t term_size)
{
  int wc;
  intptr_t i, j;

  for (i = start, j = 0; i < end; i++) {
    wc = text[i];
    if ((wc & 0xF800) == 0xD800) {
      i++;
    }
    j++;
  }

  if (j + term_size >= bufsize)
    buf = (mzchar *)scheme_malloc_atomic((j + term_size) * sizeof(mzchar));

  for (i = start, j = 0; i < end; i++) {
    wc = text[i];
    if ((wc & 0xF800) == 0xD800) {
      i++;
      wc = ((wc & 0x3FF) << 10) + ((((unsigned short *)text)[i]) & 0x3FF);
      wc += 0x10000;
    }
    buf[j++] = wc;
  }

  *ulen = j;

  return buf;
}

/**********************************************************************/
/*                     machine type details                           */
/**********************************************************************/

/*************************** Windows **********************************/

#ifdef DOS_FILE_SYSTEM
# include <windows.h>
void machine_details(char *buff)
{
  OSVERSIONINFO info;
  BOOL hasInfo;
  char *p;

  info.dwOSVersionInfoSize = sizeof(info);

  GetVersionEx(&info);

  hasInfo = FALSE;

  p = info.szCSDVersion;

  while (p < info.szCSDVersion + sizeof(info.szCSDVersion) &&
	 *p) {
    if (*p != ' ') {
      hasInfo = TRUE;
      break;
    }
    p = p XFORM_OK_PLUS 1;
  }

  sprintf(buff,"Windows %s %ld.%ld (Build %ld)%s%s",
	  (info.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS) ?
	  "9x" :
	  (info.dwPlatformId == VER_PLATFORM_WIN32_NT) ?
	  "NT" : "Unknown platform",
	  info.dwMajorVersion,info.dwMinorVersion,
	  (info.dwPlatformId == VER_PLATFORM_WIN32_NT) ?
	  info.dwBuildNumber :
	  info.dwBuildNumber & 0xFFFF,
	  hasInfo ? " " : "",hasInfo ? info.szCSDVersion : "");
}
#endif

/***************************** Unix ***********************************/

#if !defined(DOS_FILE_SYSTEM)
READ_ONLY static char *uname_locations[] = { "/bin/uname",
				   "/usr/bin/uname",
				   /* The above should cover everything, but
				      just in case... */
				   "/sbin/uname",
				   "/usr/sbin/uname",
				   "/usr/local/bin/uname",
				   "/usr/local/uname",
				   NULL };

static int try_subproc(Scheme_Object *subprocess_proc, char *prog)
{
  Scheme_Object *a[5];
  mz_jmp_buf * volatile savebuf, newbuf;

  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;

  if (!scheme_setjmp(newbuf)) {
    a[0] = scheme_false;
    a[1] = scheme_false;
    a[2] = scheme_false;
    a[3] = scheme_make_locale_string(prog);
    a[4] = scheme_make_locale_string("-a");
    _scheme_apply_multi(subprocess_proc, 5, a);
    scheme_current_thread->error_buf = savebuf;
    return 1;
  } else {
    scheme_clear_escape();
    scheme_current_thread->error_buf = savebuf;
    return 0;
  }
}

void machine_details(char *buff)
{
  Scheme_Object *subprocess_proc;
  int i;
  Scheme_Config *config;
  Scheme_Security_Guard *sg;
  Scheme_Cont_Frame_Data cframe;
 
  /* Use the root security guard so we can test for and run
     executables. */
  config = scheme_current_config();
  sg = (Scheme_Security_Guard *)scheme_get_param(config, MZCONFIG_SECURITY_GUARD);
  while (sg->parent) { sg = sg->parent; }
  config = scheme_extend_config(config, MZCONFIG_SECURITY_GUARD, (Scheme_Object *)sg);

  scheme_push_continuation_frame(&cframe);
  scheme_install_config(config);

  subprocess_proc = scheme_builtin_value("subprocess");

  for (i = 0; uname_locations[i]; i++) {
    if (scheme_file_exists(uname_locations[i])) {
      /* Try running it. */
      if (try_subproc(subprocess_proc, uname_locations[i])) {
	Scheme_Object *sout, *sin, *serr;
	intptr_t c;

	sout = scheme_current_thread->ku.multiple.array[1];
	sin = scheme_current_thread->ku.multiple.array[2];
	serr = scheme_current_thread->ku.multiple.array[3];

	scheme_close_output_port(sin);
	scheme_close_input_port(serr);

	/* Read result: */
	strcpy(buff, "<unknown machine>");
	c = scheme_get_bytes(sout, 1023, buff, 0);
	buff[c] = 0;

	scheme_close_input_port(sout);

	/* Remove trailing whitespace (especially newlines) */
	while (c && portable_isspace(((unsigned char *)buff)[c - 1])) {
	  buff[--c] = 0;
	}

        scheme_pop_continuation_frame(&cframe);

	return;
      }
    }
  }

  strcpy(buff, "<unknown machine>");

  scheme_pop_continuation_frame(&cframe);
}
#endif


/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_string.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_string_converter_type, mark_string_convert);
}

END_XFORM_SKIP;

#endif
