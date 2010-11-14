/* DO NOT EDIT THIS FILE. */
/* This file was generated by xctocc from "wxs_chce.xc". */


#if defined(__GNUC__)
# define MAYBE_UNUSED __attribute__((unused))
#pragma GCC diagnostic ignored "-Wwrite-strings"
#else
# define MAYBE_UNUSED
#endif

#if defined(_MSC_VER)
# include "wx.h"
#endif
#if defined(OS_X)
# include "common.h"
#endif

#include "wx_choic.h"

#ifndef wx_msw
void wxSetComboBoxFont(wxFont *f)
{
}
#endif




#ifdef wx_x
# define BM_SELECTED(map) ((map)->selectedTo)
#endif
#if defined(wx_mac) || defined(wx_msw)
# define BM_SELECTED(map) ((map)->selectedInto)
#endif
# define BM_IN_USE(map) ((map)->selectedIntoDC)




#define ESCAPE_NO_RET_VAL /*empty*/


#include "wxscheme.h"
#include "wxs_chce.h"

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static Scheme_Object *choiceStyle_wxVERTICAL_LABEL_sym = NULL;
static Scheme_Object *choiceStyle_wxHORIZONTAL_LABEL_sym = NULL;
static Scheme_Object *choiceStyle_wxINVISIBLE_sym = NULL;

static void init_symset_choiceStyle(void) {
  REMEMBER_VAR_STACK();
  wxREGGLOB(choiceStyle_wxVERTICAL_LABEL_sym);
  choiceStyle_wxVERTICAL_LABEL_sym = WITH_REMEMBERED_STACK(scheme_intern_symbol("vertical-label"));
  wxREGGLOB(choiceStyle_wxHORIZONTAL_LABEL_sym);
  choiceStyle_wxHORIZONTAL_LABEL_sym = WITH_REMEMBERED_STACK(scheme_intern_symbol("horizontal-label"));
  wxREGGLOB(choiceStyle_wxINVISIBLE_sym);
  choiceStyle_wxINVISIBLE_sym = WITH_REMEMBERED_STACK(scheme_intern_symbol("deleted"));
}

MAYBE_UNUSED static int unbundle_symset_choiceStyle(Scheme_Object *v, const char *where) {
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, v);
  if (!choiceStyle_wxINVISIBLE_sym) WITH_VAR_STACK(init_symset_choiceStyle());
  Scheme_Object *i INIT_NULLED_OUT, *l = v;
  long result = 0;
  while (SCHEME_PAIRP(l)) {
  i = SCHEME_CAR(l);
  if (0) { }
  else if (i == choiceStyle_wxVERTICAL_LABEL_sym) { result = result | wxVERTICAL_LABEL; }
  else if (i == choiceStyle_wxHORIZONTAL_LABEL_sym) { result = result | wxHORIZONTAL_LABEL; }
  else if (i == choiceStyle_wxINVISIBLE_sym) { result = result | wxINVISIBLE; }
  else { break; } 
  l = SCHEME_CDR(l);
  }
  if (SCHEME_NULLP(l)) { READY_TO_RETURN; return result; }
  if (where) WITH_VAR_STACK(scheme_wrong_type(where, "choiceStyle symbol list", -1, 0, &v));
  READY_TO_RETURN;
  return 0;
}

MAYBE_UNUSED static int istype_symset_choiceStyle(Scheme_Object *v, const char *where) {
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, v);
  if (!choiceStyle_wxINVISIBLE_sym) WITH_VAR_STACK(init_symset_choiceStyle());
  Scheme_Object *i INIT_NULLED_OUT, *l = v;
  long result = 1;
  while (SCHEME_PAIRP(l)) {
  i = SCHEME_CAR(l);
  if (0) { }
  else if (i == choiceStyle_wxVERTICAL_LABEL_sym) { ; }
  else if (i == choiceStyle_wxHORIZONTAL_LABEL_sym) { ; }
  else if (i == choiceStyle_wxINVISIBLE_sym) { ; }
  else { break; } 
  l = SCHEME_CDR(l);
  }
  if (SCHEME_NULLP(l)) { READY_TO_RETURN; return result; }
  if (where) WITH_VAR_STACK(scheme_wrong_type(where, "choiceStyle symbol list", -1, 0, &v));
  READY_TO_RETURN;
  return 0;
}





#define CB_FUNCTYPE wxFunction 


#undef CALLBACKCLASS
#undef CB_REALCLASS
#undef CB_UNBUNDLE
#undef CB_USER

#define CALLBACKCLASS os_wxChoice
#define CB_REALCLASS wxChoice
#define CB_UNBUNDLE objscheme_unbundle_wxChoice
#define CB_USER METHODNAME("choice%","initialization")

#undef CB_TOSCHEME
#undef CB_TOC
#define CB_TOSCHEME wxChoiceCallbackToScheme
#define CB_TOC wxChoiceCallbackToC


class CALLBACKCLASS;





extern wxCommandEvent *objscheme_unbundle_wxCommandEvent(Scheme_Object *,const char *,int);
extern Scheme_Object *objscheme_bundle_wxCommandEvent(wxCommandEvent *);

static void CB_TOSCHEME(CB_REALCLASS *obj, wxCommandEvent *event);

#undef l_ADDRESS
#undef l_DEREF
#undef l_TEST
#undef l_POINT
#undef l_TYPE
#undef l_LIST_ITEM_BUNDLE
#undef l_LIST_ITEM_UNBUNDLE
#undef l_MAKE_LIST
#undef l_MAKE_ARRAY
#undef l_EXTRA
#undef l_TERMINATE
#undef l_COPY
#undef l_OKTEST
#undef l_INTTYPE

#define l_ADDRESS 
#define l_DEREF 
#define l_NEWATOMIC (UseGC)
#define l_NULLOK 0
#define l_TEST 
#define l_POINT 
#define l_EXTRA 0
#define l_TERMINATE 
#define l_COPY l_COPYDEST=l_COPYSRC;
#define l_OKTEST 
#define l_INTTYPE int
#define l_DIRECTMALLOC 0

#define l_TYPE string
#define l_LIST_ITEM_BUNDLE objscheme_bundle_string
#define l_LIST_ITEM_UNBUNDLE objscheme_unbundle_string
#define l_MAKE_LIST __MakestringList
#define l_MAKE_ARRAY __MakestringArray





MAYBE_UNUSED static Scheme_Object *l_MAKE_LIST(l_TYPE l_POINT *f, l_INTTYPE c)
{
  Scheme_Object *cdr = scheme_null, *obj = NULL;
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, cdr);

  while (c--) {
    obj = WITH_VAR_STACK(l_LIST_ITEM_BUNDLE(l_ADDRESS f[c]));
    cdr = WITH_VAR_STACK(scheme_make_pair(obj, cdr));
  }
  
  READY_TO_RETURN;

  return cdr;
}

MAYBE_UNUSED static l_TYPE l_POINT *l_MAKE_ARRAY(Scheme_Object *l, l_INTTYPE *c, char *who)
{
  Scheme_Object *orig_l = l;
  int i = 0;
  long len;
  l_TYPE l_POINT *f = NULL;

  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, l);
  VAR_STACK_PUSH(1, orig_l);
  VAR_STACK_PUSH(2, f);

  len = WITH_VAR_STACK(scheme_proper_list_length(l));
  if (len < 0) WITH_VAR_STACK(scheme_wrong_type(who, "proper-list", -1, 0, &l));
  if (c) *c = len;

  if (!(len + l_EXTRA)) {
    READY_TO_RETURN;
    return NULL;
  }

#if l_DIRECTMALLOC
  f = (l_TYPE l_POINT *)WITH_VAR_STACK(GC_malloc_atomic(sizeof(l_TYPE l_POINT) * (len + l_EXTRA)));
#else
  f = WITH_VAR_STACK(new l_NEWATOMIC l_TYPE l_POINT[len + l_EXTRA]);
#endif

  while (!SCHEME_NULLP(l)) {
    if (!SCHEME_LISTP(l)) {
      WITH_VAR_STACK(scheme_arg_mismatch(who, "expected a proper list: ", orig_l));
      READY_TO_RETURN;
      return NULL;
    }

#define l_COPYDEST f[i]
#define l_COPYSRC (l_DEREF WITH_VAR_STACK(l_LIST_ITEM_UNBUNDLE(SCHEME_CAR(l), who l_TEST)))

    l_COPY

    l_OKTEST

    i++;

    l = SCHEME_CDR(l);
  }
  l_TERMINATE

  READY_TO_RETURN;

  return f;
}


#define RANGECLASS wxChoice

#define THISOBJECT ((RANGECLASS *)((Scheme_Class_Object *)THEOBJ)->primdata)











class os_wxChoice : public wxChoice {
 public:
  Scheme_Object *callback_closure;

  os_wxChoice CONSTRUCTOR_ARGS((class wxPanel* x0, wxFunction x1, nstring x2, int x3 = -1, int x4 = -1, int x5 = -1, int x6 = -1, int x7 = 0, string* x8 = NULL, int x9 = 0, class wxFont* x10 = NULL, string x11 = "checkBox"));
  ~os_wxChoice();
  void OnDropFile(epathname x0);
  Bool PreOnEvent(class wxWindow* x0, class wxMouseEvent* x1);
  Bool PreOnChar(class wxWindow* x0, class wxKeyEvent* x1);
  void OnSize(int x0, int x1);
  void OnSetFocus();
  void OnKillFocus();
#ifdef MZ_PRECISE_GC
  void gcMark();
  void gcFixup();
#endif
};

#ifdef MZ_PRECISE_GC
void os_wxChoice::gcMark() {
  wxChoice::gcMark();
  gcMARK_TYPED(Scheme_Object *, callback_closure);
}
void os_wxChoice::gcFixup() {
  wxChoice::gcFixup();
  gcFIXUP_TYPED(Scheme_Object *, callback_closure);
}
#endif

static Scheme_Object *os_wxChoice_class;

os_wxChoice::os_wxChoice CONSTRUCTOR_ARGS((class wxPanel* x0, wxFunction x1, nstring x2, int x3, int x4, int x5, int x6, int x7, string* x8, int x9, class wxFont* x10, string x11))
CONSTRUCTOR_INIT(: wxChoice(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11))
{
}

os_wxChoice::~os_wxChoice()
{
    objscheme_destroy(this, (Scheme_Object *) __gc_external);
}

static Scheme_Object *os_wxChoiceOnDropFile(int n, Scheme_Object *p[]);

void os_wxChoice::OnDropFile(epathname x0)
{
  Scheme_Object *p[POFFSET+1] INIT_NULLED_ARRAY({ NULLED_OUT INA_comma NULLED_OUT });
  Scheme_Object *v;
  Scheme_Object *method INIT_NULLED_OUT;
#ifdef MZ_PRECISE_GC
  os_wxChoice *sElF = this;
#endif
  static void *mcache = 0;

  SETUP_VAR_STACK(6);
  VAR_STACK_PUSH(0, method);
  VAR_STACK_PUSH(1, sElF);
  VAR_STACK_PUSH_ARRAY(2, p, POFFSET+1);
  VAR_STACK_PUSH(5, x0);
  SET_VAR_STACK();

  method = objscheme_find_method((Scheme_Object *) ASSELF __gc_external, os_wxChoice_class, "on-drop-file", &mcache);
  if (!method || OBJSCHEME_PRIM_METHOD(method, os_wxChoiceOnDropFile)) {
    SET_VAR_STACK();
    READY_TO_RETURN; ASSELF wxChoice::OnDropFile(x0);
  } else {
  mz_jmp_buf *savebuf, newbuf; Scheme_Thread *thread;
  p[POFFSET+0] = WITH_VAR_STACK(objscheme_bundle_pathname((char *)x0));
  ESCAPE_BLOCK(ESCAPE_NO_RET_VAL)
  p[0] = (Scheme_Object *) ASSELF __gc_external;

  v = WITH_VAR_STACK(scheme_apply(method, POFFSET+1, p));
  { thread = scheme_get_current_thread(); thread->error_buf = savebuf; }
  
     READY_TO_RETURN;
  }
}

static Scheme_Object *os_wxChoicePreOnEvent(int n, Scheme_Object *p[]);

Bool os_wxChoice::PreOnEvent(class wxWindow* x0, class wxMouseEvent* x1)
{
  Scheme_Object *p[POFFSET+2] INIT_NULLED_ARRAY({ NULLED_OUT INA_comma NULLED_OUT INA_comma NULLED_OUT });
  Scheme_Object *v;
  Scheme_Object *method INIT_NULLED_OUT;
#ifdef MZ_PRECISE_GC
  os_wxChoice *sElF = this;
#endif
  static void *mcache = 0;

  SETUP_VAR_STACK(7);
  VAR_STACK_PUSH(0, method);
  VAR_STACK_PUSH(1, sElF);
  VAR_STACK_PUSH_ARRAY(2, p, POFFSET+2);
  VAR_STACK_PUSH(5, x0);
  VAR_STACK_PUSH(6, x1);
  SET_VAR_STACK();

  method = objscheme_find_method((Scheme_Object *) ASSELF __gc_external, os_wxChoice_class, "pre-on-event", &mcache);
  if (!method || OBJSCHEME_PRIM_METHOD(method, os_wxChoicePreOnEvent)) {
    SET_VAR_STACK();
    return FALSE;
  } else {
  mz_jmp_buf *savebuf, newbuf; Scheme_Thread *thread;
  p[POFFSET+0] = WITH_VAR_STACK(objscheme_bundle_wxWindow(x0));
  p[POFFSET+1] = WITH_VAR_STACK(objscheme_bundle_wxMouseEvent(x1));
  ESCAPE_BLOCK(1)
  p[0] = (Scheme_Object *) ASSELF __gc_external;

  v = WITH_VAR_STACK(scheme_apply(method, POFFSET+2, p));
  { thread = scheme_get_current_thread(); thread->error_buf = savebuf; }
  
  {
     Bool resval;
     resval = WITH_VAR_STACK(objscheme_unbundle_bool(v, "pre-on-event in choice%"", extracting return value"));
     READY_TO_RETURN;
     return resval;
  }
  }
}

static Scheme_Object *os_wxChoicePreOnChar(int n, Scheme_Object *p[]);

Bool os_wxChoice::PreOnChar(class wxWindow* x0, class wxKeyEvent* x1)
{
  Scheme_Object *p[POFFSET+2] INIT_NULLED_ARRAY({ NULLED_OUT INA_comma NULLED_OUT INA_comma NULLED_OUT });
  Scheme_Object *v;
  Scheme_Object *method INIT_NULLED_OUT;
#ifdef MZ_PRECISE_GC
  os_wxChoice *sElF = this;
#endif
  static void *mcache = 0;

  SETUP_VAR_STACK(7);
  VAR_STACK_PUSH(0, method);
  VAR_STACK_PUSH(1, sElF);
  VAR_STACK_PUSH_ARRAY(2, p, POFFSET+2);
  VAR_STACK_PUSH(5, x0);
  VAR_STACK_PUSH(6, x1);
  SET_VAR_STACK();

  method = objscheme_find_method((Scheme_Object *) ASSELF __gc_external, os_wxChoice_class, "pre-on-char", &mcache);
  if (!method || OBJSCHEME_PRIM_METHOD(method, os_wxChoicePreOnChar)) {
    SET_VAR_STACK();
    return FALSE;
  } else {
  mz_jmp_buf *savebuf, newbuf; Scheme_Thread *thread;
  p[POFFSET+0] = WITH_VAR_STACK(objscheme_bundle_wxWindow(x0));
  p[POFFSET+1] = WITH_VAR_STACK(objscheme_bundle_wxKeyEvent(x1));
  ESCAPE_BLOCK(1)
  p[0] = (Scheme_Object *) ASSELF __gc_external;

  v = WITH_VAR_STACK(scheme_apply(method, POFFSET+2, p));
  { thread = scheme_get_current_thread(); thread->error_buf = savebuf; }
  
  {
     Bool resval;
     resval = WITH_VAR_STACK(objscheme_unbundle_bool(v, "pre-on-char in choice%"", extracting return value"));
     READY_TO_RETURN;
     return resval;
  }
  }
}

static Scheme_Object *os_wxChoiceOnSize(int n, Scheme_Object *p[]);

void os_wxChoice::OnSize(int x0, int x1)
{
  Scheme_Object *p[POFFSET+2] INIT_NULLED_ARRAY({ NULLED_OUT INA_comma NULLED_OUT INA_comma NULLED_OUT });
  Scheme_Object *v;
  Scheme_Object *method INIT_NULLED_OUT;
#ifdef MZ_PRECISE_GC
  os_wxChoice *sElF = this;
#endif
  static void *mcache = 0;

  SETUP_VAR_STACK(5);
  VAR_STACK_PUSH(0, method);
  VAR_STACK_PUSH(1, sElF);
  VAR_STACK_PUSH_ARRAY(2, p, POFFSET+2);
  SET_VAR_STACK();

  method = objscheme_find_method((Scheme_Object *) ASSELF __gc_external, os_wxChoice_class, "on-size", &mcache);
  if (!method || OBJSCHEME_PRIM_METHOD(method, os_wxChoiceOnSize)) {
    SET_VAR_STACK();
    READY_TO_RETURN; ASSELF wxChoice::OnSize(x0, x1);
  } else {
  
  p[POFFSET+0] = scheme_make_integer(x0);
  p[POFFSET+1] = scheme_make_integer(x1);
  
  p[0] = (Scheme_Object *) ASSELF __gc_external;

  v = WITH_VAR_STACK(scheme_apply(method, POFFSET+2, p));
  
  
     READY_TO_RETURN;
  }
}

static Scheme_Object *os_wxChoiceOnSetFocus(int n, Scheme_Object *p[]);

void os_wxChoice::OnSetFocus()
{
  Scheme_Object *p[POFFSET+0] INIT_NULLED_ARRAY({ NULLED_OUT });
  Scheme_Object *v;
  Scheme_Object *method INIT_NULLED_OUT;
#ifdef MZ_PRECISE_GC
  os_wxChoice *sElF = this;
#endif
  static void *mcache = 0;

  SETUP_VAR_STACK(5);
  VAR_STACK_PUSH(0, method);
  VAR_STACK_PUSH(1, sElF);
  VAR_STACK_PUSH_ARRAY(2, p, POFFSET+0);
  SET_VAR_STACK();

  method = objscheme_find_method((Scheme_Object *) ASSELF __gc_external, os_wxChoice_class, "on-set-focus", &mcache);
  if (!method || OBJSCHEME_PRIM_METHOD(method, os_wxChoiceOnSetFocus)) {
    SET_VAR_STACK();
    READY_TO_RETURN; ASSELF wxChoice::OnSetFocus();
  } else {
  mz_jmp_buf *savebuf, newbuf; Scheme_Thread *thread;
  ESCAPE_BLOCK(ESCAPE_NO_RET_VAL)
  p[0] = (Scheme_Object *) ASSELF __gc_external;

  v = WITH_VAR_STACK(scheme_apply(method, POFFSET+0, p));
  { thread = scheme_get_current_thread(); thread->error_buf = savebuf; }
  
     READY_TO_RETURN;
  }
}

static Scheme_Object *os_wxChoiceOnKillFocus(int n, Scheme_Object *p[]);

void os_wxChoice::OnKillFocus()
{
  Scheme_Object *p[POFFSET+0] INIT_NULLED_ARRAY({ NULLED_OUT });
  Scheme_Object *v;
  Scheme_Object *method INIT_NULLED_OUT;
#ifdef MZ_PRECISE_GC
  os_wxChoice *sElF = this;
#endif
  static void *mcache = 0;

  SETUP_VAR_STACK(5);
  VAR_STACK_PUSH(0, method);
  VAR_STACK_PUSH(1, sElF);
  VAR_STACK_PUSH_ARRAY(2, p, POFFSET+0);
  SET_VAR_STACK();

  method = objscheme_find_method((Scheme_Object *) ASSELF __gc_external, os_wxChoice_class, "on-kill-focus", &mcache);
  if (!method || OBJSCHEME_PRIM_METHOD(method, os_wxChoiceOnKillFocus)) {
    SET_VAR_STACK();
    READY_TO_RETURN; ASSELF wxChoice::OnKillFocus();
  } else {
  mz_jmp_buf *savebuf, newbuf; Scheme_Thread *thread;
  ESCAPE_BLOCK(ESCAPE_NO_RET_VAL)
  p[0] = (Scheme_Object *) ASSELF __gc_external;

  v = WITH_VAR_STACK(scheme_apply(method, POFFSET+0, p));
  { thread = scheme_get_current_thread(); thread->error_buf = savebuf; }
  
     READY_TO_RETURN;
  }
}

static Scheme_Object *os_wxChoiceSetSelection(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "set-selection in choice%", n, p);
  int x0;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+0], "set-selection in choice%"));

  if ((x0 < 0) || (x0 >= THISOBJECT->Number())) { READY_TO_RETURN; return scheme_void; }
  WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->SetSelection(x0));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxChoiceGetSelection(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  int r;
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "get-selection in choice%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  r = WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->GetSelection());

  
  
  READY_TO_RETURN;
  return scheme_make_integer(r);
}

static Scheme_Object *os_wxChoiceNumber(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  int r;
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "number in choice%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  r = WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->Number());

  
  
  READY_TO_RETURN;
  return scheme_make_integer(r);
}

static Scheme_Object *os_wxChoiceClear(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "clear in choice%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->Clear());

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxChoiceAppend(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "append in choice%", n, p);
  string x0 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);

  
  x0 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+0], "append in choice%"));

  
  WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->Append(x0));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxChoiceOnDropFile(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "on-drop-file in choice%", n, p);
  epathname x0 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);

  
  x0 = (epathname)WITH_VAR_STACK(objscheme_unbundle_epathname(p[POFFSET+0], "on-drop-file in choice%"));

  
  if (((Scheme_Class_Object *)p[0])->primflag)
    WITH_VAR_STACK(((os_wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->wxChoice::OnDropFile(x0));
  else
    WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->OnDropFile(x0));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxChoicePreOnEvent(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  Bool r;
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "pre-on-event in choice%", n, p);
  class wxWindow* x0 INIT_NULLED_OUT;
  class wxMouseEvent* x1 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(3);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);
  VAR_STACK_PUSH(2, x1);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_wxWindow(p[POFFSET+0], "pre-on-event in choice%", 0));
  x1 = WITH_VAR_STACK(objscheme_unbundle_wxMouseEvent(p[POFFSET+1], "pre-on-event in choice%", 0));

  
  if (((Scheme_Class_Object *)p[0])->primflag)
    r = WITH_VAR_STACK(((os_wxChoice *)((Scheme_Class_Object *)p[0])->primdata)-> wxWindow::PreOnEvent(x0, x1));
  else
    r = WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->PreOnEvent(x0, x1));

  
  
  READY_TO_RETURN;
  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *os_wxChoicePreOnChar(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  Bool r;
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "pre-on-char in choice%", n, p);
  class wxWindow* x0 INIT_NULLED_OUT;
  class wxKeyEvent* x1 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(3);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);
  VAR_STACK_PUSH(2, x1);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_wxWindow(p[POFFSET+0], "pre-on-char in choice%", 0));
  x1 = WITH_VAR_STACK(objscheme_unbundle_wxKeyEvent(p[POFFSET+1], "pre-on-char in choice%", 0));

  
  if (((Scheme_Class_Object *)p[0])->primflag)
    r = WITH_VAR_STACK(((os_wxChoice *)((Scheme_Class_Object *)p[0])->primdata)-> wxWindow::PreOnChar(x0, x1));
  else
    r = WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->PreOnChar(x0, x1));

  
  
  READY_TO_RETURN;
  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *os_wxChoiceOnSize(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "on-size in choice%", n, p);
  int x0;
  int x1;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+0], "on-size in choice%"));
  x1 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+1], "on-size in choice%"));

  
  if (((Scheme_Class_Object *)p[0])->primflag)
    WITH_VAR_STACK(((os_wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->wxChoice::OnSize(x0, x1));
  else
    WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->OnSize(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxChoiceOnSetFocus(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "on-set-focus in choice%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  if (((Scheme_Class_Object *)p[0])->primflag)
    WITH_VAR_STACK(((os_wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->wxChoice::OnSetFocus());
  else
    WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->OnSetFocus());

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxChoiceOnKillFocus(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxChoice_class);
  objscheme_check_valid(os_wxChoice_class, "on-kill-focus in choice%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  if (((Scheme_Class_Object *)p[0])->primflag)
    WITH_VAR_STACK(((os_wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->wxChoice::OnKillFocus());
  else
    WITH_VAR_STACK(((wxChoice *)((Scheme_Class_Object *)p[0])->primdata)->OnKillFocus());

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxChoice_ConstructScheme(int n,  Scheme_Object *p[])
{
  SETUP_PRE_VAR_STACK(1);
  PRE_VAR_STACK_PUSH(0, p);
  os_wxChoice *realobj INIT_NULLED_OUT;
  REMEMBER_VAR_STACK();
  class wxPanel* x0 INIT_NULLED_OUT;
  wxFunction x1;
  nstring x2 INIT_NULLED_OUT;
  int x3;
  int x4;
  int x5;
  int x6;
  int x7;
  string* x8 INIT_NULLED_OUT;
  int x9;
  class wxFont* x10 INIT_NULLED_OUT;
  string x11 INIT_NULLED_OUT;

  SETUP_VAR_STACK_PRE_REMEMBERED(7);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, realobj);
  VAR_STACK_PUSH(2, x0);
  VAR_STACK_PUSH(3, x2);
  VAR_STACK_PUSH(4, x8);
  VAR_STACK_PUSH(5, x10);
  VAR_STACK_PUSH(6, x11);

  int cb_pos = 0;
  if ((n < (POFFSET+3)) || (n > (POFFSET+11))) 
    WITH_VAR_STACK(scheme_wrong_count_m("initialization in choice%", POFFSET+3, POFFSET+11, n, p, 1));
  x0 = WITH_VAR_STACK(objscheme_unbundle_wxPanel(p[POFFSET+0], "initialization in choice%", 0));
  x1 = (SCHEME_NULLP(p[POFFSET+1]) ? NULL : (WITH_REMEMBERED_STACK(objscheme_istype_proc2(p[POFFSET+1], CB_USER)), cb_pos = 1, (CB_FUNCTYPE)CB_TOSCHEME));
  x2 = (nstring)WITH_VAR_STACK(objscheme_unbundle_nullable_string(p[POFFSET+2], "initialization in choice%"));
  if (n > (POFFSET+3)) {
    x3 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+3], "initialization in choice%"));
  } else
    x3 = -1;
  if (n > (POFFSET+4)) {
    x4 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+4], "initialization in choice%"));
  } else
    x4 = -1;
  if (n > (POFFSET+5)) {
    x5 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+5], "initialization in choice%"));
  } else
    x5 = -1;
  if (n > (POFFSET+6)) {
    x6 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+6], "initialization in choice%"));
  } else
    x6 = -1;
  if (n > (POFFSET+7)) {
    x8 = NULL;
  } else
    x8 = NULL;
  if (n > (POFFSET+8)) {
    x9 = WITH_VAR_STACK(unbundle_symset_choiceStyle(p[POFFSET+8], "initialization in choice%"));
  } else
    x9 = 0;
  if (n > (POFFSET+9)) {
    x10 = WITH_VAR_STACK(objscheme_unbundle_wxFont(p[POFFSET+9], "initialization in choice%", 1));
  } else
    x10 = NULL;
  if (n > (POFFSET+10)) {
    x11 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+10], "initialization in choice%"));
  } else
    x11 = "checkBox";

  x8 = WITH_VAR_STACK(__MakestringArray((7+POFFSET < n) ? p[POFFSET+7] : scheme_null, &x7, METHODNAME("choice%","initialization")));if (!x5) x5 = -1;if (!x6) x6 = -1;
  realobj = WITH_VAR_STACK(new os_wxChoice CONSTRUCTOR_ARGS((x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)));
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(realobj->gcInit_wxChoice(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11));
#endif
  realobj->__gc_external = (void *)p[0];
  /* delete[] x8; */
  realobj->callback_closure = p[POFFSET+cb_pos];
  READY_TO_RETURN;
  ((Scheme_Class_Object *)p[0])->primdata = realobj;
  ((Scheme_Class_Object *)p[0])->primflag = 1;
  WITH_REMEMBERED_STACK(objscheme_register_primpointer(p[0], &((Scheme_Class_Object *)p[0])->primdata));
  return scheme_void;
}

void objscheme_setup_wxChoice(Scheme_Env *env)
{
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, env);

  wxREGGLOB(os_wxChoice_class);

  os_wxChoice_class = WITH_VAR_STACK(objscheme_def_prim_class(env, "choice%", "item%", (Scheme_Method_Prim *)os_wxChoice_ConstructScheme, 11));

  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "set-selection" " method", (Scheme_Method_Prim *)os_wxChoiceSetSelection, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "get-selection" " method", (Scheme_Method_Prim *)os_wxChoiceGetSelection, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "number" " method", (Scheme_Method_Prim *)os_wxChoiceNumber, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "clear" " method", (Scheme_Method_Prim *)os_wxChoiceClear, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "append" " method", (Scheme_Method_Prim *)os_wxChoiceAppend, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "on-drop-file" " method", (Scheme_Method_Prim *)os_wxChoiceOnDropFile, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "pre-on-event" " method", (Scheme_Method_Prim *)os_wxChoicePreOnEvent, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "pre-on-char" " method", (Scheme_Method_Prim *)os_wxChoicePreOnChar, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "on-size" " method", (Scheme_Method_Prim *)os_wxChoiceOnSize, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "on-set-focus" " method", (Scheme_Method_Prim *)os_wxChoiceOnSetFocus, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxChoice_class, "on-kill-focus" " method", (Scheme_Method_Prim *)os_wxChoiceOnKillFocus, 0, 0));


  WITH_VAR_STACK(scheme_made_class(os_wxChoice_class));


  READY_TO_RETURN;
}

int objscheme_istype_wxChoice(Scheme_Object *obj, const char *stop, int nullOK)
{
  REMEMBER_VAR_STACK();
  if (nullOK && XC_SCHEME_NULLP(obj)) return 1;
  obj = objscheme_unwrap(obj, os_wxChoice_class);
  if (objscheme_is_a(obj, os_wxChoice_class))
    return 1;
  else {
    if (!stop)
       return 0;
    WITH_REMEMBERED_STACK(scheme_wrong_type(stop, nullOK ? "choice% object or " XC_NULL_STR: "choice% object", -1, 0, &obj));
    return 0;
  }
}

Scheme_Object *objscheme_bundle_wxChoice(class wxChoice *realobj)
{
  Scheme_Class_Object *obj INIT_NULLED_OUT;
  Scheme_Object *sobj INIT_NULLED_OUT;

  if (!realobj) return XC_SCHEME_NULL;

  if (realobj->__gc_external)
    return (Scheme_Object *)realobj->__gc_external;

  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, obj);
  VAR_STACK_PUSH(1, realobj);

  if ((sobj = WITH_VAR_STACK(objscheme_bundle_by_type(realobj, realobj->__type))))
    { READY_TO_RETURN; return sobj; }
  obj = (Scheme_Class_Object *)WITH_VAR_STACK(scheme_make_uninited_object(os_wxChoice_class));

  obj->primdata = realobj;
  WITH_VAR_STACK(objscheme_register_primpointer(obj, &obj->primdata));
  obj->primflag = 0;

  realobj->__gc_external = (void *)obj;
  READY_TO_RETURN;
  return (Scheme_Object *)obj;
}

class wxChoice *objscheme_unbundle_wxChoice(Scheme_Object *obj, const char *where, int nullOK)
{
  if (nullOK && XC_SCHEME_NULLP(obj)) return NULL;

  REMEMBER_VAR_STACK();

  obj = objscheme_unwrap(obj, os_wxChoice_class);
  (void)objscheme_istype_wxChoice(obj, where, nullOK);
  Scheme_Class_Object *o = (Scheme_Class_Object *)obj;
  WITH_REMEMBERED_STACK(objscheme_check_valid(NULL, NULL, 0, &obj));
  if (o->primflag)
    return (os_wxChoice *)o->primdata;
  else
    return (wxChoice *)o->primdata;
}


static Scheme_Object *wxChoiceGlobalwxSetComboBoxFont(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  class wxFont* x0 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_wxFont(p[0+0], "set-combo-box-font in choice%", 1));

  
  WITH_VAR_STACK(wxSetComboBoxFont(x0));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

void objscheme_setup_wxChoiceGlobal(Scheme_Env *env)
{
  Scheme_Object *functmp INIT_NULLED_OUT;
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, env);
  functmp = WITH_VAR_STACK(scheme_make_prim_w_arity((Scheme_Prim *)wxChoiceGlobalwxSetComboBoxFont, "set-combo-box-font", 1, 1));
  WITH_VAR_STACK(scheme_install_xc_global("set-combo-box-font", functmp, env));
  READY_TO_RETURN;
}



static void CB_TOSCHEME(CB_REALCLASS *realobj, wxCommandEvent *event)
{
  Scheme_Object *p[2];
  Scheme_Class_Object *obj;
  mz_jmp_buf savebuf;
  Scheme_Thread *thread;
  SETUP_VAR_STACK(4);
  VAR_STACK_PUSH(0, obj);
  VAR_STACK_PUSH(1, event);
  VAR_STACK_PUSH(2, p[0]);
  VAR_STACK_PUSH(3, p[1]);

  p[0] = NULL;
  p[1] = NULL;

  obj = (Scheme_Class_Object *)realobj->__gc_external;

  if (!obj) {
    // scheme_signal_error("bad callback object");
    return;
  }

  p[0] = (Scheme_Object *)obj;
  p[1] = WITH_VAR_STACK(objscheme_bundle_wxCommandEvent(event));

  thread = scheme_get_current_thread();
  COPY_JMPBUF(savebuf, *(thread->error_buf));

  if (!scheme_setjmp(*(thread->error_buf)))
    WITH_VAR_STACK(scheme_apply_multi(((CALLBACKCLASS *)obj->primdata)->callback_closure, 2, p));

  thread = scheme_get_current_thread();
  COPY_JMPBUF(*(thread->error_buf), savebuf);

  READY_TO_RETURN;
}
