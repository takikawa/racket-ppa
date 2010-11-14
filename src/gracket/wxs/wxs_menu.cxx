/* DO NOT EDIT THIS FILE. */
/* This file was generated by xctocc from "wxs_menu.xc". */


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

#include "wx_menu.h"

#include "wxscheme.h"
#include "wxs_menu.h"

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif




#ifdef wx_x
# define BM_SELECTED(map) ((map)->selectedTo)
#endif
#if defined(wx_mac) || defined(wx_msw)
# define BM_SELECTED(map) ((map)->selectedInto)
#endif
# define BM_IN_USE(map) ((map)->selectedIntoDC)




#define ESCAPE_NO_RET_VAL /*empty*/


#ifdef wx_mac
# define MAC_UNUSED(x) /**/
#else
# define MAC_UNUSED(x) x
#endif
#ifdef wx_xt
# define WINMAC_UNUSED(x) x
#else
# define WINMAC_UNUSED(x) /**/
#endif

static void menuSelect(wxMenu *MAC_UNUSED(m), wxMenuBar *WINMAC_UNUSED(mb))
{
#ifdef wx_msw
  m->SelectMenu();
#endif
#ifdef wx_xt
  mb->SelectAMenu(m);
#endif
}

// @CLASSBASE wxMenuItem "menu-item" : "object"
// @END

// wxMenu is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here


#define CB_FUNCTYPE wxFunction 


#undef CALLBACKCLASS
#undef CB_REALCLASS
#undef CB_UNBUNDLE
#undef CB_USER

#define CALLBACKCLASS os_wxMenu
#define CB_REALCLASS wxMenu
#define CB_UNBUNDLE objscheme_unbundle_wxMenu
#define CB_USER METHODNAME("menu%","initialization")

#undef CB_TOSCHEME
#undef CB_TOC
#define CB_TOSCHEME wxMenuCallbackToScheme
#define CB_TOC wxMenuCallbackToC


class CALLBACKCLASS;





extern wxCommandEvent *objscheme_unbundle_wxCommandEvent(Scheme_Object *,const char *,int);
extern Scheme_Object *objscheme_bundle_wxCommandEvent(wxCommandEvent *);

static void CB_TOSCHEME(CB_REALCLASS *obj, wxCommandEvent *event);








class os_wxMenu : public wxMenu {
 public:
  Scheme_Object *callback_closure;

  os_wxMenu CONSTRUCTOR_ARGS((nstring x0 = NULL, wxFunction x1 = NULL, class wxFont* x2 = NULL));
  ~os_wxMenu();
#ifdef MZ_PRECISE_GC
  void gcMark();
  void gcFixup();
#endif
};

#ifdef MZ_PRECISE_GC
void os_wxMenu::gcMark() {
  wxMenu::gcMark();
  gcMARK_TYPED(Scheme_Object *, callback_closure);
}
void os_wxMenu::gcFixup() {
  wxMenu::gcFixup();
  gcFIXUP_TYPED(Scheme_Object *, callback_closure);
}
#endif

static Scheme_Object *os_wxMenu_class;

os_wxMenu::os_wxMenu CONSTRUCTOR_ARGS((nstring x0, wxFunction x1, class wxFont* x2))
CONSTRUCTOR_INIT(: wxMenu(x0, x1, x2))
{
}

os_wxMenu::~os_wxMenu()
{
    objscheme_destroy(this, (Scheme_Object *) __gc_external);
}

static Scheme_Object *os_wxMenumenuSelect(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "select in menu%", n, p);
  class wxMenuBar* x0 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_wxMenuBar(p[POFFSET+0], "select in menu%", 1));

  
  WITH_VAR_STACK(menuSelect(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata), x0));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuGetFont(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  class wxFont* r;
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "get-font in menu%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  r = WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->GetFont());

  
  
  READY_TO_RETURN;
  return WITH_REMEMBERED_STACK(objscheme_bundle_wxFont(r));
}

static Scheme_Object *os_wxMenuSetWidth(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "set-width in menu%", n, p);
  int x0;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+0], "set-width in menu%"));

  
  WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->SetWidth(x0));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuSetTitle(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "set-title in menu%", n, p);
  string x0 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);

  
  x0 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+0], "set-title in menu%"));

  
  WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->SetTitle(x0));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuSetLabel(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "set-label in menu%", n, p);
  ExactLong x0;
  string x1 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x1);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "set-label in menu%"));
  x1 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+1], "set-label in menu%"));

  
  WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->SetLabel(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuSetHelpString(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "set-help-string in menu%", n, p);
  ExactLong x0;
  nstring x1 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x1);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "set-help-string in menu%"));
  x1 = (nstring)WITH_VAR_STACK(objscheme_unbundle_nullable_string(p[POFFSET+1], "set-help-string in menu%"));

  
  WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->SetHelpString(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuNumber(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  int r;
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "number in menu%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  r = WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->Number());

  
  
  READY_TO_RETURN;
  return scheme_make_integer(r);
}

static Scheme_Object *os_wxMenuEnable(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "enable in menu%", n, p);
  ExactLong x0;
  Bool x1;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "enable in menu%"));
  x1 = WITH_VAR_STACK(objscheme_unbundle_bool(p[POFFSET+1], "enable in menu%"));

  
  WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->Enable(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuCheck(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "check in menu%", n, p);
  ExactLong x0;
  Bool x1;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "check in menu%"));
  x1 = WITH_VAR_STACK(objscheme_unbundle_bool(p[POFFSET+1], "check in menu%"));

  
  WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->Check(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuChecked(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  Bool r;
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "checked? in menu%", n, p);
  ExactLong x0;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "checked? in menu%"));

  
  r = WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->Checked(x0));

  
  
  READY_TO_RETURN;
  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *os_wxMenuAppendSeparator(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "append-separator in menu%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->AppendSeparator());

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuDeleteByPosition(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  Bool r;
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "delete-by-position in menu%", n, p);
  int x0;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+0], "delete-by-position in menu%"));

  
  r = WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->DeleteByPosition(x0));

  
  
  READY_TO_RETURN;
  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *os_wxMenuDelete(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  Bool r;
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "delete in menu%", n, p);
  ExactLong x0;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "delete in menu%"));

  
  r = WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->Delete(x0));

  
  
  READY_TO_RETURN;
  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *os_wxMenuAppend(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  SETUP_PRE_VAR_STACK(1);
  PRE_VAR_STACK_PUSH(0, p);
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenu_class);
  objscheme_check_valid(os_wxMenu_class, "append in menu%", n, p);
  if ((n >= (POFFSET+3)) && WITH_REMEMBERED_STACK(objscheme_istype_ExactLong(p[POFFSET+0], NULL)) && WITH_REMEMBERED_STACK(objscheme_istype_string(p[POFFSET+1], NULL)) && WITH_REMEMBERED_STACK(objscheme_istype_wxMenu(p[POFFSET+2], NULL, 0))) {
    ExactLong x0;
    string x1 INIT_NULLED_OUT;
    class wxMenu* x2 INIT_NULLED_OUT;
    nstring x3 INIT_NULLED_OUT;

    SETUP_VAR_STACK_PRE_REMEMBERED(4);
    VAR_STACK_PUSH(0, p);
    VAR_STACK_PUSH(1, x1);
    VAR_STACK_PUSH(2, x2);
    VAR_STACK_PUSH(3, x3);

    
    if ((n < (POFFSET+3)) || (n > (POFFSET+4))) 
      WITH_VAR_STACK(scheme_wrong_count_m("append in menu% (submenu case)", POFFSET+3, POFFSET+4, n, p, 1));
    x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "append in menu% (submenu case)"));
    x1 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+1], "append in menu% (submenu case)"));
    x2 = WITH_VAR_STACK(objscheme_unbundle_wxMenu(p[POFFSET+2], "append in menu% (submenu case)", 0));
    if (n > (POFFSET+3)) {
      x3 = (nstring)WITH_VAR_STACK(objscheme_unbundle_nullable_string(p[POFFSET+3], "append in menu% (submenu case)"));
    } else
      x3 = NULL;

    
    WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->Append(x0, x1, x2, x3));

    
    
    READY_TO_PRE_RETURN;
  } else  {
    ExactLong x0;
    string x1 INIT_NULLED_OUT;
    nstring x2 INIT_NULLED_OUT;
    Bool x3;

    SETUP_VAR_STACK_PRE_REMEMBERED(3);
    VAR_STACK_PUSH(0, p);
    VAR_STACK_PUSH(1, x1);
    VAR_STACK_PUSH(2, x2);

    
    if ((n < (POFFSET+2)) || (n > (POFFSET+4))) 
      WITH_VAR_STACK(scheme_wrong_count_m("append in menu% (string item case)", POFFSET+2, POFFSET+4, n, p, 1));
    x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[POFFSET+0], "append in menu% (string item case)"));
    x1 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+1], "append in menu% (string item case)"));
    if (n > (POFFSET+2)) {
      x2 = (nstring)WITH_VAR_STACK(objscheme_unbundle_nullable_string(p[POFFSET+2], "append in menu% (string item case)"));
    } else
      x2 = NULL;
    if (n > (POFFSET+3)) {
      x3 = WITH_VAR_STACK(objscheme_unbundle_bool(p[POFFSET+3], "append in menu% (string item case)"));
    } else
      x3 = FALSE;

    
    WITH_VAR_STACK(((wxMenu *)((Scheme_Class_Object *)p[0])->primdata)->Append(x0, x1, x2, x3));

    
    
    READY_TO_PRE_RETURN;
  }

  return scheme_void;
}

static Scheme_Object *os_wxMenu_ConstructScheme(int n,  Scheme_Object *p[])
{
  SETUP_PRE_VAR_STACK(1);
  PRE_VAR_STACK_PUSH(0, p);
  os_wxMenu *realobj INIT_NULLED_OUT;
  REMEMBER_VAR_STACK();
  nstring x0 INIT_NULLED_OUT;
  wxFunction x1;
  class wxFont* x2 INIT_NULLED_OUT;

  SETUP_VAR_STACK_PRE_REMEMBERED(4);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, realobj);
  VAR_STACK_PUSH(2, x0);
  VAR_STACK_PUSH(3, x2);

  int cb_pos = 0;
  if ((n > (POFFSET+3))) 
    WITH_VAR_STACK(scheme_wrong_count_m("initialization in menu%", POFFSET+POFFSET, POFFSET+3, n, p, 1));
  if (n > (POFFSET+0)) {
    x0 = (nstring)WITH_VAR_STACK(objscheme_unbundle_nullable_string(p[POFFSET+0], "initialization in menu%"));
  } else
    x0 = NULL;
  if (n > (POFFSET+1)) {
    x1 = (SCHEME_NULLP(p[POFFSET+1]) ? NULL : (WITH_REMEMBERED_STACK(objscheme_istype_proc2(p[POFFSET+1], CB_USER)), cb_pos = 1, (CB_FUNCTYPE)CB_TOSCHEME));
  } else
    x1 = NULL;
  if (n > (POFFSET+2)) {
    x2 = WITH_VAR_STACK(objscheme_unbundle_wxFont(p[POFFSET+2], "initialization in menu%", 1));
  } else
    x2 = NULL;

  
  realobj = WITH_VAR_STACK(new os_wxMenu CONSTRUCTOR_ARGS((x0, x1, x2)));
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(realobj->gcInit_wxMenu(x0, x1, x2));
#endif
  realobj->__gc_external = (void *)p[0];
  
  realobj->callback_closure = p[POFFSET+cb_pos];
  READY_TO_RETURN;
  ((Scheme_Class_Object *)p[0])->primdata = realobj;
  ((Scheme_Class_Object *)p[0])->primflag = 1;
  WITH_REMEMBERED_STACK(objscheme_register_primpointer(p[0], &((Scheme_Class_Object *)p[0])->primdata));
  return scheme_void;
}

void objscheme_setup_wxMenu(Scheme_Env *env)
{
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, env);

  wxREGGLOB(os_wxMenu_class);

  os_wxMenu_class = WITH_VAR_STACK(objscheme_def_prim_class(env, "menu%", "object%", (Scheme_Method_Prim *)os_wxMenu_ConstructScheme, 14));

  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "select" " method", (Scheme_Method_Prim *)os_wxMenumenuSelect, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "get-font" " method", (Scheme_Method_Prim *)os_wxMenuGetFont, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "set-width" " method", (Scheme_Method_Prim *)os_wxMenuSetWidth, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "set-title" " method", (Scheme_Method_Prim *)os_wxMenuSetTitle, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "set-label" " method", (Scheme_Method_Prim *)os_wxMenuSetLabel, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "set-help-string" " method", (Scheme_Method_Prim *)os_wxMenuSetHelpString, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "number" " method", (Scheme_Method_Prim *)os_wxMenuNumber, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "enable" " method", (Scheme_Method_Prim *)os_wxMenuEnable, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "check" " method", (Scheme_Method_Prim *)os_wxMenuCheck, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "checked?" " method", (Scheme_Method_Prim *)os_wxMenuChecked, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "append-separator" " method", (Scheme_Method_Prim *)os_wxMenuAppendSeparator, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "delete-by-position" " method", (Scheme_Method_Prim *)os_wxMenuDeleteByPosition, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "delete" " method", (Scheme_Method_Prim *)os_wxMenuDelete, 1, 1));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenu_class, "append" " method", (Scheme_Method_Prim *)os_wxMenuAppend, 2, 4));


  WITH_VAR_STACK(scheme_made_class(os_wxMenu_class));


  READY_TO_RETURN;
}

int objscheme_istype_wxMenu(Scheme_Object *obj, const char *stop, int nullOK)
{
  REMEMBER_VAR_STACK();
  if (nullOK && XC_SCHEME_NULLP(obj)) return 1;
  obj = objscheme_unwrap(obj, os_wxMenu_class);
  if (objscheme_is_a(obj, os_wxMenu_class))
    return 1;
  else {
    if (!stop)
       return 0;
    WITH_REMEMBERED_STACK(scheme_wrong_type(stop, nullOK ? "menu% object or " XC_NULL_STR: "menu% object", -1, 0, &obj));
    return 0;
  }
}

Scheme_Object *objscheme_bundle_wxMenu(class wxMenu *realobj)
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
  obj = (Scheme_Class_Object *)WITH_VAR_STACK(scheme_make_uninited_object(os_wxMenu_class));

  obj->primdata = realobj;
  WITH_VAR_STACK(objscheme_register_primpointer(obj, &obj->primdata));
  obj->primflag = 0;

  realobj->__gc_external = (void *)obj;
  READY_TO_RETURN;
  return (Scheme_Object *)obj;
}

class wxMenu *objscheme_unbundle_wxMenu(Scheme_Object *obj, const char *where, int nullOK)
{
  if (nullOK && XC_SCHEME_NULLP(obj)) return NULL;

  REMEMBER_VAR_STACK();

  obj = objscheme_unwrap(obj, os_wxMenu_class);
  (void)objscheme_istype_wxMenu(obj, where, nullOK);
  Scheme_Class_Object *o = (Scheme_Class_Object *)obj;
  WITH_REMEMBERED_STACK(objscheme_check_valid(NULL, NULL, 0, &obj));
  if (o->primflag)
    return (os_wxMenu *)o->primdata;
  else
    return (wxMenu *)o->primdata;
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

// wxMenuBar is really derived from wxItem
//  but that makes no sense. Enforce different hierarchy here





class os_wxMenuBar : public wxMenuBar {
 public:

  os_wxMenuBar CONSTRUCTOR_ARGS(());
  ~os_wxMenuBar();
#ifdef MZ_PRECISE_GC
  void gcMark();
  void gcFixup();
#endif
};

#ifdef MZ_PRECISE_GC
void os_wxMenuBar::gcMark() {
  wxMenuBar::gcMark();
}
void os_wxMenuBar::gcFixup() {
  wxMenuBar::gcFixup();
}
#endif

static Scheme_Object *os_wxMenuBar_class;

os_wxMenuBar::os_wxMenuBar CONSTRUCTOR_ARGS(())
CONSTRUCTOR_INIT(: wxMenuBar())
{
}

os_wxMenuBar::~os_wxMenuBar()
{
    objscheme_destroy(this, (Scheme_Object *) __gc_external);
}

static Scheme_Object *os_wxMenuBarSetLabelTop(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenuBar_class);
  objscheme_check_valid(os_wxMenuBar_class, "set-label-top in menu-bar%", n, p);
  int x0;
  string x1 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x1);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+0], "set-label-top in menu-bar%"));
  x1 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+1], "set-label-top in menu-bar%"));

  if (x0 < 0) { READY_TO_RETURN; return scheme_void; }
  WITH_VAR_STACK(((wxMenuBar *)((Scheme_Class_Object *)p[0])->primdata)->SetLabelTop(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuBarNumber(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  int r;
  p[0] = objscheme_unwrap(p[0], os_wxMenuBar_class);
  objscheme_check_valid(os_wxMenuBar_class, "number in menu-bar%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  r = WITH_VAR_STACK(((wxMenuBar *)((Scheme_Class_Object *)p[0])->primdata)->Number());

  
  
  READY_TO_RETURN;
  return scheme_make_integer(r);
}

static Scheme_Object *os_wxMenuBarEnableTop(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenuBar_class);
  objscheme_check_valid(os_wxMenuBar_class, "enable-top in menu-bar%", n, p);
  int x0;
  Bool x1;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+0], "enable-top in menu-bar%"));
  x1 = WITH_VAR_STACK(objscheme_unbundle_bool(p[POFFSET+1], "enable-top in menu-bar%"));

  if (x0 < 0) { READY_TO_RETURN; return scheme_void; }
  WITH_VAR_STACK(((wxMenuBar *)((Scheme_Class_Object *)p[0])->primdata)->EnableTop(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuBarDelete(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  Bool r;
  p[0] = objscheme_unwrap(p[0], os_wxMenuBar_class);
  objscheme_check_valid(os_wxMenuBar_class, "delete in menu-bar%", n, p);
  class wxMenu* x0 INIT_NULLED_OUT;
  int x1;

  SETUP_VAR_STACK_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_wxMenu(p[POFFSET+0], "delete in menu-bar%", 1));
  if (n > (POFFSET+1)) {
    x1 = WITH_VAR_STACK(objscheme_unbundle_integer(p[POFFSET+1], "delete in menu-bar%"));
  } else
    x1 = 0;

  
  r = WITH_VAR_STACK(((wxMenuBar *)((Scheme_Class_Object *)p[0])->primdata)->Delete(x0, x1));

  
  
  READY_TO_RETURN;
  return (r ? scheme_true : scheme_false);
}

static Scheme_Object *os_wxMenuBarAppend(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  p[0] = objscheme_unwrap(p[0], os_wxMenuBar_class);
  objscheme_check_valid(os_wxMenuBar_class, "append in menu-bar%", n, p);
  class wxMenu* x0 INIT_NULLED_OUT;
  string x1 INIT_NULLED_OUT;

  SETUP_VAR_STACK_REMEMBERED(3);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, x0);
  VAR_STACK_PUSH(2, x1);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_wxMenu(p[POFFSET+0], "append in menu-bar%", 0));
  x1 = (string)WITH_VAR_STACK(objscheme_unbundle_string(p[POFFSET+1], "append in menu-bar%"));

  
  WITH_VAR_STACK(((wxMenuBar *)((Scheme_Class_Object *)p[0])->primdata)->Append(x0, x1));

  
  
  READY_TO_RETURN;
  return scheme_void;
}

static Scheme_Object *os_wxMenuBar_ConstructScheme(int n,  Scheme_Object *p[])
{
  SETUP_PRE_VAR_STACK(1);
  PRE_VAR_STACK_PUSH(0, p);
  os_wxMenuBar *realobj INIT_NULLED_OUT;
  REMEMBER_VAR_STACK();

  SETUP_VAR_STACK_PRE_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, realobj);

  
  if (n != (POFFSET+0)) 
    WITH_VAR_STACK(scheme_wrong_count_m("initialization in menu-bar%", POFFSET+0, POFFSET+0, n, p, 1));

  
  realobj = WITH_VAR_STACK(new os_wxMenuBar CONSTRUCTOR_ARGS(()));
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(realobj->gcInit_wxMenuBar());
#endif
  realobj->__gc_external = (void *)p[0];
  
  
  READY_TO_RETURN;
  ((Scheme_Class_Object *)p[0])->primdata = realobj;
  ((Scheme_Class_Object *)p[0])->primflag = 1;
  WITH_REMEMBERED_STACK(objscheme_register_primpointer(p[0], &((Scheme_Class_Object *)p[0])->primdata));
  return scheme_void;
}

void objscheme_setup_wxMenuBar(Scheme_Env *env)
{
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, env);

  wxREGGLOB(os_wxMenuBar_class);

  os_wxMenuBar_class = WITH_VAR_STACK(objscheme_def_prim_class(env, "menu-bar%", "object%", (Scheme_Method_Prim *)os_wxMenuBar_ConstructScheme, 5));

  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenuBar_class, "set-label-top" " method", (Scheme_Method_Prim *)os_wxMenuBarSetLabelTop, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenuBar_class, "number" " method", (Scheme_Method_Prim *)os_wxMenuBarNumber, 0, 0));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenuBar_class, "enable-top" " method", (Scheme_Method_Prim *)os_wxMenuBarEnableTop, 2, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenuBar_class, "delete" " method", (Scheme_Method_Prim *)os_wxMenuBarDelete, 1, 2));
  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxMenuBar_class, "append" " method", (Scheme_Method_Prim *)os_wxMenuBarAppend, 2, 2));


  WITH_VAR_STACK(scheme_made_class(os_wxMenuBar_class));


  READY_TO_RETURN;
}

int objscheme_istype_wxMenuBar(Scheme_Object *obj, const char *stop, int nullOK)
{
  REMEMBER_VAR_STACK();
  if (nullOK && XC_SCHEME_NULLP(obj)) return 1;
  obj = objscheme_unwrap(obj, os_wxMenuBar_class);
  if (objscheme_is_a(obj, os_wxMenuBar_class))
    return 1;
  else {
    if (!stop)
       return 0;
    WITH_REMEMBERED_STACK(scheme_wrong_type(stop, nullOK ? "menu-bar% object or " XC_NULL_STR: "menu-bar% object", -1, 0, &obj));
    return 0;
  }
}

Scheme_Object *objscheme_bundle_wxMenuBar(class wxMenuBar *realobj)
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
  obj = (Scheme_Class_Object *)WITH_VAR_STACK(scheme_make_uninited_object(os_wxMenuBar_class));

  obj->primdata = realobj;
  WITH_VAR_STACK(objscheme_register_primpointer(obj, &obj->primdata));
  obj->primflag = 0;

  realobj->__gc_external = (void *)obj;
  READY_TO_RETURN;
  return (Scheme_Object *)obj;
}

class wxMenuBar *objscheme_unbundle_wxMenuBar(Scheme_Object *obj, const char *where, int nullOK)
{
  if (nullOK && XC_SCHEME_NULLP(obj)) return NULL;

  REMEMBER_VAR_STACK();

  obj = objscheme_unwrap(obj, os_wxMenuBar_class);
  (void)objscheme_istype_wxMenuBar(obj, where, nullOK);
  Scheme_Class_Object *o = (Scheme_Class_Object *)obj;
  WITH_REMEMBERED_STACK(objscheme_check_valid(NULL, NULL, 0, &obj));
  if (o->primflag)
    return (os_wxMenuBar *)o->primdata;
  else
    return (wxMenuBar *)o->primdata;
}


#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

class wxsMenuItem : public wxObject
{
#ifdef MZ_PRECISE_GC
  void *my_id;
#endif
public:
  wxsMenuItem(void);
  ~wxsMenuItem();

  ExactLong Id(void) {
#ifdef MZ_PRECISE_GC
    return (ExactLong)my_id;
#else
    return (ExactLong)this;
#endif
  }
};

wxsMenuItem::wxsMenuItem(void)
#ifndef MZ_PRECISE_GC
: wxObject(WXGC_NO_CLEANUP)
#endif
{
#ifdef MZ_PRECISE_GC
  void *mid;
  mid = GC_malloc_immobile_box(GC_malloc_weak_box(gcOBJ_TO_PTR(this), NULL, 0));
  my_id = mid;
#endif
}

wxsMenuItem::~wxsMenuItem()
{
#ifdef MZ_PRECISE_GC
  GC_free_immobile_box((void **)my_id);
#endif
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

wxsMenuItem* wxsIdToMenuItem(ExactLong id)
{
#ifdef MZ_PRECISE_GC
  if (!id)
    return NULL;
  else
    return (wxsMenuItem *)gcPTR_TO_OBJ(GC_weak_box_val(*(void **)id));
#else
  return (wxsMenuItem *)id;
#endif
}




class os_wxsMenuItem : public wxsMenuItem {
 public:

  os_wxsMenuItem CONSTRUCTOR_ARGS(());
  ~os_wxsMenuItem();
#ifdef MZ_PRECISE_GC
  void gcMark();
  void gcFixup();
#endif
};

#ifdef MZ_PRECISE_GC
void os_wxsMenuItem::gcMark() {
  wxsMenuItem::gcMark();
}
void os_wxsMenuItem::gcFixup() {
  wxsMenuItem::gcFixup();
}
#endif

static Scheme_Object *os_wxsMenuItem_class;

os_wxsMenuItem::os_wxsMenuItem CONSTRUCTOR_ARGS(())
CONSTRUCTOR_INIT(: wxsMenuItem())
{
}

os_wxsMenuItem::~os_wxsMenuItem()
{
    objscheme_destroy(this, (Scheme_Object *) __gc_external);
}

static Scheme_Object *os_wxsMenuItemId(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  ExactLong r;
  p[0] = objscheme_unwrap(p[0], os_wxsMenuItem_class);
  objscheme_check_valid(os_wxsMenuItem_class, "id in menu-item%", n, p);

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  

  
  r = WITH_VAR_STACK(((wxsMenuItem *)((Scheme_Class_Object *)p[0])->primdata)->Id());

  
  
  READY_TO_RETURN;
  return WITH_REMEMBERED_STACK(scheme_make_integer_value(r));
}

static Scheme_Object *os_wxsMenuItem_ConstructScheme(int n,  Scheme_Object *p[])
{
  SETUP_PRE_VAR_STACK(1);
  PRE_VAR_STACK_PUSH(0, p);
  os_wxsMenuItem *realobj INIT_NULLED_OUT;
  REMEMBER_VAR_STACK();

  SETUP_VAR_STACK_PRE_REMEMBERED(2);
  VAR_STACK_PUSH(0, p);
  VAR_STACK_PUSH(1, realobj);

  
  if (n != (POFFSET+0)) 
    WITH_VAR_STACK(scheme_wrong_count_m("initialization in menu-item%", POFFSET+0, POFFSET+0, n, p, 1));

  
  realobj = WITH_VAR_STACK(new os_wxsMenuItem CONSTRUCTOR_ARGS(()));
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(realobj->gcInit_wxsMenuItem());
#endif
  realobj->__gc_external = (void *)p[0];
  
  
  READY_TO_RETURN;
  ((Scheme_Class_Object *)p[0])->primdata = realobj;
  ((Scheme_Class_Object *)p[0])->primflag = 1;
  WITH_REMEMBERED_STACK(objscheme_register_primpointer(p[0], &((Scheme_Class_Object *)p[0])->primdata));
  return scheme_void;
}

void objscheme_setup_wxsMenuItem(Scheme_Env *env)
{
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, env);

  wxREGGLOB(os_wxsMenuItem_class);

  os_wxsMenuItem_class = WITH_VAR_STACK(objscheme_def_prim_class(env, "menu-item%", "object%", (Scheme_Method_Prim *)os_wxsMenuItem_ConstructScheme, 1));

  WITH_VAR_STACK(scheme_add_method_w_arity(os_wxsMenuItem_class, "id" " method", (Scheme_Method_Prim *)os_wxsMenuItemId, 0, 0));


  WITH_VAR_STACK(scheme_made_class(os_wxsMenuItem_class));


  READY_TO_RETURN;
}

int objscheme_istype_wxsMenuItem(Scheme_Object *obj, const char *stop, int nullOK)
{
  REMEMBER_VAR_STACK();
  if (nullOK && XC_SCHEME_NULLP(obj)) return 1;
  obj = objscheme_unwrap(obj, os_wxsMenuItem_class);
  if (objscheme_is_a(obj, os_wxsMenuItem_class))
    return 1;
  else {
    if (!stop)
       return 0;
    WITH_REMEMBERED_STACK(scheme_wrong_type(stop, nullOK ? "menu-item% object or " XC_NULL_STR: "menu-item% object", -1, 0, &obj));
    return 0;
  }
}

Scheme_Object *objscheme_bundle_wxsMenuItem(class wxsMenuItem *realobj)
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
  obj = (Scheme_Class_Object *)WITH_VAR_STACK(scheme_make_uninited_object(os_wxsMenuItem_class));

  obj->primdata = realobj;
  WITH_VAR_STACK(objscheme_register_primpointer(obj, &obj->primdata));
  obj->primflag = 0;

  realobj->__gc_external = (void *)obj;
  READY_TO_RETURN;
  return (Scheme_Object *)obj;
}

class wxsMenuItem *objscheme_unbundle_wxsMenuItem(Scheme_Object *obj, const char *where, int nullOK)
{
  if (nullOK && XC_SCHEME_NULLP(obj)) return NULL;

  REMEMBER_VAR_STACK();

  obj = objscheme_unwrap(obj, os_wxsMenuItem_class);
  (void)objscheme_istype_wxsMenuItem(obj, where, nullOK);
  Scheme_Class_Object *o = (Scheme_Class_Object *)obj;
  WITH_REMEMBERED_STACK(objscheme_check_valid(NULL, NULL, 0, &obj));
  if (o->primflag)
    return (os_wxsMenuItem *)o->primdata;
  else
    return (wxsMenuItem *)o->primdata;
}





static Scheme_Object *wxsMenuItemGlobalwxsIdToMenuItem(int n,  Scheme_Object *p[])
{
  WXS_USE_ARGUMENT(n) WXS_USE_ARGUMENT(p)
  REMEMBER_VAR_STACK();
  class wxsMenuItem* r;
  ExactLong x0;

  SETUP_VAR_STACK_REMEMBERED(1);
  VAR_STACK_PUSH(0, p);

  
  x0 = WITH_VAR_STACK(objscheme_unbundle_ExactLong(p[0+0], "id-to-menu-item in menu-item%"));

  
  r = WITH_VAR_STACK(wxsIdToMenuItem(x0));

  
  
  READY_TO_RETURN;
  return WITH_REMEMBERED_STACK(objscheme_bundle_wxsMenuItem(r));
}

void objscheme_setup_wxsMenuItemGlobal(Scheme_Env *env)
{
  Scheme_Object *functmp INIT_NULLED_OUT;
  SETUP_VAR_STACK(1);
  VAR_STACK_PUSH(0, env);
  functmp = WITH_VAR_STACK(scheme_make_prim_w_arity((Scheme_Prim *)wxsMenuItemGlobalwxsIdToMenuItem, "id-to-menu-item", 1, 1));
  WITH_VAR_STACK(scheme_install_xc_global("id-to-menu-item", functmp, env));
  READY_TO_RETURN;
}

