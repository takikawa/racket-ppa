/*****************************************************************************/
/* blame-the-child accounting                                                */
/*****************************************************************************/
#ifdef NEWGC_BTC_ACCOUNT

#include "../src/schpriv.h"

/*****************************************************************************/
/* thread list                                                               */
/*****************************************************************************/
inline static int current_owner(NewGC *gc, Scheme_Custodian *c);

inline static void BTC_register_new_thread(void *t, void *c)
{
  NewGC *gc = GC_get_GC();
  GC_Thread_Info *work;

  work = (GC_Thread_Info *)ofm_malloc(sizeof(GC_Thread_Info));
  ((Scheme_Thread *)t)->gc_info = work;
  work->owner = current_owner(gc, (Scheme_Custodian *)c);
  work->thread = t;

  work->next = gc->thread_infos;
  gc->thread_infos = work;
}

inline static void BTC_register_thread(void *t, void *c)
{
  NewGC *gc = GC_get_GC();
  GC_Thread_Info *work;

  work = ((Scheme_Thread *)t)->gc_info;
  work->owner = current_owner(gc, (Scheme_Custodian *)c);
}

inline static void mark_threads(NewGC *gc, int owner)
{
  GC_Thread_Info *work;

  for(work = gc->thread_infos; work; work = work->next)
    if(work->owner == owner) {
      if (((Scheme_Thread *)work->thread)->running) {
        gc->normal_thread_mark(work->thread);
        if (work->thread == scheme_current_thread) {
          GC_mark_variable_stack(GC_variable_stack, 0, get_stack_base(gc), NULL);
        }
      }
    }
}

inline static void clean_up_thread_list(NewGC *gc)
{
  GC_Thread_Info *work = gc->thread_infos;
  GC_Thread_Info *prev = NULL;

  while(work) {
    if(!pagemap_find_page(gc->page_maps, work->thread) || marked(gc, work->thread)) {
      work->thread = GC_resolve(work->thread);
      prev = work;
      work = work->next;
    } else {
      GC_Thread_Info *next = work->next;

      if(prev) prev->next = next;
      if(!prev) gc->thread_infos = next;
      free(work);
      work = next;
    }
  }
}

inline static int thread_get_owner(void *p)
{
  return ((Scheme_Thread *)p)->gc_info->owner;
}

#define OWNER_TABLE_INIT_AMT 10

inline static int create_blank_owner_set(NewGC *gc)
{
  int i;
  unsigned int curr_size = gc->owner_table_size;
  OTEntry **owner_table = gc->owner_table;
  unsigned int old_size;
  OTEntry **naya;

  for (i = 1; i < curr_size; i++) {
    if (!owner_table[i]) {
      owner_table[i] = ofm_malloc(sizeof(OTEntry));
      bzero(owner_table[i], sizeof(OTEntry));
      return i;
    }
  }

  old_size = curr_size;
  if (!curr_size) {
    curr_size = OWNER_TABLE_INIT_AMT;
  }
  else {
    curr_size *= 2;
  }
  gc->owner_table_size = curr_size;

  naya = (OTEntry **)ofm_malloc(curr_size * sizeof(OTEntry*));
  memcpy(naya, owner_table, old_size*sizeof(OTEntry*));
  gc->owner_table = owner_table = naya;
  bzero(((char*)owner_table) + (sizeof(OTEntry*) * old_size),
      (curr_size - old_size) * sizeof(OTEntry*));

  return create_blank_owner_set(gc);
}

inline static int custodian_to_owner_set(NewGC *gc,Scheme_Custodian *cust)
{
  int i;

  if (cust->gc_owner_set)
    return cust->gc_owner_set;

  i = create_blank_owner_set(gc);
  gc->owner_table[i]->originator = cust;
  cust->gc_owner_set = i;

  return i;
}

inline static int current_owner(NewGC *gc, Scheme_Custodian *c)
{
  if (!scheme_current_thread)
    return 1;
  else if (!c)
    return thread_get_owner(scheme_current_thread);
  else
    return custodian_to_owner_set(gc, c);
}

void BTC_register_root_custodian(void *_c)
{
  NewGC *gc = GC_get_GC();
  Scheme_Custodian *c = (Scheme_Custodian *)_c;

  if (gc->owner_table) {
    /* Reset */
    free(gc->owner_table);
    gc->owner_table = NULL;
    gc->owner_table_size = 0;
  }

  if (create_blank_owner_set(gc) != 1) {
    GCPRINT(GCOUTF, "Something extremely weird (and bad) has happened.\n");
    abort();
  }

  gc->owner_table[1]->originator = c;
  c->gc_owner_set = 1;
}

inline static int custodian_member_owner_set(NewGC *gc, void *cust, int set)
{
  Scheme_Custodian_Reference *box;
  Scheme_Custodian *work = (Scheme_Custodian *) gc->owner_table[set]->originator;

  while(work) {
    if(work == cust) return 1;
    box = work->parent;
    work = box ? SCHEME_PTR1_VAL(box) : NULL;
  }
  return 0;
}

inline static void account_memory(NewGC *gc, int set, long amount)
{
  gc->owner_table[set]->memory_use += amount;
}

inline static void free_owner_set(NewGC *gc, int set)
{
  OTEntry **owner_table = gc->owner_table;
  if(owner_table[set]) {
    free(owner_table[set]);
  }
  owner_table[set] = NULL;
}

inline static void clean_up_owner_table(NewGC *gc)
{
  OTEntry **owner_table = gc->owner_table;
  const int table_size = gc->owner_table_size;
  int i;

  for(i = 1; i < table_size; i++)
    if(owner_table[i]) {
      /* repair or delete the originator */
      if(!marked(gc, owner_table[i]->originator)) {
        owner_table[i]->originator = NULL;
      } else 
        owner_table[i]->originator = GC_resolve(owner_table[i]->originator);

      /* potential delete */
      if(i != 1) 
        if((owner_table[i]->memory_use == 0) && !owner_table[i]->originator)
          free_owner_set(gc, i);
    }
}

inline static unsigned long custodian_usage(NewGC*gc, void *custodian)
{
  OTEntry **owner_table = gc->owner_table;
  const int table_size = gc->owner_table_size;
  unsigned long retval = 0;
  int i;

  if(!gc->really_doing_accounting) {
    gc->park[0] = custodian;
    gc->really_doing_accounting = 1;
    garbage_collect(gc, 1);
    custodian = gc->park[0]; 
    gc->park[0] = NULL;
  }
  for(i = 1; i < table_size; i++)
    if(owner_table[i] && custodian_member_owner_set(gc, custodian, i)) 
      retval += owner_table[i]->memory_use;
  return gcWORDS_TO_BYTES(retval);
}

inline static void BTC_memory_account_mark(NewGC *gc, mpage *page, void *ptr)
{
  GCDEBUG((DEBUGOUTF, "BTC_memory_account_mark: %p/%p\n", page, ptr));
  if(page->big_page) {
    struct objhead *info = (struct objhead *)(NUM(page->addr) + PREFIX_SIZE);

    if(info->btc_mark == gc->old_btc_mark) {
      info->btc_mark = gc->new_btc_mark;
      account_memory(gc, gc->current_mark_owner, gcBYTES_TO_WORDS(page->size));
      push_ptr(ptr);
    }
  } else {
    struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);

    if(info->btc_mark == gc->old_btc_mark) {
      info->btc_mark = gc->new_btc_mark;
      account_memory(gc, gc->current_mark_owner, info->size);
      push_ptr(ptr);
    }
  }
}

inline static void mark_cust_boxes(NewGC *gc, Scheme_Custodian *cur)
{
  Scheme_Object *pr, *prev = NULL, *next;
  GC_Weak_Box *wb;

  /* cust boxes is a list of weak boxes to cust boxes */

  pr = cur->cust_boxes;
  while (pr) {
    wb = (GC_Weak_Box *)SCHEME_CAR(pr);
    next = SCHEME_CDR(pr);
    if (wb->val) {
      gc->normal_cust_box_mark(wb->val);
      prev = pr;
    } else {
      if (prev)
        SCHEME_CDR(prev) = next;
      else
        cur->cust_boxes = next;
    }
    pr = next;
  }
  cur->cust_boxes = NULL;
}

int BTC_thread_mark(void *p)
{
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}

int BTC_custodian_mark(void *p)
{
  NewGC *gc = GC_get_GC();
  if(custodian_to_owner_set(gc, p) == gc->current_mark_owner)
    return gc->normal_custodian_mark(p);
  else
    return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}

int BTC_cust_box_mark(void *p)
{
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->size;
}

inline static void mark_normal_obj(NewGC *gc, mpage *page, void *ptr)
{
  switch(page->page_type) {
    case PAGE_TAGGED: {
                        /* we do not want to mark the pointers in a thread or custodian 
                           unless the object's owner is the current owner. In the case
                           of threads, we already used it for roots, so we can just
                           ignore them outright. In the case of custodians, we do need
                           to do the check; those differences are handled by replacing
                           the mark procedure in mark_table. */
                        gc->mark_table[*(unsigned short*)ptr](ptr);
                        break;
                      }
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: { 
                       struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
                       void **temp = ptr, **end = temp + (info->size - 1);

                       while(temp < end) gcMARK(*(temp++));
                       break;
                     };
    case PAGE_TARRAY: {
                        struct objhead *info = (struct objhead *)((char*)ptr - WORD_SIZE);
                        unsigned short tag = *(unsigned short*)ptr;
                        void **temp = ptr, **end = PPTR(info) + (info->size - INSET_WORDS);

                        while(temp < end) temp += gc->mark_table[tag](temp);
                        break;
                      }
    case PAGE_XTAGGED: GC_mark_xtagged(ptr); break;
  }
}

inline static void mark_acc_big_page(NewGC *gc, mpage *page)
{
  void **start = PPTR(NUM(page->addr) + PREFIX_SIZE + WORD_SIZE);
  void **end = PPTR(NUM(page->addr) + page->size);

  switch(page->page_type) {
    case PAGE_TAGGED: 
      {
        unsigned short tag = *(unsigned short*)start;
        if((unsigned long)gc->mark_table[tag] < PAGE_TYPES) {
          /* atomic */
        } else
          gc->mark_table[tag](start); break;
      }
    case PAGE_ATOMIC: break;
    case PAGE_ARRAY: while(start < end) gcMARK(*(start++)); break;
    case PAGE_XTAGGED: GC_mark_xtagged(start); break;
    case PAGE_TARRAY: {
                        unsigned short tag = *(unsigned short *)start;
                        end -= INSET_WORDS;
                        while(start < end) start += gc->mark_table[tag](start);
                        break;
                      }
  }
}


static void btc_overmem_abort(NewGC *gc)
{
  gc->kill_propagation_loop = 1;
  GCWARN((GCOUTF, "WARNING: Ran out of memory accounting. "
        "Info will be wrong.\n"));
}

static void propagate_accounting_marks(NewGC *gc)
{
  struct mpage *page;
  void *p;
  PageMap pagemap = gc->page_maps;
  while(pop_ptr(&p) && !gc->kill_propagation_loop) {
    page = pagemap_find_page(pagemap, p);
    set_backtrace_source(p, page->page_type);
    GCDEBUG((DEBUGOUTF, "btc_account: popped off page %p:%p, ptr %p\n", page, page->addr, p));
    if(page->big_page)
      mark_acc_big_page(gc, page);
    else
      mark_normal_obj(gc, page, p);
  }
  if(gc->kill_propagation_loop)
    reset_pointer_stack();
}

static void BTC_do_accounting(NewGC *gc)
{
  const int table_size = gc->owner_table_size;
  OTEntry **owner_table = gc->owner_table;

  if(gc->really_doing_accounting) {
    Scheme_Custodian *cur = owner_table[current_owner(gc, NULL)]->originator;
    Scheme_Custodian_Reference *box = cur->global_next;
    int i;

    GCDEBUG((DEBUGOUTF, "\nBEGINNING MEMORY ACCOUNTING\n"));
    gc->doing_memory_accounting = 1;
    gc->in_unsafe_allocation_mode = 1;
    gc->unsafe_allocation_abort = btc_overmem_abort;

    if(!gc->normal_thread_mark) {
      gc->normal_thread_mark    = gc->mark_table[scheme_thread_type];
      gc->normal_custodian_mark = gc->mark_table[scheme_custodian_type];
      gc->normal_cust_box_mark  = gc->mark_table[gc->cust_box_tag];
    }

    gc->mark_table[scheme_thread_type]    = BTC_thread_mark;
    gc->mark_table[scheme_custodian_type] = BTC_custodian_mark;
    gc->mark_table[gc->ephemeron_tag]     = BTC_ephemeron_mark;
    gc->mark_table[gc->cust_box_tag]      = BTC_cust_box_mark;

    /* clear the memory use numbers out */
    for(i = 1; i < table_size; i++)
      if(owner_table[i])
        owner_table[i]->memory_use = 0;

    /* the end of the custodian list is where we want to start */
    while(SCHEME_PTR1_VAL(box)) {
      cur = (Scheme_Custodian*)SCHEME_PTR1_VAL(box);
      box = cur->global_next;
    }

    /* walk backwards for the order we want */
    while(cur) {
      int owner = custodian_to_owner_set(gc, cur);

      gc->current_mark_owner = owner;
      GCDEBUG((DEBUGOUTF,"MARKING THREADS OF OWNER %i (CUST %p)\n", owner, cur));
      gc->kill_propagation_loop = 0;
      mark_threads(gc, owner);
      mark_cust_boxes(gc, cur);
      GCDEBUG((DEBUGOUTF, "Propagating accounting marks\n"));
      propagate_accounting_marks(gc);

      box = cur->global_prev; cur = box ? SCHEME_PTR1_VAL(box) : NULL;
    }

    gc->mark_table[scheme_thread_type]    = gc->normal_thread_mark;
    gc->mark_table[scheme_custodian_type] = gc->normal_custodian_mark;
    gc->mark_table[gc->ephemeron_tag]     = mark_ephemeron;
    gc->mark_table[gc->cust_box_tag]      = gc->normal_cust_box_mark;

    gc->in_unsafe_allocation_mode = 0;
    gc->doing_memory_accounting = 0;
    gc->old_btc_mark = gc->new_btc_mark;
    gc->new_btc_mark = !gc->new_btc_mark;
  }

  clear_stack_pages();
}

inline static void BTC_add_account_hook(int type,void *c1,void *c2,unsigned long b)
{
  NewGC *gc = GC_get_GC();
  AccountHook *work;

  if(!gc->really_doing_accounting) {
    gc->park[0] = c1; 
    gc->park[1] = c2;
    gc->really_doing_accounting = 1;
    garbage_collect(gc, 1);
    c1 = gc->park[0]; gc->park[0] = NULL;
    c2 = gc->park[1]; gc->park[1] = NULL;
  }

  if (type == MZACCT_LIMIT)
    gc->reset_limits = 1;
  if (type == MZACCT_REQUIRE)
    gc->reset_required = 1;

  for(work = gc->hooks; work; work = work->next) {
    if((work->type == type) && (work->c2 == c2) && (work->c1 == c1)) {
      if(type == MZACCT_REQUIRE) {
        if(b > work->amount) work->amount = b;
      } else { /* (type == MZACCT_LIMIT) */
        if(b < work->amount) work->amount = b;
      }
      break;
    } 
  }

  if(!work) {
    work = ofm_malloc(sizeof(AccountHook));
    work->type = type; 
    work->c1 = c1; 
    work->c2 = c2; 
    work->amount = b;

    /* push work onto hooks */
    work->next = gc->hooks;
    gc->hooks = work;
  }
}

inline static void clean_up_account_hooks(NewGC *gc)
{
  AccountHook *work = gc->hooks;
  AccountHook *prev = NULL;

  while(work) {
    if((!work->c1 || marked(gc, work->c1)) && marked(gc, work->c2)) {
      work->c1 = GC_resolve(work->c1);
      work->c2 = GC_resolve(work->c2);
      prev = work;
      work = work->next;
    } else {
      /* remove work hook */
      AccountHook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) gc->hooks = next;
      free(work);
      work = next;
    }
  }
}

static unsigned long custodian_super_require(NewGC *gc, void *c)
{
  int set = ((Scheme_Custodian *)c)->gc_owner_set;
  const int table_size = gc->owner_table_size;
  OTEntry **owner_table = gc->owner_table;

  if (gc->reset_required) {
    int i;
    for(i = 1; i < table_size; i++)
      if (owner_table[i])
        owner_table[i]->required_set = 0;
    gc->reset_required = 0;
  }

  if (!owner_table[set]->required_set) {
    unsigned long req = 0, r;
    AccountHook *work = gc->hooks;

    while(work) {
      if ((work->type == MZACCT_REQUIRE) && (c == work->c2)) {
        r = work->amount + custodian_super_require(gc, work->c1);
        if (r > req)
          req = r;
      }
      work = work->next;
    }
    owner_table[set]->super_required = req;
    owner_table[set]->required_set = 1;
  }

  return owner_table[set]->super_required;
}

inline static void BTC_run_account_hooks(NewGC *gc)
{
  AccountHook *work = gc->hooks; 
  AccountHook *prev = NULL;

  while(work) {
    if( ((work->type == MZACCT_REQUIRE) && 
          ((gc->used_pages > (gc->max_pages_for_use / 2))
           || ((((gc->max_pages_for_use / 2) - gc->used_pages) * APAGE_SIZE)
             < (work->amount + custodian_super_require(gc, work->c1)))))
        ||
        ((work->type == MZACCT_LIMIT) &&
         (GC_get_memory_use(work->c1) > work->amount))) {
      AccountHook *next = work->next;

      if(prev) prev->next = next;
      if(!prev) gc->hooks = next;
      scheme_schedule_custodian_close(work->c2);
      free(work);
      work = next;
    } else {
      prev = work; 
      work = work->next;
    }
  }
}

static unsigned long custodian_single_time_limit(NewGC *gc, int set)
{
  OTEntry **owner_table = gc->owner_table;
  const int table_size = gc->owner_table_size;

  if (!set)
    return (unsigned long)(long)-1;

  if (gc->reset_limits) {
    int i;
    for(i = 1; i < table_size; i++)
      if (owner_table[i])
        owner_table[i]->limit_set = 0;
    gc->reset_limits = 0;
  }

  if (!owner_table[set]->limit_set) {
    /* Check for limits on this custodian or one of its ancestors: */
    unsigned long limit = (unsigned long)(long)-1;
    Scheme_Custodian *orig = (Scheme_Custodian *) owner_table[set]->originator, *c;
    AccountHook *work = gc->hooks;

    while(work) {
      if ((work->type == MZACCT_LIMIT) && (work->c1 == work->c2)) {
        c = orig;
        while (1) {
          if (work->c2 == c) {
            if (work->amount < limit)
              limit = work->amount;
            break;
          }
          if (!c->parent)
            break;
          c = (Scheme_Custodian*)SCHEME_PTR1_VAL(c->parent);
          if (!c)
            break;
        }
      }
      work = work->next;
    }
    owner_table[set]->single_time_limit = limit;
    owner_table[set]->limit_set = 1;
  }

  return owner_table[set]->single_time_limit;
}

long BTC_get_memory_use(NewGC* gc, void *o)
{
  Scheme_Object *arg = (Scheme_Object*)o;
  if(SAME_TYPE(SCHEME_TYPE(arg), scheme_custodian_type)) {
    return custodian_usage(gc, arg);
  }

  return 0;
}

int BTC_single_allocation_limit(NewGC *gc, size_t sizeb) {
  /* We're allowed to fail. Check for allocations that exceed a single-time
   * limit. Otherwise, the limit doesn't work as intended, because
   * a program can allocate a large block that nearly exhausts memory,
   * and then a subsequent allocation can fail. As long as the limit
   * is much smaller than the actual available memory, and as long as
   * GC_out_of_memory protects any user-requested allocation whose size
   * is independent of any existing object, then we can enforce the limit. */
  return (custodian_single_time_limit(gc, thread_get_owner(scheme_current_thread)) < sizeb);
}

static inline void BTC_clean_up(NewGC *gc) {
  clean_up_thread_list(gc);
  clean_up_owner_table(gc);
  clean_up_account_hooks(gc);
}

static inline void BTC_set_btc_mark(NewGC *gc, void* x) {
  ((struct objhead *)(x))->btc_mark = gc->old_btc_mark;
}
#endif
