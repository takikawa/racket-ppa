/* A new accouting precise GC for MzScheme
   Copyright (C) 2001, 2002 Matthew Flatt and Adam Wick
   All rights reserved.

   Please see full copyright in the documentation
   Search for "FIXME" for known improvement points 

   IF YOU'RE NOT ADAM (AND PROBABLY IF YOU ARE) READ THIS FIRST:

   This is now a hybrid copying/mark-compact collector. The nursery
   (generation 0) is copied into the old generation (generation 1),
   but the old generation compacts. This yields a nice combination
   of performance, scalability and memory efficiency.

   The following page map invariants are required:

   Outside of collection, only pages in the older generation should
   be in the global poitner -> page map.

   During the mark phase of collection, only pages which contain
   objects which may be marked should be in the page map. This means
   that during minor collections, only pages in the nursery should
   be in the map.

   During the rest of collection, only pages which contain the past
   locations of moved data should be in the page map. This means only
   the nursery and pages being compacted.
*/

#define MZ_PRECISE_GC 1 /* required for mz includes to work right */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "platforms.h"
#include "gc2.h"
#include "gc2_dump.h"

/* the number of tags to use for tagged objects */
#define NUMBER_OF_TAGS 512

#ifdef SIXTY_FOUR_BIT_INTEGERS
#define PAGEMAP64_LEVEL1_SIZE (1 << 16)
#define PAGEMAP64_LEVEL2_SIZE (1 << 16)
#define PAGEMAP64_LEVEL3_SIZE (1 << (32 - LOG_APAGE_SIZE))
#define PAGEMAP64_LEVEL1_BITS(p) (((unsigned long)(p)) >> 48)
#define PAGEMAP64_LEVEL2_BITS(p) ((((unsigned long)(p)) >> 32) & ((PAGEMAP64_LEVEL2_SIZE) - 1))
#define PAGEMAP64_LEVEL3_BITS(p) ((((unsigned long)(p)) >> LOG_APAGE_SIZE) & ((PAGEMAP64_LEVEL3_SIZE) - 1))
#else
#define PAGEMAP32_SIZE (1 << (32 - LOG_APAGE_SIZE))
#define PAGEMAP32_BITS(x) (NUM(x) >> LOG_APAGE_SIZE)
#endif

/* the page type constants */
enum {
  PAGE_TAGGED   = 0,
  PAGE_ATOMIC   = 1,
  PAGE_ARRAY    = 2,
  PAGE_TARRAY   = 3,
  PAGE_XTAGGED  = 4,
  PAGE_BIG      = 5,
  /* the number of page types. */
  PAGE_TYPES    = 6,
};

static const char *type_name[PAGE_TYPES] = { 
  "tagged", 
  "atomic", 
  "array",
  "tagged array", 
  "xtagged",
  "big" 
};


#include "newgc.h"
static THREAD_LOCAL NewGC *GC;
#define GCTYPE NewGC
#define GC_get_GC() (GC)

#include "msgprint.c"

/*****************************************************************************/
/* Collector selection. Change the definitions of these to set or unset the  */
/* particular collector you want.                                            */
/*****************************************************************************/

/* This turns on blame-the-child automatic memory accounting */
/* #define NEWGC_BTC_ACCOUNT */
/* #undef NEWGC_BTC_ACCOUNT */

/* This turns on memory tracing */
/* #define NEWGC_MEMORY_TRACE */

/* This turns on support for heap debugging (FIXME: NOT IMPLEMENTED YET) */
/* #define NEWGC_HEAP_DEBUGGING */

/* This turns on some internal debugging logs. Don't turn this on unless you
   don't care about performance and you're hacking the collector */
/* #define NEWGC_INTERNAL_DEBUGGING */

/* The initial size of generation 0. This will grow and shrink a bit as time
   goes on */
#define GEN0_INITIAL_SIZE (1 * 1024 * 1024)
#define GEN0_SIZE_FACTOR 0.5
#define GEN0_SIZE_ADDITION (512 * 1024)
#define GEN0_MAX_SIZE (32 * 1024 * 1024)
#define GEN0_PAGE_SIZE (1 * 1024 * 1024)

/* This is the log base 2 of the size of one word, given in bytes */
#ifdef SIXTY_FOUR_BIT_INTEGERS
# define LOG_WORD_SIZE 3
#else
# define LOG_WORD_SIZE 2
#endif


/* the size of a page we use for the internal mark stack */
#define STACK_PART_SIZE (1 * 1024 * 1024)


/* # define LOG_APAGE_SIZE ... see gc2_obj.h */
/* These are computed from the previous settings. You shouldn't mess with 
   them */
#define PTR(x) ((void*)(x))
#define PPTR(x) ((void**)(x))
#define NUM(x) ((unsigned long)(x))
#define WORD_SIZE (1 << LOG_WORD_SIZE)
#define WORD_BITS (8 * WORD_SIZE)
#define APAGE_SIZE (1 << LOG_APAGE_SIZE)
#define GENERATIONS 1

/* the externals */
void (*GC_collect_start_callback)(void);
void (*GC_collect_end_callback)(void);
void (*GC_collect_inform_callback)(int major_gc, long pre_used, long post_used);
void (*GC_out_of_memory)(void);
void (*GC_report_out_of_memory)(void);
unsigned long (*GC_get_thread_stack_base)(void);
void (*GC_mark_xtagged)(void *obj);
void (*GC_fixup_xtagged)(void *obj);

#include "my_qsort.c"

/*****************************************************************************/
/* OS-Level Memory Management Routines                                       */
/*****************************************************************************/
static void garbage_collect(NewGC*, int);

static void out_of_memory()
{
  if (GC_report_out_of_memory)
    GC_report_out_of_memory();
  GCPRINT(GCOUTF, "The system has run out of memory!\n");
  abort();
}

static void *ofm_malloc(size_t size) {
  void *ptr = malloc(size);
  if (!ptr) out_of_memory();
  return ptr;
}

static void *ofm_malloc_zero(size_t size) {
  void *ptr;
  ptr = ofm_malloc(size);
  memset(ptr, 0, size);
  return ptr;
}

inline static void check_used_against_max(NewGC *gc, size_t len) 
{
  gc->used_pages += (len / APAGE_SIZE) + (((len % APAGE_SIZE) == 0) ? 0 : 1);

  if(gc->in_unsafe_allocation_mode) {
    if(gc->used_pages > gc->max_pages_in_heap)
      gc->unsafe_allocation_abort(gc);
  } else {
    if(gc->used_pages > gc->max_pages_for_use) {
      garbage_collect(gc, 0); /* hopefully this will free enough space */
      if(gc->used_pages > gc->max_pages_for_use) {
        garbage_collect(gc, 1); /* hopefully *this* will free enough space */
        if(gc->used_pages > gc->max_pages_for_use) {
          /* too much memory allocated. 
           * Inform the thunk and then die semi-gracefully */
          if(GC_out_of_memory)
            GC_out_of_memory();
          out_of_memory();
        }
      }
    }
  }
}

#include "page_range.c"

#include "vm.c"

#include "protect_range.c"

static void *malloc_pages(NewGC *gc, size_t len, size_t alignment)
{
  void *ptr;
  check_used_against_max(gc, len);
  ptr = vm_malloc_pages(gc->vm, len, alignment, 0);
  if (!ptr) out_of_memory();
  return ptr;
}

static void *malloc_dirty_pages(NewGC *gc, size_t len, size_t alignment)
{
  void *ptr;
  check_used_against_max(gc, len);
  ptr = vm_malloc_pages(gc->vm, len, alignment, 1);
  if (!ptr) out_of_memory();
  return ptr;
}

static void free_pages(NewGC *gc, void *p, size_t len)
{
  gc->used_pages -= (len / APAGE_SIZE) + (((len % APAGE_SIZE) == 0) ? 0 : 1);
  vm_free_pages(gc->vm, p, len);
}


/*****************************************************************************/
/* Memory Tracing, Part 1                                                    */
/*****************************************************************************/
#ifdef NEWGC_MEMORY_TRACE
# error "memory tracing not implemented in this particular revision \
  please revert to early versions of this collector, and then nag \
Adam (awick@cs.utah.edu) to put this stuff back in"
#endif

int GC_mtrace_new_id(void *f)
{
  return 0;
}

int GC_mtrace_union_current_with(int newval)
{
  return 0;
}

/*****************************************************************************/
/* Page Map Routines                                                         */
/*****************************************************************************/

/* the page map makes a nice mapping from addresses to pages, allowing
   fairly fast lookup. this is useful. */

inline static void pagemap_set(PageMap page_maps1, void *p, mpage *value) {
#ifdef SIXTY_FOUR_BIT_INTEGERS
  unsigned long pos;
  mpage ***page_maps2;
  mpage **page_maps3;

  pos = PAGEMAP64_LEVEL1_BITS(p);
  page_maps2 = page_maps1[pos];
  if (!page_maps2) {
    page_maps2 = (mpage ***)calloc(PAGEMAP64_LEVEL2_SIZE, sizeof(mpage **));
    page_maps1[pos] = page_maps2;
  }
  pos = PAGEMAP64_LEVEL2_BITS(p);
  page_maps3 = page_maps2[pos];
  if (!page_maps3) {
    page_maps3 = (mpage **)calloc(PAGEMAP64_LEVEL3_SIZE, sizeof(mpage *));
    page_maps2[pos] = page_maps3;
  }
  page_maps3[PAGEMAP64_LEVEL3_BITS(p)] = value;
#else
  page_maps1[PAGEMAP32_BITS(p)] = value;
#endif
}

inline static struct mpage *pagemap_find_page(PageMap page_maps1, void *p) {
#ifdef SIXTY_FOUR_BIT_INTEGERS
  mpage ***page_maps2;
  mpage **page_maps3;

  page_maps2 = page_maps1[PAGEMAP64_LEVEL1_BITS(p)];
  if (!page_maps2) return NULL;
  page_maps3 = page_maps2[PAGEMAP64_LEVEL2_BITS(p)];
  if (!page_maps3) return NULL;
  return page_maps3[PAGEMAP64_LEVEL3_BITS(p)];
#else
  return page_maps1[PAGEMAP32_BITS(p)];
#endif
}

/* These procedures modify or use the page map. The page map provides us very
   fast mappings from pointers to the page the reside on, if any. The page 
   map itself serves two important purposes:

   Between collections, it maps pointers to write-protected pages, so that 
   the write-barrier can identify what page a write has happened to and
   mark it as potentially containing pointers from gen 1 to gen 0. 

   During collections, it maps pointers to "from" pages. */

/* pagemap_modify_with_size could be optimized more for the 64 bit case
   repeatedly calling pagemap_set for the 64 bit case is not optimal */
inline static void pagemap_modify_with_size(PageMap pagemap, mpage *page, long size, mpage *val) {
  void *p = page->addr;

  while(size > 0) {
    pagemap_set(pagemap, p, val);
    size -= APAGE_SIZE;
    p = (char *)p + APAGE_SIZE;
  }
}

inline static void pagemap_modify(PageMap pagemap, mpage *page, mpage *val) {
  long size = page->big_page ? page->size : APAGE_SIZE;
  pagemap_modify_with_size(pagemap, page, size, val);
}

inline static void pagemap_add(PageMap pagemap, mpage *page)
{
  pagemap_modify(pagemap, page, page);
}

inline static void pagemap_add_with_size(PageMap pagemap, mpage *page, long size)
{
  pagemap_modify_with_size(pagemap, page, size, page);
}

inline static void pagemap_remove(PageMap pagemap, mpage *page)
{
  pagemap_modify(pagemap, page, NULL);
}

inline static void pagemap_remove_with_size(PageMap pagemap, mpage *page, long size)
{
  pagemap_modify_with_size(pagemap, page, size, NULL);
}

int GC_is_allocated(void *p)
{
  NewGC *gc = GC_get_GC();
  return !!pagemap_find_page(gc->page_maps, p);
}


/*****************************************************************************/
/* Allocation                                                                */
/*****************************************************************************/

/* struct objhead is defined in gc2_obj.h */
/* Make sure alloction starts out double-word aligned. 
   The header on each allocated object is one word, so to make
   the content double-word aligned, we deeper. */
#ifdef GC_ALIGN_SIXTEEN
# ifdef SIXTY_FOUR_BIT_INTEGERS
#  define PREFIX_WSIZE 1
# else
#  define PREFIX_WSIZE 3
# endif
#elif defined(GC_ALIGN_EIGHT) 
# if defined(SIXTY_FOUR_BIT_INTEGERS)
#  define PREFIX_WSIZE 0
# else
#  define PREFIX_WSIZE 1
# endif
#else /* GC_ALIGHT_FOUR or byte aligned */
# define PREFIX_WSIZE 0
#endif
#define PREFIX_SIZE (PREFIX_WSIZE * WORD_SIZE)


/* this is the maximum size of an object that will fit on a page, in words.
   the "- 3" is basically used as a fudge/safety factor, and has no real, 
   important meaning. */
#define MAX_OBJECT_SIZEW (gcBYTES_TO_WORDS(APAGE_SIZE) - PREFIX_WSIZE - 3)


/* Generation 0. Generation 0 is a set of very large pages in a list(gc->gen0.pages),
   plus a set of smaller bigpages in a separate list(gc->gen0.big_pages). 
   The former is purely an optimization, saving us from constantly deallocating 
   and allocating the entire nursery on every GC. The latter is useful because it simplifies
   the allocation process (which is also a speed hack, come to think of it) 

   gc->gen0.pages             is the list of very large nursery pages.
   gc->gen0.curr_alloc_page   is the member of this list we are currently allocating on.
   The size count helps us trigger collection quickly when we're running out of space; see
   the test in allocate_big. 
   */
THREAD_LOCAL unsigned long GC_gen0_alloc_page_ptr = 0;
THREAD_LOCAL unsigned long GC_gen0_alloc_page_end = 0;

/* miscellaneous variables */
static const char *zero_sized[4]; /* all 0-sized allocs get this */

static size_t round_to_apage_size(size_t sizeb)
{  
  sizeb += APAGE_SIZE - 1;
  sizeb -= sizeb & (APAGE_SIZE - 1);
  return sizeb;
}

static struct mpage *malloc_mpage()
{
  struct mpage *page;
  page = ofm_malloc_zero(sizeof(struct mpage));
  return page;
}

static void free_mpage(struct mpage *page)
{
  free(page);
}

#ifdef NEWGC_BTC_ACCOUNT
static inline int BTC_single_allocation_limit(NewGC *gc, size_t sizeb);
#endif

/* the core allocation functions */
static void *allocate_big(size_t sizeb, int type)
{
  NewGC *gc = GC_get_GC();
  mpage *bpage;
  void *addr;

#ifdef NEWGC_BTC_ACCOUNT
  if(GC_out_of_memory) {
    if (BTC_single_allocation_limit(gc, sizeb)) {
      /* We're allowed to fail. Check for allocations that exceed a single-time
         limit. Otherwise, the limit doesn't work as intended, because
         a program can allocate a large block that nearly exhausts memory,
         and then a subsequent allocation can fail. As long as the limit
         is much smaller than the actual available memory, and as long as
         GC_out_of_memory protects any user-requested allocation whose size
         is independent of any existing object, then we can enforce the limit. */
      GC_out_of_memory();
    }
  }
#endif

  /* the actual size of this is the size, ceilinged to the next largest word,
     plus one word for the object header.
     This last serves many purposes, including making sure the object is 
     aligned for Sparcs. */
  sizeb = gcWORDS_TO_BYTES((gcBYTES_TO_WORDS(sizeb) + PREFIX_WSIZE + 1));

  if((gc->gen0.current_size + sizeb) >= gc->gen0.max_size) {
    if (!gc->dumping_avoid_collection)
      garbage_collect(gc, 0);
  }
  gc->gen0.current_size += sizeb;

  /* We not only need APAGE_SIZE alignment, we 
     need everything consisently mapped within an APAGE_SIZE
     segment. So round up. */
  bpage = malloc_mpage();
  if (type == PAGE_ATOMIC)
    addr = malloc_dirty_pages(gc, round_to_apage_size(sizeb), APAGE_SIZE);
  else
    addr = malloc_pages(gc, round_to_apage_size(sizeb), APAGE_SIZE);
  bpage->addr = addr;
  bpage->size = sizeb;
  bpage->big_page = 1;
  bpage->page_type = type;

  /* push new bpage onto GC->gen0.big_pages */
  bpage->next = gc->gen0.big_pages;
  if(bpage->next) bpage->next->prev = bpage;
  gc->gen0.big_pages = bpage;
  pagemap_add(gc->page_maps, bpage);

  return PTR(NUM(addr) + PREFIX_SIZE + WORD_SIZE);
}

/* ALIGN_BYTES_SIZE can assume that the argument is already word-aligned. */
/* INSET_WORDS is how many words in a tagged array can be padding, plus one; it
   must also be no more than the minimum size of a tagged element. */
#ifdef GC_ALIGN_SIXTEEN
# ifdef SIXTY_FOUR_BIT_INTEGERS
#  define ALIGN_SIZE(sizew) (((sizew) & 0x1) ? ((sizew) + 1) : (sizew))
#  define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & WORD_SIZE) ? ((sizeb) + WORD_SIZE) : (sizeb))
#  define INSET_WORDS 1
# else
#  define ALIGN_SIZE(sizew) (((sizew) & 0x3) ? ((sizew) + (4 - ((sizew) & 0x3))) : (sizew))
#  define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & (3 * WORD_SIZE)) ? ((sizeb) + ((4 * WORD_SIZE) - ((sizeb) & (3 * WORD_SIZE)))) : (sizeb))
#  define INSET_WORDS 3
# endif
#else
# ifdef GC_ALIGN_EIGHT
#  ifdef SIXTY_FOUR_BIT_INTEGERS
#   define ALIGN_SIZE(sizew) (sizew)
#   define ALIGN_BYTES_SIZE(sizeb) (sizeb)
#   define INSET_WORDS 0
#  else
#   define ALIGN_SIZE(sizew) (((sizew) & 0x1) ? ((sizew) + 1) : (sizew))
#   define ALIGN_BYTES_SIZE(sizeb) (((sizeb) & WORD_SIZE) ? ((sizeb) + WORD_SIZE) : (sizeb))
#   define INSET_WORDS 1
#  endif
# else
#  define ALIGN_SIZE(sizew) (sizew)
#  define ALIGN_BYTES_SIZE(sizeb) (sizeb)
#  define INSET_WORDS 0
# endif
#endif

inline static struct mpage *gen0_create_new_mpage(NewGC *gc) {
  mpage *newmpage;

  newmpage = malloc_mpage(gc);
  newmpage->addr = malloc_dirty_pages(gc, GEN0_PAGE_SIZE, APAGE_SIZE);
  newmpage->big_page = 0;
  newmpage->size = PREFIX_SIZE;
  pagemap_add_with_size(gc->page_maps, newmpage, GEN0_PAGE_SIZE);

  return newmpage;
}

inline static void gen0_free_mpage(NewGC *gc, mpage *page) {
  pagemap_remove_with_size(gc->page_maps, page, GEN0_PAGE_SIZE);
  free_pages(gc, page->addr, GEN0_PAGE_SIZE);
  free_mpage(page);
}

//#define OVERFLOWS_GEN0(ptr) ((ptr) > (NUM(gc->gen0.curr_alloc_page->addr) + GEN0_PAGE_SIZE))
#define OVERFLOWS_GEN0(ptr) ((ptr) > GC_gen0_alloc_page_end)

inline static size_t gen0_size_in_use(NewGC *gc) {
  return (gc->gen0.current_size + ((GC_gen0_alloc_page_ptr - NUM(gc->gen0.curr_alloc_page->addr)) - PREFIX_SIZE));
}

inline static void *allocate(size_t sizeb, int type)
{
  size_t sizew;
  unsigned long newptr;
  NewGC *gc;

  if(sizeb == 0) return zero_sized;

  sizew = ALIGN_SIZE(( gcBYTES_TO_WORDS(sizeb) + 1));
  if(sizew > MAX_OBJECT_SIZEW)  return allocate_big(sizeb, type);

  sizeb = gcWORDS_TO_BYTES(sizew);

  /* ensure that allocation will fit in a gen0 page */
  newptr = GC_gen0_alloc_page_ptr + sizeb;
  while (OVERFLOWS_GEN0(newptr)) {
    gc = GC_get_GC();
    /* bring page size used up to date */
    gc->gen0.curr_alloc_page->size = GC_gen0_alloc_page_ptr - NUM(gc->gen0.curr_alloc_page->addr);
    gc->gen0.current_size += gc->gen0.curr_alloc_page->size;

    /* try next nursery page if present */
    if(gc->gen0.curr_alloc_page->next) { 
      gc->gen0.curr_alloc_page  = gc->gen0.curr_alloc_page->next;
      GC_gen0_alloc_page_ptr    = NUM(gc->gen0.curr_alloc_page->addr) + gc->gen0.curr_alloc_page->size;
      GC_gen0_alloc_page_end    = NUM(gc->gen0.curr_alloc_page->addr) + GEN0_PAGE_SIZE;
    }
    /* WARNING: tries to avoid a collection but
     * malloc_pages can cause a collection due to check_used_against_max */
    else if (gc->dumping_avoid_collection) {
      mpage *new_mpage = gen0_create_new_mpage(gc);

      /* push page */
      new_mpage->next = gc->gen0.curr_alloc_page;
      new_mpage->next->prev = new_mpage;

      gc->gen0.curr_alloc_page  = new_mpage;
      GC_gen0_alloc_page_ptr    = NUM(new_mpage->addr);
      GC_gen0_alloc_page_end    = NUM(new_mpage->addr) + GEN0_PAGE_SIZE;
    }
    else {
      garbage_collect(gc, 0);
    }
    newptr = GC_gen0_alloc_page_ptr + sizeb;
  } 

  /* actual Allocation */
  {
    struct objhead *info;
    void *retval = PTR(GC_gen0_alloc_page_ptr);

    GC_gen0_alloc_page_ptr = newptr;

    if (type == PAGE_ATOMIC)
      *((void **)retval) = NULL; /* init objhead */
    else
      bzero(retval, sizeb);

    info = (struct objhead *)retval;
    info->type = type;
    info->size = sizew;

    return PTR(NUM(retval) + WORD_SIZE);
  }
}

inline static void *fast_malloc_one_small_tagged(size_t sizeb, int dirty)
{
  unsigned long newptr;

  sizeb += WORD_SIZE;
  sizeb = ALIGN_BYTES_SIZE(sizeb);
  newptr = GC_gen0_alloc_page_ptr + sizeb;

  if(OVERFLOWS_GEN0(newptr)) {
    return GC_malloc_one_tagged(sizeb - WORD_SIZE);
  } else {
    void *retval = PTR(GC_gen0_alloc_page_ptr);

    GC_gen0_alloc_page_ptr = newptr;

    if (dirty)
      *((void **)retval) = NULL; /* init objhead */
    else
      bzero(retval, sizeb);

    ((struct objhead *)retval)->size = (sizeb >> gcLOG_WORD_SIZE);

    return PTR(NUM(retval) + WORD_SIZE);
  }
}

void *GC_malloc_pair(void *car, void *cdr)
{
  unsigned long ptr, newptr;
  size_t sizeb;
  void *retval;

  sizeb = ALIGN_BYTES_SIZE(gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(sizeof(Scheme_Simple_Object))) + WORD_SIZE);
  ptr = GC_gen0_alloc_page_ptr;
  newptr = GC_gen0_alloc_page_ptr + sizeb;

  if(OVERFLOWS_GEN0(newptr)) {
    NewGC *gc = GC_get_GC();
    gc->park[0] = car;
    gc->park[1] = cdr;
    retval = GC_malloc_one_tagged(sizeb - WORD_SIZE);
    car = gc->park[0];
    cdr = gc->park[1];
    gc->park[0] = NULL;
    gc->park[1] = NULL;
  } else {
    struct objhead *info;

    GC_gen0_alloc_page_ptr = newptr;

    retval = PTR(ptr);
    info = (struct objhead *)retval;

    ((void **)retval)[0] = NULL; /* objhead */
    ((void **)retval)[1] = 0;    /* tag word */

    /* info->type = type; */ /* We know that the type field is already 0 */
    info->size = (sizeb >> gcLOG_WORD_SIZE);

    retval = PTR(NUM(retval) + WORD_SIZE);
  }
    
  ((short *)retval)[0] = scheme_pair_type;
  ((void **)retval)[1] = car;
  ((void **)retval)[2] = cdr;
  
  return retval;
}

/* the allocation mechanism we present to the outside world */
void *GC_malloc(size_t s)                         { return allocate(s, PAGE_ARRAY); }
void *GC_malloc_one_tagged(size_t s)              { return allocate(s, PAGE_TAGGED); }
void *GC_malloc_one_xtagged(size_t s)             { return allocate(s, PAGE_XTAGGED); }
void *GC_malloc_array_tagged(size_t s)            { return allocate(s, PAGE_TARRAY); }
void *GC_malloc_atomic(size_t s)                  { return allocate(s, PAGE_ATOMIC); }
void *GC_malloc_atomic_uncollectable(size_t s)    { void *p = ofm_malloc_zero(s); return p; }
void *GC_malloc_allow_interior(size_t s)          { return allocate_big(s, PAGE_ARRAY); }
void *GC_malloc_atomic_allow_interior(size_t s)   { return allocate_big(s, PAGE_ATOMIC); }
void *GC_malloc_tagged_allow_interior(size_t s)   { return allocate_big(s, PAGE_TAGGED); }
void *GC_malloc_one_small_dirty_tagged(size_t s)  { return fast_malloc_one_small_tagged(s, 1); }
void *GC_malloc_one_small_tagged(size_t s)        { return fast_malloc_one_small_tagged(s, 0); }
void GC_free(void *p) {}


long GC_compute_alloc_size(long sizeb)
{
  return ALIGN_BYTES_SIZE(gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(sizeb)) + WORD_SIZE);
}

long GC_initial_word(int sizeb)
{
  long w = 0;
  struct objhead info;

  sizeb = ALIGN_BYTES_SIZE(gcWORDS_TO_BYTES(gcBYTES_TO_WORDS(sizeb)) + WORD_SIZE);

  memset(&info, 0, sizeof(struct objhead));
  info.size = (sizeb >> gcLOG_WORD_SIZE);
  memcpy(&w, &info, sizeof(struct objhead));

  ((struct objhead*)&w)->size = (sizeb >> gcLOG_WORD_SIZE);

  return w;
}

long GC_alloc_alignment()
{
  return APAGE_SIZE;
}

long GC_malloc_stays_put_threshold() { return gcWORDS_TO_BYTES(MAX_OBJECT_SIZEW); }

/* this function resizes generation 0 to the closest it can get (erring high)
   to the size we've computed as ideal */
inline static void resize_gen0(NewGC *gc, unsigned long new_size)
{
  mpage *work = gc->gen0.pages;
  mpage *prev = NULL;
  unsigned long alloced_size = 0;

  /* first, make sure the big pages pointer is clean */
  gc->gen0.big_pages = NULL; 

  /* reset any parts of gen0 we're keeping */
  while(work && (alloced_size < new_size)) {
    alloced_size += GEN0_PAGE_SIZE;
    work->size = PREFIX_SIZE;
    prev = work;
    work = work->next;
  }

  /* if we're short, add more */
  while(alloced_size < new_size) {
    mpage *newpage = gen0_create_new_mpage(gc);

    if(prev)
      prev->next = newpage;
    else gc->gen0.pages = newpage;
    prev = newpage;

    alloced_size += GEN0_PAGE_SIZE;
  }

  /* deallocate any parts left over */
  if (work) {
    prev->next = NULL;

    /* remove the excess pages */
    while(work) {
      struct mpage *next = work->next;
      gen0_free_mpage(gc, work);
      work = next;
    }
  }

  /* we're going to allocate onto the first page now */
  gc->gen0.curr_alloc_page = gc->gen0.pages;
  GC_gen0_alloc_page_ptr = NUM(gc->gen0.curr_alloc_page->addr) + gc->gen0.curr_alloc_page->size;
  GC_gen0_alloc_page_end = NUM(gc->gen0.curr_alloc_page->addr) + GEN0_PAGE_SIZE;

  /* set the two size variables */
  gc->gen0.max_size = alloced_size;
  gc->gen0.current_size = 0;
}

inline static void reset_nursery(NewGC *gc)
{
  unsigned long new_gen0_size; 
  new_gen0_size = NUM((GEN0_SIZE_FACTOR * (float)gc->memory_in_use) + GEN0_SIZE_ADDITION);
  if(new_gen0_size > GEN0_MAX_SIZE)
    new_gen0_size = GEN0_MAX_SIZE;

  resize_gen0(gc, new_gen0_size);
}

/* This procedure fundamentally returns true if a pointer is marked, and
   false if it isn't. This function assumes that you're talking, at this
   point, purely about the mark field of the object. It ignores things like
   the object not being one of our GC heap objects, being in a higher gen
   than we're collectiong, not being a pointer at all, etc. */
inline static int marked(NewGC *gc, void *p)
{
  struct mpage *page;

  if(!p) return 0;
  if(!(page = pagemap_find_page(gc->page_maps, p))) return 1;
  if((NUM(page->addr) + page->previous_size) > NUM(p)) return 1;
  return ((struct objhead *)(NUM(p) - WORD_SIZE))->mark;
}

/*****************************************************************************/
/* Internal Debugging Routines                                               */
/*****************************************************************************/
#ifdef NEWGC_INTERNAL_DEBUGGING
static FILE *dump;
static int collections = 0;

static void init_debug_file(void) 
{
  /*
     char filename_buf[20];
     snprintf(filename_buf, 20, "gclog%d%d", (collections / 10), (collections % 10));
     dump = fopen(filename_buf, "a");
     collections += 1;
     */

  char *filename = ofm_malloc(8 * sizeof(char));

  filename[0] = 'g'; filename[1] = 'c'; filename[2] = 'l';
  filename[3] = 'o'; filename[4] = 'g';
  filename[5] = '0' + (collections / 10);
  filename[6] = '0' + (collections % 10);
  filename[7] = 0;

  dump = fopen(filename, "a");
  collections += 1;
}

static void close_debug_file(void)
{
  fclose(dump);
}

static void dump_region(void **start, void **end)
{
  while(start < end) {
    fprintf(dump, "%.8lx: %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx %.8lx\n", 
        NUM(start), NUM(*start), NUM(*(start + 1)), NUM(*(start + 2)),
        NUM(*(start + 3)), NUM(*(start + 4)), NUM(*(start + 5)), 
        NUM(*(start + 6)), NUM(*(start + 7)));
    start += 8;
  }
  fprintf(dump, "\n\n");
}

static void dump_heap(NewGC *gc)
{
  struct mpage *page;
  short i;

  if(collections >= 0) {
    for(page = gc->gen0.pages; page; page = page->next) {
      fprintf(dump, "Generation 0 Page (%p:%p - %p, size %i):\n", 
          page, page->addr, PTR(NUM(page->addr) + GEN0_PAGE_SIZE), page->size);
      dump_region(PPTR(NUM(page->addr) + PREFIX_SIZE), PPTR(NUM(page->addr) + page->size));
    }
    for(page = gc->gen0.big_pages; page; page = page->next) {
      fprintf(dump, "Page %p:%p (gen %i, type %i, big %i, back %i, size %i)\n",
          page, page->addr, page->generation, page->page_type, page->big_page,
          page->back_pointers, page->size);
      dump_region(PPTR(NUM(page->addr) + PREFIX_SIZE), PPTR(NUM(page->addr) + page->size));
    }
    for(i = 0; i < PAGE_TYPES; i++)
      for(page = gc->gen1_pages[i]; page; page = page->next) {
        fprintf(dump, "Page %p:%p (gen %i, type %i, big %i, back %i, size %i)\n",
            page, page->addr, page->generation, page->page_type, page->big_page,
            page->back_pointers, page->size);
        dump_region(PPTR(NUM(page->addr) + PREFIX_SIZE), PPTR(NUM(page->addr) + page->size));
      }
    fprintf(dump, "STACK:\n");
    dump_region((void*)(NUM(&i) & 0xfffffff0), (void*)(get_stack_base() & 0xfffffff0)); 
    fflush(dump);
  }
}
#endif

#ifdef NEWGC_INTERNAL_DEBUGGING
# define INIT_DEBUG_FILE() init_debug_file()
# define CLOSE_DEBUG_FILE() close_debug_file()
# define DUMP_HEAP() dump_heap()
# define DEBUGOUTF dump
# define GCDEBUG(args) { GCPRINT args; GCFLUSHOUT(); }
#else
# define INIT_DEBUG_FILE() /* */
# define CLOSE_DEBUG_FILE() /* */
# define DUMP_HEAP() /* */
# define GCDEBUG(args) /* */
#endif

#define GCWARN(args) { GCPRINT args; GCFLUSHOUT(); }
#define GCERR(args) { GCPRINT args; GCFLUSHOUT(); abort(); }

/*****************************************************************************/
/* Backtrace                                                                 */
/*****************************************************************************/

#if MZ_GC_BACKTRACE

static void backtrace_new_page(NewGC *gc, mpage *page)
{
  /* This is a little wastefull for big pages, because we'll
     only use the first few words: */
  page->backtrace = (void **)malloc_pages(gc, APAGE_SIZE, APAGE_SIZE);
}

static void free_backtrace(struct mpage *page)
{
  free_pages(page->backtrace, APAGE_SIZE);
}

static void *bt_source;
static int bt_type;

static void set_backtrace_source(void *source, int type)
{
  bt_source = source;
  bt_type = type;
}

static void record_backtrace(struct mpage *page, void *ptr)
  /* ptr is after objhead */
{
  unsigned long delta;

  delta = PPTR(ptr) - PPTR(page->addr);
  page->backtrace[delta - 1] = bt_source;
  ((long *)page->backtrace)[delta] = bt_type;
}

static void copy_backtrace_source(struct mpage *to_page, void *to_ptr,
    struct mpage *from_page, void *from_ptr)
/* ptrs are at objhead */
{
  unsigned long to_delta, from_delta;

  to_delta = PPTR(to_ptr) - PPTR(to_page->addr);
  from_delta = PPTR(from_ptr) - PPTR(from_page->addr);

  to_page->backtrace[to_delta] = from_page->backtrace[from_delta];
  to_page->backtrace[to_delta+1] = from_page->backtrace[from_delta+1];
}

static void *get_backtrace(struct mpage *page, void *ptr)
  /* ptr is after objhead */
{
  unsigned long delta;

  if (page->big_page)
    ptr = PTR((char *)page->addr + PREFIX_SIZE + WORD_SIZE);

  delta = PPTR(ptr) - PPTR(page->addr);
  return page->backtrace[delta - 1];
}


# define BT_STACK      (PAGE_TYPES + 0)
# define BT_ROOT       (PAGE_TYPES + 1)
# define BT_FINALIZER  (PAGE_TYPES + 2)
# define BT_WEAKLINK   (PAGE_TYPES + 3)
# define BT_IMMOBILE   (PAGE_TYPES + 4)

#else
# define backtrace_new_page(gc, page) /* */
# define free_backtrace(page) /* */
# define set_backtrace_source(ptr, type) /* */
# define record_backtrace(page, ptr) /* */
# define copy_backtrace_source(to_page, to_ptr, from_page, from_ptr) /* */
#endif

#define two_arg_no_op(a, b) /* */

/*****************************************************************************/
/* Routines dealing with various runtime execution stacks                    */
/*                                                                           */
/* With the exception of the "traverse" macro and resultant simplification,  */
/* this code is entirely lifted from compact.c                               */
/*****************************************************************************/
THREAD_LOCAL void **GC_variable_stack;

void **GC_get_variable_stack()
{ 
  return GC_variable_stack;
}

void GC_set_variable_stack(void **p)
{
  GC_variable_stack = p;
}

void GC_set_stack_base(void *base) 
{
  NewGC *gc = GC_get_GC();
  gc->stack_base = (unsigned long)base;
}

unsigned long GC_get_stack_base() 
{
  NewGC *gc = GC_get_GC();
  return gc->stack_base;
}

static inline void *get_stack_base(NewGC *gc) {
  if (GC_get_thread_stack_base) return (void*) GC_get_thread_stack_base();
  return (void*) gc->stack_base;
}

#include "stack_comp.c"

#define GC_X_variable_stack GC_mark_variable_stack
#define gcX(a) gcMARK(*a)
#define X_source(stk, p) set_backtrace_source((stk ? stk : p), BT_STACK)
#include "var_stack.c"
#undef GC_X_variable_stack
#undef gcX
#undef X_source

#define GC_X_variable_stack GC_fixup_variable_stack
#define gcX(a) gcFIXUP(*a)
#define X_source(stk, p) /* */
#include "var_stack.c"
#undef GC_X_variable_stack
#undef gcX
#undef X_source

/*****************************************************************************/
/* Routines for root sets                                                    */
/*****************************************************************************/

#include "roots.c"

#define traverse_roots(gcMUCK, set_bt_src) {	                  			    \
  unsigned long j;                                                        \
  Roots *roots = &gc->roots;                                              \
  if(roots->roots) {                                                      \
    sort_and_merge_roots(roots);                                          \
    for(j = 0; j < roots->count; j += 2) {                                \
      void **start = (void**)roots->roots[j];                             \
      void **end = (void**)roots->roots[j+1];                             \
      while(start < end) {                                                \
        set_bt_src(start, BT_ROOT);                                       \
        gcMUCK(*start++);                                                 \
      }                                                                   \
    }                                                                     \
  }                                                                       \
}

inline static void mark_roots(NewGC *gc) 
{
  traverse_roots(gcMARK, set_backtrace_source);
}

inline static void repair_roots(NewGC *gc)
{
  traverse_roots(gcFIXUP, two_arg_no_op);
}

#include "immobile_boxes.c"

/*****************************************************************************/
/* finalizers                                                                */
/*****************************************************************************/

static int is_finalizable_page(NewGC *gc, void *p)
{
  return (pagemap_find_page(gc->page_maps, p) ? 1 : 0);
}

#include "fnls.c"

inline static void mark_finalizer_structs(NewGC *gc)
{
  Fnl *fnl;

  for(fnl = GC_resolve(gc->finalizers); fnl; fnl = GC_resolve(fnl->next)) { 
    set_backtrace_source(fnl, BT_FINALIZER);
    gcMARK(fnl->data); 
    set_backtrace_source(&gc->finalizers, BT_ROOT);
    gcMARK(fnl);
  }
  for(fnl = gc->run_queue; fnl; fnl = fnl->next) {
    set_backtrace_source(fnl, BT_FINALIZER);
    gcMARK(fnl->data);
    gcMARK(fnl->p);
    set_backtrace_source(&gc->run_queue, BT_ROOT);
    gcMARK(fnl);
  }
}  

inline static void repair_finalizer_structs(NewGC *gc)
{
  Fnl *fnl;

  /* repair the base parts of the list */
  gcFIXUP(gc->finalizers); gcFIXUP(gc->run_queue);
  /* then repair the stuff inside them */
  for(fnl = gc->finalizers; fnl; fnl = fnl->next) {
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->p);
    gcFIXUP(fnl->next);
  }
  for(fnl = gc->run_queue; fnl; fnl = fnl->next) {
    gcFIXUP(fnl->data);
    gcFIXUP(fnl->p);
    gcFIXUP(fnl->next);
  }
}

inline static void check_finalizers(NewGC *gc, int level)
{
  Fnl *work = GC_resolve(gc->finalizers);
  Fnl *prev = NULL;

  GCDEBUG((DEBUGOUTF, "CFNL: Checking level %i finalizers\n", level));
  while(work) {
    if((work->eager_level == level) && !marked(gc, work->p)) {
      struct finalizer *next = GC_resolve(work->next);

      GCDEBUG((DEBUGOUTF, 
            "CFNL: Level %i finalizer %p on %p queued for finalization.\n",
            work->eager_level, work, work->p));
      set_backtrace_source(work, BT_FINALIZER);
      gcMARK(work->p);
      if(prev) prev->next = next;
      if(!prev) gc->finalizers = next;
      if(gc->last_in_queue) gc->last_in_queue = gc->last_in_queue->next = work;
      if(!gc->last_in_queue) gc->run_queue = gc->last_in_queue = work;
      work->next = NULL;
      --gc->num_fnls;

      work = next;
    } else { 
      GCDEBUG((DEBUGOUTF, "CFNL: Not finalizing %p (level %i on %p): %p / %i\n",
            work, work->eager_level, work->p, pagemap_find_page(gc->page_maps, work->p),
            marked(work->p)));
      prev = work; 
      work = GC_resolve(work->next); 
    }
  }
}

inline static void do_ordered_level3(NewGC *gc)
{
  struct finalizer *temp;
  Mark_Proc *mark_table = gc->mark_table;

  for(temp = GC_resolve(gc->finalizers); temp; temp = GC_resolve(temp->next))
    if(!marked(gc, temp->p)) {
      GCDEBUG((DEBUGOUTF,
            "LVL3: %p is not marked. Marking payload (%p)\n", 
            temp, temp->p));
      set_backtrace_source(temp, BT_FINALIZER);
      if(temp->tagged) mark_table[*(unsigned short*)temp->p](temp->p);
      if(!temp->tagged) GC_mark_xtagged(temp->p);
    }
}

void GC_finalization_weak_ptr(void **p, int offset)
{
  NewGC *gc = GC_get_GC();
  Weak_Finalizer *wfnl;

  gc->park[0] = p; wfnl = GC_malloc_atomic(sizeof(Weak_Finalizer));
  p = gc->park[0]; gc->park[0] = NULL;
  wfnl->p = p; wfnl->offset = offset * sizeof(void*); wfnl->saved = NULL;
  wfnl->next = gc->weak_finalizers; gc->weak_finalizers = wfnl;
}

inline static void mark_weak_finalizer_structs(NewGC *gc)
{
  Weak_Finalizer *work;

  GCDEBUG((DEBUGOUTF, "MARKING WEAK FINALIZERS.\n"));
  for(work = gc->weak_finalizers; work; work = work->next) {
    set_backtrace_source(&gc->weak_finalizers, BT_ROOT);
    gcMARK(work);
  }
}

inline static void repair_weak_finalizer_structs(NewGC *gc)
{
  Weak_Finalizer *work;
  Weak_Finalizer *prev;

  gcFIXUP(gc->weak_finalizers);
  work = gc->weak_finalizers; prev = NULL;
  while(work) {
    gcFIXUP(work->next);
    if(!marked(gc, work->p)) {
      if(prev) prev->next = work->next;
      if(!prev) gc->weak_finalizers = work->next;
      work = GC_resolve(work->next);
    } else {
      gcFIXUP(work->p);
      prev = work;
      work = work->next;
    }
  }
}

inline static void zero_weak_finalizers(NewGC *gc)
{
  Weak_Finalizer *wfnl;

  for(wfnl = GC_resolve(gc->weak_finalizers); wfnl; wfnl = GC_resolve(wfnl->next)) {
    wfnl->saved = *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset);
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = NULL;
  }
}

inline static void reset_weak_finalizers(NewGC *gc)
{
  Weak_Finalizer *wfnl;

  for(wfnl = GC_resolve(gc->weak_finalizers); wfnl; wfnl = GC_resolve(wfnl->next)) {
    if(marked(gc, wfnl->p)) {
      set_backtrace_source(wfnl, BT_WEAKLINK);
      gcMARK(wfnl->saved); 
    }
    *(void**)(NUM(GC_resolve(wfnl->p)) + wfnl->offset) = wfnl->saved;
    wfnl->saved = NULL;
  }
}

/*****************************************************************************/
/* weak boxes and arrays                                                     */
/*****************************************************************************/

#define is_marked(gc, p) marked(gc, p)
#define weak_box_resolve(p) GC_resolve(p)
#include "weak.c"
#undef is_marked
#undef weak_box_resolve

/*****************************************************************************/
/* Internal Stack Routines                                                   */
/*****************************************************************************/

/* This is the code we use to implement the mark stack. We can't, sadly, use
   the standard C stack because we'll blow it; propagation makes for a *very*
   deep stack. So we use this instead. */
typedef struct MarkSegment {
  struct MarkSegment *prev;
  struct MarkSegment *next;
  void **top;
  void **end;
  void **stop_here; /* this is only used for its address */
} MarkSegment;

static THREAD_LOCAL MarkSegment *mark_stack = NULL;

inline static MarkSegment* mark_stack_create_frame() {
  MarkSegment *mark_frame = (MarkSegment*)ofm_malloc(STACK_PART_SIZE);
  mark_frame->next = NULL;
  mark_frame->top  = PPTR(&(mark_frame->stop_here));
  mark_frame->end  = PPTR(NUM(mark_frame) + STACK_PART_SIZE);
  return mark_frame;
}

inline static void push_ptr(void *ptr)
{
  /* This happens at the very beginning */
  if(!mark_stack) {
    mark_stack = mark_stack_create_frame();
    mark_stack->prev = NULL;
  }

  /* This happens during propoagation if we go past the end of this MarkSegment*/
  if(mark_stack->top == mark_stack->end) {
    /* test to see if we already have another stack page ready */
    if(mark_stack->next) {
      /* we do, so just use it */
      mark_stack = mark_stack->next;
      mark_stack->top = PPTR(&(mark_stack->stop_here));
    } else {
      /* we don't, so we need to allocate one */
      mark_stack->next = mark_stack_create_frame();
      mark_stack->next->prev = mark_stack;
      mark_stack = mark_stack->next;
    }
  }

  /* at this point, we're guaranteed to be good to push pointers */
  *(mark_stack->top++) = ptr;
}

inline static int pop_ptr(void **ptr)
{
  if(mark_stack->top == PPTR(&mark_stack->stop_here)) {
    if(mark_stack->prev) {
      /* if there is a previous page, go to it */
      mark_stack = mark_stack->prev;
    } else {
      /* if there isn't a previous page, then we've hit the bottom of the stack */
      return 0;
    }
  }

  /* if we get here, we're guaranteed to have data */
  *ptr = *(--mark_stack->top);
  return 1;
}

inline static void clear_stack_pages(void)
{
  if(mark_stack) {
    MarkSegment *temp;
    MarkSegment *base;
    int keep = 2;

    /* go to the head of the list */
    for(; mark_stack->prev; mark_stack = mark_stack->prev) {}
    /* then go through and clear them out */
    base = mark_stack;
    for(; mark_stack; mark_stack = temp) {
      temp = mark_stack->next;
      if(keep) { 
        keep--; 
        if (!keep)
          mark_stack->next = NULL;
      } else 
        free(mark_stack);
    }
    mark_stack = base;
    mark_stack->top = PPTR(mark_stack) + 4;
  }
}

inline static void reset_pointer_stack(void)
{
  /* go to the head of the list */
  for(; mark_stack->prev; mark_stack = mark_stack->prev) {}
  /* reset the stack */
  mark_stack->top = PPTR(mark_stack) + 4;
}

/*****************************************************************************/
/* BLAME THE CHILD                                                           */
/*****************************************************************************/

#ifdef NEWGC_BTC_ACCOUNT
# include "blame_the_child.c"
#else
# define clean_up_thread_list() /* */
#endif

void GC_register_root_custodian(void *c)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_register_root_custodian(c);
#endif
}

int GC_set_account_hook(int type, void *c1, unsigned long b, void *c2)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_add_account_hook(type, c1, c2, b); 
  return 1;
#else
  return 0;
#endif
}

void GC_register_thread(void *t, void *c)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_register_thread(t, c);
#endif
}
void GC_register_new_thread(void *t, void *c)
{
#ifdef NEWGC_BTC_ACCOUNT
  BTC_register_new_thread(t, c);
#endif
}

/*****************************************************************************/
/* administration / initialization                                           */
/*****************************************************************************/

static int designate_modified(void *p)
{
  NewGC *gc = GC_get_GC();
  struct mpage *page = pagemap_find_page(gc->page_maps, p);

  if (gc->no_further_modifications) {
    GCPRINT(GCOUTF, "Seg fault (internal error during gc) at %p\n", p);
    return 0;
  }

  if(page) {
    if (!page->back_pointers) {
      page->mprotected = 0;
      vm_protect_pages(page->addr, page->big_page ? round_to_apage_size(page->size) : APAGE_SIZE, 1);
      page->back_pointers = 1;
      return 1;
    }
  } else {
    GCPRINT(GCOUTF, "Seg fault (internal error) at %p\n", p);
  }
  return 0;
}

void GC_write_barrier(void *p) 
{
  (void)designate_modified(p);
}

#include "sighand.c"

void NewGC_initialize(NewGC *newgc) {
  memset(newgc, 0, sizeof(NewGC));
  newgc->mark_table = ofm_malloc_zero(NUMBER_OF_TAGS * sizeof (Mark_Proc)); 
  newgc->fixup_table = ofm_malloc_zero(NUMBER_OF_TAGS * sizeof (Fixup_Proc)); 
#ifdef SIXTY_FOUR_BIT_INTEGERS
  newgc->page_maps = ofm_malloc_zero(PAGEMAP64_LEVEL1_SIZE * sizeof (mpage***)); 
#else
  newgc->page_maps = ofm_malloc_zero(PAGEMAP32_SIZE * sizeof (mpage*)); 
#endif
  newgc->vm = vm_create();
  newgc->protect_range = ofm_malloc_zero(sizeof(Page_Range));
  
  newgc->generations_available = 1;
  newgc->last_full_mem_use = (20 * 1024 * 1024);
  newgc->new_btc_mark = 1;
}

void GC_init_type_tags(int count, int pair, int mutable_pair, int weakbox, int ephemeron, int weakarray, int custbox)
{
  static int initialized = 0;

  if(!initialized) {
    NewGC *gc;
    initialized = 1;

    gc = ofm_malloc(sizeof(NewGC));
    GC = gc;
    NewGC_initialize(gc);

    gc->weak_box_tag    = weakbox;
    gc->ephemeron_tag   = ephemeron;
    gc->weak_array_tag  = weakarray;
# ifdef NEWGC_BTC_ACCOUNT
    gc->cust_box_tag    = custbox;
# endif

    /* Our best guess at what the OS will let us allocate: */
    gc->max_pages_in_heap = determine_max_heap_size() / APAGE_SIZE;
    /* Not all of that memory is available for allocating GCable
       objects.  There's the memory used by the stack, code,
       malloc()/free()ed memory, etc., and there's also the
       administrative structures for the GC itself. */
    gc->max_pages_for_use = gc->max_pages_in_heap / 2;

    resize_gen0(gc, GEN0_INITIAL_SIZE);

    GC_register_traversers(gc->weak_box_tag, size_weak_box, mark_weak_box, fixup_weak_box, 0, 0);
    GC_register_traversers(gc->ephemeron_tag, size_ephemeron, mark_ephemeron, fixup_ephemeron, 0, 0);
    GC_register_traversers(gc->weak_array_tag, size_weak_array, mark_weak_array, fixup_weak_array, 0, 0);
    initialize_signal_handler(gc);
    GC_add_roots(&gc->park, (char *)&gc->park + sizeof(gc->park) + 1);
    GC_add_roots(&gc->park_save, (char *)&gc->park_save + sizeof(gc->park_save) + 1);

    initialize_protect_page_ranges(gc->protect_range, malloc_dirty_pages(gc, APAGE_SIZE, APAGE_SIZE), APAGE_SIZE);
  }
  else {
    GCPRINT(GCOUTF, "HEY WHATS UP.\n");
    abort();
  }
}

void GC_gcollect(void)
{
  NewGC *gc = GC_get_GC();
  garbage_collect(gc, 1);
}

void GC_register_traversers(short tag, Size_Proc size, Mark_Proc mark,
    Fixup_Proc fixup, int constant_Size, int atomic)
{
  NewGC *gc = GC_get_GC();
  gc->mark_table[tag]  = atomic ? (Mark_Proc)PAGE_ATOMIC : mark;
  gc->fixup_table[tag] = fixup;
}

long GC_get_memory_use(void *o) 
{
  NewGC *gc = GC_get_GC();
#ifdef NEWGC_BTC_ACCOUNT
  if(o) {
    return BTC_get_memory_use(gc, o);
  }
#endif
  return gen0_size_in_use(gc) + gc->memory_in_use;
}

/*****************************************************************************/
/* Garbage collection proper ... and all the mess therein                    */
/*****************************************************************************/

/* We use two mark routines to handle propagation. Why two? The first is the
   one that we export out, and it does a metric crapload of work. The second
   we use internally, and it doesn't do nearly as much. */

/* This is the first mark routine. It's a bit complicated. */
void GC_mark(const void *const_p)
{
  mpage *page;
  void *p = (void*)const_p;
  NewGC *gc;

  if(!p || (NUM(p) & 0x1)) {
    GCDEBUG((DEBUGOUTF, "Not marking %p (bad ptr)\n", p));
    return;
  }

  gc = GC_get_GC();
  if(!(page = pagemap_find_page(gc->page_maps, p))) {
    GCDEBUG((DEBUGOUTF,"Not marking %p (no page)\n",p));
    return;
  }

  /* toss this over to the BTC mark routine if we're doing accounting */
  if(gc->doing_memory_accounting) { 
#ifdef NEWGC_BTC_ACCOUNT
    BTC_memory_account_mark(gc, page, p); return; 
#endif
  }

  if(page->big_page) {
    /* This is a bigpage. The first thing we do is see if its been marked
       previously */
    if(page->big_page != 1) {
      GCDEBUG((DEBUGOUTF, "Not marking %p on big %p (already marked)\n", p, page));
      return;
    }
    /* in this case, it has not. So we want to mark it, first off. */
    page->big_page = 2;

    /* if this is in the nursery, we want to move it out of the nursery */
    if(!page->generation) {
      page->generation = 1;

      /* remove page */
      if(page->prev) page->prev->next = page->next; else
        gc->gen0.big_pages = page->next;
      if(page->next) page->next->prev = page->prev;

      backtrace_new_page(gc, page);

      /* add to gen1 */
      page->next = gc->gen1_pages[PAGE_BIG]; 
      page->prev = NULL;
      if(page->next) page->next->prev = page;
      gc->gen1_pages[PAGE_BIG] = page;

      /* if we're doing memory accounting, then we need to make sure the
         btc_mark is right */
#ifdef NEWGC_BTC_ACCOUNT
        BTC_set_btc_mark(gc, PTR(NUM(page->addr) + PREFIX_SIZE));
#endif
    }

    page->marked_on = 1;
    record_backtrace(page, PTR(NUM(page->addr) + PREFIX_SIZE + WORD_SIZE));
    GCDEBUG((DEBUGOUTF, "Marking %p on big page %p\n", p, page));
    /* Finally, we want to add this to our mark queue, so we can 
       propagate its pointers */
    push_ptr(p);
  }
  else {
    struct objhead *ohead = (struct objhead *)(NUM(p) - WORD_SIZE);

    if(ohead->mark) {
      GCDEBUG((DEBUGOUTF,"Not marking %p (already marked)\n", p));
      return;
    }

    /* what we do next depends on whether this is a gen0 or gen1 
       object */
    if(page->generation) {
      /* this is a generation 1 object. This means we are not going
         to move it, we don't have to check to see if it's an atomic
         object masquerading as a tagged object, etc. So all we do
         is add the pointer to the mark queue and note on the page
         that we marked something on it*/
      if((NUM(page->addr) + page->previous_size) <= NUM(p)) {
        GCDEBUG((DEBUGOUTF, "Marking %p (leaving alone)\n", p));
        ohead->mark = 1;
        page->marked_on = 1;
        page->previous_size = PREFIX_SIZE;
        page->live_size += ohead->size;
        record_backtrace(page, p);
        push_ptr(p);
      } else GCDEBUG((DEBUGOUTF, "Not marking %p (it's old; %p / %i)\n",
            p, page, page->previous_size));
    } else {
      /* this is a generation 0 object. This means that we do have
         to do all of the above. Fun, fun, fun. */
      unsigned short type = ohead->type;
      struct mpage *work;
      size_t size;
      void *newplace;

      /* first check to see if this is an atomic object masquerading
         as a tagged object; if it is, then convert it */
      if(type == PAGE_TAGGED) {
        if((unsigned long)gc->mark_table[*(unsigned short*)p] < PAGE_TYPES)
          type = ohead->type = (int)(unsigned long)gc->mark_table[*(unsigned short*)p];
      }

      /* now set us up for the search for where to put this thing */
      work = gc->gen1_pages[type];
      size = gcWORDS_TO_BYTES(ohead->size);

      /* search for a page with the space to spare */
      if (work && ((work->size + size) >= APAGE_SIZE))
        work = NULL;

      /* now either fetch where we're going to put this object or make
         a new page if we couldn't find a page with space to spare */
      if(work) {
        pagemap_add(gc->page_maps, work);
        work->marked_on = 1;
        if (work->mprotected) {
          work->mprotected = 0;
          vm_protect_pages(work->addr, APAGE_SIZE, 1);
        }
        newplace = PTR(NUM(work->addr) + work->size);
      } else {
        /* Allocate and prep the page */
        work = malloc_mpage();
        work->addr = malloc_dirty_pages(gc, APAGE_SIZE, APAGE_SIZE);
        work->generation = 1;
        work->page_type = type;
        work->size = work->previous_size = PREFIX_SIZE;
        work->marked_on = 1;
        backtrace_new_page(gc, work);
        work->next = gc->gen1_pages[type];
        work->prev = NULL;
        if(work->next)
          work->next->prev = work;
        pagemap_add(gc->page_maps, work);
        gc->gen1_pages[type] = work;
        newplace = PTR(NUM(work->addr) + PREFIX_SIZE);
      }

      /* update the size */
      work->size += size;
      work->has_new = 1;

      /* transfer the object */
      memcpy(newplace, (const void *)ohead, size);
      /* mark the old location as marked and moved, and the new location
         as marked */
      ohead->mark = ohead->moved = 1;
      ((struct objhead *)newplace)->mark = 1;
      /* if we're doing memory accounting, then we need the btc_mark
         to be set properly */
#ifdef NEWGC_BTC_ACCOUNT
      BTC_set_btc_mark(gc, newplace);
#endif
      /* drop the new location of the object into the forwarding space
         and into the mark queue */
      newplace = PTR(NUM(newplace) + WORD_SIZE);
      /* record why we marked this one (if enabled) */
      record_backtrace(work, newplace);
      /* set forwarding pointer */
      GCDEBUG((DEBUGOUTF,"Marking %p (moved to %p on page %p)\n", 
            p, newplace, work));
      *(void**)p = newplace;
      push_ptr(newplace);
    }
  }
}

/* this is the second mark routine. It's not quite as complicated. */
/* this is what actually does mark propagation */
static void propagate_marks(NewGC *gc) 
{
  void *p;
  PageMap pagemap = gc->page_maps;
  Mark_Proc *mark_table = gc->mark_table;

  while(pop_ptr(&p)) {
    struct mpage *page = pagemap_find_page(pagemap, p);
    GCDEBUG((DEBUGOUTF, "Popped pointer %p\n", p));

    /* we can assume a lot here -- like it's a valid pointer with a page --
       because we vet bad cases out in GC_mark, above */
    if(page->big_page) {
      void **start = PPTR(NUM(page->addr) + PREFIX_SIZE + WORD_SIZE);
      void **end = PPTR(NUM(page->addr) + page->size);

      set_backtrace_source(start, page->page_type);

      switch(page->page_type) {
        case PAGE_TAGGED: 
          {
            unsigned short tag = *(unsigned short*)start;
            if((unsigned long)mark_table[tag] < PAGE_TYPES) {
              /* atomic */
            } else
              mark_table[tag](start); break;
          }
        case PAGE_ATOMIC: break;
        case PAGE_ARRAY: while(start < end) gcMARK(*(start++)); break;
        case PAGE_XTAGGED: GC_mark_xtagged(start); break;
        case PAGE_TARRAY: {
                            unsigned short tag = *(unsigned short *)start;
                            end -= INSET_WORDS;
                            while(start < end) start += mark_table[tag](start);
                            break;
                          }
      }
    } else {
      struct objhead *info = (struct objhead *)(NUM(p) - WORD_SIZE);

      set_backtrace_source(p, info->type);

      switch(info->type) {
        case PAGE_TAGGED: mark_table[*(unsigned short*)p](p); break;
        case PAGE_ATOMIC: break;
        case PAGE_ARRAY: {
                           void **start = p;
                           void **end = PPTR(info) + info->size;
                           while(start < end) gcMARK(*start++);
                           break;
                         }
        case PAGE_TARRAY: {
                            void **start = p;
                            void **end = PPTR(info) + (info->size - INSET_WORDS);
                            unsigned short tag = *(unsigned short *)start;
                            while(start < end) start += mark_table[tag](start);
                            break;
                          }
        case PAGE_XTAGGED: GC_mark_xtagged(p); break;
      }
    }
  }
}

void *GC_resolve(void *p)
{
  NewGC *gc = GC_get_GC();
  struct mpage *page = pagemap_find_page(gc->page_maps, p);
  struct objhead *info;

  if(!page || page->big_page)
    return p;

  info = (struct objhead *)(NUM(p) - WORD_SIZE);
  if(info->mark && info->moved) 
    return *(void**)p;
  else 
    return p;
}

void *GC_fixup_self(void *p)
{
  return p;
}

void GC_fixup(void *pp)
{
  NewGC *gc;
  struct mpage *page;
  void *p = *(void**)pp;

  if(!p || (NUM(p) & 0x1))
    return;

  gc = GC_get_GC();
  if((page = pagemap_find_page(gc->page_maps, p))) {
    struct objhead *info;

    if(page->big_page) return;
    info = (struct objhead *)(NUM(p) - WORD_SIZE);
    if(info->mark && info->moved) 
      *(void**)pp = *(void**)p;
    else GCDEBUG((DEBUGOUTF, "Not repairing %p from %p (not moved)\n",p,pp));
  } else GCDEBUG((DEBUGOUTF, "Not repairing %p from %p (no page)\n", p, pp));
}

/*****************************************************************************/
/* memory stats and traces                                                   */
/*****************************************************************************/

#ifdef MZ_GC_BACKTRACE
# define trace_page_t struct mpage
# define trace_page_type(page) (page)->page_type
  static void *trace_pointer_start(struct mpage *page, void *p) { 
    if (page->big_page) 
      return PTR(NUM(page->addr) + PREFIX_SIZE + WORD_SIZE); 
    else 
      return p; 
  }
# define TRACE_PAGE_TAGGED PAGE_TAGGED
# define TRACE_PAGE_ARRAY PAGE_ARRAY
# define TRACE_PAGE_TAGGED_ARRAY PAGE_TARRAY
# define TRACE_PAGE_ATOMIC PAGE_ATOMIC
# define TRACE_PAGE_XTAGGED PAGE_XTAGGED
# define TRACE_PAGE_MALLOCFREE PAGE_TYPES
# define TRACE_PAGE_BAD PAGE_TYPES
# define trace_page_is_big(page) (page)->big_page
# define trace_backpointer get_backtrace
# include "backtrace.c"
#else
# define reset_object_traces() /* */
# define register_traced_object(p) /* */
# define print_traced_objects(x, y, q, z) /* */
#endif

#define MAX_DUMP_TAG 256

void GC_dump_with_traces(int flags,
    GC_get_type_name_proc get_type_name,
    GC_get_xtagged_name_proc get_xtagged_name,
    GC_for_each_found_proc for_each_found,
    short trace_for_tag,
    GC_print_tagged_value_proc print_tagged_value,
    int path_length_limit)
{
  NewGC *gc = GC_get_GC();
  mpage *page;
  int i;
  static unsigned long counts[MAX_DUMP_TAG], sizes[MAX_DUMP_TAG];

  reset_object_traces();
  if (for_each_found)
    gc->dumping_avoid_collection++;

  /* Traverse tagged pages to count objects: */
  for (i = 0; i < MAX_DUMP_TAG; i++) {
    counts[i] = sizes[i] = 0;
  }
  for (page = gc->gen1_pages[PAGE_TAGGED]; page; page = page->next) {
    void **start = PPTR(NUM(page->addr) + PREFIX_SIZE);
    void **end = PPTR(NUM(page->addr) + page->size);

    while(start < end) {
      struct objhead *info = (struct objhead *)start;
      if(!info->dead) {
        unsigned short tag = *(unsigned short *)(start + 1);
        if (tag < MAX_DUMP_TAG) {
          counts[tag]++;
          sizes[tag] += info->size;
        }
        if (tag == trace_for_tag) {
          register_traced_object(start + 1);
          if (for_each_found)
            for_each_found(start + 1);
        }
      }
      start += info->size;
    }
  }
  for (page = gc->gen1_pages[PAGE_BIG]; page; page = page->next) {
    if (page->page_type == PAGE_TAGGED) {
      void **start = PPTR(NUM(page->addr) + PREFIX_SIZE);
      unsigned short tag = *(unsigned short *)(start + 1);
      if (tag < MAX_DUMP_TAG) {
        counts[tag]++;
        sizes[tag] += gcBYTES_TO_WORDS(page->size);
      }
      if ((tag == trace_for_tag)
          || (tag == -trace_for_tag)) {
        register_traced_object(start + 1);
        if (for_each_found)
          for_each_found(start + 1);
      }
    }
  }

  GCPRINT(GCOUTF, "Begin MzScheme3m\n");
  for (i = 0; i < MAX_DUMP_TAG; i++) {
    if (counts[i]) {
      char *tn, buf[256];
      if (get_type_name)
        tn = get_type_name((Type_Tag)i);
      else
        tn = NULL;
      if (!tn) {
        sprintf(buf, "unknown,%d", i);
        tn = buf;
      }
      GCPRINT(GCOUTF, "  %20.20s: %10ld %10ld\n", tn, counts[i], gcWORDS_TO_BYTES(sizes[i]));
    }
  }
  GCPRINT(GCOUTF, "End MzScheme3m\n");

  GCWARN((GCOUTF, "Generation 0: %lu of %li bytes used\n", (unsigned long) gen0_size_in_use(gc), gc->gen0.max_size));

  for(i = 0; i < PAGE_TYPES; i++) {
    unsigned long total_use = 0, count = 0;

    for(page = gc->gen1_pages[i]; page; page = page->next) {
      total_use += page->size;
      count++;
    }
    GCWARN((GCOUTF, "Generation 1 [%s]: %li bytes used in %li pages\n", 
          type_name[i], total_use, count));
  }

  GCWARN((GCOUTF,"\n"));
  GCWARN((GCOUTF,"Current memory use: %li\n", GC_get_memory_use(NULL)));
  GCWARN((GCOUTF,"Peak memory use after a collection: %li\n", gc->peak_memory_use));
  GCWARN((GCOUTF,"Allocated (+reserved) page sizes: %li (+%li)\n", 
        gc->used_pages * APAGE_SIZE, 
        vm_memory_allocated(gc->vm) - (gc->used_pages * APAGE_SIZE)));
  GCWARN((GCOUTF,"# of major collections: %li\n", gc->num_major_collects));
  GCWARN((GCOUTF,"# of minor collections: %li\n", gc->num_minor_collects));
  GCWARN((GCOUTF,"# of installed finalizers: %i\n", gc->num_fnls));
  GCWARN((GCOUTF,"# of traced ephemerons: %i\n", gc->num_last_seen_ephemerons));

  if (flags & GC_DUMP_SHOW_TRACE) {
    print_traced_objects(path_length_limit, get_type_name, get_xtagged_name, print_tagged_value);
  }

  if (for_each_found)
    --gc->dumping_avoid_collection;
}

void GC_dump(void)
{
  GC_dump_with_traces(0, NULL, NULL, NULL, 0, NULL, 0);
}

#ifdef MZ_GC_BACKTRACE

int GC_is_tagged(void *p)
{
  NewGC *gc = GC_get_GC();
  struct mpage *page;
  page = pagemap_find_page(gc->page_maps, p);
  return page && (page->page_type == PAGE_TAGGED);
}

int GC_is_tagged_start(void *p)
{
  return 0;
}

void *GC_next_tagged_start(void *p)
{
  return NULL;
}

#endif

/*****************************************************************************/
/* garbage collection                                                        */
/*****************************************************************************/

static void prepare_pages_for_collection(NewGC *gc)
{
  Page_Range *protect_range = gc->protect_range;
  struct mpage *work;
  int i;

  GCDEBUG((DEBUGOUTF, "PREPPING PAGES.\n"));
  if(gc->gc_full) {
    /* we need to make sure that previous_size for every page is reset, so
       we don't accidentally screw up the mark routine */
    if (gc->generations_available) {
      for(i = 0; i < PAGE_TYPES; i++)
        for(work = gc->gen1_pages[i]; work; work = work->next) {
          if (work->mprotected) {
            work->mprotected = 0;
            add_protect_page_range(protect_range, work->addr, work->big_page ? round_to_apage_size(work->size) : APAGE_SIZE, APAGE_SIZE, 1);
          }
        }
      flush_protect_page_ranges(protect_range, 1);
    }
    for(i = 0; i < PAGE_TYPES; i++)
      for(work = gc->gen1_pages[i]; work; work = work->next) {
        work->live_size = 0;
        work->previous_size = PREFIX_SIZE;
      }
  } else {
    /* if we're not doing a major collection, then we need to remove all the
       pages in gc->gen1_pages[] from the page map */
    PageMap pagemap = gc->page_maps;
    for(i = 0; i < PAGE_TYPES; i++)
      for(work = gc->gen1_pages[i]; work; work = work->next) {
        if (gc->generations_available) {
          if (work->back_pointers) {
            if (work->mprotected) {
              work->mprotected = 0;
              add_protect_page_range(protect_range, work->addr, work->big_page ? round_to_apage_size(work->size) : APAGE_SIZE, APAGE_SIZE, 1);
            }
          }
        }
        pagemap_remove(pagemap, work);
      }
    flush_protect_page_ranges(protect_range, 1);
  }
}

static void mark_backpointers(NewGC *gc)
{
  if(!gc->gc_full) {
    struct mpage *work;
    int i;
    PageMap pagemap = gc->page_maps;

    /* if this is not a full collection, then we need to mark any pointers
       which point backwards into generation 0, since they're roots. */
    for(i = 0; i < PAGE_TYPES; i++) {
      for(work = gc->gen1_pages[i]; work; work = work->next) {
        if(work->back_pointers) {
          /* these pages are guaranteed not to be write protected, because
             if they were, they wouldn't have this bit set */
          work->marked_on = 1;
          work->previous_size = PREFIX_SIZE;
          pagemap_add(pagemap, work);
          if(work->big_page) {
            work->big_page = 2;
            push_ptr(PPTR(NUM(work->addr) + PREFIX_SIZE));
          } else {
            if(work->page_type != PAGE_ATOMIC) {
              void **start = PPTR(NUM(work->addr) + PREFIX_SIZE);
              void **end = PPTR(NUM(work->addr) + work->size);

              while(start < end) {
                struct objhead *info = (struct objhead *)start;
                if(!info->dead) {
                  info->mark = 1;
                  /* This must be a push_ptr, and not a direct call to
                     internal_mark. This is because we need every object
                     in the older heap to be marked out of and noted as
                     marked before we do anything else */
                  push_ptr(start + 1);
                }
                start += info->size;
              }
            }
          }
          work->previous_size = PREFIX_SIZE;
        } else {
          GCDEBUG((DEBUGOUTF,"Setting previous_size on %p to %i\n", work,
                work->size));
          work->previous_size = work->size;
        }
      }
    }
  }
}

struct mpage *allocate_compact_target(NewGC *gc, mpage *work)
{
  mpage *npage;

  npage = malloc_mpage();
  npage->addr = malloc_dirty_pages(gc, APAGE_SIZE, APAGE_SIZE);
  npage->previous_size = npage->size = PREFIX_SIZE;
  npage->generation = 1;
  npage->back_pointers = 0;
  npage->big_page = 0;
  npage->page_type = work->page_type;
  npage->marked_on = 1;
  backtrace_new_page(gc, npage);
  /* Link in this new replacement page */
  npage->prev = work;
  npage->next = work->next;
  work->next = npage;
  if (npage->next)
    npage->next->prev = npage;

  return npage;
}

/* Compact when 1/4 of the space between objects is unused: */
#define should_compact_page(lsize,tsize) (lsize < (tsize - PREFIX_SIZE - (APAGE_SIZE >> 2)))

inline static void do_heap_compact(NewGC *gc)
{
  int i;
  PageMap pagemap = gc->page_maps;

  for(i = 0; i < PAGE_BIG; i++) {
    struct mpage *work = gc->gen1_pages[i], *prev, *npage;

    /* Start from the end: */
    if (work) {
      while (work->next)
        work = work->next;
    }
    npage = work;

    while(work) {
      if(work->marked_on && !work->has_new) {
        /* then determine if we actually want to do compaction */
        if(should_compact_page(gcWORDS_TO_BYTES(work->live_size),work->size)) {
          void **start = PPTR(NUM(work->addr) + PREFIX_SIZE);
          void **end = PPTR(NUM(work->addr) + work->size);
          void **newplace;
          unsigned long avail;

          GCDEBUG((DEBUGOUTF, "Compacting page %p: new version at %p\n", 
                work, npage));

          if (npage == work) {
            /* Need to insert a page: */
            npage = allocate_compact_target(gc, work);
          }
          avail = gcBYTES_TO_WORDS(APAGE_SIZE - npage->size);
          newplace = PPTR(NUM(npage->addr) + npage->size);

          while(start < end) {
            struct objhead *info;

            info = (struct objhead *)start;

            if(info->mark) {
              while (avail <= info->size) {
                npage->size = NUM(newplace) - NUM(npage->addr);
                do {
                  npage = npage->prev;
                } while (!npage->marked_on || npage->has_new);
                if (npage == work)
                  npage = allocate_compact_target(gc, work);
                avail = gcBYTES_TO_WORDS(APAGE_SIZE - npage->size);
                newplace = PPTR(NUM(npage->addr) + npage->size);
              }

              if (npage->mprotected) {
                npage->mprotected = 0;
                vm_protect_pages(npage->addr, APAGE_SIZE, 1);
              }

              GCDEBUG((DEBUGOUTF,"Moving size %i object from %p to %p\n",
                    gcWORDS_TO_BYTES(info->size), start+1, newplace+1));
              memcpy(newplace, start, gcWORDS_TO_BYTES(info->size));
              info->moved = 1;
              *(PPTR(NUM(start) + WORD_SIZE)) = PTR(NUM(newplace) + WORD_SIZE);
              copy_backtrace_source(npage, newplace, work, start);
              newplace += info->size;
              avail -= info->size;
            }
            start += info->size;	    
          }
          npage->size = NUM(newplace) - NUM(npage->addr);

          prev = work->prev;

          if(prev) prev->next = work->next; else gc->gen1_pages[i] = work->next;
          if(work->next) work->next->prev = prev;

          /* push work onto gc->release_pages */
          work->next = gc->release_pages;
          gc->release_pages = work;

          /* add the old page to the page map so fixups can find forwards */
          pagemap_add(pagemap, work);

          work = prev;
        } else { 
          work = work->prev;
        }
      } else {
        if (npage == work)
          npage = npage->prev;
        work = work->prev;
      }
    }
  }
}

static void repair_heap(NewGC *gc)
{
  struct mpage *page;
  int i;
  Fixup_Proc *fixup_table = gc->fixup_table;

  for(i = 0; i < PAGE_TYPES; i++) {
    for(page = gc->gen1_pages[i]; page; page = page->next) {
      if(page->marked_on) {
        page->has_new = 0;
        /* these are guaranteed not to be protected */
        if(page->big_page) {
          void **start = PPTR(NUM(page->addr) + PREFIX_SIZE + WORD_SIZE);
          void **end = PPTR(NUM(page->addr) + page->size);

          GCDEBUG((DEBUGOUTF, "Cleaning objs on page %p, starting with %p\n",
                page, start));
          page->big_page = 1; /* remove the mark */
          switch(page->page_type) {
            case PAGE_TAGGED: 
              fixup_table[*(unsigned short*)start](start); 
              break;
            case PAGE_ATOMIC: break;
            case PAGE_ARRAY: 
                              while(start < end) gcFIXUP(*(start++)); 
                              break;
            case PAGE_XTAGGED: 
                              GC_fixup_xtagged(start); 
                              break;
            case PAGE_TARRAY: {
                                unsigned short tag = *(unsigned short *)start;
                                end -= INSET_WORDS;
                                while(start < end) start += fixup_table[tag](start);
                                break;
                              }
          }
        } else {
          void **start = PPTR(NUM(page->addr) + page->previous_size);
          void **end = PPTR(NUM(page->addr) + page->size);

          GCDEBUG((DEBUGOUTF, "Cleaning objs on page %p, starting with %p\n",
                page, start));
          switch(page->page_type) {
            case PAGE_TAGGED: 
              while(start < end) {
                struct objhead *info = (struct objhead *)start;

                if(info->mark) {
                  info->mark = 0;
                  fixup_table[*(unsigned short*)(start+1)](start+1);
                } else {
                  info->dead = 1;
                }
                start += info->size;
              }
              break;
            case PAGE_ATOMIC:
              while(start < end) {
                struct objhead *info = (struct objhead *)start;
                if(info->mark) {
                  info->mark = 0;
                } else info->dead = 1;
                start += info->size;
              }
              break;
            case PAGE_ARRAY: 
              while(start < end) {
                struct objhead *info = (struct objhead *)start;
                size_t size = info->size;
                if(info->mark) {
                  void **tempend = (start++) + size;
                  while(start < tempend) gcFIXUP(*start++);
                  info->mark = 0;
                } else { 
                  info->dead = 1;
                  start += size;
                }
              }
              break;
            case PAGE_TARRAY:
              while(start < end) {
                struct objhead *info = (struct objhead *)start;
                size_t size = info->size;
                if(info->mark) {
                  void **tempend = (start++) + (size - INSET_WORDS);
                  unsigned short tag = *(unsigned short*)start;
                  while(start < tempend)
                    start += fixup_table[tag](start);
                  info->mark = 0;
                  start = PPTR(info) + size;
                } else {
                  info->dead = 1;
                  start += size;
                }
              }
              break;
            case PAGE_XTAGGED:
              while(start < end) {
                struct objhead *info = (struct objhead *)start;
                if(info->mark) {
                  GC_fixup_xtagged(start + 1);
                  info->mark = 0;
                } else info->dead = 1;
                start += info->size;
              }
          }
        }
      } else GCDEBUG((DEBUGOUTF,"Not Cleaning page %p\n", page));
    }
  }
}

static inline void gen1_free_mpage(PageMap pagemap, mpage *page) {
  size_t real_page_size = page->big_page ? round_to_apage_size(page->size) : APAGE_SIZE;
  pagemap_remove(pagemap, page);
  free_backtrace(page);
  free_pages(GC, page->addr, real_page_size);
  free_mpage(page);
}

static inline void cleanup_vacated_pages(NewGC *gc) {
  mpage *pages = gc->release_pages;
  PageMap pagemap = gc->page_maps;

  /* Free pages vacated by compaction: */
  while (pages) {
    mpage *next = pages->next;
    gen1_free_mpage(pagemap, pages);
    pages = next;
  }
  gc->release_pages = NULL;
}

inline static void gen0_free_big_pages(NewGC *gc) {
  mpage *work;
  mpage *next;
  PageMap pagemap = gc->page_maps;

  for(work = gc->gen0.big_pages; work; work = next) {
    next = work->next;
    pagemap_remove(pagemap, work);
    free_pages(gc, work->addr, round_to_apage_size(work->size));
    free_mpage(work);
  }
}

static void clean_up_heap(NewGC *gc)
{
  int i;
  size_t memory_in_use = 0;
  PageMap pagemap = gc->page_maps;

  gen0_free_big_pages(gc);

  for(i = 0; i < PAGE_TYPES; i++) {
    if(gc->gc_full) {
      mpage *work = gc->gen1_pages[i];
      mpage *prev = NULL;
      while(work) {
        mpage *next = work->next;
        if(!work->marked_on) {
          /* remove work from list */
          if(prev) prev->next = next; else gc->gen1_pages[i] = next;
          if(next) work->next->prev = prev;
          gen1_free_mpage(pagemap, work);
        } else {
          pagemap_add(pagemap, work);
          work->back_pointers = work->marked_on = 0;
          memory_in_use += work->size;
          prev = work; 
        }
        work = next;
      }
    } else {
      mpage *work;
      for(work = gc->gen1_pages[i]; work; work = work->next) {
        pagemap_add(pagemap, work);
        work->back_pointers = work->marked_on = 0;
        memory_in_use += work->size;
      }
    }
  }
  
  gc->memory_in_use = memory_in_use;
  cleanup_vacated_pages(gc);
}

static void protect_old_pages(NewGC *gc)
{
  Page_Range *protect_range = gc->protect_range;
  struct mpage *page;
  int i;

  for(i = 0; i < PAGE_TYPES; i++) 
    if(i != PAGE_ATOMIC)
      for(page = gc->gen1_pages[i]; page; page = page->next)
        if(page->page_type != PAGE_ATOMIC)  {
          if (!page->mprotected) {
            page->mprotected = 1;
            add_protect_page_range(protect_range, page->addr, page->size, APAGE_SIZE, 0);
          }
        }

  flush_protect_page_ranges(protect_range, 0);
}

#if 0
extern double scheme_get_inexact_milliseconds(void);
# define TIME_DECLS() double start, task_start
# define TIME_INIT() start = task_start = scheme_get_inexact_milliseconds(); fprintf(stderr, "GC (%d):\n", gc->gc_full)
# define TIME_STEP(task) fprintf(stderr, "  %s: %lf\n", task, scheme_get_inexact_milliseconds() - task_start); \
  task_start = scheme_get_inexact_milliseconds()
# define TIME_DONE() fprintf(stderr, " Total: %lf\n", scheme_get_inexact_milliseconds() - start)
#else
# define TIME_DECLS() /**/
# define TIME_INIT() /**/
# define TIME_STEP(task) /**/
# define TIME_DONE() /**/
#endif

/* Full GCs trigger finalization. Finalization releases data
   in the old generation. So one more full GC is needed to
   really clean up. The full_needed_for_finalization flag triggers 
   the second full GC. */

static void garbage_collect(NewGC *gc, int force_full)
{
  unsigned long old_mem_use = gc->memory_in_use;
  unsigned long old_gen0    = gc->gen0.current_size;
  int next_gc_full;
  TIME_DECLS();

  /* determine if this should be a full collection or not */
  gc->gc_full = force_full || !gc->generations_available 
    || (gc->since_last_full > 100) || (gc->memory_in_use > (2 * gc->last_full_mem_use));
#if 0
  printf("Collection %li (full = %i): %i / %i / %i / %i  %ld\n", number_of_gc_runs, 
      gc->gc_full, force_full, !generations_available,
      (gc->since_last_full > 100), (gc->memory_in_use > (2 * gc->last_full_mem_use)),
      gc->last_full_mem_use);
#endif

  next_gc_full = gc->gc_full;

  if (gc->full_needed_for_finalization) {
    gc->full_needed_for_finalization= 0;
    gc->gc_full = 1;
  }

  gc->number_of_gc_runs++; 
  INIT_DEBUG_FILE(); DUMP_HEAP();

  /* we don't want the low-level allocator freaking because we've gone past
     half the available memory */
  gc->in_unsafe_allocation_mode = 1;
  gc->unsafe_allocation_abort = out_of_memory;

  TIME_INIT();

  /* inform the system (if it wants us to) that we're starting collection */
  if(GC_collect_start_callback)
    GC_collect_start_callback();

  TIME_STEP("started");

  gc->no_further_modifications = 1;

  prepare_pages_for_collection(gc);
  init_weak_boxes(gc);
  init_weak_arrays(gc);
  init_ephemerons(gc);

  /* at this point, the page map should only include pages that contain
     collectable objects */

  TIME_STEP("prepared");

  /* mark and repair the roots for collection */
  mark_backpointers(gc);
  TIME_STEP("backpointered");
  mark_finalizer_structs(gc);
  mark_weak_finalizer_structs(gc);
  TIME_STEP("pre-rooted");
  mark_roots(gc);
  mark_immobiles(gc);
  TIME_STEP("rooted");
  GC_mark_variable_stack(GC_variable_stack, 0, get_stack_base(gc), NULL);

  TIME_STEP("stacked");

  /* now propagate/repair the marks we got from these roots, and do the
     finalizer passes */
  propagate_marks(gc); mark_ready_ephemerons(gc); propagate_marks(gc); 
  check_finalizers(gc, 1); mark_ready_ephemerons(gc); propagate_marks(gc);
  check_finalizers(gc, 2); mark_ready_ephemerons(gc); propagate_marks(gc);
  if(gc->gc_full) zero_weak_finalizers(gc);
  do_ordered_level3(gc); propagate_marks(gc);
  check_finalizers(gc, 3); propagate_marks(gc);
  if(gc->gc_full) {
    reset_weak_finalizers(gc); 
    propagate_marks(gc);
  }
#ifndef NEWGC_BTC_ACCOUNT
  /* we need to clear out the stack pages. If we're doing memory accounting,
     though, we might as well leave them up for now and let the accounting
     system clear them later. Better then freeing them, at least. If we're
     not doing accounting, though, there is no "later" where they'll get
     removed */
  clear_stack_pages();  
#endif

  TIME_STEP("marked");

  zero_weak_boxes(gc); 
  zero_weak_arrays(gc);
  zero_remaining_ephemerons(gc);

  TIME_STEP("zeroed");

  if(gc->gc_full) do_heap_compact(gc);

  TIME_STEP("compacted");

  /* do some cleanup structures that either change state based on the
     heap state after collection or that become useless based on changes
     in state after collection */
#ifdef NEWGC_BTC_ACCOUNT
  BTC_clean_up(gc);
#endif
  TIME_STEP("cleaned");
  repair_finalizer_structs(gc);
  repair_weak_finalizer_structs(gc);
  repair_roots(gc);
  repair_immobiles(gc);
  GC_fixup_variable_stack(GC_variable_stack, 0, get_stack_base(gc), NULL);
  TIME_STEP("reparied roots");
  repair_heap(gc);
  TIME_STEP("repaired");
  clean_up_heap(gc);
  TIME_STEP("cleaned heap");
  reset_nursery(gc);
  TIME_STEP("reset nursurey");
#ifdef NEWGC_BTC_ACCOUNT
  if (gc->gc_full)
    BTC_do_accounting(gc);
#endif
  TIME_STEP("accounted");
  if (gc->generations_available)
    protect_old_pages(gc);
  TIME_STEP("protect");
  if (gc->gc_full)
    vm_flush_freed_pages(gc->vm);
  reset_finalizer_tree(gc);

  TIME_STEP("reset");

  /* now we do want the allocator freaking if we go over half */
  gc->in_unsafe_allocation_mode = 0;

  gc->no_further_modifications = 0;

  /* If we have too many idle pages, flush: */
  if (vm_memory_allocated(gc->vm) > ((gc->used_pages << (LOG_APAGE_SIZE + 1)))) {
    vm_flush_freed_pages(gc->vm);
  }

  /* update some statistics */
  if(gc->gc_full) gc->num_major_collects++; else gc->num_minor_collects++;
  if(gc->peak_memory_use < gc->memory_in_use) gc->peak_memory_use = gc->memory_in_use;
  if(gc->gc_full)
    gc->since_last_full = 0;
  else if((float)(gc->memory_in_use - old_mem_use) < (0.1 * (float)old_mem_use))
    gc->since_last_full += 1;
  else if((float)(gc->memory_in_use - old_mem_use) < (0.4 * (float)old_mem_use))
    gc->since_last_full += 5;
  else 
    gc->since_last_full += 10;
  if(gc->gc_full)
    gc->last_full_mem_use = gc->memory_in_use;

  /* inform the system (if it wants us to) that we're done with collection */
  if (GC_collect_start_callback)
    GC_collect_end_callback();
  if (GC_collect_inform_callback)
    GC_collect_inform_callback(gc->gc_full, old_mem_use + old_gen0, gc->memory_in_use);

  TIME_STEP("ended");

  TIME_DONE();

  if (!gc->run_queue)
    next_gc_full = 0;

  /* run any queued finalizers, EXCEPT in the case where this collection was
     triggered by the execution of a finalizer. The outside world needs this
     invariant in some corner case I don't have a reference for. In any case,
     if we run a finalizer after collection, and it triggers a collection,
     we should not run the next finalizer in the queue until the "current"
     finalizer completes its execution */
  if(!gc->running_finalizers) {
    gc->running_finalizers = 1;

    /* Finalization might allocate, which might need park: */
    gc->park_save[0] = gc->park[0];
    gc->park_save[1] = gc->park[1];
    gc->park[0] = NULL;
    gc->park[1] = NULL;

    while(gc->run_queue) {
      struct finalizer *f;
      void **saved_gc_variable_stack;

      f = gc->run_queue; gc->run_queue = gc->run_queue->next;
      if(!gc->run_queue) gc->last_in_queue = NULL;

      GCDEBUG((DEBUGOUTF, "Running finalizers %p for pointer %p (lvl %i)\n", f, f->p, f->eager_level));
      saved_gc_variable_stack = GC_variable_stack;
      f->f(f->p, f->data);
      GC_variable_stack = saved_gc_variable_stack;
    }
#ifdef NEWGC_BTC_ACCOUNT
    BTC_run_account_hooks(gc);
#endif
    gc->running_finalizers = 0;

    gc->park[0] = gc->park_save[0];
    gc->park[1] = gc->park_save[1];
    gc->park_save[0] = NULL;
    gc->park_save[1] = NULL;
  }

  DUMP_HEAP(); CLOSE_DEBUG_FILE();

  if (next_gc_full)
    gc->full_needed_for_finalization = 1;
}

#if MZ_GC_BACKTRACE

static GC_get_type_name_proc stack_get_type_name;
static GC_get_xtagged_name_proc stack_get_xtagged_name;
static GC_print_tagged_value_proc stack_print_tagged_value;

static void dump_stack_pos(void *a) 
{
  GCPRINT(GCOUTF, " @%p: ", a);
  print_out_pointer("", *(void **)a, stack_get_type_name, stack_get_xtagged_name, stack_print_tagged_value);
}

# define GC_X_variable_stack GC_do_dump_variable_stack
# define gcX(a) dump_stack_pos(a)
# define X_source(stk, p) /* */
# include "var_stack.c"
# undef GC_X_variable_stack
# undef gcX
# undef X_source

void GC_dump_variable_stack(void **var_stack,
    long delta,
    void *limit,
    void *stack_mem,
    GC_get_type_name_proc get_type_name,
    GC_get_xtagged_name_proc get_xtagged_name,
    GC_print_tagged_value_proc print_tagged_value)
{
  stack_get_type_name = get_type_name;
  stack_get_xtagged_name = get_xtagged_name;
  stack_print_tagged_value = print_tagged_value;
  GC_do_dump_variable_stack(var_stack, delta, limit, stack_mem);
}

#endif

/******************************************************************************/
/*                              GC free all                                   */
/******************************************************************************/

void GC_free_all(void)
{
  NewGC *gc = GC_get_GC();
  int i;
  mpage *work;
  mpage *next;
  PageMap pagemap = gc->page_maps;

  remove_signal_handler(gc);

  gen0_free_big_pages(gc);

  for(i = 0; i < PAGE_TYPES; i++) {
    for (work = gc->gen1_pages[i]; work; work = next) {
      next = work->next;

      if (work->mprotected)
        vm_protect_pages(work->addr, work->big_page ? round_to_apage_size(work->size) : APAGE_SIZE, 1);
      gen1_free_mpage(pagemap, work);
    }
  }

  vm_flush_freed_pages(gc->vm);
  vm_free(gc->vm);
  free(gc);
}
