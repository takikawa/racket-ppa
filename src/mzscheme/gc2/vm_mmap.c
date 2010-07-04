/* 
   Provides:
      mmap-based allocator (uses alloc_cache.c)
      determine_max_heap_size()
   Requires:
      my_qsort (for alloc_cache.c)
      LOGICALLY_ALLOCATING_PAGES(len)
      ACTUALLY_ALLOCATING_PAGES(len)
      LOGICALLY_FREEING_PAGES(len)
      ACTUALLY_FREEING_PAGES(len)
   Optional:
      CHECK_USED_AGAINST_MAX(len)
      GCPRINT
      GCOUTF
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <errno.h>

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
#endif
#ifndef CHECK_USED_AGAINST_MAX
# define CHECK_USED_AGAINST_MAX(x) /* empty */
#endif

static int page_size; /* OS page size */

#ifndef MAP_ANON
int fd, fd_created;
#endif

inline static void *find_cached_pages(size_t len, size_t alignment, int dirty_ok);
static void free_actual_pages(void *p, size_t len, int zeroed);

/* Instead of immediately freeing pages with munmap---only to mmap
   them again---we cache BLOCKFREE_CACHE_SIZE freed pages. A page is
   cached unused for at most BLOCKFREE_UNMAP_AGE cycles of the
   collector. (A max age of 1 seems useful, anything more seems
   dangerous.) 

   The cache is small enough that we don't need an elaborate search
   mechanism, but we do a bit of work to collapse adjacent pages in
   the cache. */

static void *do_malloc_pages(size_t len, size_t alignment, int dirty_ok)
{
  void *r;
  size_t extra = 0;

  if (!page_size)
    page_size = getpagesize();

#ifndef MAP_ANON
  if (!fd_created) {
    fd_created = 1;
    fd = open("/dev/zero", O_RDWR);
  }
#endif

  CHECK_USED_AGAINST_MAX(len);

  /* Round up to nearest page: */
  if (len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  /* Something from the cache, perhaps? */
  r = find_cached_pages(len, alignment, dirty_ok);
  if (r)
    return r;

  extra = alignment;

#ifdef MAP_ANON
  r = mmap(NULL, len + extra, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
#else
  r = mmap(NULL, len + extra, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
#endif

  if (r  == (void *)-1)
    return NULL;

  if (extra) {
    /* We allocated too large so we can choose the alignment. */
    void *real_r;
    long pre_extra;

    real_r = (void *)(((unsigned long)r + (alignment - 1)) & (~(alignment - 1)));
    
    pre_extra = real_r - r;
    if (pre_extra)
      if (munmap(r, pre_extra))
	GCPRINT(GCOUTF, "Unmap warning: %lx, %ld, %d\n", (long)r, pre_extra, errno);
    if (pre_extra < extra) {
      if (!pre_extra) {
	/* Instead of actually unmapping, put it in the cache, and there's
	   a good chance we can use it next time: */
	ACTUALLY_ALLOCATING_PAGES(extra);
	free_actual_pages(real_r + len, extra, 1);
      } else {
	if (munmap(real_r + len, extra - pre_extra))
	  GCPRINT(GCOUTF, "Unmap warning: %lx, %ld, %d\n", (long)r, pre_extra, errno);
      }
    }
    r = real_r;
  }

  ACTUALLY_ALLOCATING_PAGES(len);
  LOGICALLY_ALLOCATING_PAGES(len);

  return r;
}

static void *malloc_pages(size_t len, size_t alignment)
{
  return do_malloc_pages(len, alignment, 0);
}

static void *malloc_dirty_pages(size_t len, size_t alignment)
{
  return do_malloc_pages(len, alignment, 1);
}

static void system_free_pages(void *p, size_t len)
{
  if (munmap(p, len)) {
    GCPRINT(GCOUTF, "Unmap warning: %lx, %ld, %d\n", (long)p, (long)len, errno);
  }
}

static void protect_pages(void *p, size_t len, int writeable)
{
  if (len & (page_size - 1)) {
    len += page_size - (len & (page_size - 1));
  }

  mprotect(p, len, (writeable ? (PROT_READ | PROT_WRITE) : PROT_READ));
}

# include "alloc_cache.c"

/*************************************************************/

# include "rlimit_heapsize.c"
