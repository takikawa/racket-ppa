#include "schpriv.h"
#include "schmach.h"
#include <ctype.h>
#include <math.h>
#include "../gc2/gc2_obj.h"

READ_ONLY static Scheme_Hash_Tree *empty_hash_tree[3];

THREAD_LOCAL_DECL(intptr_t scheme_hash_request_count);
THREAD_LOCAL_DECL(intptr_t scheme_hash_iteration_count);

READ_ONLY static Scheme_Object GONE[1];

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/* Hash calculations need to use unsigned integers, where
   wraparound behavior is defined for overflow. But some 
   parts of the published hash API use signed integers.
   The to_signed_hash() and to_unsigned_hash() macros are
   supposed to marshal unsigned to signed and back without
   any loss of unsigned information.
   FIXME: The current implementation as a cast is *not* 
   consistent with the C standard (the cast to unsigned can
   be implementation-dependent), but it works fine with all
   compilers that we currently use. */
#define to_signed_hash(v) ((intptr_t)v)
#define to_unsigned_hash(v) ((uintptr_t)v)

#ifdef OBJHEAD_HAS_HASH_BITS
/* In 3m mode, we only have 14 bits of hash code in the
   Scheme_Object header. But the GC-level object header has some
   leftover bits (currently 9, 11, 41, or 43, depending on the
   platform), so use those, too. That only works for GCable
   objects, so we use 1 of our 14 bits to indicate whether the
   other bits are present. */
# define GCABLE_OBJ_HASH_BIT 0x0004
# define OBJ_HASH_USELESS_BITS 3
#else
# define GCABLE_OBJ_HASH_BIT 0
# define OBJ_HASH_USELESS_BITS 2
#endif
#define OBJ_HASH_USEFUL_BITS (16 - OBJ_HASH_USELESS_BITS)
#define OBJ_HASH_USEFUL_MASK ((1 << OBJ_HASH_USEFUL_BITS)-1)

/* A hash code has been installed if any of these bits is
   non-zero: */
#define HASH_CODE_PRESENT_BITS 0xFFFC

#ifdef MZ_PRECISE_GC
/* keygen race conditions below are "ok", because keygen is randomness
   used to create a hashkey. Technically, a race condition allows
   undefined behavior by some C standards, but we don't expect
   compilers to actually impose a "catch fire" semantics. Make sure
   that only one thread at a time sets a hash code in a specific
   object, though, and watch out for a race with JIT-generated code
   running in a future and setting flags on pairs. */
SHARED_OK static uintptr_t keygen = GCABLE_OBJ_HASH_BIT;

XFORM_NONGCING static MZ_INLINE
uintptr_t PTR_TO_LONG(Scheme_Object *o)
{
  uintptr_t bits;
  short v;

  if (SCHEME_INTP(o))
    return (uintptr_t)o >> 1;

  v = o->keyex;

  if (!(v & HASH_CODE_PRESENT_BITS)) {
    uintptr_t local_keygen = keygen;
    v |= (short)local_keygen;
#ifdef OBJHEAD_HAS_HASH_BITS
    if (GC_is_allocated(o)) {
      OBJHEAD_HASH_BITS(o) = (local_keygen >> 16);
      v |= GCABLE_OBJ_HASH_BIT;
    } else
      v &= ~GCABLE_OBJ_HASH_BIT;
#endif
    if (!v) v = 0x1AD0;
#ifdef MZ_USE_FUTURES
    if (SCHEME_PAIRP(o) && scheme_is_multithreaded(1)) {
      /* Use CAS to avoid losing a hash code due to a conflict with
         JIT-generated `list?' test, which itself uses CAS to set "is
         a list" or "not a list" flags on pairs. */
      while (!mzrt_cas16(&o->keyex, o->keyex, v)) {
      } 
    } else
#endif
      o->keyex = v;
    keygen += (1 << OBJ_HASH_USELESS_BITS);
  }

#ifdef OBJHEAD_HAS_HASH_BITS
  if (v & GCABLE_OBJ_HASH_BIT)
    bits = OBJHEAD_HASH_BITS(o);
  else
#endif
    bits = o->type;

  /* We need to drop the low two bits of `v', which
     are used for non-hashing purposes in some types. */

  return (bits << OBJ_HASH_USEFUL_BITS) | ((v >> OBJ_HASH_USELESS_BITS) & OBJ_HASH_USEFUL_MASK);
}
#else
# define PTR_TO_LONG(p) ((uintptr_t)(p)>>2)
#endif

void scheme_set_distinct_eq_hash(Scheme_Object *o)
/* Used after cloning a value to ensure that the
   hash code is not cloned */
{
#ifdef MZ_PRECISE_GC
  o->keyex = o->keyex & ~HASH_CODE_PRESENT_BITS;
#endif
}

void scheme_install_symbol_hash_code(Scheme_Object *sym, uintptr_t h)
{
#ifdef MZ_PRECISE_GC
  /* Record a hash code for the symbol as its `eq?` hash code ---
     intended mainly to make `equal?` hashing depend only on the
     symbol content */
  short v;

  v = sym->keyex;

  if (!(v & 0xFFFC)) {
    v |= (short)(h & ~0x7);
#ifdef OBJHEAD_HAS_HASH_BITS
    if (GC_is_allocated(sym)) {
      OBJHEAD_HASH_BITS(sym) = (h >> 16);
      v |= GCABLE_OBJ_HASH_BIT;
    } else
      v &= ~GCABLE_OBJ_HASH_BIT;
#endif
    if (!v) v = 0x1AD0;
    sym->keyex = v;
  }
#endif
}

#define FILL_FACTOR 1.4

#define MIN_HTABLE_SIZE 8

typedef int (*Hash_Compare_Proc)(void*, void*);

typedef uintptr_t hash_v_t;

#define MAX_HASH_DEPTH 128

/* For detecting and debugging accidental dependencies on hash-table order,
   it might be helpful to invert the order at the lowest level: */
/* #define REVERSE_HASH_TABLE_ORDER 1 */

/*========================================================================*/
/*                         hashing functions                              */
/*========================================================================*/

static void string_hash_indices(void *_key, intptr_t *_h, intptr_t *_h2)
{
  const char *key = (char *)_key;
  uintptr_t i, h, h2;

  h2 = h = i = 0;
  while (key[i]) {
    int c = key[i++];
    h += (h << 5) + h + c;
    h2 += c;
  }

  if (_h)
    *_h = to_signed_hash(h);
  if (_h2)
    *_h2 = to_signed_hash(h2);
}

/*========================================================================*/
/*                         equality with wraps                            */
/*========================================================================*/

static Scheme_Object *apply_equal_key_wraps(Scheme_Object *key, Scheme_Object *key_wraps)
{
  if (key_wraps) {
    GC_CAN_IGNORE const char *who = (const char *)SCHEME_CAR(key_wraps);
    Scheme_Chaperone *px;
    Scheme_Object *a[2], *red;

    key_wraps = SCHEME_CDR(key_wraps);
    while (!SCHEME_NULLP(key_wraps)) {
      px = (Scheme_Chaperone *)SCHEME_CAR(key_wraps);

      red = SCHEME_BOX_VAL(px->redirects);
      red = SCHEME_VEC_ELS(red)[5];

      a[0] = px->prev;
      a[1] = key;
      key = _scheme_apply(red, 2, a);

      if (!(SCHEME_CHAPERONE_FLAGS(px) & SCHEME_CHAPERONE_IS_IMPERSONATOR)
          &&  !scheme_chaperone_of(key, a[1])) {
        scheme_wrong_chaperoned(who, "key", a[1], key);
        return 0;
      }

      key_wraps = SCHEME_CDR(key_wraps);
    }
  }

  return key;
}

static int equal_w_key_wraps(Scheme_Object *ekey, Scheme_Object *tkey, Scheme_Object *key_wraps)
{
  if (key_wraps)
    tkey = apply_equal_key_wraps(tkey, key_wraps);

  return scheme_equal(ekey, tkey);
}

/*========================================================================*/
/*                      normal mutable hash table                         */
/*========================================================================*/

#ifdef REVERSE_HASH_TABLE_ORDER
# define HASH_TO_ARRAY_INDEX(h, mask) ((mask) - (h))
#else
# define HASH_TO_ARRAY_INDEX(h, mask) (h)
#endif

/* Since mutable hash tables tend to use the low bits of a hash code,
   make sure higher bits of a fixnum are represented there: */
XFORM_NONGCING static uintptr_t fixmix(uintptr_t x) {
#ifdef SIXTY_FOUR_BIT_INTEGERS
  uintptr_t x1 = x ^ ((x >> 32) & (uintptr_t)0xFFFFFFFF);
#else
  uintptr_t x1 = x;
#endif
  uintptr_t x2 = x1 ^ ((x1 >> 16) & (uintptr_t)0xFFFF);
  uintptr_t x3 = x2 ^ ((x2 >> 8) & (uintptr_t)0xFF);

  return x3;
}

Scheme_Hash_Table *scheme_make_hash_table(int type)
{
  Scheme_Hash_Table *table;

  table = MALLOC_ONE_TAGGED(Scheme_Hash_Table);

  table->size = 0;
    
  table->iso.so.type = scheme_hash_table_type;

  if (type == SCHEME_hash_string) {
    table->make_hash_indices = string_hash_indices;
    table->compare = (Hash_Compare_Proc)strcmp;
  }

  return table;
}

void scheme_clear_hash_table(Scheme_Hash_Table *ht)
{
  ht->size = 0;
  ht->count = 0;
  ht->keys = NULL;
  ht->vals = NULL;
  ht->mcount = 0;
}

static Scheme_Object *do_hash(Scheme_Hash_Table *table, Scheme_Object *key, int set, Scheme_Object *val,
                              Scheme_Object *key_wraps,
			      GC_CAN_IGNORE Scheme_Object **_interned_key)
{
  Scheme_Object *tkey, *ekey, **keys;
  intptr_t hx, h2x;
  hash_v_t h, h2 = 0, useme = 0;
  uintptr_t mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    if (table->compare == scheme_compare_equal) {
      if (key_wraps)
        ekey = apply_equal_key_wraps(key, key_wraps);
      else
        ekey = key;
      h2 = 0;
      hx = scheme_equal_hash_key(ekey);
      h = fixmix(to_unsigned_hash(hx)) & mask;
    } else {
      GC_CAN_IGNORE intptr_t *_h2x;
      if (table->compare) {
        h2 = 0;
        _h2x = NULL;
      } else
        _h2x = &h2x;
      table->make_hash_indices((void *)key, &hx, _h2x);
      h = fixmix(to_unsigned_hash(hx)) & mask;
      if (_h2x)
        h2 = (fixmix(to_unsigned_hash(h2x)) & mask) | 1;
      ekey = NULL;
    }
  } else {
    uintptr_t lkey;
    lkey = fixmix(PTR_TO_LONG((Scheme_Object *)key));
    h = lkey & mask;
    h2 = ((lkey >> 1) & mask) | 1;
    ekey = NULL;
  }

  keys = table->keys;

  if (table->compare) {
    if (table->compare == scheme_compare_equal) {
      /* Direct calls can be significantly faster than indirect */
      scheme_hash_request_count++;
      while ((tkey = keys[HASH_TO_ARRAY_INDEX(h, mask)])) {
        if (SAME_PTR(tkey, GONE)) {
          if (set > 1) {
            useme = h;
            set = 1;
          }
        } else if (equal_w_key_wraps(ekey, tkey, key_wraps)) {
	  if (_interned_key) *_interned_key = tkey;
          if (set) {
            table->vals[HASH_TO_ARRAY_INDEX(h, mask)] = val;
            if (!val) {
              keys[HASH_TO_ARRAY_INDEX(h, mask)] = GONE;
              --table->count;
            }
            return val;
          } else
            return table->vals[HASH_TO_ARRAY_INDEX(h, mask)];
        }
        scheme_hash_iteration_count++;
        if (!h2) {
          h2x = scheme_equal_hash_key2(ekey);
          h2 = (fixmix(to_unsigned_hash(h2x)) & (table->size - 1)) | 1;
        }
        h = (h + h2) & mask;
      }
    } else {
      scheme_hash_request_count++;
      while ((tkey = keys[HASH_TO_ARRAY_INDEX(h, mask)])) {
        if (SAME_PTR(tkey, GONE)) {
          if (set > 1) {
            useme = h;
            set = 1;
          }
        } else if (!table->compare(tkey, (char *)key)) {
	  if (_interned_key) *_interned_key = tkey;
          if (set) {
            table->vals[HASH_TO_ARRAY_INDEX(h, mask)] = val;
            if (!val) {
              keys[HASH_TO_ARRAY_INDEX(h, mask)] = GONE;
              --table->count;
            }
            return val;
          } else
            return table->vals[HASH_TO_ARRAY_INDEX(h, mask)];
        }
        scheme_hash_iteration_count++;
        if (!h2) {
          table->make_hash_indices((void *)key, NULL, &h2x);
          h2 = (fixmix(to_unsigned_hash(h2x)) & (table->size - 1)) | 1;
        }
        h = (h + h2) & mask;
      }
    }
  } else {
    scheme_hash_request_count++;
    while ((tkey = keys[HASH_TO_ARRAY_INDEX(h, mask)])) {
      if (SAME_PTR(tkey, key)) {
	if (_interned_key) *_interned_key = tkey;
	if (set) {
	  table->vals[HASH_TO_ARRAY_INDEX(h, mask)] = val;
	  if (!val) {
	    keys[HASH_TO_ARRAY_INDEX(h, mask)] = GONE;
	    --table->count;
	  }
	  return val;
	} else
	  return table->vals[HASH_TO_ARRAY_INDEX(h, mask)];
      } else if (SAME_PTR(tkey, GONE)) {
	if (set > 1) {
	  useme = h;
	  set = 1;
	}
      } 
      scheme_hash_iteration_count++;
      h = (h + h2) & mask;
    }
  }

  if (!set || !val)
    return NULL;

  if (set == 1)
    h = useme;
  else if (table->mcount * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size, size;
    Scheme_Object **oldkeys = table->keys;
    Scheme_Object **oldvals = table->vals;

    if (table->count << 1 >= table->mcount)
      size = oldsize << 1;
    else
      size = oldsize;
    table->size = size;
    
    {
      Scheme_Object **ba;
      ba = MALLOC_N(Scheme_Object *, size);
      table->vals = ba;
      ba = MALLOC_N(Scheme_Object *, size);
      table->keys = ba;
    }

    table->count = 0;
    table->mcount = 0;
    for (i = 0; i < oldsize; i++) {
      if (oldkeys[i] && !SAME_PTR(oldkeys[i], GONE))
	do_hash(table, oldkeys[i], 2, oldvals[i], key_wraps, _interned_key);
    }

    goto rehash_key;
  } else {
    table->mcount++;
  }

  table->count++;
  table->keys[HASH_TO_ARRAY_INDEX(h, mask)] = key;
  table->vals[HASH_TO_ARRAY_INDEX(h, mask)] = val;

  if (_interned_key) *_interned_key = key;
  return val;
}

static Scheme_Object *do_hash_set(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val)
{
  Scheme_Object *tkey, **keys;
  hash_v_t h, h2, useme = 0;
  uintptr_t mask;
  uintptr_t lkey;
  int set = 2;

  mask = table->size - 1;

  lkey = fixmix(PTR_TO_LONG((Scheme_Object *)key));
  h = lkey & mask;
  h2 = (lkey >> 1) & mask;

  h2 |= 1;

  keys = table->keys;
  
  scheme_hash_request_count++;
  while ((tkey = keys[HASH_TO_ARRAY_INDEX(h, mask)])) {
    if (SAME_PTR(tkey, key)) {
      table->vals[HASH_TO_ARRAY_INDEX(h, mask)] = val;
      if (!val) {
	keys[HASH_TO_ARRAY_INDEX(h, mask)] = GONE;
	--table->count;
      }
      return val;
    } else if (SAME_PTR(tkey, GONE)) {
      if (set > 1) {
	useme = h;
	set = 1;
      }
    } 
    scheme_hash_iteration_count++;
    h = (h + h2) & mask;
  }

  if (!val)
    return NULL;

  if (set == 1)
    h = useme;
  else if (table->mcount * FILL_FACTOR >= table->size) {
    /* Use slow path to grow table: */
    return do_hash(table, key, 2, val, NULL, NULL);
  } else {
    table->mcount++;
  }

  table->count++;
  table->keys[HASH_TO_ARRAY_INDEX(h, mask)] = key;
  table->vals[HASH_TO_ARRAY_INDEX(h, mask)] = val;

  return val;
}

XFORM_NONGCING static Scheme_Object *do_hash_get(Scheme_Hash_Table *table, Scheme_Object *key,
                                                 GC_CAN_IGNORE Scheme_Object **_interned_key)
{
  Scheme_Object *tkey, **keys;
  hash_v_t h, h2;
  uintptr_t mask;
  uintptr_t lkey;

  mask = table->size - 1;

  lkey = fixmix(PTR_TO_LONG((Scheme_Object *)key));
  h = lkey & mask;
  h2 = (lkey >> 1) & mask;

  h2 |= 1;

  keys = table->keys;

  scheme_hash_request_count++;
  while ((tkey = keys[HASH_TO_ARRAY_INDEX(h, mask)])) {
    if (SAME_PTR(tkey, key)) {
      if (_interned_key) *_interned_key = tkey;
      return table->vals[HASH_TO_ARRAY_INDEX(h, mask)];
    }
    scheme_hash_iteration_count++;
    h = (h + h2) & mask;
  }

  return NULL;
}

void scheme_hash_set_w_key_wraps(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val,
                                 Scheme_Object *key_wraps)
{
  if (!table->vals) {
    Scheme_Object **ba;
    
    table->size = 8;
    
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->keys = ba;
  }

  if (table->make_hash_indices)
    do_hash(table, key, 2, val, key_wraps, NULL);
  else
    do_hash_set(table, key, val);
}

void scheme_hash_set(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val)
{
  scheme_hash_set_w_key_wraps(table, key, val, NULL);
}

Scheme_Object *scheme_hash_get_w_key_wraps(Scheme_Hash_Table *table, Scheme_Object *key,
                                           Scheme_Object *key_wraps,
                                           GC_CAN_IGNORE Scheme_Object **_interned_key)
{
  if (!table->vals)
    return NULL;
  else if (table->make_hash_indices)
    return do_hash(table, key, 0, NULL, key_wraps, _interned_key);
  else
    return do_hash_get(table, key, _interned_key);
}

Scheme_Object *scheme_hash_get(Scheme_Hash_Table *table, Scheme_Object *key)
{
  return scheme_hash_get_w_key_wraps(table, key, NULL, NULL);
}

Scheme_Object *scheme_hash_get_key(Scheme_Hash_Table *table, Scheme_Object *key)
{
  Scheme_Object *interned_key, *v;

  v = scheme_hash_get_w_key_wraps(table, key, NULL, &interned_key);
  if (v)
    return interned_key;
  else
    return NULL;
}

Scheme_Object *scheme_eq_hash_get(Scheme_Hash_Table *table, Scheme_Object *key)
/* Specialized to allow XFORM_NONGCING */
{
  if (!table->vals)
    return NULL;
  else
    return do_hash_get(table, key, NULL);
}

Scheme_Object *scheme_hash_get_atomic(Scheme_Hash_Table *table, Scheme_Object *key)
/* Mostly useful for acessing equal-based hash table when you don't want
   thread switches (such as in stx object manipulations). Simply grabbing the
   table's lock would be enough to make access to the table single-threaded,
   but sometimes you don't want any thread switches at all. */
{
  Scheme_Object *r;
  scheme_start_atomic();
  r = scheme_hash_get(table, key);
  scheme_end_atomic_no_swap();
  return r;
}

void scheme_hash_set_atomic(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val)
/* See rationale with scheme_hash_get_atomic. */
{
  scheme_start_atomic();
  scheme_hash_set(table, key, val);
  scheme_end_atomic_no_swap();
}

int scheme_hash_table_equal_rec(Scheme_Hash_Table *t1, Scheme_Object *orig_t1,
                                Scheme_Hash_Table *t2, Scheme_Object *orig_t2,
                                void *eql)
{
  Scheme_Object **vals, **keys, *val1, *val2, *key;
  int i;

  if ((t1->count != t2->count)
      || (t1->make_hash_indices != t2->make_hash_indices)
      || (t1->compare != t2->compare))
    return 0;
    
  keys = t1->keys;
  vals = t1->vals;
  for (i = t1->size; i--; ) {
    if (vals[i]) {
      key = keys[i];

      if (!SAME_OBJ((Scheme_Object *)t1, orig_t1))
        val1 = scheme_chaperone_hash_traversal_get(orig_t1, key, &key);
      else
        val1 = vals[i];
      
      if (!SAME_OBJ((Scheme_Object *)t2, orig_t2))
        val2 = scheme_chaperone_hash_get(orig_t2, key);
      else
        val2 = scheme_hash_get(t2, key);

      if (!val2)
	return 0;
      if (!scheme_recur_equal(val1, val2, eql))
	return 0;
    }
  }

  return 1;
}

int scheme_hash_table_equal(Scheme_Hash_Table *t1, Scheme_Hash_Table *t2)
{
  return scheme_equal((Scheme_Object *)t1, (Scheme_Object *)t2);
}

Scheme_Hash_Table *scheme_clone_hash_table(Scheme_Hash_Table *ht)
{
  Scheme_Hash_Table *table;
  Scheme_Object **ba;

  table = MALLOC_ONE_TAGGED(Scheme_Hash_Table);
  memcpy(table, ht, sizeof(Scheme_Hash_Table));
  MZ_OPT_HASH_KEY(&(table->iso)) = 0;

  if (table->size) {
    ba = MALLOC_N(Scheme_Object *, table->size);
    memcpy(ba, table->vals, sizeof(Scheme_Object *) * table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    memcpy(ba, table->keys, sizeof(Scheme_Object *) * table->size);
    table->keys = ba;
  }

  if (table->mutex) {
    Scheme_Object *sema;
    sema = scheme_make_sema(1);
    table->mutex = sema;
  }

  return table;
}

void scheme_reset_hash_table(Scheme_Hash_Table *table, int *history)
{
  if ((table->size <= 8)
      || (table->count * FILL_FACTOR > (table->size >> 1))) {
    /* Keep same size */
  } else {
    /* Shrink by one step */
    Scheme_Object **ba;
    table->size >>= 1;
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->vals = ba;
    ba = MALLOC_N(Scheme_Object *, table->size);
    table->keys = ba;
  }
  memset(table->vals, 0, sizeof(Scheme_Object *) * table->size);
  memset(table->keys, 0, sizeof(Scheme_Object *) * table->size);
  table->count = 0;
  table->mcount = 0;
}

Scheme_Object *scheme_hash_table_next(Scheme_Hash_Table *hash,
				      mzlonglong start)
{
    int i, sz = hash->size;
    if (start >= 0) {
      if ((start >= sz) || !hash->vals[start])
        return NULL;
    }
    for (i = start + 1; i < sz; i++) {
      if (hash->vals[i])
        return scheme_make_integer(i);
    }

    return scheme_false;
}

int scheme_hash_table_index(Scheme_Hash_Table *hash, mzlonglong pos,
			    Scheme_Object **_key, Scheme_Object **_val)
{
  if (pos < hash->size) {
    if (hash->vals[pos]) {
      *_key = hash->keys[pos];
      if (_val)
	*_val = hash->vals[pos];
      return 1;
    }
  }
  
  return 0;
}

/*========================================================================*/
/*                  old-style hash table, with buckets                    */
/*========================================================================*/

Scheme_Bucket_Table *
scheme_make_bucket_table (intptr_t size, int type)
{
  Scheme_Bucket_Table *table;
  size_t asize;

  table = MALLOC_ONE_TAGGED(Scheme_Bucket_Table);

  table->size = 4;
  while (table->size < size) {
    table->size <<= 1;
  }

  table->count = 0;

  table->so.type = scheme_bucket_table_type;

  asize = (size_t)table->size * sizeof(Scheme_Bucket *);
  {
    Scheme_Bucket **ba;
    ba = (Scheme_Bucket **)scheme_malloc(asize);
    table->buckets = ba;
  }

  if (type == SCHEME_hash_weak_ptr)
    table->weak = 1;
  else if (type == SCHEME_hash_late_weak_ptr)
    table->weak = 2;
  else
    table->weak = 0;
  
  return table;
}

void scheme_clear_bucket_table(Scheme_Bucket_Table *bt)
{
  Scheme_Bucket **ba;

  bt->count = 0;
  bt->size = 4;
  ba = (Scheme_Bucket **)scheme_malloc(bt->size * sizeof(Scheme_Bucket *));
  bt->buckets = ba;
}

static Scheme_Bucket *
allocate_bucket (Scheme_Bucket_Table *table, const char *key, void *val)
{
  size_t bsize;
  Scheme_Type type;
  Scheme_Bucket *bucket;

  if (table->with_home) {
    bsize = sizeof(Scheme_Bucket_With_Home);
    type = scheme_variable_type;
  } else  {
    bsize = sizeof(Scheme_Bucket);
    type = scheme_bucket_type;
  }

  bucket = (Scheme_Bucket *)scheme_malloc_tagged(bsize);

  bucket->so.type = type;

  if (type == scheme_variable_type)
    ((Scheme_Bucket_With_Flags *)bucket)->flags = GLOB_HAS_HOME_PTR;

  if (table->weak) {
#ifdef MZ_PRECISE_GC
    void *kb;
    kb = GC_malloc_weak_box((void *)key, (void **)bucket, (void **)&bucket->val - (void **)bucket, 
                            (table->weak > 1));
    bucket->key = (char *)kb;
#else
    char *kb;
    kb = (char *)MALLOC_ONE_WEAK(void *);
    bucket->key = kb;
    *(void **)bucket->key = (void *)key;
    if (table->weak > 1) {
      scheme_late_weak_reference_indirect((void **)bucket->key, (void *)key);
      scheme_late_weak_reference_indirect((void **)&bucket->val, (void *)key);
    } else {
      scheme_weak_reference_indirect((void **)bucket->key, (void *)key);
      scheme_weak_reference_indirect((void **)&bucket->val, (void *)key);
    }
#endif
  } else
    bucket->key = (char *)key;
  bucket->val = val;

  return bucket;
}

static Scheme_Bucket *
get_bucket (Scheme_Bucket_Table *table, const char *key, int add, Scheme_Bucket *b,
            Scheme_Object *key_wraps)
{
  intptr_t hx, h2x;
  hash_v_t h, h2;
  void *ekey;
  Scheme_Bucket *bucket;
  Compare_Proc compare = table->compare;
  uintptr_t mask;

 rehash_key:

  mask = table->size - 1;

  if (table->make_hash_indices) {
    if (key_wraps)
      ekey = apply_equal_key_wraps((Scheme_Object *)key, key_wraps);
    else
      ekey = (void *)key;
    table->make_hash_indices(ekey, &hx, &h2x);
    h = fixmix(to_unsigned_hash(hx)) & mask;
    h2 = fixmix(to_unsigned_hash(h2x)) & mask;
  } else {
    uintptr_t lkey;
    lkey = fixmix(PTR_TO_LONG((Scheme_Object *)key));
    h = lkey & mask;
    h2 = (lkey >> 1) & mask;
    ekey = NULL;
  }

  h2 |= 0x1;

  if (table->weak) {
    int reuse_bucket = 0;
    scheme_hash_request_count++;
    while ((bucket = table->buckets[HASH_TO_ARRAY_INDEX(h, mask)])) {
      if (bucket->key) {
	void *hk = (void *)HT_EXTRACT_WEAK(bucket->key);
	if (!hk) {
          if (!reuse_bucket)
            reuse_bucket = h + 1;
	} else if (SAME_PTR(hk, key))
	  return bucket;
	else if (key_wraps) {
          if (equal_w_key_wraps((Scheme_Object *)ekey, (Scheme_Object *)hk, key_wraps))
            return bucket;
        } else if (compare && !compare((void *)hk, ekey))
	  return bucket;
      } else if (add)
	break;
      scheme_hash_iteration_count++;
      h = (h + h2) & mask;
    }

    if (reuse_bucket && add) {
      /* Re-use a bucket slot whose key is collected: */
      /* DON'T increment counter overall... */
      h = reuse_bucket - 1;
      --table->count;
    }
  } else {
    scheme_hash_request_count++;
    while ((bucket = table->buckets[HASH_TO_ARRAY_INDEX(h, mask)])) {
      if (SAME_PTR(bucket->key, key))
	return bucket;
      else if (key_wraps) {
        if (equal_w_key_wraps((Scheme_Object *)ekey, (Scheme_Object *)bucket->key, key_wraps))
          return bucket;
      } else if (compare && !compare((void *)bucket->key, (void *)key))
	return bucket;
      scheme_hash_iteration_count++;
      h = (h + h2) & mask;
    }
  }

  if (!add)
    return NULL;

  if (table->count * FILL_FACTOR >= table->size) {
    /* Rehash */
    int i, oldsize = table->size;
    size_t asize;
    Scheme_Bucket **old = table->buckets;

    if (table->weak && (table->size > 4096)) {
      int actual = 0;

      /* It might be nice to force a GC so that the new table is
         as small as possible, but that's too expensive. */
      /* scheme_collect_garbage(); */

      /* Check actual count: */
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key && HT_EXTRACT_WEAK(old[i]->key)) {
	  actual++;
	}
      }

      if (actual * FILL_FACTOR < table->count) {
	/* Decrement size so that the table won't actually grow. */
	table->size >>= 1;
        if ((table->size > 64) && (2 * actual * FILL_FACTOR < table->count)) {
          /* Allow the table to shrink */
          table->size >>= 1;
        }
      }
    }

    table->size <<= 1;
    
    asize = (size_t)table->size * sizeof(Scheme_Bucket *);
    {
      Scheme_Bucket **ba;
      ba = (Scheme_Bucket **)scheme_malloc(asize);
      table->buckets = ba;
    }

    table->count = 0;
    if (table->weak) {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key && HT_EXTRACT_WEAK(old[i]->key))
	  get_bucket(table, (char *)HT_EXTRACT_WEAK(old[i]->key), 1, old[i], key_wraps);
      }
    } else {
      for (i = 0; i < oldsize; i++) {
	if (old[i] && old[i]->key)
	  get_bucket(table, old[i]->key, 1, old[i], key_wraps);
      }
    }

    goto rehash_key;
  }

  if (b)
    bucket = b;
  else
    bucket = allocate_bucket(table, key, NULL);

  table->buckets[HASH_TO_ARRAY_INDEX(h, mask)] = bucket;

  table->count++;

  return bucket;
}

Scheme_Bucket *
scheme_bucket_or_null_from_table_w_key_wraps (Scheme_Bucket_Table *table, const char *key, int add,
                                              Scheme_Object *key_wraps)
{
  Scheme_Bucket *b;

  b = get_bucket(table, key, add, NULL, key_wraps);

  return b;
}

Scheme_Bucket *
scheme_bucket_or_null_from_table (Scheme_Bucket_Table *table, const char *key, int add)
{
  return scheme_bucket_or_null_from_table_w_key_wraps(table, key, add, NULL);
}

Scheme_Bucket *
scheme_bucket_from_table (Scheme_Bucket_Table *table, const char *key)
{
  return scheme_bucket_or_null_from_table_w_key_wraps(table, key, 1, NULL);
}

void 
scheme_add_to_table_w_key_wraps (Scheme_Bucket_Table *table, const char *key, void *val, 
                                 int constant, Scheme_Object *key_wraps)
{
  Scheme_Bucket *b;

  b = get_bucket(table, key, 1, NULL, key_wraps);

  if (val)
    b->val = val;
  if (constant && table->with_home)
    ((Scheme_Bucket_With_Flags *)b)->flags |= GLOB_IS_CONST;
}

void 
scheme_add_to_table (Scheme_Bucket_Table *table, const char *key, void *val, 
		     int constant)
{
  scheme_add_to_table_w_key_wraps(table, key, val, constant, NULL);
}

void scheme_add_bucket_to_table(Scheme_Bucket_Table *table, Scheme_Bucket *b)
{
  get_bucket(table, table->weak ? (char *)HT_EXTRACT_WEAK(b->key) : b->key, 1, b, NULL);
}

void *
scheme_lookup_in_table_w_key_wraps (Scheme_Bucket_Table *table, const char *key,
                                    Scheme_Object *key_wraps,
				    GC_CAN_IGNORE Scheme_Object **_interned_key)
{
  Scheme_Bucket *bucket;

  bucket = get_bucket(table, key, 0, NULL, key_wraps);

  if (bucket) {
    if (_interned_key) {
      if (table->weak)
	*_interned_key = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
  else
	*_interned_key = (Scheme_Object *)bucket->key;
    }
    return bucket->val;
  } else {
    return NULL;
}
}

void *
scheme_lookup_in_table (Scheme_Bucket_Table *table, const char *key)
{
  return scheme_lookup_in_table_w_key_wraps(table, key, NULL, NULL);
}

Scheme_Object *
scheme_lookup_key_in_table (Scheme_Bucket_Table *table, const char *key)
{
  Scheme_Object *interned_key, *v;

  v = scheme_lookup_in_table_w_key_wraps(table, key, NULL, &interned_key);
  if (v)
    return interned_key;
  else
    return NULL;
}

void
scheme_change_in_table (Scheme_Bucket_Table *table, const char *key, void *naya)
{
  Scheme_Bucket *bucket;

  bucket = get_bucket(table, key, 0, NULL, NULL);

  if (bucket)
    bucket->val = naya;
}

int scheme_bucket_table_equal_rec(Scheme_Bucket_Table *t1, Scheme_Object *orig_t1,
                                  Scheme_Bucket_Table *t2, Scheme_Object *orig_t2,
                                  void *eql)
{
  Scheme_Bucket **buckets, *bucket;
  Scheme_Object *key, *val1, *val2;
  int i, weak, checked = 0;

  /* We can't compare the count values, because they're merely
     >= the number of mapped keys */

  if ((t1->weak != t2->weak)
      || (t1->make_hash_indices != t2->make_hash_indices)
      || (t1->compare != t2->compare))
    return 0;
  
  buckets = t1->buckets;
  weak = t1->weak;

  for (i = t1->size; i--; ) {
    bucket = buckets[i];
    if (bucket) {
      if (weak) {
	key = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
      } else {
	key = (Scheme_Object *)bucket->key;
      }
      if (key) {
        if (!SAME_OBJ((Scheme_Object *)t1, orig_t1))
          val1 = scheme_chaperone_hash_traversal_get(orig_t1, key, &key);
        else
          val1 = (Scheme_Object *)bucket->val;

	checked++;
      
        if (!SAME_OBJ((Scheme_Object *)t2, orig_t2))
          val2 = scheme_chaperone_hash_get(orig_t2, key);
        else
          val2 = (Scheme_Object *)scheme_lookup_in_table(t2, (const char *)key);

	if (!val2)
	  return 0;
	if (!scheme_recur_equal(val1, val2, eql))
	  return 0;
      }
    }
  }

  /* If count is checked, then all buckets must be for mapped keys. */
  if (t2->count == checked)
    return 1;

  /* Need to see whether "t2" maps exactly "checked" keys */
  buckets = t2->buckets;
  weak = t2->weak;
  for (i = t2->size; i--; ) {
    bucket = buckets[i];
    if (bucket) {
      if (weak) {
	key = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
      } else {
	key = (Scheme_Object *)bucket->key;
      }
      if (key) {
	if (!checked)
	  return 0;
	--checked;
      }
    }
  }

  return !checked;
}

int scheme_bucket_table_equal(Scheme_Bucket_Table *t1, Scheme_Bucket_Table *t2)
{
  return scheme_equal((Scheme_Object *)t1, (Scheme_Object *)t2);
}

Scheme_Bucket_Table *scheme_clone_bucket_table(Scheme_Bucket_Table *bt)
{
  Scheme_Bucket_Table *table;
  size_t asize;

  table = MALLOC_ONE_TAGGED(Scheme_Bucket_Table);
  table->so.type = scheme_bucket_table_type;
  table->size = bt->size;
  table->count = bt->count;
  table->weak = bt->weak;
  table->with_home = 0;
  table->make_hash_indices = bt->make_hash_indices;
  table->compare = bt->compare;
  if (bt->mutex) {
    Scheme_Object *sema;
    sema = scheme_make_sema(1);
    table->mutex = sema;
  }
  {
    Scheme_Bucket **ba, *bucket;
    int i;
    asize = (size_t)table->size * sizeof(Scheme_Bucket *);
    ba = (Scheme_Bucket **)scheme_malloc(asize);
    table->buckets = ba;
    memcpy(ba, bt->buckets, asize);
    /* clone individual buckets */
    for (i = table->size; i--; ) {
      bucket = ba[i];
      if (bucket) {
        if (bucket->key) {
          if (table->weak) {
            void *hk = (void *)HT_EXTRACT_WEAK(bucket->key);
            if (hk)
              bucket = allocate_bucket(table, hk, bucket->val);
          } else
            bucket = allocate_bucket(table, bucket->key, bucket->val);
          ba[i] = bucket;
        }
      }
    }
  }

  return table;
}

Scheme_Object *scheme_bucket_table_next(Scheme_Bucket_Table *hash,
					mzlonglong start)
{
  Scheme_Bucket *bucket;
  int i, sz = hash->size;
    
  if (start >= 0) {
    bucket = ((start < sz) ? hash->buckets[start] : NULL);
    if (!bucket || !bucket->val || !bucket->key) 
      return NULL;      
  }
  for (i = start + 1; i < sz; i++) {
    bucket = hash->buckets[i];
    if (bucket && bucket->val && bucket->key) {
      return scheme_make_integer(i);
    }
  }

  return scheme_false;
}

int scheme_bucket_table_index(Scheme_Bucket_Table *hash, mzlonglong pos, Scheme_Object **_key, Scheme_Object **_val)
{
  Scheme_Bucket *bucket;

  if (pos < hash->size) {
    bucket = hash->buckets[pos];
    if (bucket && bucket->val && bucket->key) {
      if (hash->weak)
	*_key = (Scheme_Object *)HT_EXTRACT_WEAK(bucket->key);
      else
	*_key = (Scheme_Object *)bucket->key;
      if (_val)
	*_val = (Scheme_Object *)bucket->val;
      return 1;
    }
  }
  
  return 0;
}

/*========================================================================*/
/*                         precise GC hashing                             */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

void scheme_init_hash_key_procs(void)
{
  register_traversers();
}

intptr_t scheme_hash_key(Scheme_Object *o)
{
  return to_signed_hash(PTR_TO_LONG(o));
}

END_XFORM_SKIP;

#endif

/*========================================================================*/
/*                           equal? hashing                               */
/*========================================================================*/

typedef struct Hash_Info {
  intptr_t depth; /* always odd */
  Scheme_Object *recur;
  Scheme_Object *insp; /* obtained lazily */
} Hash_Info;

static uintptr_t equal_hash_key(Scheme_Object *o, uintptr_t k, Hash_Info *hi);
static uintptr_t equal_hash_key2(Scheme_Object *o, Hash_Info *hi);

/* Based on Bob Jenkins's one-at-a-time hash function at
   http://www.burtleburtle.net/bob/hash/doobs.html: */
#define MZ_MIX(k) (k += (k << 10), k ^= (k >> 6))


static Scheme_Object *hash_recur(int argc, Scheme_Object **argv, Scheme_Object *prim)
{
  intptr_t v;
  Hash_Info *hi;

  hi = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  hi->depth += 2;
  hi->insp = NULL; /* in case recursive call is `parameterize'd */

  v = to_signed_hash(equal_hash_key(argv[0], 0, hi));

  hi->insp = NULL;
  
  return scheme_make_integer(v);
}

static Scheme_Object *hash_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;
  Hash_Info *hi = (Hash_Info *)p->ku.k.p2;
  uintptr_t nv;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  
  nv = equal_hash_key(v, to_unsigned_hash(p->ku.k.i1), hi);

  return scheme_make_integer_value(to_signed_hash(nv));
}

static uintptr_t overflow_equal_hash_key(Scheme_Object *o, uintptr_t k, Hash_Info *hi)
{
  Scheme_Object *nv;
  intptr_t val;
  Hash_Info *hi2;
  Scheme_Thread *p = scheme_current_thread;

  hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
  memcpy(hi2, hi, sizeof(Hash_Info));

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)hi2;
  p->ku.k.i1 = to_signed_hash(k);

  nv = scheme_handle_stack_overflow(hash_k);
  scheme_get_int_val(nv, &val);

  memcpy(hi, hi2, sizeof(Hash_Info));

  return to_unsigned_hash(val);
}

XFORM_NONGCING static uintptr_t dbl_hash_val(double d) 
  XFORM_SKIP_PROC
{
  const umzlonglong m = (umzlonglong)0x880355f21e6d1965;
  umzlonglong h = (16 * m);
  umzlonglong v;

  if (MZ_IS_NAN(d))
    return 0;

  MZ_ASSERT(sizeof(d) == sizeof(v));

  memcpy(&v, &d, sizeof(umzlonglong));

  /* based one https://code.google.com/archive/p/fast-hash/ */
#define MZ_MIX_FOR_DBL(h) \
  (h) ^= (h) >> 23;       \
  (h) *= (umzlonglong)0x2127599bf4325c37; \
  (h) ^= (h) >> 47;

  MZ_MIX_FOR_DBL(v);
  h ^= v;
  h *= m;

  MZ_MIX_FOR_DBL(h);
  
  return (uintptr_t)h;
}

XFORM_NONGCING static uintptr_t dbl_hash2_val(double d)  
  XFORM_SKIP_PROC
{
  return dbl_hash_val(d) >> (sizeof(uintptr_t) / 2);
}

#ifdef MZ_LONG_DOUBLE
XFORM_NONGCING static uintptr_t long_dbl_hash_val(long_double d) 
  XFORM_SKIP_PROC
{
  char s[LONG_DOUBLE_BYTE_LEN];
  int i;
  uintptr_t k;

  if (MZ_IS_LONG_NAN(d))
    return 0;

  /* Like `extfl->floating-point-bytes`, we assume that the
     content of a `long double' occupies the first 10 bytes: */
  memcpy(s, &d, LONG_DOUBLE_BYTE_LEN);

  k = 0;
  for (i = LONG_DOUBLE_BYTE_LEN; i--; ) {
    k += s[i];
    MZ_MIX(k);
  }

  return k;  
}

XFORM_NONGCING static uintptr_t long_dbl_hash2_val(long_double d)  
  XFORM_SKIP_PROC
{
  return long_dbl_hash_val(d) >> (sizeof(uintptr_t) / 2);
}
#endif

#define OVERFLOW_HASH() overflow_equal_hash_key(o, k - t, hi)

XFORM_NONGCING static uintptr_t fast_equal_hash_key(Scheme_Object *o, uintptr_t k, int *_done)
/* must cover eqv hash keys that are just eq hash keys */
{
  Scheme_Type t;

 top:
  t = SCHEME_TYPE(o);
  
  switch(t) {
  case scheme_true_type:
    return k + PTR_TO_LONG(o);
    break;
  case scheme_false_type:
    return k + PTR_TO_LONG(o);
    break;
  case scheme_integer_type:
    {
      uintptr_t iv = to_unsigned_hash(SCHEME_INT_VAL(o));
      return k + iv;
    }
#ifdef MZ_USE_SINGLE_FLOATS
  case scheme_float_type:
    {
      return k + dbl_hash_val(SCHEME_FLT_VAL(o));
    }
#endif
  case scheme_double_type:
    {
      return k + dbl_hash_val(SCHEME_DBL_VAL(o));
    }
#ifdef MZ_LONG_DOUBLE
  case scheme_long_double_type:
    {
      return k + long_dbl_hash_val(SCHEME_LONG_DBL_VAL(o));
    }
#endif
  case scheme_bignum_type:
    {
      int i = SCHEME_BIGLEN(o);
      bigdig *d = SCHEME_BIGDIG(o);
      uintptr_t k2;
      
      k2 = k;
      while (i--) {
	k2 = (k2 << 3) + k2 + (uintptr_t)d[i];
      }
    
      return k2;
    }
    break;
  case scheme_rational_type:
    {
      k += fast_equal_hash_key(scheme_rational_numerator(o), 0, _done);
      o = scheme_rational_denominator(o);
      break;
    }
  case scheme_complex_type:
    {
      Scheme_Complex *c = (Scheme_Complex *)o;
      k += fast_equal_hash_key(c->r, 0, _done);
      o = c->i;
      break;
    }
  case scheme_cpointer_type:
    {
      k = (k << 3) + k;
      k += (uintptr_t)((char *)SCHEME_CPTR_VAL(o) + SCHEME_CPTR_OFFSET(o));
      return k;
    }
  case scheme_char_type:
    return k + SCHEME_CHAR_VAL(o);
  case scheme_byte_string_type:
  case scheme_unix_path_type:
  case scheme_windows_path_type:
    {
      int i = SCHEME_BYTE_STRLEN_VAL(o);
      char *s = SCHEME_BYTE_STR_VAL(o);
      
      while (i--) {
	k += s[i];
        MZ_MIX(k);
      }
      
      return k;
    }
  case scheme_char_string_type:
    {
      int i = SCHEME_CHAR_STRLEN_VAL(o);
      mzchar *s = SCHEME_CHAR_STR_VAL(o);
      
      while (i--) {
	k += s[i];
        MZ_MIX(k);
      }
      
      return k;
    }
  case scheme_regexp_type:
    {
      o = scheme_regexp_source(o);
      break;
    }
# ifndef MZ_PRECISE_GC
  case scheme_keyword_type:
  case scheme_symbol_type:
    {
      Scheme_Symbol *s = (Scheme_Symbol *)o;
      if (!(MZ_OPT_HASH_KEY(&s->iso) & 0x1)) {
	/* Interned. Make key depend only on the content. */
	if (!(MZ_OPT_HASH_KEY(&s->iso) & 0xFFFC)) {
	  int i;
          unsigned int h = 0;
	  for (i = s->len; i--; ) {
	    h += (h << 5) + h + (unsigned int)s->s[i];
	  }
	  h += (h << 2);
	  if (!(((short)h) & 0xFFFC))
	    h = 0x10;
	  MZ_OPT_HASH_KEY(&s->iso) |= (((short)h) & 0xFFFC);
	}
	
	return k + (MZ_OPT_HASH_KEY(&s->iso) & 0xFFFC);
      } else
	return k + PTR_TO_LONG(o);
    }
# else
  case scheme_keyword_type:
  case scheme_symbol_type:
    return k + PTR_TO_LONG(o);
# endif
  default:
    {
      *_done = 0;
      return 0;
    }
  }

  MZ_MIX(k);
  goto top;
}

static uintptr_t equal_hash_key(Scheme_Object *o, uintptr_t k, Hash_Info *hi)
{
  Scheme_Type t;
  Scheme_Object *orig_obj;
  int done;
  uintptr_t k2;

 top:
  orig_obj = o;
  if (SCHEME_CHAPERONEP(o))
    o = ((Scheme_Chaperone *)o)->val;
  
  t = SCHEME_TYPE(o);
  if (t == scheme_hash_tree_indirection_type) {
    if (SAME_OBJ(o, orig_obj)) {
      o = (Scheme_Object *)scheme_hash_tree_resolve_placeholder((Scheme_Hash_Tree *)o);
      orig_obj = o;
    } else {
      o = (Scheme_Object *)scheme_hash_tree_resolve_placeholder((Scheme_Hash_Tree *)o);
    }
    t = SCHEME_TYPE(o);
  }
  
  k += t;

  if (hi->depth > (MAX_HASH_DEPTH << 1))
    return k;

  done = 1;
  k2 = fast_equal_hash_key(o, k, &done);
  if (done)
    return k2;
  
  switch(t) {
  case scheme_pair_type:
    {
#     include "mzhashchk.inc"
      hi->depth += 2;
      k = (k << 3) + k;
      k += equal_hash_key(SCHEME_CAR(o), 0, hi);
      o = SCHEME_CDR(o);
      /* If it continues as a list, don't count cdr direction as depth: */
      if (SCHEME_PAIRP(o) && scheme_is_list(o))
        hi->depth -= 2;
      break;
    }
  case scheme_mutable_pair_type:
    {
#     include "mzhashchk.inc"
      hi->depth += 2;
      k = (k << 3) + k;
      k += equal_hash_key(SCHEME_CAR(o), 0, hi);
      o = SCHEME_CDR(o);
      break;
    }
  case scheme_vector_type:
  case scheme_fxvector_type:
    {
      int len = SCHEME_VEC_SIZE(o), i, val;
      Scheme_Object *elem;
#     include "mzhashchk.inc"

      if (!len)
	return k + 1;
      
      hi->depth += 2;
      --len;
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
        if (SAME_OBJ(o, orig_obj))
          elem = SCHEME_VEC_ELS(o)[i];
        else
          elem = scheme_chaperone_vector_ref(orig_obj, i);
	val = equal_hash_key(elem, 0, hi);
	k = (k << 5) + k + val;
      }
      
      if (SAME_OBJ(o, orig_obj))
        o = SCHEME_VEC_ELS(o)[len];
      else
        o = scheme_chaperone_vector_ref(orig_obj, len);

      break;
    }
  case scheme_flvector_type:
    {
      intptr_t len = SCHEME_FLVEC_SIZE(o), i;
      double d;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_FLVEC_ELS(o)[i];
        k = (k << 5) + k + dbl_hash_val(d);
      }
      
      return k;
    }
#ifdef MZ_LONG_DOUBLE
  case scheme_extflvector_type:
    {
      intptr_t len = SCHEME_EXTFLVEC_SIZE(o), i;
      long_double d;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_EXTFLVEC_ELS(o)[i];
        k = (k << 5) + k + long_dbl_hash_val(d);
      }
      
      return k;
    }
#endif
  case scheme_structure_type:
  case scheme_proc_struct_type:
    {
      Scheme_Object *procs;

      procs = scheme_struct_type_property_ref(scheme_equal_property, orig_obj);
      if (procs) {
        Scheme_Object *a[2], *recur, *v;
        Hash_Info *hi2;

#       include "mzhashchk.inc"

        /* Create/cache closure to use for recursive hashing: */
        if (hi->recur) {
          recur = hi->recur;
          hi2 = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(recur)[0];
        } else {
          hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
          a[0] = (Scheme_Object *)hi2;
          recur = scheme_make_prim_closure_w_arity(hash_recur,
                                                   1, a,
                                                   "equal-hash-code/recur",
                                                   1, 1);
          hi->recur = recur;
        }
        memcpy(hi2, hi, sizeof(Hash_Info));

        a[0] = orig_obj;
        a[1] = recur;
        
        procs = SCHEME_VEC_ELS(procs)[2];
        
        v = _scheme_apply(procs, 2, a);

        if (SCHEME_INTP(v))
          return k + SCHEME_INT_VAL(v);
        else if (SCHEME_BIGNUMP(v)) {
          return k + (uintptr_t)((Scheme_Bignum *)v)->digits[0];
        } else {
          scheme_contract_error("equal-hash-code",
                                "hash procedure returned a value other than an exact integer",
                                "resul1", 1, v,
                                NULL);
          return 0;
        }
      } else {
        Scheme_Object *insp;
        if (scheme_struct_is_transparent(o))
          insp = NULL;
        else {
          insp = hi->insp;
          if (!insp) {
            insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
            hi->insp = insp;
          }
        }
        if (!insp || scheme_inspector_sees_part(o, insp, -2)) {
          int i;
          Scheme_Structure *s1 = (Scheme_Structure *)o;
          Scheme_Object *elem;
	
#         include "mzhashchk.inc"
	
          hi->depth += 2;

          /* Use structure-type names (especially for prefabs): */
          for (i = s1->stype->name_pos+1; i--; ) {
            k += equal_hash_key(s1->stype->parent_types[i]->name, 0, hi);
            MZ_MIX(k);
          }

          for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
            if (SAME_OBJ(o, orig_obj))
              elem = s1->slots[i];
            else
              elem = scheme_struct_ref(orig_obj, i);
            k += equal_hash_key(elem, 0, hi);
            MZ_MIX(k);
          }
	
          return k;
        } else
          return k + PTR_TO_LONG(o);
      }
    }
  case scheme_box_type:
    {
      SCHEME_USE_FUEL(1);
      k += 1;
      if (SAME_OBJ(o, orig_obj))
        o = SCHEME_BOX_VAL(o);
      else
        o = scheme_unbox(orig_obj);
      hi->depth += 2;
      break;
    }
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys, *key, *val;
      int i;
      uintptr_t vk;
      intptr_t old_depth;

#     include "mzhashchk.inc"

      /* mult to counteract potential explosion due to old_depth reset */
#     define GROW_RESET_DEPTH(n) ((n * 2) + 1)

      k = (k << 1) + 3;
      hi->depth = GROW_RESET_DEPTH(hi->depth);
      old_depth = hi->depth;
      
      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
          key = keys[i];
          if (SAME_OBJ(o, orig_obj))
            val = vals[i];
          else
            val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
          vk = equal_hash_key(key, 0, hi);
          MZ_MIX(vk);
	  vk += equal_hash_key(val, 0, hi);
          MZ_MIX(vk);
          k += vk;  /* can't mix k, because the key order shouldn't matter */
          hi->depth = old_depth; /* also needed to avoid order-sensitivity */
	}
      }
      
      return k;
    }
  case scheme_hash_tree_type:
  case scheme_eq_hash_tree_type:
  case scheme_eqv_hash_tree_type:
  case scheme_hash_tree_indirection_type:
    {
      Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)o;
      Scheme_Object *ik, *iv;
      int i;
      uintptr_t vk;
      intptr_t old_depth;

#     include "mzhashchk.inc"

      k = (k << 1) + 3;
      hi->depth = GROW_RESET_DEPTH(hi->depth);
      old_depth = hi->depth;

      /* hash tree holds pre-computed hashes for keys, so use those: */
      vk = scheme_hash_tree_key_hash(ht);

      for (i = scheme_hash_tree_next(ht, -1); i != -1; i = scheme_hash_tree_next(ht, i)) {
        scheme_hash_tree_index(ht, i, &ik, &iv);
        if (!SAME_OBJ(o, orig_obj))
          iv = scheme_chaperone_hash_traversal_get(orig_obj, ik, &ik);
        /* vk = equal_hash_key(ik, 0, hi); */
        /* MZ_MIX(vk); */
        vk += equal_hash_key(iv, 0, hi);
        MZ_MIX(vk);
        k += vk;  /* can't mix k, because the key order shouldn't matter */
        hi->depth = old_depth; /* also needed to avoid order-sensitivity */
      }
      
      return k;
    }
  case scheme_bucket_table_type:
    {
      Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)o;
      Scheme_Bucket **buckets, *bucket;
      const char *_key;
      Scheme_Object *key, *val;
      int i, weak;
      uintptr_t vk;
      intptr_t old_depth;
  
#    include "mzhashchk.inc"

      buckets = ht->buckets;
      weak = ht->weak;
      hi->depth = GROW_RESET_DEPTH(hi->depth);
      old_depth = hi->depth;
      
      k = (k << 1) + 7;
      
      for (i = ht->size; i--; ) {
	bucket = buckets[i];
	if (bucket) {
	  if (weak)
	    _key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	  else
	    _key = bucket->key;
	  if (_key) {
            key = (Scheme_Object *)_key;
            if (SAME_OBJ(o, orig_obj))
              val = (Scheme_Object *)bucket->val;
            else
              val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
	    vk = equal_hash_key(val, 0, hi);
            MZ_MIX(vk);
	    vk += equal_hash_key(key, 0, hi);
            MZ_MIX(vk);
            k += vk; /* can't mix k, because the key order shouldn't matter */
            hi->depth = old_depth; /* also needed to avoid order-sensitivity */
	  }
	}
      }
      
      return k;
    }
  case scheme_place_bi_channel_type:
    {
      k += 7;
      /* a bi channel has sendch and recvch, but
         sends are the same iff recvs are the same: */
      o = (Scheme_Object *)((Scheme_Place_Bi_Channel *)o)->link->sendch;
    }
    break;
  default:
    {
      Scheme_Primary_Hash_Proc h1 = scheme_type_hash1s[t];
      if (h1)
        return h1(o, k, hi);
      else
        return k + PTR_TO_LONG(o);
    }
  }

  MZ_MIX(k);
  goto top;
}

static intptr_t slow_equal_hash_key(Scheme_Object *o)
{
  Hash_Info hi;

  hi.depth = 1;
  hi.recur = NULL;
  hi.insp = NULL;

  return to_signed_hash(equal_hash_key(o, 0, &hi));
}

intptr_t scheme_equal_hash_key(Scheme_Object *o)
{
  uintptr_t k;
  int done = 1;

  k = fast_equal_hash_key(o, SCHEME_TYPE(o), &done);
  if (done)
    return to_signed_hash(k);
  else
    return slow_equal_hash_key(o);
}

intptr_t scheme_equal_hash_key2(Scheme_Object *o)
{
  Hash_Info hi;

  hi.depth = 1;
  hi.recur = NULL;
  hi.insp = NULL;

  return to_signed_hash(equal_hash_key2(o, &hi));
}

XFORM_NONGCING static uintptr_t fast_equal_hash_key2(Scheme_Object *o, int *_done)
/* must cover eqv hash keys that are just eq hash keys */
{
  Scheme_Type t;

  t = SCHEME_TYPE(o);

  *_done = 1;

  switch(t) {
  case scheme_true_type:
    return 1;
  case scheme_false_type:
    return 2;
  case scheme_integer_type:
    return (uintptr_t)t - SCHEME_INT_VAL(o);
#ifdef MZ_USE_SINGLE_FLOATS
  case scheme_float_type:
    {
      return dbl_hash2_val(SCHEME_FLT_VAL(o));
    }
#endif
  case scheme_double_type:
    {
      return dbl_hash2_val(SCHEME_DBL_VAL(o));
    }
#ifdef MZ_LONG_DOUBLE
  case scheme_long_double_type:
    {
      return long_dbl_hash2_val(SCHEME_LONG_DBL_VAL(o));
    }
#endif
  case scheme_bignum_type:
    return ((uintptr_t)SCHEME_BIGDIG(o)[0]
            + (uintptr_t)SCHEME_BIGDIG(o)[SCHEME_BIGLEN(o)-1]);
  case scheme_rational_type:
    return fast_equal_hash_key2(scheme_rational_numerator(o), _done);
  case scheme_complex_type:
    {
      uintptr_t v1, v2;
      Scheme_Complex *c = (Scheme_Complex *)o;
      v1 = fast_equal_hash_key2(c->r, _done);
      v2 = fast_equal_hash_key2(c->i, _done);
      return v1 + v2;
    }
  case scheme_char_type:
    return t;
  default:
    *_done = 0;
    return 0;
  }
}

static Scheme_Object *hash2_recur(int argc, Scheme_Object **argv, Scheme_Object *prim)
{
  intptr_t v;
  Hash_Info *hi;

  hi = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(prim)[0];
  hi->depth += 2;

  v = to_signed_hash(equal_hash_key2(argv[0], hi));
  
  return scheme_make_integer(v);
}

static Scheme_Object *hash2_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *v = (Scheme_Object *)p->ku.k.p1;
  Hash_Info *hi = (Hash_Info *)p->ku.k.p2;
  intptr_t nv;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  
  nv = to_signed_hash(equal_hash_key2(v, hi));

  return scheme_make_integer(nv);
}

static intptr_t overflow_equal_hash_key2(Scheme_Object *o, Hash_Info *hi)
{
  Scheme_Object *nv;
  intptr_t val;
  Hash_Info *hi2;
  Scheme_Thread *p = scheme_current_thread;

  hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
  memcpy(hi2, hi, sizeof(Hash_Info));

  p->ku.k.p1 = (void *)o;
  p->ku.k.p2 = (void *)hi2;

  nv = scheme_handle_stack_overflow(hash2_k);
  scheme_get_int_val(nv, &val);

  memcpy(hi, hi2, sizeof(Hash_Info));

  return val;
}

#undef OVERFLOW_HASH
#define OVERFLOW_HASH() overflow_equal_hash_key2(o, hi)

static uintptr_t equal_hash_key2(Scheme_Object *o, Hash_Info *hi)
{
  Scheme_Type t;
  Scheme_Object *orig_obj;
  uintptr_t r;
  int done;

 top:
  orig_obj = o;
  if (SCHEME_CHAPERONEP(o))
    o = ((Scheme_Chaperone *)o)->val;

  r = fast_equal_hash_key2(o, &done);
  if (done)
    return r;

  t = SCHEME_TYPE(o);

  if (hi->depth > (MAX_HASH_DEPTH << 1))
    return t;

  switch(t) {
  case scheme_pair_type:
    {
      uintptr_t v1, v2;
#     include "mzhashchk.inc"
      hi->depth += 2;
      v1 = equal_hash_key2(SCHEME_CAR(o), hi);
      v2 = equal_hash_key2(SCHEME_CDR(o), hi);
      return v1 + v2;
    }
  case scheme_mutable_pair_type:
    {
      uintptr_t v1, v2;
#     include "mzhashchk.inc"
      hi->depth += 2;
      v1 = equal_hash_key2(SCHEME_CAR(o), hi);
      v2 = equal_hash_key2(SCHEME_CDR(o), hi);
      return v1 + v2;
    }
  case scheme_cpointer_type:
    {
      return (uintptr_t)((char *)SCHEME_CPTR_VAL(o) + SCHEME_CPTR_OFFSET(o));
    }
  case scheme_vector_type:
  case scheme_fxvector_type:
    {
      int len = SCHEME_VEC_SIZE(o), i;
      uintptr_t k = 0;
      Scheme_Object *elem;

#     include "mzhashchk.inc"

      hi->depth += 2;

      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
        if (SAME_OBJ(o, orig_obj))
          elem = SCHEME_VEC_ELS(o)[i];
        else
          elem = scheme_chaperone_vector_ref(orig_obj, i);
	k += equal_hash_key2(elem, hi);
      }
      
      return k;
    }
  case scheme_flvector_type:
    {
      intptr_t len = SCHEME_FLVEC_SIZE(o), i;
      double d;
      uintptr_t k = 0;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_FLVEC_ELS(o)[i];
        k = (k << 5) + k + dbl_hash2_val(d);
      }
      
      return k;
    }
#ifdef MZ_LONG_DOUBLE
  case scheme_extflvector_type:
    {
      intptr_t len = SCHEME_EXTFLVEC_SIZE(o), i;
      long_double d;
      uintptr_t k = 0;

      if (!len)
	return k + 1;
      
      for (i = 0; i < len; i++) {
	SCHEME_USE_FUEL(1);
	d = SCHEME_EXTFLVEC_ELS(o)[i];
        k = (k << 5) + k + long_dbl_hash2_val(d);
      }
      
      return k;
    }
#endif
  case scheme_byte_string_type:
  case scheme_unix_path_type:
  case scheme_windows_path_type:
    {
      int k = 0, i = SCHEME_BYTE_STRLEN_VAL(o);
      char *s = SCHEME_BYTE_STR_VAL(o);
    
      while (i--) {
	k += s[i];
      }
    
      return k;
    }
  case scheme_char_string_type:
    {
      int k = 0, i = SCHEME_CHAR_STRLEN_VAL(o);
      mzchar *s = SCHEME_CHAR_STR_VAL(o);
    
      while (i--) {
	k += s[i];
      }
    
      return k;
    }
  case scheme_regexp_type:
    {
      o = scheme_regexp_source(o);
      goto top;
    }
  case scheme_structure_type:
  case scheme_proc_struct_type:
    {
      Scheme_Object *procs;

      procs = scheme_struct_type_property_ref(scheme_equal_property, orig_obj);
      if (procs) {
        Scheme_Object *a[2], *v, *recur;
        Hash_Info *hi2;

#       include "mzhashchk.inc"

        /* Create/cache closure to use for recursive hashing: */
        if (hi->recur) {
          recur = hi->recur;
          hi2 = (Hash_Info *)SCHEME_PRIM_CLOSURE_ELS(recur)[0];
        } else {
          hi2 = (Hash_Info *)scheme_malloc(sizeof(Hash_Info));
          a[0] = (Scheme_Object *)hi2;
          recur = scheme_make_prim_closure_w_arity(hash2_recur,
                                                   1, a,
                                                   "equal-secondary-hash-code/recur",
                                                   1, 1);
          hi->recur = recur;
        }
        memcpy(hi2, hi, sizeof(Hash_Info));
        
        a[0] = orig_obj;
        a[1] = recur;
        
        procs = SCHEME_VEC_ELS(procs)[3];
        
        v = _scheme_apply(procs, 2, a);

        if (SCHEME_INTP(v))
          return SCHEME_INT_VAL(v);
        else if (SCHEME_BIGNUMP(v)) {
          return (uintptr_t)((Scheme_Bignum *)v)->digits[0];
        } else {
          scheme_contract_error("equal-secondary-hash-code",
                                "hash procedure returned a value other than an exact integer",
                                "result", 1, v,
                                NULL);
          return 0;
        }
      } else {
        Scheme_Object *insp;
        if (scheme_struct_is_transparent(o))
          insp = NULL;
        else {
          insp = hi->insp;
          if (!insp) {
            insp = scheme_get_param(scheme_current_config(), MZCONFIG_INSPECTOR);
            hi->insp = insp;
          }
        }
        if (!insp || scheme_inspector_sees_part(o, insp, -2)) {
          int i;
          uintptr_t k = 0;
          Scheme_Structure *s1 = (Scheme_Structure *)o;
          Scheme_Object *elem;
          
#         include "mzhashchk.inc"
	
          hi->depth += 2;

          for (i = SCHEME_STRUCT_NUM_SLOTS(s1); i--; ) {
            if (SAME_OBJ(o, orig_obj))
              elem = s1->slots[i];
            else
              elem = scheme_struct_ref(orig_obj, i);
            k += equal_hash_key2(elem, hi);
          }
          
          return k;
        } else
          return t;
      }
    }
  case scheme_box_type:
    if (SAME_OBJ(o, orig_obj))
      o = SCHEME_BOX_VAL(o);
    else
      o = scheme_unbox(orig_obj);
    hi->depth += 2;
    goto top;
  case scheme_hash_table_type:
    {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)o;
      Scheme_Object **vals, **keys, *key, *val;
      int i;
      uintptr_t k = 0;
      intptr_t old_depth;
      
#     include "mzhashchk.inc"

      hi->depth = GROW_RESET_DEPTH(hi->depth);
      old_depth = hi->depth;

      keys = ht->keys;
      vals = ht->vals;
      for (i = ht->size; i--; ) {
	if (vals[i]) {
          key = keys[i];
          if (SAME_OBJ(o, orig_obj))
            val = vals[i];
          else
            val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
	  k += equal_hash_key2(key, hi);
	  k += equal_hash_key2(val, hi);
          hi->depth = old_depth;
	}
      }
      
      return k;
    }
  case scheme_hash_tree_indirection_type:
    if (!SAME_OBJ(o, orig_obj)) {
      o = (Scheme_Object *)scheme_hash_tree_resolve_placeholder((Scheme_Hash_Tree *)o);
    } else {
      o = (Scheme_Object *)scheme_hash_tree_resolve_placeholder((Scheme_Hash_Tree *)o);
      orig_obj = o;
    }
  case scheme_hash_tree_type: /* ^^^ fallthrough ^^^ */
  case scheme_eq_hash_tree_type:
  case scheme_eqv_hash_tree_type:
    {
      Scheme_Hash_Tree *ht = (Scheme_Hash_Tree *)o;
      Scheme_Object *iv, *ik;
      int i;
      uintptr_t k = 0;
      intptr_t old_depth;
      
#     include "mzhashchk.inc"

      hi->depth = GROW_RESET_DEPTH(hi->depth);
      old_depth = hi->depth;

      /* hash tree holds pre-computed hashes for keys, so use those: */
      k += scheme_hash_tree_key_hash(ht);
      
      for (i = scheme_hash_tree_next(ht, -1); i != -1; i = scheme_hash_tree_next(ht, i)) {
        scheme_hash_tree_index(ht, i, &ik, &iv);
        if (!SAME_OBJ(o, orig_obj))
          iv = scheme_chaperone_hash_traversal_get(orig_obj, ik, &ik);
        k += equal_hash_key2(iv, hi);
        hi->depth = old_depth;
      }
      
      return k;
    }
  case scheme_bucket_table_type:
    {
      Scheme_Bucket_Table *ht = (Scheme_Bucket_Table *)o;
      Scheme_Bucket **buckets, *bucket;
      const char *_key;
      Scheme_Object *key, *val;
      int i, weak;
      uintptr_t k = 0;
      intptr_t old_depth;

#     include "mzhashchk.inc"
  
      buckets = ht->buckets;
      weak = ht->weak;
      
      hi->depth = GROW_RESET_DEPTH(hi->depth);
      old_depth = hi->depth;

      for (i = ht->size; i--; ) {
	bucket = buckets[i];
	if (bucket) {
	  if (weak)
	    _key = (const char *)HT_EXTRACT_WEAK(bucket->key);
	  else
	    _key = bucket->key;
	  if (_key) {
            key = (Scheme_Object *)_key;
            if (SAME_OBJ(o, orig_obj))
              val = (Scheme_Object *)bucket->val;
            else
              val = scheme_chaperone_hash_traversal_get(orig_obj, key, &key);
	    k += equal_hash_key2(val, hi);
	    k += equal_hash_key2(key, hi);
            hi->depth = old_depth;
	  }
	}
      }
    
      return k;
    }
  case scheme_place_bi_channel_type:
    /* a bi channel has sendch and recvch, but
       sends are the same iff recvs are the same: */
    o = (Scheme_Object *)((Scheme_Place_Bi_Channel *)o)->link->sendch;
    goto top;
  default:
    {
      Scheme_Secondary_Hash_Proc h2 = scheme_type_hash2s[t];
      if (h2)
        return h2(o, hi);
      else
        return t;
    }
  }
}

intptr_t scheme_recur_equal_hash_key(Scheme_Object *o, void *cycle_data)
{
  return to_signed_hash(equal_hash_key(o, 0, (Hash_Info *)cycle_data));
}

intptr_t scheme_recur_equal_hash_key2(Scheme_Object *o, void *cycle_data)
{
  return to_signed_hash(equal_hash_key2(o, (Hash_Info *)cycle_data));
}

intptr_t scheme_eqv_hash_key(Scheme_Object *o)
{
  if (!SCHEME_INTP(o) && (SCHEME_NUMBERP(o) || SCHEME_CHARP(o))) {
    int done;
    return to_signed_hash(fast_equal_hash_key(o, 0, &done));
  } else
    return to_signed_hash(PTR_TO_LONG(o));
}

intptr_t scheme_eqv_hash_key2(Scheme_Object *o)
{
  if (!SCHEME_INTP(o) && (SCHEME_NUMBERP(o) || SCHEME_CHARP(o))) {
    int done;
    return to_signed_hash(fast_equal_hash_key2(o, &done));
  } else
    return to_signed_hash(PTR_TO_LONG(o) >> 1);
}

/*========================================================================*/
/*                        functional hash tables                          */
/*========================================================================*/

/* Based on Phil Bagwell's "Ideal Hash Trees" (2000) */

#define HASHTR_KIND_MULT(kind) (!kind ? 1 : ((kind == 1) ? 2 : 3))
#define HASH_TREE_RECORD_SIZE(kind, popcount) (sizeof(Scheme_Hash_Tree)  \
                                               + (((HASHTR_KIND_MULT(kind) * (popcount)) - mzFLEX_DELTA) \
                                                  * sizeof(Scheme_Object*)))

#define HASHTR_HAS_VAL  0x1
#define HASHTR_HAS_CODE 0x2

/* In a hash tree without HASHTR_HAS_VAL, all values are `#t`; we nodes without
  HASHTR_HAS_VAL to nodes with it on demand, but we don't go the other way */
#define NOT_IMPLICIT_VALUE(v) ((v) && !SAME_OBJ(v, scheme_true))

#define HASHTR_SUBTREEP(o) SAME_TYPE(SCHEME_TYPE(o), scheme_hash_tree_subtree_type)
#define HASHTR_COLLISIONP(o) SAME_TYPE(SCHEME_TYPE(o), scheme_hash_tree_collision_type)

/* max log word size depends on `hash_tree_bitmap_t` */
#define mzHAMT_LOG_WORD_SIZE 5
#define mzHAMT_WORD_SIZE (1 << mzHAMT_LOG_WORD_SIZE)

/* Reorder bits to make the HAMT more bushy in the case that sequential
   hash code are added: */
#define HAMT_REORDER(h) (((h) & (HIGH_PART | LOW_LOW_PARTS | (LOW_LOW_PARTS << PART_BIT_COUNT))) \
                         | (((h) & HIGH_LOW_PARTS) << PART_BIT_COUNT)    \
                         | (((h) & (HIGH_LOW_PARTS << PART_BIT_COUNT)) >> PART_BIT_COUNT))
#define HIGH_PART (~((uintptr_t)0x0) - (uintptr_t)0xFFFFFFFF)
#define LOW_LOW_PARTS ((uintptr_t)0x3333)
#define HIGH_LOW_PARTS ((uintptr_t)0xCCCC)
#define PART_BIT_COUNT 16

XFORM_NONGCING static Scheme_Hash_Tree *resolve_placeholder(Scheme_Hash_Tree *ht)
{
  /* This is ugly, but to support cyclic tables, we need a
     level of indirection */
  if (SAME_TYPE(SCHEME_TYPE(ht), scheme_hash_tree_indirection_type))
    return (Scheme_Hash_Tree *)ht->els[0];
  else
    return ht;
}

XFORM_NONGCING static int hamt_index(uintptr_t code, int shift)
{
  return (code >> shift) & ((1 << mzHAMT_LOG_WORD_SIZE) - 1);
}

XFORM_NONGCING static int hamt_popcount(hash_tree_bitmap_t x)
{
#if MZ_HAS_BUILTIN_POPCOUNT
  return __builtin_popcount(x);
#else
  /* http://bits.stephan-brumme.com/countBits.html */
  /* count bits of each 2-bit chunk */
  x  = x - ((x >> 1) & 0x55555555);
  /* count bits of each 4-bit chunk */
  x  = (x & 0x33333333) + ((x >> 2) & 0x33333333);
  /* count bits of each 8-bit chunk */
  x  = x + (x >> 4);
  /* mask out junk */
  x &= 0xF0F0F0F;
  /* add all four 8-bit chunks */
  return (x * 0x01010101) >> 24;
#endif
}

XFORM_NONGCING static int hamt_popcount_below(hash_tree_bitmap_t bitmap, int index)
{
  return hamt_popcount(bitmap & (((hash_tree_bitmap_t)1 << index) - 1));
}

XFORM_NONGCING static hash_tree_bitmap_t hamt_bit(int index)
{
  return ((hash_tree_bitmap_t)1 << index);
}

XFORM_NONGCING Scheme_Object *_mzHAMT_VAL(Scheme_Hash_Tree *ht, int pos, int popcount)
{
  return ((SCHEME_HASHTR_FLAGS(ht) & HASHTR_HAS_VAL) ? (ht)->els[(popcount)+(pos)] : scheme_true);
}

XFORM_NONGCING uintptr_t mzHAMT_KEY_CODE(Scheme_Object *o)
{
  while (1) {
    if (HASHTR_COLLISIONP(o) || HASHTR_SUBTREEP(o))
      o = ((Scheme_Hash_Tree *)o)->els[0];
    else {
      uintptr_t h;
      h = PTR_TO_LONG(o);
      return HAMT_REORDER(h);
    }
  }
}

XFORM_NONGCING uintptr_t _mzHAMT_CODE(Scheme_Hash_Tree *ht, int pos, int popcount)
{
  return ((SCHEME_HASHTR_FLAGS(ht) & HASHTR_HAS_CODE) ? (uintptr_t)(ht)->els[2*(popcount)+(pos)] : mzHAMT_KEY_CODE((ht)->els[pos]));
}

#define mzHAMT_VAL(ht, pos) _mzHAMT_VAL(ht, pos, hamt_popcount((ht)->bitmap))
#define mzHAMT_CODE(ht, pos) _mzHAMT_CODE(ht, pos, hamt_popcount((ht)->bitmap))

#define _mzHAMT_SET_VAL(ht, pos, val, popcount) (ht)->els[(popcount) + (pos)] = val
#define _mzHAMT_SET_CODE(ht, pos, code, popcount) (ht)->els[2*(popcount) + (pos)] = (Scheme_Object *)code

XFORM_NONGCING static void hamt_content_copy(Scheme_Hash_Tree *dest, Scheme_Hash_Tree *src,
                                             int dest_popcount, int src_popcount,
                                             intptr_t dest_start, intptr_t src_start,
                                             intptr_t len)
{
  memcpy(dest->els+dest_start, src->els+src_start, len*sizeof(Scheme_Object*));
  if (SCHEME_HASHTR_FLAGS(src) & HASHTR_HAS_VAL) {
    memcpy(dest->els+dest_popcount+dest_start, src->els+src_popcount+src_start, len*sizeof(Scheme_Object*));
    if (SCHEME_HASHTR_FLAGS(src) & HASHTR_HAS_CODE) {
      memcpy(dest->els+2*dest_popcount+dest_start, src->els+2*src_popcount+src_start, len*sizeof(Scheme_Object*));
    }
  } else if (SCHEME_HASHTR_FLAGS(dest) & HASHTR_HAS_VAL) {
    /* make source's implicit `#t` values explicit in dest */
    while (len--) {
      _mzHAMT_SET_VAL(dest, dest_start+len, scheme_true, dest_popcount);
    }
  }
}

XFORM_NONGCING static Scheme_Hash_Tree *hamt_assoc(Scheme_Hash_Tree *ht, uintptr_t code, int *_pos, int shift)
{
  int index, pos;

  while (1) {
    index = hamt_index(code, shift);
    if (ht->bitmap & hamt_bit(index)) {
      pos = hamt_popcount_below(ht->bitmap, index);
      if (HASHTR_SUBTREEP(ht->els[pos])) {
        ht = (Scheme_Hash_Tree *)ht->els[pos];
        shift += mzHAMT_LOG_WORD_SIZE;
      } else {
        if (code == mzHAMT_CODE(ht, pos)) {
          *_pos = pos;
          return ht;
        } else
          return NULL;
      }
    } else
      return NULL;
  }
  
  return NULL;
}

static Scheme_Hash_Tree *hamt_alloc(int kind, int popcount)
/* be sure to set `bitmap` field before a GC becomes possible */
{
  return (Scheme_Hash_Tree *)scheme_malloc_small_tagged(HASH_TREE_RECORD_SIZE(kind, popcount));
}

static Scheme_Hash_Tree *hamt_dup(Scheme_Hash_Tree *ht, int popcount, int need_value)
{
  Scheme_Hash_Tree *new_ht;
  int kind;

  kind = SCHEME_HASHTR_KIND(ht);
  new_ht = hamt_alloc(kind | (need_value ? HASHTR_HAS_VAL : 0), popcount);
  memcpy(new_ht, ht, HASH_TREE_RECORD_SIZE(kind, popcount));
  
  if (!(kind & HASHTR_HAS_VAL) && need_value) {
    /* make original's implicit `#t` values explicit in the copy */
    int i;
    SCHEME_HASHTR_FLAGS(new_ht) |= HASHTR_HAS_VAL;
    for (i = popcount; i--; ) {
      _mzHAMT_SET_VAL(new_ht, i, scheme_true, popcount);
    } 
  }

  return new_ht;
}

static Scheme_Hash_Tree *hamt_make1(Scheme_Hash_Tree *ht, int index)
/* allocates a node that has a single entry, which is another node */
{
  Scheme_Hash_Tree *new_ht;
  int kind;

  kind = SCHEME_HASHTR_KIND(ht);
  new_ht = hamt_alloc(kind, 1);
  new_ht->iso.so.type = scheme_hash_tree_subtree_type;
  SCHEME_HASHTR_FLAGS(new_ht) = kind;
  new_ht->bitmap = hamt_bit(index);
  new_ht->count = ht->count;
  new_ht->els[0] = (Scheme_Object *)ht;

  return new_ht;
}

static Scheme_Hash_Tree *hamt_make2(int kind, int shift,
                                    uintptr_t code1, Scheme_Object *key1, Scheme_Object *val1,
                                    uintptr_t code2, Scheme_Object *key2, Scheme_Object *val2)
/* allocates a subtree (at the level indicated by `shift`) for two
   values, pushing them down to further subtress as needed */
{
  int index1, index2, pos1, pos2;
  Scheme_Hash_Tree *new_ht;

  index1 = hamt_index(code1, shift);
  index2 = hamt_index(code2, shift);
  if (index1 == index2) {
    /* since hash codes map to the same index at this level,
       we need another level */
    new_ht = hamt_make2(kind, shift + mzHAMT_LOG_WORD_SIZE,
                        code1, key1, val1,
                        code2, key2, val2);
    return hamt_make1(new_ht, index1);
  } else {
    kind = kind | ((NOT_IMPLICIT_VALUE(val1) || NOT_IMPLICIT_VALUE(val2)) ? HASHTR_HAS_VAL : 0);
    new_ht = hamt_alloc(kind, 2);
    new_ht->iso.so.type = scheme_hash_tree_subtree_type;
    SCHEME_HASHTR_FLAGS(new_ht) = kind;
    new_ht->bitmap = (hamt_bit(index1) | hamt_bit(index2));
    new_ht->count = 2;
    if (HASHTR_COLLISIONP(key1))
      new_ht->count += (((Scheme_Hash_Tree *)key1)->count - 1);
    if (HASHTR_COLLISIONP(key2))
      new_ht->count += (((Scheme_Hash_Tree *)key2)->count - 1);
    if (index1 < index2) {
      pos1 = 0;
      pos2 = 1;
    } else {
      pos1 = 1;
      pos2 = 0;
    }
    new_ht->els[pos1] = key1;
    new_ht->els[pos2] = key2;
    if (SCHEME_HASHTR_FLAGS(new_ht) & HASHTR_HAS_VAL) {
      _mzHAMT_SET_VAL(new_ht, pos1, val1, 2);
      _mzHAMT_SET_VAL(new_ht, pos2, val2, 2);
      if (SCHEME_HASHTR_FLAGS(new_ht) & HASHTR_HAS_CODE) {
        _mzHAMT_SET_CODE(new_ht, pos1, code1, 2);
        _mzHAMT_SET_CODE(new_ht, pos2, code2, 2);
      }
    }
    return new_ht;
  }
}

static Scheme_Hash_Tree *hamt_set(Scheme_Hash_Tree *ht, uintptr_t code, int shift,
                                  Scheme_Object *key, Scheme_Object *val, int inc)
/* updates `ht` (at level `shift`) to replace or add the mapping for `code`,
   adjusting the overall count by `inc` */
{
  int index, pos, popcount;
  Scheme_Hash_Tree *new_ht;

  index = hamt_index(code, shift);
  pos = hamt_popcount_below(ht->bitmap, index);
  popcount = hamt_popcount(ht->bitmap);
  
  if (ht->bitmap & hamt_bit(index)) {
    /* Replacing: */
    new_ht = hamt_dup(ht, popcount, NOT_IMPLICIT_VALUE(val));
    if (HASHTR_SUBTREEP(ht->els[pos])) {
      ht = (Scheme_Hash_Tree *)ht->els[pos];
      ht = hamt_set(ht, code, shift + mzHAMT_LOG_WORD_SIZE, key, val, inc);
      new_ht->els[pos] = (Scheme_Object *)ht;
      new_ht->count += inc;
    } else {
      if (code == _mzHAMT_CODE(new_ht, pos, popcount)) {
        new_ht->els[pos] = key;
        if (SCHEME_HASHTR_FLAGS(new_ht) & HASHTR_HAS_VAL)
          _mzHAMT_SET_VAL(new_ht, pos, val, popcount);
        new_ht->count += inc;
      } else {
        /* make a new level */
        ht = hamt_make2(SCHEME_HASHTR_KIND(new_ht), shift + mzHAMT_LOG_WORD_SIZE,
                        _mzHAMT_CODE(new_ht, pos, popcount), new_ht->els[pos], _mzHAMT_VAL(new_ht, pos, popcount),
                        code, key, val);
        new_ht->els[pos] = (Scheme_Object *)ht;
        if (SCHEME_HASHTR_FLAGS(new_ht) & HASHTR_HAS_VAL)
          _mzHAMT_SET_VAL(new_ht, pos, NULL, popcount);
        new_ht->count += inc;
      }
    }
  } else {
    int kind = SCHEME_HASHTR_KIND(ht) | (NOT_IMPLICIT_VALUE(val) ? HASHTR_HAS_VAL : 0);
    new_ht = hamt_alloc(kind, popcount+1);
    memcpy(new_ht, ht, HASH_TREE_RECORD_SIZE(SCHEME_HASHTR_KIND(ht), 0));
    SCHEME_HASHTR_FLAGS(new_ht) |= kind;
    hamt_content_copy(new_ht, ht, popcount+1, popcount, 0, 0, pos);
    if (pos < popcount)
      hamt_content_copy(new_ht, ht, popcount+1, popcount, pos+1, pos, popcount-pos);
    new_ht->bitmap |= hamt_bit(index);
    new_ht->count += inc;
    new_ht->els[pos] = key;
    if (SCHEME_HASHTR_FLAGS(new_ht) & HASHTR_HAS_VAL) {
      _mzHAMT_SET_VAL(new_ht, pos, val, popcount+1);
      if (SCHEME_HASHTR_FLAGS(new_ht) & HASHTR_HAS_CODE)
        _mzHAMT_SET_CODE(new_ht, pos, code, popcount+1);
    }
  }
  
  return new_ht;
}

static Scheme_Hash_Tree *hamt_contract(Scheme_Hash_Tree *ht, int popcount, int index, int pos)
/* return a node that's smaller by one by dropping the mapping at `pos` */
{
  Scheme_Hash_Tree *new_ht;

  if (popcount == 1)
    return NULL;

  new_ht = hamt_alloc(SCHEME_HASHTR_KIND(ht), popcount-1);
  memcpy(new_ht, ht, HASH_TREE_RECORD_SIZE(SCHEME_HASHTR_KIND(ht), 0));
  hamt_content_copy(new_ht, ht, popcount-1, popcount, 0, 0, pos);
  if (pos < popcount-1)
    hamt_content_copy(new_ht, ht, popcount-1, popcount, pos, pos+1, popcount-pos-1);
  new_ht->bitmap -= hamt_bit(index);
  --new_ht->count;

  return new_ht;
}

static Scheme_Hash_Tree *hamt_remove(Scheme_Hash_Tree *ht, uintptr_t code, int shift)
/* remove the mapping for `code`, where `ht` is at the level indicated by `shift` */
{
  int index, pos, popcount;
  Scheme_Hash_Tree *sub_ht;

  index = hamt_index(code, shift);
  if (ht->bitmap & hamt_bit(index)) {
    pos = hamt_popcount_below(ht->bitmap, index);
    popcount = hamt_popcount(ht->bitmap);
    if (!HASHTR_SUBTREEP(ht->els[pos]))
      return hamt_contract(ht, popcount, index, pos);
    else {
      sub_ht = hamt_remove((Scheme_Hash_Tree *)ht->els[pos], code, shift + mzHAMT_LOG_WORD_SIZE);
      if (!SAME_OBJ((Scheme_Object *)sub_ht, ht->els[pos])) {
        if (!sub_ht)
          return hamt_contract(ht, popcount, index, pos);
        ht = hamt_dup(ht, popcount, 0);
        ht->count -= 1;
        if (((sub_ht->count == 1) && !HASHTR_SUBTREEP(sub_ht->els[0]))
            || (HASHTR_COLLISIONP(sub_ht->els[0])
                && (sub_ht->count == ((Scheme_Hash_Tree *)sub_ht->els[0])->count))) {
          /* drop extra layer that has 1 immediate entry */
          ht->els[pos] = sub_ht->els[0];
          if (!(SCHEME_HASHTR_FLAGS(ht) & HASHTR_HAS_VAL) && (SCHEME_HASHTR_FLAGS(sub_ht) & HASHTR_HAS_VAL))
            ht = hamt_dup(ht, popcount, 1);
          if (SCHEME_HASHTR_FLAGS(ht) & HASHTR_HAS_VAL) {
            _mzHAMT_SET_VAL(ht, pos, _mzHAMT_VAL(sub_ht, 0, 1), popcount);
            if (SCHEME_HASHTR_FLAGS(ht) & HASHTR_HAS_CODE)
              _mzHAMT_SET_CODE(ht, pos, _mzHAMT_CODE(sub_ht, 0, 1), popcount);
          }
        } else
          ht->els[pos] = (Scheme_Object *)sub_ht;
        return ht;
      } else
        return ht;
    }
  } else
    return ht;
}

/* this signature is different from other scheme_<hash type>_next fns */
/* but is used else where, so leave as is */
mzlonglong scheme_hash_tree_next(Scheme_Hash_Tree *tree, mzlonglong pos)
{
  mzlonglong i = pos+1;
  if (i == tree->count)
    return -1;
  else
    return i;
}

Scheme_Object *scheme_hash_tree_next_pos(Scheme_Hash_Tree *tree, mzlonglong pos)
{
  mzlonglong i = pos+1;
  if (i == tree->count)
    return scheme_false;
  else
    return scheme_make_integer_value_from_long_long(i);
}

#if REVERSE_HASH_TABLE_ORDER
# define HAMT_TRAVERSE_INIT(popcount) ((popcount)-1)
# define HAMT_TRAVERSE_NEXT(i) ((i)-1)
#else
# define HAMT_TRAVERSE_INIT(popcount) 0
# define HAMT_TRAVERSE_NEXT(i) ((i)+1)
#endif

XFORM_NONGCING static void hamt_subtree_at_index(Scheme_Hash_Tree *ht, mzlonglong pos,
                                                 Scheme_Hash_Tree **_subtree, int *_i, int *_popcount)
{
  int popcount, i;
  Scheme_Hash_Tree *sub;

  while (1) {
    popcount = hamt_popcount(ht->bitmap);
    i = HAMT_TRAVERSE_INIT(popcount);
    while (1) {
      if (HASHTR_SUBTREEP(ht->els[i])
          || HASHTR_COLLISIONP(ht->els[i])) {
        sub = (Scheme_Hash_Tree *)ht->els[i];
        if (pos < sub->count) {
          ht = sub;
          break; /* to outer loop */
        } else
          pos -= sub->count;
      } else {
        if (!pos) {
          *_subtree = ht;
          *_i = i;
          if (_popcount) *_popcount = popcount;
          return;
        }
        --pos;
      }
      i = HAMT_TRAVERSE_NEXT(i);
    }
  }
}

/* We have two different implementations of Scheme_Hash_Tree traversal
   as exposed by `unsafe-immutable-hash-next`, etc.:

   * Consecutive integers from 0 as the index values (the same as
     `scheme_hash_tree_next`), or

   * Subtree lists (for the spine of a subtree up to the root) plus
     offset into a subtree.

   The second one is more direct, but requires some allocation. To
   avoid allocation for small trees, it uses an integer encoding of a
   path.

   The first implementation is better for small trees, and the second
   is better for very large trees, but there's not a big difference.
   Small trees dominate for macro expansion, which is a big use of
   hash trees, so that gives the first implementation the edge.

   Microbenchmark:

    (let loop ([size 2])
      (define ht (for/hasheq ([i (in-range size)])
                   (values i i)))
      (define c (quotient 10000000 size))
      (printf "~s: " size)
      (time
       (for ([j (in-range c)])
         (for/fold ([v #f]) ([k (in-immutable-hash-keys ht)])
           k)))
      (unless (= size (expt 2 20))
        (loop (* size 2))))

 */

#if 1
/* ------ Implementation 1 of hash-tree traversal ------ */

Scheme_Object *scheme_unsafe_hash_tree_start(Scheme_Hash_Tree *ht)
{
  ht = resolve_placeholder(ht);

  if (!ht->count)
    return scheme_false;
  else
    return scheme_make_integer(0);
}

XFORM_NONGCING void scheme_unsafe_hash_tree_subtree(Scheme_Object *obj, Scheme_Object *args,
                                                    Scheme_Hash_Tree **_subtree, int *_i)
{
  Scheme_Hash_Tree *ht;

  if (SCHEME_NP_CHAPERONEP(obj))
    obj = SCHEME_CHAPERONE_VAL(obj);
  ht = (Scheme_Hash_Tree *)obj;
  ht = resolve_placeholder(ht);

  hamt_subtree_at_index(ht, SCHEME_INT_VAL(args), _subtree, _i, NULL);
}

XFORM_NONGCING Scheme_Object *scheme_unsafe_hash_tree_access(Scheme_Hash_Tree *subtree, int i)
{
  return _mzHAMT_VAL(subtree, i, hamt_popcount(subtree->bitmap));
}

Scheme_Object *scheme_unsafe_hash_tree_next(Scheme_Hash_Tree *ht, Scheme_Object *args)
{
  intptr_t i = SCHEME_INT_VAL(args) + 1;
  ht = resolve_placeholder(ht);
  if (i < ht->count)
    return scheme_make_integer(i);
  else
    return scheme_false;
}

#else
/* ------ Implementation 2 of hash-tree traversal ------ */

#define mzHAMT_MAX_INDEX_LEVEL 4 /* For the compressed form of the index */

Scheme_Object *make_index_frame(Scheme_Hash_Tree *ht, intptr_t i, Scheme_Object *rest)
{
  Scheme_Object *vec;
  vec = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(vec)[0] = (Scheme_Object *)ht;
  SCHEME_VEC_ELS(vec)[1] = scheme_make_integer(i);
  SCHEME_VEC_ELS(vec)[2] = rest;
  return vec;
}

#define INDEX_FRAMEP(o) SCHEME_VECTORP(o)
#define INDEX_FRAME_SUBTREE(o) ((Scheme_Hash_Tree *)(SCHEME_VEC_ELS(o)[0]))
#define INDEX_FRAME_INDEX(o) (SCHEME_INT_VAL(SCHEME_VEC_ELS(o)[1]))
#define INDEX_FRAME_REST(o) (SCHEME_VEC_ELS(o)[2])

/* instead of returning a pos, these unsafe iteration ops */
/* return a view into the tree consisting of a: */
/*   - subtree */
/*   - subtree index */
/*   - stack of parent subtrees and indices */
/* For small hashes, it uses a compressed representation as fixnums */
/*   - (#h i0) --> (+ (<< 1 5) i0) */
/*   - (#h i1 #h i0) --> (+ (<< 1 10) (<< i1 5) i0) */
/*   - (#h i2 #h i1 #h i0) --> (+ (<< 1 15) (<< i2 10) (<< i1 5) i0) */
/*   - ...  */
/* This speeds up performance of immutable hash table iteration. */
/* These unsafe ops currently do not support REVERSE_HASH_TABLE_ORDER */
/* to avoid unneeded popcount computations */
Scheme_Object *scheme_unsafe_hash_tree_start(Scheme_Hash_Tree *ht)
{
  Scheme_Object *stack = scheme_null;
  int j, i, i_n[mzHAMT_MAX_INDEX_LEVEL], level;
  Scheme_Hash_Tree *ht_n[mzHAMT_MAX_INDEX_LEVEL];

  ht = resolve_placeholder(ht);

  if (!ht->count)
    return scheme_false;

  i = hamt_popcount(ht->bitmap)-1;
  level = 0;
  
  while ((HASHTR_SUBTREEP(ht->els[i])
          || HASHTR_COLLISIONP(ht->els[i]))) {
    /* go down tree but save return point */
    if (level == -1) {
      stack = make_index_frame(ht, i, stack);
    } else if (level < mzHAMT_MAX_INDEX_LEVEL) {
      ht_n[level] = ht;
      i_n[level] = i;
      level++;
    } else {
      stack = scheme_null;
      for (j = 0; j < mzHAMT_MAX_INDEX_LEVEL; j++) {
        stack = make_index_frame(ht_n[j], i_n[j], stack);
      }
      stack = make_index_frame(ht, i, stack);
      level = -1;
    }
    ht = (Scheme_Hash_Tree *)ht->els[i];
    i = hamt_popcount(ht->bitmap)-1;
  }

  if (level == -1) {
    stack = make_index_frame(ht, i, stack);
    return stack;
  } else {
    i = (1<<mzHAMT_LOG_WORD_SIZE) + i;
    for (j = level-1; j >= 0 ; j--) {
      i = (i<<mzHAMT_LOG_WORD_SIZE) + i_n[j];
    }
    return scheme_make_integer(i);
  }
}

/* args is a (cons subtree (cons subtree-index stack-of-parents))
   or the comppressed representation as a fixnum */
XFORM_NONGCING void scheme_unsafe_hash_tree_subtree(Scheme_Object *obj, Scheme_Object *args,
                                                    Scheme_Hash_Tree **_subtree, int *_i)
{
  Scheme_Hash_Tree *subtree;
  int i;

  if (INDEX_FRAMEP(args)) {
    subtree = INDEX_FRAME_SUBTREE(args);
    i = INDEX_FRAME_INDEX(args);
  } else {
    if (SCHEME_NP_CHAPERONEP(obj))
      subtree = (Scheme_Hash_Tree *)SCHEME_CHAPERONE_VAL(obj);
    else
      subtree = (Scheme_Hash_Tree *)obj;
    subtree = resolve_placeholder(subtree);
    i = SCHEME_INT_VAL(args);
    while (i >= (1<<(2*mzHAMT_LOG_WORD_SIZE))) {
      subtree = (Scheme_Hash_Tree *)subtree->els[i & ((1<<mzHAMT_LOG_WORD_SIZE)-1)];
      i = i >> mzHAMT_LOG_WORD_SIZE;
    }
    i = i & ((1<<mzHAMT_LOG_WORD_SIZE)-1);
  }
  
  *_subtree = subtree;
  *_i =i;
}

XFORM_NONGCING Scheme_Object *scheme_unsafe_hash_tree_access(Scheme_Hash_Tree *subtree, int i)
{
  return _mzHAMT_VAL(subtree, i, hamt_popcount(subtree->bitmap));
}

/* args is a (cons subtree (cons subtree-index stack-of-parents))
   or the comppressed representation as a fixnum */
Scheme_Object *scheme_unsafe_hash_tree_next(Scheme_Hash_Tree *ht, Scheme_Object *args)
{
  Scheme_Object *stack = scheme_null;
  int j, i, i_n[mzHAMT_MAX_INDEX_LEVEL], level;
  Scheme_Hash_Tree *ht_n[mzHAMT_MAX_INDEX_LEVEL];

  if (INDEX_FRAMEP(args)) {
    ht = INDEX_FRAME_SUBTREE(args);
    i = INDEX_FRAME_INDEX(args);
    stack = INDEX_FRAME_REST(args);
    level = -1; /* -1 = too big */
  } else {
    ht = resolve_placeholder(ht);
    i = SCHEME_INT_VAL(args);
    level = 0;
    while (i >= (1<<(2*mzHAMT_LOG_WORD_SIZE))) {
      ht_n[level] = ht;
      i_n[level] = i & ((1<<mzHAMT_LOG_WORD_SIZE)-1);
      ht = (Scheme_Hash_Tree *)ht->els[i_n[level]];
      i = i >> mzHAMT_LOG_WORD_SIZE;
      level++;
    }
    i = i & ((1<<mzHAMT_LOG_WORD_SIZE)-1);
  }

  while (1) {
    if (!i) { /* pop up the tree */
      if (level == -1) {
        ht = INDEX_FRAME_SUBTREE(stack);
        i = INDEX_FRAME_INDEX(stack);
        stack = INDEX_FRAME_REST(stack);
        if (SCHEME_NULLP(stack))
          level = 0;
      } else if (!level) {
        return scheme_false;
      } else {
        level--;
        ht = ht_n[level];
        i = i_n[level];
      }
    } else { /* go to next node */
      i--;
      if (!(HASHTR_SUBTREEP(ht->els[i])
            || HASHTR_COLLISIONP(ht->els[i]))) {
        if (level == -1) {
          stack = make_index_frame(ht, i, stack);
          return stack;
        } else {
          i = (1<<mzHAMT_LOG_WORD_SIZE) + i;
          for (j = level-1; j >= 0 ; j--) {
            i = (i<<mzHAMT_LOG_WORD_SIZE) + i_n[j];
          }
          return scheme_make_integer(i);
        }
      } else { /* go down tree but save return point */
        if (level == -1) {
          stack = make_index_frame(ht, i, stack);
        } else if (level < mzHAMT_MAX_INDEX_LEVEL) {
          ht_n[level] = ht;
          i_n[level] = i;
          level++;
        } else {
          stack = scheme_null;
          for (j = 0; j < mzHAMT_MAX_INDEX_LEVEL; j++) {
            stack = make_index_frame(ht_n[j], i_n[j], stack);
          }
          stack = make_index_frame(ht, i, stack);
          level = -1;
        }
        ht = (Scheme_Hash_Tree *)ht->els[i];
        i = hamt_popcount(ht->bitmap);
      }
    }
  }
}

#endif

XFORM_NONGCING static void hamt_at_index(Scheme_Hash_Tree *ht, mzlonglong pos,
                                         Scheme_Object **_key, Scheme_Object **_val, uintptr_t *_code)
{
  int popcount, i;
  Scheme_Hash_Tree *sub;

  hamt_subtree_at_index(ht, pos, &sub, &i, &popcount);

  *_key = sub->els[i];
  if (_val)
    *_val = _mzHAMT_VAL(sub, i, popcount);
  if (_code)
    *_code = _mzHAMT_CODE(sub, i, popcount);
}

int scheme_hash_tree_index(Scheme_Hash_Tree *ht, mzlonglong pos, Scheme_Object **_key, Scheme_Object **_val)
{
  ht = resolve_placeholder(ht);

  if (pos < ht->count) {
    hamt_at_index(ht, pos, _key, _val, NULL);
    return 1;
  } else
    return 0;
}

static Scheme_Object *hamt_linear_search(Scheme_Hash_Tree *tree, int stype, Scheme_Object *key,
                                         GC_CAN_IGNORE int *_i, GC_CAN_IGNORE uintptr_t *_code,
                                         Scheme_Object *key_wraps,
                                         GC_CAN_IGNORE Scheme_Object **_interned_key)
/* in the case of hash collisions, we put the colliding elements in a
   tree that uses integers as keys; we have to search through the tree
   for keys, but the advatange of using a HAMT (instead of a list) is
   in indexing (as part of the encloding tree) and update */
{
  int i;
  Scheme_Object *found_key, *found_val;

  for (i = 0; i < tree->count; i++) {
    hamt_at_index(tree, i, &found_key, &found_val, _code);
    if (stype == scheme_eq_hash_tree_type) {
      if (SAME_OBJ(key, found_key)) {
        if (_i) *_i = i;
	if (_interned_key) *_interned_key = found_key;
        return found_val;
      }
    } else if (stype == scheme_hash_tree_type) {
      if (equal_w_key_wraps(key, found_key, key_wraps)) {
        if (_i) *_i = i;
	if (_interned_key) *_interned_key = found_key;
        return found_val;
      }
    } else {
      if (scheme_eqv(key, found_key)) {
        if (_i) *_i = i;
	if (_interned_key) *_interned_key = found_key;
        return found_val;
      }
    }
  }

  return NULL;
}

XFORM_NONGCING static Scheme_Object *hamt_eq_linear_search(Scheme_Hash_Tree *tree, Scheme_Object *key,
                                                           GC_CAN_IGNORE Scheme_Object **_interned_key)
/* specialized for `eq?`, where we know that comparison doesn't trigger a GC */
{
  int i;
  Scheme_Object *found_key, *found_val;
  uintptr_t found_code;

  for (i = 0; i < tree->count; i++) {
    hamt_at_index(tree, i, &found_key, &found_val, &found_code);
    if (SAME_OBJ(key, found_key)) {
      if (_interned_key) *_interned_key = found_key;
      return found_val;
  }
  }

  return NULL;
}

XFORM_NONGCING static uintptr_t hamt_find_free_code(Scheme_Hash_Tree *tree, int base, int shift)
/* for selecting a key when adding to a hash-collision subtree */
{
  int i, mincount, minpos;
  Scheme_Hash_Tree *subtree;
  
  for (i = 0; i < mzHAMT_WORD_SIZE; i++) {
    if (!(tree->bitmap & ((unsigned)1 << i)))
      return (i << shift) + base;
  }

  /* first layer is full; pick next layer */
  mincount = -1;
  minpos = mzHAMT_WORD_SIZE;
  for (i = mzHAMT_WORD_SIZE; i--; ) {
    if (!HASHTR_SUBTREEP(tree->els[i])) {
      uintptr_t code = ((unsigned)i << shift) + base;
      if (_mzHAMT_CODE(tree, i, mzHAMT_WORD_SIZE) == code)
        return code + ((unsigned)1 << (shift + mzHAMT_LOG_WORD_SIZE));
      else
        return code;
    } else {
      subtree = (Scheme_Hash_Tree *)tree->els[i];
      if ((mincount < 0)
          || (subtree->count < mincount)) {
        mincount = subtree->count;
        minpos = i;
      }
    }
  }

  return hamt_find_free_code((Scheme_Hash_Tree *)tree->els[minpos],
                             ((unsigned)minpos << shift) + base,
                             shift + mzHAMT_LOG_WORD_SIZE);
}

static Scheme_Hash_Tree *make_hash_tree(int eql_kind, int popcount)
{
  Scheme_Hash_Tree *ht;
  int kind = (eql_kind ? (HASHTR_HAS_CODE | HASHTR_HAS_VAL) : 0);

  ht = hamt_alloc(kind, popcount);

  ht->iso.so.type = (!eql_kind
                     ? scheme_eq_hash_tree_type
                     : ((eql_kind == 1)
                        ? scheme_hash_tree_type
                        : scheme_eqv_hash_tree_type));
  SCHEME_HASHTR_FLAGS(ht) = kind;

  return ht;
}

Scheme_Hash_Tree *scheme_make_hash_tree(int eql_kind)
{
  return empty_hash_tree[eql_kind];
}

void scheme_init_hash_tree(void)
{
  Scheme_Hash_Tree *t;

  REGISTER_SO(empty_hash_tree);

  t = make_hash_tree(0, 0);
  empty_hash_tree[0] = t;

  t = make_hash_tree(1, 0);
  empty_hash_tree[1] = t;

  t = make_hash_tree(2, 0);
  empty_hash_tree[2] = t;
}

Scheme_Hash_Tree *scheme_make_hash_tree_of_type(Scheme_Type stype)
{
  if (stype == scheme_eq_hash_tree_type)
    return scheme_make_hash_tree(SCHEME_hashtr_eq);
  else if (stype == scheme_hash_tree_type)
    return scheme_make_hash_tree(SCHEME_hashtr_equal);
  else
    return scheme_make_hash_tree(SCHEME_hashtr_eqv);
}

Scheme_Hash_Tree *scheme_make_hash_tree_placeholder(int eql_kind)
/* for cyclic immutable hash tables, we need an indirection to form
   the cycle (since we don't know in advance how large the top record
   needs to be) */
{
  Scheme_Hash_Tree *ht;

  ht = make_hash_tree(eql_kind, 1);
  ht->iso.so.type = scheme_hash_tree_indirection_type;
  ht->count = 0;
  ht->bitmap = 1;

  ht->els[0] = (Scheme_Object *)empty_hash_tree[eql_kind];

  return ht;
}

void scheme_hash_tree_tie_placeholder(Scheme_Hash_Tree *t, Scheme_Hash_Tree *base)
{
  t->count = base->count;
  t->els[0] = (Scheme_Object *)base;
}

Scheme_Hash_Tree *scheme_hash_tree_resolve_placeholder(Scheme_Hash_Tree *t)
{
  return resolve_placeholder(t);
}

XFORM_NONGCING static Scheme_Object *
scheme_eq_hash_tree_get_w_interned_key(Scheme_Hash_Tree *tree, Scheme_Object *key,
                                       GC_CAN_IGNORE Scheme_Object **_interned_key)
{
  uintptr_t h;
  int pos;

  h = PTR_TO_LONG((Scheme_Object *)key);
  h = HAMT_REORDER(h);

  tree = hamt_assoc(resolve_placeholder(tree), h, &pos, 0);
  if (!tree)
    return NULL;

  if (HASHTR_COLLISIONP(tree->els[pos])) {
    /* hash collision; linear search in subtree */
    return hamt_eq_linear_search((Scheme_Hash_Tree *)tree->els[pos], key, _interned_key);
  } else {
    if (SAME_OBJ(key, tree->els[pos])) {
      if (_interned_key) *_interned_key = tree->els[pos];
      return mzHAMT_VAL(tree, pos);
    }
  }

  return NULL;
}

Scheme_Object *scheme_eq_hash_tree_get(Scheme_Hash_Tree *tree, Scheme_Object *key)
{
  return scheme_eq_hash_tree_get_w_interned_key(tree, key, NULL);
}

Scheme_Object *scheme_hash_tree_get_w_key_wraps(Scheme_Hash_Tree *tree, Scheme_Object *key,
                                                Scheme_Object *key_wraps,
                                                GC_CAN_IGNORE Scheme_Object **_interned_key)
{
  uintptr_t h;
  int stype, pos;

  tree = resolve_placeholder(tree);
  if (!tree->count)
    return NULL;

  stype = SCHEME_TYPE(tree);

  if (stype == scheme_eq_hash_tree_type)
    return scheme_eq_hash_tree_get_w_interned_key(tree, key, _interned_key);
  else if (stype == scheme_hash_tree_type) {
    if (key_wraps)
      key = apply_equal_key_wraps(key, key_wraps);
    h = to_unsigned_hash(scheme_equal_hash_key(key));
  } else
    h = to_unsigned_hash(scheme_eqv_hash_key(key));
  h = HAMT_REORDER(h);

  tree = hamt_assoc(tree, h, &pos, 0);
  if (!tree)
    return NULL;

  if (HASHTR_COLLISIONP(tree->els[pos])) {
    /* hash collision; linear search in subtree */
    uintptr_t code;
    return hamt_linear_search((Scheme_Hash_Tree *)tree->els[pos], stype, key, NULL, &code, key_wraps,
                              _interned_key);
  } else {
    if (stype == scheme_hash_tree_type) {
      if (equal_w_key_wraps(key, tree->els[pos], key_wraps)) {
        if (_interned_key) *_interned_key = tree->els[pos];
        return mzHAMT_VAL(tree, pos);
      }
    } else {
      if (scheme_eqv(key, tree->els[pos])) {
        if (_interned_key) *_interned_key = tree->els[pos];
        return mzHAMT_VAL(tree, pos);
      }
    }
  }

  return NULL;
}

Scheme_Object *scheme_hash_tree_get(Scheme_Hash_Tree *tree, Scheme_Object *key)
{
  return scheme_hash_tree_get_w_key_wraps(tree, key, NULL, NULL);
}

Scheme_Object *scheme_hash_tree_get_key(Scheme_Hash_Tree *tree, Scheme_Object *key)
{
  Scheme_Object *interned_key, *v;

  v = scheme_hash_tree_get_w_key_wraps(tree, key, NULL, &interned_key);
  if (v)
    return interned_key;
  else
    return NULL;
}

Scheme_Hash_Tree *scheme_hash_tree_set_w_key_wraps(Scheme_Hash_Tree *tree, Scheme_Object *key, Scheme_Object *val,
                                                   Scheme_Object *key_wraps)
/* val == NULL => remove */
{
  uintptr_t h;
  Scheme_Hash_Tree *in_tree;
  Scheme_Object *ekey = key;
  int stype, pos;

  stype = SCHEME_TYPE(resolve_placeholder(tree));

  if (stype == scheme_eq_hash_tree_type)
    h = PTR_TO_LONG((Scheme_Object *)key);
  else if (stype == scheme_hash_tree_type) {
    if (key_wraps)
      ekey = apply_equal_key_wraps(ekey, key_wraps);
    h = to_unsigned_hash(scheme_equal_hash_key(ekey));
  } else
    h = to_unsigned_hash(scheme_eqv_hash_key(key));
  h = HAMT_REORDER(h);
  
  in_tree = hamt_assoc(resolve_placeholder(tree), h, &pos, 0);
  if (!in_tree) {
    if (!val)
      return tree;
    else {
      /* simple add */
      tree = resolve_placeholder(tree);
      return hamt_set(tree, h, 0, key, val, 1);
    }
  }

  if (HASHTR_COLLISIONP(in_tree->els[pos])) {
    /* hash collision */
    int i, inc;
    uintptr_t code;
    in_tree = (Scheme_Hash_Tree *)in_tree->els[pos];
    if (hamt_linear_search(in_tree, stype, key, &i, &code, key_wraps, NULL)) {
      /* key is part of the current collision */
      if (!val) {
        if (in_tree->count == 2) {
          /* no more hash collision */
          Scheme_Object *other_key, *other_val;
          hamt_at_index(in_tree, 1-i, &other_key, &other_val, &code);
          tree = resolve_placeholder(tree);
          return hamt_set(tree, h, 0, other_key, other_val, -1);
        } else {
          /* less collision */
          in_tree = hamt_remove(in_tree, code, 0);
          inc = -1;
        }
      } else {
        /* update collision */
        /* (we're not looking for a shortcut here if the current value
            matched the new value, but we could do that if it seems
            worthwhile; hopefully, collisions are relatively rare) */
        in_tree = hamt_set(in_tree, code, 0, key, val, 0);
        inc = 0;
      }
    } else {
      if (!val)
        return tree;
      else {
        /* more collision */
        code = hamt_find_free_code(in_tree, 0, 0);
        in_tree = hamt_set(in_tree, code, 0, key, val, 1);
        inc = 1;
      }
    }
    /* install updated collision tree in main tree: */
    tree = resolve_placeholder(tree);
    return hamt_set(tree, h, 0, (Scheme_Object *)in_tree, NULL, inc);
  } else {
    int same;
    
    if (stype == scheme_eq_hash_tree_type)
      same = SAME_OBJ(key, in_tree->els[pos]);
    else if (stype == scheme_hash_tree_type)
      same = equal_w_key_wraps(ekey, in_tree->els[pos], key_wraps);
    else
      same = scheme_eqv(key, in_tree->els[pos]);

    if (same) {
      /* replace */
      tree = resolve_placeholder(tree);
      if (!val) {
        tree = hamt_remove(tree, h, 0);
        if (!tree) {
           if (stype == scheme_eq_hash_tree_type)
             return empty_hash_tree[0];
           else if (stype == scheme_hash_tree_type)
             return empty_hash_tree[1];
           else
             return empty_hash_tree[2];
        } else
          return tree;
      } else if (SAME_OBJ(val, mzHAMT_VAL(in_tree, pos))
                 && SAME_OBJ(key, in_tree->els[pos])) {
        /* Shortcut: setting to the current key and value */
        return tree;
      } else
        return hamt_set(tree, h, 0, key, val, 0);
    } else {
      /* add */
      if (!val)
        return tree;
      else {
        /* new hash collision */
        in_tree = hamt_make2(SCHEME_HASHTR_KIND(in_tree) | HASHTR_HAS_VAL | HASHTR_HAS_CODE, 0,
                             0, in_tree->els[pos], mzHAMT_VAL(in_tree, pos),
                             1, key, val);
        in_tree->iso.so.type = scheme_hash_tree_collision_type;
        tree = resolve_placeholder(tree);
        return hamt_set(tree, h, 0, (Scheme_Object *)in_tree, NULL, 1);
      }
    }
  }
}

Scheme_Hash_Tree *scheme_hash_tree_set(Scheme_Hash_Tree *tree, Scheme_Object *key, Scheme_Object *val)
{
  return scheme_hash_tree_set_w_key_wraps(tree, key, val, NULL);
}

static int hamt_equal_entries(int stype, void *eql_data,
                              Scheme_Object *k1, Scheme_Object *v1, 
                              Scheme_Object *k2, Scheme_Object *v2)
{
  if (stype == scheme_eq_hash_tree_type) {
    if (SAME_OBJ(k1, k2)) {
      if (eql_data)
        return scheme_recur_equal(v1, v2, eql_data);
      else
        return SAME_OBJ(v1, v2);
    }
  } else if (stype == scheme_hash_tree_type) {
    if (eql_data) {
      if (scheme_recur_equal(k1, k2, eql_data))
        return scheme_recur_equal(v1, v2, eql_data);
    } else
      return scheme_equal(k1, k2);
  } else {
    if (scheme_eqv(k1, k2)) {
      if (eql_data)
        return scheme_recur_equal(v1, v2, eql_data);
      else
        return 1;
    }
  }
  return 0;
}

#define HAMT_NONGCING /* empty */
#define HAMT_SUBSET_OF hamt_subset_of
#define HAMT_ELEMENT_OF hamt_element_of
#define HAMT_ELEMENT_OF_COLLISION hamt_element_of_collision
#define HAMT_EQUAL_ENTRIES hamt_equal_entries
#define HAMT_IF_VAL(v, n) v
#define HAMT_USE_FUEL(n) SCHEME_USE_FUEL(n)
#include "hamt_subset.inc"

/* fast variant for eq-based sets (i.e., no separate values in table) */
#define HAMT_NONGCING XFORM_NONGCING
#define HAMT_SUBSET_OF hamt_eq_subset_of
#define HAMT_ELEMENT_OF hamt_eq_element_of
#define HAMT_ELEMENT_OF_COLLISION hamt_eq_element_of_collision
#define HAMT_EQUAL_ENTRIES(stype, eql_data, k1, v1, k2, v2) SAME_OBJ(k1, k2)
#define HAMT_IF_VAL(v, n) n
#define HAMT_USE_FUEL(n) /* empty */
#include "hamt_subset.inc"

/* fast variant for eq-based dictionaries, where values are compared with `eq?` */
#define HAMT_NONGCING XFORM_NONGCING
#define HAMT_SUBSET_OF hamt_eq_subset_match_of
#define HAMT_ELEMENT_OF hamt_eq_element_match_of
#define HAMT_ELEMENT_OF_COLLISION hamt_eq_element_match_of_collision
#define HAMT_EQUAL_ENTRIES(stype, eql_data, k1, v1, k2, v2) (SAME_OBJ(k1, k2) && SAME_OBJ(v1, v2))
#define HAMT_IF_VAL(v, n) v
#define HAMT_USE_FUEL(n) /* empty */
#include "hamt_subset.inc"

static uintptr_t hamt_combine_key_hashes(Scheme_Hash_Tree *ht)
{
  int popcount, i;
  uintptr_t k = 0, code;
  
  popcount = hamt_popcount(ht->bitmap);
  
  for (i = 0; i < popcount; i++) {
    if (HASHTR_SUBTREEP(ht->els[i])) {
      SCHEME_USE_FUEL(1);
      code = hamt_combine_key_hashes((Scheme_Hash_Tree *)ht->els[i]);
    } else if (HASHTR_COLLISIONP(ht->els[i])) {
      int count = ((Scheme_Hash_Tree *)ht->els[i])->count, j;
      code = _mzHAMT_CODE(ht, i, popcount);
      for (j = 0; j < count; j++) {
        MZ_MIX(code);
      }
    } else
      code = _mzHAMT_CODE(ht, i, popcount);
  
    k += code;

    /* Since the keys are always in the same order (we don't go into collision trees),
       it's ok to mix the total: */
    MZ_MIX(k);
  }

  return k;
}

int scheme_hash_tree_equal_rec(Scheme_Hash_Tree *t1, Scheme_Object *orig_t1,
                               Scheme_Hash_Tree *t2, Scheme_Object *orig_t2,
                               void *eql)
{
  Scheme_Object *k, *v, *v2;
  int i;

  t1 = resolve_placeholder(t1);
  t2 = resolve_placeholder(t2);
    
  if ((t1->count != t2->count)
      || (SCHEME_TYPE(t1) != SCHEME_TYPE(t2)))
    return 0;

  if (SAME_OBJ((Scheme_Object *)t1, orig_t1)
      && SAME_OBJ((Scheme_Object *)t2, orig_t2))
    return hamt_subset_of(t1, t2, 0, SCHEME_TYPE(t1), eql);
    
  for (i = scheme_hash_tree_next(t1, -1); i != -1; i = scheme_hash_tree_next(t1, i)) {
    scheme_hash_tree_index(t1, i, &k, &v);

    if (!SAME_OBJ((Scheme_Object *)t1, orig_t1))
      v = scheme_chaperone_hash_traversal_get(orig_t1, k, &k);
      
    if (!SAME_OBJ((Scheme_Object *)t2, orig_t2))
      v2 = scheme_chaperone_hash_get(orig_t2, k);
    else
      v2 = scheme_hash_tree_get(t2, k);

    if (!v2)
      return 0;
    if (!scheme_recur_equal(v, v2, eql))
      return 0;
  }

  return 1;
}

int scheme_eq_hash_tree_subset_of(Scheme_Hash_Tree *t1, Scheme_Hash_Tree *t2)
/* assumes that `t1` and `t2` are sets, as opposed to maps */
{
  t1 = resolve_placeholder(t1);
  t2 = resolve_placeholder(t2);

  if (t1->count > t2->count)
    return 0;
  
  return hamt_eq_subset_of(t1, t2, 0, scheme_eq_hash_tree_type, NULL);
}

int scheme_hash_tree_subset_of(Scheme_Hash_Tree *t1, Scheme_Hash_Tree *t2)
{
  t1 = resolve_placeholder(t1);
  t2 = resolve_placeholder(t2);

  if (t1->count > t2->count)
    return 0;
  
  return hamt_subset_of(t1, t2, 0, SCHEME_TYPE(t1), NULL);
}

int scheme_eq_hash_tree_subset_match_of(Scheme_Hash_Tree *t1, Scheme_Hash_Tree *t2)
/* assumes that `t1` and `t2` are sets, as opposed to maps */
{
  t1 = resolve_placeholder(t1);
  t2 = resolve_placeholder(t2);

  if (t1->count > t2->count)
    return 0;

  return hamt_eq_subset_match_of(t1, t2, 0, scheme_eq_hash_tree_type, NULL);
}

intptr_t scheme_hash_tree_key_hash(Scheme_Hash_Tree *ht)
{
  ht = resolve_placeholder(ht);

  return (intptr_t)hamt_combine_key_hashes(ht);
}

/*========================================================================*/
/*                         precise GC traversers                          */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#include "mzmark_hash.inc"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_hash_tree_type, hash_tree_val);
  GC_REG_TRAV(scheme_eq_hash_tree_type, hash_tree_val);
  GC_REG_TRAV(scheme_eqv_hash_tree_type, hash_tree_val);
  GC_REG_TRAV(scheme_hash_tree_subtree_type, hash_tree_val);
  GC_REG_TRAV(scheme_hash_tree_collision_type, hash_tree_val);
  GC_REG_TRAV(scheme_hash_tree_indirection_type, hash_tree_val);
}

END_XFORM_SKIP;

#endif
