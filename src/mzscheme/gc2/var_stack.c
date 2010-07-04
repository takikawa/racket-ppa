
void GC_X_variable_stack(void **var_stack, long delta, void *limit) 
{
  long size, count;
  void ***p, **a;

#if TIME
  stack_depth = 0;
#endif

  while (var_stack) {
    var_stack = (void **)((char *)var_stack + delta);

    size = *(long *)(var_stack + 1);
    p = (void ***)(var_stack + 2);

#if CHECKS
    oo_var_stack = o_var_stack;
    o_var_stack = var_stack;
#endif

    if ((var_stack == limit)
	|| (var_stack + size + 2 == limit)) {
      /* Last frame: need to X things that are shallower than the
	 limit: */
     
      while (size--) {
	a = *p;
	if (!a) {
	  /* Array */
	  count = ((long *)p)[2];
	  a = ((void ***)p)[1];
	  p += 2;
	  size -= 2;
	  a = (void **)((char *)a + delta);
	  if (SHALLOWER_STACK_ADDRESS(a, limit)) {
	    while (count--) {
	      X_source(a);
	      gcX(*a);
	      a++;
	    }
	  }
	} else {
	  a = (void **)((char *)a + delta);
	  if (SHALLOWER_STACK_ADDRESS(a, limit)) {
	    X_source(a);
	    gcX(*a);
	  }
	}
	p++;
      }

      return;
    }

    while (size--) {
      a = *p;
      if (!a) {
	/* Array */
	count = ((long *)p)[2];
	a = ((void ***)p)[1];
	p += 2;
	size -= 2;
	a = (void **)((char *)a + delta);
	while (count--) {
	  X_source(a);
	  gcX(*a);
	  a++;
	}
      } else {
	a = (void **)((char *)a + delta);
	X_source(a);
	gcX(*a);
      }
      p++;
    }

    var_stack = *var_stack;

#if TIME
    stack_depth++;
#endif
  }
}
