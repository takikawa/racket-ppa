/*
  MzScheme
  Copyright (c) 2004-2005 PLT Scheme, Inc.
  Copyright (c) 1995-2001 Matthew Flatt
 
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include "schpriv.h"

#ifndef NO_SCHEME_THREADS

Scheme_Object *scheme_always_ready_evt;

static Scheme_Object *make_sema(int n, Scheme_Object **p);
static Scheme_Object *semap(int n, Scheme_Object **p);
static Scheme_Object *hit_sema(int n, Scheme_Object **p);
static Scheme_Object *block_sema_p(int n, Scheme_Object **p);
static Scheme_Object *block_sema(int n, Scheme_Object **p);
static Scheme_Object *block_sema_breakable(int n, Scheme_Object **p);
static Scheme_Object *make_sema_repost(int n, Scheme_Object **p);

static Scheme_Object *make_channel(int n, Scheme_Object **p);
static Scheme_Object *make_channel_put(int n, Scheme_Object **p);
static Scheme_Object *channel_p(int n, Scheme_Object **p);

static Scheme_Object *make_alarm(int n, Scheme_Object **p);

static int channel_get_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo);
static int channel_put_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo);
static int channel_syncer_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo);
static int alarm_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo);
static int always_ready(Scheme_Object *w);
static int never_ready(Scheme_Object *w);

static int pending_break(Scheme_Thread *p);

int scheme_main_was_once_suspended;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

typedef struct {
  Scheme_Object so;
  double sleep_end;
} Scheme_Alarm;

/* For object-sync: */
static int sema_ready(Scheme_Object *s)
{
  return scheme_wait_sema(s, 1);
}

static Scheme_Object *sema_for_repost(Scheme_Object *s, int *repost)
{
  *repost = 1;
  return SCHEME_PTR_VAL(s);
}

void scheme_init_sema(Scheme_Env *env)
{
  Scheme_Object *o;

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  scheme_add_global_constant("make-semaphore", 
			     scheme_make_prim_w_arity(make_sema,
						      "make-semaphore", 
						      0, 1), 
			     env);
  scheme_add_global_constant("semaphore?", 
			     scheme_make_folding_prim(semap,
						      "semaphore?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("semaphore-post", 
			     scheme_make_prim_w_arity(hit_sema, 
						      "semaphore-post", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-try-wait?", 
			     scheme_make_prim_w_arity(block_sema_p, 
						      "semaphore-try-wait?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-wait", 
			     scheme_make_prim_w_arity(block_sema, 
						      "semaphore-wait", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-wait/enable-break", 
			     scheme_make_prim_w_arity(block_sema_breakable, 
						      "semaphore-wait/enable-break", 
						      1, 1), 
			     env);

  scheme_add_global_constant("semaphore-peek-evt", 
			     scheme_make_prim_w_arity(make_sema_repost,
						      "semaphore-peek-evt", 
						      1, 1), 
			     env);

  scheme_add_global_constant("make-channel", 
			     scheme_make_prim_w_arity(make_channel,
						      "make-channel",
						      0, 0), 
			     env);
  scheme_add_global_constant("channel-put-evt", 
			     scheme_make_prim_w_arity(make_channel_put,
						      "channel-put-evt",
						      2, 2), 
			     env);
  scheme_add_global_constant("channel?", 
			     scheme_make_folding_prim(channel_p,
						      "channel?",
						      1, 1, 1), 
			     env);  

  scheme_add_global_constant("alarm-evt", 
			     scheme_make_prim_w_arity(make_alarm,
						      "alarm-evt",
						      1, 1), 
			     env);

  REGISTER_SO(scheme_always_ready_evt);
  scheme_always_ready_evt = scheme_alloc_small_object();
  scheme_always_ready_evt->type = scheme_always_evt_type;
  scheme_add_global_constant("always-evt", scheme_always_ready_evt, env);

  o = scheme_alloc_small_object();
  o->type = scheme_never_evt_type;
  scheme_add_global_constant("never-evt", o, env);

  scheme_add_evt(scheme_sema_type, sema_ready, NULL, NULL, 0);
  scheme_add_evt_through_sema(scheme_semaphore_repost_type, sema_for_repost, NULL);
  scheme_add_evt(scheme_channel_type, (Scheme_Ready_Fun)channel_get_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_channel_put_type, (Scheme_Ready_Fun)channel_put_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_channel_syncer_type, (Scheme_Ready_Fun)channel_syncer_ready, NULL, NULL, 0);
  scheme_add_evt(scheme_alarm_type, (Scheme_Ready_Fun)alarm_ready, NULL, NULL, 0);
  scheme_add_evt(scheme_always_evt_type, always_ready, NULL, NULL, 0);
  scheme_add_evt(scheme_never_evt_type, never_ready, NULL, NULL, 0);
}

Scheme_Object *scheme_make_sema(long v)
{
  Scheme_Sema *sema;

  sema = MALLOC_ONE_TAGGED(Scheme_Sema);
  sema->value = v;

  sema->so.type = scheme_sema_type;

  return (Scheme_Object *)sema;
}

static Scheme_Object *make_sema(int n, Scheme_Object **p)
{
  long v;

  if (n) {
    if (!SCHEME_INTP(p[0])) {
      if (!SCHEME_BIGNUMP(p[0]) || !SCHEME_BIGPOS(p[0]))
	scheme_wrong_type("make-semaphore", "non-negative exact integer", 0, n, p);
    }

    if (!scheme_get_int_val(p[0], &v)) {
      scheme_raise_exn(MZEXN_FAIL,
		       "make-semaphore: starting value %s is too large",
		       scheme_make_provided_string(p[0], 0, NULL));
    } else if (v < 0)
      scheme_wrong_type("make-semaphore", "non-negative exact integer", 0, n, p);
  } else
    v = 0;

  return scheme_make_sema(v);
}

static Scheme_Object *make_sema_repost(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-peek-evt", "semaphore", 0, n, p);
 
  return scheme_make_sema_repost(p[0]);
}
 
Scheme_Object *scheme_make_sema_repost(Scheme_Object *sema)
{
  Scheme_Object *o;

  o = scheme_alloc_small_object();
  o->type = scheme_semaphore_repost_type;
  SCHEME_PTR_VAL(o) = sema;

  return o;
}

static Scheme_Object *semap(int n, Scheme_Object **p)
{
  return SCHEME_SEMAP(p[0]) ? scheme_true : scheme_false;
}

void scheme_post_sema(Scheme_Object *o)
{
  Scheme_Sema *t = (Scheme_Sema *)o;
  int v, consumed;

  if (t->value < 0) return;

  v = t->value + 1;
  if (v > t->value) {
    t->value = v;

    while (t->first) {
      Scheme_Channel_Syncer *w;

      w = t->first;

      t->first = w->next;
      if (!w->next)
	t->last = NULL;
      else
	t->first->prev = NULL;
      
      if ((!w->syncing || !w->syncing->result) && !pending_break(w->p)) {
	if (w->syncing) {
	  w->syncing->result = w->syncing_i + 1;
	  if (w->syncing->disable_break)
	    w->syncing->disable_break->suspend_break++;
	  scheme_post_syncing_nacks(w->syncing);
	  if (!w->syncing->reposts || !w->syncing->reposts[w->syncing_i]) {
	    t->value -= 1;
	    consumed = 1;
	  } else
	    consumed = 0;
	} else {
	  /* In this case, we will remove the syncer from line, but
	     someone else might grab the post. This is unfair, but it
	     can help improve throughput when multiple threads synchronize
	     on a lock. */
	  consumed = 1;
	}
	w->picked = 1;
      } else
	consumed = 0;

      w->in_line = 0;
      w->prev = NULL;
      w->next = NULL;

      if (w->picked) {
	scheme_weak_resume_thread(w->p);
	if (consumed)
	  break;
      }
      /* otherwise, loop to find one we can wake up */
    }

    return;
  }

  scheme_raise_exn(MZEXN_FAIL,
		   "semaphore-post: the maximum post count has already been reached");
}

void scheme_post_sema_all(Scheme_Object *o)
{
  Scheme_Sema *t = (Scheme_Sema *)o;

  while (t->first) {
    scheme_post_sema(o);
  }
  t->value = -1;
}

static Scheme_Object *hit_sema(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-post", "semaphore", 0, n, p);

  scheme_post_sema(p[0]);

  return scheme_void;
}

static int out_of_line(Scheme_Object *a)
{
  Scheme_Thread *p;
  int n, i;
  Scheme_Channel_Syncer *w;

  /* Out of one line? */
  n = SCHEME_INT_VAL(((Scheme_Object **)a)[0]);
  for (i = 0; i < n; i++) {
    w = (((Scheme_Channel_Syncer ***)a)[1])[i];
    if (w->picked)
      return 1;
  }

  /* Suspended break? */
  p = ((Scheme_Thread **)a)[2];
  if (p->external_break) {
    int v;
    --p->suspend_break;
    v = scheme_can_break(p);
    p->suspend_break++;
    if (v)
      return 1; 
  }

  /* Suspended by user? */
  if ((p->running & MZTHREAD_USER_SUSPENDED)
      || scheme_main_was_once_suspended)
    return 1;

  return 0;
}

static void get_into_line(Scheme_Sema *sema, Scheme_Channel_Syncer *w)
{
  Scheme_Channel_Syncer *last, *first;
  
  w->in_line = 1;
  w->picked = 0;

  if (SAME_TYPE(SCHEME_TYPE(sema), scheme_never_evt_type)) {
    return; /* !!!! skip everything else */
  } else if (SCHEME_SEMAP(sema)) {
    last = sema->last;
    first = sema->first;
  } else if (SCHEME_CHANNELP(sema)) {
    last = ((Scheme_Channel *)sema)->get_last;
    first = ((Scheme_Channel *)sema)->get_first;
  } else {
    last = ((Scheme_Channel_Put *)sema)->ch->put_last;
    first = ((Scheme_Channel_Put *)sema)->ch->put_first;
  }

  w->prev = last;
  if (last)
    last->next = w;
  else
    first = w;
  last = w;
  w->next = NULL;

  if (SCHEME_SEMAP(sema)) {
    sema->last = last;
    sema->first = first;
  } else if (SCHEME_CHANNELP(sema)) {
    ((Scheme_Channel *)sema)->get_last = last;
    ((Scheme_Channel *)sema)->get_first = first;
  } else {
    ((Scheme_Channel_Put *)sema)->ch->put_last = last;
    ((Scheme_Channel_Put *)sema)->ch->put_first = first;
  }
}

static void get_outof_line(Scheme_Sema *sema, Scheme_Channel_Syncer *w)
{
  Scheme_Channel_Syncer *last, *first;

  w->in_line = 0;

  if (SAME_TYPE(SCHEME_TYPE(sema), scheme_never_evt_type)) {
    return; /* !!!! skip everything else */
  } else if (SCHEME_SEMAP(sema)) {
    last = sema->last;
    first = sema->first;
  } else if (SCHEME_CHANNELP(sema)) {
    last = ((Scheme_Channel *)sema)->get_last;
    first = ((Scheme_Channel *)sema)->get_first;
  } else {
    last = ((Scheme_Channel_Put *)sema)->ch->put_last;
    first = ((Scheme_Channel_Put *)sema)->ch->put_first;
  }

  if (w->prev)
    w->prev->next = w->next;
  else
    first = w->next;
  if (w->next)
    w->next->prev = w->prev;
  else
    last = w->prev;

  if (SCHEME_SEMAP(sema)) {
    sema->last = last;
    sema->first = first;
  } else if (SCHEME_CHANNELP(sema)) {
    ((Scheme_Channel *)sema)->get_last = last;
    ((Scheme_Channel *)sema)->get_first = first;
  } else {
    ((Scheme_Channel_Put *)sema)->ch->put_last = last;
    ((Scheme_Channel_Put *)sema)->ch->put_first = first;
  }
}

static void ext_get_into_line(Scheme_Object *ch, Scheme_Schedule_Info *sinfo)
{
  Scheme_Channel_Syncer *w;

  /* Get into line */
  w = MALLOC_ONE_RT(Scheme_Channel_Syncer);
  w->so.type = scheme_channel_syncer_type;
  w->p = scheme_current_thread;
  w->syncing = (Syncing *)sinfo->current_syncing;
  w->obj = ch;
  w->syncing_i = sinfo->w_i;

  get_into_line((Scheme_Sema *)ch, w);

  scheme_set_sync_target(sinfo, (Scheme_Object *)w, NULL, NULL, 0, 0);
}

void scheme_get_outof_line(Scheme_Channel_Syncer *ch_w)
{
  get_outof_line((Scheme_Sema *)ch_w->obj, ch_w);
}

static int try_channel(Scheme_Sema *sema, Syncing *syncing, int pos, Scheme_Object **result)
{
  if (SCHEME_CHANNELP(sema)) {
    /* GET mode */
    Scheme_Channel *ch = (Scheme_Channel *)sema;
    Scheme_Channel_Syncer *w = ch->put_first, *next;
    int picked = 0;

    while (w) {
      if (w->syncing == syncing) {
	/* can't synchronize with self */
	w = w->next;
      } else {
	Scheme_Channel_Put *chp = (Scheme_Channel_Put *)w->obj;
	
	if (!w->syncing->result && !pending_break(w->p)) {
	  w->picked = 1;
	  w->syncing->result = w->syncing_i + 1;
	  if (w->syncing->disable_break)
	    w->syncing->disable_break->suspend_break++;
	  scheme_post_syncing_nacks(w->syncing);
	  if (result)
	    *result = chp->val;
	  if (syncing && (pos >= 0)) {
	    syncing->result = pos + 1;
	    if (syncing->disable_break)
	      syncing->disable_break->suspend_break++;
	    scheme_post_syncing_nacks(syncing);
	    syncing->set->argv[pos] = chp->val;
	  }
	  picked = 1;
	  scheme_weak_resume_thread(w->p);
	}
	
	next = w->next;
	get_outof_line((Scheme_Sema *)chp, w);
	w = next;
	
	if (picked)
	  return 1;
      }
    }

    return 0;
  } else {
    /* PUT mode */
    Scheme_Channel_Put *chp = (Scheme_Channel_Put *)sema;
    Scheme_Channel_Syncer *w = chp->ch->get_first, *next;
    int picked = 0;

    while (w) {
      if (w->syncing == syncing) {
	/* can't synchronize with self */
	w = w->next;
      } else {
	if (!w->syncing->result && !pending_break(w->p)) {
	  w->picked = 1;
	  w->syncing->set->argv[w->syncing_i] = chp->val;
	  w->syncing->result = w->syncing_i + 1;
	  if (w->syncing->disable_break)
	    w->syncing->disable_break->suspend_break++;
	  scheme_post_syncing_nacks(w->syncing);
	  if (syncing && (pos >= 0)) {
	    syncing->result = pos + 1;
	    if (syncing->disable_break)
	      syncing->disable_break->suspend_break++;
	    scheme_post_syncing_nacks(syncing);
	  }
	  picked = 1;
	  scheme_weak_resume_thread(w->p);
	}
	
	next = w->next;
	get_outof_line((Scheme_Sema *)chp->ch, w);
	w = next;
	
	if (picked)
	  return 1;
      }
    }

    return 0;    
  }
}

int scheme_wait_semas_chs(int n, Scheme_Object **o, int just_try, Syncing *syncing)
     /* When syncing is supplied, o can contain Scheme_Channel_Syncer
	and never-evt values, and just_try must be 0. */
{
  Scheme_Sema **semas = (Scheme_Sema **)o;
  int v, i, ii;

  if (just_try) {
    /* assert: n == 1, !syncing */
    Scheme_Sema *sema = semas[0];
    if (just_try > 0) {
      if (sema->so.type == scheme_sema_type) {
	if (sema->value) {
	  if (sema->value > 0)
	    --sema->value;
	  v = 1;
	} else
	  v = 0;
      } else {
	v = try_channel(sema, syncing, 0, NULL);
      }
    } else {
      Scheme_Cont_Frame_Data cframe;

      scheme_push_break_enable(&cframe, 1, 1);

      scheme_wait_sema((Scheme_Object *)sema, 0);

      scheme_pop_break_enable(&cframe, 0);

      return 1;
    }
  } else {
    int start_pos;

    if (n > 1) {
      if (syncing)
	start_pos = syncing->start_pos;
      else {
	Scheme_Object *rand_state;
	rand_state = scheme_get_param(scheme_current_config(), MZCONFIG_SCHEDULER_RANDOM_STATE);
	start_pos = scheme_rand((Scheme_Random_State *)rand_state);
      }
    } else
      start_pos = 0;

    /* Initial poll */
    i = 0;
    for (ii = 0; ii < n; ii++) {
      /* Randomized start position for poll ensures fairness: */
      i = (start_pos + ii) % n;

      if (semas[i]->so.type == scheme_sema_type) {
	if (semas[i]->value) {
	  if ((semas[i]->value > 0) && (!syncing || !syncing->reposts || !syncing->reposts[i]))
	    --semas[i]->value;
	  break;
	}
      } else if (semas[i]->so.type == scheme_never_evt_type) {
	/* Never ready. */
      } else if (semas[i]->so.type == scheme_channel_syncer_type) {
	/* Probably no need to poll */
      } else if (try_channel(semas[i], syncing, i, NULL))
	break;
    }

    /* In the following, syncers get changed back to channels,
       and channel puts */
    if (ii >= n) {
      Scheme_Channel_Syncer **ws, *w;

      ws = MALLOC_N(Scheme_Channel_Syncer*, n);
      for (i = 0; i < n; i++) {
	if (semas[i]->so.type == scheme_channel_syncer_type) {
	  ws[i] = (Scheme_Channel_Syncer *)semas[i];
	  semas[i] = (Scheme_Sema *)ws[i]->obj;
	} else {
	  w = MALLOC_ONE_RT(Scheme_Channel_Syncer);
	  ws[i] = w;
	  w->so.type = scheme_channel_syncer_type;
	  w->p = scheme_current_thread;
	  w->syncing = syncing;
	  w->obj = (Scheme_Object *)semas[i];
	  w->syncing_i = i;
	}
      }
      
      while (1) {
	int out_of_a_line;

	/* Get into line */
	for (i = 0; i < n; i++) {
	  if (!ws[i]->in_line) {
	    get_into_line(semas[i], ws[i]);
	  }
	}

	if (!scheme_current_thread->next) {
	  void **a;

	  /* We're not allowed to suspend the main thread. Delay
	     breaks so we get a chance to clean up. */
	  scheme_current_thread->suspend_break++;

	  a = MALLOC_N(void*, 3);
	  a[0] = scheme_make_integer(n);
	  a[1] = ws;
	  a[2] = scheme_current_thread;
	  
	  scheme_main_was_once_suspended = 0;

	  scheme_block_until(out_of_line, NULL, (Scheme_Object *)a, (float)0.0);
	  
	  --scheme_current_thread->suspend_break;
	} else {
	  /* Mark the thread to indicate that we need to clean up
	     if the thread is killed. */
	  int old_nkc;
	  old_nkc = (scheme_current_thread->running & MZTHREAD_NEED_KILL_CLEANUP);
	  if (!old_nkc)
	    scheme_current_thread->running += MZTHREAD_NEED_KILL_CLEANUP;
	  scheme_weak_suspend_thread(scheme_current_thread);
	  if (!old_nkc && (scheme_current_thread->running & MZTHREAD_NEED_KILL_CLEANUP))
	    scheme_current_thread->running -= MZTHREAD_NEED_KILL_CLEANUP;
	}

	/* We've been resumed. But was it for the semaphore, or a signal? */
	out_of_a_line = 0;
	
	/* If we get the post, we must return WITHOUT BLOCKING. 
	   MrEd, for example, depends on this special property, which ensures
	   that the thread can't be broken or killed between
	   receiving the post and returning. */

	if (!syncing) {
	  /* Poster can't be sure that we really will get it,
	     so we have to decrement the sema count here. */
	  i = 0;
	  for (ii = 0; ii < n; ii++) {
	    i = (start_pos + ii) % n;
	    if (ws[i]->picked) {
	      out_of_a_line = 1;
	      if (semas[i]->value) {
		if (semas[i]->value > 0)
		  --(semas[i]->value);
		break;
	      }
	    }
	  }
	  if (ii >= n)
	    i = n;
	} else {
	  if (syncing->result) {
	    out_of_a_line = 1;
	    i = syncing->result - 1;
	  } else {
	    out_of_a_line = 0;
	    i = n;
	  }
	}

	if (!out_of_a_line) {
	  /* We weren't woken by any semaphore/channel. Get out of line, block once 
	     (to handle breaks/kills) and then loop to get back into line. */
	  for (i = 0; i < n; i++) {
	    if (ws[i]->in_line)
	      get_outof_line(semas[i], ws[i]);
	  }
	  
	  scheme_thread_block(0); /* ok if it returns multiple times */ 
	  scheme_current_thread->ran_some = 1;
	  /* [but why would it return multiple times?! there must have been a reason...] */
	} else {

	  if ((scheme_current_thread->running & MZTHREAD_KILLED)
	      || ((scheme_current_thread->running & MZTHREAD_USER_SUSPENDED)
		  && !(scheme_current_thread->running & MZTHREAD_NEED_SUSPEND_CLEANUP))) {
	    /* We've been killed or suspended! */
	    i = -1;
	  }

	  /* We got a post from semas[i], or we were killed. 
	     Did any (other) semaphore pick us?
	     (This only happens when syncing == NULL.) */
	  if (!syncing) {
	    int j;

	    for (j = 0; j < n; j++) {
	      if (j != i) {
		if (ws[j]->picked) {
		  if (semas[j]->value) {
		    /* Consume the value and repost, because no one else
		       has been told to go, and we're accepting a different post. */
		    if (semas[j]->value > 0)
		      --semas[j]->value;
		    scheme_post_sema((Scheme_Object *)semas[j]);
		  }
		}
	      }
	    }
	  }

	  /* If we're done, get out of all lines that we're still in. */
	  if (i < n) {
	    int j;
	    for (j = 0; j < n; j++) {
	      if (ws[j]->in_line)
		get_outof_line(semas[j], ws[j]);
	    }
	  }

	  if (i == -1) {
	    scheme_thread_block(0); /* dies or suspends */
	    scheme_current_thread->ran_some = 1;
	  }

	  if (i < n)
	    break;
	}

	/* Otherwise: !syncing and someone stole the post, or we were
	   suspended and we have to start over. Either way, poll then
	   loop to get back in line an try again. */
	for (ii = 0; ii < n; ii++) {
	  i = (start_pos + ii) % n;

	  if (semas[i]->so.type == scheme_sema_type) {
	    if (semas[i]->value) {
	      if ((semas[i]->value > 0) && (!syncing || !syncing->reposts || !syncing->reposts[i]))
		--semas[i]->value;
	      break;
	    }
	  }  else if (semas[i]->so.type == scheme_never_evt_type) {
	    /* Never ready. */
	  } else if (try_channel(semas[i], syncing, i, NULL))
	    break;
	}

	if (ii < n) {
	  /* Get out of any line that we still might be in: */
	  int j;
	  for (j = 0; j < n; j++) {
	    if (ws[j]->in_line)
	      get_outof_line(semas[j], ws[j]);
	  }

	  break;
	}

	if (!syncing) {
	  /* Looks like this thread is a victim of unfair semaphore access.
	     Go into fair mode by allocating a syncing: */
	  syncing = MALLOC_ONE_RT(Syncing);
#ifdef MZTAG_REQUIRED
	  syncing->type = scheme_rt_syncing;
#endif
	  syncing->start_pos = start_pos;

	  /* Get out of all lines, and set syncing field before we get back in line: */
	  {
	    int j;
	    for (j = 0; j < n; j++) {
	      if (ws[j]->in_line)
		get_outof_line(semas[j], ws[j]);
	      ws[j]->syncing = syncing;
	    }
	  }
	}
	/* Back to top of loop to sync again */
      }
    }
    v = i + 1;
  }

  return v;
}

int scheme_wait_sema(Scheme_Object *o, int just_try)
{
  Scheme_Object *a[1];

  a[0] = o;

  return scheme_wait_semas_chs(1, a, just_try, NULL);
}

static Scheme_Object *block_sema_p(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-try-wait?", "sema", 0, n, p);

  return scheme_wait_sema(p[0], 1) ? scheme_true : scheme_false;
}

static Scheme_Object *block_sema(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-wait", "sema", 0, n, p);

  scheme_wait_sema(p[0], 0);

  /* In case a break appeared after wwe received the post,
     check for a break, because scheme_wait_sema() won't: */
  scheme_check_break_now();

  return scheme_void;
}

static Scheme_Object *block_sema_breakable(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-wait/enable-break", "sema", 0, n, p);

  scheme_wait_sema(p[0], -1);

  return scheme_void;
}

static int pending_break(Scheme_Thread *p)
{
  if (p->running & (MZTHREAD_KILLED | MZTHREAD_USER_SUSPENDED))
    return 1;

  if (p->external_break) {
    int v;

    if (!p->next) {
      /* if p is the main thread, it must have a suspension
	 to block on a channel or semaphore: */
      --p->suspend_break;
    }

    v = scheme_can_break(p);

    if (!p->next)
      p->suspend_break++;

    return v;
  }

  return 0;
}

/**********************************************************************/
/*                            Channels                                */
/**********************************************************************/

Scheme_Object *scheme_make_channel()
{
  Scheme_Channel *c;

  c = MALLOC_ONE_TAGGED(Scheme_Channel);
  c->so.type = scheme_channel_type;
  
  return (Scheme_Object *)c;
}

static Scheme_Object *make_channel(int n, Scheme_Object **p)
{
  return scheme_make_channel();
}

Scheme_Object *scheme_make_channel_put_evt(Scheme_Object *ch, Scheme_Object *v)
{
  Scheme_Channel_Put *cp;

  cp = MALLOC_ONE_TAGGED(Scheme_Channel_Put);
  cp->so.type = scheme_channel_put_type;
  cp->ch = (Scheme_Channel *)ch;
  cp->val = v;

  return (Scheme_Object *)cp;
}

static Scheme_Object *make_channel_put(int argc, Scheme_Object **argv)
{
  if (!SCHEME_CHANNELP(argv[0]))
    scheme_wrong_type("channel-put-evt", "channel", 0, argc, argv);

  return scheme_make_channel_put_evt(argv[0], argv[1]);
}

static Scheme_Object *channel_p(int n, Scheme_Object **p)
{
  return (SCHEME_CHANNELP(p[0])
	  ? scheme_true
	  : scheme_false);
}

static int channel_get_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *result;

  if (try_channel((Scheme_Sema *)ch, (Syncing *)sinfo->current_syncing, -1, &result)) {
    scheme_set_sync_target(sinfo, result, NULL, NULL, 0, 0);
    return 1;
  }

  ext_get_into_line(ch, sinfo);
  
  return 0;
}

static int channel_put_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo)
{
  if (try_channel((Scheme_Sema *)ch, (Syncing *)sinfo->current_syncing, -1, NULL))
    return 1;

  ext_get_into_line(ch, sinfo);
  
  return 0;
}

static int channel_syncer_ready(Scheme_Object *ch_w, Scheme_Schedule_Info *sinfo)
{
  Scheme_Channel_Syncer *w = (Scheme_Channel_Syncer *)ch_w;

  if (w->picked) {
    /* The value, if any, should have been tranferred already (in which
       case we would not have made it here, actually). */
    return 1;
  }

  return 0;
}

/**********************************************************************/
/*                             alarms                                 */
/**********************************************************************/

static Scheme_Object *make_alarm(int argc, Scheme_Object **argv)
{
  Scheme_Alarm *a;
  double sleep_end;

  if (!SCHEME_REALP(argv[0])) {
    scheme_wrong_type("alarm-evt", "real number", 0, argc, argv);
  }

  sleep_end = scheme_get_val_as_double(argv[0]);

  a = MALLOC_ONE_TAGGED(Scheme_Alarm);
  a->so.type = scheme_alarm_type;
  a->sleep_end = sleep_end;

  return (Scheme_Object *)a;
}

static int alarm_ready(Scheme_Object *_a, Scheme_Schedule_Info *sinfo)
{
  Scheme_Alarm *a = (Scheme_Alarm *)_a;

  if (!sinfo->sleep_end
      || (sinfo->sleep_end > a->sleep_end))
    sinfo->sleep_end = a->sleep_end;

  if (a->sleep_end <= scheme_get_inexact_milliseconds())
    return 1;

  return 0;
}

static int always_ready(Scheme_Object *w)
{
  return 1;
}

static int never_ready(Scheme_Object *w)
{
  return 0;
}


/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_SEMA_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_alarm_type, mark_alarm);
  GC_REG_TRAV(scheme_channel_syncer_type, mark_channel_syncer);
}

END_XFORM_SKIP;

#endif

#endif /* NO_SCHEME_THREADS */
