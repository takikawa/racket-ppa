/* 
   Provides:
      Mach-based allocator (uses alloc_cache.c)
      macosx_init_exception_handler() --- installs fault handler
      size_type -- the type of the heap size
      determine_max_heap_size()
   Requires:
      TEST = 0
      GENERATIONS --- zero or non-zero
      designate_modified --- when GENERATIONS is non-zero
   Optional:
      DONT_NEED_MAX_HEAP_SIZE --- to disable a provide
*/

#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>
#include <mach/mach.h>
#include <mach/mach_error.h>
#if defined(__POWERPC__) && 0
# define PPC_HAND_ROLLED_THREAD
#endif
#ifdef PPC_HAND_ROLLED_THREAD
# include <architecture/ppc/cframe.h>
#else
# include <pthread.h>
#endif

# if GENERATIONS
static int designate_modified(void *p);
# endif
# define TEST 0
#ifndef TEST
# define TEST 1
int designate_modified(void *p);
#endif

#ifdef __POWERPC__
# define ARCH_thread_state_t ppc_thread_state_t
# define ARCH_THREAD_STATE PPC_THREAD_STATE
# define ARCH_THREAD_STATE_COUNT PPC_THREAD_STATE_COUNT
#else
# define ARCH_thread_state_t i386_thread_state_t
# define ARCH_THREAD_STATE i386_THREAD_STATE
# define ARCH_THREAD_STATE_COUNT i386_THREAD_STATE_COUNT
#endif

/* the structure of an exception msg and its reply */
typedef struct rep_msg {
  mach_msg_header_t head;
  NDR_record_t NDR;
  kern_return_t ret_code;
} mach_reply_msg_t;

typedef struct exc_msg {
  mach_msg_header_t head;
  /* start of the kernel processed data */
  mach_msg_body_t msgh_body;
  mach_msg_port_descriptor_t thread;
  mach_msg_port_descriptor_t task;
  /* end of the kernel processed data */
  NDR_record_t NDR;
  exception_type_t exception;
  mach_msg_type_number_t code_cnt;
  exception_data_t code;
  /* some padding */
  char pad[512];
} mach_exc_msg_t;

/* this is a neat little mach callback */
extern boolean_t exc_server(mach_msg_header_t *in, mach_msg_header_t *out);

/* these are the globals everyone needs */
#define page_size vm_page_size
static mach_port_t task_self = 0;
static mach_port_t exc_port = 0;

/* the VM subsystem as defined by the GC files */
static void *os_vm_alloc_pages(size_t len)
{
  kern_return_t retval;
  void *r;

  if(!task_self) task_self = mach_task_self();

  /* round up to the nearest page: */
  if(len & (page_size - 1))
    len += page_size - (len & (page_size - 1));

  retval = vm_allocate(task_self, (vm_address_t*)&r, len, TRUE);
  if(retval != KERN_SUCCESS) {
    GCPRINT(GCOUTF, "Couldn't allocate memory: %s\n", mach_error_string(retval));
    abort();
  }

  return r;
}

static void os_vm_free_pages(void *p, size_t len)
{
  kern_return_t retval;

  retval = vm_deallocate(task_self, (vm_address_t)p, len);
  if(retval != KERN_SUCCESS) {
    GCPRINT(GCOUTF, "WARNING: couldn't deallocate page %p: %s\n", p,
	   mach_error_string(retval));
  }
}

static void vm_protect_pages(void *p, size_t len, int writeable)
{
  kern_return_t retval;

  if(len & (page_size - 1)) {
    len += page_size - (len & (page_size - 1));
  }

  retval = vm_protect(task_self, (vm_address_t)p, len, FALSE,
		      writeable ? VM_PROT_ALL 
		      : (VM_PROT_READ | VM_PROT_EXECUTE));
  if(retval != KERN_SUCCESS) {
    GCPRINT(GCOUTF, "WARNING: couldn't protect %li bytes of page %p%s\n",
	   len, p, mach_error_string(retval));
  }
}

#include "alloc_cache.c"

#ifndef DONT_NEED_MAX_HEAP_SIZE

static unsigned long determine_max_heap_size()
{
  struct rlimit rlim;

  getrlimit(RLIMIT_RSS, &rlim);
  return (rlim.rlim_cur == RLIM_INFINITY) ? (unsigned long)-1 : rlim.rlim_cur;
}
#endif

/* The catch_exception_raise() functions are treated specially by the
   linker, and Mach looks them up at run time. We provide
   GC_... variants due to linker confusion when the implementaiton of
   these are in a framework instead of the main binary, so that the
   main binary needs to define them and jump to the implemenations
   here. (This linker problem seems to occur when we use
   -mmacosx-version-min.) */

kern_return_t GC_catch_exception_raise_state(mach_port_t port,
                                             exception_type_t exception_type,
                                             exception_data_t exception_data,
                                             mach_msg_type_number_t data_cnt,
                                             thread_state_flavor_t *flavor,
                                             thread_state_t in_state,
                                             mach_msg_type_number_t is_cnt,
                                             thread_state_t out_state,
                                             mach_msg_type_number_t os_cnt)
{
  return KERN_FAILURE;
}

kern_return_t catch_exception_raise_state(mach_port_t port,
					  exception_type_t exception_type,
					  exception_data_t exception_data,
					  mach_msg_type_number_t data_cnt,
					  thread_state_flavor_t *flavor,
					  thread_state_t in_state,
					  mach_msg_type_number_t is_cnt,
					  thread_state_t out_state,
					  mach_msg_type_number_t os_cnt)
{
  return GC_catch_exception_raise_state(port, exception_type, exception_data,
                                        data_cnt, flavor,
                                        in_state, is_cnt,
                                        out_state, os_cnt);
}

kern_return_t GC_catch_exception_raise_state_identitity
  (mach_port_t port,  mach_port_t thread_port, mach_port_t task_port,
   exception_type_t exception_type, exception_data_t exception_data,
   mach_msg_type_number_t data_count, thread_state_flavor_t *state_flavor,
   thread_state_t in_state, mach_msg_type_number_t in_state_count,
   thread_state_t out_state, mach_msg_type_number_t out_state_count)
{
  return KERN_FAILURE;
}

kern_return_t catch_exception_raise_state_identitity
  (mach_port_t port,  mach_port_t thread_port, mach_port_t task_port,
   exception_type_t exception_type, exception_data_t exception_data,
   mach_msg_type_number_t data_count, thread_state_flavor_t *state_flavor,
   thread_state_t in_state, mach_msg_type_number_t in_state_count,
   thread_state_t out_state, mach_msg_type_number_t out_state_count)
{
  return GC_catch_exception_raise_state_identitity(port, thread_port, task_port,
                                                   exception_type, exception_data,
                                                   data_count, state_flavor,
                                                   in_state, in_state_count,
                                                   out_state, out_state_count);
}

kern_return_t GC_catch_exception_raise(mach_port_t port,
                                       mach_port_t thread_port,
                                       mach_port_t task_port,
                                       exception_type_t exception_type,
                                       exception_data_t exception_data,
                                       mach_msg_type_number_t data_count)
{
#if GENERATIONS
  /* kernel return value is in exception_data[0], faulting address in
     exception_data[1] */
  if(exception_data[0] == KERN_PROTECTION_FAILURE) {
    if (designate_modified((void*)exception_data[1]))
      return KERN_SUCCESS;
    else
      return KERN_FAILURE;
  } else 
#endif
    return KERN_FAILURE;
}

kern_return_t catch_exception_raise(mach_port_t port,
				    mach_port_t thread_port,
				    mach_port_t task_port,
				    exception_type_t exception_type,
				    exception_data_t exception_data,
				    mach_msg_type_number_t data_count)
{
  return GC_catch_exception_raise(port, thread_port, task_port,
                                  exception_type, exception_data, data_count);
}

/* this is the thread which forwards of exceptions read from the exception
   server off to our exception catchers and then back out to the other
   thread */
void exception_thread(void)
{
  mach_msg_header_t *message;
  mach_msg_header_t *reply;
  kern_return_t retval;
  
  /* allocate the space for the message and reply */
  message = (mach_msg_header_t*)malloc(sizeof(mach_exc_msg_t));
  reply = (mach_msg_header_t*)malloc(sizeof(mach_reply_msg_t));
  /* do this loop forever */
  while(1) {
    /* block until we get an exception message */
    retval = mach_msg(message, MACH_RCV_MSG, 0, sizeof(mach_exc_msg_t), 
		      exc_port, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
    /* forward off the handling of this message */
    if(!exc_server(message, reply)) {
      GCPRINT(GCOUTF, "INTERNAL ERROR: exc_server() didn't like something\n");
      abort();
    }
    /* send the message back out to the thread */
    retval = mach_msg(reply, MACH_SEND_MSG, sizeof(mach_reply_msg_t), 0, 
		      MACH_PORT_NULL, MACH_MSG_TIMEOUT_NONE, MACH_PORT_NULL);
  }
}

/* this initializes the subsystem (sets the exception port, starts the
   exception handling thread, etc) */
static void macosx_init_exception_handler() 
{
  mach_port_t thread_self, exc_port_s;
  mach_msg_type_name_t type;
  kern_return_t retval;

  /* get ids for ourself */
  if(!task_self) task_self = mach_task_self();
  thread_self = mach_thread_self();

  /* allocate the port we're going to get exceptions on */
  retval = mach_port_allocate(task_self, MACH_PORT_RIGHT_RECEIVE, &exc_port);
  if(retval != KERN_SUCCESS) {
    GCPRINT(GCOUTF, "Couldn't allocate exception port: %s\n", 
	   mach_error_string(retval));
    abort();
  }

  /* extract out the send rights for that port, which the OS needs */
  retval = mach_port_extract_right(task_self, exc_port, MACH_MSG_TYPE_MAKE_SEND,
				   &exc_port_s, &type);
  if(retval != KERN_SUCCESS) {
    GCPRINT(GCOUTF, "Couldn't extract send rights: %s\n", mach_error_string(retval));
    abort();
  }

  /* set the exception ports for this thread to the above */
  retval = thread_set_exception_ports(thread_self, EXC_MASK_BAD_ACCESS, 
				      exc_port_s, EXCEPTION_DEFAULT, 
				      ARCH_THREAD_STATE);
  if(retval != KERN_SUCCESS) {
    GCPRINT(GCOUTF, "Couldn't set exception ports: %s\n", mach_error_string(retval));
    abort();
  }

#ifdef PPC_HAND_ROLLED_THREAD 
  /* Old hand-rolled thread creation. pthread_create is fine for our
     purposes. */
 {
   /* set up the subthread */
   mach_port_t exc_thread;
   ARCH_thread_state_t *exc_thread_state;
   void *subthread_stack;

   retval = thread_create(task_self, &exc_thread);
   if(retval != KERN_SUCCESS) {
     GCPRINT(GCOUTF, "Couldn't create exception thread: %s\n", mach_error_string(retval));
     abort();
   }
   subthread_stack = (void*)malloc(page_size);
   subthread_stack += (page_size - C_ARGSAVE_LEN - C_RED_ZONE);
   exc_thread_state = (ARCH_thread_state_t*)malloc(sizeof(ARCH_thread_state_t));
   exc_thread_state->srr0 = (unsigned int)exception_thread;
   exc_thread_state->r1 = (unsigned int)subthread_stack;
   retval = thread_set_state(exc_thread, ARCH_THREAD_STATE,
			     (thread_state_t)exc_thread_state,
			     ARCH_THREAD_STATE_COUNT);
   if(retval != KERN_SUCCESS) {
     GCPRINT(GCOUTF, "Couldn't set subthread state: %s\n", mach_error_string(retval));
     abort();
   }
   retval = thread_resume(exc_thread);
   if(retval != KERN_SUCCESS) {
     GCPRINT(GCOUTF, "Couldn't resume subthread: %s\n", mach_error_string(retval));
     abort();
   }
 }
#else
 {
   pthread_t th;
   pthread_create(&th, NULL, (void *(*)(void *))exception_thread, NULL);
 }
#endif
}

#if TEST
#define MPAGE_SIZE 16384
#define BPAGE_SIZE 20034

char *normal_page = NULL;
char *big_page = NULL;

int designate_modified(void *p)
{
  if((p >= normal_page) && (p < (normal_page + MPAGE_SIZE))) {
    vm_protect_pages(p, MPAGE_SIZE, 1);
    return 1;
  }
  if((p >= big_page) && (p < (big_page + BPAGE_SIZE))) {
    vm_protect_pages(p, BPAGE_SIZE, 1);
    return 1;
  }
  printf("Unrecognized write: %p\n", p);
  return 0;
}

int main(int argc, char **argv)
{
  macosx_init_exception_handler();
  printf("Allocating test pages:\n");
  normal_page = vm_malloc_pages(MPAGE_SIZE, MPAGE_SIZE,0);
  printf("  ... normal page at %p\n", normal_page);
  big_page = vm_malloc_pages(BPAGE_SIZE, MPAGE_SIZE,0);
  printf("  ... big page at %p\n", big_page);
  printf("Setting protection on test pages\n");
  vm_protect_pages(normal_page, MPAGE_SIZE, 0);
  printf("  ... normal page %p set\n", normal_page);
  vm_protect_pages(big_page, MPAGE_SIZE, 0);
  printf("  ... big page %p set\n", big_page);
  printf("Writing to test pages\n");
  normal_page[2] = 'A';
  big_page[2] = 'A';
  printf("Reading from test pages:\n");
  printf("  ... normal_page %p's second byte is %c\n", normal_page, normal_page[2]);
  printf("  ... big_page %p's second byte is %c\n", big_page, big_page[2]);
  printf("Freeing test pages:\n");
  vm_free_pages(normal_page, MPAGE_SIZE);
  printf("  ... freed normal page\n");
  vm_free_pages(big_page, MPAGE_SIZE);
  printf("  ... freed big page\n");
}
#endif
