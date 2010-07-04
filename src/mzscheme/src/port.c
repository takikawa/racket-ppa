/*
  MzScheme
  Copyright (c) 2004-2006 PLT Scheme Inc.
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

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file implements the most platform-specific aspects of MzScheme
   port types, which means it deals with all the messy FILE and file
   descriptor issues, as well as implementing TCP. Also, `subprocess'
   is implemented here, since much of the work has to do with
   ports. */

#include "schpriv.h"
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_ULIMIT
# include <ulimit.h>
#endif
#ifdef FILES_HAVE_FDS
# include <fcntl.h>
# include <sys/types.h>
# include <sys/time.h>
# ifdef BSTRING_INCLUDE
#  include <bstring.h>
# endif
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifdef USE_ITIMER
# include <sys/types.h>
# include <sys/time.h>
# include <signal.h>
#endif
#if defined(UNIX_PROCESSES)
# include <signal.h>
# include <sys/types.h>
# include <sys/wait.h>
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#ifdef NO_ERRNO_GLOBAL
static int mzerrno = 0;
# define errno mzerrno
#else
# include <errno.h>
#endif
#ifndef DONT_IGNORE_PIPE_SIGNAL
# include <signal.h>
#endif
#ifdef USE_OSKIT_CONSOLE
# ifndef OSKIT_TEST
#  include <x86/pc/direct_cons.h>
# endif
extern int osk_not_console; /* set by cmd-line flag */
#endif
#ifdef MAC_FILE_SYSTEM
# include <Carbon.h>
#endif
#include <math.h> /* for fmod , used by default_sleep */
#include "schfd.h"

#ifndef MZ_BINARY
# define MZ_BINARY 0
#endif

#define mzAssert(x) /* if (!(x)) abort() */


/******************** Generic FILEs ********************/

typedef struct {
  MZTAG_IF_REQUIRED
  FILE *f;
} Scheme_Input_File;

typedef struct {
  MZTAG_IF_REQUIRED
  FILE *f;
} Scheme_Output_File;

/******************** Windows I/O and Subprocesses ********************/

#if defined(WINDOWS_PROCESSES) || defined(WINDOWS_FILE_HANDLES)

static void init_thread_memory();

# define WIN32_FD_HANDLES
# include <windows.h>
# include <process.h>
# include <signal.h>
# include <io.h>
# include <fcntl.h>
# define OS_SEMAPHORE_TYPE HANDLE
# define OS_MUTEX_TYPE CRITICAL_SECTION
# define OS_THREAD_TYPE HANDLE
#endif

#ifdef WINDOWS_FILE_HANDLES

# define MZ_FDS

typedef struct Win_FD_Input_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  volatile int avail, err, eof, checking;
  unsigned char *buffer;
  HANDLE checking_sema, ready_sema, you_clean_up_sema;
} Win_FD_Input_Thread;

typedef struct Win_FD_Output_Thread {
  /* This is malloced for use in a Win32 thread */
  HANDLE fd;
  int nonblocking;  /* non-zero => an NT pipe where non-blocking WriteFile
		       works. We still use a thread to detect when the
		       write has ben flushed, which in turn is needed to
		       know whether future writes will immediately succeed. */
  volatile flushed, needflush; /* Used for non-blocking, only. The flushed
				  flag communicates from the flush-testing thread
				  to the main thread. For efficiency, we request
				  flush checking only when needed (instead of
				  after every write); needflush indicates that
				  a flush check is currently needed, but hasn't
				  been started. */
  volatile int done, err_no;
  volatile unsigned int buflen, bufstart, bufend; /* used for blocking, only */
  unsigned char *buffer; /* used for blocking, only */
  HANDLE lock_sema, work_sema, ready_sema, you_clean_up_sema;
  /* lock_sema protects the fields, work_sema starts the flush or
     flush-checking thread to work, ready_sema indicates that a flush
     finished, and you_clean_up_sema is essentially a reference
     count */
} Win_FD_Output_Thread;

int scheme_stupid_windows_machine;

#endif

#if defined(WINDOWS_PROCESSES)
# include <ctype.h>
#endif

/******************** Unix Subprocesses ********************/

#if defined(UNIX_PROCESSES)
/* For process & system: */
typedef struct System_Child {
  MZTAG_IF_REQUIRED
  pid_t id;
  short done;
  int status;
  struct System_Child *next;
} System_Child;

System_Child *scheme_system_children;
#endif

typedef struct Scheme_Subprocess {
  Scheme_Object so;
  void *handle;
  int pid;
} Scheme_Subprocess;

#ifdef USE_FD_PORTS
# include <fcntl.h>
# include <sys/stat.h>
# define MZ_FDS
#endif


/******************** Mac Classic input ********************/

#ifdef MAC_FILE_SYSTEM
# define MZ_FDS
# define MAC_FILE_HANDLES
#endif

/******************** file-descriptor I/O ********************/

/* Windows/Mac I/O is piggy-backed on Unix file-descriptor I/O.  Making
   Windows file HANDLEs behave as nicely as file descriptors for
   non-blocking I/O requires a lot of work, and often a separate
   thread. The "th" and "oth" fields of Scheme_FD point to malloced
   (non-GCed) records that mediate the threads. */

#ifdef MZ_FDS

# define MZPORT_FD_BUFFSIZE 4096
# define MZPORT_FD_DIRECT_THRESHOLD MZPORT_FD_BUFFSIZE

/* The Scheme_FD type is used for both input and output */
typedef struct Scheme_FD {
  MZTAG_IF_REQUIRED
  long fd;                   /* fd is really a HANDLE in Windows */
  long bufcount, buffpos;
  char flushing, regfile, flush;
  char textmode; /* Windows: textmode => CRLF conversion; SOME_FDS_... => select definitely works */
  unsigned char *buffer;
  int *refcount;

# ifdef WINDOWS_FILE_HANDLES
  Win_FD_Input_Thread *th;   /* input mode */
  Win_FD_Output_Thread *oth; /* output mode */
# endif
} Scheme_FD;

# define MZ_FLUSH_NEVER 0
# define MZ_FLUSH_BY_LINE 1
# define MZ_FLUSH_ALWAYS 2

#endif

#ifdef SOME_FDS_ARE_NOT_SELECTABLE
# include <fcntl.h>
#endif

#if defined(WINDOWS_FILE_HANDLES) || defined(MAC_FILE_HANDLES)
# define FILENAME_EXN_E "%E"
#else
# define FILENAME_EXN_E "%e"
#endif

/******************** Globals and Prototypes ********************/

/* globals */
Scheme_Object scheme_eof[1];
Scheme_Object *scheme_orig_stdout_port;
Scheme_Object *scheme_orig_stderr_port;
Scheme_Object *scheme_orig_stdin_port;

Scheme_Object *(*scheme_make_stdin)(void) = NULL;
Scheme_Object *(*scheme_make_stdout)(void) = NULL;
Scheme_Object *(*scheme_make_stderr)(void) = NULL;

int scheme_file_open_count;

MZ_DLLSPEC int scheme_binary_mode_stdio;
void scheme_set_binary_mode_stdio(int v) { scheme_binary_mode_stdio =  v; }

static int special_is_ok;

/* locals */
#ifdef MZ_FDS
static Scheme_Object *fd_input_port_type;
#endif
#ifdef USE_OSKIT_CONSOLE
static Scheme_Object *oskit_console_input_port_type;
#endif
static Scheme_Object *file_input_port_type;
Scheme_Object *scheme_string_input_port_type;
#ifdef USE_TCP
Scheme_Object *scheme_tcp_input_port_type;
Scheme_Object *scheme_tcp_output_port_type;
#endif
#ifdef MZ_FDS
static Scheme_Object *fd_output_port_type;
#endif
static Scheme_Object *file_output_port_type;
Scheme_Object *scheme_string_output_port_type;
Scheme_Object *scheme_user_input_port_type;
Scheme_Object *scheme_user_output_port_type;
Scheme_Object *scheme_pipe_read_port_type;
Scheme_Object *scheme_pipe_write_port_type;
Scheme_Object *scheme_null_output_port_type;
Scheme_Object *scheme_redirect_output_port_type;

int scheme_force_port_closed;

#if defined(FILES_HAVE_FDS)
static int external_event_fd, put_external_event_fd, event_fd_set;
#endif

static void register_port_wait();

#ifdef MZ_FDS
static long flush_fd(Scheme_Output_Port *op,
		     const char * volatile bufstr, volatile unsigned long buflen,
		     volatile unsigned long offset, int immediate_only, int enable_break);
static void flush_if_output_fds(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data);
#endif

static Scheme_Object *subprocess(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_status(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_kill(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_pid(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_p(int c, Scheme_Object *args[]);
static Scheme_Object *subprocess_wait(int c, Scheme_Object *args[]);
static Scheme_Object *sch_shell_execute(int c, Scheme_Object *args[]);
static void register_subprocess_wait();

typedef struct Scheme_Read_Write_Evt {
  Scheme_Object so;
  Scheme_Object *port;
  Scheme_Object *v; /* peek skip or writeable special */
  char *str;
  long start, size;
} Scheme_Read_Write_Evt;

static int rw_evt_ready(Scheme_Object *rww, Scheme_Schedule_Info *sinfo);
static void rw_evt_wakeup(Scheme_Object *rww, void *fds);

static int progress_evt_ready(Scheme_Object *rww, Scheme_Schedule_Info *sinfo);

static Scheme_Object *
_scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name, int regfile);
static void default_sleep(float v, void *fds);
#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#if defined(WIN32_FD_HANDLES)
OS_SEMAPHORE_TYPE scheme_break_semaphore;
#endif

#ifdef MZ_FDS
static Scheme_Object *make_fd_input_port(int fd, Scheme_Object *name, int regfile, int textmode, int *refcount);
static Scheme_Object *make_fd_output_port(int fd, Scheme_Object *name, int regfile, int textmode, int read_too);
#endif
#ifdef USE_OSKIT_CONSOLE
static Scheme_Object *make_oskit_console_input_port();
#endif

static void force_close_output_port(Scheme_Object *port);
static void force_close_input_port(Scheme_Object *port);

static Scheme_Object *text_symbol, *binary_symbol;
static Scheme_Object *append_symbol, *error_symbol, *update_symbol;
static Scheme_Object *replace_symbol, *truncate_symbol, *truncate_replace_symbol;

Scheme_Object *scheme_none_symbol, *scheme_line_symbol, *scheme_block_symbol;

static Scheme_Object *exact_symbol;

#define READ_STRING_BYTE_BUFFER_SIZE 1024
static char *read_string_byte_buffer;

#define fail_err_symbol scheme_false

#include "schwinfd.h"

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void
scheme_init_port (Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(text_symbol);
  REGISTER_SO(binary_symbol);
  REGISTER_SO(append_symbol);
  REGISTER_SO(error_symbol);
  REGISTER_SO(replace_symbol);
  REGISTER_SO(truncate_symbol);
  REGISTER_SO(truncate_replace_symbol);
  REGISTER_SO(update_symbol);

  text_symbol = scheme_intern_symbol("text");
  binary_symbol = scheme_intern_symbol("binary");
  append_symbol = scheme_intern_symbol("append");
  error_symbol = scheme_intern_symbol("error");
  replace_symbol = scheme_intern_symbol("replace");
  truncate_symbol = scheme_intern_symbol("truncate");
  truncate_replace_symbol = scheme_intern_symbol("truncate/replace");
  update_symbol = scheme_intern_symbol("update");

  REGISTER_SO(scheme_none_symbol);
  REGISTER_SO(scheme_line_symbol);
  REGISTER_SO(scheme_block_symbol);

  scheme_none_symbol = scheme_intern_symbol("none");
  scheme_line_symbol = scheme_intern_symbol("line");
  scheme_block_symbol = scheme_intern_symbol("block");

  REGISTER_SO(exact_symbol);

  exact_symbol = scheme_intern_symbol("exact");

  REGISTER_SO(scheme_orig_stdout_port);
  REGISTER_SO(scheme_orig_stderr_port);
  REGISTER_SO(scheme_orig_stdin_port);
#ifdef MZ_FDS
  REGISTER_SO(fd_input_port_type);
  REGISTER_SO(fd_output_port_type);
#endif
#ifdef USE_OSKIT_CONSOLE
  REGISTER_SO(oskit_console_input_port_type);
#endif
  REGISTER_SO(file_input_port_type);
  REGISTER_SO(scheme_string_input_port_type);
#ifdef USE_TCP
  REGISTER_SO(scheme_tcp_input_port_type);
  REGISTER_SO(scheme_tcp_output_port_type);
#endif
  REGISTER_SO(file_output_port_type);
  REGISTER_SO(scheme_string_output_port_type);
  REGISTER_SO(scheme_user_input_port_type);
  REGISTER_SO(scheme_user_output_port_type);
  REGISTER_SO(scheme_pipe_read_port_type);
  REGISTER_SO(scheme_pipe_write_port_type);
  REGISTER_SO(scheme_null_output_port_type);
  REGISTER_SO(scheme_redirect_output_port_type);

#if defined(UNIX_PROCESSES)
  REGISTER_SO(scheme_system_children);
#endif

#ifndef DONT_IGNORE_PIPE_SIGNAL
  START_XFORM_SKIP;
  MZ_SIGSET(SIGPIPE, SIG_IGN);
  END_XFORM_SKIP;
#endif

  if (!scheme_sleep)
    scheme_sleep = default_sleep;

  scheme_eof->type = scheme_eof_type;

  scheme_string_input_port_type = scheme_make_port_type("<string-input-port>");
  scheme_string_output_port_type = scheme_make_port_type("<string-output-port>");

#ifdef MZ_FDS
  fd_input_port_type = scheme_make_port_type("<stream-input-port>");
  fd_output_port_type = scheme_make_port_type("<stream-output-port>");
#endif
#ifdef USE_OSKIT_CONSOLE
  oskit_console_input_port_type = scheme_make_port_type("<console-input-port>");
#endif

  file_input_port_type = scheme_make_port_type("<file-input-port>");
  file_output_port_type = scheme_make_port_type("<file-output-port>");

  scheme_user_input_port_type = scheme_make_port_type("<user-input-port>");
  scheme_user_output_port_type = scheme_make_port_type("<user-output-port>");

  scheme_pipe_read_port_type = scheme_make_port_type("<pipe-input-port>");
  scheme_pipe_write_port_type = scheme_make_port_type("<pipe-output-port>");

#ifdef USE_TCP
  scheme_tcp_input_port_type = scheme_make_port_type("<tcp-input-port>");
  scheme_tcp_output_port_type = scheme_make_port_type("<tcp-output-port>");
#endif

  scheme_null_output_port_type = scheme_make_port_type("<null-output-port>");
  scheme_redirect_output_port_type = scheme_make_port_type("<redirect-output-port>");

#ifdef WIN32_FD_HANDLES
  scheme_break_semaphore = CreateSemaphore(NULL, 0, 1, NULL);

  /* We'll need to know whether this is Win95 or WinNT: */
  {
    OSVERSIONINFO info;
    info.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&info);
    if (info.dwPlatformId == VER_PLATFORM_WIN32_NT)
      scheme_stupid_windows_machine = -1; /* not as stupid */
    else
      scheme_stupid_windows_machine = 1;
  }
#endif

  scheme_orig_stdin_port = (scheme_make_stdin
			    ? scheme_make_stdin()
#ifdef USE_OSKIT_CONSOLE
			    : (osk_not_console
			       ? scheme_make_named_file_input_port(stdin, scheme_intern_symbol("stdin"))
			       : make_oskit_console_input_port())
#else
# ifdef MZ_FDS
#  ifdef WINDOWS_FILE_HANDLES
			    : make_fd_input_port((int)GetStdHandle(STD_INPUT_HANDLE), scheme_intern_symbol("stdin"), 0, 0, NULL)
#  else
			    : make_fd_input_port(0, scheme_intern_symbol("stdin"), 0, 0, NULL)
#  endif
# else
			    : scheme_make_named_file_input_port(stdin, scheme_intern_symbol("stdin"))
# endif
#endif
			    );

  scheme_orig_stdout_port = (scheme_make_stdout
			     ? scheme_make_stdout()
#ifdef MZ_FDS
# ifdef WINDOWS_FILE_HANDLES
			     : make_fd_output_port((int)GetStdHandle(STD_OUTPUT_HANDLE), 
						   scheme_intern_symbol("stdout"), 0, 0, 0)
# else
			     : make_fd_output_port(1, scheme_intern_symbol("stdout"), 0, 0, 0)
# endif
#else
			     : scheme_make_file_output_port(stdout)
#endif
			     );

  scheme_orig_stderr_port = (scheme_make_stderr
			     ? scheme_make_stderr()
#ifdef MZ_FDS
# ifdef WINDOWS_FILE_HANDLES
			     : make_fd_output_port((int)GetStdHandle(STD_ERROR_HANDLE), 
						   scheme_intern_symbol("stderr"), 0, 0, 0)
# else
			     : make_fd_output_port(2, scheme_intern_symbol("stderr"), 0, 0, 0)
# endif
#else
			     : scheme_make_file_output_port(stderr)
#endif
			     );

#ifdef MZ_FDS
  scheme_add_atexit_closer(flush_if_output_fds);
  /* Note: other threads might continue to write even after
     the flush completes, but that's the threads' problem.
     All writing by the main thread will get flushed on exit
     (but not, of course, if the thread is shutdown via a
     custodian). */
#endif

#if defined(FILES_HAVE_FDS)
# ifndef USE_OSKIT_CONSOLE
  /* Set up a pipe for signalling external events: */
  {
    int fds[2];
    if (!pipe(fds)) {
      external_event_fd = fds[0];
      put_external_event_fd = fds[1];
      fcntl(external_event_fd, F_SETFL, MZ_NONBLOCKING);
      fcntl(put_external_event_fd, F_SETFL, MZ_NONBLOCKING);
    }
  }
# endif
#endif

  scheme_init_port_config();

  register_port_wait();

  scheme_add_global_constant("subprocess",
			     scheme_make_prim_w_arity2(subprocess,
						       "subprocess",
						       4, -1,
						       4, 4),
			     env);
  scheme_add_global_constant("subprocess-status",
			     scheme_make_prim_w_arity(subprocess_status,
						      "subprocess-status",
						      1, 1),
			     env);
  scheme_add_global_constant("subprocess-kill",
			     scheme_make_prim_w_arity(subprocess_kill,
						      "subprocess-kill",
						      2, 2),
			     env);
  scheme_add_global_constant("subprocess-pid",
			     scheme_make_prim_w_arity(subprocess_pid,
						      "subprocess-pid",
						      1, 1),
			     env);
  scheme_add_global_constant("subprocess?",
			     scheme_make_prim_w_arity(subprocess_p,
						      "subprocess?",
						      1, 1),
			     env);
  scheme_add_global_constant("subprocess-wait",
			     scheme_make_prim_w_arity(subprocess_wait,
						      "subprocess-wait",
						      1, 1),
			     env);


  register_subprocess_wait();

  scheme_add_global_constant("shell-execute",
			     scheme_make_prim_w_arity(sch_shell_execute,
						      "shell-execute",
						      5, 5),
			     env);

  REGISTER_SO(read_string_byte_buffer);

  scheme_add_evt(scheme_progress_evt_type, (Scheme_Ready_Fun)progress_evt_ready, NULL, NULL, 1);
  scheme_add_evt(scheme_write_evt_type, (Scheme_Ready_Fun)rw_evt_ready, rw_evt_wakeup, NULL, 1);
}

void scheme_init_port_config(void)
{
  Scheme_Config *config;

  config = scheme_current_config();

  scheme_set_param(config, MZCONFIG_INPUT_PORT,
		   scheme_orig_stdin_port);
  scheme_set_param(config, MZCONFIG_OUTPUT_PORT,
		   scheme_orig_stdout_port);
  scheme_set_param(config, MZCONFIG_ERROR_PORT,
		   scheme_orig_stderr_port);
}

Scheme_Object * scheme_make_eof (void)
{
  return scheme_eof;
}

/*========================================================================*/
/*                                fd arrays                               */
/*========================================================================*/

/* Implement fd arrays (FD_SET, etc) with a runtime-determined size.
   Also implement special hooks for Windows "descriptors", like
   even queues and semaphores. */

#ifdef USE_DYNAMIC_FDSET_SIZE
static int dynamic_fd_size;

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

void *scheme_alloc_fdset_array(int count, int permanent)
{
  /* Note: alloc only at the end, because this function
     isn't annotated. We skip annotation so that it's
     ok with OS X use from default_sleep() */

  if (!dynamic_fd_size) {
#ifdef USE_ULIMIT
    dynamic_fd_size = ulimit(4, 0);
#else
    dynamic_fd_size = getdtablesize();
#endif
    /* divide by bits-per-byte: */
    dynamic_fd_size = (dynamic_fd_size + 7) >> 3;
    /* word-align: */
    if (dynamic_fd_size % sizeof(void*))
      dynamic_fd_size += sizeof(void*) - (dynamic_fd_size % sizeof(void*));
  }

  if (permanent)
    return scheme_malloc_eternal(count * dynamic_fd_size);
  else
    return scheme_malloc_atomic(count * dynamic_fd_size);
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

void *scheme_init_fdset_array(void *fdarray, int count)
{
  return fdarray;
}

void *scheme_get_fdset(void *fdarray, int pos)
{
  return ((char *)fdarray) + (pos * dynamic_fd_size);
}

void scheme_fdzero(void *fd)
{
  memset(fd, 0, dynamic_fd_size);
}

#else

#if defined(WIN32_FD_HANDLES)
# define fdset_type win_extended_fd_set
#else
# define fdset_type fd_set
#endif

void *scheme_alloc_fdset_array(int count, int permanent)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP) || defined(WIN32_FD_HANDLES)
  void *fdarray;
  if (permanent)
    fdarray = scheme_malloc_eternal(count * sizeof(fdset_type));
  else
    fdarray = scheme_malloc_atomic(count * sizeof(fdset_type));
# if defined(WIN32_FD_HANDLES)
  if (count) {
    ((win_extended_fd_set *)fdarray)->added = 0;
    ((win_extended_fd_set *)fdarray)->num_handles = 0;
    ((win_extended_fd_set *)fdarray)->no_sleep = 0;
    ((win_extended_fd_set *)fdarray)->wait_event_mask = 0;
  }
# endif
  return fdarray;
#else
  return NULL;
#endif
}

void *scheme_init_fdset_array(void *fdarray, int count)
{
#if defined(WIN32_FD_HANDLES)
  if (count) {
    ((win_extended_fd_set *)fdarray)->added = 0;
    ((win_extended_fd_set *)fdarray)->num_handles = 0;
    ((win_extended_fd_set *)fdarray)->wait_event_mask = 0;
  }
#endif
  return fdarray;
}

void *scheme_get_fdset(void *fdarray, int pos)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP) || defined(WIN32_FD_HANDLES)
  return ((fdset_type *)fdarray) + pos;
#else
  return NULL;
#endif
}

void scheme_fdzero(void *fd)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_ZERO((fd_set *)fd);
#endif
#if defined(WIN32_FD_HANDLES)
  ((win_extended_fd_set *)fd)->added = 0;
#endif
}

#endif

void scheme_fdclr(void *fd, int n)
{
#if defined(WIN32_FD_HANDLES)
  if (FD_ISSET(n, ((fd_set *)fd)))
    --((win_extended_fd_set *)fd)->added;
#endif
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_CLR((unsigned)n, ((fd_set *)fd));
#endif
}

void scheme_fdset(void *fd, int n)
{
#if defined(WIN32_FD_HANDLES)
  if (!FD_ISSET(n, ((fd_set *)fd)))
    ((win_extended_fd_set *)fd)->added++;
#endif
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  FD_SET(n, ((fd_set *)fd));
#endif
}

int scheme_fdisset(void *fd, int n)
{
#if defined(FILES_HAVE_FDS) || defined(USE_SOCKETS_TCP)
  return FD_ISSET(n, ((fd_set *)fd));
#else
  return 0;
#endif
}

void scheme_add_fd_handle(void *h, void *fds, int repost)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fds;
  OS_SEMAPHORE_TYPE *hs;
  int i, *rps;

  i = efd->num_handles;
  hs = MALLOC_N_ATOMIC(OS_SEMAPHORE_TYPE, i + 3);
  /*                 Leave room for two more -^ */
  rps = MALLOC_N_ATOMIC(int, i + 3);
  hs[i] = (OS_SEMAPHORE_TYPE)h;
  rps[i] = repost;
  while (i--) {
    hs[i] = efd->handles[i];
    rps[i] = efd->repost_sema[i];
  }
  efd->num_handles++;
  efd->handles= hs;
  efd->repost_sema = rps;
#else
  /* Do nothing. */
#endif
}

void scheme_add_fd_nosleep(void *fds)
{
#if defined(WIN32_FD_HANDLES)
  win_extended_fd_set *efd = (win_extended_fd_set *)fds;
  efd->no_sleep = 1;
#else
#endif
}

void scheme_add_fd_eventmask(void *fds, int mask)
{
#if defined(WIN32_FD_HANDLES)
  ((win_extended_fd_set *)fds)->wait_event_mask |= mask;
#endif
}

/*========================================================================*/
/*                      Windows thread suspension                         */
/*========================================================================*/

/* MzScheme creates Windows threads for various purposes, including
   non-blocking FILE reads. Unfortunately, these threads can confuse
   the GC if they move virtual pages around while its marking. So we
   remember each created thread and suspend it during GC.

   This work is not necessary if GC_use_registered_statics is set. */


#ifdef WINDOWS_PROCESSES
typedef struct Scheme_Thread_Memory {
  MZTAG_IF_REQUIRED
  void *handle;
  void *subhandle;
  int autoclose;
  struct Scheme_Thread_Memory *prev;
  struct Scheme_Thread_Memory *next;
} Scheme_Thread_Memory;

Scheme_Thread_Memory *tm_start, *tm_next;

extern MZ_DLLIMPORT void (*GC_collect_start_callback)(void);
extern MZ_DLLIMPORT void (*GC_collect_end_callback)(void);

void scheme_init_thread_memory()
{
  REGISTER_SO(tm_start);
  REGISTER_SO(tm_next);

  /* We start with a pre-allocated tm because we
     want to register a thread before performing any
     allocations. */
#ifdef MZ_PRECISE_GC
  tm_next = (Scheme_Thread_Memory *)malloc(sizeof(Scheme_Thread_Memory));
#else
  tm_next = MALLOC_ONE_RT(Scheme_Thread_Memory);
#endif
#ifdef MZTAG_REQUIRED
  tm_next->type = scheme_rt_thread_memory;
#endif

  /* scheme_init_thread() will replace these: */
  GC_collect_start_callback = scheme_suspend_remembered_threads;
  GC_collect_end_callback = scheme_resume_remembered_threads;
}

Scheme_Thread_Memory *scheme_remember_thread(void *t, int autoclose)
{
  Scheme_Thread_Memory *tm = tm_next;

  tm->handle = t;
  tm->subhandle = NULL;
  tm->autoclose = autoclose;

  tm->prev = NULL;
  tm->next = tm_start;
  if (tm->next)
    tm->next->prev = tm;
  tm_start = tm;

#ifdef MZ_PRECISE_GC
  tm_next = (Scheme_Thread_Memory *)malloc(sizeof(Scheme_Thread_Memory));
#else
  tm_next = MALLOC_ONE_RT(Scheme_Thread_Memory);
#endif
#ifdef MZTAG_REQUIRED
  tm_next->type = scheme_rt_thread_memory;
#endif

  return tm;
}

void scheme_remember_subthread(struct Scheme_Thread_Memory *tm, void *t)
{
  tm->subhandle = t;
}

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

void scheme_forget_thread(struct Scheme_Thread_Memory *tm)
{
  if (tm->prev)
    tm->prev->next = tm->next;
  else
    tm_start = tm->next;

  if (tm->next)
    tm->next->prev = tm->prev;

#ifdef MZ_PRECISE_GC
  free(tm);
#endif
}

void scheme_forget_subthread(struct Scheme_Thread_Memory *tm)
{
  tm->subhandle = NULL;
}

void scheme_suspend_remembered_threads(void)
{
  Scheme_Thread_Memory *tm, *next, *prev = NULL;

  for (tm = tm_start; tm; tm = next) {
    next = tm->next;

    if (tm->autoclose) {
      if (WaitForSingleObject(tm->handle, 0) == WAIT_OBJECT_0) {
	CloseHandle((HANDLE)tm->handle);
	tm->handle = NULL;
	if (prev)
	  prev->next = tm->next;
	else
	  tm_start = tm->next;
#ifdef MZ_PRECISE_GC
	free(tm);
#endif
      }
    }

    if (tm->handle) {
      SuspendThread((HANDLE)tm->handle);
      if (tm->subhandle)
	SuspendThread((HANDLE)tm->subhandle);
      prev = tm;
    }
  }
}

void scheme_resume_remembered_threads(void)
{
  Scheme_Thread_Memory *tm;

  for (tm = tm_start; tm; tm = tm->next) {
    if (tm->subhandle)
      ResumeThread((HANDLE)tm->subhandle);
    ResumeThread((HANDLE)tm->handle);
  }
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

#endif

/*========================================================================*/
/*                        Generic port support                            */
/*========================================================================*/


Scheme_Object *scheme_make_port_type(const char *name)
{
  return scheme_make_symbol(name);
}

static void init_port_locations(Scheme_Port *ip)
{
  int cl;

  ip->position = 0;
  ip->readpos = 0; /* like position, but post UTF-8 decoding, collapses CRLF, etc. */
  ip->lineNumber = 1;
  ip->oldColumn = 0;
  ip->column = 0;
  ip->charsSinceNewline = 1;
  cl = SCHEME_TRUEP(scheme_get_param(scheme_current_config(), MZCONFIG_PORT_COUNT_LINES));
  ip->count_lines = cl;
}

Scheme_Input_Port *
scheme_make_input_port(Scheme_Object *subtype,
		       void *data,
		       Scheme_Object *name,
		       Scheme_Get_String_Fun get_string_fun,
		       Scheme_Peek_String_Fun peek_string_fun,
		       Scheme_Progress_Evt_Fun progress_evt_fun,
		       Scheme_Peeked_Read_Fun peeked_read_fun,
		       Scheme_In_Ready_Fun byte_ready_fun,
		       Scheme_Close_Input_Fun close_fun,
		       Scheme_Need_Wakeup_Input_Fun need_wakeup_fun,
		       int must_close)
{
  Scheme_Input_Port *ip;

  ip = MALLOC_ONE_TAGGED(Scheme_Input_Port);
  ip->p.so.type = scheme_input_port_type;
  ip->sub_type = subtype;
  ip->port_data = data;
  ip->get_string_fun = get_string_fun;
  ip->peek_string_fun = peek_string_fun;
  ip->progress_evt_fun = progress_evt_fun;
  ip->peeked_read_fun = peeked_read_fun;
  ip->byte_ready_fun = byte_ready_fun;
  ip->need_wakeup_fun = need_wakeup_fun;
  ip->close_fun = close_fun;
  ip->name = name;
  ip->ungotten_count = 0;
  ip->closed = 0;
  ip->read_handler = NULL;
  init_port_locations((Scheme_Port *)ip);

  if (progress_evt_fun == scheme_progress_evt_via_get)
    ip->unless_cache = scheme_false;

  if (must_close) {
    Scheme_Custodian_Reference *mref;
    mref = scheme_add_managed(NULL,
			      (Scheme_Object *)ip,
			      (Scheme_Close_Custodian_Client *)force_close_input_port,
			      NULL, must_close);
    ip->mref = mref;
  } else
    ip->mref = NULL;

  return (ip);
}

void scheme_set_port_location_fun(Scheme_Port *port,
				  Scheme_Location_Fun location_fun)
{
  port->location_fun = location_fun;
}

void scheme_set_port_count_lines_fun(Scheme_Port *port,
				     Scheme_Count_Lines_Fun count_lines_fun)
{
  port->count_lines_fun = count_lines_fun;
}

static int evt_input_port_p(Scheme_Object *p)
{
  return 1;
}

Scheme_Output_Port *
scheme_make_output_port(Scheme_Object *subtype,
			void *data,
			Scheme_Object *name,
			Scheme_Write_String_Evt_Fun write_string_evt_fun,
			Scheme_Write_String_Fun write_string_fun,
			Scheme_Out_Ready_Fun ready_fun,
			Scheme_Close_Output_Fun close_fun,
			Scheme_Need_Wakeup_Output_Fun need_wakeup_fun,
			Scheme_Write_Special_Evt_Fun write_special_evt_fun,
			Scheme_Write_Special_Fun write_special_fun,
			int must_close)
{
  Scheme_Output_Port *op;

  op = MALLOC_ONE_TAGGED(Scheme_Output_Port);
  op->p.so.type = scheme_output_port_type;
  op->sub_type = subtype;
  op->port_data = data;
  op->name = name;
  op->write_string_evt_fun = write_string_evt_fun;
  op->write_string_fun = write_string_fun;
  op->close_fun = close_fun;
  op->ready_fun = ready_fun;
  op->need_wakeup_fun = need_wakeup_fun;
  op->write_special_evt_fun = write_special_evt_fun;
  op->write_special_fun = write_special_fun;
  op->closed = 0;
  op->display_handler = NULL;
  op->write_handler = NULL;
  op->print_handler = NULL;
  init_port_locations((Scheme_Port *)op);

  if (must_close) {
    Scheme_Custodian_Reference *mref;
    mref = scheme_add_managed(NULL,
			      (Scheme_Object *)op,
			      (Scheme_Close_Custodian_Client *)force_close_output_port,
			      NULL, must_close);
    op->mref = mref;
  } else
    op->mref = NULL;

  return op;
}

static int evt_output_port_p(Scheme_Object *p)
{
  return 1;
}

static int output_ready(Scheme_Object *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Output_Port *op;

  op = (Scheme_Output_Port *)port;

  if (op->closed)
    return 1;

  if (SAME_OBJ(scheme_user_output_port_type, op->sub_type)) {
    /* We can't call the normal ready because that might run Scheme
       code, and this function is called by the scheduler when
       false_pos_ok is true. So, in that case, we asume that if the
       port's evt is ready, then the port is ready. (After
       all, false positives are ok in that mode.) Even when the
       scheduler isn't requesting the status, we need sinfo. */
    return scheme_user_port_write_probably_ready(op, sinfo);
  }

  if (op->ready_fun) {
    Scheme_Out_Ready_Fun rf;
    rf = op->ready_fun;
    return rf(op);
  }

  return 1;
}

static void output_need_wakeup (Scheme_Object *port, void *fds)
{
  Scheme_Output_Port *op;

  /* If this is a user output port and its evt needs a wakeup, we
     shouldn't get here. The target use above will take care of it. */

  op = (Scheme_Output_Port *)port;
  if (op->need_wakeup_fun) {
    Scheme_Need_Wakeup_Output_Fun f;
    f = op->need_wakeup_fun;
    f(op, fds);
  }
}

int scheme_byte_ready_or_user_port_ready(Scheme_Object *p, Scheme_Schedule_Info *sinfo)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;

  if (ip->closed)
    return 1;

  if (SAME_OBJ(scheme_user_input_port_type, ip->sub_type)) {
    /* We can't call the normal byte_ready because that runs Scheme
       code, and this function is called by the scheduler when
       false_pos_ok is true. So, in that case, we asume that if the
       port's evt is ready, then the port is ready. (After
       all, false positives are ok in that mode.) Even when the
       scheduler isn't requesting the status, we need sinfo. */
    return scheme_user_port_byte_probably_ready(ip, sinfo);
  } else
    return scheme_byte_ready(p);
}

static void register_port_wait()
{
  scheme_add_evt(scheme_input_port_type,
		  (Scheme_Ready_Fun)scheme_byte_ready_or_user_port_ready, scheme_need_wakeup,
		  evt_input_port_p, 1);
  scheme_add_evt(scheme_output_port_type,
		  (Scheme_Ready_Fun)output_ready, output_need_wakeup,
		  evt_output_port_p, 1);
}

XFORM_NONGCING static int pipe_char_count(Scheme_Object *p)
{
  if (p) {
    Scheme_Pipe *pipe;
    pipe = (Scheme_Pipe *)((Scheme_Input_Port *)p)->port_data;

    if (pipe->bufstart <= pipe->bufend)
      return pipe->bufend - pipe->bufstart;
    else
      return (pipe->buflen - pipe->bufstart) + pipe->bufend;
  } else
    return 0;
}

/****************************** main input reader ******************************/

static void post_progress(Scheme_Input_Port *ip)
{
  scheme_post_sema_all(ip->progress_evt);
  ip->progress_evt = NULL;
}

XFORM_NONGCING static void inc_pos(Scheme_Port *ip, int a)
{
  ip->column += a;
  ip->readpos += a;
  ip->charsSinceNewline += a;
  ip->utf8state = 0;
}

static Scheme_Object *quick_plus(Scheme_Object *s, long v)
{
  if (SCHEME_INTP(s)) {
    int k;
    k = SCHEME_INT_VAL(s);
    if ((k < 0x1000000) && (v < 0x1000000)) {
      k += v;
      return scheme_make_integer(k);
    }
  }

  /* Generic addition, but we might not be in a position to allow
     thread swaps */
  scheme_start_atomic();
  s = scheme_bin_plus(s, scheme_make_integer(v));
  scheme_end_atomic_no_swap();

  return s;
}

#define state_len(state) ((state >> 3) & 0x7)

XFORM_NONGCING static void do_count_lines(Scheme_Port *ip, const char *buffer, long offset, long got)
{
  long i;
  int c, degot = 0;

  mzAssert(ip->lineNumber >= 0);
  mzAssert(ip->column >= 0);
  mzAssert(ip->position >= 0);

  ip->oldColumn = ip->column; /* works for a single-char read, like `read' */

  ip->readpos += got; /* add for CR LF below */

  /* Find start of last line: */
  for (i = got, c = 0; i--; c++) {
    if (buffer[offset + i] == '\n' || buffer[offset + i] == '\r') {
      break;
    }
  }

  /* Count UTF-8-decoded chars, up to last line: */
  if (i >= 0) {
    int state = ip->utf8state;
    int n;
    degot += state_len(state);
    n = scheme_utf8_decode_count((const unsigned char *)buffer, offset, offset + i + 1, &state, 0, '?');
    degot += (i + 1 - n);
    ip->utf8state = 0; /* assert: state == 0, because we ended with a newline */
  }
	
  if (i >= 0) {
    int n = 0;
    ip->charsSinceNewline = c + 1;
    i++;
    /* Continue walking, back over the previous lines, to find
       out how many there were: */
    while (i--) {
      if (buffer[offset + i] == '\n') {
	if (!(i && (buffer[offset + i - 1] == '\r'))
	    && !(!i && ip->was_cr)) {
	  n++;
	} else
	  degot++; /* adjust positions for CRLF -> LF conversion */
      } else if (buffer[offset + i] == '\r') {
	n++;
      }
    }
	 	  
    mzAssert(n > 0);
    ip->lineNumber += n;
    ip->was_cr = (buffer[offset + got - 1] == '\r');
    /* Now reset column to 0: */
    ip->column = 0;
  } else {
    ip->charsSinceNewline += c;
  }

  /* Do the last line to get the column count right and to
     further adjust positions for UTF-8 decoding: */
  {
    int col = ip->column, n;
    int prev_i = got - c;
    int state = ip->utf8state;
    n = state_len(state);
    degot += n;
    col -= n;
    for (i = prev_i; i < got; i++) {
      if (buffer[offset + i] == '\t') {
	n = scheme_utf8_decode_count((const unsigned char *)buffer, offset + prev_i, offset + i, &state, 0, '?');
	degot += ((i - prev_i) - n);
	col += n;
	col = col - (col & 0x7) + 8;
	prev_i = i + 1;
      }
    }
    if (prev_i < i) {
      n = scheme_utf8_decode_count((const unsigned char *)buffer, offset + prev_i, offset + i, &state, 1, '?');
      n += state_len(state);
      col += n;
      degot += ((i - prev_i) - n);
    }
    ip->column = col;
    ip->utf8state = state;
  }

  ip->readpos -= degot;

  mzAssert(ip->lineNumber >= 0);
  mzAssert(ip->column >= 0);
  mzAssert(ip->position >= 0);
}

long scheme_get_byte_string_unless(const char *who,
				   Scheme_Object *port,
				   char *buffer, long offset, long size,
				   int only_avail,
				   int peek, Scheme_Object *peek_skip,
				   Scheme_Object *unless_evt)
{
  Scheme_Input_Port *ip;
  long got = 0, total_got = 0, gc;
  int special_ok = special_is_ok, check_special;
  Scheme_Get_String_Fun gs;
  Scheme_Peek_String_Fun ps;

  /* See also get_one_byte, below. Any change to this function
     may require a change to 1-byte specialization of get_one_byte. */

  /* back-door argument: */
  special_is_ok = 0;

  if (!size) {
    if (only_avail == -1) {
      /* We might need to break. */
      if (scheme_current_thread->external_break) {
	scheme_thread_block_enable_break(0.0, 1);
	scheme_current_thread->ran_some = 1;
      }
    }
    return 0;
  }
  if (!peek_skip)
    peek_skip = scheme_make_integer(0);

  ip = (Scheme_Input_Port *)port;

  gs = ip->get_string_fun;
  ps = ip->peek_string_fun;

  while (1) {
    SCHEME_USE_FUEL(1);

    CHECK_PORT_CLOSED(who, "input", port, ip->closed);

    if (ip->input_lock)
      scheme_wait_input_allowed(ip, only_avail);

    if (only_avail == -1) {
      /* We might need to break. */
      if (scheme_current_thread->external_break) {
	scheme_thread_block_enable_break(0.0, 1);
	scheme_current_thread->ran_some = 1;
      }
    }

    if ((ip->ungotten_count || pipe_char_count(ip->peeked_read))
	&& (!total_got || !peek)) {
      long l, i;
      unsigned char *s;

      i = ip->ungotten_count;
      /* s will be in reverse order */

      if (peek) {
	if (!SCHEME_INTP(peek_skip) || (i < SCHEME_INT_VAL(peek_skip))) {
	  peek_skip = scheme_bin_minus(peek_skip, scheme_make_integer(i));
	  i = 0;
	} else {
	  i -= SCHEME_INT_VAL(peek_skip);
	  peek_skip = scheme_make_integer(0);
	}
      }

      if (i < size)
	l = i;
      else
	l = size;

      size -= l;
      s = (unsigned char *)ip->ungotten; /* Not GC-safe! */
      while (l--) {
	buffer[offset + got++] = s[--i];
      }
      s = NULL;

      if (!peek)
	ip->ungotten_count = i;

      l = pipe_char_count(ip->peeked_read);
      if (size && l) {
	if (SCHEME_INTP(peek_skip) && (l > SCHEME_INT_VAL(peek_skip))) {
	  l -= SCHEME_INT_VAL(peek_skip);

	  if (l > size)
	    l = size;

	  if (l) {
	    scheme_get_byte_string("depipe", ip->peeked_read,
				   buffer, offset + got, l,
				   1, peek, peek_skip);
	    size -= l;
	    got += l;
	    peek_skip = scheme_make_integer(0);
	    if (!peek && ip->progress_evt)
	      post_progress(ip);
	  }
	} else
	  peek_skip = scheme_bin_minus(peek_skip, scheme_make_integer(l));
      }
      check_special = (!got || peek);
    } else
      check_special = 1;

    if (check_special && ip->ungotten_special) {
      if (!special_ok) {
	if (!peek) {
	  if (ip->progress_evt)
	    post_progress(ip);
	  ip->ungotten_special = NULL;
	}
	scheme_bad_time_for_special(who, port);
      }
      if (!peek) {
	ip->special = ip->ungotten_special;
	ip->ungotten_special = NULL;
      } else {
	if (peek_skip != scheme_make_integer(0))
	  scheme_bad_time_for_special(who, port);
      }

      if (!peek) {
	if (ip->p.position >= 0)
	  ip->p.position++;
	if (ip->p.count_lines)
	  inc_pos((Scheme_Port *)ip, 1);
      }

      if (!peek && ip->progress_evt)
	post_progress(ip);

      return SCHEME_SPECIAL;
    }

    if (got && ((only_avail == 1) || (only_avail == -1)))
      only_avail = 2;

    /* If we get this far in peek mode, ps is NULL, peek_skip is non-zero, and
       we haven't gotten anything so far, it means that we need to read before we
       can actually peek. Handle this case with a recursive peek that starts
       from the current position, then set peek_skip to 0 and go on. */
    while (peek && !ps && (peek_skip != scheme_make_integer(0)) && !total_got && !got
	   && (ip->pending_eof < 2)) {
      char *tmp;
      int v, pcc;
      long skip;
      Scheme_Cont_Frame_Data cframe;


#     define MAX_SKIP_TRY_AMOUNT 65536

      if (SCHEME_INTP(peek_skip)) {
	skip = SCHEME_INT_VAL(peek_skip);
	if (skip > MAX_SKIP_TRY_AMOUNT)
	  skip = MAX_SKIP_TRY_AMOUNT;
      } else
	skip = MAX_SKIP_TRY_AMOUNT;

      tmp = (char *)scheme_malloc_atomic(skip);
      pcc = pipe_char_count(ip->peeked_read);

      if (only_avail == -1) {
	/* To implement .../enable-break, we enable
	   breaks during the skip-ahead peek. */
	scheme_push_break_enable(&cframe, 1, 1);
      }

      v = scheme_get_byte_string_unless(who, port, tmp, 0, skip,
					(only_avail == 2) ? 2 : 0,
					1, scheme_make_integer(ip->ungotten_count + pcc),
					unless_evt);

      if (only_avail == -1) {
	scheme_pop_break_enable(&cframe, 0);
      }

      if (v == EOF) {
	ip->p.utf8state = 0;
	return EOF;
      } else if (v == SCHEME_SPECIAL) {
	ip->special = NULL;
	scheme_bad_time_for_special(who, port);
      } else if (v == skip) {
	peek_skip = scheme_bin_minus(peek_skip, scheme_make_integer(skip));
	/* Ok... ready to continue (if skip == peek_skip) */
      } else
	return 0;
    }

    if (size) {
      int nonblock;

      if (only_avail == 2) {
	if (got)
	  nonblock = 2;
	else
	  nonblock = 1;
      } else if (only_avail == -1)
	nonblock = -1;
      else
	nonblock = 0;

      if (unless_evt && SAME_TYPE(SCHEME_TYPE(unless_evt), scheme_progress_evt_type))
	unless_evt = SCHEME_PTR2_VAL(unless_evt);

      if (ip->pending_eof > 1) {
	ip->pending_eof = 1;
	gc = EOF;
      } else {
	/* Call port's get or peek function. But first, set up
	   an "unless" to detect other accesses of the port
	   if we block. */
	Scheme_Object *unless;
	  
	if (nonblock > 0) {
	  if (ip->unless)
	    unless = ip->unless;
	  else
	    unless = NULL;
	} else if (ip->unless_cache) {
	  if (ip->unless) {
	    unless = ip->unless;
	    /* Setting car to #f means that it can't be recycled */
	    SCHEME_CAR(unless) = scheme_false;
	  } else if (SCHEME_TRUEP(ip->unless_cache)) {
	    unless = ip->unless_cache;
	    ip->unless_cache = scheme_false;
	    ip->unless = unless;
	  } else {
	    unless = scheme_make_raw_pair(NULL, NULL);
	    ip->unless = unless;
	  }
	  if (unless_evt)
	    SCHEME_CDR(unless) = unless_evt;
	} else
	  unless = unless_evt;

	/* Finally, call port's get or peek: */
	if (peek && ps)
	  gc = ps(ip, buffer, offset + got, size, peek_skip, nonblock, unless);
	else {
	  gc = gs(ip, buffer, offset + got, size, nonblock, unless);

	  if (!peek && gc && ip->progress_evt
	      && (gc != EOF) 
	      && (gc != SCHEME_UNLESS_READY))
	    post_progress(ip);
	}

	/* Let other threads know that something happened,
	   and/or deregister this thread's request for information. */
	if (unless && ip->unless_cache) {
	  if (!SCHEME_CAR(unless)) {
	    /* Recycle "unless", since we were the only user */
	    ip->unless_cache = unless;
	    SCHEME_CDR(unless) = NULL;
	  } else {
	    if (SCHEME_TRUEP(SCHEME_CAR(unless))) {
	      /* gc should be SCHEME_UNLESS_READY; only a user
		 port without a peek can incorrectly produce something 
		 else */
	      if (gc == SCHEME_UNLESS_READY) {
		gc = 0;
	      }
	    } else if (gc) {
	      /* Notify other threads that something happened */
	      SCHEME_CAR(unless) = scheme_true;
	    }
	  }
	  ip->unless = NULL;
	}
      }

      if (gc == SCHEME_SPECIAL) {
	if (!got && !total_got && special_ok) {
	  if (!peek) {
	    if (ip->p.position >= 0)
	      ip->p.position++;
	    if (ip->p.count_lines)
	      inc_pos((Scheme_Port *)ip, 1);
	  }
	  
	  return SCHEME_SPECIAL;
	}

	if ((got || total_got) && only_avail) {
	  ip->ungotten_special = ip->special;
	  ip->special = NULL;
	  gc = 0;
	} else {
	  ip->special = NULL;
	  scheme_bad_time_for_special(who, port);
	  return 0;
	}
      } else if (gc == EOF) {
	ip->p.utf8state = 0;
	if (!got && !total_got) {
	  if (peek && ip->pending_eof)
	    ip->pending_eof = 2;
	  return EOF;
	}
	/* remember the EOF for next time */
	if (ip->pending_eof)
	  ip->pending_eof = 2;
	gc = 0;
	size = 0; /* so that we stop */
      } else if (gc == SCHEME_UNLESS_READY) {
	gc = 0;
	size = 0; /* so that we stop */
      }
      mzAssert(gc >= 0);
    } else
      gc = 0;

    got += gc;
    if (peek)
      peek_skip = quick_plus(peek_skip, gc);
    size -= gc;

    if (!peek) {
      /****************************************************/
      /* Adjust position information for chars got so far */
      /****************************************************/

      /* We don't get here if SCHEME_SPECIAL is returned, so
	 the positions are updated separately in the two
	 returning places above. */

      if (ip->p.position >= 0)
	ip->p.position += got;
      if (ip->p.count_lines)
	do_count_lines((Scheme_Port *)ip, buffer, offset, got);
    } else if (!ps) {
      /***************************************************/
      /* save newly peeked string for future peeks/reads */
      /***************************************************/
      if (gc) {
	if ((gc == 1) && !ip->ungotten_count && !ip->peeked_write) {
	  ip->ungotten[ip->ungotten_count++] = buffer[offset];
	} else {
	  if (!ip->peeked_write) {
	    Scheme_Object *rd, *wt;
	    scheme_pipe(&rd, &wt);
	    ip->peeked_read = rd;
	    ip->peeked_write = wt;
	  }

	  scheme_put_byte_string("peek", ip->peeked_write,
				 buffer, offset + got - gc, gc, 0);
	}
      }
    }

    offset += got;
    total_got += got;
    got = 0; /* for next round, if any */

    if (!size
	|| (total_got && ((only_avail == 1) || (only_avail == -1)))
	|| (only_avail == 2))
      break;

    /* Need to try to get more. */
  }
  
  return total_got;
}

long scheme_get_byte_string_special_ok_unless(const char *who,
					      Scheme_Object *port,
					      char *buffer, long offset, long size,
					      int only_avail,
					      int peek, Scheme_Object *peek_skip,
					      Scheme_Object *unless_evt)
{
  special_is_ok = 1;
  return scheme_get_byte_string_unless(who, port, buffer, offset, size, 
				       only_avail, peek, peek_skip, unless_evt);
}

long scheme_get_byte_string(const char *who,
			    Scheme_Object *port,
			    char *buffer, long offset, long size,
			    int only_avail,
			    int peek, Scheme_Object *peek_skip)
{
  return scheme_get_byte_string_unless(who, port,
				       buffer, offset, size,
				       only_avail,
				       peek, peek_skip,
				       NULL);
}

int scheme_unless_ready(Scheme_Object *unless)
{
  if (!unless)
    return 0;

  if (SCHEME_CAR(unless) && SCHEME_TRUEP(SCHEME_CAR(unless)))
    return 1;

  if (SCHEME_CDR(unless))
    return scheme_wait_sema(SCHEME_CDR(unless), 1);

  return 0;
}


void scheme_wait_input_allowed(Scheme_Input_Port *ip, int nonblock)
{
  while (ip->input_lock) {
    scheme_post_sema_all(ip->input_giveup);
    scheme_wait_sema(ip->input_lock, nonblock ? -1 : 0);
  }
}

static void release_input_lock(Scheme_Input_Port *ip)
{
  scheme_post_sema_all(ip->input_lock);
  ip->input_lock = NULL;
  ip->input_giveup = NULL;

  if (scheme_current_thread->running & MZTHREAD_NEED_SUSPEND_CLEANUP)
    scheme_current_thread->running -= MZTHREAD_NEED_SUSPEND_CLEANUP;
}

static void elect_new_main(Scheme_Input_Port *ip)
{
  if (ip->input_extras_ready) {
    scheme_post_sema_all(ip->input_extras_ready);
    ip->input_extras = NULL;
    ip->input_extras_ready = NULL;
  }
}

static void release_input_lock_and_elect_new_main(void *_ip)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)_ip;

  release_input_lock(ip);
  elect_new_main(ip);
}

static void check_suspended()
{
  if (scheme_current_thread->running & MZTHREAD_USER_SUSPENDED)
    scheme_thread_block(0.0);
}

static void remove_extra(void *ip_v)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)SCHEME_CAR(ip_v);
  Scheme_Object *v = SCHEME_CDR(ip_v), *ll, *prev;

  prev = NULL;
  for (ll = ip->input_extras; ll; prev = ll, ll = SCHEME_CDR(ll)) {
    if (SAME_OBJ(ll, SCHEME_CDR(v))) {
      if (prev)
	SCHEME_CDR(prev) = SCHEME_CDR(ll);
      else
	ip->input_extras = SCHEME_CDR(ll);
      SCHEME_CDR(ll) = NULL;
      break;
    }
  }

  /* Tell the main commit thread (if any) to reset */
  if (ip->input_giveup)
    scheme_post_sema_all(ip->input_giveup);
}

static int complete_peeked_read_via_get(Scheme_Input_Port *ip,
					long size)
{
  Scheme_Get_String_Fun gs;
  int did;
  
  did = 0;
  
  /* Target event is ready, so commit must succeed */
  
  /* First remove ungotten_count chars */
  if (ip->ungotten_count) {
    if (ip->ungotten_count > size)
      ip->ungotten_count -= size;
    else {
      size -= ip->ungotten_count;
      ip->ungotten_count = 0;
    }
    if (ip->progress_evt)
      post_progress(ip);
    did = 1;
  }
  
  if (size) {
    Scheme_Input_Port *pip;

    if (ip->peek_string_fun) {
      /* If the port supplies its own peek, then we don't
	 have peeked_r, so pass NULL as a buffer to the port's
	 read proc. The read proc must not block. */
      gs = ip->get_string_fun;
      pip = ip;
    } else {
      /* Otherwise, peek was implemented through peeked_{w,r}: */
      if (ip->peeked_read) {
	int cnt;
	cnt = pipe_char_count(ip->peeked_read);
	if ((cnt < size) && (ip->pending_eof == 2))
	  ip->pending_eof = 1;
	pip = (Scheme_Input_Port *)ip->peeked_read;
	gs = pip->get_string_fun;
      } else {
	gs = NULL;
	pip = NULL;
      }
    }
      
    if (gs) {
      size = gs(pip, NULL, 0, size, 1, NULL);
      if (size > 0) {
	if (ip->progress_evt)
	  post_progress(ip);
	did = 1;
      }
    }
  }
   
  return did;
}

static Scheme_Object *return_data(void *data, int argc, Scheme_Object **argv)
{
  return (Scheme_Object *)data;
}

int scheme_peeked_read_via_get(Scheme_Input_Port *ip,
			       long _size,
			       Scheme_Object *unless_evt,
			       Scheme_Object *_target_evt)
{
  Scheme_Object * volatile v, *sema, *a[3], ** volatile aa, * volatile l;
  volatile long size = _size;
  volatile int n, current_leader = 0;
  volatile Scheme_Type t;
  Scheme_Object * volatile target_evt = _target_evt;

  /* Check whether t's event value is known to be always itself: */
  t = SCHEME_TYPE(target_evt);
  if (!SAME_TYPE(t, scheme_sema_type)
      && !SAME_TYPE(t, scheme_channel_put_type)
      && !SAME_TYPE(t, scheme_always_evt_type)
      && !SAME_TYPE(t, scheme_never_evt_type)
      && !SAME_TYPE(t, scheme_semaphore_repost_type)) {
    /* Make an event whose value is itself */
    a[0] = target_evt;
    v = scheme_make_closed_prim(return_data, target_evt);
    a[1] = v;
    target_evt = scheme_wrap_evt(2, a);
    ((Scheme_Closed_Primitive_Proc *)v)->data = target_evt;
  }

  /* This commit implementation is essentially CML style, but we avoid
     actually allocating a manager thread. Instead the various
     committing threads elect a leader, and we rely on being in the
     kernel to detect when the leader is killed or suspended, in which
     case we elect a new leader. */

  while (1) {
    if (scheme_wait_sema(unless_evt, 1)) {
      if (current_leader)
	elect_new_main(ip);
      return 0;
    }

    if (!current_leader && ip->input_giveup) {
      /* Some other thread is already trying to commit.
	 Ask it to sync on our target, too */
      v = scheme_make_pair(scheme_make_integer(_size), target_evt);
      l = scheme_make_raw_pair(v, ip->input_extras);
      ip->input_extras = l;

      scheme_post_sema_all(ip->input_giveup);

      if (!ip->input_extras_ready) {
	sema = scheme_make_sema(0);
	ip->input_extras_ready = sema;
      }

      a[0] = ip->input_extras_ready;
      l = scheme_make_pair((Scheme_Object *)ip, v);
      BEGIN_ESCAPEABLE(remove_extra, l);
      scheme_sync(1, a);
      END_ESCAPEABLE();

      if (!SCHEME_CDR(v)) {
	/* We were selected, so the commit succeeded. */
	return SCHEME_TRUEP(SCHEME_CAR(v)) ? 1 : 0;
      }
    } else {
      /* No other thread is trying to commit. This one is hereby
	 elected "main" if multiple threads try to commit. */

      if (SAME_TYPE(t, scheme_always_evt_type)) {
	/* Fast path: always-evt is ready */
	return complete_peeked_read_via_get(ip, size);
      }

      /* This sema makes other threads wait before reading: */
      sema = scheme_make_sema(0);
      ip->input_lock = sema;
      
      /* This sema lets other threads try to make progress,
	 if the current target doesn't work out */
      sema = scheme_make_sema(0);
      ip->input_giveup = sema;
      
      if (ip->input_extras) {
	/* There are other threads trying to commit, and
	   as main thread, we'll help them out. */
	n = 3;
	for (l = ip->input_extras; l; l = SCHEME_CDR(l)) {
	  n++;
	}
	aa = MALLOC_N(Scheme_Object *, n);
	n = 3;
	for (l = ip->input_extras; l; l = SCHEME_CDR(l)) {
	  aa[n++] = SCHEME_CDR(SCHEME_CAR(l));
	}
      } else {
	/* This is the only thread trying to commit */
	n = 3;
	aa = a;
      }

      /* Suspend here is a problem if another thread
	 tries to commit, because this thread will be
	 responsible for multiplexing the commits. That's
	 why the thread waits on its own suspend event. */
      
      aa[0] = target_evt;
      aa[1] = ip->input_giveup;
      v = scheme_get_thread_suspend(scheme_current_thread);
      aa[2] = v;

      scheme_current_thread->running |= MZTHREAD_NEED_SUSPEND_CLEANUP;
      BEGIN_ESCAPEABLE(release_input_lock_and_elect_new_main, ip);
      v = scheme_sync(n, aa);
      END_ESCAPEABLE();

      release_input_lock(ip);
      
      if (SAME_OBJ(v, target_evt)) {
	int r;
	elect_new_main(ip);
	r = complete_peeked_read_via_get(ip, size);
	check_suspended();
	return r;
      }

      if (n > 3) {
	/* Check whether one of the others was selected: */
	for (l = ip->input_extras; l; l = SCHEME_CDR(l)) {
	  if (SAME_OBJ(v, SCHEME_CDR(SCHEME_CAR(l)))) {
	    /* Yep. Clear the cdr to tell the relevant thread
	       that it was selected, and reset the extras. */
	    v = SCHEME_CAR(l);
	    SCHEME_CDR(v) = NULL;
	    size = SCHEME_INT_VAL(SCHEME_CAR(v));
	    elect_new_main(ip);
	    if (complete_peeked_read_via_get(ip, size))
	      SCHEME_CAR(v) = scheme_true;
	    else
	      SCHEME_CAR(v) = scheme_false;
	    check_suspended();
	    return 0;
	  }
	}
      }

      if (scheme_current_thread->running & MZTHREAD_USER_SUSPENDED) {
	elect_new_main(ip);
	current_leader = 0;
	check_suspended();
      } else {
	current_leader = 1;
	
	/* Technically redundant, but avoid a thread swap
	   if we know the commit isn't going to work: */
	if (scheme_wait_sema(unless_evt, 1)) {
	  elect_new_main(ip);
	  return 0;
	}
      
	scheme_thread_block(0.0);
      }
    }
  }
}

int scheme_peeked_read(Scheme_Object *port,
		       long size,
		       Scheme_Object *unless_evt,
		       Scheme_Object *target_evt)
{
  Scheme_Input_Port *ip;
  Scheme_Peeked_Read_Fun pr;
  
  ip = (Scheme_Input_Port *)port;

  unless_evt = SCHEME_PTR2_VAL(unless_evt);

  pr = ip->peeked_read_fun;

  return pr(ip, size, unless_evt, target_evt);
}

Scheme_Object *scheme_progress_evt_via_get(Scheme_Input_Port *port)
{
  Scheme_Object *sema;

  if (port->progress_evt)
    return port->progress_evt;

  sema = scheme_make_sema(0);

  port->progress_evt = sema;

  return sema;
}

Scheme_Object *scheme_progress_evt(Scheme_Object *port)
{  
  Scheme_Input_Port *ip;
  
  ip = (Scheme_Input_Port *)port;
  
  if (ip->progress_evt_fun) {
    Scheme_Progress_Evt_Fun ce;
    Scheme_Object *evt, *o;

    ce = ip->progress_evt_fun;

    evt = ce(ip);

    o = scheme_alloc_object();
    o->type = scheme_progress_evt_type;
    SCHEME_PTR1_VAL(o) = (Scheme_Object *)port;
    SCHEME_PTR2_VAL(o) = evt;

    return o;
  }

  return NULL;
}

static int progress_evt_ready(Scheme_Object *evt, Scheme_Schedule_Info *sinfo)
{
  scheme_set_sync_target(sinfo, SCHEME_PTR2_VAL(evt), evt, NULL, 0, 1);
  return 0;
}

long scheme_get_char_string(const char *who,
			    Scheme_Object *port,
			    mzchar *buffer, long offset, long size,
			    int peek, Scheme_Object *peek_skip)
{
  int ahead_skip = 0;
  char *s;
  int total_got = 0, bsize, leftover = 0, got;

  /* read_string_byte_buffer helps avoid allocation */
  if (read_string_byte_buffer) {
    s = read_string_byte_buffer;
    read_string_byte_buffer = NULL;
  } else
    s = (char *)scheme_malloc_atomic(READ_STRING_BYTE_BUFFER_SIZE);

  while (1) {
    /* Since we want "size" more chars and we don't have leftovers, we
       need at least "size" more bytes.

       "leftover" is the number of bytes (<< READ_STRING_BYTE_BUFFER_SIZE) that
       we already have toward the first character. If the next
       character doesn't continue a leftover sequence, the next
       character actually belongs to a (leftover+1)th character. Thus,
       if leftover is positive and we're not merely peeking, ask for
       at leat one byte, but otherwise no more than size - leftover
       bytes. If size is 1, then we are forced to peek in all cases.

       Overall, if the size is big enough, we only read as many
       characters as our buffer holds. */

    bsize = size;
    if (leftover) {
      bsize -= leftover;
      if (bsize < 1) {
	/* This is the complex case. Need to peek a byte to see
	   whether it continues the leftover sequence or ends it an in
	   an error. */
	if (!peek_skip)
	  peek_skip = scheme_make_integer(0);
	special_is_ok = 1;
	got = scheme_get_byte_string_unless(who, port,
					    s, leftover, 1,
					    0, 1 /* => peek */, 
					    quick_plus(peek_skip, ahead_skip),
					    NULL);
	if (got > 0) {
	  long ulen, glen;
	  glen = scheme_utf8_decode_as_prefix((const unsigned char *)s, 0, got + leftover,
					      buffer, offset, offset + size,
					      &ulen, 0, '?');
	  if (glen && (ulen < got + leftover)) {
	    /* Got one, with a decoding error. If we weren't peeking,
	       don't read the lookahead bytes after all, yet. */
	    total_got++;
	    bsize = 0;
	    ahead_skip++;
	    size--;
	    offset++;
	    /* leftover stays the same */
	    memmove(s, s + 1, leftover);
	  } else {
	    /* Either we got one character, or we're still continuing. */
	    ahead_skip++;
	    if (!glen) {
	      /* Continuing */
	      leftover++;
	    } else {
	      /* Got one (no encoding error) */
	      leftover = 0;
	      offset++;
	      --size;
	      total_got++;
	      if (!peek) {
		/* Read the lookahead bytes and discard them */
		scheme_get_byte_string_unless(who, port,
					      s, 0, ahead_skip,
					      0, 0, scheme_make_integer(0),
					      NULL);
	      } else {
		peek_skip = quick_plus(peek_skip, ahead_skip);
	      }
	      ahead_skip = 0;
	    }
	    /* Continue with the normal decoing process (but get 0
	       more characters this time around) */
	    bsize = 0;
	  }
	} else {
	  /* Either EOF or SPECIAL -- either one ends the leftover
	     sequence in an error. We may have more leftover chars
	     than we need, but they haven't been read, yet. */
	  while (leftover && size) {
	    buffer[offset++] = '?';
	    total_got++;
	    --leftover;
	    --size;
	  }
	  return total_got;
	}
      }
    }

    if (bsize) {
      /* Read bsize bytes */
      if (bsize + leftover > READ_STRING_BYTE_BUFFER_SIZE)
	bsize = READ_STRING_BYTE_BUFFER_SIZE - leftover;
      
      got = scheme_get_byte_string_unless(who, port,
					  s, leftover, bsize,
					  0, peek, peek_skip,
					  NULL);
    } else
      got = 0;

    if (got >= 0) {
      long ulen, glen;

      glen = scheme_utf8_decode_as_prefix((const unsigned char *)s, 0, got + leftover,
					  buffer, offset, offset + size,
					  &ulen, 0, '?');
      
      total_got += glen;
      if (glen == size) {
	/* Got enough */
	read_string_byte_buffer = s;
	return total_got;
      }
      offset += glen;
      size -= glen;
      leftover = (got + leftover) - ulen;
      memmove(s, s + ulen, leftover);
      if (peek) {
	peek_skip = quick_plus(peek_skip, got);
      }
    } else {
      read_string_byte_buffer = s;

      /* Leftover bytes must be decoding-error bytes: */
      while (leftover) {
	buffer[offset++] = '?';
	total_got++;
	--leftover;
      }

      if (!total_got)
	return got; /* must be EOF */
      else
	return total_got;
    }
  }
}

static 
#ifndef NO_INLINE_KEYWORD
MSC_IZE(inline)
#endif
long get_one_byte(const char *who,
		  Scheme_Object *port,
		  char *buffer, long offset,
		  int only_avail)
{
  Scheme_Input_Port *ip;
  long gc;
  int special_ok = special_is_ok;
  Scheme_Get_String_Fun gs;

  special_is_ok = 0;

  ip = (Scheme_Input_Port *)port;

  CHECK_PORT_CLOSED(who, "input", port, ip->closed);

  if (ip->input_lock)
    scheme_wait_input_allowed(ip, only_avail);

  if (ip->ungotten_count) {
    buffer[offset] = ip->ungotten[--ip->ungotten_count];
    gc = 1;
  } else if (ip->peeked_read && pipe_char_count(ip->peeked_read)) {
    int ch;
    ch = scheme_get_byte(ip->peeked_read);
    buffer[offset] = ch;
    gc = 1;
  } else if (ip->ungotten_special) {
    if (ip->progress_evt)
      post_progress(ip);
    if (!special_ok) {
      ip->ungotten_special = NULL;
      scheme_bad_time_for_special(who, port);
      return 0;
    }
    ip->special = ip->ungotten_special;
    ip->ungotten_special = NULL;
    if (ip->p.position >= 0)
      ip->p.position++;
    if (ip->p.count_lines)
      inc_pos((Scheme_Port *)ip, 1);
    return SCHEME_SPECIAL;
  } else {
    if (ip->pending_eof > 1) {
      ip->pending_eof = 1;
      return EOF;
    } else {
      /* Call port's get function. */
      gs = ip->get_string_fun;

      gc = gs(ip, buffer, offset, 1, 0, NULL);
	
      if (ip->progress_evt && (gc > 0))
	post_progress(ip);

      if (gc < 1) {
	if (gc == SCHEME_SPECIAL) {
	  if (special_ok) {
	    if (ip->p.position >= 0)
	      ip->p.position++;
	    if (ip->p.count_lines)
	      inc_pos((Scheme_Port *)ip, 1);
	    return SCHEME_SPECIAL;
	  } else {
	    scheme_bad_time_for_special(who, port);
	    return 0;
	  }
	} else if (gc == EOF) {
	  ip->p.utf8state = 0;
	  return EOF;
	} else {
	  /* didn't get anything the first try, so use slow path: */
	  special_is_ok = special_ok;
	  return scheme_get_byte_string_unless(who, port,
					       buffer, offset, 1,
					       0, 0, NULL, NULL);
	}
      }
    }
  }

  /****************************************************/
  /* Adjust position information for chars got so far */
  /****************************************************/
  
  if (ip->p.position >= 0)
    ip->p.position++;
  if (ip->p.count_lines)
    do_count_lines((Scheme_Port *)ip, buffer, offset, 1);
  
  return gc;
}

int
scheme_getc(Scheme_Object *port)
{
  char s[MAX_UTF8_CHAR_BYTES];
  unsigned int r[1];
  int v, delta = 0;

  while(1) {
    if (delta) {
      v = scheme_get_byte_string_unless("read-char", port,
					s, delta, 1,
					0,
					delta > 0, scheme_make_integer(delta-1),
					NULL);
    } else {
      v = get_one_byte("read-char", port,
		       s, 0, 
		       0);
    }

    if ((v == EOF) || (v == SCHEME_SPECIAL)) {
      if (!delta)
	return v;
      else {
	/* This counts as a decoding error. The high bit
	   on the first character must be set. */
	return '?';
      }
    } else {
      v = scheme_utf8_decode_prefix((const unsigned char *)s, delta + 1, r, 0);
      if (v > 0) {
	if (delta) {
	  /* Need to read the peeked bytes (will ignore) */
	  v = scheme_get_byte_string_unless("read-char", port,
					    s, 0, delta,
					    0,
					    0, 0,
					    NULL);
	}
	return r[0];
      } else if (v == -2) {
	/* -2 => decoding error */
	return '?';
      } else if (v == -1) {
	/* In middle of sequence; start/continue peeking bytes */
	delta++;
      }
    }
  }
}

int
scheme_get_byte(Scheme_Object *port)
{
  char s[1];
  int v;

  v = get_one_byte("read-byte", port,
		   s, 0,
		   0);

  if ((v == EOF) || (v == SCHEME_SPECIAL))
    return v;
  else
    return ((unsigned char *)s)[0];
}

int
scheme_getc_special_ok(Scheme_Object *port)
{
  special_is_ok = 1;
  return scheme_getc(port);
}

int
scheme_get_byte_special_ok(Scheme_Object *port)
{
  special_is_ok = 1;
  return scheme_get_byte(port);
}

long scheme_get_bytes(Scheme_Object *port, long size, char *buffer, int offset)
{
  int n;
  int only_avail = 0;

  if (size < 0) {
    size = -size;
    only_avail = 1;
  }

  n = scheme_get_byte_string_unless("read-bytes", port,
				    buffer, offset, size,
				    only_avail,
				    0, 0,
				    NULL);

  if (n == EOF)
    n = 0;

  mzAssert(n >= 0);

  return n;
}

int scheme_peek_byte_skip(Scheme_Object *port, Scheme_Object *skip, Scheme_Object *unless_evt)
{
  char s[1];
  int v;

  v = scheme_get_byte_string_unless("peek-byte", port,
				    s, 0, 1,
				    0,
				    1, skip,
				    unless_evt);

  if ((v == EOF) || (v == SCHEME_SPECIAL))
    return v;
  else
    return ((unsigned char *)s)[0];
}

int scheme_peek_byte(Scheme_Object *port)
{
  return scheme_peek_byte_skip(port, NULL, NULL);
}

int
scheme_peek_byte_special_ok_skip(Scheme_Object *port, Scheme_Object *skip, Scheme_Object *unless_evt)
{
  special_is_ok = 1;
  return scheme_peek_byte_skip(port, skip, unless_evt);
}

static int do_peekc_skip(Scheme_Object *port, Scheme_Object *skip, 
			 int only_avail, int *unavail)
{
  char s[MAX_UTF8_CHAR_BYTES];
  unsigned int r[1];
  int v, delta = 0;
  Scheme_Object *skip2;

  if (unavail)
    *unavail = 0;

  while(1) {
    if (delta) {
      if (!skip)
	skip = scheme_make_integer(0);
      skip2 = quick_plus(skip, delta);
    } else
      skip2 = skip;

    v = scheme_get_byte_string_unless("peek-char", port,
				      s, delta, 1,
				      only_avail,
				      1, skip2,
				      NULL);

    if (!v) {
      *unavail = 1;
      return 0;
    }

    if ((v == EOF) || (v == SCHEME_SPECIAL)) {
      if (!delta)
	return v;
      else {
	/* This counts as a decoding error, so return '?' */
	return '?';
      }
    } else {
      v = scheme_utf8_decode_prefix((const unsigned char *)s, delta + 1, r, 0);
      if (v > 0)
	return r[0];
      else if (v == -2) {
	/* -2 => decoding error */
	return '?';
      } else if (v == -1) {
	/* In middle of sequence - keep getting bytes. */
	delta++;
      }
    }
  }
}

int scheme_peekc_skip(Scheme_Object *port, Scheme_Object *skip)
{
  return do_peekc_skip(port, skip, 0, NULL);
}

int scheme_peekc(Scheme_Object *port)
{
  return scheme_peekc_skip(port, scheme_make_integer(0));
}

int
scheme_peekc_special_ok_skip(Scheme_Object *port, Scheme_Object *skip)
{
  special_is_ok = 1;
  return scheme_peekc_skip(port, skip);
}

int
scheme_peekc_special_ok(Scheme_Object *port)
{
  return scheme_peekc_special_ok_skip(port, scheme_make_integer(0));
}

int scheme_peekc_is_ungetc(Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;

  return !ip->peek_string_fun;
}

Scheme_Object *make_read_write_evt(Scheme_Type type, 
				   Scheme_Object *port, Scheme_Object *skip, 
				   char *str, long start, long size)
{
  Scheme_Read_Write_Evt *rww;

  rww = MALLOC_ONE_TAGGED(Scheme_Read_Write_Evt);
  rww->so.type = type;
  rww->port = port;
  rww->v = skip;
  rww->str = str;
  rww->start = start;
  rww->size = size;

  return (Scheme_Object *)rww;
}

static int rw_evt_ready(Scheme_Object *_rww, Scheme_Schedule_Info *sinfo)
{
  Scheme_Read_Write_Evt *rww = (Scheme_Read_Write_Evt *)_rww;
  long v;

  if (sinfo->false_positive_ok) {
    /* Causes the thread to swap in, which we need in case there's an
       exception: */
    sinfo->potentially_false_positive = 1;
    return 1;
  }
  
  if (rww->v) {
    Scheme_Write_Special_Fun ws = ((Scheme_Output_Port *)rww->port)->write_special_fun;
    v = ws((Scheme_Output_Port *)rww->port, rww->v, 1);
    if (v) {
      scheme_set_sync_target(sinfo, scheme_true, NULL, NULL, 0, 0);
      return 1;
    } else	
      return 0;
  } else {
    v = scheme_put_byte_string("write-evt", rww->port,
			       rww->str, rww->start, rww->size,
			       2);
    if (v < 1)
      return 0;
    else if (!v && rww->size)
      return 0;
    else {
      scheme_set_sync_target(sinfo, scheme_make_integer(v), NULL, NULL, 0, 0);
      return 1;
    }
  }
}

static void rw_evt_wakeup(Scheme_Object *_rww, void *fds)
{
  Scheme_Read_Write_Evt *rww = (Scheme_Read_Write_Evt *)_rww;

  if (rww->port) {
    if (rww->so.type == scheme_write_evt_type)
      output_need_wakeup(rww->port, fds);
    else
      scheme_need_wakeup(rww->port, fds);
  }
}

Scheme_Object *scheme_write_evt_via_write(Scheme_Output_Port *port,
					  const char *str, long offset, long size)
{
  return make_read_write_evt(scheme_write_evt_type, (Scheme_Object *)port, NULL, 
			     (char *)str, offset, size);
}

Scheme_Object *scheme_write_special_evt_via_write_special(Scheme_Output_Port *port, 
							  Scheme_Object *special)
{
  return make_read_write_evt(scheme_write_evt_type, (Scheme_Object *)port, special, 
			     NULL, 0, 1);
}
	
Scheme_Object *scheme_make_write_evt(const char *who, Scheme_Object *port,
				     Scheme_Object *special, char *str, long start, long size)
{
  Scheme_Output_Port *op = (Scheme_Output_Port *)port;

  if (!special) {
    if (op->write_string_evt_fun) {
      Scheme_Write_String_Evt_Fun wse;
      wse = op->write_string_evt_fun;
      return wse(op, str, start, size);
    }
  } else {
    if (op->write_special_evt_fun) {
      Scheme_Write_Special_Evt_Fun wse = op->write_special_evt_fun;
      return wse(op, special);
    }
  }

  scheme_arg_mismatch("write-bytes-avail-evt",
		      "port does not support atomic writes: ",
		      port);
  return NULL;
}

void
scheme_ungetc (int ch, Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;

  CHECK_PORT_CLOSED("#<primitive:peek-port-char>", "input", port, ip->closed);

  if (ch == EOF) {
    if (ip->pending_eof) /* non-zero means that EOFs are tracked */
      ip->pending_eof = 2;
    return;
  } else if (ch == SCHEME_SPECIAL) {
    ip->ungotten_special = ip->special;
    ip->special = NULL;
  } else if (ch > 127) {
    unsigned char e[MAX_UTF8_CHAR_BYTES];
    unsigned int us[1];
    int len;

    us[0] = ch;
    len = scheme_utf8_encode_all(us, 1, e);

    if (ip->ungotten_count + len >= 24)
      scheme_signal_error("ungetc overflow");
    while (len) {
      ip->ungotten[ip->ungotten_count++] = e[--len];
    }
  } else {
    if (ip->ungotten_count == 24)
      scheme_signal_error("ungetc overflow");
    ip->ungotten[ip->ungotten_count++] = ch;
  }

  if (ip->p.position > 0)
    --ip->p.position;
  if (ip->p.count_lines) {
    --ip->p.column;
    --ip->p.readpos;
    if (!(--ip->p.charsSinceNewline)) {
      mzAssert(ip->p.lineNumber > 0);
      --ip->p.lineNumber;
      ip->p.column = ip->p.oldColumn;
    } else if (ch == '\t')
      ip->p.column = ip->p.oldColumn;
  }
}

int
scheme_byte_ready (Scheme_Object *port)
{
  Scheme_Input_Port *ip;
  int retval;

  ip = (Scheme_Input_Port *)port;

  CHECK_PORT_CLOSED("char-ready?", "input", port, ip->closed);

  if (ip->ungotten_count || ip->ungotten_special
      || (ip->pending_eof > 1)
      || pipe_char_count(ip->peeked_read))
    retval = 1;
  else {
    Scheme_In_Ready_Fun f = ip->byte_ready_fun;
    retval = f(ip);
  }

  return retval;
}

int
scheme_char_ready (Scheme_Object *port)
{
  int unavail;

  if (!scheme_byte_ready(port))
    return 0;

  do_peekc_skip(port, scheme_make_integer(0), 0, &unavail);
  
  return !unavail;
}

Scheme_Object *scheme_get_special(Scheme_Object *port,
				  Scheme_Object *src, long line, long col, long pos,
				  int peek, Scheme_Hash_Table **for_read)
{
  int cnt;
  Scheme_Object *a[4], *special;
  Scheme_Input_Port *ip;
  Scheme_Cont_Frame_Data cframe;

  SCHEME_USE_FUEL(1);

  ip = (Scheme_Input_Port *)port;

  /* Only `read' and similar internals should call this function. A
     caller must should ensure that there are no ungotten
     characters. */

  if (ip->ungotten_count) {
    scheme_signal_error("ungotten characters at get-special");
    return NULL;
  }
  if (!ip->special) {
    scheme_signal_error("no ready special");
    return NULL;
  }

  CHECK_PORT_CLOSED("#<primitive:get-special>", "input", port, ip->closed);

  special = ip->special;
  ip->special = NULL;

  if (peek) {
    /* do location increment, since read didn't */
    if (line > 0)
      line++;
    if (col >= 0)
      col++;
    if (pos > 0)
      pos++;
  }

  a[0] = special;
  if (!src && scheme_check_proc_arity(NULL, 2, 0, 1, a))
    cnt = 0;
  else {
    cnt = 4;
    a[0] = (src ? src : scheme_false);
    a[1] = (line > 0) ? scheme_make_integer(line) : scheme_false;
    a[2] = (col > 0) ? scheme_make_integer(col-1) : scheme_false;
    a[3] = (pos > 0) ? scheme_make_integer(pos) : scheme_false;
  }

  scheme_push_continuation_frame(&cframe);
  scheme_set_in_read_mark(src, for_read);

  special = scheme_apply(special, cnt, a);

  scheme_pop_continuation_frame(&cframe);

  return special;
}

static Scheme_Object *do_get_ready_special(Scheme_Object *port, 
					   Scheme_Object *stxsrc,
					   int peek,
					   Scheme_Hash_Table **ht)
{
  long line, col, pos;

  if (!stxsrc) {
    stxsrc = ((Scheme_Input_Port *)port)->name;
  }

  /* Don't use scheme_tell_all(), because we always want the
     MzScheme-computed values here. */
  line = scheme_tell_line(port);
  col = scheme_tell_column(port);
  pos = scheme_tell(port);

  return scheme_get_special(port, stxsrc, line, col, pos, peek, ht);
}

Scheme_Object *scheme_get_ready_read_special(Scheme_Object *port, Scheme_Object *stxsrc, Scheme_Hash_Table **ht)
{
  return do_get_ready_special(port, stxsrc, 0, ht);
}

Scheme_Object *scheme_get_ready_special(Scheme_Object *port, 
					Scheme_Object *stxsrc,
					int peek)
{
  return do_get_ready_special(port, stxsrc, peek, NULL);
}

void scheme_bad_time_for_special(const char *who, Scheme_Object *port)
{
  scheme_arg_mismatch(who, "non-character in an unsupported context, from port: ", port);
}

static Scheme_Object *check_special_args(void *sbox, int argc, Scheme_Object **argv)
{
  Scheme_Object *special;
  Scheme_Cont_Frame_Data cframe;

  if (SCHEME_TRUEP(argv[1]))
    if (!scheme_nonneg_exact_p(argv[1]) || (SAME_OBJ(argv[1], scheme_make_integer(0))))
      scheme_wrong_type("read-special", "positive exact integer or #f", 1, argc, argv);
  if (SCHEME_TRUEP(argv[2]))
    if (!scheme_nonneg_exact_p(argv[2]))
      scheme_wrong_type("read-special", "non-negative exact integer or #f", 2, argc, argv);
  if (SCHEME_TRUEP(argv[3]))
    if (!scheme_nonneg_exact_p(argv[3]) || (SAME_OBJ(argv[3], scheme_make_integer(0))))
      scheme_wrong_type("read-special", "positive exact integer or #f", 3, argc, argv);

  special = *(Scheme_Object **)sbox;
  if (!special)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "read-special: cannot be called a second time");
  *(Scheme_Object **)sbox = NULL;

  scheme_push_continuation_frame(&cframe);
  scheme_set_in_read_mark(NULL, NULL);

  special = _scheme_apply(special, 4, argv);

  scheme_pop_continuation_frame(&cframe);

  return special;
}

Scheme_Object *scheme_get_special_proc(Scheme_Object *inport)
{
  Scheme_Object *special, **sbox;
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)inport;
  special = ip->special;
  ip->special = NULL;
  
  sbox = MALLOC_ONE(Scheme_Object *);
  *sbox = special;
  return scheme_make_closed_prim_w_arity(check_special_args, 
					 sbox, "read-special",
					 4, 4);
}

void
scheme_need_wakeup (Scheme_Object *port, void *fds)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;

  if (ip->need_wakeup_fun) {
    Scheme_Need_Wakeup_Input_Fun f = ip->need_wakeup_fun;
    f(ip, fds);
  }
}

#define CHECK_IOPORT_CLOSED(who, port) \
        if (SCHEME_INPORTP(port)) { \
          CHECK_PORT_CLOSED(who, "input", port, ((Scheme_Input_Port *)port)->closed); \
        } else { \
          CHECK_PORT_CLOSED(who, "output", port, ((Scheme_Output_Port *)port)->closed); \
        }

long
scheme_tell (Scheme_Object *port)
{
  Scheme_Port *ip;
  long pos;

  ip = (Scheme_Port *)port;
  
  CHECK_IOPORT_CLOSED("get-file-position", port);

  if (!ip->count_lines || (ip->position < 0))
    pos = ip->position;
  else
    pos = ip->readpos;

  return pos;
}

long
scheme_tell_line (Scheme_Object *port)
{
  Scheme_Port *ip;
  long line;

  ip = (Scheme_Port *)port;

  if (!ip->count_lines || (ip->position < 0))
    return -1;

  CHECK_IOPORT_CLOSED("get-file-line", port);

  line = ip->lineNumber;

  return line;
}

long
scheme_tell_column (Scheme_Object *port)
{
  Scheme_Port *ip;
  long col;

  ip = (Scheme_Port *)port;

  if (!ip->count_lines || (ip->position < 0))
    return -1;
  
  CHECK_IOPORT_CLOSED("get-file-column", port);

  col = ip->column;

  return col;
}

void
scheme_tell_all (Scheme_Object *port, long *_line, long *_col, long *_pos)
{
  Scheme_Port *ip = (Scheme_Port *)port;
  long line = -1, col = -1, pos = -1;
  
  if (ip->count_lines && ip->location_fun) {
    Scheme_Location_Fun location_fun;
    Scheme_Object *r, *a[3];
    long v;
    int got, i;
    
    location_fun = ip->location_fun;
    r = location_fun(ip);

    got = (SAME_OBJ(r, SCHEME_MULTIPLE_VALUES) ? scheme_multiple_count : 1);
    if (got != 3) {
      scheme_wrong_return_arity("user port next-location",
				3, got, 
				(got == 1) ? (Scheme_Object **)r : scheme_multiple_array,
				"calling port-next-location procedure");
      return;
    }

    a[0] = scheme_multiple_array[0];
    a[1] = scheme_multiple_array[1];
    a[2] = scheme_multiple_array[2];

    for (i = 0; i < 3; i++) {
      v = -1;
      if (SCHEME_TRUEP(a[i])) {
	if (scheme_nonneg_exact_p(a[i])) {
	  if (SCHEME_INTP(a[i])) {
	    v = SCHEME_INT_VAL(a[i]);
	    if ((i != 1) && !v) {
	      a[0] = a[i];
	      scheme_wrong_type("user port next-location", 
				((i == 1) ? "non-negative exact integer or #f" : "positive exact integer or #f"),
				-1, -1, a);
	      return;
	    }
	  }
	}
      }
      switch(i) {
      case 0:
	line = v;
	break;
      case 1:
	col = v;
	break;
      case 2:
	pos = v;
	break;
      }
    }

    /* Internally, positions count from 0 instead of 1 */
    if (pos > -1)
      pos--;
  } else {
    line = scheme_tell_line(port);
    col = scheme_tell_column(port);
    pos = scheme_tell(port);
  }

  if (_line) *_line = line;
  if (_col) *_col = col;
  if (_pos) *_pos = pos;  
}

void
scheme_count_lines (Scheme_Object *port)
{
  Scheme_Port *ip = (Scheme_Port *)port;

  if (!ip->count_lines) {
    ip->count_lines = 1;
    if (ip->count_lines_fun) {
      Scheme_Count_Lines_Fun cl = ip->count_lines_fun;
      cl(ip);
    }
  }
}

void
scheme_close_input_port (Scheme_Object *port)
{
  Scheme_Input_Port *ip;

  ip = (Scheme_Input_Port *)port;

  if (!ip->closed) {
    if (ip->close_fun) {
      Scheme_Close_Input_Fun f = ip->close_fun;
      f(ip);
    }

    if (ip->progress_evt) {
      scheme_post_sema_all(ip->progress_evt);
      ip->progress_evt = NULL;
    }

    if (ip->mref) {
      scheme_remove_managed(ip->mref, (Scheme_Object *)ip);
      ip->mref = NULL;
    }

    ip->closed = 1;
    ip->ungotten_count = 0;
    ip->ungotten_special = NULL;
  }
}

static void
force_close_input_port(Scheme_Object *port)
{
  scheme_force_port_closed = 1;
  scheme_close_input_port(port);
  scheme_force_port_closed = 0;
}

int scheme_close_should_force_port_closed()
{
  return scheme_force_port_closed;
}

/****************************** main output writer ******************************/

long
scheme_put_byte_string(const char *who, Scheme_Object *port,
		       const char *str, long d, long len,
		       int rarely_block)
{
  /* Unlike the main reader, the main writer is simple. It doesn't
     have to deal with peeks and specials, so it's a thin wrapper on
     the port's function. */

  Scheme_Output_Port *op = (Scheme_Output_Port *)port;
  Scheme_Write_String_Fun ws;
  long out, llen, oout;
  int enable_break;

  CHECK_PORT_CLOSED(who, "output", port, op->closed);

  ws = op->write_string_fun;

  if (rarely_block == -1) {
    enable_break = 1;
    rarely_block = 1;
  } else
    enable_break = 0;

  if (enable_break) {
    if (scheme_current_thread->external_break) {
      scheme_thread_block_enable_break(0.0, 1);
      scheme_current_thread->ran_some = 1;
    }
  }

  if ((rarely_block == 1) && !len)
    /* By definition, a partial-progress write on a 0-length string is
       the same as a blocking flush */
    rarely_block = 0;

  llen = len;
  oout = 0;
  while (llen || !len) {
    out = ws(op, str, d, llen, rarely_block, enable_break);
    
    /* If out is 0, it might be because the port got closed: */
    if (!out) {
      CHECK_PORT_CLOSED(who, "output", port, op->closed);
    }
    
    if (out > 0) {
      op->p.position += out;
      oout += out;
      if (op->p.count_lines)
	do_count_lines((Scheme_Port *)op, str, d, out);
    }

    if (rarely_block || !len)
      break;

    llen -= out;
    d += out;
  }

  mzAssert(!rarely_block ? (oout == len) : 1);
  mzAssert((oout < 0) ? (rarely_block == 2) : 1);

  return oout;
}

void scheme_write_byte_string(const char *str, long len, Scheme_Object *port)
{
  (void)scheme_put_byte_string("write-string", port, str, 0, len, 0);
}

void scheme_write_char_string(const mzchar *str, long len, Scheme_Object *port)
{
  long blen;
  char *bstr, buf[64];

  bstr = scheme_utf8_encode_to_buffer_len(str, len, buf, 64, &blen);
  
  scheme_write_byte_string(bstr, blen, port);
}

long
scheme_put_char_string(const char *who, Scheme_Object *port,
		       const mzchar *str, long d, long len)
{
  long blen;
  char *bstr, buf[64];

  blen = scheme_utf8_encode(str, d, d + len, NULL, 0, 0);
  if (blen < 64)
    bstr = buf;
  else
    bstr = (char *)scheme_malloc_atomic(blen);
  scheme_utf8_encode(str, d, d + len, (unsigned char *)bstr, 0, 0);

  return scheme_put_byte_string(who, port, bstr, 0, blen, 0);
}

long
scheme_output_tell(Scheme_Object *port)
{
  return scheme_tell(port);
}

void
scheme_close_output_port(Scheme_Object *port)
{
  Scheme_Output_Port *op;

  op = (Scheme_Output_Port *)port;

  if (!op->closed) {
    /* call close function first; it might raise an exception */
    if (op->close_fun) {
      Scheme_Close_Output_Fun f = op->close_fun;
      f(op);
    }

    /* NOTE: Allow the possibility that some other thread finishes the
       close while f blocks. */

    if (op->mref) {
      scheme_remove_managed(op->mref, (Scheme_Object *)op);
      op->mref = NULL;
    }
    
    op->closed = 1;
  }
}

static void
force_close_output_port(Scheme_Object *port)
{
  scheme_force_port_closed = 1;
  scheme_close_output_port(port);
  scheme_force_port_closed = 0;
}

/*========================================================================*/
/*                           File port utils                              */
/*========================================================================*/

void scheme_flush_orig_outputs(void)
{
  /* Flush original output ports: */
  scheme_flush_output(scheme_orig_stdout_port);
  scheme_flush_output(scheme_orig_stderr_port);
}

void scheme_flush_output(Scheme_Object *o)
{
  scheme_put_byte_string("flush-output", o,
			 NULL, 0, 0,
			 0);
}

Scheme_Object *
scheme_file_stream_port_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *p = argv[0];

  if (SCHEME_INPORTP(p)) {
    Scheme_Input_Port *ip = (Scheme_Input_Port *)p;

    if (SAME_OBJ(ip->sub_type, file_input_port_type))
      return scheme_true;
#ifdef MZ_FDS
    else if (SAME_OBJ(ip->sub_type, fd_input_port_type))
      return scheme_true;
#endif
  } else if (SCHEME_OUTPORTP(p)) {
    Scheme_Output_Port *op = (Scheme_Output_Port *)p;

    if (SAME_OBJ(op->sub_type, file_output_port_type))
      return scheme_true;
#ifdef MZ_FDS
    else if (SAME_OBJ(op->sub_type, fd_output_port_type))
      return scheme_true;
#endif
  } else {
    scheme_wrong_type("file-stream-port?", "port", 0, argc, argv);
  }

  return scheme_false;
}

int scheme_get_port_file_descriptor(Scheme_Object *p, long *_fd)
{
  long fd = 0;
  int fd_ok = 0;

  if (SCHEME_INPORTP(p)) {
    Scheme_Input_Port *ip = (Scheme_Input_Port *)p;

    if (!ip->closed) {
      if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
	fd = MSC_IZE(fileno)((FILE *)((Scheme_Input_File *)ip->port_data)->f);
	fd_ok = 1;
      }
#ifdef MZ_FDS
      else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
	fd = ((Scheme_FD *)ip->port_data)->fd;
	fd_ok = 1;
      }
#endif
    }
  } else if (SCHEME_OUTPORTP(p)) {
    Scheme_Output_Port *op = (Scheme_Output_Port *)p;

    if (!op->closed) {
      if (SAME_OBJ(op->sub_type, file_output_port_type))  {
	fd = MSC_IZE (fileno)((FILE *)((Scheme_Output_File *)op->port_data)->f);
	fd_ok = 1;
      }
#ifdef MZ_FDS
      else if (SAME_OBJ(op->sub_type, fd_output_port_type))  {
	fd = ((Scheme_FD *)op->port_data)->fd;
	fd_ok = 1;
      }
#endif
    }
  }

  if (!fd_ok)
    return 0;

  *_fd = fd;
  return 1;
}

Scheme_Object *scheme_file_identity(int argc, Scheme_Object *argv[])
{
  long fd = 0;
  int fd_ok = 0;
  Scheme_Object *p;

  p = argv[0];

  fd_ok = scheme_get_port_file_descriptor(p, &fd);

  if (!fd_ok) {
    /* Maybe failed because it was closed... */
    if (SCHEME_INPORTP(p)) {
      Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
      
      CHECK_PORT_CLOSED("port-file-identity", "input", p, ip->closed);
    } else if (SCHEME_OUTPORTP(p)) {
      Scheme_Output_Port *op = (Scheme_Output_Port *)p;
      
      CHECK_PORT_CLOSED("port-file-identity", "output", p, op->closed);
    }

    /* Otherwise, it's just the wrong type: */
    scheme_wrong_type("port-file-identity", "file-stream-port", 0, argc, argv);
    return NULL;
  }

  return scheme_get_fd_identity(p, fd);
}

static int is_fd_terminal(int fd)
{
#if defined(WIN32_FD_HANDLES)
  if (GetFileType((HANDLE)fd) == FILE_TYPE_CHAR)
    return 1;
  else
    return 0;
#else
  return isatty(fd);
#endif
}

Scheme_Object *scheme_terminal_port_p(int argc, Scheme_Object *argv[])
{
  long fd = 0;
  int fd_ok = 0;
  Scheme_Object *p;

  p = argv[0];

  if (SCHEME_INPORTP(p)) {
    Scheme_Input_Port *ip = (Scheme_Input_Port *)p;

    if (ip->closed)
      return scheme_false;

    if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
      fd = MSC_IZE(fileno)((FILE *)((Scheme_Input_File *)ip->port_data)->f);
      fd_ok = 1;
    }
#ifdef MZ_FDS
    else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
      fd = ((Scheme_FD *)ip->port_data)->fd;
      fd_ok = 1;
    }
#endif
  } else if (SCHEME_OUTPORTP(p)) {
    Scheme_Output_Port *op = (Scheme_Output_Port *)p;

    if (op->closed)
      return scheme_false;

    if (SAME_OBJ(op->sub_type, file_output_port_type))  {
      fd = MSC_IZE (fileno)((FILE *)((Scheme_Output_File *)op->port_data)->f);
      fd_ok = 1;
    }
#ifdef MZ_FDS
    else if (SAME_OBJ(op->sub_type, fd_output_port_type))  {
      fd = ((Scheme_FD *)op->port_data)->fd;
      fd_ok = 1;
    }
#endif
  }

  if (!fd_ok)
    return scheme_false;

  return is_fd_terminal(fd) ? scheme_true : scheme_false;
}

static void filename_exn(char *name, char *msg, char *filename, int err)
{
  char *dir, *drive;
  int len;
  char *pre, *rel, *post;

  len = strlen(filename);

  if (scheme_is_relative_path(filename, len)) {
    dir = scheme_os_getcwd(NULL, 0, NULL, 1);
    drive = NULL;
  } else if (scheme_is_complete_path(filename, len)) {
    dir = NULL;
    drive = NULL;
  } else {
    dir = NULL;
    drive = scheme_getdrive();
  }

  pre = dir ? " in directory \"" : (drive ? " on drive " : "");
  rel = dir ? dir : (drive ? drive : "");
  post = dir ? "\"" : "";

  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		   "%s: %s: \"%q\"%s%q%s (" FILENAME_EXN_E ")",
		   name, msg, filename,
		   pre, rel, post,
		   err);
}

Scheme_Object *
scheme_do_open_input_file(char *name, int offset, int argc, Scheme_Object *argv[])
{
#ifdef USE_FD_PORTS
  int fd;
  struct stat buf;
#else
# ifdef WINDOWS_FILE_HANDLES
  HANDLE fd;
# else
  FILE *fp;
# endif
#endif
  char *mode = "rb";
  char *filename;
  int regfile, i;
  int m_set = 0;
  Scheme_Object *result;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type(name, SCHEME_PATH_STRING_STR, 0, argc, argv);

  for (i = 1 + offset; argc > i; i++) {
    if (!SCHEME_SYMBOLP(argv[i]))
      scheme_wrong_type(name, "symbol", i, argc, argv);

    if (SAME_OBJ(argv[i], text_symbol)) {
      mode = "rt";
      m_set++;
    } else if (SAME_OBJ(argv[i], binary_symbol)) {
      /* This is the default */
      m_set++;
    } else {
      char *astr;
      long alen;

      astr = scheme_make_args_string("other ", i, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: bad mode: %s%t", name,
		       scheme_make_provided_string(argv[i], 1, NULL),
		       astr, alen);
    }

    if (m_set > 1) {
      char *astr;
      long alen;

      astr = scheme_make_args_string("", -1, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: conflicting or redundant "
		       "file modes given%t", name,
		       astr, alen);
    }
  }

  filename = scheme_expand_string_filename(argv[0],
					   name,
					   NULL,
					   SCHEME_GUARD_FILE_READ);

  scheme_custodian_check_available(NULL, name, "file-stream");

#ifdef USE_FD_PORTS
  /* Note: assuming there's no difference between text and binary mode */
  do {
    fd = open(filename, O_RDONLY | MZ_NONBLOCKING | MZ_BINARY);
  } while ((fd == -1) && (errno == EINTR));

  if (fd == -1) {
    filename_exn(name, "cannot open input file", filename, errno);
    return NULL;
  } else {
    int ok;

    do {
      ok = fstat(fd, &buf);
    } while ((ok == -1) && (errno == EINTR));

    if (S_ISDIR(buf.st_mode)) {
      int cr;
      do {
	cr = close(fd);
      } while ((cr == -1) && (errno == EINTR));
      filename_exn(name, "cannot open directory as a file", filename, 0);
      return NULL;
    } else {
      regfile = S_ISREG(buf.st_mode);
      scheme_file_open_count++;
      result = make_fd_input_port(fd, scheme_make_path(filename), regfile, 0, NULL);
    }
  }
#else
# ifdef WINDOWS_FILE_HANDLES
  fd = CreateFileW(WIDE_PATH(filename),
		   GENERIC_READ,
		   FILE_SHARE_READ | FILE_SHARE_WRITE,
		   NULL,
		   OPEN_EXISTING,
		   0,
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    filename_exn(name, "cannot open input file", filename, GetLastError());
    return NULL;
  } else
    regfile = (GetFileType(fd) == FILE_TYPE_DISK);

  if ((mode[1] == 't') && !regfile) {
    CloseHandle(fd);
    filename_exn(name, "cannot use text-mode on a non-file device", filename, 0);
    return NULL;
  }

  result = make_fd_input_port((int)fd, scheme_make_path(filename), regfile, mode[1] == 't', NULL);
# else
  if (scheme_directory_exists(filename)) {
    filename_exn(name, "cannot open directory as a file", filename, 0);
    return NULL;
  }

#  ifdef MAC_FILE_SYSTEM
  {
    FSSpec spec;
    SInt16 refnum;

    if (scheme_mac_path_to_spec(filename, &spec)) {
      errno = FSpOpenDF(&spec, fsRdWrShPerm, &refnum);
      if (errno == noErr)
	result = make_fd_input_port(refnum, scheme_make_path(filename), 1, mode[1] == 't', NULL);
      else {
	filename_exn(name, "could not open file", filename, errno);
	return NULL;
      }
    } else {
      filename_exn(name, "could not open file", filename, 0);
      return NULL;
    }
  }
#  else
  regfile = scheme_is_regular_file(filename);

  fp = fopen(filename, mode);
  if (!fp) {
    filename_exn(name, "cannot open input file", filename, errno);
    return NULL;
  }
  scheme_file_open_count++;

  result = scheme_make_named_file_input_port(fp, scheme_make_path(filename));
#  endif
# endif
#endif

  return result;
}

Scheme_Object *
scheme_do_open_output_file(char *name, int offset, int argc, Scheme_Object *argv[], int and_read)
{
#ifdef USE_FD_PORTS
  int fd;
  int flags, regfile;
  struct stat buf;
  int ok;
#else
# ifdef WINDOWS_FILE_HANDLES
  HANDLE fd;
  int hmode, regfile;
  BY_HANDLE_FILE_INFORMATION info;
# else
  FILE *fp;
# endif
#endif
  int e_set = 0, m_set = 0, i;
  int existsok = 0;
  char *filename;
  char mode[4];
  int typepos;

  mode[0] = 'w';
  mode[1] = 'b';
  mode[2] = 0;
  mode[3] = 0;
  typepos = 1;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type(name, SCHEME_PATH_STRING_STR, 0, argc, argv);

  for (i = 1 + offset; argc > i; i++) {
    if (!SCHEME_SYMBOLP(argv[i]))
      scheme_wrong_type(name, "symbol", i, argc, argv);

    if (SAME_OBJ(argv[i], append_symbol)) {
      mode[0] = 'a';
      existsok = -1;
      e_set++;
    } else if (SAME_OBJ(argv[i], replace_symbol)) {
      existsok = 1;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_symbol)) {
      existsok = -1;
      e_set++;
    } else if (SAME_OBJ(argv[i], truncate_replace_symbol)) {
      existsok = -2;
      e_set++;
    } else if (SAME_OBJ(argv[i], update_symbol)) {
      existsok = 2;
      if (typepos == 1) {
	mode[2] = mode[1];
	typepos = 2;
      }
      mode[0] = 'r';
      mode[1] = '+';
      e_set++;
    } else if (SAME_OBJ(argv[i], error_symbol)) {
      /* This is the default */
      e_set++;
    } else if (SAME_OBJ(argv[i], text_symbol)) {
      mode[typepos] = 't';
      m_set++;
    } else if (SAME_OBJ(argv[i], binary_symbol)) {
      /* This is the default */
      m_set++;
    } else {
      char *astr;
      long alen;

      astr = scheme_make_args_string("other ", i, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: bad mode: %s%s", name,
		       scheme_make_provided_string(argv[i], 1, NULL),
		       astr, alen);
    }

    if (m_set > 1 || e_set > 1) {
      char *astr;
      long alen;

      astr = scheme_make_args_string("", -1, argc, argv, &alen);
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "%s: conflicting or redundant "
		       "file modes given%t", name,
		       astr, alen);
    }
  }

  filename = scheme_expand_string_filename(argv[0],
					   name, NULL,
					   (SCHEME_GUARD_FILE_WRITE
					    | ((existsok && (existsok != -1))
					       ? SCHEME_GUARD_FILE_DELETE
					       : 0)
					    /* append mode: */
					    | ((mode[0] == 'a')
					       ? SCHEME_GUARD_FILE_READ
					       : 0)
					    /* update mode: */
					    | ((existsok > 1)
					       ? SCHEME_GUARD_FILE_READ
					       : 0)));

  scheme_custodian_check_available(NULL, name, "file-stream");

#ifdef USE_FD_PORTS
  /* Note: assuming there's no difference between text and binary mode */

  flags = (and_read ? O_RDWR : O_WRONLY) | O_CREAT;

  if (mode[0] == 'a')
    flags |= O_APPEND;
  else if (existsok < 0)
    flags |= O_TRUNC;

  if (existsok > 1)
    flags -= O_CREAT;
  else if (existsok > -1)
    flags |= O_EXCL;

  do {
    fd = open(filename, flags | MZ_NONBLOCKING | MZ_BINARY, 0666);
  } while ((fd == -1) && (errno == EINTR));

  if (errno == ENXIO) {
    /* FIFO with no reader? Try opening in RW mode: */
    flags -= O_WRONLY;
    flags |= O_RDWR;
    do {
      fd = open(filename, flags | MZ_NONBLOCKING | MZ_BINARY, 0666);
    } while ((fd == -1) && (errno == EINTR));
  }

  if (fd == -1) {
    if (errno == EISDIR) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: \"%q\" exists as a directory",
		       name, filename);
    } else if (errno == EEXIST) {
      if (!existsok)
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
			 "%s: file \"%q\" exists", name, filename);
      else {
	do {
	  ok = unlink(filename);
	} while ((ok == -1) && (errno == EINTR));

	if (ok)
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "%s: error deleting \"%q\"",
			   name, filename);
	do {
	  fd = open(filename, flags | MZ_BINARY, 0666);
	} while ((fd == -1) && (errno == EINTR));
      }
    }

    if (fd == -1) {
      filename_exn(name, "cannot open output file", filename, errno);
      return NULL; /* shouldn't get here */
    }
  }

  do {
    ok = fstat(fd, &buf);
  } while ((ok == -1) && (errno == EINTR));

  regfile = S_ISREG(buf.st_mode);
  scheme_file_open_count++;
  return make_fd_output_port(fd, scheme_make_path(filename), regfile, 0, and_read);
#else
# ifdef WINDOWS_FILE_HANDLES
  if (!existsok)
    hmode = CREATE_NEW;
  else if (existsok < 0)
    hmode = OPEN_ALWAYS;
  else if (existsok  == 1)
    hmode = CREATE_ALWAYS;
  else if (existsok  == 2)
    hmode = OPEN_ALWAYS;

  fd = CreateFileW(WIDE_PATH(filename),
		   GENERIC_WRITE | (and_read ? GENERIC_READ : 0),
		   FILE_SHARE_READ | FILE_SHARE_WRITE,
		   NULL,
		   hmode,
		   FILE_FLAG_BACKUP_SEMANTICS, /* lets us detect directories in NT */
		   NULL);

  if (fd == INVALID_HANDLE_VALUE) {
    int err;
    err = GetLastError();
    if ((err == ERROR_ACCESS_DENIED) && (existsok < -1)) {
      /* Delete and try again... */
      if (DeleteFile(filename)) {
	fd = CreateFile(filename,
			GENERIC_WRITE,
			FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL,
			hmode,
			0,
			NULL);
	if (fd == INVALID_HANDLE_VALUE)
	  err = GetLastError();
      } else {
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "%s: error deleting \"%q\" (%E)",
			 name, filename, GetLastError());
	return NULL;
      }
    } else if (err == ERROR_FILE_EXISTS) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: file \"%q\" exists", name, filename);
      return NULL;
    }

    if (fd == INVALID_HANDLE_VALUE) {
      filename_exn(name, "cannot open output file", filename, err);
      return NULL;
    }
  }

  if (GetFileInformationByHandle(fd, &info)) {
    if (info.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
      CloseHandle(fd);
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: \"%q\" exists as a directory",
		       name, filename);
      return NULL;
    }
  }

  regfile = (GetFileType(fd) == FILE_TYPE_DISK);

  if ((mode[1] == 't') && !regfile) {
    CloseHandle(fd);
    filename_exn(name, "cannot use text-mode on a non-file device", filename, 0);
    return NULL;
  }

  if (regfile && (existsok < 0)) {
    if (mode[0] == 'a')
      SetFilePointer(fd, 0, NULL, FILE_END);
    else
      SetEndOfFile(fd);
  }

  scheme_file_open_count++;
  return make_fd_output_port((int)fd, scheme_make_path(filename), regfile, mode[1] == 't', and_read);
# else
  if (scheme_directory_exists(filename)) {
    if (!existsok)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: \"%q\" exists as a directory",
		       name, filename);
    else
      filename_exn(name, "cannot open directory as a file", filename, errno);
    return scheme_void;
  }


#  ifdef MAC_FILE_SYSTEM
  {
    FSSpec spec;
    SInt16 refnum;
    int creating = 0;

    if (scheme_mac_path_to_spec(filename, &spec)) {
      if (existsok == 1) {
	/* In case it's there: */
	FSpDelete(&spec);
      }

      errno = FSpCreate(&spec, 'MrEd', 'TEXT', smSystemScript);
      if (errno == dupFNErr) {
	if (!existsok) {
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
			   "%s: file \"%q\" exists", name, filename);
	  return NULL;
	}
      } else
	creating = 1;
      errno = FSpOpenDF(&spec, fsRdWrShPerm, &refnum);
      if ((errno == noErr) && (existsok < 0)) {
	/* truncate or truncate/replace */
	SetEOF(refnum, 0);
      }

      if (errno == noErr) {
	if (creating)
	  scheme_file_create_hook(filename);

	scheme_file_open_count++;
	return make_fd_output_port(refnum, scheme_make_path(filename), 1, mode[1] == 't', and_read);
      } else {
	filename_exn(name, "could not open file", filename, errno);
	return NULL;
      }
    } else {
      filename_exn(name, "could not open file", filename, 0);
      return NULL;
    }
  }
#  else

  if (and_read) {
    scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		     "%s: not supported on this platform",
		     name);
    return NULL;
  }

  if (scheme_file_exists(filename)) {
    int uok;

    if (!existsok)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: file \"%q\" exists", name, filename);
    do {
      uok = MSC_IZE(unlink)(filename);
    } while ((uok == -1) && (errno == EINTR));

    if (uok)
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
		       "%s: error deleting \"%q\" (%e)",
		       name, filename, errno);
  }

  fp = fopen(filename, mode);
  if (!fp) {
    if (existsok < -1) {
      /* Can't truncate; try to replace */
      if (scheme_file_exists(filename)) {
	int uok;

	do {
	  uok = MSC_IZE(unlink)(filename);
	} while ((uok == -1) && (errno == EINTR));

	if (uok)
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM_EXISTS,
			   "%s: error deleting \"%q\"",
			   name, filename);
	else {
	  fp = fopen(filename, mode);
	}
      }
    }
    if (!fp)
      filename_exn(name, "cannot open output file", filename, errno);
  }
  scheme_file_open_count++;

  return scheme_make_file_output_port(fp);
#  endif
# endif
#endif
}

Scheme_Object *scheme_open_input_file(const char *name, const char *who)
{
  Scheme_Object *a[1];

  a[0]= scheme_make_path(name);
  return scheme_do_open_input_file((char *)who, 0, 1, a);
}

Scheme_Object *scheme_open_output_file(const char *name, const char *who)
{
  Scheme_Object *a[2];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  return scheme_do_open_output_file((char *)who, 0, 2, a, 0);
}

Scheme_Object *scheme_open_output_file_with_mode(const char *name, const char *who, int text)
{
  Scheme_Object *a[3];

  a[0]= scheme_make_path(name);
  a[1] = truncate_replace_symbol;
  a[2] = (text ? text_symbol : binary_symbol);
  return scheme_do_open_output_file((char *)who, 0, 3, a, 0);
}

Scheme_Object *
scheme_file_position(int argc, Scheme_Object *argv[])
{
  FILE *f;
  Scheme_Indexed_String *is;
  int fd;
#ifdef MZ_FDS
  int had_fd;
#endif
  int wis;

  if (!SCHEME_OUTPORTP(argv[0]) && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("file-position", "port", 0, argc, argv);
  if (argc == 2) {
    if (!SCHEME_EOFP(argv[1])) {
      int ok = 0;

      if (SCHEME_INTP(argv[1])) {
	ok = (SCHEME_INT_VAL(argv[1]) >= 0);
      }
      
      if (SCHEME_BIGNUMP(argv[1])) {
	ok = SCHEME_BIGPOS(argv[1]);
      }
      
      if (!ok)
	scheme_wrong_type("file-position", "non-negative exact integer or eof", 1, argc, argv);
    }
  }

  f = NULL;
  is = NULL;
  wis = 0;
  fd = 0;
#ifdef MZ_FDS
  had_fd = 0;
#endif

  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;

    op = (Scheme_Output_Port *)argv[0];
    if (SAME_OBJ(op->sub_type, file_output_port_type)) {
      f = ((Scheme_Output_File *)op->port_data)->f;
#ifdef MZ_FDS
    } else if (SAME_OBJ(op->sub_type, fd_output_port_type)) {
      fd = ((Scheme_FD *)op->port_data)->fd;
      had_fd = 1;
#endif
    } else if (SAME_OBJ(op->sub_type, scheme_string_output_port_type)) {
      is = (Scheme_Indexed_String *)op->port_data;
      wis = 1;
    } else if (argc < 2)
      return scheme_make_integer(scheme_output_tell(argv[0]));
  } else if (SCHEME_INPORTP(argv[0])) {
    Scheme_Input_Port *ip;

    ip = (Scheme_Input_Port *)argv[0];
    if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
      f = ((Scheme_Input_File *)ip->port_data)->f;
#ifdef MZ_FDS
    } else if (SAME_OBJ(ip->sub_type, fd_input_port_type)) {
      fd = ((Scheme_FD *)ip->port_data)->fd;
      had_fd = 1;
#endif
    } else if (SAME_OBJ(ip->sub_type, scheme_string_input_port_type))
      is = (Scheme_Indexed_String *)ip->port_data;
    else if (argc < 2) {
      long pos;
      pos = ((Scheme_Input_Port *)argv[0])->p.position;
      if (pos < 0) {
	scheme_raise_exn(MZEXN_FAIL,
			 "the port's current position is not known: %v",
			 ip);
      }
      return scheme_make_integer_value(pos);
    }
  }

  if (!f
#ifdef MZ_FDS
      && !had_fd
#endif
      && !is)
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "file-position: setting position allowed for file-stream and string ports only;"
		     " given %s and position %s",
		     scheme_make_provided_string(argv[0], 2, NULL),
		     scheme_make_provided_string(argv[1], 2, NULL));

  if ((argc > 1) && SCHEME_BIGNUMP(argv[1]))
    scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		     "file-position: new position is too large: %s for port: %s",
		     scheme_make_provided_string(argv[1], 2, NULL),
		     scheme_make_provided_string(argv[0], 2, NULL));

  if (argc > 1) {
    long n;
    int whence;

    if (SCHEME_INTP(argv[1])) {
      n = SCHEME_INT_VAL(argv[1]);
      whence = SEEK_SET;
    } else {
      n = 0;
      whence = SEEK_END;
    }
      
    if (f) {
      if (fseek(f, n, whence)) {
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "file-position: position change failed on file (%e)",
			 errno);
      }
#ifdef MZ_FDS
    } else if (had_fd) {
      long lv;
      
      if (SCHEME_OUTPORTP(argv[0])) {
	flush_fd((Scheme_Output_Port *)argv[0], NULL, 0, 0, 0, 0);
      }
      
# ifdef WINDOWS_FILE_HANDLES
      lv = SetFilePointer((HANDLE)fd, n, NULL, 
			  ((whence == SEEK_SET) ? FILE_BEGIN : FILE_END));
# else
#  ifdef MAC_FILE_HANDLES
			  {
	errno = SetFPos(fd, ((whence == SEEK_SET) ? fsFromStart : fsFromLEOF), n);
	if (errno == noErr)
	  lv = 0;
	else
	  lv = -1;
      }
#  else
      lv = lseek(fd, n, whence);
#  endif
# endif

      if (lv < 0) {
# ifdef WINDOWS_FILE_HANDLES
	int errid;
	errid = GetLastError();
	errno = errid;
# endif
	scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			 "file-position: position change failed on stream (" FILENAME_EXN_E ")",
			 errno);
      }

      if (SCHEME_INPORTP(argv[0])) {
	/* Get rid of buffered data: */
	Scheme_FD *sfd;
	sfd = (Scheme_FD *)((Scheme_Input_Port *)argv[0])->port_data;
	sfd->bufcount = 0;
	sfd->buffpos = 0;
	/* 1 means no pending eof, but can set: */
	((Scheme_Input_Port *)argv[0])->pending_eof = 1;
      }
#endif
    } else {
      if (whence == SEEK_END) {
	n = is->size;
      }
      if (wis) {
	if (is->index > is->u.hot)
	  is->u.hot = is->index;
	if (is->size < is->index + n) {
	  /* Expand string up to n: */
	  char *old;

	  old = is->string;
	  is->size = is->index + n;
	  {
	    char *ca;
	    ca = (char *)scheme_malloc_atomic(is->size + 1);
	    is->string = ca;
	  }
	  memcpy(is->string, old, is->index);
	}
	if (n > is->u.hot)
	  memset(is->string + is->u.hot, 0, n - is->u.hot);
      } else {
	/* Can't really move past end of read string, but pretend we do: */
	if (n > is->size) {
	  is->u.pos = n;
	  n = is->size;
	} else
	  is->u.pos = 0;
      }
      is->index = n;
    }

    /* Remove any chars saved from peeks: */
    if (SCHEME_INPORTP(argv[0])) {
      Scheme_Input_Port *ip;
      ip = (Scheme_Input_Port *)argv[0];
      ip->ungotten_count = 0;
      if (pipe_char_count(ip->peeked_read)) {
	ip->peeked_read = NULL;
	ip->peeked_write = NULL;
      }
    }

    return scheme_void;
  } else {
    long p;
    if (f) {
      p = ftell(f);
#ifdef MZ_FDS
    } else if (had_fd) {
# ifdef WINDOWS_FILE_HANDLES
      p = SetFilePointer((HANDLE)fd, 0, NULL, FILE_CURRENT);
# else
#  ifdef MAC_FILE_HANDLES
      {
	SInt32 pos;
	errno = GetFPos(fd, &pos);
	if (errno == noErr)
	  p = pos;
	else
	  p = -1;
      }
#  else
      p = lseek(fd, 0, 1);
#  endif
# endif
      if (p < 0) {
	if (SCHEME_INPORTP(argv[0])) {
	  p = scheme_tell(argv[0]);
	} else {
	  p = scheme_output_tell(argv[0]);
	}
      } else {
	if (SCHEME_OUTPORTP(argv[0])) {
	  p += ((Scheme_FD *)((Scheme_Output_Port *)argv[0])->port_data)->bufcount;
	} else {
	  p -= ((Scheme_FD *)((Scheme_Input_Port *)argv[0])->port_data)->bufcount;
	}
      }
#endif
    } else if (wis)
      p = is->index;
    else {
      /* u.pos > index implies we previously moved past the end with file-position */
      if (is->u.pos > is->index)
	p = is->u.pos;
      else
	p = is->index;
    }

    /* Back up for un-gotten & peeked chars: */
    if (SCHEME_INPORTP(argv[0])) {
      Scheme_Input_Port *ip;
      ip = (Scheme_Input_Port *)argv[0];
      p -= ip->ungotten_count;
      p -= pipe_char_count(ip->peeked_read);
    }

    return scheme_make_integer(p);
  }
}

long scheme_set_file_position(Scheme_Object *port, long pos)
{
  if (pos >= 0) {
    Scheme_Object *a[2];

    a[0] = port;
    a[1] = scheme_make_integer(pos);
    (void)scheme_file_position(2, a);
    return 0;
  } else {
    Scheme_Object *n;
    n = scheme_file_position(1, &port);
    return SCHEME_INT_VAL(n);
  }
}

Scheme_Object *
scheme_file_buffer(int argc, Scheme_Object *argv[])
{
  Scheme_Port *p = NULL;

  if (!SCHEME_OUTPORTP(argv[0]) && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("file-stream-buffer-mode", "port", 0, argc, argv);

  p = (Scheme_Port *)argv[0];

  if (argc == 1) {
    Scheme_Buffer_Mode_Fun bm;

    bm = p->buffer_mode_fun;
    if (bm) {
      switch (bm(p, -1)) {
      case MZ_FLUSH_NEVER:
	return scheme_block_symbol;
      case MZ_FLUSH_BY_LINE:
	return scheme_line_symbol;
      case MZ_FLUSH_ALWAYS:
	return scheme_none_symbol;
      }
    }

    return scheme_false;
  } else {
    Scheme_Object *s = argv[1];
    Scheme_Buffer_Mode_Fun bm;

    if (!SAME_OBJ(s, scheme_block_symbol)
	&& !SAME_OBJ(s, scheme_line_symbol)
	&& !SAME_OBJ(s, scheme_none_symbol))
      scheme_wrong_type("file-stream-buffer-mode", "'none, 'line, or 'block", 1, argc, argv);

    if (SCHEME_INPORTP(argv[0]) && SAME_OBJ(s, scheme_line_symbol))
      scheme_arg_mismatch("file-stream-buffer-mode", 
			  "'line buffering not supported for an input port: ",
			  argv[0]);

    bm = p->buffer_mode_fun;
    if (bm) {
      int mode;
      if (SAME_OBJ(s, scheme_block_symbol))
	mode = MZ_FLUSH_NEVER;
      else if (SAME_OBJ(s, scheme_line_symbol))
	mode = MZ_FLUSH_BY_LINE;
      else
	mode = MZ_FLUSH_ALWAYS;

      bm(p, mode);
    } else {
      scheme_arg_mismatch("file-stream-buffer-mode", 
			  "cannot set buffer mode on port: ",
			  argv[0]);
    }

    return scheme_void;
  }
}

/*========================================================================*/
/*                          FILE input ports                              */
/*========================================================================*/

static int
file_byte_ready (Scheme_Input_Port *port)
{
  return 1;
}

static long file_get_string(Scheme_Input_Port *port,
			    char *buffer, long offset, long size,
			    int nonblock,
			    Scheme_Object *unless_evt)
{
  FILE *fp;
  Scheme_Input_File *fip;
  int c;

  fip = (Scheme_Input_File *)port->port_data;
  fp = fip->f;

  c = fread(buffer XFORM_OK_PLUS offset, 1, size, fp);

  if (c <= 0) {
    if (!feof(fp)) {
      scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		       "error reading from file port %V (%e)",
		       port->name, errno);
      return 0;
    } else
      c = EOF;
#ifndef DONT_CLEAR_FILE_EOF
    clearerr(fp);
#endif
  }

  return c;
}

static void
file_close_input(Scheme_Input_Port *port)
{
  Scheme_Input_File *fip;

  fip = (Scheme_Input_File *)port->port_data;

  fclose(fip->f);
  --scheme_file_open_count;
}

static void
file_need_wakeup(Scheme_Input_Port *port, void *fds)
{
}

static int
file_buffer_mode(Scheme_Port *p, int mode)
{
  FILE *f;
  int bad;

  if (mode < 0)
    return -1; /* unknown mode */

  if (SCHEME_INPORTP(p))
    f = ((Scheme_Output_File *)((Scheme_Input_Port *)p)->port_data)->f;
  else
    f = ((Scheme_Output_File *)((Scheme_Output_Port *)p)->port_data)->f;
  
  if (mode == MZ_FLUSH_NEVER)
    bad = setvbuf(f, NULL, _IOFBF, 0);
  else if (mode == MZ_FLUSH_BY_LINE)
    bad = setvbuf(f, NULL, _IOLBF, 0);
  else
    bad = setvbuf(f, NULL, _IONBF, 0);
  
  if (bad) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "file-stream-buffer-mode: error changing buffering (%e)",
		     errno);
  }

  return mode;
}


static Scheme_Object *
_scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name, int regfile)
{
  Scheme_Input_Port *ip;
  Scheme_Input_File *fip;

  if (!fp)
    scheme_signal_error("make-file-input-port(internal): "
			"null file pointer");

  fip = MALLOC_ONE_RT(Scheme_Input_File);
#ifdef MZTAG_REQUIRED
  fip->type = scheme_rt_input_file;
#endif

  fip->f = fp;

  ip = scheme_make_input_port(file_input_port_type,
			      fip,
			      name,
			      file_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_peeked_read_via_get,
			      file_byte_ready,
			      file_close_input,
			      file_need_wakeup,
			      1);
  ip->p.buffer_mode_fun = file_buffer_mode;

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_named_file_input_port(FILE *fp, Scheme_Object *name)
{
  return _scheme_make_named_file_input_port(fp, name, 0);
}

Scheme_Object *
scheme_make_file_input_port(FILE *fp)
{
  return scheme_make_named_file_input_port(fp, scheme_intern_symbol("file"));
}

/*========================================================================*/
/*                           fd input ports                               */
/*========================================================================*/

#ifdef MZ_FDS

# ifdef WINDOWS_FILE_HANDLES
static long WindowsFDReader(Win_FD_Input_Thread *th);
static void WindowsFDICleanup(Win_FD_Input_Thread *th);
# endif

/* forward decl: */
static void fd_need_wakeup(Scheme_Input_Port *port, void *fds);

#ifdef SOME_FDS_ARE_NOT_SELECTABLE
static int try_get_fd_char(int fd, int *ready)
{
  int old_flags, c;
  unsigned char buf[1];

  old_flags = fcntl(fd, F_GETFL, 0);
  fcntl(fd, F_SETFL, old_flags | MZ_NONBLOCKING);
  do {
    c = read(fd, buf, 1);
  } while ((c == -1) && errno == EINTR);
  fcntl(fd, F_SETFL, old_flags);

  if (c < 0) {
    *ready = 0;
    return 0;
  } else {
    *ready = 1;
    if (!c)
      return EOF;
    else
      return buf[0];
  }
}
#endif

static int
fd_byte_ready (Scheme_Input_Port *port)
{
  Scheme_FD *fip;

  fip = (Scheme_FD *)port->port_data;

  if (fip->regfile || port->closed)
    return 1;

  if (fip->bufcount)
    return 1;
  else {
#ifdef WINDOWS_FILE_HANDLES
    if (!fip->th) {
      /* No thread -- so wait works. This case isn't actually used
	 right now, because wait doesn't seem to work reliably for
	 anything that we can recognize other than regfiles, which are
	 handled above. */
      if (WaitForSingleObject((HANDLE)fip->fd, 0) == WAIT_OBJECT_0)
	return 1;
    } else {
      /* Has the reader thread pulled in data? */
      if (fip->th->checking) {
	/* The thread is still trying, last we knew. Check the
	   data-is-ready sema: */
	if (WaitForSingleObject(fip->th->ready_sema, 0) == WAIT_OBJECT_0) {
	  fip->th->checking = 0;
	  return 1;
	}
      } else if (fip->th->avail || fip->th->err || fip->th->eof)
	return 1; /* other thread found data */
      else {
	/* Doesn't have anything, and it's not even looking. Tell it
	   to look: */
	fip->th->checking = 1;
	ReleaseSemaphore(fip->th->checking_sema, 1, NULL);
      }
    }

    return 0;
#else
# ifdef MAC_FILE_HANDLES
    return 1;
# else
    int r;
    DECL_FDSET(readfds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};

    INIT_DECL_FDSET(readfds, 1);
    INIT_DECL_FDSET(exnfds, 1);

    MZ_FD_ZERO(readfds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(fip->fd, readfds);
    MZ_FD_SET(fip->fd, exnfds);

    do {
      r = select(fip->fd + 1, readfds, NULL, exnfds, &time);
    } while ((r == -1) && (errno == EINTR));

# ifdef SOME_FDS_ARE_NOT_SELECTABLE
    /* Try a non-blocking read: */
    if (!r && !fip->textmode) {
      int c, ready;

      c = try_get_fd_char(fip->fd, &ready);
      if (ready) {
	if (c != EOF) {
	  fip->buffpos = 0;
	  fip->buffer[0] = (unsigned char)c;
	  fip->bufcount = 1;
	}
	r = 1;
      }
    }
# endif

    return r;
# endif
#endif
  }
}

static long fd_get_string(Scheme_Input_Port *port,
			  char *buffer, long offset, long size,
			  int nonblock,
			  Scheme_Object *unless)
{
  Scheme_FD *fip;
  long bc;

  if (unless && scheme_unless_ready(unless))
    return SCHEME_UNLESS_READY;

  fip = (Scheme_FD *)port->port_data;

  if (fip->bufcount) {
    if (size == 1) {
      buffer[offset] = fip->buffer[fip->buffpos++];
      --fip->bufcount;
      return 1;
    } else {
      bc = ((size <= fip->bufcount)
	    ? size
	    : fip->bufcount);

      memcpy(buffer + offset, fip->buffer + fip->buffpos, bc);
      fip->buffpos += bc;
      fip->bufcount -= bc;

      return bc;
    }
  } else {
    if ((nonblock == 2) && (fip->flush == MZ_FLUSH_ALWAYS))
      return 0;

    while (1) {
      /* Loop until a read succeeds. */
      int none_avail = 0;
      int target_size, target_offset, ext_target;
      char *target;

      /* If no chars appear to be ready, go to sleep. */
      while (!fd_byte_ready(port)) {
	if (nonblock > 0)
	  return 0;

	scheme_block_until_unless((Scheme_Ready_Fun)fd_byte_ready,
				  (Scheme_Needs_Wakeup_Fun)fd_need_wakeup,
				  (Scheme_Object *)port,
				  0.0, unless,
				  nonblock);

	scheme_wait_input_allowed(port, nonblock);

	if (scheme_unless_ready(unless))
	  return SCHEME_UNLESS_READY;
      }

      if (port->closed) {
	/* Another thread closed the input port while we were waiting. */
	/* Call scheme_getc to signal the error */
	scheme_get_byte((Scheme_Object *)port);
      }

      /* Another thread might have filled the buffer, or
	 if SOME_FDS_ARE_NOT_SELECTABLE is set,
	 fd_byte_ready might have read one character. */
      if (fip->bufcount) {
	bc = ((size <= fip->bufcount)
	      ? size
	      : fip->bufcount);

	memcpy(buffer + offset, fip->buffer + fip->buffpos, bc);
	fip->buffpos += bc;
	fip->bufcount -= bc;

	return bc;
      }

      if ((size >= MZPORT_FD_DIRECT_THRESHOLD) && (fip->flush != MZ_FLUSH_ALWAYS)) {
	ext_target = 1;
	target = buffer;
	target_offset = offset;
	target_size = size;
      } else {
	ext_target = 0;
	target = (char *)fip->buffer;
	target_offset = 0;
	if (fip->flush == MZ_FLUSH_ALWAYS)
	  target_size = 1;
	else
	  target_size = MZPORT_FD_BUFFSIZE;
      }

#ifdef WINDOWS_FILE_HANDLES
      if (!fip->th) {
	/* We can read directly. This must be a regular file, where
	   reading never blocks. */
	DWORD rgot, delta;

	if (fip->textmode) {
	  ext_target = 0;
	  target = fip->buffer;
	  target_offset = 0;
	  if (fip->flush == MZ_FLUSH_ALWAYS)
	    target_size = 1;
	  else
	    target_size = MZPORT_FD_BUFFSIZE;
	}

	rgot = target_size;

	/* Pending CR in text mode? */
	if (fip->textmode == 2) {
	  delta = 1;
	  if (rgot > 1)
	    rgot--;
	  fip->buffer[0] = '\r';
	} else
	  delta = 0;

	if (ReadFile((HANDLE)fip->fd, target XFORM_OK_PLUS target_offset + delta, rgot, &rgot, NULL)) {
	  bc = rgot;
	} else {
	  int errid;
	  bc = -1;
	  errid = GetLastError();
	  errno = errid;
	}

	/* bc == 0 and no err => EOF */

	/* Finish text-mode handling: */
	if (fip->textmode && (bc >= 0)) {
	  int i, j;
	  unsigned char *buf;

	  if (fip->textmode == 2) {
	    /* we had added a CR */
	    bc++;
	    fip->textmode = 1;
	  }

	  /* If bc is only 1, then we've reached the end, and
	     any leftover CR there should stay. */
	  if (bc > 1) {
	    /* Collapse CR-LF: */
	    buf = fip->buffer;
	    for (i = 0, j = 0; i < bc - 1; i++) {
	      if ((buf[i] == '\r')
		  && (buf[i+1] == '\n')) {
		buf[j++] = '\n';
		i++;
	      } else
		buf[j++] = buf[i];
	    }
	    if (i < bc) /* common case: didn't end with CRLF */
	      buf[j++] = buf[i];
	    bc = j;
	    /* Check for CR at end; if there, save it to maybe get a
	       LF on the next read: */
	    if (buf[bc - 1] == '\r') {
	      bc--;
	      fip->textmode = 2; /* 2 indicates a leftover CR */
	    }
	  }
	}

      } else {
	ext_target = 0;

	/* If we get this far, there's definitely data available.
	   Extract data made available by the reader thread. */
	if (fip->th->eof) {
	  bc = 0;
	} else if (fip->th->err) {
	  bc = -1;
	  errno = fip->th->err;
	} else {
	  bc = fip->th->avail;
	  fip->th->avail = 0;
	}
      }
#else
# ifdef MAC_FILE_HANDLES
      {
	SInt32 cnt = target_size;

	errno = FSRead(fip->fd, &cnt, target + target_offset);
	if (!cnt && (errno != eofErr))
	  bc = -1;
	else
	  bc = cnt;
      }
# else
      if (fip->regfile) {
	do {
	  bc = read(fip->fd, target + target_offset, target_size);
	} while ((bc == -1) && (errno == EINTR));
      } else {
	/* We use a non-blocking read here, even though we've waited
	   for input above, because an external process might have
	   gobbled the characters that we expected to get. */
	int old_flags;

	old_flags = fcntl(fip->fd, F_GETFL, 0);
	fcntl(fip->fd, F_SETFL, old_flags | MZ_NONBLOCKING);
	do {
	  bc = read(fip->fd, target + target_offset, target_size);
	} while ((bc == -1) && errno == EINTR);
	fcntl(fip->fd, F_SETFL, old_flags);

	if ((bc == -1) && (errno == EAGAIN)) {
	  none_avail = 1;
	  bc = 0;
	}
      }
# endif
#endif

      if (!none_avail) {
	if (ext_target && (bc > 0)) {
	  return bc;
	}

	fip->bufcount = bc;

	if (fip->bufcount < 0) {
	  fip->bufcount = 0;
	  fip->buffpos = 0;
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "error reading from stream port %V (" FILENAME_EXN_E ")",
			   port->name, errno);
	  return 0;
	}

	if (!fip->bufcount) {
	  fip->buffpos = 0;
	  return EOF;
	} else {
	  bc = ((size <= fip->bufcount)
		? size
		: fip->bufcount);

	  memcpy(buffer + offset, fip->buffer, bc);
	  fip->buffpos = bc;
	  fip->bufcount -= bc;

	  return bc;
	}
      } else if (nonblock > 0) {
	return 0;
      }
    }
  }
}

static void
fd_close_input(Scheme_Input_Port *port)
{
  Scheme_FD *fip;

  fip = (Scheme_FD *)port->port_data;

  if (fip->refcount)
    *fip->refcount -= 1;

#ifdef WINDOWS_FILE_HANDLES
  if (fip->th) {
    /* -1 for checking means "shut down" */
    fip->th->checking = -1;
    ReleaseSemaphore(fip->th->checking_sema, 1, NULL);

    /* Try to get out of cleaning up the records (since they can't be
       cleaned until the thread is also done: */
    if (WaitForSingleObject(fip->th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* The other thread exited and left us with clean-up: */
      WindowsFDICleanup(fip->th);
    } /* otherwise, thread is responsible for clean-up */
  }
  if (!fip->refcount || !*fip->refcount) {
    CloseHandle((HANDLE)fip->fd);
  }
#else
  if (!fip->refcount || !*fip->refcount) {
# ifdef MAC_FILE_HANDLES
    FSClose(fip->fd);
# else
    {
      int cr;
      do {
	cr = close(fip->fd);
      } while ((cr == -1) && (errno == EINTR));
    }
# endif
  }
#endif

  --scheme_file_open_count;
}

static void
fd_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_FD *fip;

#ifdef WINDOWS_FILE_HANDLES
#else
# ifdef MAC_FILE_HANDLES
# else
  void *fds2;
  int n;
# endif
#endif

  fip = (Scheme_FD *)port->port_data;

#ifdef WINDOWS_FILE_HANDLES
  if (fip->th) {
    /* See fd_byte_ready */
    if (!fip->th->checking) {
      if (fip->th->avail || fip->th->err || fip->th->eof) {
	/* Data is ready. We shouldn't be trying to sleep, so force an
	   immediate wake-up: */
	scheme_add_fd_nosleep(fds);
      } else {
	fip->th->checking = 1;
	ReleaseSemaphore(fip->th->checking_sema, 1, NULL);
	scheme_add_fd_handle((void *)fip->th->ready_sema, fds, 1);
      }
    } else
      scheme_add_fd_handle((void *)fip->th->ready_sema, fds, 1);
  } else if (fip->regfile) {
    /* regular files never block */
    scheme_add_fd_nosleep(fds);
  } else {
    /* This case is not currently used. See fd_byte_ready. */
    scheme_add_fd_handle((void *)fip->fd, fds, 0);
  }
#else
# ifdef MAC_FILE_HANDLES
# else
  n = fip->fd;
  MZ_FD_SET(n, (fd_set *)fds);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(n, (fd_set *)fds2);
# endif
#endif
}

static int fd_input_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_FD *fd;
    
  fd = (Scheme_FD *)((Scheme_Input_Port *)p)->port_data;

  if (mode < 0) {
    return fd->flush;
  } else {
    fd->flush = mode;
    return mode;
  }
}

static Scheme_Object *
make_fd_input_port(int fd, Scheme_Object *name, int regfile, int win_textmode, int *refcount)
{
  Scheme_Input_Port *ip;
  Scheme_FD *fip;
  unsigned char *bfr;

  fip = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fip->type = scheme_rt_input_fd;
#endif

  bfr = (unsigned char *)scheme_malloc_atomic(MZPORT_FD_BUFFSIZE);
  fip->buffer = bfr;

  fip->fd = fd;
  fip->bufcount = 0;

  fip->regfile = regfile;
#ifdef SOME_FDS_ARE_NOT_SELECTABLE
  if (regfile || isatty(fd))
    fip->textmode = 1;
#else
  fip->textmode = win_textmode;  
#endif

  fip->refcount = refcount;

  fip->flush = MZ_FLUSH_NEVER;

  ip = scheme_make_input_port(fd_input_port_type,
			      fip,
			      name,
			      fd_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_peeked_read_via_get,
			      fd_byte_ready,
			      fd_close_input,
			      fd_need_wakeup,
			      1);
  ip->p.buffer_mode_fun = fd_input_buffer_mode;

  ip->pending_eof = 1; /* means that pending EOFs should be tracked */

#ifdef WINDOWS_FILE_HANDLES
  if (!regfile) {
    /* To get non-blocking I/O for anything that can block, we create
       a separate reader thread.

       Yes, Windows NT pipes support non-blocking reads, but there
       doesn't seem to be any way to use WaitForSingleObject to sleep
       until characters are ready. PeekNamedPipe can be used for
       polling, but not sleeping. */

    Win_FD_Input_Thread *th;
    DWORD id;
    HANDLE h;
    OS_SEMAPHORE_TYPE sm;

    th = (Win_FD_Input_Thread *)malloc(sizeof(Win_FD_Input_Thread));
    fip->th = th;

    /* Replace buffer with a malloced one: */
    bfr = (unsigned char *)malloc(MZPORT_FD_BUFFSIZE);
    fip->buffer = bfr;
    th->buffer = bfr;

    th->fd = (HANDLE)fd;
    th->avail = 0;
    th->err = 0;
    th->eof = 0;
    th->checking = 0;
    
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->checking_sema = sm;
    sm = CreateSemaphore(NULL, 0, 1, NULL);
    th->ready_sema = sm;
    sm = CreateSemaphore(NULL, 1, 1, NULL);
    th->you_clean_up_sema = sm;

    h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDReader, th, 0, &id);

    scheme_remember_thread(h, 1);
  }
#endif

  return (Scheme_Object *)ip;
}

# ifdef WINDOWS_FILE_HANDLES

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

static long WindowsFDReader(Win_FD_Input_Thread *th)
{
  DWORD toget, got;

  if (GetFileType((HANDLE)th->fd) == FILE_TYPE_PIPE) {
    /* Reading from a pipe will return early when data is available. */
    toget = MZPORT_FD_BUFFSIZE;
  } else {
    /* Non-pipe: get one char at a time: */
    toget = 1;
  }

  while (!th->eof && !th->err) {
    /* Wait until we're supposed to look for input: */
    WaitForSingleObject(th->checking_sema, INFINITE);

    if (th->checking < 0)
      break;

    if (ReadFile(th->fd, th->buffer, toget, &got, NULL)) {
      th->avail = got;
      if (!got)
	th->eof = 1;
    } else {
      int err;
      err = GetLastError();
      if (err == ERROR_BROKEN_PIPE)
	th->eof = 1;
      else
	th->err = err;
    }

    /* Notify main program that we found something: */
    ReleaseSemaphore(th->ready_sema, 1, NULL);
  }

  /* We have to clean up if the main program has abandoned us: */
  if (WaitForSingleObject(th->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
    WindowsFDICleanup(th);
  } /* otherwise, main program is responsible for clean-up */

  return 0;
}

static void WindowsFDICleanup(Win_FD_Input_Thread *th)
{
  CloseHandle(th->checking_sema);
  CloseHandle(th->ready_sema);
  CloseHandle(th->you_clean_up_sema);
  free(th->buffer);
  free(th);
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

# endif

#endif

Scheme_Object *
scheme_make_fd_input_port(int fd, Scheme_Object *name, int regfile, int textmode)
{
#ifdef MZ_FDS
  return make_fd_input_port(fd, name, regfile, textmode, NULL);
#else
  return NULL;
#endif
}

/*========================================================================*/
/*                    OSKit console input ports                           */
/*========================================================================*/

#ifdef USE_OSKIT_CONSOLE

# ifdef OSKIT_TEST
static Scheme_Object *normal_stdin;
static int direct_cons_trygetchar() { return scheme_byte_ready(normal_stdin) ? scheme_get_byte(normal_stdin) : -1; }
static void direct_cons_putchar(int c) { }
# define convert_scan_code(x) x
# else
#  include "pc_keys.inc"
# endif

typedef struct osk_console_input {
  MZTAG_IF_REQUIRED
  int count, size, ready;
  unsigned char *buffer;
  struct osk_console_input *next; /* typeahead */
} osk_console_input;

static int
osk_byte_ready (Scheme_Input_Port *port)
{
  osk_console_input *osk, *orig;
  int k;

  if (port->closed)
    return 1;

  osk = orig = (osk_console_input *)port->port_data;

  while (osk->ready) {
    if (osk->next)
      osk = osk->next;
    else {
      osk->next = MALLOC_ONE_RT(osk_console_input);
#ifdef MZTAG_REQUIRED
      osk->type = scheme_rt_oskit_console_input;
#endif
      osk = osk->next;
      osk->count = osk->size = osk->ready = 0;
      osk->buffer = NULL;
      osk->next = NULL;
    }
  }

  k = direct_cons_trygetchar();
  k = convert_scan_code(k); /* defined in pc_keys.inc; handles ctl-alt-del */
  if (k > 0) {
    if (k == 3) { /* Ctl-C */
      scheme_break_thread(NULL);
    } else if (k == 4) { /* Ctl-D */
      if (!osk->count)
	/* ready with !count => EOF */
	osk->ready = 1;
    } else if (k == 8) { /* Backspace */
      if (osk->count) {
	direct_cons_putchar(8);
	direct_cons_putchar(' '); /* space erases old letter */
	direct_cons_putchar(8);
	--osk->count;
      }
    } else {
      if (osk->count == osk->size) {
	char *naya;
	osk->size = osk->size ? 2 * osk->size : 256;
	naya = scheme_malloc_atomic(osk->size);
	memcpy(naya, osk->buffer, osk->count);
	osk->buffer = naya;
      }
      osk->buffer[osk->count++] = k;
      if (k == 13 || k == 10) { /* Return/newline */
	direct_cons_putchar(13);
	direct_cons_putchar(10);
	osk->ready = 1;
      } else
	direct_cons_putchar(k);
    }
  }

  if (orig->ready)
    return 1;
  else
    return 0;
}

static int osk_get_string(Scheme_Input_Port *port,
			  char *buffer, int offset, int size,
			  int nonblock, Scheme_Object *unless)
{
  int c;
  osk_console_input *osk;

  while (!osk_byte_ready(port)) {
    if (nonblock > 0) {
      return 0;
    }
    
    scheme_block_until_unless(osk_byte_ready, NULL, (Scheme_Object *)port, 0.0,
			      unless,
			      nonblock);

    scheme_wait_input_allowed(port, nonblock);
    
    if (scheme_unless_ready(unless))
      return SCHEME_UNLESS_READY;
  }

  if (port->closed) {
    /* Another thread closed the input port while we were waiting. */
    /* Call scheme_getc to signal the error */
    scheme_getc((Scheme_Object *)port);
  }

  osk = (osk_console_input *)port->port_data;

  if (!osk->count) {
    /* EOF */
    osk->ready = 0;
    return EOF;
  }

  c = osk->buffer[osk->ready - 1];
  osk->ready++;
  if (osk->ready > osk->count) {
    if (osk->next) {
      /* Copy typeahead to here */
      osk_console_input *next = osk->next;
      memcpy(osk, next, sizeof(osk_console_input));
    } else
      osk->ready = osk->count = 0;
  }

  buffer[offset] = c;
  return 1;
}

static void
osk_close_input(Scheme_Input_Port *port)
{
}

static void
osk_need_wakeup(Scheme_Input_Port *port, void *fds)
{
# ifdef OSKIT_TEST
  /* for testing, write to stdout is almost certainly ready: */
  void *fdw;
  fdw = MZ_GET_FDSET(fds, 1);
  MZ_FD_SET(1, (fd_set *)fdw);
# endif

  /* In OSKit, makes select() return immediately */
  MZ_FD_SET(0, (fd_set *)fds);
}

static Scheme_Object *
make_oskit_console_input_port()
{
  Scheme_Input_Port *ip;
  osk_console_input *osk;

  osk = MALLOC_ONE_RT(osk_console_input);
#ifdef MZTAG_REQUIRED
  osk->type = scheme_rt_oskit_console_input;
#endif

  osk->count = osk->size = osk->ready = 0;
  osk->buffer = NULL;
  osk->next = NULL;

# ifdef OSKIT_TEST
  REGISTER_SO(normal_stdin);
  normal_stdin = scheme_make_named_file_input_port(stdin, scheme_intern_symbol("stdin"));
# endif

  ip = scheme_make_input_port(oskit_console_input_port_type,
			      osk,
			      scheme_intern_symbol("stdin"),
			      osk_get_string,
			      NULL,
			      scheme_progress_evt_via_get,
			      scheme_get_string_unless_via_get,
			      osk_byte_ready,
			      osk_close_input,
			      osk_need_wakeup,
			      1);

  return (Scheme_Object *)ip;
}

void scheme_check_keyboard_input(void)
{
  if (!osk_not_console)
    osk_byte_ready((Scheme_Input_Port *)scheme_orig_stdin_port);
}

#endif

/*========================================================================*/
/*                           FILE output ports                            */
/*========================================================================*/

/* Note that we don't try to implement non-blocking writes on FILE
   objects. In Unix, a program could conceiveably open a named pipe
   and block on it. */

static void file_flush(Scheme_Output_Port *port)
{
  if (fflush(((Scheme_Output_File *)port->port_data)->f)) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "error flushing file port (%e)",
		     errno);
  }
}

static long
file_write_string(Scheme_Output_Port *port,
		  const char *str, long d, long llen,
		  int rarely_block, int enable_break)
{
  FILE *fp;
  long len = llen;

  fp = ((Scheme_Output_File *)port->port_data)->f;

  if (!len) {
    file_flush(port);
    return 0;
  }

  if (fwrite(str XFORM_OK_PLUS d, len, 1, fp) != 1) {
    scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
		     "error writing to file port (%e)",
		     errno);
    return 0;
  }

  if (rarely_block) {
    file_flush(port);
  } else {
    while (len--) {
      if (str[d] == '\n' || str[d] == '\r') {
	file_flush(port);
	break;
      }
      d++;
    }
  }

  return llen;
}

static void
file_close_output(Scheme_Output_Port *port)
{
  Scheme_Output_File *fop = (Scheme_Output_File *)port->port_data;
  FILE *fp = fop->f;

  fclose(fp);
  --scheme_file_open_count;
}

Scheme_Object *
scheme_make_file_output_port(FILE *fp)
{
  Scheme_Output_File *fop;
  Scheme_Output_Port *op;

  if (!fp)
    scheme_signal_error("make-file-out-port(internal): "
			"null file pointer");

  fop = MALLOC_ONE_RT(Scheme_Output_File);
#ifdef MZTAG_REQUIRED
  fop->type = scheme_rt_output_file;
#endif

  fop->f = fp;

  op = scheme_make_output_port(file_output_port_type,
			       fop,
			       scheme_intern_symbol("file"),
			       scheme_write_evt_via_write,
			       file_write_string,
			       NULL,
			       file_close_output,
			       NULL,
			       NULL,
			       NULL,
			       1);
  op->p.buffer_mode_fun = file_buffer_mode;

  return (Scheme_Object *)op;
}

/*========================================================================*/
/*                             fd output ports                            */
/*========================================================================*/

#ifdef MZ_FDS

#ifdef WINDOWS_FILE_HANDLES
static long WindowsFDWriter(Win_FD_Output_Thread *oth);
static void WindowsFDOCleanup(Win_FD_Output_Thread *oth);
#endif

static int
fd_flush_done(Scheme_Object *port)
{
  Scheme_FD *fop;

  fop = (Scheme_FD *)((Scheme_Output_Port *)port)->port_data;

  return !fop->flushing;
}

static void wait_until_fd_flushed(Scheme_Output_Port *op, int enable_break)
{
  scheme_block_until_enable_break(fd_flush_done, NULL, (Scheme_Object *)op, 
				  0.0, enable_break);
}

#ifdef WINDOWS_FILE_HANDLES
static int win_fd_flush_done(Scheme_Object *_oth)
{
  /* For checking whether the output thread has finished a flush. */

  Win_FD_Output_Thread *oth = (Win_FD_Output_Thread *)_oth;
  int done;

  WaitForSingleObject(oth->lock_sema, INFINITE);
  if (oth->nonblocking) {
    if (oth->needflush) {
      oth->needflush = 0;
      oth->flushed = 0;
      ReleaseSemaphore(oth->work_sema, 1, NULL); /* start trying to flush */
      done = 0;
    } else
      done = oth->flushed;
  } else
    done = (oth->err_no || !oth->buflen);
  ReleaseSemaphore(oth->lock_sema, 1, NULL);

  return done;
}

static void win_fd_flush_needs_wakeup(Scheme_Object *_oth, void *fds)
{
  /* For sleping until the output thread has finished a flush. */

  /* Double-check that we're not already done: */
  if (win_fd_flush_done(_oth))
    scheme_add_fd_nosleep(fds);
  else {
    /* Not done. Thread will notify us through ready_sema: */
    Win_FD_Output_Thread *oth = (Win_FD_Output_Thread *)_oth;

    scheme_add_fd_handle(oth->ready_sema, fds, 1);
  }
}
#endif

static int
fd_write_ready (Scheme_Object *port)
{
  /* As always, the result of this function is only meaningful when
     the port has been flushed. */

  Scheme_FD *fop;

  fop = (Scheme_FD *)((Scheme_Output_Port *)port)->port_data;

  if (fop->regfile || ((Scheme_Output_Port *)port)->closed)
    return 1;

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth) {
    /* Pipe output that can block... */
    int retval;
    Win_FD_Output_Thread *oth = fop->oth;

    WaitForSingleObject(oth->lock_sema, INFINITE);
    if (oth->nonblocking) {
      if (oth->needflush) {
	oth->needflush = 0;
	oth->flushed = 0;
	ReleaseSemaphore(oth->work_sema, 1, NULL); /* start trying to flush */
	retval = 0;
      } else
	retval = oth->flushed;
    } else
      retval = (oth->err_no || (oth->buflen < MZPORT_FD_BUFFSIZE));
    if (!retval)
      WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
    ReleaseSemaphore(oth->lock_sema, 1, NULL);

    return retval;
  } else
    return 1; /* non-blocking output, such as a console, or haven't written yet */
#else
# ifdef MAC_FILE_HANDLES
  return 1;
# else
  {
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
    int sr;

    INIT_DECL_FDSET(writefds, 1);
    INIT_DECL_FDSET(exnfds, 1);

    MZ_FD_ZERO(writefds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(fop->fd, writefds);
    MZ_FD_SET(fop->fd, exnfds);

    do {
      sr = select(fop->fd + 1, NULL, writefds, exnfds, &time);
    } while ((sr == -1) && (errno == EINTR));

    return sr;
  }
# endif
#endif
}


static void
fd_write_need_wakeup(Scheme_Object *port, void *fds)
{
  Scheme_FD *fop;

#ifdef WINDOWS_FILE_HANDLES
#else
# ifdef MAC_FILE_HANDLES
# else
  void *fds2;
  int n;
# endif
#endif

  fop = (Scheme_FD *)((Scheme_Output_Port *)port)->port_data;

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth && !fd_write_ready(port))
    scheme_add_fd_handle(fop->oth->ready_sema, fds, 1);
  else
    scheme_add_fd_nosleep(fds);
#else
# ifdef MAC_FILE_HANDLES
# else
  n = fop->fd;
  fds2 = MZ_GET_FDSET(fds, 1);
  MZ_FD_SET(n, (fd_set *)fds2);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(n, (fd_set *)fds2);
# endif
#endif
}

static void release_flushing_lock(void *_fop)
{
  Scheme_FD *fop;

  fop = (Scheme_FD *)_fop;

  fop->flushing = 0;
}

static long flush_fd(Scheme_Output_Port *op,
		     const char * volatile bufstr, volatile unsigned long buflen, volatile unsigned long offset,
		     int immediate_only, int enable_break)
     /* immediate_only == 1 => write at least one character, then give up;
	immediate_only == 2 => never block */
{
  Scheme_FD * volatile fop = (Scheme_FD *)op->port_data;
  volatile long wrote = 0;

  if (fop->flushing) {
    if (scheme_force_port_closed) {
      /* Give up */
      return 0;
    }

    if (immediate_only == 2)
      return 0;

    wait_until_fd_flushed(op, enable_break);

    if (op->closed)
      return 0;
  }

  if (!bufstr) {
    bufstr = (char *)fop->buffer;
    buflen = fop->bufcount;
  }

  if (buflen) {
    fop->flushing = 1;
    fop->bufcount = 0;
    /* If write is interrupted, we drop chars on the floor.
       Not ideal, but we'll go with it for now.
       Note that write_string_avail supports break-reliable
       output through `immediate_only'. */

    while (1) {
      long len;
      int errsaved, full_write_buffer;

#ifdef WINDOWS_FILE_HANDLES
      DWORD winwrote;

      full_write_buffer = 0;

      if (fop->regfile) {
	/* Regular files never block, so this code looks like the Unix
	   code.  We've cheated in the make_fd proc and called
	   FILE_TYPE_CHAR devices (e.g., console) regular files,
	   because they cannot block, either. */
	int orig_len;

	if (fop->textmode) {
	  /* Convert LF to CRLF. We're relying on the fact that WriteFile
	     will write everything. */
	  int c = 0;
	  unsigned int i;

	  for (i = offset; i < buflen; i++) {
	    if (bufstr[i] == '\n')
	      c++;
	  }

	  orig_len = buflen - offset;

	  if (c) {
	    char *naya;
	    int j;

	    naya = scheme_malloc_atomic(orig_len + c);

	    for (i = offset, j = 0; i < buflen; i++) {
	      if (bufstr[i] == '\n') {
		naya[j++] = '\r';
		naya[j++] = '\n';
	      } else
		naya[j++] = bufstr[i];
	    }

	    bufstr = naya;
	    offset = 0;
	    buflen = orig_len + c;
	  }
	} else
	  orig_len = 0; /* not used */

	/* Write bytes. If we try to write too much at once, the result
	   is ERROR_NOT_ENOUGH_MEMORY (as opposed to a partial write). */
	{
	  int ok;
	  long towrite = buflen - offset;

	  while (1) {
	    ok = WriteFile((HANDLE)fop->fd, bufstr XFORM_OK_PLUS offset, towrite, &winwrote, NULL);
	    if (!ok)
	      errsaved = GetLastError();
	    
	    if (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY)) {
	      towrite = towrite >> 1;
	      if (!towrite)
		break;
	    } else
	      break;
	  }

	  if (ok) {
	    if (fop->textmode) {
	      if (winwrote != buflen) {
		/* Trouble! This shouldn't happen. We pick an random error msg. */
		errsaved = ERROR_NEGATIVE_SEEK;
		len = -1;
	      } else {
		len = orig_len;
		buflen = orig_len; /* so we don't loop! */
	      }
	    } else
	      len = winwrote;
	  } else {
	    len = -1;
	  }
	}
      } else {
	errsaved = 0;
	len = -1;

	/* If we don't have a thread yet, we'll need to start it. If
	   we have a non-blocking pipe, we can try the write (and
	   we'll still need the thread to determine when the data is
	   flushed). */
	if (!fop->oth || fop->oth->nonblocking) {
	  int nonblocking;

	  /* If we don't have a thread, this is our first write attempt.
	     Determine whether this is a non-blocking pipe: */
	  if (!fop->oth) {
	    /* The FILE_TYPE_PIPE test is currently redundant, I think,
	       but better safe than sorry. */
	    nonblocking = ((scheme_stupid_windows_machine < 0)
			   && (GetFileType((HANDLE)fop->fd) == FILE_TYPE_PIPE));
	  } else
	    nonblocking = 1; /* must be, or we would not have gotten here */

	  if (nonblocking) {
	    /* Unless we're still trying to flush old data, write to the
	       pipe and have the other thread start flushing it. */
	    DWORD old, nonblock = PIPE_NOWAIT;
	    int ok, flushed;

	    if (fop->oth) {
	      if (fop->oth->needflush) {
		/* Not flushed, but we haven't promised not to block: */
		flushed = 1;
	      } else {
		WaitForSingleObject(fop->oth->lock_sema, INFINITE);
		flushed = fop->oth->flushed;
		ReleaseSemaphore(fop->oth->lock_sema, 1, NULL);
	      }
	    } else
	      flushed = 1; /* haven't written anything before */

	    if (flushed) {
	      /* Put the pipe in non-blocking mode and write. */

	      int towrite;

	      towrite = buflen - offset;

	      /* Apparently, the semantics of non-blocking pipe writes
	         is not partial writes, but giving up entirely when
	         the other end isn't being read. In other words, if we
	         try to write too much and nothing is being pulled
	         from the pipe, winwrote will be set to 0. Also, if
		 we try to write too much at once, the result is a
		 ERROR_NOT_ENOUGH_MEMORY error. Account for these
	         behaviors by trying to write less each iteration when the
	         write fails. (Yuck.) */
	      while (1) {
		GetNamedPipeHandleState((HANDLE)fop->fd, &old, NULL, NULL, NULL, NULL, 0);
		SetNamedPipeHandleState((HANDLE)fop->fd, &nonblock, NULL, NULL);
		ok = WriteFile((HANDLE)fop->fd, bufstr XFORM_OK_PLUS offset, towrite, &winwrote, NULL);
		if (!ok)
		  errsaved = GetLastError();
		SetNamedPipeHandleState((HANDLE)fop->fd, &old, NULL, NULL);

		if ((ok && !winwrote)
		    || (!ok && (errsaved == ERROR_NOT_ENOUGH_MEMORY))) {
		  towrite = towrite >> 1;
		  if (!towrite) {
		    break;
		  }
		} else
		  break;
	      }
	    } else {
	      /* Don't try to write while flushing. */
	      ok = 1;
	      winwrote = 0;
	    }

	    if (ok) {
	      if (!winwrote) {
		full_write_buffer = 1;
	      } else {
		len = winwrote;
	      }
	    }
	  } else
	    full_write_buffer = 0; /* and create the writer thread... */

	  if (!fop->oth) {
	    /* We create a thread even for pipes that can be put in
	       non-blocking mode, because that seems to be the only
	       way to get evt behavior. */
	    Win_FD_Output_Thread *oth;
	    HANDLE h;
	    DWORD id;
	    unsigned char *bfr;
	    OS_SEMAPHORE_TYPE sm;

	    oth = malloc(sizeof(Win_FD_Output_Thread));
	    fop->oth = oth;

	    oth->nonblocking = nonblocking;

	    if (!nonblocking) {
	      bfr = (unsigned char *)malloc(MZPORT_FD_BUFFSIZE);
	      oth->buffer = bfr;
	      oth->flushed = 0;
	      oth->needflush = 0;
	    } else {
	      oth->buffer = NULL;
	      oth->flushed = (len <= 0);
	      oth->needflush = 1;
	    }

	    oth->buflen = 0;
	    oth->bufstart = 0;
	    oth->bufend = 0;

	    oth->fd = (HANDLE)fop->fd;
	    oth->err_no = 0;
	    oth->done = 0;
	    sm = CreateSemaphore(NULL, 1, 1, NULL);
	    oth->lock_sema = sm;
	    sm = CreateSemaphore(NULL, 0, 1, NULL);
	    oth->work_sema = sm;
	    sm = CreateSemaphore(NULL, 1, 1, NULL);
	    oth->ready_sema = sm;
	    sm = CreateSemaphore(NULL, 1, 1, NULL);
	    oth->you_clean_up_sema = sm;

	    h = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)WindowsFDWriter, oth, 0, &id);

	    scheme_remember_thread(h, 1);
	  }
	}

	/* We have a thread, if only to watch when the flush is
	   done... */

	if (!fop->oth->nonblocking) {
	  /* This case is only for Win 95/98/Me anonymous pipes.  We
	     haven't written anything yet! We write to a buffer read
	     by the other thread, and return -- the other thread takes
	     care of writing. Thus, as long as there's room in the
	     buffer, we don't block, and we can tell whether there's
	     room. Technical problem: if multiple ports are attched to
	     the same underlying pipe (different handle, same
	     "device"), the port writes can get out of order. We try
	     to avoid the problem by sleeping --- it's only Win
	     95/98/Me, after all. */

	  Win_FD_Output_Thread *oth = fop->oth;

	  WaitForSingleObject(oth->lock_sema, INFINITE);
	  if (oth->err_no)
	    errsaved = oth->err_no;
	  else if (oth->buflen == MZPORT_FD_BUFFSIZE) {
	    full_write_buffer = 1;
	    WaitForSingleObject(oth->ready_sema, 0); /* clear any leftover state */
	  } else {
	    long topp;
	    int was_pre;

	    if (!oth->buflen) {
	      /* Avoid fragmenting in circular buffer: */
	      oth->bufstart = 0;
	      oth->bufend = 0;
	    }

	    /* Write to top part of circular buffer, then bottom part
	       if anything's left. */

	    if (oth->bufstart <= oth->bufend) {
	      was_pre = 1;
	      topp = MZPORT_FD_BUFFSIZE;
	    } else {
	      was_pre = 0;
	      topp = oth->bufstart;
	    }

	    winwrote = topp - oth->bufend;
	    if (winwrote > buflen - offset)
	      winwrote = buflen - offset;

	    memcpy(oth->buffer + oth->bufend, bufstr + offset, winwrote);
	    oth->buflen += winwrote;
	    len = winwrote;

	    oth->bufend += winwrote;
	    if (oth->bufend == MZPORT_FD_BUFFSIZE)
	      oth->bufend = 0;

	    if (was_pre) {
	      if (winwrote < buflen - offset) {
		/* Try continuing with a wrap-around: */
		winwrote = oth->bufstart - oth->bufend;
		if (winwrote > buflen - offset - len)
		  winwrote = buflen - offset - len;

		memcpy(oth->buffer + oth->bufend, bufstr + offset + len, winwrote);
		oth->buflen += winwrote;
		oth->bufend += winwrote;
		len += winwrote;
	      }
	    }
	    /* Let the other thread know that it should start trying
	       to write, if it isn't already: */
	    ReleaseSemaphore(oth->work_sema, 1, NULL);
	    Sleep(0); /* to decrease the chance of re-ordering flushes */
	  }
	  ReleaseSemaphore(oth->lock_sema, 1, NULL);
	} else if (len > 0) {
	  /* We've already written, which implies that no flush is
	     in progress. We'll need a flush check in the future. */
	  fop->oth->needflush = 1;
	}
      }
#else
# ifdef MAC_FILE_HANDLES
      {
	SInt32 put = buflen - offset;
	errsaved = FSWrite(fop->fd, &put, bufstr + offset);
	if (errsaved != noErr)
	  len = -1;
	else
	  len = put;
	full_write_buffer = 0;
      }
# else
      int flags;

      flags = fcntl(fop->fd, F_GETFL, 0);
      fcntl(fop->fd, F_SETFL, flags | MZ_NONBLOCKING);

      do {
	len = write(fop->fd, bufstr + offset, buflen - offset);
      } while ((len == -1) && (errno == EINTR));

      errsaved = errno;
      fcntl(fop->fd, F_SETFL, flags);

      full_write_buffer = (errsaved == EAGAIN);
# endif
#endif

      if (len < 0) {
	if (scheme_force_port_closed) {
	  /* Don't signal exn or wait. Just give up. */
	  return wrote;
	} else if (full_write_buffer) {
	  /* Need to block; remember that we're holding a lock. */
	  if (immediate_only == 2) {
	    fop->flushing = 0;
	    return wrote;
	  }

	  BEGIN_ESCAPEABLE(release_flushing_lock, fop);
	  scheme_block_until_enable_break(fd_write_ready,
					  fd_write_need_wakeup,
					  (Scheme_Object *)op, 0.0,
					  enable_break);
	  END_ESCAPEABLE();
	} else {
	  fop->flushing = 0;
	  scheme_raise_exn(MZEXN_FAIL_FILESYSTEM,
			   "error writing to stream port (" FILENAME_EXN_E ")",
			   errsaved);
	  return 0; /* doesn't get here */
	}
      } else if ((len + offset == buflen) || immediate_only) {
	fop->flushing = 0;
	return wrote + len;
      } else {
	offset += len;
	wrote += len;
      }
    }
  }

  return wrote;
}

static long
fd_write_string(Scheme_Output_Port *port,
		const char *str, long d, long len,
		int rarely_block, int enable_break)
{
  /* Note: !flush => !rarely_block, !len => flush */

  Scheme_FD *fop;
  long l;
  int flush = (!len || rarely_block);

  fop = (Scheme_FD *)port->port_data;

  if (!len) {
    if (fop->bufcount)
      flush_fd(port, NULL, 0, 0, rarely_block, enable_break);

    if (fop->bufcount)
      return -1;
    else
      return 0;
  }

  if (!fop->bufcount && flush) {
    /* Nothing buffered. Write directly. */
    return flush_fd(port, str, d + len, d, rarely_block, enable_break);
  }

  if (fop->flushing) {
    if (rarely_block == 2)
      return -1; /* -1 means 0 written && still have unflushed */
    wait_until_fd_flushed(port, enable_break);
  }

  /* Might have been closed while we waited */
  if (port->closed)
    return 0;

  l = MZPORT_FD_BUFFSIZE - fop->bufcount;
  if ((len <= l) && (!flush || !rarely_block)) {
    memcpy(fop->buffer + fop->bufcount, str + d, len);
    fop->bufcount += len;
  } else {
    if (fop->bufcount) {
      flush_fd(port, NULL, 0, 0, (rarely_block == 2) ? 2 : 0, enable_break);
      if (rarely_block && fop->bufcount)
	return -1; /* -1 means 0 written && still have unflushed */
    }

    if (!flush && (len <= MZPORT_FD_BUFFSIZE)) {
      memcpy(fop->buffer, str + d, len);
      fop->bufcount = len;
    } else
      return flush_fd(port, str, len + d, d, rarely_block, enable_break);
  }

  /* If we got this far, !rarely_block. */

  if ((flush || (fop->flush == MZ_FLUSH_ALWAYS)) && fop->bufcount) {
    flush_fd(port, NULL, 0, 0, 0, enable_break);
  } else if (fop->flush == MZ_FLUSH_BY_LINE) {
    long i;

    for (i = len; i--; ) {
      if (str[d] == '\n' || str[d] == '\r') {
	flush_fd(port, NULL, 0, 0, 0, enable_break);
	break;
      }
      d++;
    }
  }

  return len;
}

static void
fd_close_output(Scheme_Output_Port *port)
{
  Scheme_FD *fop = (Scheme_FD *)port->port_data;

  if (fop->bufcount)
    flush_fd(port, NULL, 0, 0, 0, 0);

  if (fop->flushing && !scheme_force_port_closed)
    wait_until_fd_flushed(port, 0);

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth) {
    if (!scheme_force_port_closed) {
      /* If there's a work thread, wait until the port
	 is *really* flushed! */
      scheme_block_until(win_fd_flush_done, win_fd_flush_needs_wakeup, (Scheme_Object *)fop->oth, 0.0);
    }
  }
#endif

  /* Make sure no close happened while we blocked above! */
  if (port->closed)
    return;

  if (fop->refcount)
    *fop->refcount -= 1;

#ifdef WINDOWS_FILE_HANDLES
  if (fop->oth) {
    fop->oth->done = 1;
    ReleaseSemaphore(fop->oth->work_sema, 1, NULL);

    /* Try to leave clean-up to the other thread: */
    if (WaitForSingleObject(fop->oth->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
      /* Other thread is already done, so we're stuck with clean-up: */
      WindowsFDOCleanup(fop->oth);
    } /* otherwise, thread is responsible for clean-up */
    fop->oth = NULL;
  }
  if (!fop->refcount || !*fop->refcount) {
    CloseHandle((HANDLE)fop->fd);
  }
#else
  if (!fop->refcount || !*fop->refcount) {
# ifdef MAC_FILE_HANDLES
    FSClose(fop->fd);
# else
    {
      int cr;
      do {
	cr = close(fop->fd);
      } while ((cr == -1) && (errno == EINTR));
    }
# endif
  }
#endif

  --scheme_file_open_count;
}

static int fd_output_buffer_mode(Scheme_Port *p, int mode)
{
  Scheme_FD *fd;
    
  fd = (Scheme_FD *)((Scheme_Output_Port *)p)->port_data;
  
  if (mode < 0) {
    return fd->flush;
  } else {
    int go;
    go = (mode > fd->flush);
    fd->flush = mode;
    if (go)
      flush_fd((Scheme_Output_Port *)p, NULL, 0, 0, 0, 0);
    return mode;
  }
}

static Scheme_Object *
make_fd_output_port(int fd, Scheme_Object *name, int regfile, int win_textmode, int and_read)
{
  Scheme_FD *fop;
  unsigned char *bfr;
  Scheme_Object *the_port;

  fop = MALLOC_ONE_RT(Scheme_FD);
#ifdef MZTAG_REQUIRED
  fop->type = scheme_rt_input_fd;
#endif

  bfr = (unsigned char *)scheme_malloc_atomic(MZPORT_FD_BUFFSIZE);
  fop->buffer = bfr;

  fop->fd = fd;
  fop->bufcount = 0;

#ifdef WINDOWS_FILE_HANDLES
  /* Character devices can't block output, right? */
  if (GetFileType((HANDLE)fop->fd) == FILE_TYPE_CHAR)
    regfile = 1;
  /* The work thread is created on demand in fd_flush. */
#endif

  fop->regfile = regfile;
  fop->textmode = win_textmode;

  if (fd == 2) {
    /* No buffering for stderr: */
    fop->flush = MZ_FLUSH_ALWAYS;
  } else if (is_fd_terminal(fd)) {
    /* Line-buffering for terminal: */
    fop->flush = MZ_FLUSH_BY_LINE;
  } else {
    /* Block-buffering for everything else: */
    fop->flush = MZ_FLUSH_NEVER;
  }

  the_port = (Scheme_Object *)scheme_make_output_port(fd_output_port_type,
						      fop,
						      name,
						      scheme_write_evt_via_write,
						      fd_write_string,
						      (Scheme_Out_Ready_Fun)fd_write_ready,
						      fd_close_output,
						      (Scheme_Need_Wakeup_Output_Fun)fd_write_need_wakeup,
						      NULL,
						      NULL,
						      1);
  ((Scheme_Port *)the_port)->buffer_mode_fun = fd_output_buffer_mode;

  if (and_read) {
    int *rc;
    Scheme_Object *a[2];
    rc = (int *)scheme_malloc_atomic(sizeof(int));
    *rc = 2;
    fop->refcount = rc;
    a[1] = the_port;
    a[0] = make_fd_input_port(fd, name, regfile, win_textmode, rc);
    return scheme_values(2, a);
  } else
    return the_port;
}

static void flush_if_output_fds(Scheme_Object *o, Scheme_Close_Custodian_Client *f, void *data)
{
  if (SCHEME_OUTPORTP(o)) {
    Scheme_Output_Port *op = (Scheme_Output_Port *)o;
    if (SAME_OBJ(op->sub_type, fd_output_port_type)) {
      scheme_flush_output(o);
    }
  }
}

#ifdef WINDOWS_FILE_HANDLES

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

static long WindowsFDWriter(Win_FD_Output_Thread *oth)
{
  DWORD towrite, wrote, start;
  int ok, more_work = 0, err_no;

  if (oth->nonblocking) {
    /* Non-blocking mode (Win NT pipes). Just flush. */
    while (!oth->done) {
      WaitForSingleObject(oth->work_sema, INFINITE);

      FlushFileBuffers(oth->fd);

      WaitForSingleObject(oth->lock_sema, INFINITE);
      oth->flushed = 1;
      ReleaseSemaphore(oth->ready_sema, 1, NULL);
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }
  } else {
    /* Blocking mode. We do the writing work.  This case is only for
       Win 95/98/Me anonymous pipes. */
    while (!oth->err_no) {
      if (!more_work)
	WaitForSingleObject(oth->work_sema, INFINITE);

      if (oth->done)
	break;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      towrite = oth->buflen;
      if (towrite > (MZPORT_FD_BUFFSIZE - oth->bufstart))
	towrite = MZPORT_FD_BUFFSIZE - oth->bufstart;
      start = oth->bufstart;
      ReleaseSemaphore(oth->lock_sema, 1, NULL);

      ok = WriteFile(oth->fd, oth->buffer + start, towrite, &wrote, NULL);
      if (!ok)
	err_no = GetLastError();
      else
	err_no = 0;

      WaitForSingleObject(oth->lock_sema, INFINITE);
      if (!ok)
	oth->err_no = err_no;
      else {
	oth->bufstart += wrote;
	oth->buflen -= wrote;
	if (oth->bufstart == MZPORT_FD_BUFFSIZE)
	  oth->bufstart = 0;
	more_work = oth->buflen > 0;
      }
      if ((oth->buflen < MZPORT_FD_BUFFSIZE) || oth->err_no)
	ReleaseSemaphore(oth->ready_sema, 1, NULL);
      ReleaseSemaphore(oth->lock_sema, 1, NULL);
    }
  }
  if (WaitForSingleObject(oth->you_clean_up_sema, 0) != WAIT_OBJECT_0) {
    WindowsFDOCleanup(oth);
  } /* otherwise, main thread is responsible for clean-up */

  return 0;
}

static void WindowsFDOCleanup(Win_FD_Output_Thread *oth)
{
  CloseHandle(oth->lock_sema);
  CloseHandle(oth->work_sema);
  CloseHandle(oth->you_clean_up_sema);
  if (oth->buffer)
    free(oth->buffer);
  free(oth);
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

#endif

#endif

Scheme_Object *
scheme_make_fd_output_port(int fd, Scheme_Object *name, int regfile, int textmode, int read_too)
{
#ifdef MZ_FDS
  return make_fd_output_port(fd, name, regfile, textmode, read_too);
#else
  return NULL;
#endif
}

/*========================================================================*/
/*                        system/process/execute                          */
/*========================================================================*/

/* Unix, and Windows support --- all mixed together */

#define MZ_FAILURE_STATUS -1

#ifdef PROCESS_FUNCTION

# define USE_CREATE_PIPE

#ifdef WINDOWS_PROCESSES
# ifdef USE_CREATE_PIPE
#  define _EXTRA_PIPE_ARGS
static int MyPipe(int *ph, int near_index) {
  HANDLE r, w;
  SECURITY_ATTRIBUTES saAttr;

  /* Set the bInheritHandle flag so pipe handles are inherited. */
  saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
  saAttr.bInheritHandle = TRUE;
  saAttr.lpSecurityDescriptor = NULL;

  if (CreatePipe(&r, &w, &saAttr, 0)) {
    HANDLE a[2], naya;

    a[0] = r;
    a[1] = w;

    /* Change the near end to make it non-inheritable, then
       close the inheritable one: */
    if (!DuplicateHandle(GetCurrentProcess(), a[near_index],
			 GetCurrentProcess(), &naya, 0,
			 0, /* not inherited */
			 DUPLICATE_SAME_ACCESS)) {
      CloseHandle(a[0]);
      CloseHandle(a[1]);
      return 1;
    } else {
      CloseHandle(a[near_index]);
      a[near_index] = naya;
    }

    ph[0] = (long)a[0];
    ph[1] = (long)a[1];

    return 0;
  } else
    return 1;
}
#  define PIPE_FUNC MyPipe
# else
#  include <Process.h>
#  include <fcntl.h>
# define PIPE_FUNC(pa, nearh) MSC_IZE(pipe)(pa)
#  define _EXTRA_PIPE_ARGS , 256, _O_BINARY
# endif
#else
# define _EXTRA_PIPE_ARGS
# define PIPE_FUNC(pa, nearh) MSC_IZE(pipe)(pa)
#endif

#endif

/**************** Unix: signal stuff ******************/

#if defined(UNIX_PROCESSES)

# define WAITANY(s) waitpid((pid_t)-1, s, WNOHANG)

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

void scheme_block_child_signals(int block)
{
  sigset_t sigs;

  sigemptyset(&sigs);
  sigaddset(&sigs, SIGCHLD);
#ifdef USE_ITIMER
  sigaddset(&sigs, SIGPROF);
#endif
  sigprocmask(block ? SIG_BLOCK : SIG_UNBLOCK, &sigs, NULL);
}

static void child_done(int ingored)
{
  pid_t result;
  int status;
  System_Child *sc, *prev;

  do {
    do {
      result = WAITANY(&status);
    } while ((result == -1) && (errno == EINTR));

    if (result > 0) {
      if (WIFEXITED(status))
	status = WEXITSTATUS(status);
      else
	status = MZ_FAILURE_STATUS;

      prev = NULL;
      for (sc = scheme_system_children; sc; prev = sc, sc = sc->next) {
	if (sc->id == result) {
	  sc->done = 1;
	  sc->status = status;

	  if (prev)
	    prev->next = sc->next;
	  else
	    scheme_system_children = sc->next;

	  scheme_signal_received();
	  break;
	}
      }
    }
  } while (result > 0);

# ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGCHLD, child_done);
# endif
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

static int sigchld_installed = 0;

static void init_sigchld(void)
{
  if (!sigchld_installed) {
    /* Catch child-done signals */
    START_XFORM_SKIP;
    MZ_SIGSET(SIGCHLD, child_done);
    END_XFORM_SKIP;

    sigchld_installed = 1;
  }
}

#endif

/*========================================================================*/
/*                           null output ports                            */
/*========================================================================*/

static long
null_write_bytes(Scheme_Output_Port *port,
		 const char *str, long d, long len,
		 int rarely_block, int enable_break)
{
  return len;
}

static void
null_close_out (Scheme_Output_Port *port)
{
}

static Scheme_Object *
null_write_evt(Scheme_Output_Port *op, const char *str, long offset, long size)
{
  Scheme_Object *a[2];
  a[0] = scheme_always_ready_evt;
  a[1] = scheme_make_closed_prim(return_data, scheme_make_integer(size));
  return scheme_wrap_evt(2, a);
}

static Scheme_Object *
null_write_special_evt(Scheme_Output_Port *op, Scheme_Object *v)
{
  Scheme_Object *a[2];
  a[0] = scheme_always_ready_evt;
  a[1] = scheme_make_closed_prim(return_data, scheme_true);
  return scheme_wrap_evt(2, a);
}

static int 
null_write_special(Scheme_Output_Port *op, Scheme_Object *v, int nonblock)
{
  return 1;
}

Scheme_Object *
scheme_make_null_output_port(int can_write_special)
{
  Scheme_Output_Port *op;

  op = scheme_make_output_port(scheme_null_output_port_type,
			       NULL,
			       scheme_intern_symbol("null"),
			       null_write_evt,
			       null_write_bytes,
			       NULL,
			       null_close_out,
			       NULL,
			       (can_write_special
				? null_write_special_evt
				: NULL),
			       (can_write_special
				? null_write_special
				: NULL),
			       0);

  return (Scheme_Object *)op;
}

/*========================================================================*/
/*                         redirect output ports                          */
/*========================================================================*/

static long
redirect_write_bytes(Scheme_Output_Port *op,
		     const char *str, long d, long len,
		     int rarely_block, int enable_break)
{
  return scheme_put_byte_string("redirect-output",
				(Scheme_Object *)op->port_data,
				str, d, len,
				rarely_block);
}

static void
redirect_close_out (Scheme_Output_Port *port)
{
}

static Scheme_Object *
redirect_write_evt(Scheme_Output_Port *op, const char *str, long offset, long size)
{
  return scheme_make_write_evt("redirect-write-evt", 
			       (Scheme_Object *)op->port_data,
			       NULL, (char *)str, offset, size);
}

static Scheme_Object *
redirect_write_special_evt(Scheme_Output_Port *op, Scheme_Object *special)
{
  return scheme_make_write_evt("redirect-write-evt", 
			       (Scheme_Object *)op->port_data,
			       special, NULL, 0, 0);
}

static int 
redirect_write_special(Scheme_Output_Port *op, Scheme_Object *special, int nonblock)
{
  Scheme_Object *v, *a[2];

  a[0] = (Scheme_Object *)op->port_data;
  a[1] = special;

  if (nonblock)
    v = scheme_write_special(2, a);
  else
    v = scheme_write_special(2, a);
  
  return SCHEME_TRUEP(v);
}

Scheme_Object *
scheme_make_redirect_output_port(Scheme_Object *port)
{
  Scheme_Output_Port *op;
  int can_write_special;

  can_write_special = !!((Scheme_Output_Port *)port)->write_special_fun;

  op = scheme_make_output_port(scheme_redirect_output_port_type,
			       port,
			       scheme_intern_symbol("redirect"),
			       redirect_write_evt,
			       redirect_write_bytes,
			       NULL,
			       redirect_close_out,
			       NULL,
			       (can_write_special
				? redirect_write_special_evt
				: NULL),
			       (can_write_special
				? redirect_write_special
				: NULL),
			       0);

  return (Scheme_Object *)op;
}

/*********** Unix/Windows: process status stuff *************/

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)

static int subp_done(Scheme_Object *sp)
{
  void *sci = ((Scheme_Subprocess *)sp)->handle;

#if defined(UNIX_PROCESSES)
  System_Child *sc = (System_Child *)sci;
  return sc->done;
#endif
#ifdef WINDOWS_PROCESSES
  DWORD w;
  if (sci) {
    if (GetExitCodeProcess((HANDLE)sci, &w))
      return w != STILL_ACTIVE;
    else
      return 1;
  } else
    return 1;
#endif
}

static void subp_needs_wakeup(Scheme_Object *sp, void *fds)
{
#ifdef WINDOWS_PROCESSES
  void *sci = ((Scheme_Subprocess *)sp)->handle;
  scheme_add_fd_handle((void *)(HANDLE)sci, fds, 0);
#endif
}

#endif

static Scheme_Object *subprocess_status(int argc, Scheme_Object **argv)
{
  Scheme_Subprocess *sp = (Scheme_Subprocess *)argv[0];

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_type("subprocess-status", "subprocess", 0, argc, argv);

#if defined(PROCESS_FUNCTION) && !defined(MAC_CLASSIC_PROCESS_CONTROL)
  {
    int going = 0, status = MZ_FAILURE_STATUS;

#if defined(UNIX_PROCESSES)
    System_Child *sc = (System_Child *)sp->handle;

    if (sc->done)
      status = sc->status;
    else
      going = 1;
#else
# ifdef WINDOWS_PROCESSES
    DWORD w;
    if (sp->handle) {
      if (GetExitCodeProcess((HANDLE)sp->handle, &w)) {
	if (w == STILL_ACTIVE)
	  going = 1;
	else
	  status = w;
      }
    }
# endif
#endif

    if (going)
      return scheme_intern_symbol("running");
    else
      return scheme_make_integer_value(status);
  }
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s: not supported on this platform",
		   "subprocess-status");
#endif
}


static void register_subprocess_wait()
{
#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)
  scheme_add_evt(scheme_subprocess_type, subp_done,
		  subp_needs_wakeup, NULL, 0);
#endif
}

static Scheme_Object *subprocess_wait(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_type("subprocess-wait", "subprocess", 0, argc, argv);

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)
  {
    Scheme_Subprocess *sp = (Scheme_Subprocess *)argv[0];

    scheme_block_until(subp_done, subp_needs_wakeup, (Scheme_Object *)sp, (float)0.0);

    return scheme_void;
  }
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
                 "%s: not supported on this platform",
                 "subprocess-wait");
#endif
}

static Scheme_Object *subprocess_kill(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_type("subprocess-kill", "subprocess", 0, argc, argv);

#if defined(UNIX_PROCESSES) || defined(WINDOWS_PROCESSES)
  {
    Scheme_Subprocess *sp = (Scheme_Subprocess *)argv[0];

#if defined(UNIX_PROCESSES)
    {
      System_Child *sc = (System_Child *)sp->handle;

      while (1) {
	if (sc->done)
	  return scheme_void;

	if (!kill(sp->pid, SCHEME_TRUEP(argv[1]) ? SIGKILL : SIGINT))
	  return scheme_void;

	if (errno != EINTR)
	  break;
	/* Otherwise we were interrupted. Try `kill' again. */
      }
    }
#else
    if (SCHEME_TRUEP(argv[1])) {
      DWORD w;
      int errid;

      if (!sp->handle)
	return scheme_void;

      if (GetExitCodeProcess((HANDLE)sp->handle, &w)) {
	if (w != STILL_ACTIVE)
	  return scheme_void;
	if (TerminateProcess((HANDLE)sp->handle, 1))
	  return scheme_void;
      }
      errid = GetLastError();
      errno = errid;
    } else
      return scheme_void;
#endif

    scheme_raise_exn(MZEXN_FAIL, "subprocess-kill: failed (%E)", errno);

    return NULL;
  }
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s: not supported on this platform",
		   "subprocess-wait");
#endif
}

static Scheme_Object *subprocess_pid(int argc, Scheme_Object **argv)
{
  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type))
    scheme_wrong_type("subprocess-pid", "subprocess", 0, argc, argv);

  return scheme_make_integer_value(((Scheme_Subprocess *)argv[0])->pid);
}

static Scheme_Object *subprocess_p(int argc, Scheme_Object **argv)
{
  return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_subprocess_type)
	  ? scheme_true
	  : scheme_false);
}

/*********** Windows: command-line construction *************/

#ifdef WINDOWS_PROCESSES
static char *cmdline_protect(char *s)
{
  char *naya;
  int ds;
  int has_space = 0, has_quote = 0, was_slash = 0;

  for (ds = 0; s[ds]; ds++) {
    if (isspace(s[ds]) || (s[ds] == '\'')) {
      has_space = 1;
      was_slash = 0;
    } else if (s[ds] == '"') {
      has_quote += 1 + (2 * was_slash);
      was_slash = 0;
    } else if (s[ds] == '\\') {
      was_slash++;
    } else
      was_slash = 0;
  }

  if (has_space || has_quote) {
    char *p;
    int wrote_slash = 0;

    naya = scheme_malloc_atomic(strlen(s) + 3 + 3*has_quote);
    naya[0] = '"';
    for (p = naya + 1; *s; s++) {
      if (*s == '"') {
	while (wrote_slash--) {
	  *(p++) = '\\';
	}
	*(p++) = '"'; /* endquote */
	*(p++) = '\\';
	*(p++) = '"'; /* protected */
	*(p++) = '"'; /* start quote again */
	wrote_slash = 0;
      } else if (*s == '\\') {
	*(p++) = '\\';
	wrote_slash++;
      } else {
	*(p++) = *s;
	wrote_slash = 0;
      }
    }
    *(p++) = '"';
    *p = 0;

    return naya;
  }

  return s;
}

static long mz_spawnv(char *command, const char * const *argv,
		      int exact_cmdline, int sin, int sout, int serr, int *pid)
{
  int i, l, len = 0;
  long cr_flag;
  char *cmdline;
  STARTUPINFOW startup;
  PROCESS_INFORMATION info;

  if (exact_cmdline) {
    cmdline = (char *)argv[1];
  } else {
    for (i = 0; argv[i]; i++) {
      len += strlen(argv[i]) + 1;
    }

    cmdline = (char *)scheme_malloc_atomic(len);

    len = 0;
    for (i = 0; argv[i]; i++) {
      l = strlen(argv[i]);
      memcpy(cmdline + len, argv[i], l);
      cmdline[len + l] = ' ';
      len += l + 1;
    }
    --len;
    cmdline[len] = 0;
  }

  memset(&startup, 0, sizeof(startup));
  startup.cb = sizeof(startup);
  startup.dwFlags = STARTF_USESTDHANDLES;
  startup.hStdInput = (HANDLE)sin;
  startup.hStdOutput = (HANDLE)sout;
  startup.hStdError = (HANDLE)serr;

  /* If none of the stdio handles are consoles, specifically
     create the subprocess without a console: */
  if ((GetFileType(startup.hStdInput) != FILE_TYPE_CHAR)
      && (GetFileType(startup.hStdOutput) != FILE_TYPE_CHAR)
      && (GetFileType(startup.hStdError) != FILE_TYPE_CHAR))
    cr_flag = CREATE_NO_WINDOW;
  else
    cr_flag = 0;

  if (CreateProcessW(WIDE_PATH_COPY(command), WIDE_PATH_COPY(cmdline), 
		     NULL, NULL, 1 /*inherit*/,
		     cr_flag, NULL, NULL,
		     &startup, &info)) {
    CloseHandle(info.hThread);
    *pid = info.dwProcessId;
    return (long)info.hProcess;
  } else
    return -1;
}

static void close_subprocess_handle(void *sp, void *ignored)
{
  Scheme_Subprocess *subproc = (Scheme_Subprocess *)sp;
  CloseHandle(subproc->handle);
}

#endif /* WINDOWS_PROCESSES */

/*********** All: The main system/process/execute function *************/

static Scheme_Object *subprocess(int c, Scheme_Object *args[])
     /* subprocess(out, in, err, exe, arg ...) */
{
  const char *name = "subprocess";
#if defined(PROCESS_FUNCTION) && !defined(MAC_CLASSIC_PROCESS_CONTROL)
  char *command;
  int to_subprocess[2], from_subprocess[2], err_subprocess[2];
  int i, pid;
  char **argv;
  Scheme_Object *in, *out, *err;
#if defined(UNIX_PROCESSES)
  System_Child *sc;
#else
  void *sc = 0;
#endif
  Scheme_Object *inport;
  Scheme_Object *outport;
  Scheme_Object *errport;
  Scheme_Object *a[4];
  Scheme_Subprocess *subproc;
#if defined(WINDOWS_PROCESSES)
  int exact_cmdline = 0;
#endif
#if defined(WINDOWS_PROCESSES)
  int spawn_status;
#endif

  /*--------------------------------------------*/
  /* Sort out ports (create later if necessary) */
  /*--------------------------------------------*/

  if (SCHEME_TRUEP(args[0])) {
    outport = args[0];
    if (SCHEME_OUTPORTP(outport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &outport))) {
#ifdef PROCESS_FUNCTION
      Scheme_Output_Port *op = (Scheme_Output_Port *)outport;

      if (SAME_OBJ(op->sub_type, file_output_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Output_File *)op->port_data)->f);
	from_subprocess[1] = tmp;
      }
# ifdef MZ_FDS
      else if (SAME_OBJ(op->sub_type, fd_output_port_type))
	from_subprocess[1] = ((Scheme_FD *)op->port_data)->fd;
# endif
#endif
    } else
      scheme_wrong_type(name, "file-stream-output-port", 0, c, args);
  } else
    outport = NULL;

  if (SCHEME_TRUEP(args[1])) {
    inport = args[1];
    if (SCHEME_INPORTP(inport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &inport))) {
#ifdef PROCESS_FUNCTION
      Scheme_Input_Port *ip = (Scheme_Input_Port *)inport;

      if (SAME_OBJ(ip->sub_type, file_input_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Input_File *)ip->port_data)->f);
	to_subprocess[0] = tmp;
      }
# ifdef MZ_FDS
      else if (SAME_OBJ(ip->sub_type, fd_input_port_type))
	to_subprocess[0] = ((Scheme_FD *)ip->port_data)->fd;
# endif
#endif
    } else
      scheme_wrong_type(name, "file-stream-input-port", 1, c, args);
  } else
    inport = NULL;

  if (SCHEME_TRUEP(args[2])) {
    errport = args[2];
    if (SCHEME_OUTPORTP(errport) && SCHEME_TRUEP(scheme_file_stream_port_p(1, &errport))) {
#ifdef PROCESS_FUNCTION
      Scheme_Output_Port *op = (Scheme_Output_Port *)errport;

      if (SAME_OBJ(op->sub_type, file_output_port_type)) {
	int tmp;
	tmp = MSC_IZE(fileno)(((Scheme_Output_File *)op->port_data)->f);
	err_subprocess[1] = tmp;
      }
# ifdef MZ_FDS
      else if (SAME_OBJ(op->sub_type, fd_output_port_type))
	err_subprocess[1] = ((Scheme_FD *)op->port_data)->fd;
# endif
#endif
    } else
      scheme_wrong_type(name, "file-stream-output-port", 2, c, args);
  } else
    errport = NULL;

  if (!SCHEME_PATH_STRINGP(args[3]))
    scheme_wrong_type(name, SCHEME_PATH_STRING_STR, 3, c, args);

  /*--------------------------------------*/
  /*          Sort out arguments          */
  /*--------------------------------------*/

  argv = MALLOC_N(char *, c - 3 + 1);
  {
    char *ef;
    ef = scheme_expand_string_filename(args[3],
				       (char *)name, 
				       NULL,
				       SCHEME_GUARD_FILE_EXECUTE);
    argv[0] = ef;
  }
  {
    /* This is for Windows: */
    char *np;
    int nplen;
    nplen = strlen(argv[0]);
    np = scheme_normal_path_seps(argv[0], &nplen, 0);
    argv[0] = np;
  }

  if ((c == 6) && SAME_OBJ(args[4], exact_symbol)) {
    argv[2] = NULL;
    if (!SCHEME_CHAR_STRINGP(args[5]) || scheme_any_string_has_null(args[5]))
      scheme_wrong_type(name, CHAR_STRING_W_NO_NULLS, 5, c, args);
    {
      Scheme_Object *bs;
      bs = scheme_char_string_to_byte_string(args[5]);
      argv[1] = SCHEME_BYTE_STR_VAL(bs);
    }
#ifdef WINDOWS_PROCESSES
    exact_cmdline = 1;
#else
    /* 'exact-full only works in windows */
    scheme_arg_mismatch(name,
			"exact command line not supported on this platform: ",
			args[5]);
#endif
  } else {
    for (i = 4; i < c; i++) {
      if (!SCHEME_CHAR_STRINGP(args[i]) || scheme_any_string_has_null(args[i]))
	scheme_wrong_type(name, CHAR_STRING_W_NO_NULLS, i, c, args);
      {
	Scheme_Object *bs;
	bs = scheme_char_string_to_byte_string_locale(args[i]);
	argv[i - 3] = SCHEME_BYTE_STR_VAL(bs);
      }
    }
    argv[c - 3] = NULL;
  }

  command = argv[0];

  if (!inport || !outport || !errport)
    scheme_custodian_check_available(NULL, name, "file-stream");

  /*--------------------------------------*/
  /*          Create needed pipes         */
  /*--------------------------------------*/

  if (!inport && PIPE_FUNC(to_subprocess, 1 _EXTRA_PIPE_ARGS))
    scheme_raise_exn(MZEXN_FAIL, "%s: pipe failed (%e)", name, errno);
  if (!outport && PIPE_FUNC(from_subprocess, 0 _EXTRA_PIPE_ARGS)) {
    if (!inport) {
      MSC_IZE(close)(to_subprocess[0]);
      MSC_IZE(close)(to_subprocess[1]);
    }
    scheme_raise_exn(MZEXN_FAIL, "%s: pipe failed (%e)", name, errno);
  }
  if (!errport && PIPE_FUNC(err_subprocess, 0 _EXTRA_PIPE_ARGS)) {
    if (!inport) {
      MSC_IZE(close)(to_subprocess[0]);
      MSC_IZE(close)(to_subprocess[1]);
    }
    if (!outport) {
      MSC_IZE(close)(from_subprocess[0]);
      MSC_IZE(close)(from_subprocess[1]);
    }
    scheme_raise_exn(MZEXN_FAIL, "%s: pipe failed (%e)", name, errno);
  }

#if defined(WINDOWS_PROCESSES)

  /*--------------------------------------*/
  /*        Execute: Windows              */
  /*--------------------------------------*/

  /* Windows: quasi-stdin is locked, and we'll say it doesn't matter */
  fflush(stdin);
  fflush(stdout);
  fflush(stderr);

  {
    Scheme_Object *tcd;

    if (!exact_cmdline) {
      /* protect spaces, etc. in the arguments: */
      for (i = 0; i < (c - 3); i++) {
	char *cla;
	cla = cmdline_protect(argv[i]);
	argv[i] = cla;
      }
    }

    /* Set real CWD - and hope no other thread changes it! */
    tcd = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);
    scheme_os_setcwd(SCHEME_BYTE_STR_VAL(tcd), 0);

    spawn_status = mz_spawnv(command, (const char * const *)argv,
			     exact_cmdline,
			     to_subprocess[0],
			     from_subprocess[1],
			     err_subprocess[1],
			     &pid);

    if (spawn_status != -1)
      sc = (void *)spawn_status;
  }

# define mzCLOSE_PIPE_END(x) CloseHandle((HANDLE)(x))
#else


  /*--------------------------------------*/
  /*            Execute: Unix             */
  /*--------------------------------------*/

  {
    init_sigchld();

    sc = MALLOC_ONE_RT(System_Child);
#ifdef MZTAG_REQUIRED
    sc->type = scheme_rt_system_child;
#endif
    sc->id = 0;
    sc->done = 0;

    scheme_block_child_signals(1);

    pid = fork();

    if (pid > 0) {
      sc->next = scheme_system_children;
      scheme_system_children = sc;
      sc->id = pid;
    } else {
#ifdef USE_ITIMER
      /* Turn off the timer. */
      /* SIGPROF is masked at this point due to
	 block_child_signals() */
      struct itimerval t, old;
      sigset_t sigs;

      t.it_value.tv_sec = 0;
      t.it_value.tv_usec = 0;
      t.it_interval.tv_sec = 0;
      t.it_interval.tv_usec = 0;

      setitimer(ITIMER_PROF, &t, &old);

      /* Clear already-queued PROF signal, if any: */
      START_XFORM_SKIP;
      sigemptyset(&sigs);
      while (!sigpending(&sigs)) {
	if (sigismember(&sigs, SIGPROF)) {
	  sigprocmask(SIG_SETMASK, NULL, &sigs);
	  sigdelset(&sigs, SIGPROF);
	  sigsuspend(&sigs);
	  sigemptyset(&sigs);
	} else
	  break;
      }
      END_XFORM_SKIP;
#endif
    }

    scheme_block_child_signals(0);
  }

  switch (pid)
    {
    case -1:
      /* Close unused descriptors. */
      if (!inport) {
	MSC_IZE(close)(to_subprocess[0]);
	MSC_IZE(close)(to_subprocess[1]);
      }
      if (!outport) {
	MSC_IZE(close)(from_subprocess[0]);
	MSC_IZE(close)(from_subprocess[1]);
      }
      if (!errport) {
	MSC_IZE(close)(err_subprocess[0]);
	MSC_IZE(close)(err_subprocess[1]);
      }
      scheme_raise_exn(MZEXN_FAIL, "%s: fork failed", name);
      return scheme_false;

    case 0: /* child */

      {
	/* Copy pipe descriptors to stdin and stdout */
	MSC_IZE(dup2)(to_subprocess[0], 0);
	MSC_IZE(dup2)(from_subprocess[1], 1);
	MSC_IZE(dup2)(err_subprocess[1], 2);

	/* Close unwanted descriptors. */
	if (!inport) {
	  MSC_IZE(close)(to_subprocess[0]);
	  MSC_IZE(close)(to_subprocess[1]);
	}
	if (!outport) {
	  MSC_IZE(close)(from_subprocess[0]);
	  MSC_IZE(close)(from_subprocess[1]);
	}
	if (!errport) {
	  MSC_IZE(close)(err_subprocess[0]);
	  MSC_IZE(close)(err_subprocess[1]);
	}

#ifdef CLOSE_ALL_FDS_AFTER_FORK
	/* Actually, unwanted includes everything
	   except stdio. */
#ifdef USE_ULIMIT
	i = ulimit(4, 0);
#else
	i = getdtablesize();
#endif
	while (i-- > 3) {
	  int cr;
	  do {
	    cr = close(i);
	  } while ((cr == -1) && (errno == EINTR));
	}
#endif
      }

      /* Set real CWD */
      {
	Scheme_Object *dir;
	dir = scheme_get_param(scheme_current_config(), MZCONFIG_CURRENT_DIRECTORY);
	scheme_os_setcwd(SCHEME_PATH_VAL(dir), 0);
      }

      /* Exec new process */

      {
	int err;

	/* Reset ignored signals: */
	START_XFORM_SKIP;
#ifndef DONT_IGNORE_FPE_SIGNAL
	MZ_SIGSET(SIGFPE, SIG_DFL);
#endif
#ifndef DONT_IGNORE_PIPE_SIGNAL
	MZ_SIGSET(SIGPIPE, SIG_DFL);
#endif
	END_XFORM_SKIP;

	err = MSC_IZE(execv)(command, argv);

	/* If we get here it failed; give up */

        /* using scheme_signal_error will leave us in the forked process,
	   so use scheme_console_printf instead */
        scheme_console_printf("mzscheme: exec failed (%d)\n", err);

	/* back to MzScheme signal dispositions: */
	START_XFORM_SKIP;
#ifndef DONT_IGNORE_FPE_SIGNAL
	MZ_SIGSET(SIGFPE, SIG_IGN);
#endif
#ifndef DONT_IGNORE_PIPE_SIGNAL
	MZ_SIGSET(SIGPIPE, SIG_IGN);
#endif
	END_XFORM_SKIP;

	_exit(1);
      }

    default: /* parent */

      break;
    }
# define mzCLOSE_PIPE_END(x) MSC_IZE(close)(x)
#endif

  /*--------------------------------------*/
  /*      Close unneeded descriptors      */
  /*--------------------------------------*/

  if (!inport) {
    mzCLOSE_PIPE_END(to_subprocess[0]);
    out = NULL;
    scheme_file_open_count += 1;
  } else
    out = scheme_false;
  if (!outport) {
    mzCLOSE_PIPE_END(from_subprocess[1]);
    in = NULL;
    scheme_file_open_count += 1;
  } else
    in = scheme_false;
  if (!errport) {
    mzCLOSE_PIPE_END(err_subprocess[1]);
    err = NULL;
    scheme_file_open_count += 1;
  } else
    err = scheme_false;

  /*--------------------------------------*/
  /*        Create new port objects       */
  /*--------------------------------------*/

  in = (in ? in : make_fd_input_port(from_subprocess[0], scheme_intern_symbol("subprocess-stdout"), 0, 0, NULL));
  out = (out ? out : make_fd_output_port(to_subprocess[1], scheme_intern_symbol("subprocess-stdin"), 0, 0, 0));
  err = (err ? err : make_fd_input_port(err_subprocess[0], scheme_intern_symbol("subprocess-stderr"), 0, 0, 0));

  /*--------------------------------------*/
  /*          Return result info          */
  /*--------------------------------------*/

  subproc = MALLOC_ONE_TAGGED(Scheme_Subprocess);
  subproc->so.type = scheme_subprocess_type;
  subproc->handle = (void *)sc;
  subproc->pid = pid;
# if defined(WINDOWS_PROCESSES)
  scheme_add_finalizer(subproc, close_subprocess_handle, NULL);
# endif

#define cons scheme_make_pair

  a[0] = (Scheme_Object *)subproc;
  a[1] = in;
  a[2] = out;
  a[3] = err;

  return scheme_values(4, a);

#else
# ifdef MAC_CLASSIC_PROCESS_CONTROL

  /*--------------------------------------*/
  /*            Macintosh hacks           */
  /*--------------------------------------*/

  {
    int i;
    Scheme_Object *a[4], *appname;
    Scheme_Subprocess *subproc;

    for (i = 0; i < 3; i++) {
      if (!SCHEME_FALSEP(args[i]))
	scheme_arg_mismatch(name,
			    "non-#f port argument not allowed on this platform: ",
			    args[i]);
    }

    if (c > 4) {
      if (c == 5) {
	Scheme_Object *bs;
	if (!SCHEME_PATH_STRINGP(args[3]))
	  scheme_wrong_type(name, SCHEME_PATH_STRING_STR, 3, c, args);
	if (SCHEME_PATHP(args[3]))
	  bs = args[3];
	else
	  bs = scheme_char_string_to_path(args[3]);
	if (strcmp(SCHEME_PATH_VAL(bs), "by-id"))
	  scheme_arg_mismatch(name,
			      "in five-argument mode on this platform, the 4th argument must be \"by-id\": ",
			      args[3]);

	appname = args[4];
	i = scheme_mac_start_app((char *)name, 1, appname);
      } else
	scheme_arg_mismatch(name,
			    "extra arguments after the application id are "
			    "not allowed on this platform: ",
			    args[5]);
    } else {
      appname = args[3];
      i = scheme_mac_start_app((char *)name, 0, appname);
    }

    if (!i) {
      scheme_raise_exn(MZEXN_FAIL, "%s: launch failed for application: %Q", name, appname);
      return NULL;
    }

    subproc = MALLOC_ONE_TAGGED(Scheme_Subprocess);
    subproc->type = scheme_subprocess_type;

    a[0] = (Scheme_Object *)subproc;
    a[1] = scheme_false;
    a[2] = scheme_false;
    a[3] = scheme_false;

    return scheme_values(4, a);
  }

# else
  /*--------------------------------------*/
  /*  Subprocess functionality disabled   */
  /*--------------------------------------*/

  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "%s: not supported on this platform",
		   name);
  return NULL;
# endif
#endif
}

static Scheme_Object *sch_shell_execute(int c, Scheme_Object *argv[])
{
  int show;
  char *dir;
#ifdef WINDOWS_PROCESSES
# define mzseSHOW(x) x
#else
# define mzseSHOW(x) 1
#endif

  if (!SCHEME_FALSEP(argv[0]) && !SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("shell-execute", "string or #f", 0, c, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_type("shell-execute", "string", 1, c, argv);
  if (!SCHEME_CHAR_STRINGP(argv[2]))
    scheme_wrong_type("shell-execute", "string", 2, c, argv);
  if (!SCHEME_PATH_STRINGP(argv[3]))
    scheme_wrong_type("shell-execute", SCHEME_PATH_STRING_STR, 3, c, argv);
  {
    show = 0;
# define mzseCMP(id, str)			       \
    if (SAME_OBJ(scheme_intern_symbol(str), argv[4])   \
        || SAME_OBJ(scheme_intern_symbol(# id), argv[4])) \
      show = mzseSHOW(id)
    mzseCMP(SW_HIDE, "sw_hide");
    mzseCMP(SW_MAXIMIZE, "sw_maximize");
    mzseCMP(SW_MINIMIZE, "sw_minimize");
    mzseCMP(SW_RESTORE, "sw_restore");
    mzseCMP(SW_SHOW, "sw_show");
    mzseCMP(SW_SHOWDEFAULT, "sw_showdefault");
    mzseCMP(SW_SHOWMAXIMIZED, "sw_showmaximized");
    mzseCMP(SW_SHOWMINIMIZED, "sw_showminimized");
    mzseCMP(SW_SHOWMINNOACTIVE, "sw_showminnoactive");
    mzseCMP(SW_SHOWNA, "sw_showna");
    mzseCMP(SW_SHOWNOACTIVATE, "sw_shownoactivate");
    mzseCMP(SW_SHOWNORMAL, "sw_shownormal");

    if (!show)
      scheme_wrong_type("shell-execute", "show-mode symbol", 4, c, argv);
  }

  dir = scheme_expand_string_filename(argv[3],
				      "shell-execute", NULL,
				      SCHEME_GUARD_FILE_EXISTS);
#ifdef WINDOWS_PROCESSES
  {
    SHELLEXECUTEINFOW se;
    int nplen;
    Scheme_Object *sv, *sf, *sp;

    nplen = strlen(dir);
    dir = scheme_normal_path_seps(dir, &nplen, 0);

    if (SCHEME_FALSEP(argv[0]))
      sv = scheme_false;
    else
      sv = scheme_char_string_to_byte_string(argv[0]);
    sf = scheme_char_string_to_byte_string(argv[1]);
    sp = scheme_char_string_to_byte_string(argv[2]);

    memset(&se, 0, sizeof(se));
    se.fMask = SEE_MASK_NOCLOSEPROCESS | SEE_MASK_FLAG_DDEWAIT;
    se.cbSize = sizeof(se);
    if (SCHEME_FALSEP(sv))
      se.lpVerb = NULL;
    else {
      se.lpVerb = WIDE_PATH_COPY(SCHEME_BYTE_STR_VAL(sv));
    }
    se.lpFile = WIDE_PATH_COPY(SCHEME_BYTE_STR_VAL(sf));
    se.lpParameters = WIDE_PATH_COPY(SCHEME_BYTE_STR_VAL(sp));
    se.lpDirectory = WIDE_PATH_COPY(dir);
    se.nShow = show;
    se.hwnd = NULL;

    /* Used to use ShellExecuteEx(&se) here. Not sure why it doesn't work,
       and the problem was intermittent (e.g., worked for opening a URL
       with IE as the default browser, but failed with Netscape). */
    if (ShellExecuteW(se.hwnd, se.lpVerb, se.lpFile, se.lpParameters, se.lpDirectory, se.nShow)) {
      if (se.hProcess) {
	Scheme_Subprocess *subproc;

	subproc = MALLOC_ONE_TAGGED(Scheme_Subprocess);

	subproc->so.type = scheme_subprocess_type;
	subproc->handle = (void *)se.hProcess;
	subproc->pid = 0;
	scheme_add_finalizer(subproc, close_subprocess_handle, NULL);

	return (Scheme_Object *)subproc;
      } else
	return scheme_false;
    } else {
      scheme_signal_error("shell-execute: execute failed for: %V (%E)",
			  argv[1],
			  GetLastError());
      return NULL;
    }
  }
#else
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "shell-execute: not supported on this platform");
  return NULL;
#endif
}

/*========================================================================*/
/*                             sleeping                                   */
/*========================================================================*/

/* This code is used to implement sleeping when MzScheme is completely
   blocked on external objects, such as ports. For Unix, sleeping is
   essentially just a select(). For Windows, we essentially have to
   implement select() ourselves, so that it works with both TCP
   connections and stream ports all at once. */

/********************* Windows TCP watcher *****************/

#if defined(WIN32_FD_HANDLES)
typedef struct
{
  MZTAG_IF_REQUIRED
  fd_set *rd, *wr, *ex;
} Tcp_Select_Info;

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

static long select_for_tcp(void *data)
{
  /* Precise GC: we rely on the fact that a GC can't occur
     during this thread's lifetime. */
  Tcp_Select_Info *info = (Tcp_Select_Info *)data;

  select(0, info->rd, info->wr, info->ex, NULL);

  return 0;
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

# ifdef USE_WINSOCK_TCP
#  define TCP_T SOCKET
# else
#  define TCP_T int
# endif

# ifndef MZ_PF_INET
#  define MZ_PF_INET PF_INET
# endif

#endif

/****************** Windows cleanup  *****************/

#if defined(WIN32_FD_HANDLES)
static void clean_up_wait(long result, OS_SEMAPHORE_TYPE *array,
			  int *rps, int count)
{
  if ((result >= (long)WAIT_OBJECT_0) && (result < (long)WAIT_OBJECT_0 + count)) {
    result -= WAIT_OBJECT_0;
    if (rps[result])
      ReleaseSemaphore(array[result], 1, NULL);
  }

  /* Clear out break semaphore */
  WaitForSingleObject(scheme_break_semaphore, 0);
}
#endif

/******************** Main sleep function  *****************/
/* The simple select() stuff is buried in Windows complexity. */

/* This sleep function is not allowed to allocate in OS X, because it
   is called in a non-main thread. */

#ifdef OS_X
# ifdef MZ_XFORM
START_XFORM_SKIP;
# endif
#endif

static void default_sleep(float v, void *fds)
{
  /* REMEMBER: don't allocate in this function (at least not GCable
     memory) for OS X. Not that FD setups are ok, because they use
     eternal mallocs. */

#ifdef USE_OSKIT_CONSOLE
  /* Don't really sleep; keep polling the keyboard: */
  if (!v || (v > 0.01))
    v = 0.01;
#endif

  if (!fds) {
    /* Nothing to block on - just sleep for some amount of time. */
#if defined(FILES_HAVE_FDS)
    /* Sleep by selecting on the external event fd */
    struct timeval time;
    long secs = (long)v;
    long usecs = (long)(fmod(v, 1.0) * 1000000);

    if (v && (v > 100000))
      secs = 100000;
    if (usecs < 0)
      usecs = 0;
    if (usecs >= 1000000)
      usecs = 999999;

    time.tv_sec = secs;
    time.tv_usec = usecs;

    if (external_event_fd) {
      DECL_FDSET(readfds, 1);

      INIT_DECL_FDSET(readfds, 1);

      MZ_FD_ZERO(readfds);
      MZ_FD_SET(external_event_fd, readfds);

      select(external_event_fd + 1, readfds, NULL, NULL, &time);
    } else {
      select(0, NULL, NULL, NULL, &time);
    }

#else
# ifndef NO_SLEEP
#  ifndef NO_USLEEP
   usleep((unsigned)(v * 1000));
#   else
   sleep(v);
#  endif
# endif
#endif
  } else {
    /* Something to block on - sort our the parts in Windows. */

#if defined(FILES_HAVE_FDS) || defined(USE_WINSOCK_TCP)
    int limit;
    fd_set *rd, *wr, *ex;
    struct timeval time;

#ifdef SIGCHILD_DOESNT_INTERRUPT_SELECT
    if (scheme_system_children) {
      /* Better poll every second or so... */
      if (!v || (v > 1))
	v = 1;
    }
#endif

    {
      long secs = (long)v;
      long usecs = (long)(fmod(v, 1.0) * 1000000);

      if (v && (v > 100000))
	secs = 100000;
      if (usecs < 0)
	usecs = 0;
      if (usecs >= 1000000)
	usecs = 999999;

      time.tv_sec = secs;
      time.tv_usec = usecs;
    }

# ifdef USE_WINSOCK_TCP
    limit = 0;
# else
#  ifdef USE_ULIMIT
    limit = ulimit(4, 0);
#  else
#   ifdef FIXED_FD_LIMIT
    limit = FIXED_FD_LIMIT;
#   else
    limit = getdtablesize();
#   endif
#  endif
#endif

    rd = (fd_set *)fds;
    wr = (fd_set *)MZ_GET_FDSET(fds, 1);
    ex = (fd_set *)MZ_GET_FDSET(fds, 2);

    /******* Start Windows stuff *******/

#if defined(WIN32_FD_HANDLES)
    {
      long result;
      OS_SEMAPHORE_TYPE *array, just_two_array[2], break_sema;
      int count, *rps, just_two_rps[2];
      int fd_added;

      if (((win_extended_fd_set *)rd)->no_sleep)
	return;

      fd_added = (((win_extended_fd_set *)rd)->added
		  || ((win_extended_fd_set *)wr)->added
		  || ((win_extended_fd_set *)ex)->added);
      count = ((win_extended_fd_set *)fds)->num_handles;
      array = ((win_extended_fd_set *)fds)->handles;
      rps = ((win_extended_fd_set *)fds)->repost_sema;

      /* add break semaphore: */
      if (!count) {
	array = just_two_array;
	rps = just_two_rps;
      }
      rps[count] = 0;
      break_sema = scheme_break_semaphore;
      array[count++] = break_sema;

      if (count && !fd_added) {
	/* Simple: just wait for HANDLE-based input: */
	/* Extensions may handle events */
	if (((win_extended_fd_set *)fds)->wait_event_mask
	    && GetQueueStatus(((win_extended_fd_set *)fds)->wait_event_mask))
	  result = WAIT_TIMEOUT; /* doesn't matter... */
	else {
	  DWORD msec;
	  if (v) {
	    if (v > 100000)
	      msec = 100000000;
	    else
	      msec = (DWORD)(v * 1000);
	  } else {
	    msec = INFINITE;
	  }
	  result = MsgWaitForMultipleObjects(count, array, FALSE, msec,
					     ((win_extended_fd_set *)fds)->wait_event_mask);
	}
	clean_up_wait(result, array, rps, count);
	return;
      } else if (count) {
	/* What a mess! We must wait for either HANDLE-based input or TCP
	   status. Use a thread to select() for TCP status, and then
	   hit a semaphore if the status changes. Meanwhile, in this
	   thread, wait on both the console input and the semaphore.
	   When either happens, kill the thread. */
	OS_THREAD_TYPE th;
	Tcp_Select_Info *info;
	TCP_T fake;
	struct Scheme_Thread_Memory *thread_memory;

	info = MALLOC_ONE_RT(Tcp_Select_Info);
#ifdef MZTAG_REQUIRED
	info->type = scheme_rt_tcp_select_info;
#endif

	fake = socket(MZ_PF_INET, SOCK_STREAM, 0);
	FD_SET(fake, ex);

	info->rd = rd;
	info->wr = wr;
	info->ex = ex;

	{
	  DWORD id;
	  th = CreateThread(NULL, 4096,
			    (LPTHREAD_START_ROUTINE)select_for_tcp,
			    info, 0, &id);
	  /* Not actually necessary, since GC can't occur during the
	     thread's life, but better safe than sorry if we change the
	     code later. */
	  thread_memory = scheme_remember_thread((void *)th, 0);
	}

	rps[count] = 0;
	array[count++] = th;

	if (((win_extended_fd_set *)fds)->wait_event_mask
	    && GetQueueStatus(((win_extended_fd_set *)fds)->wait_event_mask))
	  result = WAIT_TIMEOUT; /* doesn't matter... */
	else {
	  DWORD msec;
	  if (v) {
	    if (v > 100000)
	      msec = 100000000;
	    else
	      msec = (DWORD)(v * 1000);
	  } else {
	    msec = INFINITE;
	  }
	  result = MsgWaitForMultipleObjects(count, array, FALSE,
					     v ? (DWORD)(v * 1000) : INFINITE,
					     ((win_extended_fd_set *)fds)->wait_event_mask);
	}
	clean_up_wait(result, array, rps, count);

	closesocket(fake); /* cause selector thread to end */

	WaitForSingleObject(th, INFINITE);
	scheme_forget_thread(thread_memory);
	CloseHandle(th);

	return;
      }
    }
#endif

#ifdef USE_WINSOCK_TCP
    /* Stupid Windows: give select() empty fd_sets and it ignores the timeout. */
    if (!rd->fd_count && !wr->fd_count && !ex->fd_count) {
      if (v)
	Sleep((DWORD)(v * 1000));
      return;
    }
#endif

    /******* End Windows stuff *******/

#if defined(FILES_HAVE_FDS)
    /* Watch for external events, too: */
    if (external_event_fd)
      MZ_FD_SET(external_event_fd, rd);
#endif

    select(limit, rd, wr, ex, v ? &time : NULL);

#endif
  }

#if defined(FILES_HAVE_FDS)
  /* Clear external event flag */
  if (external_event_fd) {
    char buf[10];
    read(external_event_fd, buf, 10);
    event_fd_set = 0;
  }
#endif
}

#ifdef OS_X
# ifdef MZ_XFORM
END_XFORM_SKIP;
# endif
#endif

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

void scheme_signal_received(void)
/* Ensure that MzScheme wakes up if asleep. */
{
#if defined(FILES_HAVE_FDS)
  if (put_external_event_fd && !event_fd_set) {
    event_fd_set = 1;
    write(put_external_event_fd, "!", 1);
  }
#endif
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

int scheme_get_external_event_fd(void)
{
#if defined(FILES_HAVE_FDS)
  return external_event_fd;
#else
  return 0;
#endif
}

#ifdef USE_WIN32_THREAD_TIMER

static HANDLE itimer;
static OS_SEMAPHORE_TYPE itimer_semaphore;
static long itimer_delay;

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

static long ITimer(void)
{
  WaitForSingleObject(itimer_semaphore, INFINITE);

  while (1) {
    if (WaitForSingleObject(itimer_semaphore, itimer_delay / 1000) == WAIT_TIMEOUT) {
      scheme_fuel_counter = 0;
      WaitForSingleObject(itimer_semaphore, INFINITE);
    }
  }
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

void scheme_start_itimer_thread(long usec)
{
  DWORD id;

  if (!itimer) {
    itimer = CreateThread(NULL, 4096, (LPTHREAD_START_ROUTINE)ITimer, NULL, 0, &id);
    itimer_semaphore = CreateSemaphore(NULL, 0, 1, NULL);
    scheme_remember_thread(itimer, 0);
  }

  itimer_delay = usec;
  ReleaseSemaphore(itimer_semaphore, 1, NULL);
}

#endif

/*========================================================================*/
/*                       memory debugging help                            */
/*========================================================================*/


#ifdef MEMORY_COUNTING_ON
void scheme_count_input_port(Scheme_Object *port, long *s, long *e,
			     Scheme_Hash_Table *ht)
{
  Scheme_Input_Port *ip = (Scheme_Input_Port *)port;

  *e = (ht ? scheme_count_memory(ip->read_handler, ht) : 0);
  *s = sizeof(Scheme_Input_Port);

  if (ip->sub_type == file_input_port_type)
    *s += sizeof(Scheme_Input_File);
  else if (ip->sub_type == scheme_string_input_port_type) {
    Scheme_Indexed_String *is;
    is = (Scheme_Indexed_String *)ip->port_data;
    *s += (sizeof(Scheme_Indexed_String)
	   + is->size);
  } else if (ip->sub_type == scheme_tcp_input_port_type) {
    if (ht && !scheme_hash_get(ht, (Scheme_Object *)ip->port_data)) {
      scheme_hash_set(ht, (Scheme_Object *)ip->port_data, scheme_true);
    }
  } else if (ip->sub_type == scheme_user_input_port_type) {
    Scheme_Object **d;
    d = (Scheme_Object **)ip->port_data;
    *s += (3 * sizeof(Scheme_Object *));
    *e += (ht
	   ? (scheme_count_memory(d[0], ht)
	      + scheme_count_memory(d[1], ht)
	      + scheme_count_memory(d[2], ht))
	   : 0);
  } else if (ip->sub_type == scheme_pipe_read_port_type) {
    if (ht && !scheme_hash_get(ht, (Scheme_Object *)ip->port_data)) {
      Scheme_Pipe *p = (Scheme_Pipe *)ip->port_data;
      scheme_hash_set(ht, (Scheme_Object *)ip->port_data, scheme_true);
      *s += (sizeof(Scheme_Pipe) + p->buflen);
    }
  }
}

void scheme_count_output_port(Scheme_Object *port, long *s, long *e,
			      Scheme_Hash_Table *ht)
{
  Scheme_Output_Port *op = (Scheme_Output_Port *)port;

  *e = 0;
  *s = sizeof(Scheme_Output_Port);

  if (op->sub_type == file_output_port_type)
    *s += sizeof(Scheme_Output_File);
  else if (op->sub_type == scheme_string_output_port_type) {
    Scheme_Indexed_String *is;
    is = (Scheme_Indexed_String *)op->port_data;
    *s += (sizeof(Scheme_Indexed_String)
	   + is->size);
  } else if (op->sub_type == scheme_tcp_output_port_type) {
    if (!scheme_hash_get(ht, (Scheme_Object *)op->port_data)) {
      scheme_hash_set(ht, (Scheme_Object *)op->port_data, scheme_true);
    }
  } else if (op->sub_type == scheme_user_output_port_type) {
    Scheme_Object **d;
    d = (Scheme_Object **)op->port_data;
    *s += (2 * sizeof(Scheme_Object *));
    *e += (ht
	   ? (scheme_count_memory(d[0], ht)
	      + scheme_count_memory(d[1], ht))
	   : 0);
  } else if (op->sub_type == scheme_pipe_read_port_type) {
    if (!scheme_hash_get(ht, (Scheme_Object *)op->port_data)) {
      Scheme_Pipe *p = (Scheme_Pipe *)op->port_data;
      scheme_hash_set(ht, (Scheme_Object *)op->port_data, scheme_true);
      *s += (sizeof(Scheme_Pipe) + p->buflen);
    }
  }
}
#endif

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_PORT_C
#include "mzmark.c"

static void register_traversers(void)
{
#ifdef WINDOWS_PROCESSES
  GC_REG_TRAV(scheme_rt_thread_memory, mark_thread_memory);
#endif
  GC_REG_TRAV(scheme_rt_input_file, mark_input_file);
#if defined(WIN32_FD_HANDLES)
  GC_REG_TRAV(scheme_rt_tcp_select_info, mark_tcp_select_info);
#endif
  GC_REG_TRAV(scheme_rt_output_file, mark_output_file);

#ifdef MZ_FDS
  GC_REG_TRAV(scheme_rt_input_fd, mark_input_fd);
#endif

#if defined(UNIX_PROCESSES)
  GC_REG_TRAV(scheme_rt_system_child, mark_system_child);
#endif

#ifdef USE_OSKIT_CONSOLE
  GC_REG_TRAV(scheme_rt_oskit_console_input, mark_oskit_console_input);
#endif

  GC_REG_TRAV(scheme_subprocess_type, mark_subprocess);
  GC_REG_TRAV(scheme_write_evt_type, mark_read_write_evt);
}

END_XFORM_SKIP;

#endif
