#lang scribble/doc
@(require "utils.rkt")

@title[#:tag "cs-start"]{Starting and Declaring Initial Modules}

As sketched in @secref["cs-embedding"], and embedded instance of
Racket CS is started with @cppi{racket_boot}. Functions such as
@cppi{racket_embedded_load_bytes} help to initialize a Racket
namespace with already-compiled modules.

For functions and struct fields that contain a path in @cpp{char*}
form, the path is treated as UTF-8 encoded on Windows.

@section[#:tag "cs-boot-arguments"]{Boot and Configuration}

@function[(void racket_boot [racket_boot_arguments_t* boot_args])]{

Initializes a Racket CS instance. A main thread is created and then
suspended, waiting for further evaluation via @cppi{racket_apply},
@cppi{racket_eval}, and similar functions.

A @cpp{racket_boot_arguments_t} struct contains fields to specify how
@cppi{racket_boot} should initialize a Racket instance. New fields may
be added in the future, but in that case, a @cpp{0} or @cpp{NULL}
value for a field will imply backward-compatible default.

Fields in @cppdef{racket_boot_arguments_t}:

@itemlist[

 @item{@cpp{const char *} @cppdef{boot1_path} --- a path to a file
       containing a Chez Scheme image file with base functionality.
       Normally, the file is called @filepath{petite.boot}. The path
       should contain a directory separator, otherwise Chez Scheme
       will consult its own search path.}

 @item{@cpp{long} @cppdef{boot1_offset} --- an offset into
       @cpp{boot1_path} to read for the first boot image, which allows
       boot images to be combined with other data in a single file.
       The image as distributed is self-terminating, so no size or
       ending offset is needed.}

 @item{@cpp{const char *} @cppdef{boot2_path} --- like
       @cpp{boot1_path}, but for the image that contains compiler
       functionality, normally called @filepath{scheme.boot}.}

 @item{@cpp{long} @cppdef{boot2_offset} --- an offset into
       @cpp{boot2_path} to read for the second boot image.}
       
 @item{@cpp{const char *} @cppdef{boot3_path} --- like
       @cpp{boot1_path}, but for the image that contains Racket
       functionality, normally called @filepath{racket.boot}.}

 @item{@cpp{long} @cppdef{boot3_offset} --- an offset into
       @cpp{boot2_path} to read for the thirf boot image.}

 @item{@cpp{int} @cpp{argc} and @cpp{char **} @cpp{argv} ---
       command-line arguments to be processed the same as for a
       stand-alone @exec{racket} invocation. If @var{argv} is
       @cpp{NULL}, the command line @exec{-n} is used, which loads
       boot files without taking any further action.}

  @item{@cpp{const char *} @cppdef{exec_file} --- a path to use for
       @racket[(system-type 'exec-file)], usually @cpp{argv[0]} using
       the @cpp{argv} delivered to a program's @cpp{main}. This
       field must not be @cpp{NULL}.}

  @item{@cpp{const char *} @cppdef{run_file} --- a path to use for
       @racket[(system-type 'run-file)]. If the field is @cpp{NULL},
       the value of @cppi{exec_file} is used.}

  @item{@cpp{const char *} @cppdef{collects_dir} --- a path to use as
       the main @filepath{collects} directory for locating library
       collections. If this field holds @cpp{NULL} or @cpp{""}, then
       the library-collection search path is initialized as empty.}

  @item{@cpp{const char *} @cppdef{config_dir} --- a path to used as an
       @filepath{etc} directory that holds configuration information,
       including information about installed packages. If the value if
       @cpp{NULL}, @cpp{"etc"} is used.}

  @item{@cpp{wchar_t *} @cppdef{dll_dir} --- a path used to find DLLs,
       such as @exec{iconv} support. Note that this path uses wide
       characters, not a UTF-8 byte encoding.}

  @item{@cpp{int} @cppdef{cs_compiled_subdir} --- A true value indicates
       that the @racket[use-compiled-file-paths] parameter should be
       initialized to have a platform-specific subdirectory of
       @filepath{compiled}, which is used for a Racket CS installation
       that overlays a Racket BC installation.}

]}

@; ----------------------------------------------------------------------

@section[#:tag "cs-embedded-load"]{Loading Racket Modules}

@together[(
@function[(void racket_embedded_load_bytes [const-char* code] [uptr len] [int as_predefined])]
@function[(void racket_embedded_load_file [const-char* path] [int as_predefined])]
@function[(void racket_embedded_load_file_region [const-char* path] [uptr start] [uptr end] [int as_predefined])]
)]{

These functions evaluate Racket code, either in memory as @var{code}
or loaded from @var{path}, in the initial Racket thread. The intent is
that the code is already compiled. Normally, also, the contains module
declarations. The @seclink["c-mods" #:doc raco-doc]{@exec{raco ctool
--c-mods}} and @seclink["c-mods" #:doc raco-doc]{@exec{raco ctool
--mods}} commands generate code suitable for loading with these
functions, and @DFlag{c-mods} mode generates C code that calls
@cppi{racket_embedded_load_bytes}.

If @var{as_predefined} is true, then the code is loaded during the
creation of any new Racket @tech[#:doc reference-doc]{place} in the
new place, so that modules declared by the code are loaded in the new
place, too.}
