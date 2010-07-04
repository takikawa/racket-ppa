#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base)
          "common.ss")

@(define (xflag str) (as-index (DFlag str)))
@(define (pxflag str) (as-index (DPFlag str)))

@title[#:tag "cc"]{Compiling and Linking C Extensions}

A @deftech{dynamic extension} is a shared library (a.k.a. DLL) that
extends PLT Scheme using the C API. An extension can be loaded
explicitly via @scheme[load-extension], or it can be loaded implicitly
through @scheme[require] or @scheme[load/use-compiled] in place of a
source @scheme[_file] when the extension is located at

@schemeblock[
(build-path "compiled" "native" (system-library-subpath)
            (path-add-suffix _file (system-type 'so-suffix)))
]

relative to @scheme[_file].

For information on writing extensions, see @other-manual[inside-doc].

Three @|mzc| modes help for building extensions:

@itemize[

 @item{@DFlag{cc} : Runs the host system's C compiler, automatically
       supplying flags to locate the PLT Scheme header files and to
       compile for inclusion in a shared library.}

 @item{@DFlag{ld} : Runs the host system's C linker, automatically
       supplying flags to locate and link to the PLT Scheme libraries
       and to generate a shared library.}

 @item{@DFlag{xform} : Transforms C code that is written without
       explicit GC-cooperation hooks to cooperate with PLT Scheme's 3m
       garbage collector; see @secref[#:doc inside-doc "overview"] in
       @other-manual[inside-doc].}

]

Compilation and linking build on the @schememodname[dynext/compile]
and @schememodname[dynext/link] libraries. The following @|mzc| flags
correspond to setting or accessing parameters for those libraries: @xflag{tool},
@xflag{compiler}, @xflag{ccf}, @xflag{ccf}, @xflag{ccf-clear},
@xflag{ccf-show}, @xflag{linker}, @pxflag{ldf}, @xflag{ldf},
@xflag{ldf-clear}, @xflag{ldf-show}, @pxflag{ldl}, @xflag{ldl-show},
@pxflag{cppf}, @pxflag{cppf} @pxflag{cppf-clear}, and @xflag{cppf-show}.

The @as-index{@DFlag{3m}} flag specifies that the extension is to be
loaded into the 3m variant of PLT Scheme. The @as-index{@DFlag{cgc}}
flag specifies that the extension is to be used with the CGC. The
default depends on @|mzc|: @DFlag{3m} if @|mzc| itself is running in
3m, @DFlag{cgc} if @|mzc| itself is running in CGC.
