#lang scribble/doc
@(require "common.rkt" (for-label file/tar file/gzip racket/file))

@title[#:tag "tar"]{@exec{tar} File Creation}

@defmodule[file/tar]{The @racketmodname[file/tar] library provides
utilities to create archive files in USTAR format, like the archive
that the Unix utility @exec{pax} generates. Long paths are supported
using either the POSIX.1-2001/pax or GNU format for long paths. The
resulting archives contain only directories, files, and symbolic
links, and owner information is not preserved; the owner that is
stored in the archive is always ``root.''

Symbolic links (on Unix and Mac OS) are not followed by default.}


@defproc[(tar [tar-file path-string?]
              [path path-string?] ...
              [#:follow-links? follow-links? any/c #f]
              [#:exists-ok? exists-ok? any/c #f]
              [#:format format (or/c 'pax 'gnu 'ustar) 'pax]
              [#:path-prefix path-prefix (or/c #f path-string?) #f]
              [#:path-filter path-filter (or/c #f (path? . -> . any/c)) #f]
              [#:timestamp timestamp (or/c #f exact-integer?) #f]
              [#:get-timestamp get-timestamp
                               (path? . -> . exact-integer?)
                               (if timestamp
                                   (lambda (p) timestamp)
                                   file-or-directory-modify-seconds)])
         exact-nonnegative-integer?]{

Creates @racket[tar-file], which holds the complete content of all
@racket[path]s.  The given @racket[path]s are all expected to be
relative paths for existing directories and files (i.e., relative
to the current directory).  If a nested path is provided as a
@racket[path], its ancestor directories are also added to the
resulting tar file, up to the current directory (using
@racket[pathlist-closure]). If @racket[follow-links?] is false, then
symbolic links are included in the resulting tar file as links.

If @racket[exists-ok?] is @racket[#f], then an exception is raised if
@racket[tar-file] exists already. If @racket[exists-ok?] is true, then
@racket[tar-file] is truncated or replaced if it exists already.

The @racket[format] argument determines the handling of long paths and
long symbolic-link targets. If @racket[format] is @racket['pax], then
POSIX.1-2001/pax extensions are used. If @racket[format] is
@racket['gnu], then GNU extensions are used. If @racket[format] is
@racket['ustar], then @racket[tar] raises an error for too-long paths
or symbolic-link targets.

If @racket[path-prefix] is not @racket[#f], then it is prefixed to
each path in the archive.

The @racket[get-timestamp] function is used to obtain the modification
date to record in the archive for each file or directory.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.1.1.1" @elem{Added the @racket[#:exists-ok?] argument.}
         #:changed "6.3.0.3" @elem{Added the @racket[#:follow-links?] argument.}
         #:changed "6.3.0.11" @elem{Added the @racket[#:path-filter] argument.}
         #:changed "6.7.0.4" @elem{Added the @racket[#:format] argument and
                                   effectively changed its default from @racket['ustar]
                                   to @racket['pax].}
         #:changed "7.3.0.3" @elem{Added the @racket[#:timestamp] argument.}]}


@defproc[(tar->output [paths (listof path?)]
                      [out output-port? (current-output-port)]
                      [#:follow-links? follow-links? any/c #f]
                      [#:format format (or/c 'pax 'gnu 'ustar) 'pax]
                      [#:path-prefix path-prefix (or/c #f path-string?) #f]
                      [#:path-filter path-filter (or/c #f (path? . -> . any/c)) #f]
                      [#:timestamp timestamp (or/c #f exact-integer?) #f]
                      [#:get-timestamp get-timestamp
                                       (path? . -> . exact-integer?)
                                       (if timestamp
                                           (lambda (p) timestamp)
                                           file-or-directory-modify-seconds)])
         exact-nonnegative-integer?]{

Like @racket[tar], but packages each of the given @racket[paths] in a @exec{tar} format
archive that is written directly to the @racket[out].  The specified
@racket[paths] are included as-is (except for adding @racket[path-prefix], if any); if a directory is specified, its
content is not automatically added, and nested directories are added
without parent directories.

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.3.0.3" @elem{Added the @racket[#:follow-links?] argument.}
         #:changed "6.3.0.11" @elem{Added the @racket[#:path-filter] argument.}
         #:changed "6.7.0.4" @elem{Added the @racket[#:format] argument and
                                   effectively changed its default from @racket['ustar]
                                   to @racket['pax].}
         #:changed "7.3.0.3" @elem{Added the @racket[#:timestamp] argument.}]}


@defproc[(tar-gzip [tar-file path-string?]
                   [paths path-string?] ...
                   [#:follow-links? follow-links? any/c #f]
                   [#:exists-ok? exists-ok? any/c #f]
                   [#:format format (or/c 'pax 'gnu 'ustar) 'pax]
                   [#:path-prefix path-prefix (or/c #f path-string?) #f]
                   [#:timestamp timestamp (or/c #f exact-integer?) #f]
                   [#:get-timestamp get-timestamp
                                    (path? . -> . exact-integer?)
                                    (if timestamp
                                        (lambda (p) timestamp)
                                        file-or-directory-modify-seconds)])
         void?]{

Like @racket[tar], but compresses the resulting file with @racket[gzip].

@history[#:changed "6.0.0.3" @elem{Added the @racket[#:get-timestamp] argument.}
         #:changed "6.1.1.1" @elem{Added the @racket[#:exists-ok?] argument.}
         #:changed "6.3.0.3" @elem{Added the @racket[#:follow-links?] argument.}
         #:changed "6.7.0.4" @elem{Added the @racket[#:format] argument and
                                   effectively changed its default from @racket['ustar]
                                   to @racket['pax].}
         #:changed "7.3.0.3" @elem{Added the @racket[#:timestamp] argument.}]}
