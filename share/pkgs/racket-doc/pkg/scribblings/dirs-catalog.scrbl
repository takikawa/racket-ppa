#lang scribble/manual
@(require "common.rkt"
          scribble/bnf
          (for-label pkg/dirs-catalog
                     racket/base
                     racket/contract/base))

@title[#:tag "dirs-catalog"]{Package Directories Catalog}

@defmodule[pkg/dirs-catalog]{The @racketmodname[pkg/dirs-catalog] module
provides @racket[create-dirs-catalog], which generates a @tech{package
catalog} (as a directory) that refers to a set of packages that are
found in a given set of directories. Packages are discovered in the
given directory as subdirectories that have an @filepath{info.rkt}
file.}

For example, the main Racket development repository includes a
@filepath{pkgs} directory that holds packages such as @pkgname{base},
and @racket[create-dirs-catalog] constructs a catalog to be used to
install those packages.

When run directly as a program, @racketmodname[pkg/dirs-catalog] expects a
destination catalog followed by any number paths for directories that
hold packages:

@commandline{racket -l- pkg/dirs-catalog @nonterm{dest-catalog} @nonterm{dir} ...}

The @DFlag{immediate}, @DFlag{link}, @DFlag{merge}, @DFlag{check-metadata}, and
@DFlag{quiet} flags correspond to optional keyword arguments of
@racket[create-dirs-catalog].

@history[#:added "6.1.1.6"
         #:changed "6.90.0.4" @elem{Added @DFlag{immediate}.}]

@defproc[(create-dirs-catalog [catalog-path path-string?]
                              [dirs (listof path-string?)]
                              [#:immediate? immediate? any/c #f]
                              [#:link? link? any/c #f]
                              [#:merge? merge? any/c #f]
                              [#:check-metadata? check-metadata? any/c #f]
                              [#:status-printf status-printf (string? any/c ... -> void?) void])
           void?]{

Creates or modifies @racket[catalog-path] as a directory that works as
a catalog (see @secref["catalog-protocol"]) to list the packages that
are contained in each directory specified by @racket[dirs]. Packages
are discovered in @racket[dirs] as subdirectories that have an
@filepath{info.rkt} file; if @racket[immediate?] is true, then each
directory is @racket[dirs] is checked for an immediate @filepath{info.rkt}
file before checking subdirectories.

If @racket[link?] is true, then the catalog specifies that the package
should be installed as a directory link, as opposed to copies.

If @racket[merge?] is true, then existing catalog entries in
@racket[catalog-path] are preserved, otherwise old catalog entries are
removed.

To create author and description information for each package in the
catalog, @racket[create-dirs-catalog] looks for a @racket[pkg-authors]
and @racket[pkg-desc] definition in each package's @filepath{info.rkt}
file. If either definition is missing and @racket[check-metadata?] is
true, an error is reported.

If a package's @filepath{info.rkt} file defines @racket[pkg-name],
then the defined name is used as the package's name instead of the
package directory's name.

@history[#:changed "6.90.0.4" @elem{Added the @racket[#:immediate] argument.}
         #:changed "7.3.0.2" @elem{Added support for @racket[pkg-name] to name a package.}]}
