#lang scribble/doc
@(require scribble/manual
          "common.rkt"
          (for-label racket/base
                     racket/contract
                     setup/dirs))

@title[#:tag "config-file"]{Installation Configuration and Search Paths}

A @deftech{configuration directory} path is built into the Racket executable as
selected at install time, and its location can be changed via the
@envvar{PLTCONFIGDIR} directory or @DFlag{config}/@Flag{G}
command-line flag. Use @racket[find-config-dir] to locate the
configuration directory.

Modify the @as-index{@filepath{config.rktd}} file in the @tech{configuration directory}
to configure other directories as described below. Use the
@racketmodname[setup/dirs] library (which combines information from
the configuration files and other sources) to locate configured
directories, instead of reading @filepath{config.rktd} directly.
A @filepath{config.rktd} file can also appear in the directory
@racket[(build-path (find-system-path 'addon-dir) "etc")], but it
controls only the results of @racket[find-addon-tethered-console-bin-dir] and
@racket[find-addon-tethered-gui-bin-dir].

The path of the @deftech{main collection directory} is built into the
Racket executable, and it can be changed via the
@DFlag{collects}/@Flag{X} flag, so it has no entry in
@filepath{config.rktd}. Most paths that are specified in
@filepath{config.rktd} have default values that are relative to the
main collection directory. The paths of the @tech{configuration directory} and
@tech{main collection directory} thus work together to determine a
Racket configuration.

A @filepath{config.rktd} file in the configuration directory should
contain a @racket[read]able hash table with any of the following
symbolic keys, where a relative path is relative to the @tech{main collection
directory}:

@itemlist[

 @item{@indexed-racket['installation-name] --- a string for the installation
       name, which is used to determine user- and version-specific paths,
       such as the initial path produced by @racket[find-library-collection-paths]
       and the location of packages that are installed in @exec{user} 
       @tech[#:doc '(lib "pkg/scribblings/pkg.scrbl")]{package
       scope}. The default is @racket[(version)].}

 @item{@indexed-racket['lib-dir] --- a path, string, or byte string for the
       @deftech{main library directory}. It defaults to a @filepath{lib}
       sibling directory of the @tech{main collection directory}.}

 @item{@indexed-racket['lib-search-dirs] --- a list of paths, strings, byte
       strings, or @racket[#f] representing the search path for
       directories containing foreign libraries. Each @racket[#f] in
       the list, if any, is replaced with the default search path,
       which is the user- and version-specific @filepath{lib}
       directory followed by the @tech{main library directory}.}

 @item{@indexed-racket['dll-dir] --- a path, string, or byte string for a
       directory containing shared libraries for the main
       executable. It defaults to the @tech{main library directory}.}

 @item{@indexed-racket['share-dir] --- a path, string, or byte string for the
       @deftech{main shared-file directory}, which normally includes installed packages.
       It defaults to a @filepath{share} sibling directory of the main
       collection directory.}

 @item{@indexed-racket['links-file] --- a path, string, or byte string for the
       @tech[#:doc reference-doc]{collection links file}. It defaults
       to a @filepath{links.rktd} file in the @tech{main shared-file directory}.}

 @item{@indexed-racket['links-search-files] --- like @racket['lib-search-dirs],
       but for @tech[#:doc reference-doc]{collection links file}.}

 @item{@indexed-racket['pkgs-dir] --- a path, string, or byte string
       for packages that have @exec{installation} @tech[#:doc '(lib
       "pkg/scribblings/pkg.scrbl")]{package scope}. It defaults to
       @filepath{pkgs} in the main shared-file directory.}

 @item{@indexed-racket['pkgs-search-dirs] --- similar to
       @racket['lib-search-dirs], but for packages in roughly
       @exec{installation} @tech[#:doc '(lib
       "pkg/scribblings/pkg.scrbl")]{package scope}. More precisely, a
       @racket[#f] value in the list is replaced with the directory
       specified by @racket['pkgs-dir], and that point in the search
       list corresponds to @exec{installation} scope. Paths before or
       after a @racket[#f] value in the list can be selected as a
       scopes to start searches at that path's point in the list.
       Directories listed in @racket['pkgs-search-dirs] typically oblige
       a corresponding entry in @racket['links-search-files], where
       the corresponding entry is @filepath{links.rktd} within the
       directory.

       @history[#:changed "7.0.0.19" @elem{Adapt the package-search path in
                                           a general way for a directory scope.}]}

 @item{@indexed-racket['bin-dir] --- a path, string, or byte string for the
       installation's directory containing executables. It defaults to a
       @filepath{bin} sibling directory of the @tech{main collection
       directory}.}

 @item{@indexed-racket['gui-bin-dir] --- a path, string, or byte
       string for the installation's directory containing GUI
       executables. It defaults to a the @racket['bin-dir] value, if
       configured, or otherwise defaults in a platform-specific way:
       to the @filepath{bin} sibling directory of the @tech{main
       collection directory} on Unix, and to the parent of the
       @tech{main collection directory} on Windows and Mac OS.

       @history[#:added "6.8.0.2"]}

 @item{@indexed-racket['apps-dir] --- a path, string, or byte string
       for the installation's directory for @filepath{.desktop} files.
       It defaults to a @filepath{applications} subdirectory of the
       @tech{main shared-file directory}.}

 @item{@indexed-racket['man-dir] --- a path, string, or byte string for the
       installation's man-page directory. It defaults to a @filepath{man}
       sibling directory of the @tech{main collection directory}.}

 @item{@indexed-racket['doc-dir] --- a path, string, or byte string for the
       main documentation directory. The value defaults to a
       @filepath{doc} sibling directory of the
       @tech{main collection directory}.}

 @item{@indexed-racket['doc-search-dirs] --- like @racket['lib-search-dirs],
       but for directories containing documentation.}

 @item{@indexed-racket['doc-search-url] --- a URL string that is augmented
       with version and search-tag queries to form a remote
       documentation reference.}

 @item{@indexed-racket['doc-open-url] --- a URL string or @racket[#f];
       a string supplies a URL that is used instead of a local path to
       search and maybe open documentation pages (which normally makes
       sense only in an environment where opening a local HTML file
       does not work).}

 @item{@indexed-racket['include-dir] --- a path, string, or byte string for
       the main directory containing C header files. It defaults to an
       @filepath{include} sibling directory of the @tech{main collection
       directory}.}

 @item{@indexed-racket['include-search-dirs] --- like
       @racket[doc-search-dirs], but for directories containing C
       header files.}

 @item{@indexed-racket['catalogs] --- a list of URL strings used as the search
       path for resolving package names. An @racket[#f] in the list
       is replaced with the default search path. A string that does not
       start with alphabetic characters followed by @litchar{://} is
       treated as a path, where a relative path is relative to the
       configuration directory.}

 @item{@indexed-racket['default-scope] --- either @racket["user"] or
       @racket["installation"], determining the default @tech[#:doc
       '(lib "pkg/scribblings/pkg.scrbl")]{package scope} for
       package-management operations.}

 @item{@indexed-racket['download-cache-dir] --- a path string used as
       the location for storing downloaded package archives. When not
       specified, packages are cached in a @filepath{download-cache}
       directory in the user's cache directory as reported by
       @racket[(find-system-path 'cache-dir)].}

 @item{@indexed-racket['download-cache-max-files] and
       @indexed-racket['download-cache-max-bytes] --- real numbers that
       determine limits on the download cache. When not specified, the
       cache is allowed to hold up to 1024 files that total up to
       64@|~|MB.}

 @item{@indexed-racket['build-stamp] --- a string that identifies a build,
       which can be used to augment the Racket version number to more
       specifically identify the build. An empty string is normally
       appropriate for a release build.}

 @item{@indexed-racket['absolute-installation?] --- a boolean that is
       @racket[#t] if the installation uses absolute path names,
       @racket[#f] otherwise.}

 @item{@indexed-racket['cgc-suffix] --- a string used as the suffix (before
       the actual suffix, such as @filepath{.exe}) for a
       @filepath{CGC} executable. Use Windows-style casing, and the
       string will be downcased as appropriate (e.g., for a Unix
       binary name). A @racket[#f] value means that if the
       @exec{racket} binary identifies itself as CGC, then the suffix
       is @racket[""], otherwise it is @racket["CGC"].}

 @item{@indexed-racket['3m-suffix] --- analogous to @racket['cgc-suffix], but
       for 3m. A @racket[#f] value means that if the @exec{racket}
       binary identifies itself as 3m, then the suffix is
       @racket[""], otherwise it is @racket["BC"].}

 @item{@indexed-racket['cs-suffix] --- analogous to @racket['cgc-suffix], but
       for CS. A @racket[#f] value means that if the @exec{racket}
       binary identifies itself as CS, then the suffix is
       @racket[""], otherwise it is @racket["CS"].}

 @item{@indexed-racket['config-tethered-console-bin-dir] and
       @indexed-racket['config-tethered-gui-bin-dir] --- a path for a
       directory to hold extra copies of executables that are tied to the
       configuration directory (as reported by @racket[find-config-dir])
       that is active at the time the executables are created. See also
       @racket[find-config-tethered-console-bin-dir] and
       @racket[find-config-tethered-gui-bin-dir].}

  @item{@indexed-racket['interactive-file] and
        @indexed-racket['gui-interactive-file] --- a module path
        to the interactive module that runs when the REPL
        runs on startup, unless the
        @Flag{q}/@DFlag{no-init-file} is provided. Defaults to
        @racket['racket/interactive] and
        @racket['racket/gui/interactive].}

  ]
