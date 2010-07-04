#lang scribble/doc

@(require scribble/manual
          scribble/bnf
	  scribble/eval
          (for-label scheme)
          (for-label planet/config)
          (for-label planet/util))

@(define-syntax-rule (eg (code resl) ...)
   (interaction
     (eval:alts code resl)
     ...))

@title{@bold{PLaneT}: Automatic Package Distribution}

@author["Jacob Matthews" "Robert Bruce Findler"]

The @PLaneT system is a method for automatically sharing code packages,
both as libraries and as full applications, that gives every user of a
@PLaneT client the illusion of having a local copy of every code
package on the server. It
consists of @link["http://planet.plt-scheme.org/"]{the central @PLaneT
package repository}, a server that holds all PLaneT packages, and
the PLaneT client, built into PLT Scheme, which transparently
interacts with the server on your behalf when necessary.

@table-of-contents[]

@section{Using PLaneT}

To use a @PLaneT package in a program, require it using the
@scheme[planet] @scheme[require] form (see @(secref "require" #:doc
'(lib "scribblings/reference/reference.scrbl")) for a full reference
on the features of the @scheme[require] statement in general and the
exact allowed grammar of PLaneT require statements). Here we explain
how to use PLaneT by example.

@subsection[#:tag "finding-a-package"]{Finding a Package}

If you are new to PLaneT, the first thing to do is visit
@link["http://planet.plt-scheme.org/"]{the PLaneT repository web site}
and see what packages are available. People contribute new PLaneT
packages all the time --- if you want to be notified whenever a new or
updated package is released, you can subscribe to the
(announcement-only) 
@link["http://mailman.cs.uchicago.edu/mailman/listinfo/planet-announce"]{PLaneT-announce mailing list} 
or use an RSS reader to subscribe to 
@link["http://planet.plt-scheme.org/300/planet.rss"]{PLaneT's RSS feed}.

To use a package from PLaneT in your program, the easiest thing to do
is copy the @scheme[require] code snippet off of that package's page
and paste it ino your program. For instance, to use Schematics'
@link["http://planet.plt-scheme.org/users/schematics/spgsql.plt"]{spgsql.plt}
package (a library for interacting with the
@link["http://www.postgresql.org/"]{PostgresQL} database), as of this
writing you would copy and paste the line:

@schemeblock[(require (planet "spgsql.ss" ("schematics" "spgsql.plt" 2 3)))]

into your program. This line requires the file @filepath{spgsql.ss} in package
version 2.3 of the @filepath{spgsql.plt} package written by
@filepath{schematics}. That does two things: first, it downloads and
installs a version of @filepath{spgsql.plt} that is compatible with
package version 2.3 from @link["http://planet.plt-scheme.org/"]{the
central PLaneT repository} if a compatible version hasn't already been
installed. Second, it requires the module in file @filepath{spgsql.ss}
from that package, making all of its exported bindings available for use.

Unlike with most package-distribution systems, package downloading and
installation in PLaneT is @emph{transparent}: there's no need for
you to do anything special the first time you want to use a package,
and there's no need for you to even know whether or not a particular
package is installed on your computer or the computers where your code
will be deployed.

@subsection{Shorthand Syntax}

As of PLT Scheme version 4.0, the code snippet above can also be
written using a new shorter syntax:

@schemeblock[(require (planet schematics/spgsql:2:3/spgsql))]

The two forms behave identically. In the abbreviated syntax, however,
it is illegal to write the trailing @filepath{.ss} suffix on the file
name to be required or the trailing @filepath{.plt} on the package file
name. (They are mandatory for the long-form syntax.) It is also legal
in the abbreviated syntax to omit a filename to be required entirely;
in that case, PLaneT requires the file @filepath{main.ss} in the given
package.

@subsection{Networking troubles}

Sometimes, when PLaneT tries to download and install a
package for the first time, your operating system may block
it from access to the network. If you are uncomfortable
giving DrScheme free access to the network (or if your
attempts to do so do not seem to work), then you can use
your browser to manually install a planet package.

To see how this works, lets assume you want to install the PLAI package
and @schemeblock[(require (planet plai/plai:1))] is not working for you.
@itemize[
@item{First, 
fire up a command-line window and use @tt{planet url} to 
determine the url for downloading the package. 
To find the url for version @tt{(1 1)} of the plai package,
do this:

@tt{% planet url plai plai.plt 1 1}

and get this as a response:

@tt{http://planet.plt-scheme.org/servlets/planet-servlet.ss?lang=%224.1.5.3%22&name=%22plai.plt%22&maj=1&min-lo=1&min-hi=%23f&path=%28%22plai%22%29}}

@item{Copy and paste that url into your browser, which
should trigger the dowload of a file called
@tt{plai.plt}. Note that your browser will probably try to
call the file something else. Rename it to @tt{plai.plt}.}

@item{Now run the command-line tool one more time to install the plt file:

@tt{% planet fileinject plai plai.plt 1 1}

This command should be run from the same directory where you saved @tt{plai.plt}.

This command may fail, since version @tt{(1 1)} of the PLAI
package depends on @tt{cce/scheme:4:1}. If it does, simply
repeat the above steps for that package first, and then
continue with the @tt{fileinject} command for PLAI.}

@item{Finally, to check that the installation is successful,
run @tt{planet show}. You should see output like this
(possibly with slightly different version numbers, if the
packages have been updated since this was written):
@verbatim{
Normally-installed packages:
  cce	scheme.plt	4 1
  plai	plai.plt	1 1
}}
]

Once that is complete, PLaneT will use that version of the
package for any subsequent @scheme[require]s and won't try
to use the network.

@subsection{Fine-Grained Control Over Package Imports}

The PLaneT client is designed to balance two competing goals:
transparent upgradability and control over the effect of a package
requirement. To that end, the most basic PLaneT require form offers
maximum upgradability, but several more specialized forms allow
finer-grained control over what versions of the named package may be
downloaded.

@margin-note{Package versions should not be confused with program or library
versions; a @italic{package version} is a PLaneT-specific version
number that encodes backwards-compatibility information.}

The most basic planet require line, which is what is used in the form

@schemeblock[(require (planet "spgsql.ss" ("schematics" "spgsql.plt" 2 3)))]

in longhand notation, or 

@schemeblock[(require (planet schematics/spgsql:2:3/spgsql))]

in shorthand notation, should be read ``Require from PLaneT
@italic{any} release of Schematics' @filepath{spgsql.plt} package that
is backwards-compatible with package version 2.3.'' (The actual
package version used is determined by @seclink["search-order"]{the
PLaneT search order}.) To signal this explicitly, it is possible to
write

@schemeblock[(require (planet "spgsql.ss" ("schematics" "spgsql.plt" 2 (+ 3))))]

or

@schemeblock[(require (planet schematics/spgsql:2:>=3/spgsql))]

both of which mean the same thing as the first pair of require lines.

@margin-note{See @secref{backwards-compatibility} for a more detailed discussion of
backwards-compatibility obligations for PLaneT packages.}
The notion of ``backwards-compatibility'' has a specific meaning in
PLaneT: by definition, for the purposes of automation, a package is
considered to be backwards-compatible with any other package of the
same owner, name, and major version, and any @italic{lower} minor
version. Package maintainers are responsible for marking new releases
that break backwards-compatibility by incrementing their major-version
number. This means that all of the above require specifications will
match any release of @filepath{unlib.plt} with major package version 3
(and any minor version), but will @italic{never} match releases of
@filepath{unlib.plt} with higher (or lower) major version numbers.

Of course a package author may make a mistake and introduced a
backwards-incompatibility unintentionally, or may fix a bug that code
in third-party libraries was already working around. In those cases,
it may help to make use of the ``upper bound'' form of the planet
require, in longhand form:

@schemeblock[(require (planet "reduction-semantics.ss" 
                              ("robby" "redex.plt" 4 (- 3))))]

and using shorthand notation:

@schemeblock[(require (planet robby/redex:4:<=3/reduction-semantics))]

In this require line, any version of the package @filepath{redex.plt}
from package version 4.0 to package version 4.3 will match the require
spec (though as with any PLaneT require specification,
@seclink["search-order"]{the PLaneT package search order} determines
which package is actually loaded).

It is also possible to specify both an upper and a lower bound, using
the planet require's ``range'' form:

@schemeblock[(require (planet "test.ss" ("schematics" "schemeunit.plt" 2 (9 10))))]

or

@schemeblock[(require (planet schematics/schemeunit:2:9-10/test))]

This form matches any package in the specified range (inclusive on
both ends), in this example the specifications match either package
version 2.9 or 2.10 of the @filepath{schemeunit.plt} package, but do
not match version with higher or lower minor version numbers (or any
other major version number).

Using the range form, it is possible to require a specific version of
a package as a special case (choosing the upper and lower bounds to be
equal), but this is a common enough case that it has special support
with the ``exact-match'' form:

@schemeblock[(require (planet "unzip.ss" ("dherman" "zip.plt" 2 (= 1))))]

or

@schemeblock[(require (planet dherman/zip:2:=1/unzip))]

match only the exact package version 2.1 of the @filepath{zip.plt} package.

@;@subsection{Linkage}

@;@subsection{The Diamond Property}

@subsection{Monitoring PLaneT's progress}

PLaneT logs information about what it is doing to the @tt{info}
log (via @scheme[log-info]). 
In DrScheme, you can view the logs from the @onscreen{Show Log}
menu item in the @onscreen{View} menu, and MzScheme's logging output
can be controlled via command-line options and via environment
variables. See 
@secref["logging" #:doc '(lib "scribblings/reference/reference.scrbl")] 
for more details.

@section[#:tag "search-order"]{The PLaneT Search Order}

PLaneT has four strategies it uses in order to match a request with an
appropriate package that.

@subsection{Previous Linkage}

Whenever a file requires a package via PLaneT and that requirement is
satisfied, the system makes a note of exactly which package satisfied
that requirement and from then on always uses that exact same package,
even if a newer version is available. This is done to prevent "magic
upgrades" in which a program stops working after installation because
an unrelated package was installed. Such connections are called links
and are stored in a user-specific table called the linkage table.

@subsection{Acceptable Local Package}

If the PLaneT client doesn't have any previous linkage information, it
checks its list of already-installed PLaneT packages for one that
meets the requirement, and uses it if available. Both PLaneT-installed
packages and packages established through a development link 
(see @secref{devlinks})
are checked simultaneously at this stage.

@subsection{Acceptable Remote Package}

If there is no acceptable local package, the PLaneT client sends 
a request to the PLaneT server for a new package that would satisfy
the requirement. The server then finds the newest matching package
and sends it back to the client, which then installs it and uses
it to satisfy the original requirement.

@subsection{Cached Installation Archive}

If the remote server cannot be contacted (or fails in any way to
deliver an acceptable package), the PLaneT client consults the 
uninstalled-packages cache, a cache of all previously-downloaded
packages, even those that are not currently installed. PLT Scheme 
users who frequently upgrade their installations may have many 
packages downloaded but not installed at any given time; this step 
is intended to ensure that these users can still run programs even
if they temporarily lose network connection.

@section[#:tag "cmdline"]{The @exec{planet} Command-Line Tool}

The @exec{planet} command-line tool allows a command-line interface to
the most commonly-performed PLaneT tasks. It is invoked from the
command line as

@commandline{planet @italic{subcommand} arg ...}

where @italic{command} is a subcommand from the following list, and
@exec{arg} is a sequence of arguments determined by that subcommand:

@(define (cmd name desc)
    @item{@(seclink name (exec name)): @desc})

@itemize[
  @cmd["create"]{create a PLaneT archive from a directory}
  @cmd["install"]{download and install a given package}
  @cmd["remove"]{remove the specified package from the local cache}
  @cmd["show"]{list the packages installed in the local cache}
  @cmd["clearlinks"]{clear the linkage table, allowing upgrades}
  @cmd["fileinject"]{install a local file to the planet cache}
  @cmd["link"]{create a development link}
  @cmd["unlink"]{remove development link associated with the given package}
  @cmd["fetch"]{download a package file without installing it}
  @cmd["url"]{get a URL for the given package}
  @cmd["open"]{unpack the contents of the given package}
  @cmd["structure"]{display the structure of a given .plt archive}
  @cmd["print"]{display a file within of the given .plt archive}]

Each of these commands is described in more detail below. All the
functionality of the command-line tool is also provided with a programmatic interface by 
@seclink["util.ss"]{the @filepath{util.ss} library}.

@subsection[#:tag "create"]{@exec{create}}

Usage:
@commandline{planet create [ <option> ... ] <path>}
Create a PLaneT archive in the current directory whose contents are the
directory @exec{<path>}.

@exec{<option>} is one of:
@itemize[
  @item{@exec{-f, --force}: force a package to be created even if its info.ss file contains
    errors.}]

@subsection[#:tag "install"]{@exec{install}}

Usage:
@commandline{planet install <owner> <pkg> <maj> <min>}
Download and install the package that @scheme[(require (planet "file.ss" (<owner> <pkg> <maj> <min>)))]
would install.

@subsection[#:tag "remove"]{@exec{remove}}

Usage:
@commandline{planet remove [ <option> ... ] <owner> <pkg> <maj> <min>}
Remove the specified package from the local cache, optionally also removing its
distribution file.

@exec{<option>} is one of:
@itemize[
	@item{@exec{-e, --erase}: also remove the package's distribution file from the
    uninstalled-package cache}]

@subsection[#:tag "show"]{@exec{show}}

Usage:
@commandline{planet show [ <option> ... ]}
List the packages installed in the local cache.

@exec{<option>} is one of:
@itemize[
  @item{@exec{-p, --packages}: show packages only (default)}
  @item{@exec{-l, --linkage}: show linkage table only}
  @item{@exec{-a, --all}: show packages and linkage}]

@subsection[#:tag "clearlinks"]{@exec{clearlinks}}

Usage:
@commandline{planet clearlinks}
Clear the linkage table, allowing upgrades.

@subsection[#:tag "fileinject"]{@exec{fileinject}}

Usage:
@commandline{planet fileinject <owner> <plt-file> <maj> <min>}
Install local file <plt-file> into the planet cache as though it had been
downloaded from the planet server.  It is treated as though it had the given owner name as its owner name, 
the given file's filename as the its package name, and the given major and minor version numbers.

@subsection[#:tag "link"]{@exec{link}}

Usage:
@commandline{planet link  <owner> <pkg> <maj> <min> <path>}
Create a development link (see @secref{devlinks}) between the given
package specifier and the specified directory name.

@subsection[#:tag "unlink"]{@exec{unlink}}

Usage:
@commandline{planet unlink <owner> <pkg> <maj> <min>}
Remove any development link (see @secref{devlinks}) associated with
the given package.

@subsection[#:tag "fetch"]{@exec{fetch}}

Usage:
@commandline{planet fetch <owner> <pkg> <maj> <min>}
Download the given package file from the central PLaneT repository without installing it.

@subsection[#:tag "url"]{@exec{url}}

Usage:
@commandline{planet url <owner> <pkg> <maj> <min>}
Get a URL for the given package.

This is never necessary for normal use of planet, but may be helpful in some
circumstances for retrieving packages.

@subsection[#:tag "open"]{@exec{open}}

Usage:
@commandline{planet open <plt-file> <target>}
Unpack the contents of the given package into the given directory without
installing.

This command is not necessary for normal use of planet. It is intended to allow
you to inspect package contents offline without needing to install the package.

@subsection[#:tag "structure"]{@exec{structure}}

Usage:
@commandline{planet structure <plt-file>}
Print the structure of the PLaneT archive named by <plt-file> to the standard
output port.

This command does not unpack or install the named .plt file.

@subsection[#:tag "print"]{@exec{print}}

Usage:
@commandline{planet print <plt-file> <path>}

Print the contents of the file named by <path>, which must be a relative path
within the PLaneT archive named by <plt-file>, to the standard output port.

This command does not unpack or install the named .plt file.

@section[#:tag "hash-lang-planet"]{The @schememodname[planet] Language}

@defmodulelang[planet]

When used with @hash-lang[], @schememodname[planet] must be followed
by a short-form PLaneT path. The path is used in the same way that
@hash-lang[] uses plain identifiers: @schemeidfont{/lang/reader} is
added to the given path to determine a module that supplies a module
reader.

The @schememodname[planet] module (as opposed to the reader used with
@hash-lang[]) implements the @exec{planet} command-line tool.

@section{Utility Libraries}

The planet collection provides configuration and utilities for using PLaneT. 

@subsection{Client Configuration}

@defmodule[planet/config]

The @schememodname[planet/config] library provides several parameters
useful for configuring how PLaneT works.

Note that while these parameters can be useful to modify
programmatically, PLaneT code runs at module-expansion time, so
most user programs cannot set them until PLaneT has already
run. Therefore, to meaningfully change these settings, it is best to
manually edit the config.ss file.

@defparam[PLANET-DIR dir path-string?]{
The root PLaneT directory. If the environment variable 
@indexed-envvar{PLTPLANETDIR} is
set, default is its value; otherwise the default is the directory in
which @filepath{config.ss} is found.}

@defparam[CACHE-DIR dir path-string?]{
The root of the PLaneT client's cache directory.}

@defparam[UNINSTALLED-PACKAGE-CACHE dir path-string?]{
The root of the PLaneT client's uninstalled-packages cache. PLaneT
stores package distribution files in this directory, and searches for
them in this directory for them if necessary. Unlike the main PLaneT
cache, which contains compiled files and is specific to each
particular version of PLT Scheme, the uninstalled package cache is
shared by all versions of PLT Scheme that use the same package
repository, and it is searched if a package is not installed in the
primary cache and cannot be downloaded from the central PLaneT repository
(for instance due to a loss of Internet connectivity). This behavior
is intended to primarily benefit users who upgrade their PLT Scheme
installations frequently.}

@defparam[LINKAGE-FILE file path-string?]{
The file to use as the first place PLaneT looks to determine how a
particular PLaneT dependence in a file should be satisfied. The
contents of this file are used to ensure that no "magic upgrades"
occur after a package is installed. The default is the file @filepath{LINKAGE}
in the root PLaneT directory.}

@defparam[LOG-FILE file (or/c path-string? false?)]{
If @scheme[#f], indicates that no logging should take place. Otherwise
specifies the file into which logging should be written. The default
is the file @filepath{INSTALL-LOG} in the root PLaneT directory.}

@defboolparam[USE-HTTP-DOWNLOADS? bool]{
PLaneT can use two different protocols to retrieve packages. If @scheme[#t],
PLaneT will use the HTTP protocol; if @scheme[#f] it will use the custom-built
PLaneT protocol. The default value for this parameter is @scheme[#t] and setting
this parameter to @scheme[#f] is not recommended.}

@defparam[HTTP-DOWNLOAD-SERVLET-URL url string?]{
The URL  for the servlet that will provide PLaneT packages if 
@scheme[USE-HTTP-DOWNLOADS?] is @scheme[#t], represented as a string.  
This defaults to the value of the @indexed-envvar{PLTPLANETURL} environment
variable if it is set and otherwise is
@scheme["http://planet.plt-scheme.org/servlets/planet-servlet.ss"].}

@defparam[PLANET-SERVER-NAME host string?]{
The name of the PLaneT server to which the client should connect if
@scheme[USE-HTTP-DOWNLOADS?] is @scheme[#f]. The default value for this parameter is
@scheme["planet.plt-scheme.org"].}

@defparam[PLANET-SERVER-PORT port natural-number?]{
The port on the server the client should connect to if
@scheme[USE-HTTP-DOWNLOADS?] is @scheme[#f]. The default value for this parameter is
@scheme[270].}

@subsection[#:tag "util.ss"]{Utilities}

@defmodule[planet/util]

The @schememodname[planet/util] library supports examination of the pieces of
PLaneT. It is meant primarily to support debugging and to allow easier
development of higher-level package-management tools. The
functionality exposed by @seclink["cmdline"]{the @exec{planet} command-line tool} is
also available programmatically through this library.

@defproc[(download/install-pkg [owner string?]
			       [pkg string?]
			       [maj natural-number/c]
			       [min natural-number/c])
         (or/c pkg? #f)]{
Downloads and installs the package specifed by the given owner name,
package name, major and minor version number. Returns false if no such
package is available; otherwise returns a package structure for the
installed package.}

@defparam[current-cache-contents contents
          (listof
           (list/c string? 
                   (listof 
                    (list/c string? 
                            (cons/c natural-number/c 
                                    (listof natural-number/c))))))]{
Holds a listing of all package names and versions installed in the
local cache.}

@defproc[(current-linkage)
         (listof (list/c path-string? 
                         (list/c string?
                                 (list/c string?) 
                                 natural-number/c
                                 natural-number/c)))]{
Returns the current linkage table.

The linkage table is an association between file locations (encoded as path strings)
and concrete planet package versions. If a require line in the associated file requests a package,
this table is consulted to determine a particular concrete package to satisfy the request.}

@defproc[(make-planet-archive [directory path-string?]
                              [output-file (or/c path? path-string?) 
                                           (string-append (path->string name) ".plt")]) 
         path-string?]{
Makes a .plt archive file suitable for PLaneT whose contents are all
files in the given directory and returns that file's name.  If the
optional filename argument is provided, that filename will be used as
the output file's name.}

@defproc[(unpack-planet-archive [plt-file (or/c path? path-string?)]
				[output-dir (or/c path? path-string?)])
 	 any]{
Unpacks the PLaneT archive with the given filename, placing its contents
into the given directory (creating that path if necessary).}

@defproc[(remove-pkg [owner string?]
		     [pkg   string?]
		     [maj   natural-number/c]
		     [min   natural-number/c])
	 any]{
Removes the specified package from the local planet cache.}

@defproc[(display-plt-file-structure [plt-file (or/c path-string? path?)])
         any]{
Print a tree representing the file and directory structure of the
PLaneT archive .plt file named by @scheme[plt-file] to @scheme[(current-output-port)].}

@defproc[(display-plt-archived-file [plt-file (or/c path-string? path?)]
				    [file-to-print string?])
	 any]{
Print the contents of the file named @scheme[file-to-print] within the 
PLaneT archive .plt file named by @scheme[plt-file] to @scheme[(current-output-port)].}

@defproc[(unlink-all) any]{
Removes the entire linkage table from the system, which will force all
modules to relink themselves to PLaneT modules the next time they run.}

@defproc[(add-hard-link [owner string?]
			[pkg   string?]
			[maj   natural-number/c]
			[min   natural-number/c]
			[dir   path?])
	 any]{
Adds a development link between the specified package and the given
directory; once a link is established, PLaneT will treat the cache as
having a package with the given owner, name, and version whose files
are located in the given path. This is intended for package
development; users only interested in using PLaneT packages
available online should not need to create any development links.

If the specified package already has a development link, this function
first removes the old link and then adds the new one.}

@defproc[(remove-hard-link [owner string?]
	 		   [pkg   string?]
			   [maj   natural-number/c]
			   [min   natural-number/c])
 	 any]{
Removes any hard link that may be associated with the given package.}

@defproc[(resolve-planet-path [spec quoted-planet-require-spec?])
	 path?]{
Returns the file system path to the file specified by the given quoted 
planet require specification. This function downloads and installs the
specified package if necessary, but does not verify that the actual
file within it actually exists.}

@deftogether[(
@defform[(this-package-version)]
@defform[(this-package-version-name)]
@defform[(this-package-version-owner)]
@defform[(this-package-version-maj)]
@defform[(this-package-version-min)]
)]{
Macros that expand into expressions that evaluate to information about
the name, owner, and version number of the package in which they 
appear. @scheme[this-package-version] returns a list consisting of a string
naming the package's owner, a string naming the package, a number
indicating the package major version and a number indicating the
package minor version, or @scheme[#f] if the expression appears outside the
context of a package. The others are convenience macros that
select out the relevant field, or return @scheme[#f] if the expression
appears outside the context of a PLaneT package.}

@subsection{Terse Status Updates}

@defmodule[planet/terse-info]

This module provides access to some PLaneT status information. This
module is first loaded by PLaneT in the initial namespace (when
PLaneT's resolver is loaded), but PLaneT uses @scheme[dynamic-require] to load
this module each time it wants to announce information. Similarly, the
state of which procedures are registered (via @scheme[planet-terse-register]) 
is saved in the namespace, making the listening and information producing
namespace-specific.

@defproc[(planet-terse-register
          [proc (-> (or/c 'download 'install 'finish) string? any/c)]
          [namespace namespace? (current-namespace)]) void?]{
Registers @scheme[proc] as a function to be called when
@scheme[planet-terse-log] is called with a matching namespace argument.
 Note that @scheme[proc] is called 
asynchronously (ie, on some thread other than the one calling @scheme[planet-terse-register]).
}

@defproc[(planet-terse-log [id (or/c 'download 'install 'finish)]
                           [msg string?]
                           [namespace namespace? (current-namespace)]) void?]{
  This function is called by PLaneT to announce when things are happening.
The namespace passed along is used to identify the procs to notify.
}

@section{Developing Packages for PLaneT}

To put a package on PLaneT, or release an upgrade to an
already-existing package:

@subsection{Write Your Package}

PLaneT can distribute whatever programs you write, but keep
these guidelines in mind as you write:
@itemize[
  @item{Organize your code into modules. Since the PLaneT client is
integrated into the @scheme[require] form, it works best if your code
is arranged into modules.}
  @item{When one module in your program depends on another, it is best 
to require it using the relative-file-name form rather than the
planet require form. For instance, if your program contains files
primary.ss and helper.ss where primary.ss requires helper, use the form

@scheme[(require "helper.ss")]

instead of

@scheme[(require (planet "helper.ss" ("username" "packagename.plt" 1 0)))]

in files that will also be a part of the package.}]

@subsubsection[#:tag "devlinks"]{Development Links}

To aid development, PLaneT allows users to establish direct
associations between a particular planet package
with an arbitrary directory on the filesystem, for instance connecting the package named by the require line

@scheme[(require (planet "file.ss" ("my" "mypackage.plt" 1 0)))]

to the directory @filepath{/home/myname/svn/mypackages/devel/}.

These associations are intended to allow developers to use their own
directory structures, version control systems, and so on while still
being able to use the packages they create as though they were
distributed directly by PLaneT. Development links are local to a
particular user and repository (but not to a particular MzScheme minor
revision).

To establish a development link, use the @exec{planet} command-line tool:

@commandline{planet link myname mypackage.plt 1 0 ~/svn/mypackages/devel}

Once you are finished developing a package, you should remove any
development links you have established for it, again using the planet
command-line tool:

@commandline{planet unlink myname mypackage.plt 1 0}

You may alternately use the functions @scheme[add-hard-link] and @scheme[remove-hard-link].

@subsection{Prepare Your Distribution}

@subsubsection{Arrange Files Into a Directory}

Make sure that all source files, documentation, etc. that you want to
be a part of the package are in a single directory and its
subdirectories. Furthermore make sure that nothing else, @italic{e.g.}
unneeded backup files, is in that directory (with the exception that
the subdirectories and files CVS or Subversion creates are
automatically skipped by the packaging tool).

@subsubsection{Create Documentation [Optional]}

Use Scribble to write documentation for your package. See
@other-manual['(lib "scribblings/scribble/scribble.scrbl")]
for instructions on how to write Scribble documentation.

@subsubsection{Create an @filepath{info.ss} File [Optional]}

If you put a file named @filepath{info.ss} in your package's root directory, the
PLaneT system (as well as the rest of the PLT Scheme tool suite) will
look in it for descriptive metadata about your package. The PLaneT
system looks for certain names in that file:

@itemize[

@item{The @indexed-scheme['blurb] field: If present, the blurb field
should contain a list of XHTML fragments encoded as x-expressions (see
the xml collection for details) that PLaneT will use as a short
description of your project.}

@item{The @indexed-scheme['release-notes] field: If present, the
release-notes field should contain a list of XHTML fragments encoded
as x-expressions (see the xml collection for details) that PLaneT will
use as a short description of what's new in this release of your
package.}

@item{The @indexed-scheme['categories] field: If present, the categories
field should be a list of symbols corresponding to the categories
under which this package should be listed.

The valid categories are:

@itemize[
  @item{@indexed-scheme['devtools]:         Development Tools}
  @item{@indexed-scheme['net]:              Networking and Protocols}
  @item{@indexed-scheme['media]:            Graphics and Audio}
  @item{@indexed-scheme['xml]:              XML-Related}
  @item{@indexed-scheme['datastructures]:   Data Structures and Algorithms}
  @item{@indexed-scheme['io]:               Input/Output and Filesystem}
  @item{@indexed-scheme['scientific]:       Mathematical and Scientific}
  @item{@indexed-scheme['system]:           Hardware/Operating System-Specific Tools}
  @item{@indexed-scheme['ui]:               Textual and Graphical User Interface}
  @item{@indexed-scheme['metaprogramming]:  Metaprogramming Tools}
  @item{@indexed-scheme['planet]:           PLaneT-Related}
  @item{@indexed-scheme['misc]:             Miscellaneous}]

If you put symbols other than these the categories field, they will be
ignored. If you put no legal symbols in the categories field or do not
include this field in your info.ss file, your package will be
categorized as "Miscellaneous."}

@item{The @indexed-scheme['can-be-loaded-with] field:
If present, the can-be-loaded-with field should be a quoted datum of
one of the following forms:

@schemegrammar[
can-be-loaded-with 'all
     	           'none
		   (list 'all-except VER-SPEC ...)
		   (list 'only VER-SPEC ...)]

where VER-SPEC is a PLaneT package version specification.

Depending on your package's behavior, it may or may not be okay for
multiple versions of the same package to be loaded at one time on the
entire system --- for instance, if your package relies on writing to a
particular file and assumes that nothing else writes to that same
file, then multiple versions of the same package being loaded
simultaneously may be a problem. This field allows you to specify
whether your package can be loaded simultaneously with older versions
of itself. If its value is @indexed-scheme['all], then the package may be
loaded with any older version. If it is @indexed-scheme['none], then it
may not be loaded with older versions at all. If it is @scheme[(list
'all-except VER-SPEC ...)] then any package except those that match
one of the given VER-SPEC forms may be loaded with this package; if it
is @scheme[(list 'only VER-SPEC ...)]  then only packages that match
one of the given VER-SPEC forms may be loaded with this package.

When checking to see if a package may be loaded, PLaneT compares it to
all other currently-loaded instances of the same package with any
version: for each comparison, it checks to see if the newer package's
can-be-loaded-with field allows the older package to be loaded. If all
such comparisons succeed then the new package may be loaded; otherwise
PLaneT signals an error.

The default for this field is @indexed-scheme['none] as a conservative
protection measure. For many packages it is safe to set this field to
@indexed-scheme['any].}

@item{The @indexed-scheme['homepage] field:
If present, the URL field should be a string corresponding to a URL
for the package. PLaneT provides this link with the description of your
package on the main PLaneT web page.}

@item{The @indexed-scheme['primary-file] field:
If present, the primary-file field should be a either a string
corresponding to the name (without path) of the main Scheme source
file of your package, or a list of such strings. The PLaneT web page
corresponding to this package will present all files listed here as
interface files for your package; it will give direct links to each
package and a listing of all names provided by the package along with
their contracts (if present).

If you include only a single string, it will be used as the require
line printed on your package's page. If you include a list of strings,
then the first legal file string in the list will be used.}

@item{The @indexed-scheme['required-core-version] field: If present, the
required-core-version field should be a string with the same syntax as
the output of the @scheme[version] function. Defining this field
indicates that PLaneT should only allow users of a version of mzscheme
equal to or more recent than the version specified by this field. This
allows you finer-grained control of your package's core-language
requirements than its inclusion in a particular repository; for
instance, setting this field to @scheme["300.2"] would cause the PLaneT server
not to serve it to MzScheme v300.1 or older clients.}

@item{The @indexed-scheme['version] field:
If present, the version field should be a string that describes the 
version number of this code that should be presented to users (e.g., 
@scheme["0.15 alpha"]). This field does not override or in any way interact 
with your package's package version number, which is assigned by 
PLaneT, but may be useful to users.}

@item{The @indexed-scheme['repositories] field: If present, the repositories
field should be a list consisting of some subset of the strings
@scheme["4.x"] and @scheme["3xx"]. The string @scheme["4.x"] indicates
that this package should be included in the v4.x repository (which
contains packages that are intended to run in PLT Scheme versions at
or above version 4.0), and the string @scheme["3xx"] indicates that
the package should be included in the v3xx repository (containing
packages intended to run in PLT Scheme versions in the 3xx series). A
single package (and a single version of a package) may be included in
multiple repositories with the same PLaneT version number.}]

In addition, PLaneT uses the setup-plt installer to install packages
on client machines, so most fields it looks for can be included with
their usual effects. In particular, adding a @indexed-scheme['name]
field indicates that the Scheme files in the package should be
compiled during installation; it is a good idea to add it.

An example info.ss file looks like this:

@schememod[
setup/infotab
(define name "My Application")
(define blurb
  '("My application runs 60% faster on 20% less peanut "
    "butter. It even shows a fancy graphic!"))
(define primary-file "my-app.ss")
(define categories '(system xml))
]

See the PLT mzc: MzScheme Compiler Manual, chapter 7 for more
information on info.ss files.

@subsection{Build a Distribution Archive}

Use the planet command-line tool in its archive-creation mode to
create a planet archive:

@commandline{planet create /home/jacob/my-app/}

This will create a planet archive named @filepath{my-app.plt} in the current
directory whose contents are the contents of @filepath{/home/jacobm/my-app} and
all its subdirectories. 

Alternately, you can run @scheme[make-planet-archive] with the name of the directory
you've prepared as its argument:

@scheme[(make-planet-archive "/home/jacob/my-app/")]

This function will build a packaged version of your directory and
return the path to that package. The path will always be a file named
@filepath{X.plt}, where @filepath{X} is the name of the directory you
gave to @scheme[make-planet-archive], located in that same directory.

You can now test that your archive file works as intended using the
planet command-line tool in its install mode:
@commandline{planet fileinject <owner> <path to your .plt file> <maj> <min>}
installs the specified file into your local PLaneT cache as
though it had been downloaded from the PLaneT server with the given
owner name and major and minor versions. After you run this command,
you can require your package on your local machine using

@scheme[(require (planet <file> (<owner> <.plt file name without path> <maj> <min>)))]

to verify everything works. After you do so, you can use
@commandline{planet remove <owner> <.plt file name without path> <maj> <min>}
to remove the test package from your local cache. (Not removing it is
safe as long as you use the same name and version numbers the package
will have on the PLaneT server; otherwise you may experience
problems.)

@subsection[#:tag "backwards-compatibility"]{Determine Your Package's Backwards-Compatibility}

If you are updating a previously-released package, you must decide
whether your package is a backwards-compatible change or not. A rule
of thumb is to remember that modules written to work with the
previously-released version of your package should unmodified with the
new package. This means that at a minimum, a backwards compatible
update should:
@itemize[
@item{Contain all the same Scheme source files in that the previous
version contained in directories intended for public access}
@item{In each public file, provide at least all the bindings that the
previous version provided}
@item{For each name provided with a contract (see @(secref #:doc '(lib
"scribblings/guide/guide.scrbl") "contracts" )), provide it
with a contract that is at least as permissive as the previous
contract}]
A backwards-compatible upgrade may, however:
@itemize[
@item{Change any behavior that
reasonable consumers of your package would not consider guaranteed
(@italic{e.g.}, by fixing bugs or improving the efficiency of
operations).}
@item{Remove files in clearly-marked private
sections. By convention, the contents of any directory called
@filepath{private} are considered private and should not be relied
upon by external users of your package.}
@item{Extend the set of names exported by a module.}]
Currently these rules are guidelines only, but in the future some or
all of them may be enforced programmatically. Ultimately, though, no
technical device can precisely capture what it means for a package to
be backwards-compatible with a previous version, so you should use your
best judgment.

@subsection{Submit Your Package}

Go to @link["http://planet.plt-scheme.org/"]{the central PLaneT
package repository web page} and click on the link marked "contribute
a package / log in" in the upper-right-hand corner. If you have not
yet created an account, then do so on that page by providing your
name, a user name, an email address, and a password and then
responding to the confirmation message delivered to the email address
you provide.

Once you have an account, then if this is a new package then upload it
using the "Contribute a package" section in your user account page. If
this is a package update then click "update this package" next to its
name in the "Manage your packages" section of your user account page,
then upload the .plt file and indicate on the form whether your update
is backwards-compatible with the prior version or not.
