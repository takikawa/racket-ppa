This is the source code distribution for minimal Racket.

If this directory is part of a clone of the Git repository for Racket,
then the clone's root directory includes a makefile to both build
minimal Racket and install packages. See "build.md" in the clone's
root directory.

========================================================================
 License and external links
========================================================================

Racket is distributed under the MIT license and the Apache version 2.0
license, at your option. See "LICENSE.txt" for more information.

Compiled executables, documentation, and up-to-date information:
   http://racket-lang.org/

Pre-compiled daily snapshots:
   http://snapshot.racket-lang.org/

Main development repository:
   https://github.com/racket/racket

Report bugs:
   https://github.com/racket/racket/issues


========================================================================
 Racket CS (Chez Scheme) versus Racket BC (ByteCode / Before Chez)
========================================================================

This source directory contains implementations for two different
versions of Racket: the original BC implementation that is
substantially implemented in C, and the CS implementation that is
implemented in Chez Scheme and Racket (compiled to Chez Scheme).

Racket CS
---------

By default, `configure` and the Windows scripts build the CS
implementation of Racket.

Chez Scheme is included in Racket source distributions and the source
repository.

To build Racket CS on Windows, see See "worksp\README.txt" for
information.

If you need more information specific to Racket CS, see
"cs/README.txt".

Racket BC
---------

To build Racket BC on Unix variants or Mac OS:

 * ... in addition to Racket CS: supply `--enable-cs --enable-bc` to
   `configure`.

   The generated Racket BC executables will have a "bc" suffix. A
   plain `make` will still build Racket CS; use `make bc` to build and
   `make install-bc` to install.

 * ... by itself: supply `--enable-bcdefault` to `configure`.
 
   The generated Racket BC executables will *not* have a "bc" suffix.

To build Racket BC on Windows, see See "worksp\README.txt" for
information.

If you need more information specific to Racket BC, see
"bc/README.txt".


========================================================================
 Compiling for supported Unix variants (including Linux)
========================================================================

Quick instructions:

 From this directory (where the `configure` file is), run the following
 commands:

   mkdir build
   cd build
   ../configure
   make
   make install

 Those commands will create an in-place installation of Racket and
 store the results of intermediate compilation in a separate "build"
 subdirectory, which is useful if you need to update your sources,
 delete the build, and start from scratch.

 You can also run the typical `./configure && make && make install` if
 you don't anticipate updating/rebuilding, but it will be harder to
 restart from scratch should you need to.

 Some build modes may require GNU Make. For example, when building the
 Racket CS implementation, GNU Make is required. When building the
 Racket BC implementation, the content of the "foreign" subdirectory
 requires GNU Make if no installed "libffi" is detected. If the build
 fails with another variant of `make`, please try using GNU Make.

Detailed instructions:

 0. If you have an old Racket installation in the target directory,
    remove it (unless you are using an "in-place" build from a
    repository as described below).

    On Unix variants other than Mac OS, to run `racket/draw` and
    `racket/gui` programs, you will not only need the packages that
    supply those libraries, you'll need Cairo, Pango, and GTk
    installed. These libraries are not distributed with Racket, and
    they are not needed for compilation, except for building
    documentation that uses `racket/draw`. More info about required
    libs is available at http://docs.racket-lang.org/draw/libs.html
    and http://docs.racket-lang.org/gui/libs.html.

 1. Select (or create) a build directory.

    It's better to run the build in a directory other than the one
    containing `configure`, especially if you're getting sources via
    git. A common way to start a git-based build is:

        cd [here]
        mkdir build
        cd build

    where "[here]" is the directory containing this "README.txt" file
    and the `configure` script. The Git repository is configured to
    support this convention by ignoring `build` in this directory.

    A separate build directory is better in case the makefile
    organization changes, or in case the makefiles lack some
    dependencies. In those cases, when using a "build" subdirectory,
    you can just delete and re-create "build" without mangling your
    source tree.

 2. From your build directory, run the script `configure` (which is in
    the same directory as this README), with optional command-line
    arguments like `--prefix=TARGETDIR`.

    For example, if you want to install into "/usr/local/racket", then
    run:

      [here]configure --prefix=/usr/local/racket

    Again, "[here]" is the directory path containing the `configure`
    script. If you follow the convention of running from a "build"
    subdirectory, "[here]" is just "../". If you build from the
    current directory, "[here]" is possibly unnecessary, or possibly
    just "./", depending on your shell and PATH setting.

    If the `--prefix` flag is omitted and if directories like `bindir`
    and `libdir` appear to be the default paths or the
    `--enable-origtree` flag is specified, then executables are built
    for an in-place installation (i.e., the parent of the directory
    containing this README will be used directly). Unless
    `--enable-shared` is used, the "racket" directory can be moved
    later; most system administrators would recommend that you use
    `--enable-shared`, but the Racket developers distribute
    executables built without `--enable-shared`.

    The `configure` script generates the makefiles for building Racket
    and/or GRacket. The current directory at the time `configure` is
    run will be used as working space for building the executables
    (independent of `--prefix`). This build directory does not have to
    be in the source tree, even for an in-place build. It's ok to run
    `configure` from its own directory (as in the first example
    above), but it's better to pick a separate build directory that is
    otherwise empty (as in the second example).

    The `configure` script accepts many other flags that adjust the
    build process. Run `configure --help` for more information. In
    addition, a specific compiler can be selected through environment
    variables. For example, to select the SGI compilers for Irix
    instead of gcc, run configure as

         env CC=cc CXX=CC [here]configure

    To add an include path, be sure to use CPPFLAGS="-I..." instead of
    CFLAGS="-I...". The CPPFLAGS variable controls C pre-processing,
    which includes C compilation, and the Racket build normally uses
    the C pre-processor directly for some parts of the build.

    If you re-run `configure` after running `make`, then products of
    the `make` may be incorrect due to changes in the compiler command
    line. To be safe, run `make clean' each time after running
    `configure`. To be even safer, run `configure` in a fresh build
    directory every time.

    When building for multiple platforms or configurations out of the
    same source directory, beware of cached `configure` information in
    "config.cache". Avoid this problem entirely by using a separate
    build directory (but the same source) for each platform or
    configuration.

 3. Run `make`.  [As noted above, this might need to be GNU `make`.]

    Executables and libraries are placed in subdirectories of the
    build directory. For example, the `racketcs` executable appears in
    the "cs/c" directory.

    You can run executables in-place before `make install`, but if you
    haven't yet built ".zo" bytecode files from Racket sources in
    "../collects", startup will be very slow.

 4. Run `make install`.

    This step copies executables and libraries into place within the
    target installation. For example, the "racket" executable is
    copied into the "bin" directory for an in-place build, or into the
    executable directory for a `--prefix` build.

    For a `--prefix` build, this step also creates a "config.rktd"
    module in an "etc" directory, so that various Racket tools and
    libraries can find the installation directories. At this stage, in
    case you are packaging an installation instead of installing
    directly, you can redirect the installation by setting the
    "DESTDIR" makefile variable to an absolute path for the
    packaging area. For example, `make DESTDIR=/tmp/racket-build
    install` places the installation into "/tmp/racket-build" instead
    of the location originally specified with `--prefix`. The
    resulting installation will not work, however, until it is moved
    to the location originally specified with `--prefix`.

    Finally, the `make install` step compiles ".zo" bytecode files for
    installed Racket source, generates launcher programs like DrRacket
    (if it's already installed as a package), and builds documentation
    (again, if installed). Use `make plain-install` to install without
    compiling ".zo" files, creating launchers, or building
    documentation.

    For a `--prefix` build, unless `--enable-sharezo` is specified,
    "compiled" directories containin ".zo" files are moved from
    "share" to "lib" as the last step of installation. (The
    "config.rktd" file is updated so that `current-compile-file-roots`
    is initialized to find the relocated ".zo" files.) For Racket BC,
    ".zo" files are architecture-independent, and `--enable-sharezo`
    was the default installation mode through Racket version 8.0. To
    prepare additional packages (i.e., package that are not included
    with the source distribution) in installation scope without
    `--enable-sharezo`, then it's easiest to first install in-place,
    then configure and install again for a `--prefix` build; that way,
    the packages installed in-place will get carried along, and their
    "compiled" directories will be moved appropriately.

    If the installation fails because the target directory cannot be
    created, or because the target directory is not the one you want,
    then you can try repeating step 4 after running `configure` again
    with a new `--prefix` value.  That is, sometimes it is not necessary
    to repeat step 3 (so try it and find out).  On other platforms and
    configurations, it is necessary to start with a clean build
    directory when changing the `--prefix` value, because the path gets
    wired into shared objects.

    If you build frequently from the Git-based sources, beware that you
    may accumulate user- and version-specific information in your
    "add-ons" directory, which you can most easily find by evaluating
      (find-system-path 'addon-dir)
    in Racket. In addition, on Mac OS or if you configure with
    `--enabled-shared` for Racket BC, you may accumulate many unused
    versions of the dynamic libraries in your installation target.

After an "in-place" install from a source distribution, the
"racket/src" directory is no longer needed, and it can be safely
deleted. Build information is recorded in a "buildinfo" file in the
installation.

For a build without `--prefix` (or with `--enable-origtree`) and
without `--enable-shared`, you can safely move the install tree,
because all file references within the installation are relative.


========================================================================
 Compiling for Mac OS
========================================================================

First, install Xcode and command-line tools from Apple.

After installing developer tools, follow the Unix instructions above,
but note the following:

 * If you are building from a source distribution (as opposed to a Git
   repository checkout), then most likely "racket-lib" is already
   included and installed as part of the the distribution, but without
   dependencies of "racket-lib" that are specific to Mac OS. In that
   case, use

      raco pkg update --auto racket-lib

   using `raco` as created by `make install` to download and install
   the dependencies.

 * The Racket build creates a framework, "Racket.framework", which is
   installed into "racket/lib".  This framework is used by the `racket`
   executable that goes into "racket/bin".

 * The GRacket build creates a GUI-executable variant of the Racket
   executable. The GRacket build process also downloads (from github)
   pre-built libraries for Cairo, Pango, etc.

 * The `--enable-shared` flag for `configure` must not be used,
   because builds create and use frameworks by default. Furthermore,
   `--disable-shared` is not supported. (Unless you use
   `--enable-xonx`...)

 * To build an X11- and Gtk-based GRacket, run `configure` with the
   `--enable-xonx` flag. Frameworks are not used for such builds, so
   `--enable-shared` is allowed. The `--enable-xonx` flag also affects
   the Racket build, so that `system-type` reports 'unix. Pre-built
   libraries are not downloaded in this mode; you must have Cairo,
   Pango, and GTk installed.

 * To use `--prefix` without `--enable-xonx`, you must also supply
   `--enable-macprefix`. BEWARE! The directory structure for a
   non-xonx build does not fit a typical Unix directory structure. For
   example, frameworks are written directly to a "lib" subdirectory,
   and executables like "GRacket.app" are written directly to the
   prefix directory. (Requiring `--enable-macprefix` with `--prefix`
   for a non-xonx build helps prevent accidental installation of a
   Mac-style directory structure on top of an existing Unix-style
   directory structure.)


========================================================================
 Compiling for Windows
========================================================================

To compile with Microsoft Visual C, see the instructions in
"worksp\README.txt".

To compile with MinGW tools, follow the Unix instructions above; do
not use `--enable-shared`, because DLLs will be generated
automatically. The result is a Windows-style build. If you are using a
variant of MinGW without "libdelayimp.a", get the implementation of
"delayimp.c" from MinGW-w64 and compile it to "libdelayimp.a".


========================================================================
 Cross-compiling
========================================================================

Cross-compilation requires at least two flags to `configure`:

 * `--host=OS`, where OS is something like `i386-gnu-linux` to
   indicate the target platform.

   The `configure` script uses OS to find suitable compilation tools,
   such as `OS-gcc` and `OS-strip`.

 * `--enable-racket=RACKET`, where RACKET is a path to a Racket
   executable that runs on the build platform; the executable must be
   the same version of Racket and the same virtual machine (i.e., CS
   or BC) as being built for the target platform.

   This flag is needed because building and installing Racket requires
   running (an existing build of) Racket.

   Use "auto" for RACKET to indicate that Racket should be built
   automatically for the current platform. In that case, `make` will
   run `configure` again (with no arguments) in a "local" subdirectory
   to create a build for the current platform.

An additional flag is needed for building Racket CS, unless the flag
`--enable-racket=auto` is used:

 * `--enable-scheme=DIR`, where DIR is a path that has a "ChezScheme"
   directory where Chez Scheme is built for the host system.

Some less commonly needed `configure` flags are for Racket BC:

 * `--enable-stackup`, if the target platform`s stack grows up.

 * `--enable-bigendian`, if target platform is big-endian.

 * `--enable-cify` or `--disable-cify` if the JIT availability on the
    target platform is different than the build platform; use
    `--enable-cify` if the JIT is not abailable on the target
    platform.


========================================================================
 Cross-compiling for Android
========================================================================

As an example of cross-compiling Racket for Android on ARMv7 using the
NDK, use (all on one line)

  configure --host=arm-linux-androideabi 
            --enable-sysroot="[sysroot]"
            --enable-racket=auto

If you use the NDK script "make-standalone-toolchain.sh" to generate a
toolchain directory, then include that directory's "bin" in your PATH
(so that `arm-linux-androideabi-gcc`, etc., are found), and you can
omit `--enable-sysroot` (or specify [sysroot] as the toolchain
directory's "sysroot" subdirectory).

In other NDK configurations, you may have

 [ndk]/toolchains/arm-linux-androideabi-[comp]/prebuilt/[platform]/bin

in your PATH (so that `arm-linux-androideabi-gcc`, etc., are found)
where [ndk] is the path to the installed NDK and for the [comp] of
your choice and the [platform] used to compile, and then [sysroot] is

 [ndk]/platforms/android-[N]/arch-arm

where [N] is a target version of Android (such as 14).

For 64-bit ARM, replace "arm" above with "aarch64", and replace
"androideabi" with "android".

When building BC, you may need to add `--disable-cify` for 32-bit ARM
and `--enable-cify` for 64-bit ARM instead of inheriting the build
machine's disposition.


========================================================================
 Cross-compiling for iOS
========================================================================

To compile the Racket runtime system as a Framework for iOS, use (all
on one line)

  configure --host=[arch]-apple-darwin
            --enable-ios="[sdk]"
            --enable-racket=auto

where [arch] is one of

 - armv7, armv7s, or aarch64: to run on iOS
 - x86_64 or aarch64: to run on the simulator

The [sdk] argument is a path to an iOS SDK for "iPhoneOS" or
"iPhoneSimulator". The corresponding SDK is located in the standard
place within the XCode application. For example, "iPhoneOS" becomes
the path (all on one line)

  /Applications/Xcode.app/Contents/Developer/Platforms/
    iPhoneOS.platform/Developer/SDKs/iPhoneOS.sdk

To use an existing Racket build for the build platform using
`--enable-racket`, be sure to include `--enable-scheme` for Racket CS.
For example, if you have built Racket CS for your machine at
"/path/to/racket" using the default `make` target, you can configure
the cross build using that Racket binary and a path to the Chez Scheme
build folder as follows (all on one line)

  configure --host=[arch]-apple-darwin
            --enable-ios="[sdk]"
            --enable-racket=/path/to/racket/bin/racket
            --enable-scheme=/path/to/racket/src/build/cs/c

Currently, iOS enforces W^X protection on memory pages, which is
technically a problem for Racket CS. See the note about "writes to
`space_code` memory" in "ChezScheme/c/segment.c" for the implications.
If you avoid passing newly-allocated code between threads and avoid
`#:blocking?` foreign callbacks, you might not run into any issues.

When building BC for iOS, you may need to add `--disable-cify` for
32-bit target and `--enable-cify` for 64-bit target instead of
inheriting the build machine's disposition.


========================================================================
 Modifying Racket
========================================================================

See "cs/README.txt" and "bc/README.txt" for information about
modifying those implementations of Racket, but one thing they have in
common is updating the Racket version number. The source for the
Racket version number is shared in "version/racket_version.h".

The version number for the "base" package needs to be updated
separately. If this directory is part of a clone of the Git repository
for Racket, then the "base" version is in "../../pkgs/base/info.rkt".

Unfortunately, there's no single source for the version number in both
Racket and "base". Those are extracted as subtrees into separate
distributions, and the point of a version in each place is to detect a
mismatch between those extracted distributions. The "version.rktl"
test in Racket's core test suite effectively checks that they're in
sync within the Racket repo.


========================================================================
 Test Suite
========================================================================

Tests for the core Racket implementation are in a "racket-test-core"
package. The tests are developed in the same Git repository as the
core Racket implementation in a "pkgs" directory at the repository
root.


========================================================================
 Implementation Organization
========================================================================

Everything in this "src" directory contributes to the implementation
of the `racket` executable (and variants), while "../collects"
contains the additional Racket libraries that are included in a
minimal Racket distribution.

Sources for the Racket CS implementation
----------------------------------------

 * "cs" --- `racket` CS executable

 * "thread" --- thread scheduler

 * "io" --- I/O

    This layer uses the "racketio" library to access OS facilties.

 * "regexp" --- regexp matcher

See also the shared sources below, which includes rktio, the macro
expander, and schemify.

Sources for the Racket BC implementation
----------------------------------------

 * "bc" --- `racket` BC executable

   This implementation can build from "scratch" with a C compiler, but
   first by building a CGC variant of Racket to transform the C
   sourses to build a (normal) 3m variant.

 * "mzcom" --- MzCOM executable (for Windows)

 * "mysink" --- `ffi/unsafe/com` helper DLL (for Windows)

 * "cify" --- a Racket-to-C compiler

   This compiler is used only when embedding the expander as C code in
   Racket BC, instead of Racket bytecode, which is the default for
   platforms where the Racket BC JIT is not supported.

See also the shared sources below, which includes rktio and the macro
expander.

Sources shared by both Racket implementations
---------------------------------------------

 * "expander" --- macro expander implementation

   This expander is both included in Racket executables and used to
   expand itself for inclusion in executables. It's also used to
   expand other libraries for inclusion in Racket CS executables, but
   already-expanded versions are included with source in
   "cs/schemified".

   If you change the expander, run `make` in its directory to generate
   the "startup.inc" file that holds the expander's implementation for
   Racket BC. Also, run `make` in "cs" to rebuild expanded libraries
   for Racket CS.

 * "rktio" --- portability layer for low-level I/O

   If you change "rktio.h", then be sure to regenerate "rktio.rktl"
   and "rktio.inc" using an existing Racket implementation that has
   the "parser-tools" package installed.

 * "schemify" --- a Racket-to-Scheme compiler, used by "cs" and "cify"

   Similar to "expander", this layer is applied to itself and other
   libraries for inclusion in "cs". If you change it, be sure to run
   `make` in "cs".

 * "start" --- main-executable wrapper

   Startup wrappers used by both the Racket CS and BC implementations.

 * "worksp" --- Windows projects and build scripts

 * "mac" --- scripts for generating Mac OS ".app"s

 * "setup-go.rkt" --- helper script

   The "setup-go.rkt" script is a bootstrapping too that is used by
   parts of the build that need to run Racket programs in the process
   of building Racket.

More shared utilities
---------------------

 * "native-libs" --- build scripts for some native-library packages

   These script are not used when building Racket, but they are used
   to build some separately distributed packages.

 * "common" --- Racket libraries used by "thread", "io", etc.

 * "ac" --- `autoconf` helpers

 * "lt" --- `libtool` and `configure` support

 * "utils" --- miscellaneous
