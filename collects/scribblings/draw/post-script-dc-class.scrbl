#lang scribble/doc
@(require "common.ss")

@defclass/title[post-script-dc% object% (dc<%>)]{

A @scheme[post-script-dc%] object is a PostScript device context, that
 can write PostScript files on any platform. See also
 @scheme[ps-setup%] and @racket[pdf-dc%].

@|PrintNote|

See also @scheme[printer-dc%].


@defconstructor[([interactive any/c #t]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                 [use-paper-bbox any/c #f]
                 [as-eps any/c #t]
                 [width (or/c (and/c real? (not/c negative?)) #f) #f]
                 [height (or/c (and/c real? (not/c negative?)) #f) #f]
                 [output (or/c path-string? output-port? #f) #f])]{

If @scheme[interactive] is true, the user is given a dialog for
 setting printing parameters (see @scheme[get-ps-setup-from-user]);
 the resulting configuration is installed as the current
 configuration). If the user chooses to print to a file (the only
 possibility under Windows and Mac OS X), another dialog is given to
 select the filename.  If the user hits cancel in either of these
 dialogs, then @method[dc<%> ok?] returns @scheme[#f].

If @scheme[parent] is not @scheme[#f], it is used as the parent window of
 the configuration dialog.

If @scheme[interactive] is @scheme[#f], then the settings returned by
 @scheme[current-ps-setup] are used. A file dialog is still presented
 to the user if the @method[ps-setup% get-file] method returns
 @scheme[#f] and @racket[output] is @racket[#f], and the user may 
 hit @onscreen{Cancel} in that case so that @method[dc<%> ok?] returns @scheme[#f].

If @scheme[use-paper-bbox] is @scheme[#f], then the PostScript
 bounding box for the output is determined by @racket[width] and
 @racket[height]. If @scheme[use-paper-bbox] is not @scheme[#f], then
 the bounding box is determined by the current paper size (as
 specified by @scheme[current-ps-setup]). When @racket[width] or
 @racket[height] is @racket[#f], then the corresponding dimension is
 determined by the paper size, even if @racket[use-paper-bbox] is
 @racket[#f].

@index["Encapsulated PostScript (EPS)"]{If} @scheme[as-eps] is
 @scheme[#f], then the generated PostScript does not include an
 Encapsulated PostScript (EPS) header, and instead includes a generic
 PostScript header. The margin and translation factors specified by
 @racket[current-ps-setup] are used only when @racket[as-eps] is
 @racket[#f]. If @racket[as-eps] is true, then the generated
 PostScript includes a header that identifiers it as EPS.

When @racket[output] is not @racket[#f], then file-mode output is
 written to @racket[output]. If @racket[output] is @racket[#f], then
 the destination is determined via @racket[current-ps-setup] or by
 prompting the user for a pathname. When @racket[output] is a port,
 then data is written to @racket[port] by a thread that is created
 with the @racket[post-script-dc%] instance; in case that writing
 thread's custodian is shut down, calling @method[dc<%> end-doc]
 resumes the port-writing thread with @racket[thread-resume]
 and @racket[(current-thread)] as the second argument.

See also @scheme[ps-setup%] and @scheme[current-ps-setup]. The
settings for a particular @scheme[post-script-dc%] object are fixed to
the values in the current configuration when the object is created
(after the user has interactively adjusted them when
@scheme[interactive] is true).

}}
