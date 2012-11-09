#lang scribble/doc
@(require "common.rkt" (for-label file/ico))

@title[#:tag "ico"]{ICO File Reading and Writing}

@defmodule[file/ico]

The @racketmodname[file/ico] library provides functions for reading
and writing @filepath{.ico} files, which contain one or more icons.
Each icon is up to 256 by 256 pixels, has a particular depth (i.e.,
bits per pixel used to represent a color), and mask (i.e., whether a
pixel is shown, except that the mask may be ignored for 32-bit icons
that have an alpha value per pixel).

@defproc[(ico? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] represents an icon, @racket[#f]
otherwise.}

@deftogether[(
@defproc[(ico-width [ico ico?]) (integer-in 1 256)]
@defproc[(ico-height [ico ico?]) (integer-in 1 256)]
@defproc[(ico-depth [ico ico?]) (one-of/c 1 2 4 8 16 24 32)]
)]{

Returns the width or height of an icon in pixels, or the depth in bits per
pixel.}

@defproc[(read-icos [src (or/c path-string? input-port?)])
         (listof ico?)]{

Parses @racket[src] as an @filepath{.ico} to extract a list of icons.}

@defproc[(read-icos-from-exe [src (or/c path-string? input-port?)])
         (listof ico?)]{

Parses @racket[src] as an @filepath{.exe} to extract the list of
icons that represent the Windows executable.}

@defproc[(write-icos [icos (listof ico?)]
                     [dest (or/c path-string? output-port?)]
                     [#:exists exists (or/c 'error 'append 'update 'can-update
                                            'replace 'truncate
                                            'must-truncate 'truncate/replace)
                               'error])
          void?]{

Writes each icon in @racket[icos] to @racket[dest] as an
@filepath{.ico} file. If @racket[dest] is not an output port,
@racket[exists] is passed on to @racket[open-output-file] to open
@racket[dest] for writing.}

@defproc[(replace-icos [icos (listof ico?)]
                       [dest (or/c path-string? output-port?)])
          void?]{

Writes icons in @racket[icos] to replace icons in @racket[dest] as an
Windows executable. Only existing icon sizes and depths in the
executable are replaced, and best matches for the existing sizes and
depth are drawn from @racket[icos] (adjusting the scale and depth f a
best match as necessary).}

@defproc[(ico->argb [ico ico?]) bytes?]{

Converts an icon to an ARGB byte string, which has the icon's pixels
in left-to-right, top-to-bottom order, with four bytes (alpha, red,
green, and blue channels) for each pixel.}

@defproc[(argb->ico [width (integer-in 1 256)]
                    [height (integer-in 1 256)]
                    [bstr bytes?]
                    [#:depth depth (one-of/c 1 2 4 8 24 32) 32])
         ico?]{

Converts an ARGB byte string (in the same format as from
@racket[ico->argb]) to an icon of the given width, height, and depth.

The @racket[bstr] argument must have a length @racket[(* 4 width height)],
and @racket[(* width depth)] must be a multiple of 8.}
