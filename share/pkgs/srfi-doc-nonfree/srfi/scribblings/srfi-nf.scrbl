#lang scribble/doc
@; SPDX-License-Identifier: (Apache-2.0 OR MIT)
@;
@; This Racket code in this package, including this file, is under the
@; Apache 2.0 and MIT licenses. The user can choose the license under
@; which they will be using the software.
@;
@; However, the SRFI 5 document has a restrictive licenses:
@; see the file "srfi-5.html" for the specific license.
@; (The implementation of `srfi/5` found in the
@; "srfi-lib" package is not licensed restrictively.)
@(require srfi/scribblings/util
          scribble/manual
          scribble/eval
          scriblib/render-cond
          scribble/core
          scribble/html-properties
          (for-syntax scheme/base)
          (for-label scheme/base
                     racket/stream))

@; ----------------------------------------------------------------------

@title{SRFI Nonfree Libraries and Documentation}

The @link[#:style srfi-std "https://srfi.schemers.org/"]{Scheme Requests for
Implementation} (a.k.a. @deftech{SRFI}) process allows individual
members of the Scheme community to propose libraries and extensions to
be supported by multiple Scheme implementations.

Racket is distributed with implementations of many SRFIs, most of
which can be implemented as libraries. To import the bindings of SRFI
@math{n}, use

@racketblock[
(require @#,elem{@racketidfont{srfi/}@math{n}})
]

This document lists the SRFIs that are supported by Racket and
provides a link to the original SRFI specification (which is also
distributed as part of Racket's documentation).

The following SRFI specification documents are licensed restrictively.  

@table-of-contents[]


@; ----------------------------------------

@srfi[5]{A compatible let form with signatures and rest arguments}

@redirect[5 '(
 (let #t "unnamed")
)]

Racket provides a free implementation of this SRFI in the @racket[srfi-lib] package.
Only the SRFI specification document is nonfree.

@; ----------------------------------------

@index-section[]
