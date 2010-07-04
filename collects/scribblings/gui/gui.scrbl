#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@title[#:tag-prefix '(lib "gui.scrbl" "scribblings" "gui") 
       #:tag "top"]{PLT Scheme GUI: MrEd}

@declare-exporting[(lib "mred")]

This reference manual describes the MrEd GUI toolbox that is part of
 PLT Scheme. See @secref[#:doc '(lib "guide.scrbl" "scribblings"
 "guide") "mred"] in @italic{@link["../guide/index.html"]{A Guide to
 PLT Scheme}} for an introduction to MrEd.

The @scheme[(lib "mred")] module provides all of the class, interface,
 and procedure bindings defined in this manual. The
 @schememodname[big-gui] language (for use with @schemefont{#module})
 extends the @schememodname[big] language with @scheme[(lib "mred")].

@bold{This reference describes a potential future version of PLT Scheme.
      It does not match the current implementation.}

@table-of-contents[]

@;------------------------------------------------------------------------

@include-section["guide.scrbl"]
@include-section["reference.scrbl"]
@include-section["config.scrbl"]

@;------------------------------------------------------------------------

@index-section["mred-index"]
