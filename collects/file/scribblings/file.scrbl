#lang scribble/doc
@(require "common.ss")

@title{@bold{File}: PLT File Format Libraries}

@table-of-contents[]

@include-section["gzip.scrbl"]
@include-section["gunzip.scrbl"]
@include-section["zip.scrbl"]
@include-section["tar.scrbl"]
@include-section["md5.scrbl"]
@include-section["gif.scrbl"]

@(bibliography
  (bib-entry #:key "Gervautz1990"
   #:author "M. Gervautz and W. Purgathofer"
   #:title "A simple method for color quantization: Octree quantization"
   #:location "Graphics Gems"
   #:date "1990")
  
  (bib-entry #:key "Clark1996"
   #:author "Dean Clark"
   #:title "Color Quantization using Octrees"
   #:location "Dr. Dobbs Journal"
   #:date "January 1, 1996"
   #:url "http://www.ddj.com/184409805"))

@index-section[]
