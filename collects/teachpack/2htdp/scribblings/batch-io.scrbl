#lang scribble/doc

@(require scheme/sandbox scribble/manual scribble/eval scribble/core
          scribble/html-properties scribble/latex-properties
          2htdp/batch-io
          "shared.rkt"
          (for-label scheme teachpack/2htdp/batch-io))

@(require scheme/runtime-path)
@(define-runtime-path here ".")
@(define io-style-extras
   (list (make-css-addition (build-path here "io.css"))
         (make-tex-addition (build-path here "io.tex"))))
@(define (file-is f)
  (define x (parameterize ([current-directory here]) (read-file f)))
  (centered
    (tabular #:style (make-style "FileBox" io-style-extras)
      (list (list (verbatim x))))))

@(define-syntax examples-batch-io
  (syntax-rules ()
    [(_ d ...)
     (let ()
       (define me (make-base-eval))
       (begin
         (interaction-eval #:eval me (require 2htdp/batch-io))
         (interaction-eval #:eval me d)
         ...)
       (me `(,current-directory ,here))
       (interaction-eval #:eval me (require lang/htdp-intermediate-lambda))
       me)]))

@; -----------------------------------------------------------------------------

@(define-syntax-rule (reading name ctc s)
   @defproc[(@name [f (and/c string? file-exists?)]) @ctc ]{
 reads the content of file @racket[f] and produces it as @s .} )

@teachpack["batch-io"]{Batch Input/Output}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/batch-io]

The batch-io teachpack introduces several functions and a form for reading
 content from files and one function for writing to a file.

All functions that read a file consume the name of a file and possibly
 additional arguments. They assume that the specified file exists in the
 same folder as the program; if not they signal an error:
@itemlist[

@item{@reading[read-file string?]{a string, including newlines}

@examples[#:eval (examples-batch-io)
(read-file "data.txt")
]
assuming the file named @racket["data.txt"] has this shape: 
@(file-is "data.txt")
Note how the leading space in the second line translates into the space
between the newline indicator and the word @racket["good"] in the result.}

@item{@reading[read-1strings (listof 1string?)]{a list of one-char strings, one per character}

@examples[#:eval (examples-batch-io)
(read-1strings "data.txt")
]
Note how this function reproduces all parts of the file faithfully,
including spaces and newlines.}

@item{@reading[read-lines (listof string?)]{a list of strings, one per line}
@examples[#:eval (examples-batch-io)
(read-lines "data.txt")
]
when @racket["data.txt"] is the name of the same file as in the preceding
item. And again, the leading space of the second line shows up in the
second string in the list.}

@item{@reading[read-words (listof string?)]{a list of strings, one per white-space separated token in the file}

@examples[#:eval (examples-batch-io)
(read-words "data.txt")
]
This time, however, the extra leading space of the second line of
@racket["data.txt"] has disappeared in the result. The space is considered
a part of the separator that surrounds the word @racket["good"].
}

@item{@reading[read-words/line (listof string?)]{a list of lists, one per line; each line is represented as a list of white-space separated tokens}

@examples[#:eval (examples-batch-io)
(read-words/line "data.txt")
]
The results is similar to the one that @racket[read-words] produces,
except that the organization of the file into lines is preserved. 
In particular, the empty third line is represented as an empty list of words. 
}

@item{@reading[read-csv-file (listof (listof any/c))]{a list of lists of comma-separated values}

@examples[#:eval (examples-batch-io)
(read-csv-file "data.csv")
]
where the file named @racket["data.csv"] has this shape: 
@(file-is "data.csv")
It is important to understand that the rows don't have to have the same
length. Here the third line of the file turns into a row of three
elements. 
}

@item{@defproc[(@read-csv-file/rows [f (and/c string? exists?)][s
 (-> (listof any/c) X?)]) (listof X?)]{reads the content of file @racket[f] and
 produces it as list of rows, each constructed via @racket[s]}

@examples[#:eval (examples-batch-io)
(read-csv-file/rows "data.csv" (lambda (x) x))
(read-csv-file/rows "data.csv" length)
]
 The first example shows how @racket[read-csv-file] is just a short form
 for @racket[read-csv-file/rows]; the second one simply counts the
 number of separated tokens and the result is just a list of numbers. 
 In many cases, the function argument is used to construct a structure from
 a row.}
]

There is only one writer function at the moment: 
@itemlist[

@item{@defproc[(write-file [f string?] [cntnt string?]) string?]{
 turns @racket[cntnt] into the content of file @racket[f], located in the
 same folder (directory) as the program. If the write succeeds, the
 function produces the name of the file (@racket[f]); otherwise it signals
 an error.}

@examples[#:eval (examples-batch-io)
(if (string=? (write-file "output.txt" "good bye") "output.txt")
    (write-file "output.txt" "cruel world")
    (write-file "output.txt" "cruel world"))
]
 After evaluating this examples, the file named @racket["output.txt"]
 looks like this: 
 @(file-is "output.txt")
 Explain why.
}
]

@(parameterize ([current-directory here])
   (with-handlers ([exn:fail:filesystem? void])
     (delete-file "output.txt")))

@section{Testing}

@defform[(simulate-file process str ...)]{
 simulates a file system for the function @racket[process], which reads a
 file and may produce one. Note: this form is under development and will be
 documented in a precise manner after it is finalized and useful for a wide
 audience.} 
