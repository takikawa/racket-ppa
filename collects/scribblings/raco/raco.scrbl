#lang scribble/doc
@(require scribble/manual
          "common.ss")

@title{@exec{raco}: Racket Command-Line Tools}

The @exec{raco} program supports various Racket tasks from a command
line. The first argument to @exec{raco} is always a specific command
name. For example, @exec{raco make} starts a command to compile a
Racket source module to bytecode format.

The set of commands available through @exec{raco} is extensible. Use
@exec{raco help} to get a complete list of available commands for your
installation. This manual covers the commands that are available in
a typical Racket installation.

@table-of-contents[]

@include-section["make.scrbl"]
@include-section["exe.scrbl"]
@include-section["dist.scrbl"]
@include-section["plt.scrbl"]
@include-section["planet.scrbl"]
@include-section["setup.scrbl"]
@include-section["decompile.scrbl"]
@include-section["ctool.scrbl"]
@include-section["command.scrbl"]
