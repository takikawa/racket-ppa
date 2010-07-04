#lang scribble/doc
@(require "common.ss")

@title[#:tag "wheres-the-collection"]{Where is the collection?}

If you obtained the server and client by installing a @filepath{.plt}
file, then the @filepath{handin-server} and @filepath{handin-client}
directories might be in your PLT addon space.  Start MzScheme, and
enter @schemeblock[(collection-path "handin-server")]
@schemeblock[(collection-path "handin-client")] to find out where
these collections are.
