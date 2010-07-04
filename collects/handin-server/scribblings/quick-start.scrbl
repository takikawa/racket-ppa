#lang scribble/doc
@(require "common.ss")

@title{Quick Start for a Test Drive}

@itemize[
@item{Create a new directory.}

@item{Copy @filepath{server-cert.pem} from the
  @filepath{handin-client} collection to the new directory.

  NOTE: For real use, you need a new certificate.

  NOTE: See also @secref{wheres-the-collection}.}

@item{Copy @filepath{private-key.pem} from the
  @filepath{handin-server} collection to the new directory.

  NOTE: For real use, you need a new key.}

@item{Create a file @filepath{users.ss} with the following content:
  @schemeblock[
    ((tester ("8fe4c11451281c094a6578e6ddbf5eed"
              "Tester" "1" "test@cs")))]}

@item{Make a @filepath{test} subdirectory in your new directory.}

@item{Create a file @filepath{config.ss} with the following content:
  @schemeblock[((active-dirs ("test"))
                (https-port-number 7980))]}

@item{In your new directory, run @commandline{mred-text -l handin-server}}

@item{In the @filepath{handin-client} collection, edit
  @filepath{info.ss} and uncomment the lines that define
  @scheme[server:port], @scheme[tools], @scheme[tool-names], and
  @scheme[tool-icons].}

@item{Run @commandline{setup-plt -l handin-client}

  NOTE: Under Windows, the executable is @exec{Setup PLT} instead of
  @exec{setup-plt}.

  NOTE: The command line arguments are optional, it restricts the
  setup work to the specified collection.}

@item{Start DrScheme, click @onscreen{Handin} to run the client,
  submit with username ``@tt{tester}'' and password ``@tt{pw}''.

  The submitted file will be @filepath{.../test/tester/handin.scm}.}

@item{Check the status of your submission by pointing a web browser at
  @tt{https://localhost:7980/}.  Note the ``s'' in ``https''.  Use the
  ``@tt{tester}'' username and ``@tt{pw}'' password, as before.

  NOTE: The @scheme[https-port-number] line in the
  @filepath{config.ss} file enables the embedded secure server.  You
  can remove it if you don't want it.}
]
