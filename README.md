# CL-SPEC

This library is an implementation of Ruby rspec for behavioral testing, in
Common Lisp.

## Concepts

Specifications are an alternative to the standard unit test syntax
(SUnit, JUnit, and their ports).

A specification is a list of examples.  Each example has a name, and
a body.  The body can contain any lisp code, as well as forms that
begin with `=>`.

A specification is run to produce a list of results: which examples
passed, and which failed (and with what condition, although for now
the condition text is not very useful).

See the Lisp files in the examples directory for some very simple
examples of specifications.

## Loading

    (asdf:oos 'asdf:load-op :cl-spec)

## Usage

There are three ways to run a spec.

First, by evaluating it (from the command line, or in an editor
window).  This runs the specification, and returns a short string
counting the number of tests that succeeded, or listing the names of
the tests that failed.  The intent is that this will be useful to look
at in your minibuffer or on your console.

Second, by applying `RUN-SPECIFICATION` to a path name (`STRING` or
`PATHNAME`):
    (run-specification "examples/failing-spec.lisp")

This prints a text summary of the pass/fail status to standard output,
like so:

    .FF..
    
    1)
    "should fail" FAILED
    Condition EXPECTATION-NOT-MET was signalled.
    /users/osteele/documents/projects/cl-spec/examples/failing-spec.lisp
    
    2)
    "should also fail" FAILED
    Condition EXPECTATION-NOT-MET was signalled.
    /users/osteele/documents/projects/cl-spec/examples/failing-spec.lisp
    
    Finished in 0.0050000004 seconds
    
    5 examples, 2 failures

Finally, the `:FORMAT` keyword can be used to create an HTML file instead:

    (run-specification "examples/plus-spec.lisp" :format 'html)

The HTML files in the examples directory were thus produced.


## License

This package is Copyright 2007 by Oliver Steele.  It is available under the
terms of the MIT License.
