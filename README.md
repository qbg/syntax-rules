# syntax-rules

A library for defining hygienic macros based on pattern matching. This library
provides many features of `syntax-parse` from Racket and provides a
`syntax-rules`-based interface and a `syntax-case`-based interface to its
facilities.

## Usage

The syntax-rules wiki page has a
[reference](https://github.com/qbg/syntax-rules/wiki/Reference) page that
documents the usage of the library.

## Limitations

The arglists of a macro defined by defsyntax-rules/defsyntax-case is the list
of rules.

## License

Copyright (C) 2010 Brian Goslinga

Distributed under the Eclipse Public License, the same as Clojure.
