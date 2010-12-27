# syntax-rules

A library for defining hygienic macros based on pattern matching. This library
is an adaptation of syntax-rules from Scheme and incorporates some of the
features in syntax-parse from Racket.

## Usage

Macros are defined using `#'qbg.syntax-rules/defsyntax-rules`. A defsyntax-rules
form is of the form `(defsyntax-rules name literals rt+)`, where `rt` is a rule-
template pair (see Examples), and `literals` is a vector of symbols to be treated
as literals in the template.

Certain lists are treated specially when they appear in a pattern/template. The
list `(+literal <item>)` will cause `<item>` to be treated as a literal during
matching and filling in the template.

The list `(+& <pattern> ...)` in like an ellipsis, except there can be multiple
patterns under it, where each pattern is repeated in sequence (see the `plet`
example in Examples).

The list `(+describe <mesg> <pattern>)` will push `<mesg>` on the description
stack during the matching of `<pattern>`. `<mesg>` should be a string that
describes what type of expression `<pattern>` is going to match.

The list `(+head <pattern> ...)` will match each `<pattern>` in turn in the
current context. That is, `(1 (+head 2 3) 4)` and `(1 2 3 4)` are equivalent
patterns.

The list `(+and <pattern> ...)` and `(+or <pattern> ...)` will match if all/at
least one of `<pattern>` matches respectively.

The list `(+pattern <pattern> <template>)` will match `<pattern>` against the
result of filling in `<template>` with the current progress of the matching.
This operation consumes no input.

When a macro defined defsyntax-rules encounters a syntax error (that is, when
none of the rules match), all of the rules are examined to determine which one
has matched the most of the form.  An error message is then generated from the
message that was on the top of the describe stack when the matching failed.

When the template is filled in, symbols that are not pattern variables are
treated in one of two ways: a symbol having a top-level definition becomes
namespace-qualified, and a symbol that does not is automatically gensym'd. All
special forms that do not `#'resolve` are treated as literal symbols.

## Examples

A `while` macro can be defined as:
    (defsyntax-rules while []
      (while condition body ...)
      (loop []
        (when condition
	  body ...
	  (recur))))

A for-each-style `for` macro that supports multiple syntaxes can be defined as:
    (defsyntax-rules for []
      (for var :in coll body ...)
      (dorun (map (fn [var] body ...) coll))
      (for coll :as var body ...)
      (for var :in coll body ...))
Here `(for x :in [1 2 3] (println x))` and `(for [1 2 3] :as x (println x))`
will have the same effect.

The above `for` example can be simplified by the use of +or and +head patterns:
    (defsyntax-rules for []
      (for (+or (+head var :in coll)
      	        (+head coll :as var))
	   body ...)
      (dorun (map (fn [var] body ...) coll)))

A CL/Scheme-style `let` with the flat binding structure of Clojure's `let` can
be defined as:
    (defsyntax-rules plet []
      (plet [(+& var rhs)] body ...)
      ((fn [var ...] body ...) rhs ...))
With this definition, `(plet [a 1 b 2] (+ a b))` will evaluate to `3`.

A squaring macro can be defined as:
    (defsyntax-rules square []
      (square n)
      (let [x n]
        (* x x)))
Because of the auto-gensym, `(square (+ 2 3))` will macroexpand into something
of the form `(let* [x205 (+ 2 3)] (clojure.core/* x205 x205))`.

`defsyntax-rules` can also be defined in terms of itself:
    (defsyntax-rules defsyntax-rules [& forms &form]
      (defsyntax-rules name [literals ...] (+& rules templates))
      (let [ar (make-apply-rules 'name '[literals ...] '(rules ...) '(templates ...))]
        (defmacro name
	  [& forms]
	  (ar &form))))

## Limitations

There is currently no way to give a macro a docstring.

There is currently no way to give a meaningful argument list to a macro.

The syntax error messages do not indicate where the macro was used.

## License

Copyright (C) 2010 Brian Goslinga

Distributed under the Eclipse Public License, the same as Clojure.
