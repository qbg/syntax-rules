# syntax-rules

A library for defining hygienic macros based on pattern matching. This library
provides many features of `syntax-parse` from Racket and provides a
`syntax-rules`-based interface and a `syntax-case`-based interface to its
facilities.

## Usage

The syntax-rules wiki page has a
[reference](https://github.com/qbg/syntax-rules/wiki/Reference) page that
documents the usage of the library.

## Builtin syntax classes

`c-symbol`, `c-number`, `c-keyword`, `c-map`, `c-set`, and `c-string`  are the
syntax classes for symbols, numbers, keywords, maps, sets, and strings.

`c-pred` is the predicate syntax class. It takes two parameters: `pred` and
`mesg`. A form will match this syntax class only if `(pred form)` returns true.
`mesg` is a string that describes the types of forms match by `pred`. 

## Examples

A `while` macro can be defined as:
    (defsyntax-rules while
      "Repeatedly execute body until condition is false"
      []
      (while condition body ...)
      (loop []
        (when condition
	  body ...
	  (recur))))

A for-each-style `for` macro that supports multiple syntaxes can be defined as:
    (defsyntax-rules for
      "for-each style for loop"
      []
      (for var :in coll body ...)
      (dorun (map (fn [var] body ...) coll))
      (for coll :as var body ...)
      (for var :in coll body ...))
Here `(for x :in [1 2 3] (println x))` and `(for [1 2 3] :as x (println x))`
will have the same effect.

The above `for` example can be simplified by the use of `+or` and `+head` patterns:
    (defsyntax-rules for
      "for-each style for loop"
      []
      (for (+or :& [var :in coll]
      	        :& [coll :as var])
	   body ...)
      (dorun (map (fn [var] body ...) coll)))

A CL/Scheme-style `let` with the flat binding structure of Clojure's `let` can
be defined as:
    (defsyntax-rules plet
      "Scheme-style let"
      []
      (plet [:& [var rhs] ...] body ...)
      ((fn [var ...] body ...) rhs ...))
With this definition, `(plet [a 1 b 2] (+ a b))` will evaluate to `3`.

A squaring macro can be defined as:
    (defsyntax-rules square
      "Square n"
      []
      (square n)
      (let [x n]
        (* x x)))
Because of the auto-gensym, `(square (+ 2 3))` will macroexpand into something
of the form `(let* [x205 (+ 2 3)] (clojure.core/* x205 x205))`.

`+or` and `+head` patterns are a powerful tool when combined with the
ellipsis. With the combination of these three patterns, keyword arguments can be
supported to some degree. Even more, the keywords do not need to have a uniform
structure, that is they can take a varying number of arguments:
    (defsyntax-rules foo
      "An interesting example"
      []
      (foo (+or :& [:a a] :& [:b b c]) ...)
      '[[a ...] [[b c] ...]])
    
    (foo :a 1 :b 2 3 :a 4 :a 5 :b 6 7 :b 8 9 :a 10)
    ;=> [[1 4 5 10] [[2 3] [6 7] [8 9]]]

A better version of `plet` can be defined using syntax classes:
    (defsyntax-class binding-vector []
      "binding vector"
      []
      [:& [var :> c-symbol rhs] ...]
      :fail-when (check-duplicate (syntax (var ...))) "duplicate variable name")
    
    (defsyntax-rules plet
      "Scheme-style let"
      []
      (plet bv :> binding-vector body ...)
      ((fn [bv.var ...] body ...) bv.rhs ...))
The definition is this way because to `plet` it is considered a syntax error for
multiple `var`s to have the same name. We can see how this plays out in action:
    (plet [a 1, a 2] (+ a b))
    ; java.lang.Exception: plet: duplicate variable name in: a (qbg/syntax_rules.clj:2) (NO_SOURCE_FILE:0)
    
    (plet [a 1, b 2, c] (+ a b))
    ; java.lang.Exception: plet: expected binding vector in: [a 1 b 2 c] (qbg/syntax_rules.clj:4) (NO_SOURCE_FILE:0)
    
    (plet 17)
    ; java.lang.Exception: plet: expected binding vector in: 17 (qbg/syntax_rules.clj:5) (NO_SOURCE_FILE:0)
As seen, the use of syntax classes provide a sharper error message than the
first definition.

We can also define macros using the `syntax-case`-based interface:
    (defsyntax-case adder
      "Add two numbers together"
      []
      (adder a b)
      `(+ ~(syntax a) ~(syntax b)))
    
    (macroexpand '(adder 1 2))
    ;=> (clojure.core/+ 1 2)

## Limitations

The arglists of a macro defined by defsyntax-rules/defsyntax-case is the list
of rules.

## License

Copyright (C) 2010 Brian Goslinga

Distributed under the Eclipse Public License, the same as Clojure.
