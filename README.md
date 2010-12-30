# syntax-rules

A library for defining hygienic macros based on pattern matching. This library
provides many features of `syntax-parse` from Racket and provides a
`syntax-rules`-based interface and a `syntax-case`-based interface to its
facilities.

## Usage

Macros are defined using `#'qbg.syntax-rules/defsyntax-rules`. A defsyntax-rules
form is of the form `(defsyntax-rules name literals rt+)`, where `rt` is a rule-
template pair (see Examples), and `literals` is a vector of symbols to be treated
as literals in the template.

Certain lists are treated specially when they appear in a pattern/template. The
list `(+literal <item>)` will cause `<item>` to be treated as a literal during
matching and filling in the template.

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

The list `(+guard <code> <mesg>)` will execute the form `<code>`. If `<code>`
returns a true value, `<mesg>` will be reported as the type of error with the
result being the form that caused the error. This operation consumes no input.

The list `(+var <name> <class> <args> ...)` is a pattern variable declaration
for `<name>` with the restriction that it will only match if the syntax class
named by `<class>` with `<args>` as arguments matches. A pattern variable named
`foo` that is defined in the syntax class can be accessed by `<name>.foo`. See
`defsyntax-class` for defining syntax classes.

The list `(+code <code>)` will during the filling of the template be replaced by
the value of the execution of `<code>`. Any nested `syntax` uses will be treated
as separate templates, and so auto-gensyms will be different.

The list `(+options [<patterns>+ ]+)` is like
`(+or (+head <patterns>+)+) ...`, that is, the repetition of the `+or` of a
sequence of head patterns except that pattern variables will have an ellipis
depth one less than they would otherwise. Because it is an error for pattern
variables to be bound more than one time, this operation will consume the right
amount of input.

When a macro defined defsyntax-rules encounters a syntax error (that is, when
none of the rules match), all of the rules are examined to determine which one
has matched the most of the form.  An error message is then generated from the
message that was on the top of the describe stack when the matching failed.

When the template is filled in, symbols that are not pattern variables are
treated in one of two ways: a symbol having a top-level definition becomes
namespace-qualified, and a symbol that does not is automatically gensym'd. All
special forms that do not `#'resolve` are treated as literal symbols.

## Syntax sugar

`:! <form>` may be used to treat `<form>` as a literal value.

`:& [<patterns> ...]` may be used as an abbreviation for
`(+head <patterns> ...)`.

`<var> :> <syntax-class>` may be used as an abbreviation for
`(+var <var> <syntax-class>)`; if `<syntax-class>` is a list, the first element
will name the syntax class and the remaining elements will be the syntax class's
arguments.

## Additional functions/macros

`#'qbg.syntax-rules/syntax` takes some template (and optionally a vector of
literals) and fills in the template using the result of the current match. This
is the mechanism to gain access to the values of pattern variables in a guard.

`#'qbg.syntax-rules/absent?` is a function that takes the name of a pattern
variable (a symbol) and returns true if the pattern variable has not been bound
in the current match.

`#'qbg.syntax-rules/check-duplicate` is a function suitable for some guards. It
will return the element in a collection that is duplicated if there is one, or
false otherwise.

`#'qbg.syntax-rules/defsyntax-class` is a macro for defining a syntax class. It
takes the name of the syntax class, the argument vector for the syntax class,
the description of the syntax class, the vector of literals used in the syntax
class, and then finally its body. The body is split into multiple parts; each
part consists of a pattern, and then various options. The :fail-when option
takes two arguments, a message and an expression; the semantics are that of a
`+guard` directive. The :with option takes a pattern and a template; its
semantics are equivalent to that of a `+pattern` directive. See the second
`plet` example for a definition of a syntax class. 

`#'qbg.syntax-rules/defsyntax-case` is the syntax-case version of
`defsyntax-rules`. That is, instead of a template there is an expression to
execute when the corresponding rule matches.

## Builtin syntax classes

`c-symbol`, `c-number`, `c-keyword`, `c-map`, `c-set`, and `c-string`  are the
syntax classes for symbols, numbers, keywords, maps, sets, and strings.

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

The above `for` example can be simplified by the use of `+or` and `+head` patterns:
    (defsyntax-rules for []
      (for (+or :& [var :in coll]
      	        :& [coll :as var])
	   body ...)
      (dorun (map (fn [var] body ...) coll)))

A CL/Scheme-style `let` with the flat binding structure of Clojure's `let` can
be defined as:
    (defsyntax-rules plet []
      (plet [:& [var rhs] ...] body ...)
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
      (defsyntax-rules name [literals ...] :& [rules templates] ...)
      (let [ar (make-apply-rules 'name '[literals ...] '(rules ...) '(templates ...))]
        (defmacro name
	  [& forms]
	  (ar &form))))

`+or` and `+head` patterns are a powerful tool when combined with the
ellipsis. With the combination of these three patterns, keyword arguments can be
supported to some degree. Even more, the keywords do not need to have a uniform
structure, that is they can take a varying number of arguments:
    (defsyntax-rules foo []
      (foo (+or :& [:a a] :& [:b b c]) ...)
      '[[a ...] [[b c] ...]])
    
    (foo :a 1 :b 2 3 :a 4 :a 5 :b 6 7 :b 8 9 :a 10)
    ;=> [[1 4 5 10] [[2 3] [6 7] [8 9]]]

A better version of `plet` can be defined using syntax classes:
    (defsyntax-class binding-vector []
      "binding vector"
      []
      [:& [var rhs] ...]
      :fail-when (check-duplicate (syntax (var ...))) "duplicate variable name"
    
    (defsyntax-rules plet []
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
    (defsyntax-case adder []
      (adder a b)
      `(+ ~(syntax a) ~(syntax b)))
    
    (macroexpand '(adder 1 2))
    ;=> (clojure.core/+ 1 2)

## Limitations

There is currently no way to give a macro a docstring.

There is currently no way to give a meaningful argument list to a macro.

## License

Copyright (C) 2010 Brian Goslinga

Distributed under the Eclipse Public License, the same as Clojure.
