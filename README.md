# syntax-rules

syntax-rules is a mechanism in Scheme for defining hygienic macros based off of
pattern matching.  This project provides a version of syntax-rules that designed
with Clojure in mind.

## Usage

A syntax-rules macro is defined using #'qbg.syntax-rules/defsyntax-rules.
defsyntax-rules takes the name of the macro to be defined and then a series of
rule template pairs.  There is currently no way to give a meaningful docstring
or arglist to the defined macro.

When the macro is invoked, each rule will be tried in turn; upon the first
successful rule, the corresponding template will be filled in and returned as
the expansion.  Symbols appearing in the rule are pattern variables and will
match any expression; in the template, they will be replaced with what they
matched. Lists and vectors will match lists and vectors where each sub-pattern
matches. Anything else is treated as a literal.

Any list of the form (+literal item) in a rule/template will be treated such
that item is a literal. This is useful for preventing the interpretation of
items such as & that would otherwise have special behavior.  A list of the form
(+& <patterns>) is equivalent to & <patterns> except that other patterns may
follow (& would normally consume all remaining patterns in the list).  Note
during matching only the first ampersand clause will be used; all patterns
after it will be ignored. (+describe <description> <pattern>) will describe the
pattern using description. This is useful for providing better error messages.

In the case that no rule matches the macro invocation, a bad syntax exception
will be thrown that contains the incorrect code.

Any sequence of patterns appearing after an & will match against the rest of the
current sequence.  The pattern variables appearing in these patterns must appear
after the corresponding number of & in the template.

If a symbol appearing in a pattern is not a pattern variable, on of two things
will happen: if it is a special form, or any symbol that can be
namespace-qualified, then it will stay the same, or be namespace-qualified (in
all other cases).  If it is not one of those symbols, then the symbol will be
automatically gensymed.

For example, we can define a version of let that does bindings in parallel (like
let from Common Lisp/Scheme) as so:
(defsyntax-rules plet
  (plet [& var rhs] & body)
  ((fn [& var] & body) & rhs))

Using this definition, (plet [a 1, b 2] (+ a b)) will evaluate to 3.

To see the gensyming in action, we can take a look at a squaring macro:
(defsyntax-rules square
  (square n)
  (let [x n] (* x x)))

In this case, (square (+ 2 3)) will macroexpand into something like
(let* [x205 n] (clojure.core/* x205 x205))

## License

Copyright (C) 2010 Brian Goslinga

Distributed under the Eclipse Public License, the same as Clojure.
