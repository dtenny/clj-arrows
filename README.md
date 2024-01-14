# About

Provides the threading and/or transformation macros of Clojure to an arguably
stupid level of compatibility. Also provides some diamond wand macros
not found (at the time of this writing) in Clojure just so I can avoid importing
other arrow macro packages.

Most people will have no need of this package unless they've been doing a lot of Clojure.
It isn't like the world needs another arrow macro package.
See the 'prior art' section if you want the nitty gritty details.

# Clojure Compatability / Rationale

I don't generally try for exact CL replicas of Clojure syntax and semantics
with Clojure tooling adaptations to Common Lisp (see my various CLJ-* repos). Instead I try
to adapt Clojure idioms to CL style, perhaps returning a list instead of a
vector, or multiple values instead of a list. 

That said, all symbols in _this_ package (CLJ-ARROWS) with namesakes
in Clojure's core library provide _exactly_ the same behavior
as Clojure at the time of this writing (Dec 2022), AFAIK.

# Usage

    (ql:quickload :clj-arrows)

Add it to the USE list of your package, or not, it will work either way with regard
to `<>` symbol processing in diamond want macros, e.g.

   (clj-arrows:-<> 1 (list 2 3) (list 4 <> 5)) => (4 (1 2 3) 5)

To run the tests:

    (ql:quickload :clj-arrows-test)
    (clj-arrows-test:run-tests)

Tested with sbcl 2.3.0 and abcl 1.9.0 in a Fedora 35 linux.

# Documentation

Refer to docstrings on the exported symbols which provide both semantic descriptions
as well as examples of usage.  If you have used similar macros elsewhere note
the following potential compatability issues
(described in more detail in 'prior art' below).

1. If the macro has a clojure.core namesake in Clojure, it behaves identically.
2. Point #1 means that the `cond->` and `cond->>` macros use unparenthesized test + expr pairs, unlike CL's `cond` function and some `cond->` macro implementations.
3. The diamond-wand implementation allows multiple `<>` substitutions, and does not evaluate the substituted expression more than once.


# Exported symbols

   ;; Clojure compatible macros

   #:->
   #:->>
   #:as->
   #:cond->
   #:cond->>
   #:some->
   #:some->>

   ;; Additional macros not in clojure at the time of this writing.

   #:-<>
   #:-<>>
   #:some-<>
   #:some-<>>


# Prior art (and license notes FMI)

Clojure, of course, and the following packages while looking for Clojure substitutes
in Common Lisp.

## [arrow-macros](https://github.com/hipeta/arrow-macros/)

MIT license.

I'm guessing this is what most Common Lisp users use. It is a nice library and
it was my go-to library for a while in CL. It's the only one that supports
Clojure `cond->` semantics (yay!), and it also supports multiple `<>` substitutions 
in diamond wand macros.

I had three issues with the library:

1. There are no docstrings on the exported macros. If ever macros deserved docstrings, it's these.
2. There is a bug (IMO) in the diamond wand macros.
3. The behavior of `->` and similar macros is not consistent with Clojure (see below).

On 2, the bug is such that diamond wand substitutions won't work unless you
:USE the package (or are in the :arrow-macros package) so that its internal
`(eq '<> x)` checks will work.  If `<>` reference was interned in
another package, which it will be if you don't :USE the arrow-macros package, then
you'll get syntax errors. See [issues](https://github.com/hipeta/arrow-macros/issues/4).

On 3, basically ARROW-MACROS is smart enough to recognize lambda forms, and avoid
inserting the threaded value into the form. E.g.

In Clojure:
`(-> 1 (fn [x] (+ x 1)))` expands to `(fn 1 [x] (+ x 1))`.  Oops. 

In CLJ-ARROWS and perhaps other packages, the same thing happens:
`(-> 1 (lambda (x) (+ x 1)))` expands to `(lambda 1 (x) (+ x 1))`.  
Oops, but compatible with clojure.

However the ARROW-MACROS package will expand it to `(funcall (lambda (x) (+ x 1)) 1)`. 
It also do some funcall transformations on FUNCTION references.

What ARROW-MACROS does with respect to functions is nice, but inconsistent with Clojure.
Nice experiment, but "do what I mean" taste.  If you're
trying to keep your brain from breaking because you switch-hit between the two
languages (and perhaps share code between the two), stupid consistency may be
better.  Or not. Whatever. For now the CLJ-ARROWS package does it the Clojure way
and my mental hobgoblins are at rest.

The way you normally work around this FN/LAMBDA or similar problem in Clojure
(and in CL with CLJ-ARROWS) is to simply
add a set of parens, e.g. `(-> 1 ((lambda (x) (+ x 1))))`.

## [arrows](https://github.com/Harleqin/arrows)

Incompatible `cond->` semantics.  CC0 license.

## [cl-swiss-arrows](https://github.com/andy128k/cl-swiss-arrows/)

Focuses only on diamond wand macros, doesn't have the clojure macros.  No license stated.

CL-SWISS-ARROWS supports multiple `<>` substitutions in forms, and avoids
re-evaluating the substituted expression.  This is unlike some implementations
including the Clojure SWISS-ARROWS implementation that appears to have been
the inspiration for diamond wand macros. CLJ-ARROWS adopts the behavior of CL-SWISS-ARROWS.

## [cl-threading-macros](https://github.com/horellana/cl-threading-macros)

Only has `->` and `->>`.  GPL3 license.

## [simple-arrows](https://github.com/digikar99/simple-arrows)

Only has `->` and `->>`. No license stated.

## [swiss-arrows](https://github.com/rplevy/swiss-arrows)

This is the primary clojure addon package 
(in clojure, for clojure, but not in clojure.core) cited
as inspiration for some of the above Common Lisp arrow packages.

Note that this clojure version of diamond wand macros allows only a single substitution
in a form, while CLJ-ARROWS supports CL-SWISS-ARROWS behavior that allows multiple
substutitions (evaluated only once).
