ERFI
====

ERFI, standing for "Emacs Lisp Reconstruction for Indivisuals", is a set of
libraries to fill the gap of default Emacs Lisp and practical applications.
ERFI is developed under the slogan of "Make and keep codes readable, maitenable
and elegant with admissible efficiency loss".  To realize it, APIs of ERFI are
based on SRFI, as you may know, libraries standing for "Scheme Requests for
Implementation".  ERFI also takes in philosophy of Gauche and Haskell.


## Introduction

An example is `erfi:let`, an implementation of named let with interface of
SRFI-5.  This realizes Tail Recursion Optimization (not Tail Call Optimization)
by expanding it to usual efficient while loop.  One can use it for recursion
without spending stack;  calculation of factorial with nomral recursion will
cause stack overflow.  (Emacs's default stack size is 600.)  A simple example is
the following:

```emacs-lisp
(require 'erfi-macros)

(defun fact (n)
  (if (< n 0)
      "Argument out of range"
      (erfi:let lp ((n n) (r 1))
        (if (zerop n)
            r
            (lp (- n 1) (* r n))))))

(fact 5)
; => 120
```

The above code is tedious and uninteresting;  everyone can write down it using
explicit while loop.  However, the real power of `erfi:let` will be seen, for
example, when you use it for loop more than one dimensional.  In my humble
opinion, these recursion, looking like normal recursion, is more readable than
"loop" langage in CL because named let is in the range of Lisp langage.  Writing
complicated loop will show the difference vividly.

Another aspect is functional programming.  ERFI recommends and supports it in
the case that performance requirement is not so strict.  There are a lot of
functions and macros to write functional codes easily and elegantly.  In
general, FP makes codes simple and expresses directly what the code should do;
lesser side effects make appearance of bugs rarerer.

```emacs-lisp
(require 'erfi-macros)
(erfi:use-short-macro-name)
(require 'erfi-srfi-1)

;; SRFI-26 `cut' is shorter `lambda' for special case.
(mapcar (cut * 2 <>) (erfi:iota 10))
; => (0 2 4 6 8 10 12 14 16 18)

;; 2 * 5!
(apply (cut '* 2 <...>) (erfi:iota 5 1))
; => 240

;; "uniq" in shells for list.
(nreverse (erfi:foldl (lambda (acc x)
                        (if (eq x (car acc))
                            acc
                            (cons x acc)))
                      '()
                      '(a a a b b c c b c d d d)))
; => (a b c b c d)

;; Non anaphoric `aif'
(if-let1 ans (y-or-n-p "Do you like anaphoric macros and dynamic bindings?")
  (message "You'll be tangled with bugs caused by them.")
  (message "Ditto."))

(let1 alist `((:foo . ,(erfi:iota 10))
              (:bar . ,(make-list 10 9))
              (:bla . "not used")
              (:baz . (1 9 8 8 0 1 1 8)))
  ;; Take assosiated values, take only odds, and take head 3 element.
  (erfi:map (erfi:$ erfi:take <> 3 $ erfi:filter 'oddp $ cdr-safe $ 'assq $*)
            '(:foo :bar :baz)
            (erfi:circular-list alist)))
; => ((1 3 5) (9 9 9) (1 9 1))
```

Here `erfi:$` is Haskell-ish composition from Gauche, which emphasizes flow of
data and allows one to avoid deep indentations.


## Features and states

|  Feature                                                    | Library            | State                | Test       |
| ----------------------------------------------------------- | ------------------ | -------------------- | ---------- |
|  Macros imported from SRFI and Gauche                       | erfi-macros.el     | Complete and matured | Good       |
|  SRFI-1 List Library                                        | erfi-srfi-1.el     | Complete and matured | Incomplete |
|  SRFI-13 String Library                                     | erfi-srfi-13.el    | Not all              | Incomplete |
|  Features from Gauche                                       | erfi-gauche.el     | As I need            | Incomplete |
|  Hash tables                                                | erfi-hash-table.el | As I need            | Good       |
|  Regular expressions                                        | erfi-regexp.el     | As I need            | Incomplete |
|  Haskell influenced record syntax                           | erfi-hairs.el      | Experimental         | Incomplete |
|  Name spaces                                                | erfi-namespace.el  | Under construction   | Nothing    |
|  Wrappers of Emacs APIs and auxiliaries for practical apps. | erfi-emacs.el      | As I need            | Incomplete |
|  Miscellaneous, currently only etched overlays              | erfi-misc.el       | As I need            | Incomplete |


## TODO

- Write complete test for `erfi-srfi-1.el`.
- Test and compare implementations of SRFI-1.
- Write doc string in `erfi-srfi-1.el`.
