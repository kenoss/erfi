;;; erfi-srfi-13-test.el ---  -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: 
;; URL: https://github.com/kenoss/erfi
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;; 

;;; Code:


(require 'ert)

(eval-when-compile
  (require 'cl)
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'erfi-srfi-13)



;;;
;;; Auxiliary fnuction
;;;

(ert-deftest erfi-test:srfi-13:intersperse ()
  (should (equal '(0 a 1 a 2)
                 (erfi:intersperse 'a '(0 1 2))))
  (should (equal '()
                 (erfi:intersperse 'a '())))
  )



;;;
;;; SRFI-13
;;;

(ert-deftest erfi-test:srfi-13:string-join ()
  (should (equal "foo:bar:baz"
                 (erfi:string-join '("foo" "bar" "baz") ":")))
  (should (equal "foo:bar:baz:"
                 (erfi:string-join '("foo" "bar" "baz") ":" 'suffix)))
  ;; Infix grammar is ambiguous wrt empty list vs. empty string,
  (should (equal ""
                 (erfi:string-join '()   ":")))
  (should (equal ""
                 (erfi:string-join '("") ":")))
  ;; but suffix & prefix grammars are not.
  (should (equal ""
                 (erfi:string-join '()   ":" 'suffix)))
  (should (equal ":"
                 (erfi:string-join '("") ":" 'suffix)))
  (should (equal ""
                 (erfi:string-join '()   ":" 'prefix)))
  (should (equal ":"
                 (erfi:string-join '("") ":" 'prefix)))
  )


(ert-deftest erfi-test:srfi-13:xsubstring ()
  (let1 str "01234"
    (should (equal "01234"
                   (erfi:xsubstring str 0)))
    (should (equal "01234"
                   (erfi:xsubstring str 0 5)))
    (should (equal "123"
                   (erfi:xsubstring str 1 4)))
    (should (equal "340123"
                   (erfi:xsubstring str -2 4)))
    (should (equal "123401"
                   (erfi:xsubstring str 1 7)))
    (should (equal "12340123"
                   (erfi:xsubstring str 6 14)))
    (should (equal "42342342"
                   (erfi:xsubstring str 2 10 2 5)))

    ;; (from to) must be a range.
    (should-error (erfi:xsubstring str 3 1))
    ;; start and end must be in `(0 ,(length str))
    (should-error (erfi:xsubstring str 0 0 -3 2))
    (should-error (erfi:xsubstring str 0 2 0 10))
    ;; Error if start = end,
    (should-error (erfi:xsubstring str 0 5 3 3))
    ;; except for the case from = to.
    (should (equal ""
                   (erfi:xsubstring str 2 2 3 3)))
    ))


;;; erfi-srfi-13-test.el ends here
