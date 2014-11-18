;;; erfi-gauche-test.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>

;; Apache License, Version 2.0

;;; Commentary:

;;; Code:



(require 'ert)

(require 'erfi-macros)
(erfi:use-short-macro-name)

(require 'cl-lib)
(require 'cl)



;;;

(ert-deftest erfi-test:length<=? ()
  (should (eq t
              (erfi:length<=? '(a b) 2)))
  (should (eq nil
              (erfi:length<=? '(a b) 1)))
  (should (eq t
              (erfi:length<=? '()    0)))
  ;; dotted list cases
  (should (eq t
              (erfi:length<=? 'a       0)))
  (should (eq nil
              (erfi:length<=? '(a . b) 0)))
  (should (eq t
              (erfi:length<=? '(a . b) 1)))
  )


(ert-deftest erfi-test:take*+drop* ()
  (should (equal '(a b c)
                 (erfi:take* '(a b c d) 3)))
  (should (equal '(a b c d)
                 (erfi:take* '(a b c d) 6)))
  (should (equal '(a b c d nil nil)
                 (erfi:take* '(a b c d) 6 t)))
  (should (equal '(a b c d z z)
                 (erfi:take* '(a b c d) 6 t 'z)))
  (should (equal '(d)
                 (erfi:drop* '(a b c d) 3)))
  (should (equal '()
                 (erfi:drop* '(a b c d) 6)))
  (should (equal '((a b c d z z) ())
                 (erfi:split-at* '(a b c d) 6 t 'z)))
  )


(ert-deftest erfi-test:list-split ()
  (should (equal '((0 1) (3 4))
                 (erfi:list-split '(0 1 2 3 4) (lambda (x) (= 2 x)))))
  (should (equal '(() (1) (3) ())
                 (erfi:list-split '(0 1 2 3 4) 'evenp)))
  (should (equal '((a) (c) (d) (nil))
                 (erfi:list-split '(0 a 2 c 4 5 6 d 8 nil) 'integerp)))
  (should (equal '(nil (a) (c) nil nil (d) (nil))
                 (erfi:list-split '(0 a 2 c 4 5 6 d 8 nil) 'integerp t)))
  )


;;; erfi-gauche-test.el ends here
