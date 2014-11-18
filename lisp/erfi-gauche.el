;;; erfi-gauche.el --- Features from Gauche -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: extensions, lisp
;; URL: https://github.com/kenoss/erfi
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'cl)
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'erfi-srfi-1)



;;; Combinators

(defun erfi:complement (pred)
  (lambda (x) (not (funcall pred x))))



;;;
;;; Lists
;;;

(defun erfi:length<=? (xs k)
  "[Gauche] Return t if and only if XS has length less than or equal to K."
  (progn
    (unless (integerp k)
      (lwarn 'erfi :error "`erfi:length<=?': argument out of range: %s" k)
      (error "`erfi:length<=?': argument out of range: %s" k))
    (while (and (consp xs)
                (not (zerop k)))
      (pop xs)
      (decf k))
;    (and (not (zerop k)) (not (consp xs)))))
    (not (and (zerop k) (consp xs)))))


(defun erfi:take* (xs i &optional fill? padding)
  "[Gauche] More tolerant version of `erfi:take'.
I can be less than length of XS.  In that case, return XS if FILL? is nil.
If non-nil, return newly allocated list, filled with PADDING.  PADDING defaults
to nil."
  (let1 rs '()
    (unless (and (integerp i) (<= 0 i))
      (lwarn 'erfi :error "`erfi:take*': argument out of range: %s" i)
      (error "`erfi:take*': argument out of range: %s" i))
    (let1 j (catch 'break
              (while (not (zerop i))
                (when (null xs)
                  (throw 'break (if fill? i 0)))
                (push (pop xs) rs)
                (decf i))
              0)
      (erfi:append-reverse rs (make-list j padding)))))

(defun erfi:drop* (xs i)
  "[Gauche] More tolerant version of `erfi:drop'.
I can be greater than length of XS.  In that case, return nil."
  (progn
    (unless (and (integerp i) (<= 0 i))
      (lwarn 'erfi :error "`erfi:drop*': argument out of range: %s." i)
      (error "`erfi:drop*': argument out of range."))
    (while (not (zerop i))
      (pop xs)
      (decf i))
    xs))

(defun erfi:split-at* (xs i &optional fill? padding)
  "[Gauche] More tolerant version of `erfi:split-at'.
Rerturn a list (list (erfi:take* xs i fill? padding) (erfi:drop* xs i)) ."
  (let1 rs '()
    (unless (and (integerp i) (<= 0 i))
      (lwarn 'erfi :error "`erfi:split-at*': argument out of range: %s" i)
      (error "`erfi:split-at*': argument out of range: %s" i))
    (let1 j (catch 'break
              (while (not (zerop i))
                (when (null xs)
                  (throw 'break (if fill? i 0)))
                (push (pop xs) rs)
                (decf i))
              0)
      (erfi:values (erfi:append-reverse rs (make-list j padding)) xs))))


(defun erfi:list-split (xs pred &optional not-collapse)
  "Like `string-split' in Gauche, split XS at the elements satisfying PRED,
and return a list of them.  If NOT-COLLAPSE is t, nil is inserted for the place
of successive match.

Examples:

  (erfi:list-split '(0 1 2 3 4) (lambda (x) (= 2 x)))
  => ((0 1) (3 4))
  (erfi:list-split '(0 1 2 3 4) 'evenp)
  => (() (1) (3) ())
  (erfi:list-split '(0 a 2 c 4 5 6 d 8 nil) 'integerp)
  => ((a) (c) (d) (nil))
  (erfi:list-split '(0 a 2 c 4 5 6 d 8 nil) 'integerp t)
  => (nil (a) (c) nil nil (d) (nil))"
  (erfi:let lp ((xs xs) (stack '()) (res '()))
    (cond ((null xs)
           (nreverse (if (and (not not-collapse) (null stack))
                         res
                         (cons (nreverse stack) res))))
          ((funcall pred (car xs))
           (lp (cdr xs) '() (if (and (not not-collapse) (null stack))
                                res
                                (cons (nreverse stack) res))))
          (t
           (lp (cdr xs) (cons (car xs) stack) res)))))




(provide 'erfi-gauche)
;;; erfi-gauche.el ends here
