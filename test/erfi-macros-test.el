;;; erfi-macros-test.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>

;; Apache License, Version 2.0

;;; Commentary:

;; 

;;; Code:



(require 'ert)

(require 'erfi-macros)
(erfi:use-short-macro-name)

(require 'cl-lib)
(require 'cl)



(defun erfi:undefined ()
  'undefined)



(ert-deftest erfi-test:cut ()
  (flet ((cl-gensym (&optional prefix) 'G000))
    ;; Argument place
    (should (equal '(function (lambda (G000) (funcall '+ G000 1)))
                   (macroexpand '(cut '+ <> 1))))
    (should (equal '(function (lambda (G000 G000)
                                (funcall '+ G000 1 G000)))
                   (macroexpand '(cut '+ <> 1 <>))))
    (should (equal '(function (lambda (G000 &rest G000)
                                (apply '+ G000 1 G000)))
                   (macroexpand '(cut '+ <> 1 <...>))))
    ;; Undefined in SRFI-26
    (should-error  (macroexpand '(cut '+ <> 1 <...> 2)))
    ;; Function place
    (should (equal '(function (lambda (G000)
                                (funcall G000 1)))
                   (macroexpand '(cut <> 1))))
    (should (equal '(function (lambda (G000 G000)
                                (funcall G000 G000 1)))
                   (macroexpand '(cut <> <> 1))))
    (should (equal '(function (lambda (G000 G000 G000)
                                (funcall G000 G000 1 G000)))
                   (macroexpand '(cut <> <> 1 <>))))
    (should (equal '(function (lambda (G000 G000 &rest G000)
                                (apply G000 G000 1 G000)))
                   (macroexpand '(cut <> <> 1 <...>))))
    ;; Undefined in SRFI-26
    (should-error  (macroexpand '(cut <> <> 1 <...> 2)))
    ))

(ert-deftest erfi-test:cut:allows-no-quote ()
  (flet ((cl-gensym (&optional prefix) 'G000))
    ;; We can omit quote of function;
    (should (equal '(function (lambda (G000) (+ G000 1)))
                   (macroexpand '(cut + <> 1))))
    (should (equal '(function (lambda (G000 G000) (+ G000 1 G000)))
                   (macroexpand '(cut + <> 1 <>))))
    ;; but we can't with rest parameters <...>.
    (should (equal '(function (lambda (G000 &rest G000) (apply + G000 1 G000)))
                   (macroexpand '(cut + <> 1 <...>))))
    (should-error (funcall (cut + <> 1 <...>) 0 1 2))
    ;; If one want to use a name of function for a variable, use explicit funcall.
    (let1 list (lambda (x) (* 2 x))
      (should (equal '(1)
                     (funcall (cut list <>) 1)))
      (should (equal 2
                     (funcall (cut funcall list <>) 1))))
    ))

(ert-deftest erfi-test:cut:does-not-evaluate ()
  (flet ((cl-gensym (&optional prefix) 'G000))
    (should (equal '(function (lambda (G000)
                                (funcall '+ (- 1 2) G000)))
                   (macroexpand-all '(cut '+ (- 1 2) <>))))
    ))

(ert-deftest erfi-test:cute:does-evaluate ()
  (flet ((cl-gensym (&optional prefix) 'G000))
    (should (equal '(let ((G000 (- 1 2)))
                      (function (lambda (G000)
                                  (funcall '+ G000 G000))))
                   (macroexpand-all '(cute '+ (- 1 2) <>))))
    (should (equal '(function (lambda (G000) (list G000 '(a b c))))
                   (macroexpand '(cute list <> '(a b c)))))
    (should (equal '(let ((b "bar"))
                      (let ((G000 (cons 'a (cons b '(c)))))
                        (function (lambda (G000)
                                    (list G000 G000)))))
                   (macroexpand-all '(let1 b "bar" (cute list <> `(a ,b c))))))
    ))

;; Import from http://srfi.schemers.org/srfi-26/check.scm
(ert-deftest erfi-test:cut:import ()
  (should (equal (funcall (cut 'list)) '()))
  (should (equal (funcall (cut 'list <...>)) '()))
  (should (equal (funcall (cut 'list 1)) '(1)))
  (should (equal (funcall (cut 'list <>) 1) '(1)))
  (should (equal (funcall (cut 'list <...>) 1) '(1)))
  (should (equal (funcall (cut 'list 1 2)) '(1 2)))
  (should (equal (funcall (cut 'list 1 <>) 2) '(1 2)))
  (should (equal (funcall (cut 'list 1 <...>) 2) '(1 2)))
  (should (equal (funcall (cut 'list 1 <...>) 2 3 4) '(1 2 3 4)))
  (should (equal (funcall (cut 'list 1 <> 3 <>) 2 4) '(1 2 3 4)))
  (should (equal (funcall (cut 'list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)))
  (should (equal (let* ((x 'wrong) (y (cut list x))) (setq x 'ok) (funcall y)) '(ok)))
  (should (equal (let ((a 0))
                   (mapcar (cut + (progn (setq a (+ a 1)) a) <>)
                           '(1 2))
                   a)
                 2))
  )

;; Import from http://srfi.schemers.org/srfi-26/check.scm
(ert-deftest erfi-test:cute:import ()
  (should (equal (funcall (cute 'list)) '()))
  (should (equal (funcall (cute 'list <...>)) '()))
  (should (equal (funcall (cute 'list 1)) '(1)))
  (should (equal (funcall (cute 'list <>) 1) '(1)))
  (should (equal (funcall (cute 'list <...>) 1) '(1)))
  (should (equal (funcall (cute 'list 1 2)) '(1 2)))
  (should (equal (funcall (cute 'list 1 <>) 2) '(1 2)))
  (should (equal (funcall (cute 'list 1 <...>) 2) '(1 2)))
  (should (equal (funcall (cute 'list 1 <...>) 2 3 4) '(1 2 3 4)))
  (should (equal (funcall (cute 'list 1 <> 3 <>) 2 4) '(1 2 3 4)))
  (should (equal (funcall (cute 'list 1 <> 3 <...>) 2 4 5 6) '(1 2 3 4 5 6)))
  (should (equal (let1 lexical-binding t
                   (let ((a 0))
                     (mapcar (cute + (progn (setq a (+ a 1)) a) <>)
                             '(1 2))
                     a))
                 1))
  )



(ert-deftest erfi-test:case ()
  (should (equal 'zero
                 (erfi:case (car '(0 1 2))
                   ((0) 'zero)
                   ((1) 'one)
                   ((2) 'two))))
  (should (equal 'one
                 (erfi:case (cadr '(0 1 2))
                   ((0) 'zero)
                   ((1) 'one)
                   ((2) 'two))))
  (should (equal 'undefined
                 (erfi:case 3
                   ((0) 'zero)
                   ((1) 'one)
                   ((2) 'two))))
  (should (equal 'hoge
                 (erfi:case 'other
                   ((0) 'zero)
                   ((1) 'one)
                   (else 'hoge))))
  (should (equal 'hoge
                 (erfi:case 'other
                   ((0) 'zero)
                   ((1) 'one)
                   (t 'hoge))))
  (should-error (erfi:case 'other
                  ((0) 'zero)
                  (else 'one)
                  (else 'hoge)))
  (should-error (erfi:ecase 3
                  ((0) 'zero)
                  ((1) 'one)
                  ((2) 'two)))
  (should (eq 'composite
              (erfi:case (* 2 3)
                ((2 3 5 7) 'prime)
                ((1 4 6 8 9) 'composite))))
  (should (eq 'undefined
              (erfi:case (car '(c d))
                ((a) 'a)
                ((b) 'b))))
  (should (eq 'consonant
              (erfi:case (car '(c d))
                ((a e i o u) 'vowel)
                ((w y) 'semivowel)
                (else 'consonant))))
  (should (= 7
             (erfi:case 6
               ((2 4 6 8) => (erfi:cut '+ <> 1))
               (else => (erfi:cut '- <> 1)))))
  (should (= 4
             (erfi:case 5
               ((2 4 6 8) => (erfi:cut '+ <> 1))
               (else => (erfi:cut '- <> 1)))))
  )



(ert-deftest erfi-test:let:auxiliary-function ()
  (should (equal '((a 1) (b 2) (c 3))
                 (erfi:zip2 '(a b c) '(1 2 3 4 5))))
  (should (equal '(5 7 9)
                 (erfi:map2 '+ '(1 2 3) '(4 5 6 7 8))))
  (should (equal '((1 2) (3 4 5))
                 (erfi:split-at '(1 2 3 4 5) 2)))
  (should-error (erfi:split-at '(1 2 3) 5))
  (should-error (erfi:split-at '(1 2 3) -2))
  (should-error (erfi:split-at '(1 2 3) 1.5))
  (should (equal '(3 4 5)
                 (erfi:drop '(1 2 3 4 5) 2)))
  (should-error (erfi:drop '(1 2 3) 5))
  (should-error (erfi:drop '(1 2 3) -2))
  (should-error (erfi:drop '(1 2 3) 1.5))
  (should (equal '(1 2)
                 (erfi:take '(1 2 3 4 5) 2)))
  (should-error (erfi:take '(1 2 3) 5))
  (should-error (erfi:take '(1 2 3) -2))
  (should-error (erfi:take '(1 2 3) 1.5))
  (should (equal 5
                 (erfi:last '(1 2 3 4 5))))
  (should (eq 1
              (erfi:any1 'identity '(nil nil nil 1 nil))))
  (should (eq nil
              (erfi:any1 'integerp '(nil t nil a))))
  (should (eq nil
              (erfi:every1 'integerp '(nil t nil a))))
  (should (eq t
              (erfi:every1 'integerp '(1 2 3 4))))
  )



(ert-deftest erfi-test:list-receive ()
  (flet ((cl-gensym (&optional prefix) 'G000))
    (should (equal '(let ((all (divrem 13 4)))
                      all)
                   (macroexpand '(erfi%list-receive all (divrem 13 4)
                                   all))))
    (should (equal '(let* ((G000 (divrem 13 4))
                           (a (nth 0 G000))
                           (b (nth 1 G000)))
                      a)
                   (macroexpand '(erfi%list-receive (a b) (divrem 13 4)
                                   a))))
    (should (equal '(let* ((G000 (divrem 13 4))
                           (a (nth 0 G000))
                           (b (nth 1 G000))
                           (c (nthcdr 2 G000)))
                      a)
                   (macroexpand '(erfi%list-receive (a b . c) (divrem 13 4)
                                   a))))
    ))



(defvar dummy-func-alist
  `((naive-funcall . ,(lambda (expr) '(REPLACED)))
    (apply . ,(lambda (expr) '(REPLACED)))))



(ert-deftest erfi-test:let:code-walk ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(f)
                                         t dummy-func-alist))))
  (should (eq :not-appear
              (car (erfi:let:code-walk-1 'f
                                         '(g)
                                         t dummy-func-alist))))
  (should (eq :not-appear
              (car (erfi:let:code-walk-1 'f
                                         'a
                                         t dummy-func-alist))))
  (should (eq :only-tail-call
              (car (erfi:let:code-walk:aux 'f
                                           '((g)) '((f) a)
                                           t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk:aux 'f
                                           '((g)) '(f a)
                                           t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk:aux 'f
                                           '((f)) '((g) a)
                                           t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:if ()
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(if a f g)
                                         t dummy-func-alist))))
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(if a (f) (g))
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:cond ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(cond (a a) (t (f)))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(cond (a a) ((f) (f)))
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:and ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(and a (f))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(and (f) a)
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:let ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(let ((a 1)) a (f))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(let ((a (f))) (f) a)
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(let ((a (f))) a)
                                         t dummy-func-alist))))
  ;; the followings raise warning... test this!
  ;; If `let' hide let-name, `erfi:let' regard let-name is used even if it does not appear
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(let ((f 1) (a 2)) a)
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(let ((f 1) (a 2)) (f))
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:apply ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(if (zerop x) 0 (apply f 0 (cons 0 1)))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(if (zerop x) 0 (apply f (f 0 1) (cons 0 1)))
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:naive-funcall ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(f (g 1))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(g (f 1))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(f (f 1))
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:funcall ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(funcall f (g 1))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(funcall g (f 1))
                                         t dummy-func-alist))))
  (should (eq :not-appear
              (car (erfi:let:code-walk-1 'f
                                         '(funcall g 1)
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(funcall (if (< 0 x)
                                                       f
                                                       'identity)
                                                   (* -2 x))
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:apply-2 ()
  (should (eq :only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(apply f (g 1))
                                         t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f
                                         '(apply g (f 1))
                                         t dummy-func-alist))))
  (should (eq :not-appear
              (car (erfi:let:code-walk-1 'f
                                         '(apply 'f 1)
                                         t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:complex-example ()
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk 'fact
                                       '(if (zerop n) 1 (* n (fact (- n 1))))
                                       t dummy-func-alist))))
  (should (eq :only-tail-call
              (car (erfi:let:code-walk 'fact*
                                       '(if (zerop n) r (fact* (- n 1) (* r n)))
                                       t dummy-func-alist))))
  )
(ert-deftest erfi-test:let:code-walk:lambda ()
  (should (eq :not-appear
              (car (erfi:let:code-walk-1 'f '(lambda (x) x) t dummy-func-alist))))
  ;; the followings raise warning... test this!
  ;; If `lambda' hide let-name, `erfi:let' regard let-name is used even if it does not appear
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f '(lambda (x f) x) t dummy-func-alist))))
  (should (eq :not-only-tail-call
              (car (erfi:let:code-walk-1 'f '(lambda (x f) (f)) t dummy-func-alist))))
  )



(ert-deftest erfi-test:let:execution ()
  (should (eq 120
              (erfi:let fact ((n 5))
                (if (zerop n)
                    1
                    (* n (fact (- n 1)))))))
  (should (eq 120
              (erfi:let fact* ((n 5) (r 1))
                (if (zerop n)
                    r
                    (fact* (- n 1) (* r n))))))
  (should (equal '(just-a-silly-contrived-example hoge 5 hoge 4 hoge 3 1 2)
                 (erfi:let (blast (r '()) . (x 1 2 (+ 1 2) 4 5))
                   (if (>= 1 (length x))
                       (cons 'just-a-silly-contrived-example r)
                       (apply blast `(,(car x) ,(cadr x) ,@r) 'hoge (cddr x))))))
  (should (equal '(hoge 0 hoge 1 hoge 2)
                 (erfi:let lp ((lis '(0 1 2)))
                   (if (null lis)
                       '()
                       `(hoge ,(car lis) ,@(lp (cdr lis)))))))
  )



(ert-deftest erfi-test:$ ()
  (flet ((cl-gensym (&optional prefix) 'arg))
    (should (equal '(f a b c)
                   (macroexpand '(erfi:$ f a b c))))
    (should (equal '(function (lambda (arg) (f a b c arg)))
                   (macroexpand '(erfi:$ f a b c $))))
    (should (equal '(f (g a b c))
                   (macroexpand '(erfi:$ f $ g a b c))))
    (should (equal '(function (lambda (arg) (f (g a b c arg))))
                   (macroexpand '(erfi:$ f $ g a b c $))))
    (should (equal '(f (g (h a b c)))
                   (macroexpand '(erfi:$ f $ g $ h a b c))))
    (should (equal '(f a (g b (h c)))
                   (macroexpand '(erfi:$ f a $ g b $ h c))))
    (should (equal '(function (lambda (arg) (f a (g b (h c arg)))))
                   (macroexpand '(erfi:$ f a $ g b $ h c $))))
    (should (equal '(function (lambda (&rest arg) (apply f a b c arg)))
                   (macroexpand '(erfi:$ f a b c $*))))
    (should (equal '(apply f a b (g c d))
                   (macroexpand '(erfi:$ f a b $* g c d))))
    (should (equal '(function (lambda (arg) (apply f a b (g c d arg))))
                   (macroexpand '(erfi:$ f a b $* g c d $))))
    (should (equal '(function (lambda (&rest arg) (apply f a b (apply g c d arg))))
                   (macroexpand '(erfi:$ f a b $* g c d $*))))
    (should (equal '(function (lambda (&rest arg) (f a b (apply g c d arg))))
                   (macroexpand '(erfi:$ f a b $ g c d $*))))
    (should (equal '(funcall 'f (g (funcall 'h a b c)))
                   (macroexpand '(erfi:$ 'f $ g $ 'h a b c))))

    (should (equal '((ff))
                   (macroexpand '(erfi:$ $ ff))))
    (should (equal '(((fff)))
                   (macroexpand '(erfi:$ $ $ fff))))
    (should-error (macroexpand '(erfi:$)))
    ;; These may be useful in Scheme but not in Emacs Lisp since `funcall' is necessary.
    (should (equal '(function (lambda (arg) (arg)))
                   (macroexpand '(erfi:$ $))))
    (should (equal '(function (lambda (arg) ((arg))))
                   (macroexpand '(erfi:$ $ $))))

    ;; <> can designate the place of argument like `cut'.
    (should (equal '(f (g c) b)
                   (macroexpand '(erfi:$ f <> b $ g c))))
    (should (equal '(function (lambda (arg) (f a b (g arg d))))
                   (macroexpand '(erfi:$ f a b $ g <> d $))))
    (should (equal '(function (lambda (arg) (f (g arg d) b)))
                   (macroexpand '(erfi:$ f <> b $ g <> d $))))
    (should-error (macroexpand '(erfi:$ f a <> $ g c)))
    ))





(macroexpand '(erfi:dynamic-let ((a 0) (b 1)) body1 body2))
; => (let
;        ((G33404 a)
;         (G33405 b))
;      (unwind-protect
;          (progn
;            (setq a 0)
;            (setq b 1)
;            body1 body2)
;        (progn
;          (setq a G33404)
;          (setq b G33405))))

(macroexpand '(erfi:case (* 2 3)
                         ((2 3 5 7) 'prime)
                         ((1 4 6 8 9) 'composite)))
; => (let
;        ((G30228
;          (* 2 3)))
;      (cond
;       ((memq G30228 ...)
;        (progn ...))
;       ((memq G30228 ...)
;        (progn ...))
;       (t
;        (erfi:undefined))))
(macroexpand '(erfi:ecase (* 2 3)
                         ((2 3 5 7) 'prime)
                         ((1 4 6 8 9) 'composite)))
; => (let
;        ((G30229
;          (* 2 3)))
;      (cond
;       ((memq G30229 ...)
;        (progn ...))
;       ((memq G30229 ...)
;        (progn ...))
;       (t
;        (error "ERROR: ecase test fell through: got %s, expecting one of %s" G30229 ...))))



;;; erfi-macros-test.el ends here
