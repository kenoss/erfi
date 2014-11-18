;;; erfi-test.el ---

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>

;; Apache License, Version 2.0

;;; Commentary:

;; 

;;; Code:



;; (setq max-lisp-eval-depth (+ max-lisp-eval-depth 1000))
;; (setq max-specpdl-size (+ max-specpdl-size 1000))
;; (setq eval-expression-print-length nil)
;; (setq eval-expression-print-level nil)


(require 'ert)

(require 'erfi-macros)
(require 'erfi-srfi-1)



;;; SRFI-1

(ert-deftest erfi-test:srfi-1:constructors ()
  (should (equal '(a a a a a)
                 (erfi:make-list 5 'a)))
  (should-error (erfi:make-list -1 'a))
  (should (equal '(0 1 2 3 4)
                 (erfi:list-tabulate 5 'identity)))
  (should-error (erfi:list-tabulate -1 'identity))
  (should (equal '(a b c a b)
                 (erfi:take (erfi:circular-list 'a 'b 'c) 5)))
  (should (equal '(0 1 2 3 4)
                 (erfi:iota 5)))
  ;; This test does not hold due to errors.
  ;; (should (equal '(0 -0.1 -0.2 -0.3 -0.4)
  ;;                (erfi:iota 5 0 -0.1)))
  )

(ert-deftest erfi-test:srfi-1:predicates ()
  (should (eq t
              (erfi:proper-list? '(0 1 2))))
  (should (eq nil
              (erfi:proper-list? '(0 1 . 2))))
  (should (eq nil
              (erfi:proper-list? (erfi:circular-list 0 1 2))))

  (should (eq nil
              (erfi:circular-list? '(0 1 2))))
  (should (eq nil
              (erfi:circular-list? '(0 1 . 2))))
  (should (eq t
              (erfi:circular-list? (erfi:circular-list 0 1 2))))
  (should (eq t
              (erfi:circular-list? (list* -2 -1 (erfi:circular-list 0 1 2)))))

  (should (eq nil
              (erfi:dotted-list? '(0 1 2))))
  (should (eq t
              (erfi:dotted-list? '(0 1 . 2))))
  (should (eq nil
              (erfi:dotted-list? (erfi:circular-list 0 1 2))))

  (should (eq t
              (erfi:null-list? nil)))
  (should (eq nil
              (erfi:null-list? '(a))))
  (should (eq nil
              (erfi:null-list? '(a b))))
  (should-error (erfi:null-list? 'a))

  (should (eq t
              (erfi:list= 'eq '(a b c) '(a b c))))
  (should (eq t
              (erfi:list= 'eq '(a b c) '(a b c) '(a b c))))
  (should (eq nil
              (erfi:list= 'eq '(a b c) '(0 1 2))))
  (should (eq nil
              (erfi:list= 'eq '(a b c) '(a b c) '(0 1 2))))
  (should (eq t
              (erfi:list= 'equal '(a (b) c) '(a (b) c) '(a (b) c))))
  (should (eq nil
              (erfi:list= 'equal '(a (b) c) '(a (b) c) '(a b c))))
  )

(ert-deftest erfi-test:srfi-1:selectors ()
  (should (equal '(0 1)
                 (erfi:take '(0 1 2 3 4) 2)))
  (should (equal '(0 1)
                 (erfi:take! (list 0 1 2 3 4) 2)))
  (should (equal '(0 1)
                 (rlet1 x (list 0 1 2 3 4)
                   (erfi:take! x 2))))
  (should-error (erfi:take! (list 0 1 2 3 4) -1))

  (should (equal '(0 1 2)
                 (erfi:drop-right '(0 1 2 3 4) 2)))
  (should (equal '(0 1 2)
                 (erfi:drop-right! (list 0 1 2 3 4) 2)))
  (should (equal '(0 1 2)
                 (rlet1 x (list 0 1 2 3 4)
                   (erfi:drop-right! x 2))))

  (should (equal '((0 1) (2 3 4))
                 (erfi:split-at! (list 0 1 2 3 4) 2)))
  (should (equal '(0 1)
                 (rlet1 x (list 0 1 2 3 4)
                   (erfi:split-at! x 2))))

  (should (equal 4
                 (erfi:last '(0 1 2 3 4))))
  (should (equal 3
                 (erfi:last '(0 1 2 3 . 4))))
  (should (equal '(4)
                 (erfi:last-pair '(0 1 2 3 4))))
  (should (equal '(3 . 4)
                 (erfi:last-pair '(0 1 2 3 . 4))))
  )

(ert-deftest erfi-test:srfi-1:searching ()
  (should (equal '(0 1 2 3 4)
                 (erfi:find-tail 'evenp '(0 1 2 3 4))))
  (should (equal '(1 2 3 4)
                 (erfi:find-tail 'oddp '(0 1 2 3 4))))
  (should (equal '()
                 (erfi:find-tail 'stringp '(0 1 2 3 4))))
  )

(ert-deftest erfi-test:srfi-1:association-lists ()
  (should (equal '((b . 1) (c . 3))
                 (erfi:alist-delete 'a '((a . 0) (b . 1) (a . 2) (c . 3)))))
  (should (equal '((b . 1) (c . 3))
                 (let1 lis (list '(a . 0) '(b . 1) '(a . 2) '(c . 3))
                   (erfi:alist-delete! 'a lis))))
  (should (equal '((a . 0) (b . 1) (c . 3))
                 (let1 lis (list '(a . 0) '(b . 1) '(a . 2) '(c . 3))
                   (erfi:alist-delete! 'a lis)
                   lis)))
  (should (equal '((a . 10) (b . 1) (c . 3))
                 (erfi:alist-update 'a 10 '((a . 0) (b . 1) (a . 2) (c . 3)))))
  (should (equal '((a . 10) (b . 1) (a . 2) (c . 3))
                 (let1 lis (list '(a . 0) '(b . 1) '(a . 2) '(c . 3))
                   (erfi:alist-update! 'a 10 lis))))
  )

(ert-deftest erfi-test:srfi-1:filtering&partitioning ()
  (should (equal '(1 3 5 7 9)
                 (erfi:filter 'oddp (erfi:iota 10))))
  (should (equal '()
                 (erfi:filter 'stringp (erfi:iota 10))))
  )













(ert-deftest erfi:srfi-1:others-test ()
  (let1 lis '(0 1 2 3 4)
    (should (= (length lis)
               (erfi:length+ lis))))
  (should (eq nil
              (erfi:length+ (erfi:circular-list 0 1 2 3 4))))

  (let1 lis '(0 1 2 3 4)
    (should (equal lis
                   (apply 'erfi:append! (erfi:zip lis)))))
  (should (equal '(0 1 2 3 4)
                 (erfi:append! '() (list 0 1) '() (list 2 3 4) '())))
  (should (equal '(0 1 2 3 4 . tail)
                 (erfi:append! '() (list 0 1) '() (list 2 3 4) 'tail)))
  (should (equal '()
                 (erfi:append!)))
  (should (equal 'tail
                 (erfi:append! 'tail)))

  (let1 n 10000
    (should (equal (erfi:iota n)
                   (erfi:concatenate (mapcar 'list (erfi:iota n)))))
    (should (equal (erfi:iota n)
                   (erfi:concatenate! (mapcar 'list (erfi:iota n))))))

  (should (equal '(2 1 0 3 4 5)
                 (append-reverse '(0 1 2) '(3 4 5))))

  (should (equal '(0 111 422 933)
                 (erfi:map '+ '(0 1 2 3 4) '(0 10 20 30 40) '(0 100 400 900))))

  (should (equal '((one 1 odd) (two 2 even) (three 3 odd))
                 (erfi:zip '(one two three)
                           '(1 2 3)
                           '(odd even odd even odd even odd even))))
  (should (equal '((1) (2) (3))
                 (erfi:zip '(1 2 3))))
  (should (equal '((3 nil) (1 t) (4 nil) (1 t))
                 (erfi:zip '(3 1 4 1) (erfi:circular-list nil t))))

  (should (= 3
             (erfi:count 'evenp '(0 1 2 3 4))))
  (should (= 3
             (erfi:count '< '(1 2 4 8) '(2 4 6 8 10 12 14 16))))
  (should (= 0
             (erfi:count '< '(1 2 4 8) (erfi:circular-list 0 1 2 3 4))))
  (should (= 3
             (let1 grater-than (lambda (&rest xs)
                                 (erfi:let iter ((xs xs))
                                   (if (null (cdr xs))
                                       t
                                       (and (< (car xs) (cadr xs))
                                            (iter (cdr xs))))))
               (erfi:count grater-than
                           (erfi:circular-list 0 1 2 3 4)
                           '(0 1 4 9 16 25)
                           '(3 1 4 15 92 65)))))
  )



(ert-deftest erfi:srfi-1:misc-test ()
  (should (eq t
              (erfi:any 'integerp '(a 3 b 2.7))))
  (should (eq nil
              (erfi:any 'integerp '(a 3.1 b 2.7))))
  (should (eq t
              (erfi:any '<
                        '(3 1 4 1 5)
                        '(2 7 1 8 2))))
  (should (eq nil
              (erfi:any '<
                        '(3 9 4 9 5)
                        '(2 7 1 8 2))))
  (should (eq nil
              (erfi:any '<
                        '(3 9 4)
                        '(2 7 1 8 2))))

  (should (eq t
              (erfi:every 'integerp '(1 3 7))))
  (should (eq nil
              (erfi:any 'integerp '(a 3.1 b 2.7))))
  (should (eq nil
              (erfi:every '<
                          '(3 1 4 1 5)
                          '(2 7 1 8 2))))
  (should (eq t
              (erfi:every '<
                          '(1 4 0 5 1)
                          '(2 7 1 8 2))))
  (should (eq t
              (erfi:every '<
                          '(1 4 0)
                          '(2 7 1 8 2))))

  (should (= 2
             (erfi:list-index 'evenp '(3 1 4 1 5 9))))
  (should (eq nil
              (erfi:list-index 'evenp '(3 1 1 5 9))))
  (should (= 1
             (erfi:list-index '< '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))
  (should (eq nil
              (erfi:list-index '= '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))))
  )








;; (erfi:lset<= 'eq '(a '(a b a) '(a b c c))
;; (erfi:lset<= 'eq '(c) '(a b a) '(a b c c))
;; (erfi:lset<= 'eq)
;; (erfi:lset<= 'eq '(a))

;; (erfi:lset= 'eq '(b e a) '(a e b) '(e e b a))
;; (erfi:lset= 'eq '(b e a) '(a e b) '(e e b c))
;; (erfi:lset= 'eq '(b e a c) '(a e b) '(e e b a))
;; (erfi:lset= 'eq)
;; (erfi:lset= 'eq '(a))

;; (erfi:lset-adjoin 'eq '(a b c d c e) 'a 'e 'i 'o 'u)


; (erfi:lset-intersection 'eq '(a b c d e) '(a e i o u))
; (erfi:lset-difference 'eq '(a b c d e) '(a e i o u))
; (erfi:lset-xor 'eq '(a b c d e) '(a e i o u))
; (erfi:lset-diff+intersection 'eq '(a b c d e) '(a e i o u))




;;; erfi-test.el ends here
