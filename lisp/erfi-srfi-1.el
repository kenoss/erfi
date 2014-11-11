;;; erfi-srfi-1.el --- SRFI-1 -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: extensions, lisp
;; Namespace: erfi:
;; URL: https://github.com/kenoss/erfi
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;; This library provides list utility functions in SRFI-1.

;; Currently values (`erfi:values') is emulated by returning list.
;; Currently all linear update functions (have suffix "!") are properly
;; destructive. However these may change in future.

;; This library is written almost without cl package. Runtime cl-lib.el
;; dependency is only due to `cl-acons' for alias `erfi:alist-cons'.
;; The `erfi:let' macro is not used if the implementation is not too
;; complicated.  This is because:  1. a little additional cost arises
;; comparing to raw while loop;  (This is not a problem usually but
;; SRFI-1 functions are fundamental and often occur in most-inner-loop.)
;; 2. `erfi-macros.el' can depend on them in the feture release.

;; The following functions are exported with prefix "erfi:".
;; (Exception: `caaar' ... are exported without prefix. They are defined
;; by `defsubst'.)
;;
;;   Constructors
;;
;;       xcons cons* make-list list-tabulate
;;       make-list
;;       list-copy circular-list iota
;;
;;   Predicates
;;
;;       proper-list? circular-list? dotted-list?
;;       not-pair? null-list?
;;       list=
;;
;;   Selectors
;;
;;       car cdr ... cddadr cddddr
;;       list-ref
;;       take       drop
;;       take-right drop-right
;;       take!      drop-right!
;;       split-at   split-at!
;;       last last-pair
;;
;;   Miscellaneous: length, append, concatenate, reverse, zip & count
;;
;;       length+
;;       append! concatenate concatenate! reverse!
;;       append-reverse append-reverse!
;;       zip unzip1 unzip2 unzip3 unzip4 unzip5
;;       count
;;
;;   Fold, unfold & map
;;
;;       map for-each
;;       fold       unfold       pair-fold       reduce
;;       fold-right unfold-right pair-fold-right reduce-right
;;       append-map append-map!
;;       map! pair-for-each filter-map map-in-order
;;
;;   Filtering & partitioning
;;
;;       filter  partition  remove
;;       filter! partition! remove!
;;
;;   Searching
;;
;;       member
;;       find find-tail
;;       any every
;;       list-index
;;       take-while drop-while take-while!
;;       span break span! break!
;;
;;   Deleting
;;
;;       delete  delete-duplicates
;;       delete! delete-duplicates!
;;
;;   Association lists
;;
;;       assoc
;;       alist-cons alist-copy
;;       alist-delete alist-delete!
;;
;;   Set operations on lists
;;
;;       lset<= lset= lset-adjoin
;;       lset-union             lset-union!
;;       lset-intersection      lset-intersection!
;;       lset-difference        lset-difference!
;;       lset-xor               lset-xor!
;;       lset-diff+intersection lset-diff+intersection!

;; The followings are extended by this library in backwards-compatible ways:
;;
;;     make-list (The second argument filling may omited.)
;;     member assoc (Extended to take an optional comparison procedure.)

;; The following functions are already provided by default Emacs Lisp:
;;
;;     cons list
;;     first second third fourth fifth sixth seventh eighth ninth tenth
;;     length
;;     append  reverse
;;     memq
;;     assq

;; The following functions are not provided:
;;
;;     pair? null?
;;     car+cdr
;;     memv
;;     assv
;;     set-car! set-cdr!


;; Due to the lack of author's experience of Emacs Lisp, there may exist
;; useless functions providing same features already implimented in
;; Emacs Lisp.  I think we should not have exactly same functions,
;; except that there exist performance issue, or it was done by defalias
;; for the purpose of coherence of appearence of codes. So please inform
;; me if you found it.

;; For more details, see SRFI documentation.


;;; Code:



(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions))
  (require 'cl))

(require 'cl-lib) ; only for cl-acons

(require 'erfi-macros)
(eval-when-compile
  (erfi:use-short-macro-name))



;;;
;;; Constructors
;;;

(defun erfi:xcons (d a)
  (cons a d))

(defalias 'erfi:cons* 'list*)

(defun erfi:make-list (n &optional fill)
  (progn
    (when (< n 0)
      (error "erfi:make-list: negative length given: %s\n" n))
    (rlet1 rs '()
      (while (< 0 n)
        (push fill rs)
        (decf n)))))

(defun erfi:list-tabulate (n init-proc)
  (progn
    (when (< n 0)
      (error "erfi:make-list: negative length given: %s\n" n))
    (setq n (1- n))
    (rlet1 rs '()
      (while (<= 0 n)
        (push (funcall init-proc n) rs)
        (decf n)))))

(defalias 'erfi:list-copy 'copy-sequence)

(defun erfi:circular-list (x &rest xs)
;  `#1=(,@xs . #1#))
  (let1 ys (cons x xs)
    (rlet1 head ys
      (while (not (null (cdr ys)))
        (pop ys))
      (setcdr ys head))))

(defun erfi:iota (count &optional start step)
  (let ((x (or start 0))
        (d (or step 1)))
    (let1 rs '()
      (while (< 0 count)
        (push x rs)
        (incf x d)
        (decf count))
      (nreverse rs))))



;;;
;;; Predicates
;;;

; pair? ...

(defmacro erfi:circular-list?:aux (f x)
  `(if (not (consp ,x))
       nil
       (let ((sub-list (list ,x))
             (y (cdr ,x)))
         (while (and (consp y)
                     (not (erfi:any1 (cut 'eq y <>) sub-list)))
           (push y sub-list)
           (pop y))
         ,(funcall f 'y))))
(defun erfi:proper-list? (x)
  (erfi:circular-list?:aux (lambda (y) `(null ,y)) x))
(defun erfi:circular-list? (x)
  (erfi:circular-list?:aux (lambda (y) `(consp ,y)) x))
(defun erfi:dotted-list? (x)
  (erfi:circular-list?:aux (lambda (y) `(not (or (null ,y) (consp ,y)))) x))

(defalias 'erfi:not-pair? 'atom)

(defun erfi:null-list? (x)
  "[SRFI-1] Return t if X is nil, nil if X is a proper list.
Raise an error if X is a non-nil atom."
  ;; cdr for non-nil atom raise an error and (cdr nil) is nil.
  (and (atom x)
       (null (cdr x))))

;; (defun erfi:list= (elt= &rest xss)
;;   (cond ((>= 1 (length xss))
;;          t)
;;         ((let1 len (length (car xss))
;;            (not (erfi:every1 (lambda (xs) (eq len (length xs))) (cdr xss))))
;;          nil)
;;         (t
;;          (erfi:let outer-iter ((xss xss))
;;            (if (null (car xss))
;;                t
;;                (erfi:let inner-iter ((ys (mapcar 'car xss)))
;;                  (if (null (cdr ys))
;;                      (outer-iter (mapcar 'cdr xss))
;;                      (and (funcall elt= (car ys) (cadr ys))
;;                           (inner-iter (cdr ys))))))))))
(defun erfi:list= (elt= &rest xss)
  (if (let1 len (length (car xss))
        (not (erfi:every1 (lambda (xs) (eq len (length xs))) (cdr xss))))
      nil
      (erfi:let lp1 ((xss xss))
        (if (null (cdr xss))
            t
            (erfi:let lp2 ((xs (car xss))
                           (ys (cadr xss)))
              (if (null xs)
                  (lp1 (cdr xss))
                  (and (funcall elt= (car xs) (car ys))
                       (lp2 (cdr xs) (cdr ys)))))))))



;;;
;;; Selectors
;;;

;(when (not (fboundp 'caaar))
(when t
  (defsubst caaar (x) (car (car (car x))))
  (defsubst caadr (x) (car (car (cdr x))))
  (defsubst cadar (x) (car (cdr (car x))))
  (defsubst caddr (x) (car (cdr (cdr x))))
  (defsubst cdaar (x) (cdr (car (car x))))
  (defsubst cdadr (x) (cdr (car (cdr x))))
  (defsubst cddar (x) (cdr (cdr (car x))))
  (defsubst cdddr (x) (cdr (cdr (cdr x))))
  (defsubst caaaar (x) (car (car (car (car x)))))
  (defsubst caaadr (x) (car (car (car (cdr x)))))
  (defsubst caadar (x) (car (car (cdr (car x)))))
  (defsubst caaddr (x) (car (car (cdr (cdr x)))))
  (defsubst cadaar (x) (car (cdr (car (car x)))))
  (defsubst cadadr (x) (car (cdr (car (cdr x)))))
  (defsubst caddar (x) (car (cdr (cdr (car x)))))
  (defsubst cadddr (x) (car (cdr (cdr (cdr x)))))
  (defsubst cdaaar (x) (cdr (car (car (car x)))))
  (defsubst cdaadr (x) (cdr (car (car (cdr x)))))
  (defsubst cdadar (x) (cdr (car (cdr (car x)))))
  (defsubst cdaddr (x) (cdr (car (cdr (cdr x)))))
  (defsubst cddaar (x) (cdr (cdr (car (car x)))))
  (defsubst cddadr (x) (cdr (cdr (car (cdr x)))))
  (defsubst cdddar (x) (cdr (cdr (cdr (car x)))))
  (defsubst cddddr (x) (cdr (cdr (cdr (cdr x)))))
  )

(defun erfi:list-ref (clist n)
  (if (< n 0)
      (error "argument out of range: %s" n)
      (nth n clist)))

;; `take' `drop' `take-right' `drop-right' `split-at' are defined in erfi-macros.el .

(defmacro erfi:split-at!:aux (name x i f)
  `(cond ((< ,i 0)
          (error "%s: argument out of range: %s\n" ,name ,i))
         ((zerop ,i)
          '())
         (t
          (let1 y ,x
            (while (and (consp y)
                        (not (= ,i 1)))
              (pop y)
              (decf ,i))
            (if (= ,i 1)
                ,(funcall f x 'y)
                (error ,(concat "%s: input list is too short (expected at least %s elements, "
                                "but only %s elements long): %s\n")
                       ,name ,i (length ,x) ,x))))))
(defun erfi:split-at! (x i)
  (erfi:split-at!:aux 'erfi:split-at! x i
                      (lambda (x y)
                        ;; `(rlet1 r (list ,x (cdr ,y))
                        ;;    (setcdr ,y '())))))
                        `(let1 z (cdr ,y) ; bug: z may conflict x or y.
                           (setcdr ,y '())
                           (erfi:values ,x z)))))
(defun erfi:take! (x i)
  (erfi:split-at!:aux 'erfi:take! x i
                      (lambda (x y)
                        `(progn
                           (setcdr ,y '())
                           ,x))))
(defun erfi:drop-right! (flist i)
  (let1 j (- (length flist) i)
    (cond ((or (< j 0) (< i 0))
           (error "argument out of range: %s\n" i))
          ((zerop j)
           '())
          (t
           (let1 y flist
             (while (not (= j 1))
               (pop y)
               (decf j))
             (setcdr y '())
             flist)))))



;;;
;;; Others
;;;

(defun erfi:length+ (clist)
  (if (erfi:circular-list? clist)
      nil
      (length clist)))

(defalias 'erfi:append! 'nconc)
(put 'erfi:append! 'function-documentation
     "[SRFI-1+] Append lists destructively and return the result list.

Each but the last argument must be a proper list;  The last one may be any value
at all.  It is guaranteed nil in the REST are ignored and do not raise errors.

It is guaranteed to alter cons cells in the argument lists except the last one.
The last argument is never altered; the result list shares structure with this
parameter.")


(defsubst erfi:concatenate (xss)
  (apply 'append xss))
(defsubst erfi:concatenate! (xss)
  (apply 'nconc xss))

(defalias 'erfi:reverse! 'nreverse)

(defun erfi:append-reverse (rev-head tail)
  "[SRFI-1]"
  (progn
    (while (not (null rev-head))
      (push (car rev-head) tail)
      (pop rev-head))
    tail))
(defalias 'erfi:append-reverse! 'erfi:append-reverse)

(defun erfi:zip (xs &rest xss)
  "[SRFI-1]"
  (erfi:let lp ((xss (cons xs xss)) (rss '()))
    (if (erfi:any1 'erfi:null-list? xss)
        (nreverse rss)
        (lp (mapcar 'cdr xss) (cons (mapcar 'car xss) rss)))))

(defun erfi:unzip1 (lis)
  "[SRFI-1]"
  (mapcar 'car lis))
(defun erfi:unzip2 (lis)
  "[SRFI-1]"
  (let ((res1 '())
        (res2 '()))
    (while (not (null lis))
      (push (nth 0 (car lis)) res1)
      (push (nth 1 (car lis)) res2)
      (pop lis))
    (erfi:values (nreverse res1) (nreverse res2))))
(defun erfi:unzip3 (lis)
  "[SRFI-1]"
  (let ((res1 '())
        (res2 '())
        (res3 '()))
    (while (not (null lis))
      (push (nth 0 (car lis)) res1)
      (push (nth 1 (car lis)) res2)
      (push (nth 2 (car lis)) res3)
      (pop lis))
    (erfi:values (nreverse res1) (nreverse res2) (nreverse res3))))
(defun erfi:unzip4 (lis)
  "[SRFI-1]"
  (let ((res1 '())
        (res2 '())
        (res3 '())
        (res4 '()))
    (while (not (null lis))
      (push (nth 0 (car lis)) res1)
      (push (nth 1 (car lis)) res2)
      (push (nth 2 (car lis)) res3)
      (push (nth 3 (car lis)) res4)
      (pop lis))
    (erfi:values (nreverse res1) (nreverse res2) (nreverse res3) (nreverse res4))))
(defun erfi:unzip5 (lis)
  "[SRFI-1]"
  (let ((res1 '())
        (res2 '())
        (res3 '())
        (res4 '())
        (res5 '()))
    (while (not (null lis))
      (push (nth 0 (car lis)) res1)
      (push (nth 1 (car lis)) res2)
      (push (nth 2 (car lis)) res3)
      (push (nth 3 (car lis)) res4)
      (push (nth 4 (car lis)) res5)
      (pop lis))
    (erfi:values (nreverse res1) (nreverse res2) (nreverse res3) (nreverse res4) (nreverse res5))))

(defun erfi:count (pred xs &rest yss)
  "[SRFI-1]
\n(fn pred clist1 ...)"
  (if (null yss)
      (rlet1 n 0
        (while (not (null xs))
          (when (funcall pred (car xs))
            (incf n))
          (pop xs)))
      (erfi:let lp ((n 0) (xss (cons xs yss)))
        (if (erfi:any1 'erfi:null-list? xss)
            n
            (lp (if (apply pred (mapcar 'car xss))
                      (1+ n)
                      n)
                  (mapcar 'cdr xss))))))


;;;
;;; Fold, unfold & map
;;;

(defun erfi:fold (kons knil xs &rest xss)
  "[R5RS+]
\n(fn kons kinl clist1 ...)
"
  (let ((xss (cons xs xss))
        (acc knil))
    (while (not (erfi:any1 'null xss))
      (setq acc (apply kons (append (mapcar 'car xss) (list acc))))
      (setq xss (mapcar 'cdr xss)))
    acc))
(defun erfi:foldl (kons knil xs)
  "[Haskell] foldl :: (a -> b -> a) -> a -> [b] -> a
"
  (let1 acc knil
    (while (not (null xs))
      (setq acc (funcall kons acc (car xs)))
      (setq xs (cdr xs)))
    acc))
(defun erfi:fold-right (kons knil xs &rest xss)
  "[R5RS+]
\n(fn kons kinl clist1 ...)
"
  (apply 'erfi:fold kons knil (reverse xs) (mapcar 'reverse xss)))
(defun erfi:pair-fold (kons knil xs &rest xss)
  "[SRFI-1]
\n(fn kons kinl clist1 ...)
"
  (let ((xss (cons xs xss))
        (yss nil)
        (acc knil))
    (while (not (erfi:any1 'null xss))
      (setq yss (mapcar 'cdr xss))
      (setq acc (apply kons (append xss (list acc))))
      (setq xss yss))
    acc))
  ;; (erfi:let lp ((xss (cons xs xss)) (acc knil))
  ;;   (if (erfi:any1 'null xss)
  ;;       acc
  ;;       (let1 yss (mapcar 'cdr xss)
  ;;         (lp yss (apply kons (append xss (list acc))))))))
(defun erfi:pair-fold-right (kons knil xs &rest xss)
  "[SRFI-1]
\n(fn kons kinl clist1 ...)
"
  (erfi:pair-fold-right:aux kons knil (cons xs xss)))
(defun erfi:pair-fold-right:aux (kons knil xss)
  (if (erfi:any1 'null xss)
      knil
      (apply kons (append xss (list (erfi:pair-fold-right:aux kons knil (mapcar 'cdr xss)))))))
(defun erfi:reduce (f ridentity lis)
  (if (null lis)
      ridentity
      (erfi:fold f (car lis) (cdr lis))))
(defun erfi:reduce-right (f ridentity lis)
  (if (null lis)
      ridentity
      (erfi:fold f (car lis) (reverse (cdr lis)))))
(defun erfi:unfold (pred-stop f next-seed seed &optional tail-gen)
  (cdr (rlet1 dummy-head (list nil)
         (let1 tail-cons dummy-head
           (while (not (funcall pred-stop seed))
             (setcdr tail-cons (cons (funcall f seed) nil))
             (pop tail-cons)
             (setq seed (funcall next-seed seed)))
           (when tail-gen
             (setcdr tail-cons (funcall tail-gen seed)))))))
;; (defmacro erfi:unfold%macro (pred-continue-gen f next-seed seed tail-code-gen)
;;   `(cdr (rlet1 dummy-head (list nil)
;;           (let1 res dummy-head
;;             (while ,(pred-continue-gen seed)
;;               (setcdr res 
(defun erfi:unfold-right (pred-stop f next-seed seed &optional tail)
  (progn
    (while (not (funcall pred-stop seed))
      (push (funcall f seed) tail)
      (setq seed (funcall next-seed seed)))
    tail))

(defun erfi:map (proc xs &rest xss)
  "[RnRS+]"
  (if (null xss)
      ;; Fast pass.
      (mapcar proc xs)
      ;; General case.
      (erfi:let lp ((xss (cons xs xss)) (rs '()))
        (if (erfi:any1 'erfi:null-list? xss)
            (nreverse rs)
            (lp (mapcar 'cdr xss) (cons (apply proc (mapcar 'car xss)) rs))))))
(defalias 'erfi:map! 'erfi:map)
(defun erfi:map-in-order (proc xs &rest xss)
  "[SRFI-1]"
  (if (null xss)
      ;; Fast pass.
      (erfi:let lp ((xs xs) (rs '()))
        (if (erfi:null-list? xs)
            (nreverse rs)
            (lp (cdr xs) (cons (funcall proc (car xs)) rs))))
      ;; General case.
      (erfi:let lp ((xss (cons xs xss)) (rs '()))
        (if (erfi:any1 'erfi:null-list? xss)
            (nreverse rs)
            (lp (mapcar 'cdr xss) (cons (apply proc (mapcar 'car xss)) rs))))))
(defun erfi:for-each (proc xs &rest xss)
  "[RnRS+]"
  (if (null xss)
      ;; Fast pass.
      (mapc proc xs)
      ;; General case.
      (erfi:let lp ((xss (cons xs xss)))
        (if (erfi:any1 'erfi:null-list? xss)
            nil
            (progn
              (apply proc (mapcar 'car xss))
              (lp (mapcar 'cdr xss)))))))
(defun erfi:append-map (proc xs &rest xss)
  "[SRFI-1]"
  (erfi:concatenate (apply 'erfi:map proc xs xss)))
(defun erfi:append-map! (proc xs &rest xss)
  "[SRFI-1]"
  (erfi:concatenate! (apply 'erfi:map proc xs xss)))
;; (pair-for-each (lambda (pair) (display pair) (newline)) '(a b c)) ==>
;;     (a b c)
;;     (b c)
;;     (c)
(defun erfi:filter-map (proc xs &rest xss)
  "[SRFI-1]"
  (if (null xss)
      ;; Fast pass.
      (erfi:let lp ((xs xs) (rs '()))
        (if (erfi:null-list? xs)
            (nreverse rs)
            (lp (cdr xs) (if-let1 value (funcall proc (car xs))
                           (cons value rs)
                           rs))))
      ;; General case.
      (erfi:let lp ((xss (cons xs xss)) (rs '()))
        (if (erfi:any1 'erfi:null-list? xss)
            (nreverse rs)
            (lp (cdr xs) (if-let1 value (apply proc (mapcar 'car xss))
                           (cons value rs)
                           rs))))))



;;;
;;; Filtering & partitioning
;;;

;; (defmacro erfi:filter:aux (xs pred-exp)
;;   `(let1 res '()
;;      (while (not (null ,xs))
;;        (when ,pred-exp
;;          (push (car ,xs) res))
;;        (pop ,xs))
;;      (nreverse res)))
;; (defun erfi:filter (pred xs)
;;   (erfi:filter:aux xs (funcall pred (car xs))))
;; (defun erfi:remove (pred xs)
;;   (erfi:filter:aux xs (not (funcall pred (car xs)))))

; (defun erfi:filter (pred lis)
;;   ; The returned list does not share a common tail with the argument list
;;   (cdr (rlet1 dummy-head (list nil)
;;          (let1 tail-cons dummy-head
;;            (while (consp lis)
;;              (when (funcall pred (car lis))
;;                (setcdr tail-cons (cons (car lis) nil))
;;                (pop tail-cons))
;;              (pop lis))))))
(defun erfi:filter (pred lis)
  ; The returned list share a common tail with the argument list if possible
  (cdr (rlet1 dummy-head (list nil)
         (let1 tail-cons dummy-head
           (let ((xs lis)
                 (ys lis))
             (while (consp ys)
               (when (not (funcall pred (car ys)))
                 (while (not (eq xs ys))
                   (setcdr tail-cons (cons (car xs) nil))
                   (pop xs)
                   (pop tail-cons))
                 (pop xs))
               (pop ys)
               (setcdr tail-cons xs)))))))
(defun erfi:filter! (pred lis)
  (rlet1 head (erfi:find-tail pred lis)
    (when (not (null head))
      (let1 tail-cons head
        (while (not (null (cdr tail-cons)))
          (when (not (funcall pred (cadr tail-cons)))
            (setcdr tail-cons (cddr tail-cons)))
          (pop tail-cons))))))

(defun erfi:remove (pred xs)
  (erfi:filter (lambda (x) (not (funcall pred x))) xs))
(defun erfi:remove! (pred xs)
  (erfi:filter! (lambda (x) (not (funcall pred x))) xs))

;; (defun erfi:partition! (pred xs)
;;   (let ((sat-head)
;;         (nsat-head)
;;         (sat-tail)
;;         (nsat-tail)
(defun erfi:partition (pred xs)
  (let ((sat '())
        (nsat '()))
    (while (not (null xs))
      (if (funcall pred (car xs))
          (push (car xs) sat)
          (push (car xs) nsat))
      (pop xs))
    (erfi:values (nreverse sat) (nreverse nsat))))
(defalias 'erfi:partition! 'erfi:partition)



;;;
;;; Searching
;;;

(defun erfi:find (pred clist)
  (let1 res (erfi:find-tail pred clist)
    (if res
        (car res)
        nil)))
(defun erfi:find-tail (pred xs)
  "[SRFI-1]
\n(fn pred clist)
"
  (if (funcall pred (car xs))
      xs
      (progn
        (while (and (not (null (cdr xs)))
                    (not (funcall pred (cadr xs))))
          (pop xs))
        (cdr xs))))

(defun erfi:any (pred xs &rest xss)
  "[SRFI-1]
\n(fn pred clist1 ...)
"
  (if (null xss)
      ;; Fast pass.
      (erfi:any1 pred xs)
      ;; General case.
      (let ((xss (cons xs xss))
            (res nil))
        (while (and (not res)
                    (not (erfi:any1 'erfi:null-list? xss)))
          (setq res (apply pred (mapcar 'car xss)))
          (setq xss (mapcar 'cdr xss)))
        res)))
(defun erfi:every (pred xs &rest xss)
  "[SRFI-1]
\n(fn pred clist1 ...)
"
  (if (null xss)
      ;; Fast pass.
      (erfi:every1 pred xs)
      ;; General case.
      (let ((xss (cons xs xss))
            (res t))
        (while (and res
                    (not (erfi:any1 'erfi:null-list? xss)))
          (setq res (apply pred (mapcar 'car xss)))
          (setq xss (mapcar 'cdr xss)))
        res)))
(defun erfi:list-index (pred xs &rest xss)
  (if (null xs)
      ;; Fast pass.
      (let ((i 0))
        (while (and (not (null xs))
                    (not (funcall pred (car xs))))
          (incf i)
          (pop xs))
        (if (null xs)
            nil
            i))
      ;; General case.
      (let ((xss (cons xs xss))
            (i 0))
        (while (and (not (erfi:any1 'erfi:null-list? xss))
                    (not (apply pred (mapcar 'car xss))))
          (incf i)
          (setq xss (mapcar 'cdr xss)))
        (if (erfi:any1 'erfi:null-list? xss)
            nil
            i))))

(defun erfi:take-while (pred xs)
  "[SRFI-1]"
  (let1 rs '()
    (while (and (not (null xs))
                (funcall pred (car xs)))
      (push (car xs) rs)
      (pop xs))
    (nreverse rs)))
(defun erfi:take-while! (pred xs)
  "[SRFI-1]"
  (if (not (funcall pred (car xs)))
      '()
      (prog1 xs
        (let1 tail-cons xs
          (while (and (not (null (cdr tail-cons)))
                      (funcall pred (cadr tail-cons)))
            (pop tail-cons))
          (setcdr tail-cons '())))))
(defun erfi:drop-while (pred xs)
  "[SRFI-1]"
  (while (and (not (null xs))
              (funcall pred (car xs)))
    (pop xs))
  xs)
(defun erfi:span! (pred xs)
  "[SRFI-1]"
  (if (not (funcall pred (car xs)))
      (erfi:values '() xs)
      (let1 tail-cons xs
        (while (and (not (null (cdr tail-cons)))
                    (funcall pred (cadr tail-cons)))
          (pop tail-cons))
        (let1 rest (cdr tail-cons)
          (setcdr tail-cons '())
          (erfi:values xs rest)))))
(defun erfi:break! (pred xs)
  "[SRFI-1]"
  (if (funcall pred (car xs))
      (erfi:values '() xs)
      (let1 tail-cons xs
        (while (and (not (null (cdr tail-cons)))
                    (not (funcall pred (cadr tail-cons))))
          (pop tail-cons))
        (let1 rest (cdr tail-cons)
          (setcdr tail-cons '())
          (erfi:values xs rest)))))

(defun erfi:member (x ys &optional elt=)
  (if (null elt=)
      (member x ys)
      (progn
        (while (and (not (null ys))
                    (not (funcall elt= x (car ys))))
          (pop ys))
        ys)))



;;;
;;; Deletion
;;;

(defun erfi:delete (x ys &optional elt=)
  "[SRFI-1] Delete elements non-destructively."
  (let1 elt= (or elt= 'equal)
    (let1 res '()
      (while (not (null ys))
        (when (not (funcall elt= x (car ys)))
          (push (car ys) res))
        (pop ys))
      (nreverse res))))
(defun erfi:delete! (x ys &optional elt=)
  (if (null elt=)
      (delete x ys)
      (progn
        (while (and (not (null ys))
                    (funcall elt= x (car ys)))
          (pop ys))
        (if (null ys)
            nil
            (rlet1 head ys
              (let1 tail-cons head
                (while (not (null (cdr tail-cons)))
                  (when (funcall elt= x (cadr tail-cons))
                    (setcdr tail-cons (cddr tail-cons)))
                  (pop tail-cons))))))))

(defun erfi:delete-duplicates (xs &optional elt=)
  (let1 elt= (or elt= 'equal)
    (if (or (null xs) (null (cdr xs)))
        xs
        (erfi:let lp ((xs xs) (res '()))
          (if (null xs)
              (nreverse res)
              (lp (erfi:delete (car xs) (cdr xs) elt=) (cons (car xs) res)))))))
(defun erfi:delete-duplicates! (xs &optional elt=)
  (let1 elt= (if elt= (lambda (x y) (funcall elt= y x)) 'equal)
    (if (null xs)
        nil
        (rlet1 head xs
          (erfi:let lp ((xs xs) (dict (list (car xs))))
            (cond ((null xs)
                   nil)
                  ((erfi:member (cadr xs) dict elt=)
                   (setcdr xs (cddr xs))
                   (lp xs dict))
                  (t
                   (lp (cdr xs) (cons (cadr xs) dict)))))))))



;;;
;;; Association lists
;;;

(defun erfi:assoc (key alist key=)
  (let1 key= (or key= 'equal)
    (while (and (not (null alist))
                (not (funcall key= key (caar alist))))
      (pop alist))
    (if (null alist)
        nil
        (car alist))))

(defalias 'erfi:alist-cons 'cl-acons)

(defun erfi:alist-copy (alist)
  (let1 res '()
    (while (not (null alist))
      (push (cons (caar alist) (cdar alist)) res)
      (pop alist))
    (nreverse res)))

(defun erfi:alist-delete (key alist &optional key=)
  "Return a copy of ALIST (as list) if KEY does not appear."
  (let ((key= (or key= 'equal))
        (res '()))
    (while (not (null alist))
      (when (not (funcall key= key (caar alist)))
        (push (car alist) res))
      (pop alist))
    (nreverse res)))
(defun erfi:alist-delete! (key alist &optional key=)
  (let1 key= (or key= 'equal)
    (while (and (not (null alist))
                (funcall key= key (caar alist)))
      (pop alist))
    (rlet1 head alist
      (while (not (null (cdr alist)))
        (if (funcall key= key (caadr alist))
            (setcdr alist (cddr alist))
            (pop alist))))))

(defun erfi:alist-update (key value alist &optional key=)
  (acons key value
         (erfi:alist-delete key alist key=)))
(defun erfi:alist-update! (key value alist &optional key=)
  (let1 pair (erfi:find (let1 key= (or key= 'equal)
                          (lambda (x) (funcall key= key (car x))))
                        alist)
    (if pair
        (progn
          (setcdr pair value)
          alist)
        (acons key value alist))))



;;;
;;; Set operations on lists
;;;

(defun erfi:lset<= (elt= &rest xss)
  (if (null xss)
      t
      (erfi:let lp ((xss xss))
        (if (null (cdr xss))
            t
            (and (let1 ys (cadr xss)
                   (erfi:every1 (cut 'erfi:member <> ys elt=) (car xss)))
                 (lp (cdr xss)))))))
(defun erfi:lset= (elt= &rest xss)
  (and (apply 'erfi:lset<= elt= xss)
       (apply 'erfi:lset<= elt= (nreverse xss))))

(defun erfi:lset-adjoin (elt= xs &rest ys)
  "[SRFI-1]
\n(fn elt= set elt1 ...)"
  (erfi:lset-union elt= xs ys))

(defun erfi:lset-union (elt= &rest xss)
  "[SRFI-1]
\n(fn elt= set1 ...)"
;;   (erfi:fold (cut 'erfi:lset-union:aux elt= <> <>)
;;              '() xss))
;; (defun erfi:lset-union:aux (elt= xs ys)
;;   (
  (if (null xss)
      '()
      (erfi:let lp1 ((xss (cdr xss)) (res (car xss)))
        (if (null xss)
            res
            (erfi:let lp2 ((xs (car xss)) (res res))
              (if (null xs)
                  (lp1 (cdr xss) res)
                  (let1 x (car xs)
                    (if (erfi:any (lambda (r) (funcall elt= r x)) res)
                        (lp2 (cdr xs) res)
                        (lp2 (cdr xs) (cons x res))))))))))

(defun erfi:lset-intersection (elt= xs &rest yss)
  "[SRFI-1]
\n(fn elt= list1 ...)"
  (let1 yss (erfi:delete xs yss 'eq)
    (cond ((erfi:any 'erfi:null-list? yss)
           '())
          ((null yss)
           xs)
          (t
           (erfi:filter (lambda (x)
                          (erfi:every (lambda (ys) (erfi:member x ys)) yss))
                        xs)))))

(defun erfi:lset-difference (elt= xs &rest yss)
  "[SRFI-1]
\n(fn elt= list1 ...)"
  (let1 yss (erfi:delete xs yss 'eq)
    (cond ((erfi:any 'erfi:null-list? yss)
           '())
          ((null yss)
           xs)
          (t
           (erfi:filter (lambda (x)
                          (erfi:every (lambda (ys) (not (erfi:member x ys))) yss))
                        xs)))))

(defun erfi:lset-xor (elt= &rest xss)
  "[SRFI-1]
\n(fn elt= list1 ...)"
  (if (null xss)
      '()
      (erfi:foldl (cut 'erfi%lset-xor-2 elt= <> <>) (car xss) (cdr xss))))
(defun erfi%lset-xor-2 (elt= xs ys)
  (let* ((r (erfi:lset-diff+intersection elt= xs ys))
         (xs-minus-ys (car r))
         (xs-int-ys (cadr r)))
    (cond ((null xs-minus-ys)
           (erfi:lset-difference elt= ys xs))
          ((null xs-int-ys)
           (append ys xs))
          (t
           (erfi:let lp ((ys ys) (res xs-minus-ys))
             (cond ((null ys)
                    res)
                   ((erfi:member (car ys) xs-int-ys elt=)
                    (lp (cdr ys) res))
                   (t
                    (lp (cdr ys) (cons (car ys) res)))))))))

(defun erfi:lset-diff+intersection (elt= xs &rest yss)
  "[SRFI-1]
\n(fn elt= list1 ...)"
  (let1 yss (erfi:delete xs yss 'eq)
    (cond ((erfi:any 'erfi:null-list? yss)
           '())
          ((null yss)
           xs)
          (t
           (erfi:partition (lambda (x)
                             (erfi:every (lambda (ys) (not (erfi:member x ys elt=))) yss))
                           xs)))))



(provide 'erfi-srfi-1)
;;; erfi-srfi-1.el ends here
