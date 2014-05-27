;;; erfi-macros.el --- Scheme macros, in particular, named let with tail recursion optimization -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: extensions, lisp
;; URL: https://github.com/kenoss/erfi
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;; Usage:
;;   (eval-when-compile
;;     (require 'erfi-macros)
;;     (erfi:use-short-macro-name))
;; Then the following macros are available:
;;   `let1' `if-let1' `rlet1' `and-let*'
;;   `cut' `cute'
;;   `erfi:cond'
;;   `erfi:case' `erfi:ecase'
;;   `erfi:let'
;; The main feture of this package is `erfi:let', an implementation of SRFI-5,
;; which let one to use named let with tail recursion optimization
;; (not tail CALL optimization).  At the compile time this expand TRO to usual
;; efficient `while' loop, as it have been done by hand.  Note that there is
;; several restrictions because it is not a true language extension.
;; It is recommended using with `lexical-binding' t.  When you face a problem,
;; try to use `macroexpand' or throw a bug repport.
;;
;; For more details, see ../test/erfi-macros-test.el .

;;; Code:



(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions)))
(require 'cl-lib)

(defgroup erfi
  nil
  "Group for ERFI"
  :group 'lisp)
(defgroup erfi-macros
  nil
  "Group for ERFI macros"
  :group 'erfi)



;; `let1', `if-let1', `rlet1'
(defmacro erfi:let1 (var expr &rest body)
  (declare (indent 2))
  "[Gauche] Equivalent to (let ((VAR EXPR)) BODY ...)"
  `(let ((,var ,expr))
     ,@body))
(defmacro erfi:if-let1 (var expr then &rest else)
  (declare (indent 2))
  "[Gauche] Equivalent to (let ((VAR EXPR)) (if VAR THEN ELSE))"
  `(let ((,var ,expr))
     (if ,var ,then ,@else)))
(defmacro erfi:rlet1 (var expr &rest body)
  (declare (indent 2))
  "[Gauche] Equivalent to (let ((VAR EXPR)) BODY ... VAR)"
  `(let ((,var ,expr))
     ,@body
     ,var))

(defmacro erfi:and-let* (bindings &rest body)
  (declare (indent 1))
  "[SRFI-2]"
  (erfi:and-let*:aux bindings body))
(defun erfi:and-let*:aux (bindings body)
  `(let ,(list (car bindings))
     (when ,(caar bindings)
       ,@(let ((bs (cdr bindings)))
           (if (null bs)
               body
               (list (erfi:and-let*:aux bs body)))))))

;; [SRFI-26] `cut', `cute'
(defmacro erfi:cut (&rest spec)
  "[SRFI-26]"
  (let* ((partial-args '())
         (dotted-flag nil)
         (args (mapcar (lambda (x)
                         (cond ((eq x '<>)
                                (let ((y (cl-gensym)))
                                  (push y partial-args)
                                  y))
                               ((eq x '<...>)
                                (let ((y (cl-gensym)))
                                  (push '&rest partial-args)
                                  (push y partial-args)
                                  (setq dotted-flag t)
                                  y))
                               (t
                                x)))
                       spec)))
    (if dotted-flag
        `(lambda ,(nreverse partial-args) (apply ,@args))
        `(lambda ,(nreverse partial-args) (funcall ,@args)))))
(defmacro erfi:cute (&rest spec)
  "[SRFI-26]"
  (let* ((partial-args '())
         (dotted-flag nil)
         (bindings '())
         (args (mapcar (lambda (x)
                         (cond ((eq x '<>)
                                (let ((y (cl-gensym)))
                                  (push y partial-args)
                                  y))
                               ((eq x '<...>)
                                (let ((y (cl-gensym)))
                                  (push '&rest partial-args)
                                  (push y partial-args)
                                  (setq dotted-flag t)
                                  y))
                               (t
                                (let ((y (cl-gensym)))
                                  (push `(,y ,x) bindings)
                                  y))))
                       spec)))
    (if dotted-flag
        `(let ,bindings (lambda ,(nreverse partial-args) (apply ,@args)))
        `(let ,bindings (lambda ,(nreverse partial-args) (funcall ,@args))))))

;; [R5RS][SRFI-61] `cond'
(defmacro erfi:cond (&rest clauses)
  "[R5RS][SRFI-61]"
  (if (null clauses)
      (error "erfi: syntax-error: at least one clause is required for cond: %s" '(erfi:cond))
      (erfi:cond:aux clauses)))
(defun erfi:cond:aux (clauses)
  (let ((c (car-safe clauses))
        (v (cl-gensym)))
    (cond ((null clauses)
           '(erfi:undefined))
          ((eq '=> (cadr c))
           `(let ((,v ,(car c)))
              (if ,v
                  (,(caddr c) ,v)
                  ,(erfi:cond:aux (cdr clauses)))))
          ((eq '=> (caddr c))
           `(let ((,v ,(car c)))
              (if (,(cadr c) ,v)
                  (,(cadddr c) ,v)
                  ,(erfi:cond:aux (cdr clauses)))))
          ((eq 'else (car c))
           (if (not (null (cdr clauses)))
               (error "erfi: syntax-error: 'else' clause followed by more clauses: %s"
                      `(erfi:cond ,@clauses))
               `(progn ,@(cdr c))))
          (t
           `(if ,(car c)
                (progn ,@(cdr c))
                ,(erfi:cond:aux (cdr clauses)))))))

;; [R5RS][SRFI-87] `case', `ecase'
(defsubst erfi:case:block (var c)
  (if (eq '=> (cadr c))
      `(funcall ,(caddr c) ,var)
      `(progn ,@(cdr c))))
(defun erfi:case:aux (var key clauses case-or-ecase)
  (let ((cs '())
        (kss '()))
    (while (not (null (cdr clauses)))
      (push `((memq ,var ',(caar clauses)) ,(erfi:case:block var (car clauses)))
            cs)
      (push (caar clauses) kss)
      (pop clauses))
    (if (or (eq 't (caar clauses))
            (eq 'else (caar clauses)))
        (push `(t ,(erfi:case:block var (car clauses)))
              cs)
        (progn
          (push `((memq ,var ',(caar clauses)) ,(erfi:case:block var (car clauses)))
                 cs)
          (push (caar clauses) kss)))
     (push `(t ,(if (eq 'case case-or-ecase)
                    '(erfi:undefined)
                    `(error "ERROR: ecase test fell through: got %s, expecting one of %s"
                            ,var ,(apply 'append (nreverse kss)))))
            cs)
    `(let ((,var ,key))
       (cond ,@(nreverse cs)))))
(defmacro erfi:case (key &rest clauses)
  (declare (indent 1))
  "[R5RS][SRFI-87]"
  (if (null clauses)
      (error "erfi: syntax-error: at least one clause is required for case: %s" '(erfi:case))
      (let ((var (cl-gensym)))
        (erfi:case:aux var key clauses 'case))))
(defmacro erfi:ecase (key &rest clauses)
  (declare (indent 1))
  "[Gauche]"
  (if (null clauses)
      (error "erfi: syntax-error: at least one clause is required for case: %s" '(erfi:ecase))
      (let ((var (cl-gensym)))
        (erfi:case:aux var key clauses 'ecase))))



;;; named let
;; auxiliary functions for `erfi:let'
(defun erfi:zip2 (xs ys)
  (let ((res '()))
    (while (and (not (null xs))
                (not (null ys)))
      (push (list (pop xs) (pop ys)) res))
    (nreverse res)))
(defun erfi:any1 (pred xs)
  (let ((res nil))
    (while (and (not res)
                (not (null xs)))
      (setq res (funcall pred (pop xs))))
    res))
(defun erfi:every1 (pred xs)
  (let ((res t))
    (while (and res
                (not (null xs)))
      (setq res (funcall pred (pop xs))))
    res))

(defun erfi:map2 (proc xs ys)
  (let ((res '()))
    (while (and (not (null xs))
                (not (null ys)))
      (push (funcall proc (pop xs) (pop ys))
            res))
    (nreverse res)))

(defun erfi:split-at (xs i)
  "[SRFI-1] Rerturn a list (list (erfi:take xs i) (erfi:drop xs i)) ."
  (let ((rs '()))
    (while (not (zerop i))
      (push (pop xs) rs)
      (decf i))
    (list (nreverse rs) xs)))
(defun erfi:take (xs i)
  "[SRFI-1]"
  (let ((rs '()))
    (while (not (zerop i))
      (push (pop xs) rs)
      (decf i))
    (nreverse rs)))
(defun erfi:drop (xs i)
  "[SRFI-1]"
  (while (not (zerop i))
    (pop xs)
    (decf i))
  xs)
(defun erfi:take-right (xs i)
  "[SRFI-1]"
  (erfi:drop xs (- (length xs) i)))
(defun erfi:drop-right (xs i)
  "[SRFI-1]"
  (erfi:take xs (- (length xs) i)))

(defun erfi:last (xs)
  "[SRFI-1]"
  (while (not (null (cdr xs)))
    (pop xs))
  (car xs))


;; [R5RS][SRFI-5] (named) let, with tail recursion optimization
(defmacro erfi:let (&rest args)
  (declare (indent 2))
  "[R5RS][SRFI-5] `let' and named let with tail recursion optimization.

  (erfi:let ((VAR1 VALUE1) ...) body ...)
  (erfi:let LET-NAME ((VAR1 VALUE1) ...) BODY ...)
  (erfi:let LET-NAME ((VAR1 VALUE1) ... . (REST-VAR REST-VALUE)) BODY ...)
  (erfi:let (LET-NAME (VAR1 VALUE1) ...) BODY ...)
  (erfi:let (LET-NAME (VAR1 VALUE1) ... . (REST-VAR REST-VALUE)) BODY ...)

The first form is the same to normal `let'.

In the other forms, if all appearance of LET-NAME in body are function calls and
are in tail context, those calls are optimized using `while' loop.  If not,
those will be expanded with `letrec'.
"
  (cond ((not (listp (car args)))
         (erfi:let:parse-bindings (car args) (cadr args) (cddr args)))
        ((not (listp (caar args)))
         (erfi:let:parse-bindings (caar args) (cdar args) (cdr args)))
        (t
         (erfi:let:parse-bindings nil (car args) (cdr args)))))
(defun erfi:let:parse-bindings (name bindings body)
  (let ((bindings* '()))
    (while (and (consp bindings)
                (consp (car bindings))
                (not (eq '&rest (car bindings))))
      (push (pop bindings) bindings*))
    (cond ((eq '&rest (car bindings))
           (if (null name)
               (erfi:let:without-name (nreverse bindings*) (cadr bindings) body)
               (erfi:let:with-name name (nreverse bindings*) (cadr bindings) body)))
          ((not (null bindings))
           (if (null name)
               (erfi:let:without-name (nreverse bindings*) bindings body)
               (erfi:let:with-name name (nreverse bindings*) bindings body)))
          (t
           (if (null name)
               (erfi:let:without-name (nreverse bindings*) nil body)
               (erfi:let:with-name name (nreverse bindings*) nil body))))))
(defun erfi:let:parse-spec (args)
  "Return (let-name bindings rest-bind body)."
  (cond ((not (listp (car args)))
         (erfi:let:parse-spec:aux (car args) (cadr args) (cddr args)))
        ((not (listp (caar args)))
         (erfi:let:parse-spec:aux (caar args) (cdar args) (cdr args)))
        (t
         (erfi:let:parse-spec:aux nil (car args) (cdr args)))))
(defun erfi:let:parse-spec:aux (name bindings body)
  (let ((bindings* '()))
    (while (and (consp bindings)
                (consp (car bindings))
                (not (eq '&rest (car bindings))))
      (push (pop bindings) bindings*))
    (cond ((eq '&rest (car bindings))
           (list name (nreverse bindings*) (cadr bindings) body))
          ((not (null bindings))
           (list name (nreverse bindings*) bindings body))
          (t
           (list name (nreverse bindings*) nil body)))))
(defun erfi:let:build (&rest args)
  (if (null (car args))
      (apply 'erfi:let:without-name (cdr args))
      (apply 'erfi:let:with-name args)))
(defun erfi:let:without-name (bindings rest-binding body)
  `(let ,(if (not rest-binding)
             `(,@bindings)
             `(,@bindings (,(car rest-binding) (list ,@(cdr rest-binding)))))
     ,@body))
(defun erfi:let:with-name (name bindings rest-binding body)
  (let* ((repeat (make-symbol "--erfi-repeat--"))
         (continue (make-symbol "--erfi-continue--"))
         (result (make-symbol "--erfi-result--"))
         (vars (mapcar 'car bindings))
         (rest-var (car-safe rest-binding))
         (init-args (mapcar 'cadr bindings))
         (rest-init-arg (cdr-safe rest-binding))
         (new-vars (mapcar 'cl-gensym bindings)) ;necessary for (erfi:let f ((n 5) (r 1)) (f (- n 1) (* r a)))
         (new-rest-var (cl-gensym))
         (func-alist (erfi:let:with-name:make-func-alist name repeat vars rest-var new-vars new-rest-var)))
    (let* ((res (erfi:let:code-walk name body t func-alist))
           (judge (car res))
           (new-body (cadr res)))
      (erfi:ecase judge
        ((:not-appear)
         (erfi:let:without-name bindings rest-binding body))
        ((:not-only-tail-call)
         `(letrec ((,name (lambda (,@vars ,@(if rest-var `(&rest ,rest-var) `()))
                            (let ((,continue t)
                                  (,result nil)
                                  ,@(erfi:zip2 new-vars vars) ; temporary
;                                  ,new-vars
                                  ,@(mapcar (lambda (x) `(,x nil)) vars)
                                  ,@(if (not rest-var)
                                        nil
                                        `((,rest-var nil)
                                          (,new-rest-var nil))))
                              (while ,continue
                                ,@(erfi:map2 (lambda (x y) `(setq ,x ,y)) vars new-vars)
                                ,@(if (not rest-var)
                                      nil
                                      `((setq ,rest-var ,new-rest-var)))
                                (catch ',repeat
                                  ,@(erfi:drop-right new-body 1)
                                  (setq ,result ,(erfi:last new-body))
                                  (setq ,continue nil)))
                              ,result))))
            (funcall ,name ,@init-args ,@(if rest-var `(,rest-init-arg) nil))))
        ((:only-tail-call)
         `(let ((,continue t)
                (,result nil)
                ,@(erfi:zip2 new-vars init-args)
                ,@(mapcar (lambda (x) `(,x nil)) vars)
                ,@(if (not rest-var)
                      nil
                      `((,rest-var nil)
                        (,new-rest-var (list ,@rest-init-arg)))))
            (while ,continue
              ,@(erfi:map2 (lambda (x y) `(setq ,x ,y)) vars new-vars)
              ,@(if (not rest-var)
                    nil
                    `((setq ,rest-var ,new-rest-var)))
              (catch ',repeat
                ,@(erfi:drop-right new-body 1)
                (setq ,result ,(erfi:last new-body))
                (setq ,continue nil)))
            ,result))))))
(defun erfi:let:with-name:make-func-alist (name repeat vars rest-var new-vars new-rest-var)
  `((naive-funcall .
     ,(lambda (expr)
        (let* ((vs new-vars)
               (args expr)
               (ws args)
               (res '()))
          (while (not (null vs))
            (when (null ws)
              (error "erfi:let: compile-error: wrong number of arguments for %s (required %s, got %s)"
                     name (length vars) (length args)))
            (push `(setq ,(pop vs) ,(pop ws))
                  res))
          (when rest-var
            (push `(setq ,new-rest-var (list ,@ws))
                  res))
          (push `(throw ',repeat nil)
                res)
          `(progn ,@(nreverse res)))))
    (apply .
     ,(lambda (args)
        (let* ((vs new-vars)
               (ws args)
               (res '()))
          (while (and (not (null vs)) (not (null (cdr ws))))
            (push `(setq ,(pop vs) ,(pop ws))
                   res))
          (push `(let ((--erfi-var-- (list* ,@ws)))
                   ,@(mapcar (lambda (v) `(setq ,v (pop --erfi-var--))) vs)
                   ;; TODO: write raising an error if things are wrong
                   ,@(if (not rest-var)
                         nil
                         `((setq ,new-rest-var --erfi-var--)))
                   ;; Should raise an error if wrong number of arguments?
                   )
                res)
          (push `(throw ',repeat nil)
                 res)
          `(progn ,@(nreverse res)))))
    ))

(defun erfi:let:code-walk (name body tail-context-flag func-alist)
  "[internal] Return a list (only-tail-call-flag new-body) .  The value only-tail-call-flag
is one of :not-appear, :only-tail-call, :not-only-tail-call .  The value new-body
is rewritten body with TRO."
  (let* ((init&last (erfi:split-at body (- (length body) 1)))
         (res (erfi:let:code-walk:aux name (car init&last) (cadr init&last)
                                      tail-context-flag func-alist)))
    `(,(car res) (,@(cadr res) ,@(caddr res)))))
(defun erfi:let:code-walk:aux
  (name body-list-not-in-tail-context body-list-in-tail-context tail-context-flag func-alist)
  "[internal] Return a list
 (only-tail-call-flag new-body-list-not-in-tail-context new-body-list-in-tail-context)"
  (let* ((res-pairs-n (mapcar (lambda (b) (erfi:let:code-walk-1 name b nil func-alist))
                              body-list-not-in-tail-context))
         (res-pairs-t (mapcar (lambda (b) (erfi:let:code-walk-1 name b tail-context-flag func-alist))
                              body-list-in-tail-context))
         (result-cdr (list (mapcar 'cadr res-pairs-n) (mapcar 'cadr res-pairs-t))))
    (cond ((or (not (erfi:every1 (lambda (x) (eq :not-appear (car x)))
                                 res-pairs-n))
               (erfi:any1 (lambda (x) (eq :not-only-tail-call (car x)))
                          res-pairs-t))
           `(:not-only-tail-call . ,result-cdr))
          ((erfi:every1 (lambda (x) (eq :not-appear (car x)))
                        res-pairs-t)
           `(:not-appear . ,result-cdr))
          (t ;; <=> (erfi:every1 (lambda (x) (eq :only-tail-call (car x))) res-pairs-t)
           `(:only-tail-call . ,result-cdr)))))
(defun erfi:let:code-walk-1 (name body tail-context-flag func-alist)
  "[internal] Return a list (only-tail-call-flag new-body) .  The value only-tail-call-flag
is one of :not-appear, :only-tail-call, :not-only-tail-call .  The value new-body
is rewritten body with TRO.

Second argument BODY must be one of the following form:
...
"
  (if (atom body)
      `(,(if (eq name body)
             :not-only-tail-call
             :not-appear)
        ,body)
      (let ((b (if (memq (car body) '(lambda erfi:let)) ; more?
                   body
                   (macroexpand body))))
        (erfi:let:code-walk-1:aux name b tail-context-flag func-alist))))
(defun erfi:let:code-walk-1:aux (name b tail-context-flag func-alist)
  (erfi:case (car b)
    ((if)
     (let ((res (erfi:let:code-walk:aux name (list (cadr b)) (cddr b) tail-context-flag func-alist)))
       `(,(car res) (if ,@(cadr res) ,@(caddr res)))))
    ((cond)
     (let* ((res-list (mapcar (lambda (clause) (erfi:let:code-walk name clause tail-context-flag func-alist))
                              (cdr b)))
            (judge-for-tail-call (cond ((erfi:every1 (lambda (x) (eq :not-appear (car x))) res-list)
                                        :not-appear)
                                       ((erfi:any1 (lambda (x) (eq :not-only-tail-call (car x))) res-list)
                                        :not-only-tail-call)
                                       (t
                                        :only-tail-call))))
       `(,judge-for-tail-call (cond ,@(mapcar 'cadr res-list)))))
    ((lambda)
     (let* ((let-name-hidden (erfi:any1 (lambda (x) (eq name x)) (cadr b)))
            (res (if let-name-hidden
                     `(:not-only-tail-call ,(caddr b))
                     (erfi:let:code-walk name (cddr b) nil func-alist))))
       (when let-name-hidden
         (lwarn 'erfi-macros :warning "`%s' should not hide variable %s used for `erfi:let'" (car b) name))
       `(,(car res) (lambda ,(cadr b) ,@(cadr res)))))
    ((and or progn)
     (let ((res (erfi:let:code-walk name (cdr b) tail-context-flag func-alist)))
       `(,(car res) (,(car b) ,@(cadr res)))))
    ;; Should raise an error for conflict of tag with that of `erfi:let'?
    ((catch)
     (let ((res (erfi:let:code-walk name (cddr b) tail-context-flag func-alist)))
       `(,(car res) (,(car b) ,(cadr b) ,@(cadr res)))))
    ((let let*)
     (let* ((let-name-hidden (erfi:any1 (lambda (x) (eq name (car x))) (cadr b)))
            (res (if let-name-hidden
                     `(:not-only-tail-call ,(caddr b))
                     (erfi:let:code-walk name (cddr b) tail-context-flag func-alist)))
            (bindings-res (mapcar (lambda (x) (erfi:let:code-walk-1 name (cadr x) nil func-alist))
                                  (cadr b)))
            (judge (cond ((or (eq :not-only-tail-call (car res))
                              (erfi:any1 (lambda (x) (not (eq :not-appear (car x))))
                                         bindings-res))
                          :not-only-tail-call)
                         (t
                          (car res)))))
       (when let-name-hidden
         (lwarn 'erfi-macros :warning "`%s' should not hide variable %s used for `erfi:let'" (car b) name))
       `(,judge (,(car b) ,(erfi:zip2 (mapcar 'car (cadr b))
                                      (mapcar 'cadr bindings-res))
                 ,@(cadr res)))))
    ((erfi:let)
     (cl-destructuring-bind (c-let-name c-bindings c-rest-bind c-body) (erfi:let:parse-spec (cdr b))
       (let* ((let-name-hidden (erfi:any1 (lambda (x) (eq name (car x))) (cons c-rest-bind c-bindings)))
              (body-res (if let-name-hidden
                            `(:not-only-tail-call ,c-body)
                            (erfi:let:code-walk name c-body tail-context-flag func-alist)))
              (bindings-res (mapcar (lambda (x) (erfi:let:code-walk-1 name (cadr x) nil func-alist))
                                    c-bindings))
              (rest-bind-res (if (null c-rest-bind)
                                 `(:not-appear '())
                                 (erfi:let:code-walk name (cadr c-rest-bind) nil func-alist)))
              (judge (cond ((or (eq :not-only-tail-call (car body-res))
                                (not (eq :not-appear (car rest-bind-res)))
                                (erfi:any1 (lambda (x) (not (eq :not-appear (car x))))
                                           bindings-res))
                            :not-only-tail-call)
                           (t
                            (car body-res)))))
         (when let-name-hidden
           (lwarn 'erfi-macros :warning "`%s' should not hide variable %s used for `erfi:let'" (car b) name))
         `(,judge ,(erfi:let:build c-let-name
                                   (erfi:zip2 (mapcar 'car c-bindings) (mapcar 'cadr bindings-res))
                                   (cons (car c-rest-bind) (cadr rest-bind-res))
                                   (cadr body-res))))))
    ((funcall)
     (if (eq name (cadr b))
         (erfi:let:code-walk-1:aux name (cdr b) tail-context-flag func-alist)
         (let* ((res (erfi:let:code-walk:aux name (cddr b) nil nil func-alist))
                (judge (if (eq :not-appear (car res))
                           :not-appear
                           :not-only-tail-call)))
           `(,judge (funcall ,(cadr b) ,@(cadr res))))))
    ((apply)
     (let* ((res (erfi:let:code-walk:aux name (cddr b) nil nil func-alist))
            (judge (if (eq :not-appear (car res))
                       (if (eq name (cadr b))
                           :only-tail-call
                           :not-appear)
                       :not-only-tail-call)))
       (cond ((not (eq name (cadr b)))
              `(,judge (apply ,(cadr b) ,@(cadr res))))
             ((eq :not-only-tail-call judge)
              `(,judge (apply ,name ,@(cadr res))))
             (t
              `(,judge ,(funcall (cdr (assq 'apply func-alist)) (cadr res)))))))
    (else
     (cond ((eq name (car b))
            (let* ((res (erfi:let:code-walk:aux name (cdr b) nil nil func-alist))
                   (judge (if (and tail-context-flag (eq :not-appear (car res)))
                              :only-tail-call
                              :not-only-tail-call)))
              (if (eq :not-only-tail-call judge)
                  `(,judge (funcall ,(car b) ,@(cadr res)))
                  `(,judge ,(funcall (cdr (assq 'naive-funcall func-alist)) (cadr res))))))
           (t
            (let* ((res (erfi:let:code-walk:aux name (cdr b) nil nil func-alist)))
              `(,(car res) (,(car b) ,@(cadr res)))))))))
;;; END OF named let



(defun erfi:use-short-macro-name ()
  (mapc (lambda (x)
          (defalias x (intern (concat "erfi:" (symbol-name x)))))
        '(let1 if-let1 rlet1 and-let* cut cute)))



(provide 'erfi-macros)
;;; erfi-macros.el ends here
