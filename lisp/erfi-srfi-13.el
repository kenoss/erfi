;;; erfi-srfi-13.el --- SRFI-13 -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: extensions, lisp
;; Namespace: erfi:
;; URL: https://github.com/kenoss/erfi
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;;; Code:


(eval-when-compile
  (require 'cl))

(eval-when-compile
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'erfi-srfi-1)



;;;
;;; Auxiliary function
;;;

(defun erfi:intersperse (item xs)
  "[Haskell] Return a newely allocated list.

Example:

  (erfi:intersperse 'a '(0 1 2))  => (0 a 1 a 2)
\n(fn item list)"
  (if (null xs)
      '()
      (erfi:let lp ((xs (cdr xs)) (res (list (car xs))))
        (if (null xs)
            (nreverse res)
            (lp (cdr xs) `(,(car xs) ,item ,@res))))))



;;;
;;; SRFI-13
;;;

(defsubst erfi:string-null? (str)
  "[SRFI-13]"
  (string= "" str))

(defun erfi:string->list (str &optional start end)
  (append (substring str (or start 0) end) nil))

(defsubst erfi:list->string (char-list)
  (apply 'string char-list))

(defalias 'erfi:string-append 'concat)
(defsubst erfi:string-concatenate (string-list)
  (apply 'concat string-list))


(defun erfi:string-join (strs &optional delim grammer)
  "[SRFI-13] Join STRS with DELIM under GRAMMER.

GRAMMER := 'infix | 'strict-infix | 'suffix | 'prefix
DELIM defaults to \" \".
GRAMMER defaults to 'infix.

Examples:

  (erfi:string-join '(\"foo\" \"bar\" \"baz\") \":\")         => \"foo:bar:baz\"
  (erfi:string-join '(\"foo\" \"bar\" \"baz\") \":\" 'suffix) => \"foo:bar:baz:\"

  ;; Infix grammar is ambiguous wrt empty list vs. empty string,
  (erfi:string-join '()   \":\") => \"\"
  (erfi:string-join '(\"\") \":\") => \"\"

  ;; but suffix & prefix grammars are not.
  (erfi:string-join '()   \":\" 'suffix) => \"\"
  (erfi:string-join '(\"\") \":\" 'suffix) => \":\"

For more details, see SRFI document."
  (let ((delim (or delim " "))
        (grammer (or grammer 'infix)))
    (let1 lis (erfi:intersperse delim strs)
      (if (null lis)
          (if (eq 'strict-infix grammer)
              (lwarn 'erfi :error
                     "`erfi:string-join': can't join empty list of strings with strict-infix grammer.")
              "")
          (erfi:string-concatenate (erfi:case grammer
                                     ((prefix) (cons delim lis))
                                     ;; Note that `erfi:intersperse' returns newly allocated list.
                                     ((suffix) (erfi:append! lis (list delim)))
                                     (else     lis)))))))


(defun erfi:xsubstring (str from &optional to start end)
  "[SRFI-13] Enhanced `substring'.

STR is string and the other arguments are integers.

  (erfi:xsubstring str from to start end)
is equivalent to
  (erfi:xsubstring (substring str start end) from to)
except for the case FROM = TO, which returns null string.

START defaults to 0.
END defaults to length of STR.
TO defaults to (+ from (- end start)) .

This function returns substring of \"string of inifinite length\" generated
by STR.

You can use this function to perform a variety of tasks:

  To rotate a string left:  (erfi:xsubstring \"abcdef\" 2)   => \"cdefab\"
  To rotate a string right: (erfi:xsubstring \"abcdef\" -2)  => \"efabcd\"
  To replicate a string:    (erfi:xsubstring \"abc\" 0 7)    => \"abcabca\"

For more details, see SRFI documentation."
  (let1 len (length str)
    (when (or (and start (not (<= 0 start)))
              (and end   (not (<  start end))
                         (not (<= end len)))
              (and to    (not (<= from to)))
              (and start end (= start end)
                   to (not (= from to))))
      (lwarn 'erfi-srfi-13 :error "`erfi:xsubstring': argument out of range")
      (error "`erfi:xsubstring': argument out of range"))
    (if (and to (= from to))
        ""
        (let* ((str (substring str (or start 0) end))
               (len (length str))
               (to (or to (+ from (- (or start 0)) (or end len))))
               (q1 (floor from len))
               (r1 (mod   from len))
               (q2 (floor to   len))
               (r2 (mod   to   len)))
          (if (and (= q1 q2) (<= r1 r2))
              (substring str r1 r2)
              (erfi:string-concatenate `(,(substring str (mod from len) len)
                                         ,@(make-list (- q2 q1 1) str)
                                         ,(substring str 0 (mod to len)))))))))


(provide 'erfi-srfi-13)
;;; erfi-srfi-13.el ends here
