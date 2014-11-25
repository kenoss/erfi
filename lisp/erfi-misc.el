;;; erfi-misc.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>
;; Keywords: extensions, lisp
;; URL: https://github.com/kenoss/erfi
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;; Namespace "erfi-emacs" may be changed in the future.

;;; Code:


(eval-when-compile
  (setq byte-compile-warnings '(not cl-functions))
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'erfi-srfi-1)



;;;
;;; Symbol and keyword name
;;;

(defun erfi:keyword-name (keyword)
  "Return KEYWORDS's name without semicolon, a string."
  (substring (symbol-name keyword) 1 nil))
(defun erfi:symbol/keyword-name (x)
  "Return a string. If X is symbol it is symbol name. If X is keyword it is keyword name without semicolon."
  (cond ((keywordp x) (erfi:keyword-name x))
        ((symbolp x) (symbol-name x))
        (t (error "symbol or keyword required"))))



;;;
;;; Strings
;;;

(defun erfi:normalize-strings-length (str-list &optional alignment min-length)
  "Normalize length of strings to the maximum length of string in STR-LIST
and MIN-LENGTH.

ALIGNMENT can be 'left or 'right, defaults to 'left.
MIN-LENGTH defaults to 0."
  (let1 len (format "%s" (max (or min-length 0)
                              (erfi:foldl (lambda (acc s) (max acc (length s))) 0 str-list)))
    (erfi:case alignment
      ((left nil) (mapcar (lambda (s) (format (concat "%-" len "s") s)) str-list))
      ((right)    (mapcar (lambda (s) (format (concat "%"  len "s") s)) str-list)))))



;;;
;;; Etched overlays
;;;

(defun erfi-emacs:etched-overlays-in (start end &optional object)
  "Return a list of `(,range ,overlay-properties), where range is `(,s ,e)."
  ;; Take care of properties of first character.  `next-single-property-change'
  ;; only detect that between (1+ START) and END.
  (erfi:let lp ((res (let1 ps (text-properties-at start)
                       (if (memq 'overlay-plist ps)
                           `(((,start ,(+ start (cadr (memq 'overlay-length ps))))
                              ,(cadr (memq 'overlay-plist ps))))
                           '())))
                (pos start))
    (let1 next (and (< pos end)
                    (next-single-property-change pos 'overlay-plist object end))
      (if (or (null next) (= next end))
          (nreverse res)
          (let ((len (get-text-property next 'overlay-length object))
                (prop (get-text-property next 'overlay-plist object)))
            (if (null len) ; Skip the ends of overlays.
                (lp res next)
                (lp (cons `((,next ,(+ next len)) ,prop) res) next)))))))

(defun erfi-emacs:buffer-substring/etched-overlays (start end &optional buffer)
  "Like `buffer-substring', but embed overlays to the string."
  (with-current-buffer (or buffer (current-buffer))
    (let ((str (buffer-substring start end))
          (overlays (overlays-in start end)))
      (dolist (ol overlays)
        (let ((s (- (overlay-start ol) start))
              (e (- (overlay-end ol) start)))
          (add-text-properties s e
                               `(overlay-length ,(- e s)
                                 overlay-plist ,(overlay-properties ol))
                               str)))
      str)))

(defun erfi-emacs:recover-etched-overlays! (start end &optional buffer)
  "Recover etched overlays between START and END in BUFFER.
BUFFER defaults to the current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (etched-overlays (erfi-emacs:etched-overlays-in start end buffer)))
    (remove-text-properties start end '(overlay-length nil overlay-plist nil) buffer)
    (dolist (eo etched-overlays)
      (destructuring-bind ((s e) plis) eo
        (let1 ol (make-overlay s e buffer)
          (while (not (null plis))
            (overlay-put ol (car plis) (cadr plis))
            (setq plis (cddr plis))))))))
(defun erfi-emacs:insert/etched-overlays (str)
  "Insert string and recover etched overlays."
  (let1 p (point)
    (insert str)
    (erfi-emacs:recover-etched-overlays! p (point))))

(defun erfi-emacs:make-button-string/etched-overlays (&rest properties)
  "Make button string."
  (with-temp-buffer
    (apply 'insert-button properties)
    (erfi-emacs:buffer-substring/etched-overlays (point-min) (point-max))))


(provide 'erfi-misc)
;;; erfi-misc.el ends here
