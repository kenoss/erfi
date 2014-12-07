;;; erfi-emacs.el --- Utility functions for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno@senecio>
;; Keywords: 
;; URL: https://github.com/erfi
;; Package-Requires: ((emacs "24"))

;; Apache License, Version 2.0

;;; Commentary:

;; 

;;; Code:


(eval-when-compile
  (require 'cl)
  (require 'erfi-macros)
  (erfi:use-short-macro-name))

(require 'cl-lib)



;;;
;;; One time hook
;;;

(defun erfi:add-one-time-hook (hook func &rest app local)
  "Like `add-hook', but FUNCTION is called only once.

FUNCTION must be a symbol.
\n(fn hook function &rest append local)"
  (let1 sym (intern (symbol-name (cl-gensym "erfi:add-one-time-hook:")))
    (eval `(defun ,sym ()
             (unwind-protect
                 (funcall ,(cond ((symbolp func)   `(quote ,func))
                                 ((functionp func) func)
                                 (t                (lwarn 'erfi :error "Wrong type argument"))))
               (remove-hook ',hook ',sym)))
          t)
    (add-hook hook sym app local)))

(defmacro erfi:with-temporary-hook (hook func &rest args)
  "Evaluate BODY with FUNCTION added to HOOK.
If third argument is a list and its car is keyword :options,
cdr of that list must be a plist and it designates options.  Keywords are
:append and :local, values are used for `add-hook' and `remove-hook'.
\n(fn hook function [(:options ,@plist)] &rest body)"
  ;; `functionp' causes unpleasant result if FUNC is not defined
  ;;  before this macro expantion.
  (declare (indent 2))
  (let* ((func* (if (symbolp func)
                    func
                    (let1 sym (cl-gensym "erfi:with-temporary-hook:")
                      (eval `(defun ,sym ()
                               (funcall ,func))
                            t))))
         (flag (eq :options (and (listp (car args)) (caar args))))
         (option-plist (when flag (cdar args)))
         (body (if flag (cdr args) args)))
    `(unwind-protect
         (progn
           (add-hook ,hook ',func*
                     ,@(when flag
                         (list (plist-get option-plist :append)
                               (plist-get option-plist :local))))
           ,@body)
       (remove-hook ,hook ',func*
                    ,@(when flag
                        (list (plist-get option-plist :local)))))))



;;;
;;; Highlight line until next command
;;;

(eval-when-compile (require 'hl-line))

(defvar *erfi-emacs-hl-buffers* nil)

(defun erfi-emacs-hl-turn-on-until-next-command ()
  "Turn on `hl-line-mode' until next command."
  (progn
    (add-hook 'pre-command-hook 'erfi-emacs-hl-turn-off/pre-command-hook)
    (unless hl-line-mode
      (push (current-buffer) *erfi-emacs-hl-buffers*))
    (hl-line-mode +1)))

(defun erfi-emacs-hl-turn-off/pre-command-hook ()
  (unwind-protect
      (while *erfi-emacs-hl-buffers*
        (let1 buf (pop *erfi-emacs-hl-buffers*)
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (hl-line-mode -1)))))
    (remove-hook 'pre-command-hook 'erfi-emacs-hl-turn-off/pre-command-hook)))


(provide 'erfi-emacs)
;;; erfi-emacs.el ends here
