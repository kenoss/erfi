;;; erfi-emacs-test.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>

;; Apache License, Version 2.0

;;; Commentary:

;; 

;;; Code:


(require 'ert)

(require 'erfi-macros)
(erfi:use-short-macro-name)

(require 'erfi-emacs)



;;;
;;; One time hook
;;;

(defvar *erfi-test:add-one-time-hook:counter* nil)

(defun erfi-test:add-one-time-hook:fn ()
  (incf *erfi-test:add-one-time-hook:counter*))

(ert-deftest erfi-test:add-one-time-hook ()
  (let1 *erfi-test:add-one-time-hook:counter* 0
    (erfi:add-one-time-hook 'activate-mark-hook 'erfi-test:add-one-time-hook:fn)
    (with-temp-buffer
      (should (= 0 *erfi-test:add-one-time-hook:counter*))
      (set-mark 0)
      (should (= 1 *erfi-test:add-one-time-hook:counter*))
      (set-mark 0)
      (should (= 1 *erfi-test:add-one-time-hook:counter*))))

  (let1 *erfi-test:add-one-time-hook:counter* 0
    (erfi:add-one-time-hook 'activate-mark-hook
                            ;; This use is not recommended.
                            '(lambda () (incf *erfi-test:add-one-time-hook:counter*)))
    (with-temp-buffer
      (should (= 0 *erfi-test:add-one-time-hook:counter*))
      (set-mark 0)
      (should (= 1 *erfi-test:add-one-time-hook:counter*))
      (set-mark 0)
      (should (= 1 *erfi-test:add-one-time-hook:counter*))))
  )

(ert-deftest erfi-test:with-temporary-hook ()
  (should (equal '(unwind-protect
                      (progn
                        (add-hook 'hook 'erfi:with-temporary-hook:0)
                        body1
                        body2)
                    (remove-hook 'hook 'erfi:with-temporary-hook:0))
                 (flet ((make-symbol (&optional prefix) (intern prefix)))
                   (let1 *gensym-counter* 0
                     (macroexpand '(erfi:with-temporary-hook
                                       'hook 'func
                                     body1
                                     body2))))))
  (should (equal '(unwind-protect
                      (progn
                        (add-hook 'hook 'erfi:with-temporary-hook:0 app 'loc)
                        body1
                        body2)
                    (remove-hook 'hook 'erfi:with-temporary-hook:0 'loc))
                 (flet ((make-symbol (&optional prefix) (intern prefix)))
                   (let1 *gensym-counter* 0
                     (macroexpand '(erfi:with-temporary-hook
                                       'hook 'func (:options :append app
                                                             :local 'loc)
                                       body1
                                       body2))))))
  )


;;; erfi-emacs-test.el ends here
