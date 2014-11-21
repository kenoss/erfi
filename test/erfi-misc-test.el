;;; erfi-misc-test.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2014  Ken Okada

;; Author: Ken Okada <keno.ss57@gmail.com>

;; Apache License, Version 2.0

;;; Commentary:

;; 

;;; Code:


(require 'ert)

(require 'erfi-macros)
(erfi:use-short-macro-name)

(require 'erfi-misc)



;;;
;;; Strings
;;;

(ert-deftest erfi-test:normalize-strings-length ()
  (should (equal '("hoge    "
                   "foo     "
                   "f       "
                   "bar     "
                   "hogehoge")
                 (erfi:normalize-strings-length '("hoge" "foo" "f" "bar" "hogehoge"))))
  (should (equal '("hoge    "
                   "foo     "
                   "f       "
                   "bar     "
                   "hogehoge")
                 (erfi:normalize-strings-length '("hoge" "foo" "f" "bar" "hogehoge") 'left)))
  (should (equal '("    hoge"
                   "     foo"
                   "       f"
                   "     bar"
                   "hogehoge")
                 (erfi:normalize-strings-length '("hoge" "foo" "f" "bar" "hogehoge") 'right)))
  (should (equal '("hoge      "
                   "foo       "
                   "f         "
                   "bar       "
                   "hogehoge  ")
                 (erfi:normalize-strings-length '("hoge" "foo" "f" "bar" "hogehoge") nil 10)))
  )



;;;
;;; Etched overlays
;;;

(ert-deftest erfi-test:make-button-string/etched-overlays ()
  (with-temp-buffer
    (erfi-emacs:insert/etched-overlays
     (erfi-emacs:make-button-string/etched-overlays "fsf"
                                                    'action (lambda (x) (browse-url (button-get x 'url)))
                                                    'url "http://www.fsf.org"))
    (should (equal "http://www.fsf.org"
                   (overlay-get (car (overlays-in (point-min) (point-max))) 'url))))
  )


;;; erfi-misc-test.el ends here
