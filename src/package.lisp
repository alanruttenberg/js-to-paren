;;;; package.lisp
;;;
;;; Define the packages used by the js-to-paren system.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :js-to-paren
  (:use :cl :cl-ppcre :parenscript)
  (:nicknames :jsps)
  (:export
   #:parse
   #:process
   #:syntax-error))
