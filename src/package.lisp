;;;; package.lisp
;;;
;;; Define the packages used by the js-on-cl system.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :js-on-cl
  (:use :cl :cl-ppcre)
  (:nicknames :js+cl :cl+js)
  (:export
   #:parse
   #:process
   #:syntax-error))
