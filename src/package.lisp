;;;; package.lisp
;;;
;;; Define the packages used by the js-to-paren system.
;;;
;;; Copyright (c) 2005 James Wright
;;; See LICENSE for full licensing details.

;; Eventually this may want to be several sub-packages, but let's start simple for now
(defpackage :jwacs
  (:use :cl :cl-ppcre)
  (:nicknames :jw)
  (:export
   #:parse
   #:process
   #:syntax-error))


(defpackage :js-to-paren
  (:use :jwacs)
  (:nicknames :jsps)
  (:export
   ))
