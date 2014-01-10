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
  (:use :jwacs :cl)
  (:nicknames :jsps)

  (:import-from
   "JWACS" "PARSE" jwacs::return-statement-arg
   jwacs::function-decl-body jwacs::function-decl-parameters
   jwacs::function-decl-name jwacs::function-expression-body
   jwacs::function-expression-parameters
   jwacs::function-expression-name jwacs::statement-block-statements
   jwacs::comma-expr-exprs jwacs::for-body jwacs::for-step
   jwacs::for-condition jwacs::for-initializer jwacs::while-body
   jwacs::while-condition jwacs::do-statement-body
   jwacs::do-statement-condition jwacs::if-statement-else-statement
   jwacs::if-statement-then-statement jwacs::if-statement-condition
   jwacs::conditional-false-arg jwacs::conditional-true-arg
   jwacs::conditional-condition jwacs::property-access-field
   jwacs::property-access-target jwacs::new-expr-args
   jwacs::new-expr-constructor jwacs::object-literal-properties
   jwacs::array-literal-elements jwacs::string-literal-value
   jwacs::special-value-symbol jwacs::identifier-name
   jwacs::fn-call-args jwacs::fn-call-fn
   jwacs::unary-operator-op-symbol jwacs::unary-operator-arg
   jwacs::binary-operator-op-symbol jwacs::binary-operator-right-arg
   jwacs::binary-operator-left-arg jwacs::numeric-literal-value
   jwacs::var-decl-initializer jwacs::var-decl-name
   jwacs::var-decl-statement-var-decls
   jwacs::expression jwacs::var-decl-statement jwacs::var-decl
   jwacs::numeric-literal jwacs::binary-operator jwacs::unary-operator
   jwacs::fn-call jwacs::identifier jwacs::special-value
   jwacs::string-literal jwacs::array-literal jwacs::object-literal
   jwacs::new-expr jwacs::property-access jwacs::conditional
   jwacs::if-statement jwacs::do-statement jwacs::while jwacs::for
   jwacs::comma-expr jwacs::statement-block jwacs::function-expression
   jwacs::function-decl jwacs::return-statement
   jwacs::*symbols-to-paren-tokens*)
  (:import-from "PARENSCRIPT" "CREATE" "GETPROP")) 