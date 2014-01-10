;;;; jwacs-tests.asd
;;;
;;; Defines an asdf system containing unit tests for jwacs.

(defpackage :js-on-cl-tests-system
  (:use :cl :asdf))
(in-package :js-on-cl-tests-system)

;;;; ======= System definition =====================================================================
(asdf:defsystem js-on-cl-tests
    :version "0.1"
    :author "James Wright <chumsley@gmail.com>, Greg Smolyn <greg@smolyn.org>"
    :licence "BSD License <http://www.opensource.org/licenses/bsd-license.php>"
    :serial t
    :components
    ((:module "external"
              :components
              ((:file "rt")))
     (:module "tests"
              :serial t
              :components
              ((:file "package")
               (:file "test-utils")
               (:file "test-lexer")
               (:file "test-parser")
               (:file "test-pretty-print")
               (:file "test-static-analysis")
               (:file "test-type-analysis")
               (:file "test-ugly-print")
;               (:file "test-source-transformations")
;               (:file "test-shift-decls-transformation")
;               (:file "test-explicitize")
;               (:file "test-shadow-values-transformation")
;               (:file "test-cps-transformation")
;               (:file "test-loop-transformation")
;               (:file "test-trampoline-transformation")
;               (:file "test-runtime-transformation")
 ;              (:js-on-cl-file "lang-tests"))))
	       )))
    :depends-on (js-on-cl))

;;;; ======= Test operation ========================================================================
(defmethod perform ((o test-op) (c (eql (find-system 'js-on-cl-tests))))
  (operate 'load-op :js-on-cl)
  (funcall (intern (symbol-name '#:do-tests) (find-package :js-on-cl-tests))))

(defmethod operation-done-p ((o test-op) (c (eql (find-system 'js-on-cl-tests))))
  nil)