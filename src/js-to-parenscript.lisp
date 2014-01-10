(in-package :js-on-cl)

(defparameter *symbols-to-paren-tokens*
  (let ((ht (make-hash-table :test 'eq)))
    (maphash #'(lambda (k v)
                (setf (gethash k ht) v))
             *symbols-to-tokens*)
    (loop for (k v) in '(;; where do these appears?
                         ;;(:COLON ":")
                         ;;(:HOOK "?")
                         (:LOGICAL-OR "or")
                         (:ASSIGN "setf")
                         (:BAR2 "or")
                         (:BANG "not")
                         (:POST-INCR "incf")
                         (:MINUS2 "decf")
                         (:POST-DECR "decf")
                         (:PLUS2 "incf")
                         (:PRE-INCR "incf")
                         (:PRE-DECR "decf")
                         (:LOGICAL-NOT "not"))
          do (setf (gethash k ht) v))
    ht)
  "Map from token symbol to parenscript token.")

(defparameter *symbols-to-paren-tokens*
  (let ((ht (make-hash-table :test 'eq)))
    (maphash #'(lambda (k v)
                (setf (gethash k ht) v))
             *symbols-to-tokens*)
    (loop for (k v) in '(;; where do these appears?
                         ;;(:COLON ":")
                         ;;(:HOOK "?")
                         (:LOGICAL-OR "or")
                         (:ASSIGN "setf")
                         (:BAR2 "or")
                         (:BANG "not")
                         (:POST-INCR "incf")
                         (:MINUS2 "decf")
                         (:POST-DECR "decf")
                         (:PLUS2 "incf")
                         (:PRE-INCR "incf")
                         (:PRE-DECR "decf")
                         (:LOGICAL-NOT "not"))
          do (setf (gethash k ht) v))
    ht)
  "Map from token symbol to parenscript token.")



(defun js-intern (js-literal-string)
  "interns a camel-cased js string to an appropriate lispy symbol"
  (intern
   (string-upcase
    (as-lisp-style-symbol-string
     js-literal-string))))

(defun as-lisp-style-symbol-string (js-style-symbol-string)
  "converts camelCasedStrings to not-so camel-cased-strings"
  (cl-ppcre:regex-replace-all
   (cl-ppcre:create-scanner "[A-Z]")
   js-style-symbol-string ;"AnAniMal"
   #'(lambda (match &rest ignored)
       (format nil "-~A" (string-downcase match)))
   :simple-calls t))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop repeat (length names) collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro expand-progn-subexp (subexp)
  (once-only (subexp)
    `(if (eql 1 (length ,subexp))
      (as-paren (first ,subexp))
      `(progn
        ,@(mapcar #'as-paren ,subexp)))))

(defgeneric as-paren (js-elem)
  (:documentation "converts a javascript element to a parenscript element.
Input is an abstract javascript form and output is a parenscript form."))

(defmethod as-paren ((expr expression))
  (print "an expression!")
  expr)

(defmethod as-paren ((lengthy-decl var-decl-statement))
  "Converts a var declaration statement like var x = 3, y = 34; to a
series of (defvar x 3) (defvar y 34) forms."
  (expand-progn-subexp (var-decl-statement-var-decls lengthy-decl)))

(defmethod as-paren ((decl var-decl))
  (let ((name (js-intern
	       (var-decl-name decl)))
	(value (var-decl-initializer decl)))
    (if (null value)
	`(defvar ,name)
	`(defvar ,name ,(as-paren value) ))))

(defmethod as-paren ((js-form numeric-literal))
  (let ((number (numeric-literal-value js-form)))
    number))

(defmethod as-paren ((js-form binary-operator))
  (let ((left-arg (binary-operator-left-arg js-form))
	(right-arg (binary-operator-right-arg js-form))
	(op (token-to-paren (binary-operator-op-symbol js-form))))
    `(,op
      ,(as-paren left-arg)
      ,(as-paren right-arg))))

(defun token-to-paren (tok)
  (js-intern
   (or
    (gethash tok *symbols-to-paren-tokens*)
    (string-downcase (string tok)))))

(defmethod as-paren ((js-form unary-operator))
  (let ((arg (unary-operator-arg js-form))
	(op (token-to-paren (unary-operator-op-symbol js-form))))
    `(,op
      ,(as-paren arg) )))

(defmethod as-paren ((js-form fn-call))
  (let ((fn-identifier (fn-call-fn js-form))
	(fn-args (fn-call-args js-form)))
    `(,(as-paren fn-identifier)
      ,@(mapcar #'as-paren fn-args))))

(defmethod as-paren ((js-form identifier))
  (let ((name (identifier-name js-form)))
    (js-intern name)))

(defmethod as-paren ((js-form special-value))
  (let ((keyword-sym (special-value-symbol js-form)))
    (token-to-paren keyword-sym)))

(defmethod as-paren ((js-form string-literal))
  (let ((value (string-literal-value js-form)))
    value))

(defmethod as-paren ((js-form array-literal))
  (let ((elements (array-literal-elements js-form)))
    `(array ,@(mapcar #'as-paren elements))))

(defmethod as-paren ((js-form object-literal))
  (let ((properties (object-literal-properties js-form)))
    `(ps:create ,@(mapcan
		#'(lambda (entry)
		    `(,(as-paren (car entry))
		      ,(as-paren (cdr entry))))
		properties))))

(defmethod as-paren ((js-form new-expr))
  (let ((constructor (new-expr-constructor js-form))
	(args (new-expr-args js-form)))
    (if (null args)
	`(new ,(as-paren constructor))
	`(new (,(as-paren constructor)
	       ,@(mapcar #'as-paren args))))))


(defmethod as-paren ((js-form property-access))
  (let ((target (property-access-target js-form))
	(field (property-access-field js-form)))
    `(ps:getprop ,(as-paren target) ,(as-paren field) )))

(defmethod as-paren ((js-form conditional))
  (let ((condition (conditional-condition js-form))
	(true-arg (conditional-true-arg js-form))
	(false-arg (conditional-false-arg js-form)))
    `(if ,(as-paren condition)
      ,(as-paren true-arg)
      ,(as-paren false-arg) )))

(defmethod as-paren ((js-form if-statement))
  (let ((condition (if-statement-condition js-form))
	(then-arg (if-statement-then-statement js-form))
	(else-arg (if-statement-else-statement js-form)))
    (if (not else-arg)
	`(when ,(as-paren condition) ,(as-paren then-arg))
	`(if ,(as-paren condition) ,(as-paren then-arg) ,(as-paren else-arg)))))

(defmethod as-paren ((js-form do-statement))
  (let ((condition (do-statement-condition js-form))
	(body (do-statement-body js-form)))
    `(while t
      ,(as-paren body)
      (if ,(as-paren condition)
	  (continue) (break)))))

(defmethod as-paren ((js-form while))
  (let ((condition (while-condition js-form))
	(body (while-body js-form)))
    `(while ,(as-paren condition) ,(as-paren body))))

(defmethod as-paren ((js-form for))
  (let ((initializer (for-initializer js-form))
	(condition (for-condition js-form))
	(step (for-step js-form))
	(body (for-body js-form)) )
    `(progn
      ,(as-paren initializer)
      (while ,(as-paren condition)
        ,(as-paren body) ,(as-paren step)))))


(defmethod as-paren ((js-form comma-expr))
  (expand-progn-subexp (comma-expr-exprs js-form)))

(defmethod as-paren ((js-form statement-block))
  (expand-progn-subexp (statement-block-statements js-form)))

(defmethod as-paren ((js-form function-expression))
  (let ((name (function-expression-name js-form))
	(params (function-expression-parameters js-form))
	(body (function-expression-body js-form)))
    `(,(if (null name) 'lambda 'defun)
      ,(mapcar #'js-intern params)
      ,@(mapcar #'as-paren body))))

(defmethod as-paren ((js-form function-decl))
  (with-accessors ((name function-decl-name)
                   (params function-decl-parameters)
                   (body function-decl-body))
                  js-form
                  `(defun ,(js-intern name) ,(mapcar #'js-intern params)
                     ,@(mapcar #'as-paren body))))

(defmethod as-paren ((js-form return-statement))
  `(return ,(as-paren (return-statement-arg js-form))))

;; (defmethod as-paren ((js-form y))
;;   (let* ((body (try-body js-form))
;;          (catch-clause (try-catch-clause js-form))
;;          (binding (catch-clause-binding catch-clause))
;;          (catch-body (catch-clause-body catch-clause))
;;          (finally (try-finally-clause js-form)))
;;     `(try
;;       ,(expand-progn-subexp body)
;;       (:catch (,(js-intern binding))
;;         ,(expand-progn-subexp catch-body))
;;       ,@(when finally
;;               `((:finally
;;                  ,(expand-progn-subexp
;;                    (finally-clause-body finally))))))))
  
(defun js-to-paren (js-text)
  (apply #'list 'progn (mapcar #'as-paren (parse js-text))))

(defvar *example-script* "function getElementPosition(elemID) {
    var offsetTrail = document.getElementById(elemID);
    var offsetLeft = 0;
    var offsetTop = 0;
    while (offsetTrail) {
        offsetLeft += offsetTrail.offsetLeft;
        offsetTop += offsetTrail.offsetTop;
        offsetTrail = offsetTrail.offsetParent;
    }
    if (navigator.userAgent.indexOf(\"Mac\") != -1 &&
        typeof document.body.leftMargin != \"undefined\") {
        offsetLeft += document.body.leftMargin;
        offsetTop += document.body.topMargin;
    }
    return {left:offsetLeft, top:offsetTop};
}")

(defun example-usage ()
  (js-to-paren *example-script*))