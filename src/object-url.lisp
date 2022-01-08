(in-package :tkview)

(defgeneric object-url (object &rest args)
  (:documentation "Return the URL of the object.
OBJECT ::= INSTANCE | TYPE
ARGS ::= &key PREFIX PAGE-TYPE QUERY"))

(defmethod object-url ((object string) &key prefix page-type query)
  (let* ((path (format nil "~:[~;~:*~A~]~A~:[~;~:*/~A~]"
                       prefix object page-type))
         (quri (quri:make-uri :path path :query query)))
    (quri:render-uri quri)))

;; (defmethod object-url ((object mito:dao-table-class) &rest args
;;                        &key (base-url (object-url (type-of object))) &allow-other-keys)
;;   (remf args :base-url)
;;   (let ((url (format nil "~A/~A" base-url (mito:object-id object))))
;;     (apply #'object-url url args)))

;; (defmethod object-url ((object tkmito.mixin:has-code) &rest args
;;                        &key (base-url (object-url (type-of object))) &allow-other-keys)
;;   (remf args :base-url)
;;   (let ((url (format nil "~A/~A" base-url (tkmito.mixin:code object))))
;;     (apply #'object-url url args)))

(defmethod object-url ((object mito:dao-class) &rest args
                       &key (base-url (object-url (type-of object))) &allow-other-keys)
  (remf args :base-url)
  (let* ((id (if (typep object 'tkmito.mixin:has-code)
                 (tkmito.mixin:code object)
                 (mito:object-id object)))
         (url (format nil "~A/~A" base-url id)))
    (apply #'object-url url args)))
