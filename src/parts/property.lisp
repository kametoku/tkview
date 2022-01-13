(in-package :cl)
(defpackage :tkview.parts.property
  (:use :cl :attribute)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:shadow #:formatter)
  (:import-from :reblocks/html
                :with-html)
  (:export :make-property-widget))
(in-package :tkview.parts.property)

(defwidget property-widget ()
  ((formatter :initarg :formatter
              :initform #'attributes
              :reader formatter)
   (object :initarg :object
           :reader object)))

(defmethod reblocks/widget:render ((widget property-widget))
  (let ((formatter (formatter widget))
        (object (object widget)))
    (with-html
      ;; TODO: render [back] button
      (object-page-attributes formatter object))))

(defun make-property-widget (&rest args &key object formatter)
  (declare (ignore object formatter))
  (apply #'make-instance 'property-widget args))
