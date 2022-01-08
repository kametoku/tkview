(in-package :cl)
(defpackage :tkview.parts.property
  (:use :cl :attribute)
  (:import-from :weblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :weblocks/html
                :with-html)
  (:export :make-property-widget))
(in-package :tkview.parts.property)

(defwidget property-widget ()
  ((row-formatter :initarg :row-formatter
                  :initform #'attributes
                  :reader row-formatter)
   (object :initarg :object
           :reader object)))

(defmethod weblocks/widget:render ((widget property-widget))
  (let ((row-formatter (row-formatter widget))
        (object (object widget)))
    (with-html
      ;; TODO: render [back] button
      (object-page-attributes row-formatter object))))

(defun make-property-widget (&rest args &key object row-formatter)
  (declare (ignore object row-formatter))
  (apply #'make-instance 'property-widget args))
