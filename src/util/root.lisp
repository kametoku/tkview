(defpackage :tkview.util.root
  (:use :cl)
  (:import-from :weblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :weblocks/html
                :with-html)
  (:export :make-root-widget
           :root
           :find-child))
(in-package :tkview.util.root)

(defwidget root-widget ()
  ((children :type list
             :initarg :children
             :initform nil
             :accessor children)))

(defmethod weblocks/widget:render ((widget root-widget))
  (log:info "Handling request: ~A ~A"
            (weblocks/request:get-method) (weblocks/request:get-uri))
  (dolist (child (children widget))
    (weblocks/widget:render child)))

(defun make-root-widget (&key children)
  (make-instance 'root-widget :children children))

(defun root ()
  (weblocks/widgets/root:get))

(defun find-child (type &key (key #'type-of))
  "Find a child of the root widget whose type is TYPE."
  (find type (children (root)) :key key))
