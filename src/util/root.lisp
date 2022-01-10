(defpackage :tkview.util.root
  (:use :cl)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
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

(defmethod reblocks/widget:render ((widget root-widget))
  (log:info "Handling request: ~A ~A"
            (reblocks/request:get-method) (reblocks/request:get-uri))
  (dolist (child (children widget))
    (reblocks/widget:render child)))

(defun make-root-widget (&key children)
  (make-instance 'root-widget :children children))

(defun root ()
  (reblocks/widgets/root:get))

(defun find-child (type &key (key #'type-of))
  "Find a child of the root widget whose type is TYPE."
  (find type (children (root)) :key key))
