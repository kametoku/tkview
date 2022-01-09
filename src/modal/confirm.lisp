(defpackage :tkview.modal.confirm
  (:use :cl)
  (:import-from :weblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :weblocks/html
                :with-html)
  (:export #:confirm-modal
           #:render-confirm-message
           #:make-confirm-modal))
(in-package :tkview.modal.confirm)

(defwidget confirm-modal (tkview.modal:modal-widget)
  ((message :initarg :message
            :initform "Are you sure?"
            :accessor message))
  (:default-initargs :title "Confirmation"))

(defmethod render-confirm-message ((widget confirm-modal))
  (with-html
    (:i :class "blue big question circle outline icon")
    " " 
    (let ((message (message widget)))
      (if (functionp message)
          (funcall message widget)
          message))))

(defmethod tkview.modal:render-content ((widget confirm-modal))
  (with-html
    (:div :class "content"
          (render-confirm-message widget))))

(defun make-confirm-modal (&rest args)
  (apply #'make-instance 'confirm-modal args))
