(defpackage :tkview.modal.new
  (:use :cl :tkview.modal)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:export #:new-modal
           #:create-object
           #:make-new-modal))
(in-package :tkview.modal.new)

(defwidget new-modal (tkview.modal.edit:edit-modal)
  ((object-type :initarg :object-type
                :initform nil
                :reader object-type))
  (:default-initargs :title-icon "plus circle"
                     :approve-label "Create"
                     :deny-label "Cancel"))

(defun new-modal-title (object-type)
  (format nil "Create ~:(~A~)" object-type))

(defmethod initialize-instance :around ((widget new-modal) &rest args
                                        &key object-type
                                          (title (new-modal-title object-type))
                                        &allow-other-keys)
  (apply #'call-next-method widget :title title args))

(defgeneric create-object (widget object-type &rest args)
  (:method (widget object-type &rest args)
    (let ((object (apply #'make-instance object-type args)))
      (apply #'tkmito.model:validate object args)
      (mito:save-dao object))))

(defmethod tkview.modal:on-approve ((widget new-modal) object &rest args)
  (apply #'create-object widget (object-type widget)
         (tkview.modal.edit:sanitize-arguments widget args))
  '("success" :message "Created successfully."))
  
(defmethod tkview.modal.edit:make-form-content ((widget new-modal))
  (attribute:make-new-widget (tkview.modal.edit:formatter widget)
                             (object-type widget)))

(defun make-new-modal (&rest args &key object object-type formatter description
                       &allow-other-keys)
  (declare (ignore object-type formatter description))
  (when object
    (error "OBJECT parameter cannot be non-nil for the new modal."))
  (apply #'make-instance 'new-modal args))
