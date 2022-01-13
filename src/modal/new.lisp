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
                     :deny-label "Cancel"
                     :on-approve #'create-object))

(defun new-modal-title (object-type)
  (format nil "Create ~:(~A~)" object-type))

(defmethod initialize-instance :around ((widget new-modal) &rest args
                                        &key object-type
                                          (title (new-modal-title object-type))
                                        &allow-other-keys)
  (apply #'call-next-method widget :title title args))

(defmethod create-object (object &rest args &key widget &allow-other-keys)
  (let* ((object-type (object-type widget))
         (args (tkview.modal.edit:sanitize-arguments widget args))
         (object (apply #'make-instance object-type args)))
    (apply #'tkmito.model:validate object args)
    (mito:save-dao object)
    '("success" :message "Created successfully.")))
  
(defmethod tkview.modal.edit:make-form-content ((widget new-modal) formatter object)
  (attribute:make-new-widget formatter object))

(defun make-new-modal (&rest args &key object formatter description &allow-other-keys)
  (declare (ignore object formatter description))
  (apply #'make-instance 'new-modal args))
