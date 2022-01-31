(defpackage :tkview.modal.edit
  (:use :cl :tkview.modal)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:shadow #:formatter)
  (:export #:edit-modal
           #:formatter
           #:description
           #:edit-modal-title
           #:sanitize-arguments
           #:update-object
           #:make-form-content
           #:make-edit-modal))
(in-package :tkview.modal.edit)

(defwidget edit-modal (modal-widget)
  ((formatter :initarg :formatter
              :initform #'attribute:attributes
              :reader formatter)
   (description :initarg :description
                :initform nil
                :reader description)
   (form-content :initarg nil
                 :accessor form-content))
  (:default-initargs :title-icon "edit"
                     :use-form-p t
                     :approve-label "OK"
                     :deny-label "Cancel"))

(defgeneric edit-modal-title (object)
  (:documentation "Returns the title string of edit modal for OBJECT.")
  (:method (object)
    (format nil "Edit ~:(~A~)" (type-of object)))
  (:method ((object mito:dao-table-class))
    (let ((id (mito:object-id object)))
      (format nil "Edit ~:(~A~) #~A" (type-of object) id)))
  (:method ((object tkmito.mixin:has-code))
    (let ((code (tkmito.mixin:code object)))
      (if code
          (format nil "Edit ~:(~A~) ~A" (type-of object) code)
          (call-next-method)))))

(defmethod initialize-instance :around ((widget edit-modal) &rest args
                                        &key object (title (edit-modal-title object))
                                        &allow-other-keys)
  (apply #'call-next-method widget :title title args))

(defgeneric sanitize-arguments (widget args)
  (:documentation "Converts some parameters to database representation.")
  (:method ((widget edit-modal) args)
    (dolist (widget (attribute:input-widgets (form-content widget)))
      (setf (getf args (tkutil:string-to-keyword (fui.modules:name widget)))
            (fui.modules:value widget)))
    args))

(defgeneric update-object (widget object &rest args)
  (:documentation "Update slots of OBJECT per ARGS.")
  (:method (widget object &rest args)
    (apply #'tkmito.model:update object args)))

(defmethod tkview.modal:on-approve ((widget edit-modal) object &rest args)
  (unless object
    (error "No object to update."))
  (apply #'update-object widget object (sanitize-arguments widget args))
  '("success" :message "Updated successfully."))

(defgeneric make-form-content (widget))

(defmethod make-form-content ((widget edit-modal))
  (attribute:make-edit-widget (formatter widget) (object widget)))

(defmethod render-form-content ((widget edit-modal))
  (let ((form-content (make-form-content widget)))
    (render form-content)
    (setf (form-content widget) form-content)))

(defmethod render-form-content :around ((widget edit-modal))
  (with-html
    (:div :class "content"
          (when (description widget)
            (:div :class "ui grid"
                  (:div :class "column" (description widget))))
          (call-next-method))))
  
(defun make-edit-modal (&rest args &key object formatter description &allow-other-keys)
  (declare (ignore object formatter description))
  (apply #'make-instance 'edit-modal args))
