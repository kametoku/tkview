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
                     :deny-label "Cancel"
                     :on-approve #'update-object))

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

(defmethod sanitize-arguments ((widget edit-modal) args)
  "Converts some parameters to database representation."
  (remf args :widget)
  (dolist (widget (attribute:input-widgets (form-content widget)))
    (setf (getf args (tkutil:string-to-keyword (fui.modules:name widget)))
          (fui.modules:value widget)))
  args)

(defmethod update-object (object &rest args &key widget &allow-other-keys)
  (apply #'tkmito.model:update object (sanitize-arguments widget args))
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
