(defpackage :tkview.modal.edit
  (:use :cl :tkview.modal)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:export #:edit-modal
           #:description
           #:edit-modal-title
           #:sanitize-arguments
           #:update-object
           #:make-form-content
           #:make-edit-modal))
(in-package :tkview.modal.edit)

(defwidget edit-modal (modal-widget)
  ((row-formatter :initarg :row-formatter
                  :initform #'attribute:attributes
                  :reader row-formatter)
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

(defmethod make-form-content ((widget edit-modal) formatter object)
  (attribute:make-edit-widget formatter object))

(defmethod render-form-content ((widget edit-modal))
  (let* ((formatter (row-formatter widget))
         (object (object widget))
         (form-content (make-form-content widget formatter object)))
    (render form-content)
    (setf (form-content widget) form-content)))

(defmethod render-form-content :around ((widget edit-modal))
  (with-html
    (:div :class "content"
          (when (description widget)
            (:div :class "ui grid"
                  (:div :class "column" (description widget))))
          (call-next-method))))
  
(defun make-edit-modal (&rest args &key object row-formatter description &allow-other-keys)
  (declare (ignore object row-formatter description))
  (apply #'make-instance 'edit-modal args))
