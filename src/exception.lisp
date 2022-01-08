(in-package :cl-user)
(defpackage tkview.exception
  (:use :cl)
  (:export :exception
           :exception-message
           :exception-diagnostic
           :exception-condition
           :error-message))
(in-package :tkview.exception)

(define-condition exception (error)
  ((message :initarg :message :type string
            :initform nil
            :reader exception-message)
   (diagnostic :initarg :diagnostic :type string
               :initform nil
               :reader exception-diagnostic)
;;    (uuid :initarg :uuid :type string
;;          :initform (uuid:make-v4-uuid)
;;          :reader exception-uuid)
   (condition :initarg :condition :type condition
              :initform nil
              :reader exception-condition))
  (:report (lambda (condition stream)
             (format stream
                     "~:[~;~:*~A~]~:[~;~:*: ~A~]~:[~;~:*: ~A~]"
                     (exception-message condition)
                     (exception-diagnostic condition)
                     (exception-condition condition)))))

(defgeneric error-message (condition)
  (:documentation "Returns error message for users.")
  (:method (condition)
    (values (format nil "~A" condition)
            t)))

(defgeneric exception-diagnostic (condition)
  (:method (condition)
    (format nil "Unhandled error condition ~A" (type-of condition))))

;; (defgeneric exception-uuid (condition)
;;   (:method (condition) nil))
