(defpackage :tkview.modal
  (:use :cl)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:export #:modal-widget
           #:object
           #:parent
           #:on-show
           #:on-approve
           #:render-title
           #:render-content
           #:render-action
           #:render-form-content
           #:update-modal
           #:show-modal))
(in-package :tkview.modal)

(defwidget modal-widget (fui.modules:modal-widget)
  ((object :initarg :object :initform nil :reader object)
   (parent :initarg :parent :initform nil :reader parent)
   (on-show :initarg :on-show :initform #'on-show :reader %on-show)
   (on-approve :initarg :on-approve :initform #'on-approve :reader %on-approve)
   (href :initarg :href :initform nil :reader href)
   (target :initarg :target :initform nil :reader target)
   (title :initarg :title :initform "" :reader title)
   (title-icon :initarg :title-icon :initform nil :reader title-icon)
   (approve-label :initarg :approve-label :initform "Yes" :reader approve-label)
   (approve-icon :initarg :approve-icon :initform "checkmark" :reader approve-icon)
   (deny-label :initarg :deny-label :initform "No" :reader deny-label)
   (use-form-p :initarg :use-form-p :initform nil :reader use-form-p)
   (rendered-p :initarg :rendered-p :initform nil :accessor rendered-p)))

(defmethod initialize-instance :after ((widget modal-widget) &rest args)
  (declare (ignore args))
  (unless (object widget)
    (setf (slot-value widget 'object) widget))
  ;; Make sure that the mdandatory slots are given.
  (with-slots (object parent) widget
;;     (assert object)
    (check-type parent reblocks/widget:widget))
  (with-slots (on-approve href approve-label) widget
    (assert (or (not approve-label) on-approve href)))
  (with-slots (approve-label use-form-p) widget
    (assert (or (not use-form-p) approve-label)))
  (when (rendered-p widget)
    ;; render modal widget anyway. (no pnopone)
    (render widget)))

(defmethod fui.modules:init-parameters ((widget modal-widget))
  '(on-approve (lambda (element)
                 ;; don't close modal on approval to validate input
                 ;; data before submit the form.
                 ps:false)))

(defmethod fui.modules:default-div-class ((widget modal-widget))
  "ui top aligned modal")

(defmethod render-title ((widget modal-widget))
  (let ((title-icon (format nil "~@[~A icon~]" (title-icon widget))))
    (with-html
      (:i :class "close icon")
      (:div :class "header"
            (unless (tkutil:blankp title-icon)
              (:i :class title-icon))
            (title widget)))))

(defmethod render-content ((widget modal-widget))
  (with-html
    (:div :class "content"
          (:div :class "ui inverted red segment"
                (:p "The `render-content' method is not provided for the derived widget."
                    (:br)
                    "Please implement render-content method for your own widget.")))))

(defgeneric on-show (modal object &rest args)
  (:method ((modal modal-widget) object &rest args)
    (declare (ignore args))))

(defgeneric on-approve (modal object &rest args)
  (:documentation "Action on approve.")
  (:method ((modal modal-widget) object &rest args)
    (declare (ignore args))))

(defmacro js-event-action (widget function)
  (let ((gfunction (gensym)))
    `(let ((,gfunction ,function))
       (when ,gfunction
         (reblocks/actions:make-js-action
          (lambda (&rest args)
            (apply ,gfunction (object ,widget) :widget widget args)))))))

(define-condition update-modal (error)
  ((level :initarg :level :initform "info" :reader level)
   (message :initarg :message :initform nil :reader message)))

(defun update-modal (&rest args &key level message)
  (declare (ignore level message))
  (apply #'error 'update-modal args))

(defun feedback-message (result &rest args)
  "Show the result in flash or toast.
If result is a string such as \"success\", it shows the result with a toast.
If result is a keyword such as :error, it shows the result with a flash.
ARGS ::= :message message"
  (cond ((stringp result)
         (apply #'fui.modules:send-toast :compact nil :class result
                                         :position "top center" args))
        ((keywordp result)
         (apply #'tkview.util.flash:flash-now :level result args))
        (result
         (error "Bad argument."))))

(defun do-action (widget action &rest args)
  (handler-case
      (unwind-protect
           (let ((result (apply action widget (object widget) args)))
             (reblocks/response:send-script
              (fui.modules:js-method widget 'modal "hide")) ; Hide the modal.
             (apply #'feedback-message result)
             (update (parent widget)))
        (reblocks/response:send-script '(screen-unlock)))
    (update-modal (condition)
      (let ((message (message condition)))
        (update widget)
        (reblocks/response:send-script
         (fui.modules:js-show-modal widget))
        (when message
          (feedback-message (or (level condition) "info") :message message))))
    (error (condition)
      (multiple-value-bind (message raw-p)
          (tkutil.exception:error-message condition)
        (log:error condition message)
        (feedback-message "error" :message message :raw-p raw-p)))))

(defmacro js-event-handler (widget action &optional (js-code '((event))))
  "Create a Reblocks action and return JavaScript code.
ACTION is executed on server side with web page screen locked.
ACTION should return the result that will be passed to `feedback-message'
to show the result in flash or toast."
  (let ((gaction (gensym)))
    `(let ((,gaction ,action))
       (when ,gaction
         (reblocks-parenscript:make-js-handler
          :lisp-code ((&rest args)
                      (apply #'do-action ,widget ,gaction args))
          :js-code (,(car js-code)
                    (screen-lock)
                    ,@(cdr js-code)))))))

(defmethod render-action ((widget modal-widget))
  (let ((on-approve (js-event-handler widget (%on-approve widget)))
        (deny-label (deny-label widget))
        (approve-label (approve-label widget))
        (approve-icon (approve-icon widget)))
    (with-html
      (:div :class "actions"
            (when deny-label
              (:div :class (if approve-label
                               "ui black deny button"
                               "ui block deny button")
                    deny-label))
            (when approve-label
              (:a :class (if approve-icon
                             "ui positive right labeled icon button"
                             "ui positive right button")
                  :onclick on-approve
                  :href (href widget) :target (target widget)
                  approve-label
                  (when approve-icon
                    (:i :class (format nil "~A icon" approve-icon)))))))))

(defmethod render-modal ((widget modal-widget))
  (progn
    (render-title widget)
    (render-content widget)
    (render-action widget)))

(defmethod render-form-content ((widget modal-widget))
  (render-content widget))

(defmethod render-form ((widget modal-widget))
  (let ((form-id (lack.util:generate-random-id))
        (approve-icon (approve-icon widget)))
    (with-html
      (render-title widget)
      (:div :class "content"
            (reblocks-ui/form:with-html-form
                (:POST (lambda (&rest args)
                         (apply #'do-action widget (%on-approve widget) args))
                 :class "ui form"
                 :extra-submit-code (ps:ps (screen-lock))
                 :id form-id)
              (render-form-content widget)))
      (:div :class "actions"
            (:div :class "ui block deny button"
                  (deny-label widget))
            (:button :class (if approve-icon
                                "ui positive right labeled icon button"
                                "ui positive right button")
                     :form form-id
                     :type "submit"
                     (approve-label widget)
                     (when approve-icon
                       (:i :class (format nil "~A icon" approve-icon))))))))

(defmethod reblocks/widget:render ((widget modal-widget))
  (if (use-form-p widget)
      (render-form widget)
      (render-modal widget)))

(defmethod show-modal ((widget modal-widget))
  "Returns reblocks action that shows the modal."
  (reblocks/actions:make-js-action
   (lambda (&rest args)
     (when (%on-show widget)
       (apply (%on-show widget) widget (object widget) args))
     (cond ((rendered-p widget) (update widget))
           (t (update widget
                :inserted-after (reblocks/page:page-root-widget (reblocks/page:current-page)))
              (setf (rendered-p widget) t)))
     (tkview.util.flash:flash-now)
     (reblocks/response:send-script
      (fui.modules:js-show-modal widget)))))

;; (defun make-command-modal (&rest args &key object parent on-show on-approve
;;                            &allow-other-keys)
;;   (declare (ignorable object parent on-show on-approve))
;;   (apply #'make-instance 'command-modal args))
;; ;;   (apply #'make-instance 'command-modal :parameters '(closable false) args))
