(defpackage :tkview.parts.command-menu-bar
  (:use :cl)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:export #:command-menu-bar
           #:menu
           #:executable-p
           #:make-command-menu-bar))
(in-package :tkview.parts.command-menu-bar)

(defgeneric command-menu-bar (object))

(defun menu (&rest args &key label icon command onclick href target modal confirm
                          row-formatter
                          on-show on-approve rendered-p
                          enabled hide extra-class object)
  (declare (ignorable label icon command onclick href target modal confirm
                      row-formatter
                      on-show on-approve rendered-p
                      enabled hide extra-class object))
  (apply #'list args))

(defgeneric executable-p (object command)
  (:documentation "Returns non-nil if COMMAND is executable on OBJECT.
Otherwise, returns nil.")
  (:method (object command) t))
          
(defwidget command-menu-bar ()
  ((menu-bar-formatter :initarg :menu-bar-formatter
                       :reader menu-bar-formatter)
   (object :initarg :object
           :initform nil
           :reader object)
   (parent :initarg :parent
           :initform nil
           :reader parent)))

(defun render-command-menu (widget &rest menu-formatter
;;                             &key label icon command onclick href target modal
                            &key label icon command onclick href target modal
                              (row-formatter #'attribute:attributes)
                              on-show on-approve rendered-p
                              confirm (enabled #'executable-p)
                              hide extra-class (object (object widget)))
  (declare (ignorable menu-formatter))
  (check-type widget command-menu-bar)
  (let* ((object (tkutil:ensure-value object))
         (hide (tkutil:ensure-value hide object))
         object-type)
    (when (symbolp object)
      (setf object-type object)
      (setf object nil))
    (when hide (return-from render-command-menu))
    (let* ((command (tkutil:ensure-value command object))
           (label (tkutil:ensure-value label object))
           (icon (tkutil:ensure-value icon object))
           (disabled (unless (tkutil:ensure-value enabled object command)
                       '("disabled")))
           (onclick (tkutil:ensure-value onclick object widget))
           (href (tkutil:ensure-value href object))
           (target (tkutil:ensure-value target object))
           (confirm (tkutil:ensure-value confirm object))
           (make-modal-args (list :object object :parent (parent widget)
                                  :object-type object-type
                                  :row-formatter row-formatter
                                  :on-show on-show :on-approve on-approve
                                  :href href :target target :rendered-p rendered-p))
           (make-modal-args (loop for (key value) on make-modal-args by #'cddr
                                  when value nconc (list key value)))
           (modal (apply #'tkutil:ensure-value modal make-modal-args))
           (extra-class (tkutil:ensure-list
                         (tkutil:ensure-value extra-class object))))
      (when (and modal confirm)
        (error "Arguments modal and confirm can't be used together."))
      (when confirm
        (setf modal (apply #'tkview.modal.confirm:make-confirm-modal
                           :message confirm make-modal-args)))
      (when modal
        (check-type modal tkview.modal:modal-widget)
        (if onclick
            (error "Cannot use onclick argument with modal or confirm argument."))
        (setf href nil target nil)
        (setf onclick (tkview.modal:show-modal modal)))
      (with-html
        (:a :class (format nil "~{~A~^ ~}" (append extra-class disabled '("item")))
            :onclick onclick :href href :target target
            (when icon (:i :class (format nil "~A icon" icon)))
            label)))))

(defmethod reblocks/widget:render ((widget command-menu-bar))
  (let ((menu-bar-formatter (menu-bar-formatter widget)))
    (with-html
      (:div :class "ui compact menu"
            (loop for menu-formatter in menu-bar-formatter
                  do (apply #'render-command-menu widget menu-formatter))))))

(defun make-command-menu-bar (&rest args &key menu-bar-formatter object parent)
  (declare (ignorable menu-bar-formatter object parent))
  (apply #'make-instance 'command-menu-bar args))
