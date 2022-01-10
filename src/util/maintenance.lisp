(defpackage :tkview.util.maintenance
  (:use #:cl)
  (:import-from #:reblocks/widget
                #:defwidget)
  (:import-from #:reblocks/html
                #:with-html)
  (:export #:render-maintenance-page
           #:make-maintenance-widget))
(in-package :tkview.util.maintenance)

(defparameter *default-lock-filename* "/tmp/maintenance.lock")

(defwidget maintenance ()
  ((child :initarg :child
          :type reblocks/widget:widget)
   (lock-filename :initarg :lock-filename
                  :type pathname))
  (:documentation "This widget should be used as a toplevel widget.
It will show \"Temporarily out of service\" if there is a file /tmp/maintenance.lock"))

(defun make-maintenance-widget (child-widget &key (lock-filename *default-lock-filename*))
  (check-type child-widget reblocks/widget:widget)
  (make-instance 'maintenance
                 :child child-widget
                 :lock-filename (merge-pathnames lock-filename)))

(defgeneric render-maintenance-page (widget)
  (:method ((widget maintenance))
    (let ((title "Temporarily out of service"))
      (setf (reblocks/page:get-title) title)
      (with-html
        (:h1 title)
        (:p "Site is under maintenance.")
        (:p "We'll bring it back as soon as possible.")))))

(defun render-health-check-page ()
  (reblocks/response:immediate-response
   "Alive!" :content-type "text/plain" :code 200))

(defmethod reblocks/widget:render ((widget maintenance))
  (when (ppcre:scan "^/healthcheck$" (reblocks/request:get-path))
    (render-health-check-page))
  (with-slots (lock-filename child) widget
    (let ((lock-exists (probe-file lock-filename)))
      (if lock-exists
          (render-maintenance-page widget)
          (reblocks/widget:render child)))))
