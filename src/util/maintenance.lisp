(defpackage :tkview.util.maintenance
  (:use #:cl)
  (:import-from #:weblocks/widget
                #:defwidget)
  (:import-from #:weblocks/html
                #:with-html)
  (:export #:render-maintenance-page
           #:make-maintenance-widget))
(in-package :tkview.util.maintenance)

(defparameter *default-lock-filename* "/tmp/maintenance.lock")

(defwidget maintenance ()
  ((child :initarg :child
          :type weblocks/widget:widget)
   (lock-filename :initarg :lock-filename
                  :type pathname))
  (:documentation "This widget should be used as a toplevel widget.
It will show \"Temporarily out of service\" if there is a file /tmp/maintenance.lock"))

(defun make-maintenance-widget (child-widget &key (lock-filename *default-lock-filename*))
  (check-type child-widget weblocks/widget:widget)
  (make-instance 'maintenance
                 :child child-widget
                 :lock-filename (merge-pathnames lock-filename)))

(defgeneric render-maintenance-page (widget)
  (:method ((widget maintenance))
    (let ((title "Temporarily out of service"))
      (setf (weblocks/page:get-title) title)
      (with-html
        (:h1 title)
        (:p "Site is under maintenance.")
        (:p "We'll bring it back as soon as possible.")))))

(defun render-health-check-page ()
  (weblocks/response:immediate-response
   "Alive!" :content-type "text/plain" :code 200))

(defmethod weblocks/widget:render ((widget maintenance))
  (when (ppcre:scan "^/healthcheck$" (weblocks/request:get-path))
    (render-health-check-page))
  (with-slots (lock-filename child) widget
    (let ((lock-exists (probe-file lock-filename)))
      (if lock-exists
          (render-maintenance-page widget)
          (weblocks/widget:render child)))))
