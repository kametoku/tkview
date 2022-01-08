(defpackage :tkview.util.flash
  (:use :cl)
  (:import-from :weblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :weblocks/html
                :with-html)
  (:export :make-flash-widget
           :flash-widget
           :with-flash-widget
           :flash
           :flash-now))
(in-package :tkview.util.flash)

(defwidget flash-widget ()
  ())

(defun flash (&rest args &key level header message diagnostic uuid escape-p)
  ;; LEVEL: :warning, :info, :success, or :error
  (declare (ignore level header message diagnostic uuid escape-p))
  (push args (weblocks/session:get-value :flash)))

(defun render-message (&key (level :info) (header "") (message "") diagnostic uuid escape-p)
  (with-html
    (:div :class (format nil "ui ~:[~;~:*~(~A ~)~]message" level)
          (if escape-p
              (progn
                (:div :class "header" header)
                (:p message
                    (when diagnostic (:br) diagnostic)))
              (progn
                (:div :class "header" (:raw header))
                (:p (:raw message)
                    (when diagnostic (:br) (:raw diagnostic)))))
          (when uuid
            (:div (:span :class "ui small text" :style "color:gray;"
                         ("UUID: ~A" uuid)))))))

(defmethod weblocks/widget:render ((widget flash-widget))
  (prog1
      (with-html
        (:div :class "ui container"
              (dolist (flash (weblocks/session:get-value :flash))
                (apply #'render-message flash))
              (:p)))
    (weblocks/session:delete-value :flash)))

(defun make-flash-widget ()
  (make-instance 'flash-widget))

 
(defun flash-now (&rest args)
  "ARGS ::= [ &key level header message diagnostic uuid escape-p ]"
  (let ((flash-widget (tkview.util.root:find-child 'tkview.util.flash:flash-widget)))
    (unless flash-widget (error "No flash widget found."))
    (when args
      (apply #'flash args))
    (weblocks/widget:update flash-widget)
;;     (weblocks/response:send-script "window.scrollTo(0, 0);")))
    (weblocks/response:send-script
     '(ps:chain window (scroll-to 0 0)))))
