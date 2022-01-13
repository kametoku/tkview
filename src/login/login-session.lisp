(in-package :tkview.login)

(defwidget login-session ()
  ((child :initarg :child
          :type reblocks/widget:widget
          :reader child)
   (login-page-title :initarg :login-page-title
                     :type string
                     :initform "Login"
                     :accessor login-page-title)
   (login-path :initarg :login-path
               :type string
               :initform "/login"
               :reader login-path)
   (logout-path :initarg :logout-path
                :type string
                :initform "/logout"
                :reader logout-path)
   (top-path :initarg :top-path
             :type string
             :initform "/"
             :reader top-path)
   (session-user-key :initarg :session-user-key
                     :type keyword
                     :initform :user
                     :reader session-user-key)))

(defun session-user (widget)
  (check-type widget login-session)
  (reblocks/session:get-value (session-user-key widget)))

(defun (setf session-user) (email widget)
  (check-type widget login-session)
  (let ((key (session-user-key widget)))
    (setf (reblocks/session:get-value key) email)))

(defun loginp (widget)
  "Returns non-nil if the user has logged in with a valid account.
The return value is user object that is identified by `find-user'.
Otherwise, if not logged in or the current login account became invalid,
the function returns nil. In the case of invalid login account, the error
message is displayed in the page by `tkview.util.flash:flash'."
  (check-type widget login-session)
  (handler-case
      (let ((email (session-user widget)))
        (when email
          (find-valid-user email)))
    (error (condition)
      (setf (session-user widget) nil)
      (tkview.util.flash:flash
       :level :error
       :header (format nil "User account ~A is not available." email)
       :message (format nil "~A" condition))
      nil)))

(defun on-login (widget path)
  (lambda (user)
    (setf (session-user widget) user)
    (reblocks/response:redirect path)))

(defmethod reblocks/widget:render ((widget login-session))
  (let ((path (reblocks/request:get-path))
        (user (loginp widget))
;;         (path-re (format nil "^~A(|/.*)$" (top-path widget)))
        )
    (log:debug path)
    (cond ;; ((not (cl-ppcre:scan path-re path)) nil)
          ((equal path (logout-path widget))
           (setf (session-user widget) nil)
           (reblocks/response:redirect (login-path widget)))
          ((equal path (login-path widget))
           (render (make-login-page
                    :title (login-page-title widget)
                    :on-success (on-login widget (top-path widget)))))
          ((not user)
           (render (make-login-page
                    :title (login-page-title widget)
                    :on-success (on-login widget path))))
          (t
           (tkutil.auth:with-current-user user
             (reblocks/widget:render (child widget)))))))

(defun login-session-page-p (widget &key (path (reblocks/request:get-path)))
  "Return non-nil if PATH is a page under the login session of WIDGET."
  (check-type widget login-session)
  (let ((top-path (top-path widget)))
    (cond ((null top-path))
          ((string= top-path ""))
          ((string= top-path "/"))
          (t (let ((path-re (format nil "^~A(|/.*)$" top-path)))
               (ppcre:scan path-re path))))))

(defun make-login-session-widget (child-widget &rest args
                                  &key login-path logout-path top-path
                                    session-user-key login-page-title)
  (declare (ignore login-path logout-path top-path session-user-key login-page-title))
  (check-type child-widget reblocks/widget:widget)
  (apply #'make-instance 'login-session :child child-widget args))
