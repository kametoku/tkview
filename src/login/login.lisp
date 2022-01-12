(in-package :tkview.login)

(defwidget login-widget ()
  ((title :initarg :title
          :type string
          :initform "Login"
          :accessor title)
   (on-success :initarg :on-success
               :type (or string function) ; XXX
               :accessor on-success)))

(defun find-valid-user (email)
  "Find the valid user of EMAIL."
  (handler-bind
      ((error (lambda (condition)
                (log:warn "Could not find the valid account." email condition)
                (sleep 2))))
    (let ((user (tkutil.auth:find-user email)))
      (tkutil.auth:check-user-availability user)
      user)))

(defun auth (email password)
;;   (log:debug email password)
  (unwind-protect
       (handler-bind
           ((error (lambda (condition)
                     (log:error "Login failed: ~A: ~A" (type-of condition) condition)
                     (sleep 2))))
         (tkutil.auth:auth-user email password))
    (reblocks/response:send-script "screenUnlock();")))

(defun render-login-form (widget)
  (check-type widget login-widget)
  (reblocks-ui/form:with-html-form
      (:POST (lambda (&key email password &allow-other-keys)
               (auth email password)
               (let ((on-success (on-success widget)))
                 (cond ((stringp on-success)
                        (reblocks/response:redirect on-success))
                       ((functionp on-success)
                        (funcall on-success email))
                       (t
                        (log:error "bad parameter" on-success)
                        (error "internal error."))))) ;XXX
       :class "ui large form"
       :extra-submit-code "screenLock();")
    (:div :class "ui stacked segment"
          (:div :class "field"
                (:div :class "ui left icon input"
                      (:i :class "user icon")
                      (:input :type "text" :name "email" :placeholder "Email address"
                              :autocomplete "username" :required t)))
          (:div :class "field"
                (:div :class "ui left icon input"
                      (:i :class "lock icon")
                      (:input :type "password" :name "password" :placeholder "Password"
                              :autocomplete "current-password")))
          (:button :class "ui fluid large teal button" :type "submit" "Login"))))


(defmethod reblocks/widget:render ((widget login-widget))
  (let ((title (title widget))
        (email-auth-modal (make-reset-password-modal :parent widget)))
    (setf (reblocks/page:get-title) title)
    (with-html
      (:div :class "ui middle aligned center aligned grid"
            (:div :class "five wide column"
                  (:div :class "ui teal header" title)
                  (render-login-form widget)
                  (:div :class "ui message"
                        (:a :onclick (tkview.modal:show-modal email-auth-modal)
                            "Forgot password?")
;;                         (:raw "&nbsp;/&nbsp;")
;;                         (:a :href "/admin/register"
;;                             "Register now")
                        ))))))

(defun make-login-page (&rest args &key title on-success)
  (declare (ignore title on-success))
  (apply #'make-instance 'login-widget args))
