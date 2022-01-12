(in-package :tkview.login)

(defclass %user ()
  ((user :initarg :user :reader user)))

(defmethod tkview.modal.edit:update-object :after ((object %user) &rest args)
  (let ((user (user object)))
    (apply #'tkmito.model:update user args)))

(defwidget password-modal (tkview.modal.edit:edit-modal)
  ()
  (:default-initargs :row-formatter #'user-password-attributes
                     :description "Please enter new password and click [OK] button."
                     :title-icon "unlock"
                     :title "Change Password"))

(defun user-password-attributes (object)
  (when (eql object '%user) (setf object nil))
  (check-type object (or %user null))
  (attrs (:object object)
         (attr "Password" ""
               :name "password" :placeholder "Password" :type "password" :edit-only t
               :minlength 8 :required t :autofocus t)
         (attr "Confirm Password" ""
               :name "password-confirm" :placeholder "Password for confirmation"
               :type "password" :edit-only t
               :minlength 8 :required t)))

(defmethod attributes ((object %user))
  (user-password-attributes object))

(defun make-password-modal (&rest args &key object &allow-other-keys)
  (let ((user (make-instance '%user :user object))
        (title (format nil "Change Password for User ~A"
                       (tkutil.auth:friendly-user-name object))))
    (apply #'make-instance 'password-modal :title title :object user args)))
