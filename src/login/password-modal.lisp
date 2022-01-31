(in-package :tkview.login)

(defwidget password-modal (tkview.modal.edit:edit-modal)
  ()
  (:default-initargs :formatter #'user-password-attributes
                     :description "Please enter new password and click [OK] button."
                     :title-icon "unlock"
                     :title "Change Password"))

(defun user-password-attributes (object)
  (attrs (:object object)
         (attr "Password" ""
               :name "password" :placeholder "Password" :type "password" :edit-only t
               :minlength 8 :required t :autofocus t)
         (attr "Confirm Password" ""
               :name "password-confirm" :placeholder "Password for confirmation"
               :type "password" :edit-only t
               :minlength 8 :required t)))

(defun make-password-modal (&rest args &key object &allow-other-keys)
  (let ((title (format nil "Change Password for User ~A"
                       (tkutil.auth:friendly-user-name object))))
    (apply #'make-instance 'password-modal :title title :object object
           :formatter #'user-password-attributes args)))
