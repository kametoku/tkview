(in-package :tkview.login)

(defwidget reset-password-modal (email-auth-modal)
  ()
  (:default-initargs :title-icon "unlock" :title "Reset Password"))

(defmethod on-email-filled ((widget reset-password-modal) &key email phrase)
  "Send the email of phrase to the user."
  (let ((user (find-valid-user email)))
    (send-phrase-by-email user :email email :phrase phrase)))

(defmethod on-phrase-matched ((widget reset-password-modal) &key email phrase)
  "Show the password modal for the user of EMAIL."
  (declare (ignore phrase))
  (let ((password-modal
          (make-password-modal :object (find-valid-user email)
                               :parent (tkview.modal:parent widget))))
    (update password-modal :inserted-after widget)
    (reblocks/response:send-script
     (fui.modules:js-show-modal password-modal))))

(defmethod prompt-for-email ((widget reset-password-modal))
  "Please enter your account email address to reset the password.")

(defmethod prompt-for-phrase ((widget reset-password-modal))
  "You will receive a phrase to verify here so that you can reset your account password.")
  
(defun make-reset-password-modal (&rest args)
  (apply #'make-instance 'reset-password-modal args))
