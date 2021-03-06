(in-package :tkview.login)

(defparameter *expiration-minutes* 10)

(defclass email-auth ()
  ((email :initarg :email :initform nil :accessor email)
   (phrase :initform nil :accessor phrase)
   (expected-phrase :accessor expected-phrase)
   (expiration :accessor expiration)))

(defun generate-phrase ()
  "Generates 6-digit random phrase code."
  (format nil "~6,'0D" (random 1000000)))

(defun update-expected-phrase (email-auth)
  (check-type email-auth email-auth)
  (setf (expected-phrase email-auth) (generate-phrase))
  (setf (expiration email-auth) (local-time:adjust-timestamp (local-time:now)
                                  (offset :minute *expiration-minutes*))))

(defmethod initialize-instance :after ((email-auth email-auth) &rest args)
  (declare (ignore args))
  (update-expected-phrase email-auth))

(define-condition phrase-expired (error)
  ((phrase :initarg :phrase :initform nil :reader phrase))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Phrase expired.  Please retry with new phrase."))))

(define-condition phrase-not-matched (error)
  ((phrase :initarg :phrase :initform nil :reader phrase))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Phrase not matched."))))

(defun check-phrase (email-auth &key (phrase (phrase email-auth)))
  (check-type email-auth email-auth)
  (when (local-time:timestamp> (local-time:now) (expiration email-auth))
    (error 'phrase-expired :phrase phrase))
  (unless (string= phrase (expected-phrase email-auth))
    (sleep 2)
    (error 'phrase-not-matched :phrase phrase)))

(defun email-auth-attributes (object)
  (when (eql object 'email-auth) (setf object nil))
  (check-type object (or email-auth null))
  (attrs (:object object)
         (if (email object)
             (attr "Phrase" #'phrase
                   :name "phrase" :placeholder "Enter your phrase code"
                   :type "text" :required t)
             (attr "Email" #'email
                   :name "email" :placeholder "Email address"
                   :type "email" :required t))))

(defwidget email-auth-modal (tkview.modal.edit:edit-modal)
  ()
  (:default-initargs :object (make-instance 'email-auth)
                     :formatter #'email-auth-attributes
                     :approve-label "Next"
                     :approve-icon "arrow alternate circle right"))

(defmethod send-phrase-by-email (user &key email phrase)
  (log:debug user email phrase)
  (error "Please implement `send-phrase-by-email' for user ~A." user))

(defmethod on-email-filled ((widget email-auth-modal) &key email)
  (log:debug widget email)
  (error "Please implement `on-email-filled' for user ~A." widget))

(defmethod on-phrase-matched ((widget email-auth-modal) &key email)
  (log:debug widget email)
  (error "Please implement `on-email-filled' for user ~A." widget))

(defmethod prompt-for-email ((widget email-auth-modal))
  "Please enter your email address.")

(defmethod prompt-for-phrase ((widget email-auth-modal))
  "You will receive a phrase to verify here.")

(defmethod tkview.modal.edit:description ((widget email-auth-modal))
  (let ((object (tkview.modal:object widget)))
    (check-type object email-auth)
    (if (email object)
        (prompt-for-phrase widget)
        (prompt-for-email widget))))

(defmethod tkview.modal:on-approve ((widget email-auth-modal) (object email-auth)
                                    &key &allow-other-keys)
  (call-next-method)
  (let ((phrase (phrase object))
        (email (email object)))
    (cond (phrase
           (check-phrase object)
           (on-phrase-matched widget :email email :phrase phrase)
           (list "success" :message "Phrase matched."))
          (email
           (on-email-filled widget :email email :phrase (expected-phrase object))
           (tkview.modal:update-modal
            :message "Sent a phrase to your email address."))
          (t                            ; shouldn't happen
           (error "Internal error.")))))

(defmethod tkview.modal:on-show ((widget email-auth-modal) object
                                 &key &allow-other-keys)
  (setf (email object) nil
        (phrase object) nil)
  (update-expected-phrase object))
  
(defun make-email-auth-modal (&rest args &key (title "Email authentication")
                              &allow-other-keys)
  (apply #'make-instance 'email-auth-modal
         :title-icon "unlock" :title title args))
