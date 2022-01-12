(defpackage :tkview.login
  (:use :cl)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:import-from :attribute
                :attributes
                :attrs
                :attr)
  (:export #:make-login-session-widget
           #:session-user
           #:loginp
           #:login-session-page-p

           #:make-login-page
           #:make-reset-password-modal
           #:make-password-modal

           #:email-auth-modal
           #:send-phrase-by-email
           #:on-email-filled
           #:on-phrase-matched
           #:prompt-for-email
           #:prompt-for-phrase
           #:make-email-auth-modal
           ))

