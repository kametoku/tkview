(defsystem "tkview"
  :version "0.1.0"
  :author "Tokuya Kameshima"
  :license ""
  :depends-on ("alexandria"
               "cl-csv"
               "cl-ppcre"
               "cl-reexport"
               "quri"
               "weblocks"
               "weblocks-lass"
               "weblocks-parenscript"
               "weblocks-ui"

               ;; not available from Ultralisp site
               "attribute"   ; https://github.com/kametoku/attribute
               "cl-fomantic" ; https://github.com/kametoku/cl-fomantic
               "tkmito"      ; https://github.com/kametoku/tkmito
               "tkutil"      ; https://github.com/kametoku/tkutil
               )
  :pathname "src"
  :components ((:module "exception"
                :pathname ""
                :components ((:file "exception")))
               (:module "util"
                :components ((:file "root")
                             (:file "flash")
                             (:file "maintenance")
                             (:file "not-found")
                             (:file "screen-lock")))
               (:module "modal"
                :components ((:file "modal")
                             (:file "confirm")
                             (:file "edit")
                             (:file "new")
                             (:file "email-auth")))
               (:module "parts"
                :components ((:file "command-menu-bar")
                             (:file "table")
                             (:file "object-list")
                             (:file "property")))
               (:file "tkview")
               (:file "object-url"))
  :description "Common Lisp Web Views with Weblocks."
  :in-order-to ((test-op (test-op "tkview/test"))))

(defsystem "tkview/tests"
  :author ""
  :license ""
  :depends-on ("tkview"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "tkview"))))
  :description "Test system for tkview"
  :perform (test-op (op c) (symbol-call :rove :run c)))
