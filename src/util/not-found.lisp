(defpackage :tkview.util.not-found
  (:use :cl)
  (:export :page-not-found))
(in-package :tkview.util.not-found)

(defun page-not-found (&key (title "Page not found"))
  "Interrupts any processing or rendering and to show 404 page instead."
  (setf (weblocks/page:get-title) title)
  (weblocks/response:immediate-response
   (weblocks/html:with-html-string
     (weblocks/page:render (weblocks/app:get-current)
                           (weblocks/html:with-html-string
                             (:div :class "ui container"
                                   (:h1 "404")
                                   (:h2 "Page not found")))))
   :content-type "text/html"
   :code 404))


