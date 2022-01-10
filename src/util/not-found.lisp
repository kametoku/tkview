(defpackage :tkview.util.not-found
  (:use :cl)
  (:export :page-not-found))
(in-package :tkview.util.not-found)

(defun page-not-found (&key (title "Page not found"))
  "Interrupts any processing or rendering and to show 404 page instead."
  (setf (reblocks/page:get-title) title)
  (reblocks/response:immediate-response
   (reblocks/html:with-html-string
     (reblocks/page:render (reblocks/app:get-current)
                           (reblocks/html:with-html-string
                             (:div :class "ui container"
                                   (:h1 "404")
                                   (:h2 "Page not found")))))
   :content-type "text/html"
   :code 404))


