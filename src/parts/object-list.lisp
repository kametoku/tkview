(defpackage :tkview.parts.object-list
  (:use :cl)
  (:import-from :weblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :weblocks/html
                :with-html)
  (:export #:define-object-list-widget))
(in-package :tkview.parts.object-list)

(defmacro define-object-list-widget (widget &key object-type searcher title icon
                                              menu-bar-formatter)
  "Define an object list page of WIDGET."
  `(progn
     (weblocks/widget:defwidget ,widget () ())
     (defmethod weblocks/widget:render ((widget ,widget))
       (let ((table-widget (tkview.parts.table:make-table-widget
                            :object-type ,object-type :searcher ,searcher))
             (title ,title))
         (setf (weblocks/page:get-title) title)
         (with-html
           (:h2 :class "ui primary header"
                (when ,icon (:i :class ,(format nil "~A icon" icon)))
                title)
           (weblocks/widget:render table-widget)
           ,(when menu-bar-formatter
              `(let ((menu-bar
                       (tkview.parts.command-menu-bar:make-command-menu-bar
                        :menu-bar-formatter ,menu-bar-formatter
                        :object ,object-type
                        :parent widget)))
                 (with-html (:div :class "ui hidden divider"))
                 (render menu-bar))))))))
