(defpackage :tkview.util.screen-lock
  (:use :cl)
  (:import-from :weblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :weblocks/html
                :with-html)
  (:export :make-screen-lock-widget))
(in-package :tkview.util.screen-lock)

(defwidget screen-lock-widget ()
  ())

(defvar *screen-lock-dependencies*
  (list
   (weblocks-parenscript:make-dependency
     (defun screen-unlock ()
       (let ((screen-lock (ps:chain document (get-element-by-id "screenLock"))))
         (ps:chain screen-lock parent-node (remove-child screen-lock))))
     (defun screen-lock (&optional (timeout -1))
       (let* ((element (ps:chain document (create-element "div")))
              (style (ps:@ element style)))
         (setf (ps:@ element id) "screenLock")
         (setf (ps:@ style height) "100%"
               (ps:@ style left) "0px"
               (ps:@ style position) "fixed"
               (ps:@ style top) "0px"
               (ps:@ style width) "100%"
               (ps:@ style z-index) "9999"
               (ps:@ style opacity) "0.0"
               (ps:@ style background-color) "white"
               (ps:@ style background-image) "url(/images/spinner.gif)"
               (ps:@ style background-repeat) "no-repeat"
               (ps:@ style background-position) "center center")
         (let ((obj-body (ps:chain document (get-elements-by-tag-name "body")
                                   (item 0))))
           (ps:chain obj-body (append-child element)))
         (set-timeout (lambda () (setf (ps:@ element style opacity) "0.5")) 200)
         (when (>= timeout 0)
           (set-timeout #'screen-unlock timeout)))))))

;; function screenLock(timeout = -1) {
;;     var element = document.createElement('div');
;;     element.id = 'screenLock';
;;     element.style.height = '100%';
;;     element.style.left = '0px';
;;     element.style.position = 'fixed';
;;     element.style.top = '0px';
;;     element.style.width = '100%';
;;     element.style.zIndex = '9999';
;;     element.style.opacity = '0.0';
;;     element.style.backgroundColor = 'white';
;;     element.style.backgroundImage = 'url(/images/spinner.gif)';
;;     element.style.backgroundRepeat = 'no-repeat';
;;     element.style.backgroundPosition = 'center center';
;;     var objBody = document.getElementsByTagName('body').item(0);
;;     objBody.appendChild(element);
;;     setTimeout(function() { element.style.opacity = '0.5'; }, 200);
;;     if (timeout >= 0) {
;;         setTimeout(function() { screenUnlock(); }, timeout);
;;     }
;; }
;; function screenUnlock(){
;;     var screenLock = document.getElementById('screenLock');
;;     screenLock.parentNode.removeChild(screenLock);
;; }

(defmethod weblocks/dependencies:get-dependencies ((widget screen-lock-widget))
  (append *screen-lock-dependencies*
          (call-next-method)))

(defmethod weblocks/widget:render ((widget screen-lock-widget))
  ;; nothing to do
  )

(defun make-screen-lock-widget ()
  (make-instance 'screen-lock-widget))
