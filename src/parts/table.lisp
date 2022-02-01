(in-package :cl)
(defpackage :tkview.parts.table
  (:use :cl :attribute)
  (:import-from :reblocks/widget
                :defwidget
                :render
                :update)
  (:import-from :reblocks/html
                :with-html)
  (:shadow #:formatter)
  (:export :make-table-widget))
(in-package :tkview.parts.table)

(defparameter *download-limit* 2000)

(defwidget table-widget ()
  ((formatter :initarg :formatter
              :initform #'attributes
              :reader formatter)
   (columns :reader columns) ; columns ::= ((name colname date-column-p)*)
   (date-columns :reader date-columns)  ; date-columns ::= colname*
   (sort-columns :reader sort-columns)  ; sort-columns ::= colname*
   (object-type :initarg :object-type
                :reader object-type)
   (search-parameters :initarg :search-parameters
                      :initform nil
                      :accessor search-parameters)
   (sort-parameters :initarg :sort-parameters
                    :initform nil
                    :accessor sort-parameters)
   (searcher :initarg :searcher
             :reader searcher)
   (search-args :initarg :search-args
                :initform nil
                :reader search-args)
   (no-filter-p :initarg :no-filter-p
                :initform nil
                :accessor no-filter-p)))

(defmethod initialize-instance :after ((widget table-widget) &rest args)
  (declare (ignore args))
  (let* ((formatter (formatter widget))
         (object-type (object-type widget))
         (columns (build-columns formatter object-type))
         (date-columns (loop for (name colname date-column-p) in columns
                             when date-column-p
                               collect colname))
         (sort-columns (loop for (name colname date-column-p) in columns
                             collect colname)))
    (setf (slot-value widget 'columns) columns)
    (setf (slot-value widget 'date-columns) date-columns)
    (setf (slot-value widget 'sort-columns) sort-columns)))

(defun request-parameters ()
  "Return the request parameters as plist."
  (let ((parameters (reblocks/request:get-parameters)))
    (loop for (key . value) in parameters
;;           nconc (list (intern key :keyword) value))))
          nconc (list (tkutil:string-to-keyword key) value))))

(defun date-time (string)
  (local-time:parse-timestring string :fail-on-error nil :offset (* 9 60 60)))

(defun column (column valid-columns)
  (tkutil:string-to-keyword column :valid-keywords valid-columns))

(defun sort-direction (direction)
  (tkutil:string-to-keyword direction :valid-keywords '(:asc :desc)))

(defun request-search-parameters (widget
                                  &optional (parameters (request-parameters)))
  "Extract search parameters from the request query paramenters.
NB: parameter names are case-insensitive."
  (check-type widget table-widget)
  (destructuring-bind (&key query status filter-by
                         date-column start-date end-date
                       &allow-other-keys)
      parameters
    (let* ((object-type (object-type widget))
           (status-filter-key (getf (status-filter object-type) :key))
           (secondary-filter-key (getf (secondary-filter object-type) :key))
           (date-columns (date-columns widget)))
      (nconc
       (list :query (tkutil:trim query)
             :date-column (column date-column date-columns)
             :start-date (format-date (date-time start-date))
             :end-date (format-date (date-time end-date)))
       (when status-filter-key
         (list status-filter-key status))
       (when secondary-filter-key
         (list secondary-filter-key filter-by))))))

(defun request-sort-parameters (widget
                                &optional (parameters (request-parameters)))
  "Extract sort parameters from the request query paramenters.
NB: parameter names are case-insensitive."
  (check-type widget table-widget)
  (destructuring-bind (&key sort-column sort-direction page per-page
                       &allow-other-keys)
      parameters
    (let* ((sort-columns (sort-columns widget))
           (sort-column-default (car sort-columns)))
      (list :sort-column (column (or sort-column sort-column-default) sort-columns)
            :sort-direction (sort-direction (or sort-direction "desc"))
            :page (tkutil:to-integer page :default 1)
            :per-page (tkutil:to-integer per-page :default 10)))))

(defun render-sort-switch (widget label column)
  (check-type widget table-widget)
  (destructuring-bind (&key sort-column sort-direction &allow-other-keys)
      (sort-parameters widget)
    (with-html
      (multiple-value-bind (icon key value) ;;action)
          (cond ((not (eql column sort-column))
                 (values "sort icon" :sort-column column))
                ((eql sort-direction :desc)
                 (values "sort down icon" :sort-direction :asc))
                (t
                 (values "sort up icon" :sort-direction :desc)))
        (:button :style "border:none;background-color:transparent;color:#337ab7;font-weight:bold;"
                 :onclick (reblocks/actions:make-js-action
                           (lambda (&key &allow-other-keys)
                             (setf (getf (sort-parameters widget) key) value)
                             (update widget)))
                 label
                 (:span :style "white-space:nowrap;" " " (:i :class icon)))))))

(defun encode-params (parameters)
  (let ((alist (loop for (name value) on parameters by #'cddr
                     collect (cons (tkutil:keyword-to-string name :downcase t)
                                   (tkutil:ensure-string value)))))
  (quri:url-encode-params alist)))

(defun render-search-box (widget)
  (check-type widget table-widget)
  (destructuring-bind (&key query date-column start-date end-date &allow-other-keys)
      (search-parameters widget)
    (with-html
      (reblocks-ui/form:with-html-form
          (:post (lambda (&rest args)
                   (log:debug args)
                   (let* ((object-type (object-type widget))
                          (parameters (request-search-parameters widget args))
;;                           (url-params (encode-params parameters))
                          )
                     (log:debug parameters)
                     (setf (search-parameters widget) parameters)
                     (setf (getf (sort-parameters widget) :page) 1)
                     (update widget)
;;                      (reblocks/response:send-script
;;                       `(setf (ps:@ window location search) ,url-params))
                     ))
           :class "ui form")
        (:div :class "inline fields"
              (:div :class "field"
                    (:div :class "ui icon input"
                          (:input :type "text" :placeholder "Search"
                                  :name "query" :value query
                                  ;; (:i :class "search icon")
                                  )))
              (:div :class "field"
                    (:div :class "ui labeled action input"
                          (:label :for "date-column" :class "ui icon label"
                                  (:i :class "calendar icon"))
                          (:select :class "ui compact selection dropdown"
                            :name "date-column"
                            (loop for (name value date-column-p) in (columns widget)
                                  if date-column-p
                                    do (:option :value value :selected (eql value date-column)
                                                name)))))
              (:div :class "field"
                    (render (fui.modules:make-calendar-widget
                             :name "start-date" :placeholder "Start Date"
                             :end-calendar-name "end-date"
                             :value start-date)))
              (:div :class "field"
                    (render (fui.modules:make-calendar-widget
                             :name "end-date" :placeholder "End Date"
                             :start-calendar-name "start-date"
                             :value end-date)))
              (:div :class "fields"
                    (:button :class "ui icon button" :type "submit"
                             (:i :class "search icon"))
                    (:button :class "ui icon button" :type "submit"
                             :data-tooltip
                             (format nil "Download is limited to ~A items."
                                     *download-limit*)
                             :onclick (reblocks/actions:make-js-action
                                       (lambda (&rest args)
                                         (declare (ignorable args))
                                         (update widget)
                                         (let* ((params
                                                  (encode-params
                                                   (append
                                                    (list :format "csv")
                                                    (search-parameters widget)
                                                    (sort-parameters widget))))
                                                (url (format nil "?~A" params)))
                                           (reblocks/response:redirect url))))
                             (:i :class "download icon"))))))))

(defun render-pagination-per-page-item (widget per-page &key active)
  (with-html
    (if active
        (:a :class "active item" per-page)
        (:a :class "item"
            :onclick (reblocks/actions:make-js-action
                      (lambda (&key &allow-other-keys)
                        (let ((sort-parameters (sort-parameters widget)))
                          (setf (getf sort-parameters :page) 1)
                          (setf (getf sort-parameters :per-page) per-page))
                        (update widget)))
            per-page))))

(defun render-pagination-per-page (widget)
  (check-type widget table-widget)
  (let ((per-page (getf (sort-parameters widget) :per-page)))
    (with-html
      (:div :class "ui tiny pagination menu"
            (loop for pp in '(10 25 50 100 200 500)
                  do (render-pagination-per-page-item
                      widget pp :active (= pp per-page)))))))

(defun render-pagination-item (widget page &key active (text page))
  (check-type widget table-widget)
  (with-html
    (cond ((not page)
           (:div :class "disabled item" (:raw text)))
          (active
           (:a :class "active item" page))
          (t
           (:a :class "item"
               :onclick (reblocks/actions:make-js-action
                         (lambda (&key &allow-other-keys)
                           (setf (getf (sort-parameters widget) :page) page)
                           (update widget)))
               (if (stringp text) (:raw text) text))))))

(defun render-pagination (widget total)
  (check-type widget table-widget)
  (destructuring-bind (&key page (per-page 10) &allow-other-keys)
      (sort-parameters widget)
    (let ((pages (ceiling total per-page)))
      (when (> pages 1)
        (with-html
          (cond ((or (not (integerp page)) (< page 1)) (setf page 1))
                ((> page pages) (setf page pages)))
          (let ((start (max (- page 2) 1))
                (end (min (+ page 2) pages))
                (previous (and (> page 1) (1- page)))
                (next (and (< page pages) (1+ page))))
            (:div :class "ui tiny pagination menu"
                  (render-pagination-item widget previous :text "&laquo;") ; "<<"
                  (when (> start 1)
                    (render-pagination-item widget 1))
                  (when (> start 2)
                    (render-pagination-item widget nil :text "&hellip;")) ; "..."
                  (loop for i from start to end
                        do (render-pagination-item widget i :active (= i page)))
                  (when (< end (1- pages))
                    (:div :class "disabled item" (:raw "&hellip;"))) ; "..."
                  (when (< end pages)
                    (render-pagination-item widget pages))
                  (render-pagination-item widget next :text "&raquo;") ; ">>"
                  )))))))

(defun render-menu-item (widget &key label key value active-p)
  (check-type widget table-widget)
  (check-type key keyword)
  (let ((onclick (reblocks/actions:make-js-action
                  (lambda (&key &allow-other-keys)
                    (setf (getf (search-parameters widget) key) value)
                    (update widget)))))
    (with-html
      (:a :class (if active-p "active item" "item")
          :onclick onclick label))))

(defun render-menu-bar (widget &key key values (class "ui menu"))
  (check-type widget table-widget)
  (when (and key values)
    (let ((current-value (getf (search-parameters widget) key)))
      (with-html
        (:div :class class
              (loop for value in values
                    ;; VALUE := (value :label label)
                    for active-p = (string-equal (car value) current-value)
                    do (apply #'render-menu-item
                              widget :key key :active-p active-p :value value))
              (render-menu-item widget :key key :label "ALL"
                                       :active-p (not current-value)))))))

(defun render-filter-menu-bar (widget filter-func &key class)
  (check-type widget table-widget)
  (let* ((object-type (object-type widget))
         (filter (funcall filter-func object-type)))
    ;; FILTER := (:KEY KEY :VALUES VALUES)
    (when filter
      (apply #'render-menu-bar widget :class class filter))))

(defun render-status-filter-menu-bar (widget)
  (check-type widget table-widget)
  (render-filter-menu-bar widget #'status-filter :class "ui tabular menu"))

(defun render-secondary-filter-menu-bar (widget)
  (check-type widget table-widget)
  (render-filter-menu-bar widget #'secondary-filter :class "ui secondary menu"))

(defun filter (list &key (page 1) (per-page 100)
                      (offset (if page (* (1- page) per-page) 0))
                      (limit (or per-page 100))
               &allow-other-keys)
  (let ((length (length list)))
    (cond ((>= offset length) nil)
          ((>= (+ limit offset) length) (subseq list offset))
          (t (subseq list offset (+ offset limit))))))

(defun search-objects (widget)
  (check-type widget table-widget)
  (let* ((searcher (searcher widget))
         (no-filter-p (no-filter-p widget))
         (args (append (unless no-filter-p
                         (search-parameters widget))
                       (sort-parameters widget)
                       (search-args widget))))
    (if (functionp searcher)
        (tkmito.search:with-paginate (object-type widget)
          (apply searcher args))
        (values (apply #'filter searcher args) (length searcher)))))

(defun render-table (widget)
  (check-type widget table-widget)
  (let* ((formatter (formatter widget))
         (columns (columns widget))
         (no-filter-p (no-filter-p widget)))
    (multiple-value-bind (objects total)
        (search-objects widget)
      (with-html
        (unless no-filter-p
          (render-secondary-filter-menu-bar widget)
          (render-status-filter-menu-bar widget)
          (render-search-box widget))
        (when (> total 10)
          (render-pagination-per-page widget))
        (:table :class "ui selectable celled striped compact small single line table"
                (:thead
                 (:tr
;;                   (:th (:input :type "checkbox" :onclick "toggle(this)"))
                  (loop for elem in columns
                        for label = (nth 0 elem)
                        for column = (nth 1 elem)
                        if column
                          do (:th (render-sort-switch widget label column))
                        else
                          do (:th label))))
                (:tbody
                 (loop for object in objects
;;                        do (:tr
;;                            (:td (:input :type "checkbox" :name "ids[]"
;;                                         :value (mito:object-id object)
;;                                         :form "action_form"))
                       do (object-list-page-attributes formatter object))))
        (render-pagination widget total)))))

(defun download-csv (widget)
  (check-type widget table-widget)
  (let* ((formatter (formatter widget))
         (object-type (object-type widget))
         (search-parameters (search-parameters widget))
         (sort-parameters (sort-parameters widget))
         (searcher (searcher widget))
         (disposition
           (format nil "attachment; filename=~A.csv"
                   (tkutil:keyword-to-string object-type :downcase t))))
    (remf sort-parameters :page)
    (remf sort-parameters :per-page)
    (let ((objects (apply searcher :limit *download-limit*
                          (append search-parameters sort-parameters)))
          (stream (make-string-output-stream)))
      (attribute:write-csv stream formatter objects object-type)
      (reblocks/response:immediate-response
       (get-output-stream-string stream)
       :content-type "text/csv"
       :headers (list :content-disposition disposition)))))

(defmethod reblocks/widget:render ((widget table-widget))
  (if (string-equal (reblocks/request:get-parameter "format") "csv")
      (download-csv widget)
      (render-table widget)))

(defun make-table-widget (&rest args &key object-type formatter
                                       searcher search-args no-filter-p)
  (declare (ignorable object-type formatter searcher search-args no-filter-p))
  (let ((widget (apply #'make-instance 'table-widget args)))
    (setf (search-parameters widget) (request-search-parameters widget))
    (setf (sort-parameters widget) (request-sort-parameters widget))
    widget))
