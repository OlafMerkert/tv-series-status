(defpackage :tv-series-episodes-finder
  (:nicknames :tvs-find)
  (:use :cl :ol
        :ol-date-utils)
  (:export
   :episode-nr
   :air-date
   :season-nr
   :tv-series-epguides
   :date->string
   :string->date
   :series-name
   :tse-data
   :load-tse-data
   :save-tse-data
   :download-all-episodes
   :tv-series
   :identifier
   :series-title
   :episode
   :episode-title
   :alle
   :all-series
   :schedule-download
   :unschedule-all
   :last-download-time
   :get-series-by-id))

(in-package :tvs-find)

(setf cxml:*catalog* (cxml:make-catalog))

;; TODO lookup on the web is very, very slow!!
(defun dtd-resolver (pubid sysid)
  (declare (ignore pubid))
  (when (eq (puri:uri-scheme sysid) :http)
   (drakma:http-request sysid :want-stream t)))

(defclass/f tv-series ()
  (identifier
   series-title))

(defclass/f tv-series-epguides (tv-series)
  (episode-list-url))

(create-standard-print-object tv-series identifier)

;;; retrieval from epguides
(defmethod information-page-url ((tv-series tv-series-epguides))
  (format nil "http://epguides.com/~(~A~)/" (identifier tv-series)))

(defun url->dom (url)
  (chtml:parse
   (drakma:http-request url)
   (cxml-dom:make-dom-builder)))

(defun find-series-title (document)
  (awhen (css-selectors:query "title" document)
    (let ((html-title (dom:data (dom:first-child (first it)))))
      (subseq html-title 0
              (aif (position #\( html-title :from-end t)
                   (- it 1) nil)))))

(defun find-csv-url (document)
  "Very conveniently, one can download a csv data file of all episodes
of a selected series from epguides.com.  This function extracts the
url of the csv data from the overview page of the series."
  (find-if (lambda (x)
             (search "CSV" x))
           (mapcar (lambda (x) (dom:get-attribute x "href"))
                   (css-selectors:query "a[onclick]" document))))

(defmethod fill-slots ((tv-series tv-series-epguides))
  (let* ((info-page (information-page-url tv-series))
         (document (url->dom info-page)))
    (with-slots (series-title episode-list-url) tv-series
      (setf series-title (find-series-title document)
            episode-list-url (concatenate 'string info-page (find-csv-url document))))))

(defpar tv-series-epguides
    (mapcar
     (lambda (id)
       (aprog1 (make-instance 'tv-series-epguides :identifier id)
         (fill-slots it)))
     (sort '(blackmirror
             BigBangTheory
             ;; HowIMetYourMother
             TwoandaHalfMen
             NewGirl
             Mentalist
             ;; Flashpoint
             ;; Nikita
             CovertAffairs
             ;; DontTrusttheBinApartment23
             GameofThrones
             ;; OnceUponaTime
             )
           #'string<=
           :key (compose #'string-downcase #'symbol-name))))

(defparameter all-series
  (make-instance 'tv-series
                 :identifier 'alle
                 :series-title "Alle"))

(defun get-series-by-id (id)
  (find id tv-series-epguides :key #'identifier))

(defun collect-text (dom-node)
  "Just print out all the text-nodes which are descendants of the
given DOM-NODE."
  (with-output-to-string (stream)
    (labels ((rec (dom-node)
               (if (dom:text-node-p dom-node)
                   (princ (dom:node-value dom-node) stream)
                   (map 'nil #'rec (dom:child-nodes dom-node)))))
      (rec dom-node))))

;; date formatting
(declaim (inline string->date date->string))
(defun string->date (string)
  "YYYY-MM-DD -> local-time object"
  (ol-date-utils:parse-date string))

(defparameter wochentage #("So" "Mo" "Di" "Mi" "Do" "Fr" "Sa"))

(defun date->string (date)
  "local-time object -> WT Ta.Mo.Jahr or ??? if date is NIL."
  (if date
      (ol-date-utils:print-date date)
      "???"))

(defun find-all-episodes (tv-series)
  "Download the episode information for the TV-SERIES."
  (transform-csv
   (download-csv tv-series)
   tv-series))

(defun download-csv (tv-series)
  "Download the csv and return it as lisp data."
  (cl-csv:read-csv (collect-text (first (css-selectors:query "pre" (url->dom (episode-list-url tv-series)))))))

(defclass/f episode (tv-series prevalence-utils:prevailing)
  (season-nr
   episode-nr
   episode-title
   air-date))

(create-standard-print-object episode identifier (season-nr episode-nr))

(defun transform-csv (csv tv-series)
  (let* ((stripped-csv (remove-if #'length=1 csv))
         ;; determine the columns containing the interesting information
         (column-indices
          (mapcar (lambda (x) 
                    (position (third x) (elt stripped-csv 0)
                              :test #'string-equal))
                  extract-table)))
    (map 'vector
         (lambda (row)
           (apply #'make-instance 'episode
                  :identifier (identifier tv-series)
                  :series-title (series-title tv-series)
                  (mapcan (lambda (col x)
                            (list (second x)
                                  (extract-transform (first x)
                                                     (elt row col))))
                          column-indices extract-table)))
         (subseq stripped-csv 1))))

(defparameter extract-table
  '((season-nr   :season-nr      "season")
    (episode-nr  :episode-nr     "episode")
    (title       :episode-title  "title")
    (air-date    :air-date       "airdate")))

(defun parse-integer/non-number (string)
  "Parse an integer from a string, interpret the empty string as 0."
  (if (string= string "")
      0
      (parse-integer string)))

(defgeneric extract-transform (type string)
  (:documentation "depending on type, transform a given string to the
  appropriate format, be it number, string or date."))

(defmethod extract-transform ((type (eql 'season-nr)) string)
  (parse-integer/non-number string))

(defmethod extract-transform ((type (eql 'episode-nr)) string)
  (parse-integer/non-number string))

(defmethod extract-transform ((type (eql 'title)) string)
   string)

(defparameter months
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defmethod extract-transform ((type (eql 'air-date)) string)
  (let ((parts (ppcre:split "[ /]"  string))
        day month year)
    (case (length parts)
      ((2) (setf day   17
                 month (+ 1 (position (elt parts 0) months :test #'string-equal))
                 year  (parse-integer (elt parts 1))))
      ((3) (setf day   (parse-integer (elt parts 0))
                 month (+ 1 (position (elt parts 1) months :test #'string-equal))
                 year  (parse-integer (elt parts 2))))
      (t (setf day   17
               month 1
               year  20)))
    (encode-date day month year)))


(defun strip-empty-episodes (epi-list)
  (remove-if-not #'episode-title epi-list))

;;; local storage in cl-prevalence
(prevalence-utils:define-prevalence-storage #P"/var/tmp/tse-data-store/")
(define-storage tse-data)

(defun download-all-episodes ()
  "Iterate over the predefined list of series, download the episode
information and store it both in a special var and in prevalence."
  (setf tse-data
        (apply #'concatenate 'vector
         (mapcar
          #'find-all-episodes
          tv-series-epguides))
        (cl-prevalence:get-root-object prevalence-utils:storage
                                       'last-download-time)
        (local-time:now))
  (save-tse-data))

(defun last-download-time ()
  (cl-prevalence:get-root-object prevalence-utils:storage
                                 'last-download-time))

;;; regular downloading of new data
(defpar daily (clon:make-typed-cron-schedule :day-of-month '*))

#+sbcl
(defun schedule-download ()
  (clon:schedule-function 'download-all-episodes
                          (clon:make-scheduler daily :allow-now-p t)
                          :name "TVS download"
                          :thread t))

#+sbcl
(defun unschedule-all ()
  (mapc #'sb-ext:unschedule-timer (sb-ext:list-all-timers)))

(defun clear-cache ()
  (setf tse-data #()
        (cl-prevalence:get-root-object prevalence-utils:storage 'last-download-time)
        nil)
  (save-tse-data))

(defun open-series-page (series-id)
  (run-program "/usr/bin/xdg-open"
               (information-page-url (get-series-by-id series-id))))
