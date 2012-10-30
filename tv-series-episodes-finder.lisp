(in-package :cl-user)

(defpackage :tv-series-episodes-finder
  (:nicknames :tvs-find)
  (:use :cl :ol
        :css-selectors)
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
   :all-series))

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
  (information-page-url
   episode-list-url))

(create-standard-print-object tv-series identifier)

;;; retrieval from epguides

(defun make-series-epguides (x)
  (destructuring-bind (identifier title information-page-url episode-list-url) x
    (make-instance 'tv-series-epguides
                   :identifier identifier
                   :series-title title
                   :information-page-url information-page-url
                   :episode-list-url episode-list-url)))

(defparameter tv-series-epguides
  (mapcar #'make-series-epguides
   '((tbbt "The Big Bang Theory" "http://epguides.com/BigBangTheory/"
      "http://epguides.com/common/exportToCSV.asp?rage=8511")
     (himym "How I Met Your Mother" "http://epguides.com/HowIMetYourMother/"
      "http://epguides.com/common/exportToCSV.asp?rage=3918")
     (taahm "Two and a Half Men" "http://epguides.com/TwoandaHalfMen/"
      "http://epguides.com/common/exportToCSV.asp?rage=6454")
     (ngirl "New Girl" "http://epguides.com/NewGirl/"
      "http://epguides.com/common/exportToCSV.asp?rage=28304")
     (mental "The Mentalist" "http://epguides.com/Mentalist/"
      "http://epguides.com/common/exportToCSV.asp?rage=18967")
     (flash "Flashpoint" "http://epguides.com/Flashpoint/"
      "http://epguides.com/common/exportToCSV.asp?rage=18531edited")
     (nikita "Nikita" "http://epguides.com/Nikita/"
      "http://epguides.com/common/exportToCSV.asp?rage=25189")
     (covert "Covert Affairs" "http://epguides.com/CovertAffairs/"
      "http://epguides.com/common/exportToCSV.asp?rage=23686")
     (bitch "Don't trust the bitch in apartment 25"
      "http://epguides.com/DontTrusttheBinApartment23/"
      "http://epguides.com/common/exportToCSV.asp?rage=23727"))))

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


(defun find-csv-url (id)
  "Very conveniently, one can download a csv data file of all episodes
of a selected series from epguides.com.  This function extracts the
url of the csv data from the overview page of the series."
  (let* ((document (chtml:parse
                   (drakma:http-request
                    (information-page-url (get-series-by-id id)))
                   (cxml-dom:make-dom-builder)))
         (csv-url (find-if (lambda (x)
                             (search "CSV" x))
                           (mapcar (lambda (x) (dom:get-attribute x "href"))
                                   (query "a[onclick]" document)))))
    csv-url))

(defun download-csv (tv-series)
  "Download the csv and return it as lisp data."
  (cl-csv:read-csv (drakma:http-request (episode-list-url tv-series))))


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
  (let ((parts (split-sequence:split-sequence #\/ string))
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
               year  70)))
    (local-time:encode-timestamp
     0 0 0 0
     day
     month
     (cond ((>= year 100) year)
           ((< year 30)
            (+ 2000 year))
           (t (+ 1900 year))))))


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
          tv-series-epguides)))
  (save-tse-data))
