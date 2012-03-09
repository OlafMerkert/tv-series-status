(in-package :cl-user)

(defpackage :tv-series-episodes-finder
  (:nicknames :tvs-find)
  (:use :cl :ol
        :css-selectors)
  (:export
   :total-nr
   :episode-nr
   :title
   :air-date
   :season-nr
   :tv-series-wp
   :date->string
   :string->date
   :series-name
   :tse-data
   :download-all-episodes))

(in-package :tvs-find)

(setf cxml:*catalog* (cxml:make-catalog))

;;; retrieval from wikipedia

(defparameter tv-series-wp
  '((tbbt   2 "http://en.wikipedia.org/wiki/List_of_The_Big_Bang_Theory_episodes"   "The Big Bang Theory")
    (himym  3 "http://en.wikipedia.org/wiki/List_of_How_I_Met_Your_Mother_episodes" "How I Met Your Mother")
    (mental 2 "http://en.wikipedia.org/wiki/Mentalist_episodes"                     "The Mentalist")
    (flash  2 "http://en.wikipedia.org/wiki/List_of_Flashpoint_episodes"            "Flashpoint")
    (nikita 2 "http://en.wikipedia.org/wiki/List_of_Nikita_episodes"                "Nikita")))

(defun find-season-tables (document)
  ;; first table contains an overview of the seasons."
  (remove-if-not
   (lambda (x)
     (or (dom:has-attribute x "width")
         (search "width" (dom:get-attribute x "style")
                 :test #'char=)))
   (rest (query "table.wikitable" document))))

(defun find-episodes (season)
  (query "tr.vevent" season))

(defun collect-text (dom-node)
  (with-output-to-string (stream)
    (labels ((rec (dom-node)
               (if (dom:text-node-p dom-node)
                   (princ (dom:node-value dom-node) stream)
                   (map 'nil #'rec (dom:child-nodes dom-node)))))
      (rec dom-node))))

(defun string->date (string)
  "YYYY-MM-DD -> local-time object"
  (local-time:encode-timestamp
   0 0 0 0
   (parse-integer (subseq string 8 10))
   (parse-integer (subseq string 5 7))
   (parse-integer (subseq string 0 4))))

(defparameter wochentage #("Mo" "Di" "Mi" "Do" "Fr" "Sa" "So"))

(defun date->string (date)
  (if date
      (format nil
              "~A ~2,'0D.~2,'0D.~4,'0D"
              (aref wochentage
                    (local-time:timestamp-day-of-week date))
              (local-time:timestamp-day date)
              (local-time:timestamp-month date)
              (local-time:timestamp-year date))
      "???"))

(defparameter date-column 2)

;; TODO deal with non-supplied date
(defun extract-date (td)
  (aand (query "span.published" td)
        (dom:node-value (dom:first-child (first it)))
        (and (<= 10 (length it)) it)
        (string->date it)))

(defun find-episode-data (episode)
  (destructuring-bind
        (total-nr episode-nr title &rest other)
      (query "td" episode)
    `((total-nr   . ,(parse-integer (dom:node-value (dom:first-child total-nr))))
      (episode-nr . ,(parse-integer (dom:node-value (dom:first-child episode-nr))) )
      (title      . ,(aif (query "b" title)
                          (collect-text (first it))
                          nil))
      (air-date   . ,(extract-date (nth date-column other))))))

(defun find-all-episodes (id)
  (let ((document (cxml:parse
                   (drakma:http-request
                    (second (assoc1 id tv-series-wp)))
                   (cxml-dom:make-dom-builder)))
        (season-counter 0)
        (date-column (first (assoc1 id tv-series-wp))))
    (mappend (lambda (season)
               (incf season-counter)
               (mapcar
                (lambda (epi)
                  (cons `(season-nr . ,season-counter)
                        (find-episode-data epi)))
                (find-episodes season)))
             (find-season-tables document))))

(bind-multi ((slot episode-nr title air-date season-nr series-name))
  (defun slot (epi)
    (assoc1 'slot epi)))

(defun strip-empty-episodes (epi-list)
  (remove-if-not #'title epi-list))

;;; local storage in cl-prevalence
(prevalence-utils:define-prevalence-storage #P"/var/tmp/tse-data-store/")
(define-storage tse-data)

(defun download-all-episodes ()
  (setf tse-data
        (apply #'concatenate 'vector
         (mapcar
          (lambda (x)
            (map 'vector (lambda (y) (list* (first x) `(series-name . ,(fourth x)) y))
                 (strip-empty-episodes
                  (find-all-episodes (first x)))))
          tv-series-wp)))
  (save-tse-data))
