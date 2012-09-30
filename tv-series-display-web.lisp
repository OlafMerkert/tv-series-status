(in-package :cl-user)

(defpackage :tv-series-display-web
  (:nicknames :tvs-web)
  (:use :cl :ol
        :iterate
        :tvs-find
        :tvs-filter
        :cl-who)
  (:export
   :start-server))

(in-package :tvs-web)

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))
  (push
   (hunchentoot:create-static-file-dispatcher-and-handler
    "/tv-series/style.css"
    #P"/home/olaf/Projekte/tv-series-status/style.css"
    "text/css")
   hunchentoot:*dispatch-table*))

(defparameter *html-output* nil)
(setf cl-who::*indent* 4)

(defmacro with-html (&body body)
  (if (eq (first body) :top)
      `(with-html-output-to-string (*html-output* nil :prologue t :indent cl-who::*indent*)
         ,@(rest body))
      `(with-html-output (*html-output*)
         ,@body)))

(defparameter *even-row* nil)

(hunchentoot:define-easy-handler (tv-series-display :uri "/tv-series")
    (series time-range)
  (let ((series-symbol (aif (find series tv-series-epguides :key (lambda (x) (mkstr (identifier x))) :test #'string-equal)
                            (identifier it)
                           'alle))
        (time-range-symbol (cond ((string-equal time-range "past") :past)
                                 ((string-equal time-range "future") :future)
                                 ((string-equal time-range "week") :week)
                                 (t :alles))))
   (with-html
     :top
     (:html
      (:head
       (:title #1=(esc "TV Serien Status Monitor"))
       (:link :rel "stylesheet" :type "text/css" :href "/tv-series/style.css"))
      (:body
       (:h1 #1#)
       (range-select-form series-symbol time-range-symbol)
       (:h2 "Liste der Episoden")
       (:table
        (:thead
         (:tr :class "even"
              (:th "Serie")
              (:th "Se")
              (:th "Ep")
              (:th "Titel")
              (:th "Erstausstrahlung")))
        (:tbody
         (setf *even-row* nil)
         (map nil #'episode-html-row
              (filter-epi-array
               time-range-symbol
               series-symbol
               0
               tse-data)))))))))

(hunchentoot:define-easy-handler (tv-series-download :uri "/tv-series/download")
    ()
  #|(download-all-episodes)|#
  (hunchentoot:redirect "/tv-series"))

(defun range-select-form (current-series time-range)
  (with-html
    (:form
     :method "get"
     :action "/tv-series"
     (:select
      :name "series"
      (:option :value "alle" "Alle")
      (iter (for series in tv-series-epguides)
            (htm (:option :value (identifier series)
                          :selected (if (eq (identifier series) current-series)
                                        "selected")
                          (esc (series-title series))))))
     (:input :type "radio" :name "time-range" :value "alles"
             :id "time-alles"
             :checked (if (eq time-range :alles) "checked"))
     (:label :for "time-alles" "Alles")
     (:input :type "radio" :name "time-range" :value "past"
             :id "time-past"
             :checked (if (eq time-range :past) "checked"))
     (:label :for "time-past" "Vergangene")
     (:input :type "radio" :name "time-range" :value "future"
             :id "time-future"
             :checked (if (eq time-range :future) "checked"))
     (:label :for "time-future" (esc "Zukünfige"))
     (:input :type "radio" :name "time-range" :value "week"
             :id "time-week"
             :checked (if (eq time-range :week) "checked"))
     (:label :for "time-week" "Diese Woche")

     (:input :type "submit" :value "Aktualisieren"))
    #|(:form
     :method "get"
     :action "/tv-series/download"
     (:input :type "submit" :value "Herunterladen"))|#))

(defun episode-html-row (episode)
  (with-html
    (:tr :class (if *even-row* "even" "odd")
     (:td :class "title"
          (esc (series-title episode)))
     (:td :class "number"
          (fmt "~D" (season-nr episode)))
     (:td :class "number"
          (fmt "~2,'0D" (episode-nr episode)))
     (:td :class "title"
          (esc (episode-title episode)))
     (:td :class "date"
          (esc (date->string (air-date episode))))
     (notf *even-row*))))
